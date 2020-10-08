import 'dart:async';
import 'dart:io';
import 'dart:convert';
import 'dart:isolate';
import 'dart:developer' as dev;
import 'package:vm_service/vm_service_io.dart' as vms;
import 'package:vm_service/utils.dart' as vmutils;

class ReaderInput {
  Stream<String> _in;
  String _buf;
  StreamSubscription<String> _sub;
  Completer<String> _completer;

  ReaderInput(this._in) {
    void read1(String s) {
      assert(_buf == null);
      if (s == "") return;
      _completer.complete(s);
      _sub.pause();
    }

    _sub = _in.listen(read1, onDone: () {
      assert(_buf == null);
      _completer.complete(null);
      _sub = null;
    });
    _sub.pause();
  }

  Future<String> read() {
    if (_sub == null) return null;
    if (_buf != null) {
      final buf = _buf;
      _buf = null;
      return Future.value(buf);
    }
    _completer = Completer();
    _sub.resume();
    return _completer.future;
  }

  void unread(String s) {
    assert(_buf == null);
    assert(_sub != null);
    _buf = s=="" ? null : s;
  }

  Future close() {
    final f = _sub.cancel();
    _sub = null;
    return f;
  }
}

final macros = List<Future<dynamic> Function(ReaderInput)>(128);
final dispatchMacros = List<Future<dynamic> Function(ReaderInput)>(128);

Future<dynamic> Function(ReaderInput) getMacro(int codeunit) {
  if (codeunit < macros.length)
    return macros[codeunit];
  return null;
}

bool isMacro(int codeUnit) {
  return null != getMacro(codeUnit);
}

int cu0(String s) {
  return s.codeUnitAt(0);
}

Future<dynamic> dispatchMacro(ReaderInput r) async {
  final s = await r.read();
  if (s == null) throw FormatException("EOF while reading dispatch sequence.");
  final cu = cu0(s);
  if (cu < dispatchMacros.length) {
    final macroreader = dispatchMacros[cu];
    if (macroreader != null) {
      r.unread(s.substring(1));
      return macroreader(r);
    }
  }
  throw FormatException("Unepxected dispatch sequence: #" + s.substring(0,1));
}

final SPACE_REGEXP = RegExp(r"[\s,]*");


bool isTerminating(int codeunit) {
  String ch = String.fromCharCode(codeunit);
  if ("'#".indexOf(ch) >= 0) return false;
  if (isMacro(codeunit)) return true;
  if (SPACE_REGEXP.matchAsPrefix(ch).end > 0) return true;
  return false;
}

/// Reads one value (including null) from [r].
///
/// [delim] specifies a codeunit upon which to stop reading.
/// [read] returns [r] itself on EOF unless [upto] is specified
/// in which case an exception is thrown.
Future<dynamic> read(ReaderInput r, [int delim = -1]) async {
  while(true) {
    final s = await r.read();
    if (s == null) {
      if (delim < 0) return r;
      throw FormatException("Unexpected EOF, expected " + String.fromCharCode(delim));
    }
    final i = SPACE_REGEXP.matchAsPrefix(s).end; // match can't fail because *
    if (i == s.length) continue;

    final ch = s.codeUnitAt(i);
    if (ch == delim) {
      r.unread(s.substring(i+1));
      return r;
    }

    final macroreader = getMacro(ch);
    if (macroreader != null) {
      r.unread(s.substring(i+1));
      final v = await macroreader(r);
      if (v == r) continue;
      return v;
    }

    r.unread(s.substring(i));
    final token = await readToken(r);
    return interpretToken(token);
  }
}

Future<String> readToken(r) async {
  final sb = StringBuffer();
  var s = "";
  var i = 0;
  while(true) {
    if (i == s.length) {
      sb.write(s);
      s = await r.read();
      if (s == null) break;
      i = 0;
    }
    int cu = s.codeUnitAt(i);
    if (isTerminating(cu)) {
      sb.write(s.substring(0, i));
      r.unread(s.substring(i));
      break;
    }
    i++;
  }
  return sb.toString();
}

final INT_REGEXP = RegExp(r"([-+]?)(?:(0)|([1-9][0-9]*)|0[xX]([0-9A-Fa-f]+)|0([0-7]+)|([1-9][0-9]?)[rR]([0-9A-Za-z]+)|0[0-9]+)(N)?$");
final DOUBLE_REGEXP = RegExp(r"([-+]?[0-9]+(\.[0-9]*)?([eE][-+]?[0-9]+)?)(M)?$");

// differs from Clojure: :3a and a/:x are not valid
final SYMBOL_REGEXP = RegExp(r"([:]{1,2})?(?:([^0-9/:].*)/)?(/|[^0-9/:][^/]*)$");

class Symbol {
  final String namespace;
  final String name;
  const Symbol._internal(this.namespace, this.name);
  const factory Symbol(String namespace, String name) = Symbol._internal;
  String toString() { return namespace != null ? "$namespace/$name" : "$name"; }
}

class Keyword {
  final String namespace;
  final String name;
  const Keyword._internal(this.namespace, this.name);
  const factory Keyword(String namespace, String name) = Keyword._internal;
  String toString() { return namespace != null ? ":$namespace/$name" : ":$name"; }
}

dynamic interpretToken(String token) {
  switch (token) {
    case 'nil': return null;
    case 'true': return true;
    case 'false': return false;
  }

  var m = INT_REGEXP.matchAsPrefix(token);
  if (m != null) {
    final parse = m.group(8) == null
    ? (m.group(1) == "-"
       ? (String s, int radix) => -int.parse(s, radix: radix)
       : (String s, int radix) => int.parse(s, radix: radix))
    : (m.group(1) == "-"
       ? (String s, int radix) => -BigInt.parse(s, radix: radix)
       : (String s, int radix) => BigInt.parse(s, radix: radix));
    if (m.group(2) != null) return 0;
    if (m.group(3) != null) return parse(m.group(3), 10);
    if (m.group(4) != null) return parse(m.group(4), 16);
    if (m.group(5) != null) return parse(m.group(5), 8);
    if (m.group(7) != null) return parse(m.group(7), int.parse(m.group(6)));
    throw FormatException("Invalid number.");
  }

  m = DOUBLE_REGEXP.matchAsPrefix(token);
  if (m != null) {
    if (m.group(4) != null) throw FormatException("BigDecimal not supported yet.");
    return double.parse(token);
  }

  m = SYMBOL_REGEXP.matchAsPrefix(token);
  if (m != null) {
    final namespace = m.group(2);
    final name = m.group(3);
    if (m.group(1) != null) // TODO ::kw
      return Keyword(namespace, name);
    return Symbol(namespace, name);
  }
  return "TOK" + token;
}

final unexpectedMacroReader = (String msg) =>
  (ReaderInput r) {
    throw FormatException("Unexpected " + msg);
  };

Future<List> readDelimited(ReaderInput r, int delim) async {
    final ret = List();
    while(true) {
      final v = await read(r, delim);
      if (v == r) return ret;
      ret.add(v);
    }
}

final COMMENT_CONTENT_REGEXP=RegExp(r"[^\r\n]*");
final STRING_CONTENT_REGEXP=RegExp("(?:[^\"\\\\]|\\\\.)*");
final STRING_ESC_REGEXP=RegExp(r"\\(?:u([0-9a-fA-F]{0,4})|([0-7]{1,3})|(.))");

void initMacros() {
  // list
  macros[cu0("(")]=(ReaderInput r) async => await readDelimited(r, cu0(")"));
  // quote
  macros[cu0("'")]=(ReaderInput r) async => ["QUOTE", await read(r)];
  // malformed
  macros[cu0(")")]=unexpectedMacroReader("closing parenthesis");
  macros[cu0("]")]=unexpectedMacroReader("closing square bracket");
  macros[cu0("}")]=unexpectedMacroReader("closing curly brace");
  // dispatch
  macros[cu0("#")]=dispatchMacro;
  // discard
  dispatchMacros[cu0("_")]=(ReaderInput r) async {await read(r); return r; };
  // set
  dispatchMacros[cu0("{")]=(ReaderInput r) async => Set.from(await readDelimited(r, cu0("}")));
  // comment
  macros[cu0(";")]=dispatchMacros[cu0("!")]=(ReaderInput r) async {
    while(true) {
      final s = await r.read();
      if (s == null) return r;
      final i = COMMENT_CONTENT_REGEXP.matchAsPrefix(s).end;
      if (i < s.length) {
        r.unread(s.substring(i));
        return r;
      }
    }
  };
  Future<String> readStringContent(ReaderInput r) async {
    final sb = StringBuffer();
    while(true) {
      final s = await r.read();
      if (s == null) throw FormatException("Unexpected EOF while reading a string.");
      final i = STRING_CONTENT_REGEXP.matchAsPrefix(s).end;
      sb.write(s.substring(0, i));
      if (i < s.length) {
        r.unread(s.substring(i+1));
        return sb.toString();
      }
    }
  }
  // string
  macros[cu0("\"")]=(ReaderInput r) async {
    return (await readStringContent(r)).replaceAllMapped(STRING_ESC_REGEXP, (Match m) {
      if (m.group(1) != null) {
        if (m.group(1).length < 4) throw FormatException("Unsupported escape for character: \\u${m.group(1)}; \\u MUST be followed by 4 hexadecimal digits");
        return String.fromCharCode(int.parse(m.group(1), radix: 16));
      }
      if (m.group(2) != null) return String.fromCharCode(int.parse(m.group(2), radix: 8));
      switch(m.group(3)) {
        case '"': case '\\': return m.group(3);
        case 'b': return "\b";
        case 'n': return "\n";
        case 'r': return "\r";
        case 't': return "\t";
        case 'f': return "\f";
      }
      throw FormatException("Unsupported escape character: \\${m.group(3)}");
    });
  };
  // regexp
  dispatchMacros[cu0("\"")]=(ReaderInput r) async =>  RegExp(await readStringContent(r));
}

clj_print(dynamic x) {
  print(x.toString());
}

var reloads = 0;

Future<bool> reload() async {
  // Build Websocket URI
  final serverUri = (await dev.Service.getInfo()).serverUri;
  final wsUri = vmutils.convertToWebSocketUrl(serviceProtocolUrl: serverUri).toString();
  // Get VM Service
  final service = await vms.vmServiceConnectUri(wsUri);

  final vm = await service.getVM();
  // Reload first isolate
  final res = await service.reloadSources(dev.Service.getIsolateID(Isolate.current));
  print("reload ${++reloads}");
  return res.success;
}

Future main() async {
  initMacros();
  final rdr = ReaderInput(stdin.transform(utf8.decoder)); //.transform(const LineSplitter()));
  print("Clojure Dart v0.0.ε");
  try {
    while(true) {
      stdout.write("=> ");
      final expr = await read(rdr);
      await reload();
      await clj_print(expr);
      emit(expr, stdout);
      stdout.write("\n");
    }
  } finally {
    rdr.close();
  }
}

const DOT = Symbol(null, ".");
const NEW = Symbol(null, "new");

dynamic macroexpand1(dynamic expr) {
  if ((expr is List) && (expr.length > 0)) {
    final first = expr[0];
    if (first is Symbol) {
      final name = first.name;
      if (first.name.endsWith(".")) {
        return List()..add(NEW)..add(Symbol(null, first.name.substring(0,first.name.length-1)))..addAll(expr.getRange(1, expr.length));
      }
      if (first.name.startsWith(".")) {
        return List()..add(DOT)..add(expr[1])..add(Symbol(null, first.name.substring(1)))..addAll(expr.getRange(2, expr.length));
      }
    }
    return expr;
  }
  return expr;
}

dynamic macroexpand(dynamic expr) {
  var prev;
  do {
    prev = expr;
    expr = macroexpand1(expr);
  } while (prev != expr);
  return expr;
}

void emitArgs(Iterable args, StringSink out, [comma = false]) {
  // var comma = false;
  var key = true;
  var named = false;
  args.forEach((arg) {
    if (named) {
      if (key && comma) out.write(",");
      if (key) out..write(arg.name)..write(":");
      else emit(arg, out);
      comma=true;
      key=!key;
      return;
    }
    if ((arg is Symbol) && (arg.name == "&")) {
      named = key = true;
      return;
    }
    if (comma) out.write(",");
    emit(arg, out);
    comma=true;
  });
}

void emitNew(List expr,  StringSink out) {
  out..write("(")..write(expr[1].name)..write("(");
  emitArgs(expr.getRange(2, expr.length), out);
  out.write("))");
}

void emitStr(String s, StringSink out) {
  out..write('"')
    ..write(s.replaceAllMapped(RegExp("([\x00-\x1f])|[\$\"]"), (m) {
      if (m.group(1) != null) return "\\x${m.group(1).codeUnitAt(0).toRadixString(16).padLeft(2,'0')}";
      return "\\${m.group(0)}";
    }))
    ..write('"');
}

void emit(dynamic expr,  StringSink out) {
  expr=macroexpand(expr);
  if (expr is List) {
    if (expr.first == NEW) {
      emitNew(expr, out);
      return;
    }
    out.write("(");
    emit(expr.first, out);
    out.write("(");
    emitArgs(expr.getRange(1, expr.length), out);
    out.write("))");
    return;
  }
  if (expr is String) {
    emitStr(expr, out);
    return;
  }
  out.write(expr.toString());
}

/*
class MyApp extends StatelessWidget {
  @override
  Widget build(BuildContext context) {
    return MaterialApp(
      title: 'Welcome to Flutter',
      home: Scaffold(
        appBar: AppBar(
          title: Text('Welcome to Flutter'),
        ),
        body: Center(
          child: Text('Hello World'),
        ),
      ),
    );
  }
}

void main() => runApp(MyApp());

(defn main []
  (Flutter/main ; 1 gestion des imports
    (reify StatelessWidget ; 2 quel constructeur parent ?
      (build [_ ^BuildContext context]
        (MaterialApp. & ; 3 named arguments
          :title "Welcome to Flutter"
          :home (Scaffold. &
            :appBar (AppBar. & :title (Text. "Welcome to Flutter"))
            :body (Center. & :child (Text. "Hello World"))))))))

(defn main []
  (Flutter/main ; 1 gestion des imports
    (reify StatelessWidget ; 2 quel constructeur parent ?
      (build [_ ^BuildContext context]
        (MaterialApp... ; 3 named arguments
          :title "Welcome to Flutter"
          :home (Scaffold...
            :appBar (AppBar... :title (Text. "Welcome to Flutter"))
            :body (Center... :child (Text. "Hello World"))))))))

*/
