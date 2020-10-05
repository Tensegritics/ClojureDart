import 'dart:async';
import 'dart:io';
import 'dart:convert';

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

Future<dynamic> dispatchMacro(ReaderInput r) async {
  final s = await r.read();
  if (s == null) throw FormatException("EOF while reading dispatch sequence.");
  final cu = s.codeUnitAt(0);
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

void initMacros() {
  macros["(".codeUnitAt(0)]=(ReaderInput r) async => await readDelimited(r, ")".codeUnitAt(0));
  macros["'".codeUnitAt(0)]=(ReaderInput r) async => ["QUOTE", await read(r)];
  macros[")".codeUnitAt(0)]=unexpectedMacroReader("closing parenthesis");
  macros["]".codeUnitAt(0)]=unexpectedMacroReader("closing square bracket");
  macros["}".codeUnitAt(0)]=unexpectedMacroReader("closing curly brace");
  macros["#".codeUnitAt(0)]=dispatchMacro;
  dispatchMacros["_".codeUnitAt(0)]=(ReaderInput r) async {await read(r); return r; };
  dispatchMacros["{".codeUnitAt(0)]=(ReaderInput r) async => Set.from(await readDelimited(r, "}".codeUnitAt(0)));
}

Future main() async {
  initMacros();
  final rdr = ReaderInput(stdin.transform(utf8.decoder)); //.transform(const LineSplitter()));
  print("Clojure Dart v0.0.ε");
  try {
    while(true) {
      stdout.write("=> ");
      print((await read(rdr)).toString());
    }
  } finally {
    rdr.close();
  }
}
