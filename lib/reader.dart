import 'dart:async';
import 'dart:io';
import 'cljd.dart';

class Reader {
  // for stream management and pushback
  Stream<String> _in;
  String _buf;
  StreamSubscription<String> _sub;
  Completer<String> _completer;

  Future<String> _read() {
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

  void _unread(String s) {
    assert(_buf == null);
    assert(_sub != null);
    _buf = s=="" ? null : s;
  }

  Future close() {
    final f = _sub.cancel();
    _sub = null;
    return f;
  }

  // for actual reader
  // 1. macros
  final _macros = List<Future<dynamic> Function(Reader)>(128);
  final _dispatchMacros = List<Future<dynamic> Function(Reader)>(128);

  Future<dynamic> Function(Reader) _getMacro(int codeunit) {
    if (codeunit < _macros.length)
      return _macros[codeunit];
    return null;
  }

  bool _isMacro(int codeUnit) {
    return null != _getMacro(codeUnit);
  }

  bool _isTerminating(int codeunit) {
    String ch = String.fromCharCode(codeunit);
    if ("'#".indexOf(ch) >= 0) return false;
    if (_isMacro(codeunit)) return true;
    if (SPACE_REGEXP.matchAsPrefix(ch).end > 0) return true;
    return false;
  }

  void _initMacros() {
    // list
    _macros[cu0("(")]=(Reader r) async => await readDelimited(r, cu0(")"));
    // vector
    _macros[cu0("[")]=(Reader r) async => Vector(await readDelimited(r, cu0("]")));
    // quote
    _macros[cu0("'")]=(Reader r) async => ["QUOTE", await r.read()];
    // malformed
    _macros[cu0(")")]=unexpectedMacroReader("closing parenthesis");
    _macros[cu0("]")]=unexpectedMacroReader("closing square bracket");
    _macros[cu0("}")]=unexpectedMacroReader("closing curly brace");
    // dispatch
    _macros[cu0("#")]=dispatchMacro;
    // discard
    _dispatchMacros[cu0("_")]=(Reader r) async {await r.read(); return r; };
    // set
    _dispatchMacros[cu0("{")]=(Reader r) async => Set.from(await readDelimited(r, cu0("}")));
    // comment
    _macros[cu0(";")]=_dispatchMacros[cu0("!")]=(Reader r) async {
      while(true) {
        final s = await r._read();
        if (s == null) return r;
        final i = COMMENT_CONTENT_REGEXP.matchAsPrefix(s).end;
        if (i < s.length) {
          r._unread(s.substring(i));
          return r;
        }
      }
    };
    Future<String> readStringContent(Reader r) async {
      final sb = StringBuffer();
      while(true) {
        final s = await r._read();
        if (s == null) throw FormatException("Unexpected EOF while reading a string.");
        final i = STRING_CONTENT_REGEXP.matchAsPrefix(s).end;
        sb.write(s.substring(0, i));
        if (i < s.length) {
          r._unread(s.substring(i+1));
          return sb.toString();
        }
      }
    }
    // string
    _macros[cu0("\"")]=(Reader r) async {
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
    _dispatchMacros[cu0("\"")]=(Reader r) async =>  RegExp(await readStringContent(r));
  }


  Reader(this._in) {
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

    _initMacros();
  }

  /// Reads one value (including null).
  ///
  /// [delim] specifies a codeunit upon which to stop reading.
  /// [read] returns itself on EOF unless [delim] is specified
  /// in which case an exception is thrown.
  Future<dynamic> read([int delim = -1]) async {
    while(true) {
      final s = await _read();
      if (s == null) {
        if (delim < 0) return this;
          throw FormatException("Unexpected EOF, expected " + String.fromCharCode(delim));
      }
      final i = SPACE_REGEXP.matchAsPrefix(s).end; // match can't fail because *
      if (i == s.length) continue;

      final ch = s.codeUnitAt(i);
      if (ch == delim) {
        _unread(s.substring(i+1));
        return this;
      }

      final macroreader = _getMacro(ch);
      if (macroreader != null) {
        _unread(s.substring(i+1));
        final v = await macroreader(this);
        if (v == this) continue;
          return v;
      }

      _unread(s.substring(i));
      final token = await readToken(this);
      return interpretToken(token);
    }
  }
}


int cu0(String s) {
  return s.codeUnitAt(0);
}

Future<dynamic> dispatchMacro(Reader r) async {
  final s = await r._read();
  if (s == null) throw FormatException("EOF while reading dispatch sequence.");
  final cu = cu0(s);
  if (cu < r._dispatchMacros.length) {
    final macroreader = r._dispatchMacros[cu];
    if (macroreader != null) {
      r._unread(s.substring(1));
      return macroreader(r);
    }
  }
  throw FormatException("Unepxected dispatch sequence: #" + s.substring(0,1));
}

final SPACE_REGEXP = RegExp(r"[\s,]*");

Future<String> readToken(Reader r) async {
  final sb = StringBuffer();
  var s = "";
  var i = 0;
  while(true) {
    if (i == s.length) {
      sb.write(s);
      s = await r._read();
      if (s == null) break;
      i = 0;
    }
    int cu = s.codeUnitAt(i);
    if (r._isTerminating(cu)) {
      sb.write(s.substring(0, i));
      r._unread(s.substring(i));
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
  throw FormatException("Can't interpret token: $token");
}

final unexpectedMacroReader = (String msg) =>
  (Reader r) {
    throw FormatException("Unexpected " + msg);
  };

Future<List> readDelimited(Reader r, int delim) async {
    final ret = List();
    while(true) {
      final v = await r.read(delim);
      if (v == r) return ret;
      ret.add(v);
    }
}

final COMMENT_CONTENT_REGEXP=RegExp(r"[^\r\n]*");
final STRING_CONTENT_REGEXP=RegExp("(?:[^\"\\\\]|\\\\.)*");
final STRING_ESC_REGEXP=RegExp(r"\\(?:u([0-9a-fA-F]{0,4})|([0-7]{1,3})|(.))");
