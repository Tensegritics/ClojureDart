import 'dart:io';
import 'dart:isolate';
import 'dart:developer' as dev;
import 'dart:collection';
import 'package:vm_service/vm_service_io.dart' as vms;
import 'package:vm_service/utils.dart' as vmutils;

import 'cljd.dart';
import 'evalexpr.dart' as evalexpr;

Future<bool> reload() async {
  final serverUri = (await dev.Service.getInfo()).serverUri;
  final wsUri = vmutils.convertToWebSocketUrl(serviceProtocolUrl: serverUri).toString();
  final service = await vms.vmServiceConnectUri(wsUri);
  final res = await service.reloadSources(dev.Service.getIsolateID(Isolate.current));
  return res.success;
}

Future eval(x) async {
  final out = File("lib/evalexpr.dart").openWrite();
  try {
    out.write("import 'dart:io';\n");
    out.write("import 'cljd.dart';\n\n");
    out.write("Future exec() async {\n_redef();\n");
    emit(x, {}, out, "return ");
    out.write("}\n\n");
    ns.emit(out);
  } finally {
    await out.close();
  }
  if (!await reload()) {
//    await File("lib/evalexpr.dart").writeAsString("Future exec() => null;\n");
//    await reload(); // TODO throw or msg on false
  }
  return evalexpr.exec();
}

/// Half namespace, half lib
class NamespaceLib {
  final _flds = Map<Symbol, String>();
  final _fns = Map<Symbol, String>();
  final _dirty = Set();
  void def(Symbol name, String init) {
    _fns.remove(name);
    if (_flds.containsKey(name)) _dirty.add(name);
    _flds[name] = init;
  }
  void defn(Symbol name, String decl) {
    _flds.remove(name);
    _dirty.remove(name);
    _fns[name] = decl;
  }
  void emit(StringSink out) {
    for(var decl in _fns.values) {
      out..write(decl)..write("\n");
    }
    for(var init in _flds.values) {
      out..write("var ")..write(init)..write("\n");
    }
    out.write("_redef() {\n");
    for(var sym in _dirty) {
      out..write(_flds[sym])..write("\n");
    }
    out.write("}\n\n");
    _dirty.clear();
  }
}

var ns = NamespaceLib();

const DOT = Symbol(null, ".");
const NEW = Symbol(null, "new");
const FN = Symbol(null, "fn");
const LET = Symbol(null, "let");
const DEF = Symbol(null, "def");
const DO = Symbol(null, "do");
const IF = Symbol(null, "if");
const LOOP = Symbol(null, "loop");
const RECUR = Symbol(null, "recur");
const AMPERSAND = Symbol(null, "&");

dynamic macroexpand1(dynamic expr) {
  if ((expr is List) && (expr.length > 0)) {
    final first = expr[0];
    if (first is Symbol) {
      final name = first.name;
      if (name.endsWith(".") && name != ".") {
        return List()..add(NEW)..add(Symbol(null, name.substring(0,name.length-1)))..addAll(expr.getRange(1, expr.length));
      }
      if (name.startsWith(".") && name != ".") {
        return List()..add(DOT)..add(expr[1])..add(Symbol(null, name.substring(1)))..addAll(expr.getRange(2, expr.length));
      }
      if (first == DO) {
        return List()..add(LET)..add(Vector([]))..addAll(expr.getRange(1, expr.length));
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

void emitParams(Iterable params, StringSink out) {
  out.write("(");
  var comma = false;
  params.forEach((param) {
      if (comma) out.write(",");
      out.write(param);
      comma=true;
  });
  out.write(")");
}

void emitNew(List expr, env, StringSink out, String locus) {
  final sb = StringBuffer(locus);
  emitExpr(expr[1], env, sb);
  emitArgs(expr.getRange(2, expr.length), env, out, sb.toString());
}

void emitDot(List expr, env, StringSink out, String locus) {
  final match = RegExp(r"^(-)?(.*)").matchAsPrefix(expr[2].name);
  final sb = StringBuffer(locus);
  if (isAtomic(expr[1])) {
    emitExpr(expr[1], env, sb);
  } else {
    var f = tmp();
    out.write("var $f;\n");
    emit(expr[1], env, out, "$f=");
    sb.write(f);
  }
  sb.write(".${match.group(2)}");
  if (match.group(1) == null) {
    emitArgs(expr.getRange(3, expr.length), env, out, sb.toString());
  } else {
    out.write(sb..write(";\n"));
  }
}

assoc(m, k, v) {
  final m1 = Map.of(m as Map);
  m1[k]=v;
  return m1;
}
lookup(m, k, [v]) {
  return m.containsKey(k) ? m[k] : v;
}

var _munge=0;
String munge(Symbol v, env) {
  return lookup(env, v) == null ? v.name : "${v.name}__${++_munge}";
}

bool isVariadic(List params) {
  return params.contains(AMPERSAND);
}

void emitBodies(List bodies, env, StringSink out) {
  bodies.sort((a, b) => a.first.length.compareTo(b.first.length));
  var isAlsoVariadic = isVariadic(bodies.last.first);
  var smallestArity = bodies.length == 1 && isAlsoVariadic ? bodies.first.first.length - 2 : bodies.first.first.length;
  var biggestArity = bodies.last.first.length;
  out.write("(");
  List paramsAlias = [];
  for (var i = 0; i < (isAlsoVariadic ? 8 :  biggestArity) ; i++) {
    final temp = "p_$i";
    paramsAlias.add(temp);
    if (i > 0) out.write(",");
    if (i == smallestArity) out.write("[");
    out.write("$temp");
    if (i >=  smallestArity) out.write("=MISSING_ARG");
  }
  if (bodies.length > 1 || isAlsoVariadic) out.write(']');
  out.write(") {\n");
  for (var i = 0; i < bodies.length - 1; i++) {
    var params = bodies[i][0];
    out.write("if (${paramsAlias[params.length]} == MISSING_ARG) {\n");
    var bodyenv = env;
    for(var i = 0; i < params.length; i++) {
      var varname = munge(params[i], bodyenv);
      out.write("var ${varname}=${paramsAlias[i]};\n");
      bodyenv = assoc(bodyenv, params[i], varname);
    }
    emitBody(1, bodies[i], bodyenv, out, "return ");
    out.write("}\n");
  }
  var params = bodies.last.first;
  var bodyenv = env;
  for (var i = 0; i < params.length; i++) {
    if (params[i] == AMPERSAND) {
      var varname = munge(params[i+1], bodyenv);
      out.write("var $varname=[${paramsAlias.skip(i).toList().join(', ')}].takeWhile((e) => e != MISSING_ARG).toList();\n");
      bodyenv = assoc(bodyenv, params[i+1], varname);
      break;
    }
    var varname = munge(params[i], bodyenv);
    out.write("var $varname=${paramsAlias[i]};\n");
    bodyenv = assoc(bodyenv, params[i], varname);
  }
  emitBody(1, bodies.last, bodyenv, out, "return ");
  out.write("}");
}

List extractBodies(List expr) {
  bool namedFn = expr[1] is Symbol;
  final offset = namedFn ? 2 : 1;
  return expr[offset] is Vector ? [expr.skip(offset).toList()] : expr.skip(offset).toList();
}

void emitFn(List expr, env, StringSink out, String locus) {
  bool namedFn = expr[1] is Symbol;
  var bodies = extractBodies(expr);
  if (namedFn) {
    final fnname = munge(expr[1], env);
    env = assoc(env, expr[1], fnname);
    out.write(fnname);
    emitBodies(bodies, env, out);
    out.write("\n$locus$fnname;\n");
  } else {
    out.write(locus);
    emitBodies(bodies, env, out);
    out.write(";\n");
  }
}

void emitTopFn(Symbol name, List expr, env, StringSink out) {
  bool namedFn = expr[1] is Symbol;
  // @TODO : on n'est pas clair sur la nature du name (dixit @cgrand)
  if (namedFn) {
    env = assoc(env, expr[1], name.toString());
  }
  emitSymbol(name, env, out);
  emitBodies(extractBodies(expr), env, out);
  out.write("\n");
}

void emitIf(List expr, env, StringSink out, String locus) {
  final test = tmp();
  out.write("var $test;\n");
  emit(expr[1], env, out, "$test=");
  out.write("if ($test) {\n");
  emit(expr[2], env, out, locus);
  out.write("}else{\n");
  emit((expr.length == 4) ? expr[3] : null, env, out, locus);
  out.write("}\n");
  return;
}

Map emitBinding(List pair, env, StringSink out) { // not great to return Map
  final sym = pair[0];
  final varname = munge(sym, env);
  out.write("var $varname;\n");
  emit(pair[1], env, out, "$varname=");
  return assoc(env, sym, varname);
}

void emitBody(int from, List exprs, env, StringSink out, String locus) {
  for (var i = from; i < exprs.length-1; i++) {
    emit(exprs[i], env, out, ""); // pure effect
  }
  emit(exprs.last, env, out, locus);
}

void emitLet(List expr, env, StringSink out, String locus) {
  dynamic bindings = expr[1];
  final bindings1 = [];
  for (var i = 0; i < bindings.length; i += 2) {
    bindings1.add(bindings.sublist(i, i+2 > bindings.length ? bindings.length : i + 2));
  }
  env = bindings1.fold(env, (acc, elem) {
      return emitBinding(elem, acc, out);
  });
  emitBody(2, expr, env, out, locus);
}

void emitLoop(List expr, env, StringSink out, String locus) {
  dynamic bindings = expr[1];
  final bindings1 = [];
  for (var i = 0; i < bindings.length; i += 2) {
    bindings1.add(bindings.sublist(i, i+2 > bindings.length ? bindings.length : i + 2));
  }
  env = bindings1.fold(env, (acc, elem) {
      return emitBinding(elem, acc, out);
  });
  out.write("do {\n");
  emitBody(2, expr, env, out, locus);
  out.write("break;\n} while(true);\n");
}

void emitRecur(List expr, env, StringSink out, String locus) {
  var args=List();
  for(var i = 1; i < expr.length; i++) {
    final arg = tmp();
    args.add(arg);
    out.write("var $arg;\n");
    emit(expr[i], env, out, "${arg}=");
  }
  out.write("continue;\n");
}

void emitSymbol(Symbol sym, env, StringSink out) {
  out.write(lookup(env, sym) ?? sym.name);
}

void emitStr(String s, StringSink out) {
  out..write('"')
    ..write(s.replaceAllMapped(RegExp("([\x00-\x1f])|[\$\"]"), (m) {
      if (m.group(1) == null) return "\\${m.group(0)}";
      switch(m.group(1)) {
        case '\b': return "\\b";
        case '\n': return "\\n";
        case '\r': return "\\r";
        case '\t': return "\\t";
        case '\f': return "\\f";
        case '\v': return "\\v";
      }
      return "\\x${m.group(1).codeUnitAt(0).toRadixString(16).padLeft(2,'0')}";
    }))
    ..write('"');
}

class tmp {
  static var _counter=0;
  String _name;
  tmp() { _name="_${++_counter}"; }
  toString() => _name;
}

bool isAtomic(x) {
  return x == null || x is Symbol || x is Keyword || x is String || x is bool || x is num;
}

void emitFnCall(List expr, env, StringSink out, String locus) {
  final sb = StringBuffer(locus);
  if (isAtomic(expr[0])) {
    emitExpr(expr[0], env, sb);
  } else {
    var f = tmp();
    out.write("var $f;\n");
    emit(expr[0], env, out, "$f=");
    sb.write(f);
  }
  emitArgs(expr.getRange(1, expr.length), env, out, sb.toString());
}

void emitArgs(Iterable expr, env, StringSink out, String locus) {
  var named = false;
  var keypos = true;
  final args = List.of(expr.map((x) {
    keypos = !keypos;
    if (x == AMPERSAND) {
      named = true;
      keypos = false;
      return x;
    }
    if (named && keypos || isAtomic(x))
      return x;
    final arg = tmp();
    out.write("var $arg;\n");
    emit(x, env, out, "${arg}=");
    return arg;
  }));
  out.write("$locus(");
  var comma=false;
  named = false;
  args.forEach((arg) {
    if (arg == AMPERSAND) {
      named=true;
      keypos=false;
      return;
    }
    if (comma) out.write(", ");
    comma=true;
    keypos = !keypos;
    if (named && keypos) {
      out.write("${arg.name}: ");
      comma = false;
      return;
    }
    emitExpr(arg, env, out);
  });
  out.write(");\n");
}

void emitExpr(dynamic expr, env, StringSink out) {
  if (expr is Symbol) {
    emitSymbol(expr, env, out);
  } else if (expr is String) {
    emitStr(expr, out);
  } else {
    out.write(expr.toString());
  }
}

void emitDef(dynamic expr, env, StringSink out, String locus) {
  final sym = expr[1];
  final value = macroexpand(expr[2]);
  final sb = StringBuffer();
  if (value is List && value[0] == FN) {
    emitTopFn(sym, value, env, sb);
    ns.defn(sym, sb.toString());
  } else {
    emitSymbol(sym, env, sb);
    sb.write("=");
    if (isAtomic(value)) {
      emitExpr(value, env, sb);
    } else {
      sb.write("(){\n");
      emit(value, env, sb, "return ");
      sb.write("}()");
    }
    sb.write(";\n");
    ns.def(sym, sb.toString());
  }
  emit(expr[1], env, out, locus);
}

void emit(dynamic expr, env, StringSink out, String locus) {
  expr=macroexpand(expr);
  if (expr is List) {
    if (expr.first == NEW) {
      emitNew(expr, env, out, locus);
      return;
    }
    if (expr.first == DOT) {
      emitDot(expr, env, out, locus);
      return;
    }
    if (expr.first == FN) {
      emitFn(expr, env, out, locus);
      return;
    }
    if (expr.first == LET) {
      emitLet(expr, env, out, locus);
      return;
    }
    if (expr.first == DEF) {
      emitDef(expr, env, out, locus);
      return;
    }
    if (expr.first == IF) {
      emitIf(expr, env, out, locus);
      return;
    }
    if (expr.first == LOOP) {
      emitLoop(expr, env, out, locus);
      return;
    }
    if (expr.first == RECUR) {
      emitRecur(expr, env, out, locus);
      return;
    }
    emitFnCall(expr, env, out, locus);
    return;
  }
  out.write(locus);
  emitExpr(expr, env, out);
  out.write(";\n");
}
