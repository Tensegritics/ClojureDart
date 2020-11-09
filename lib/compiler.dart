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
    out.write("import 'dart:io';\nFuture exec() async {\n");
    emit(x, {}, out, "return ");
    out.write("}\n");
  } finally {
    await out.close();
  }
  if (!await reload()) {
    await File("lib/evalexpr.dart").writeAsString("Future exec() => null;\n");
    await reload(); // TODO throw or msg on false
  }
  return evalexpr.exec();
}

const DOT = Symbol(null, ".");
const NEW = Symbol(null, "new");
const FN = Symbol(null, "fn");
const LET = Symbol(null, "let");
const DO = Symbol(null, "do");
const IF = Symbol(null, "if");
const LOOP = Symbol(null, "loop");
const RECUR = Symbol(null, "recur");

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

void emitArgs(Iterable args, env, StringSink out, [comma = false]) {
  // var comma = false;
  var key = true;
  var named = false;
  args.forEach((arg) {
    if (named) {
      if (key && comma) out.write(",");
      if (key) out..write(arg.name)..write(":");
      else out.write(arg);
      comma=true;
      key=!key;
      return;
    }
    if ((arg is Symbol) && (arg.name == "&")) {
      named = key = true;
      return;
    }
    if (comma) out.write(",");
    out.write(arg);
    comma=true;
  });
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

List emitArgsDeclaration(Iterable expr, env, StringSink out) { // FIXME returning a LIST is not cool dude
  var decls = List();
  var args = expr.toList();
  for (var i = 0; i < args.length; i++) {
    if (((args[i] is Symbol) && (args[i].name == "&")) || (args[i] is Keyword)) {
      decls.add(args[i]);
      continue;
    }
    final arg = "_arg${_counter++}";
    decls.add(arg);
    out.write("var $arg;\n");
    emit(args[i], env, out, "${arg}=");
  }
  return decls;
}

void emitNew(List expr, env, StringSink out, String locus) {
  var args = emitArgsDeclaration(expr.getRange(2, expr.length), env, out);
  out..write("$locus(")..write(expr[1].name)..write("(");
  emitArgs(args, env, out);
  out.write("));\n");
}

void emitDot(List expr, env, StringSink out, String locus) {
  final match = RegExp(r"^-(.*)").matchAsPrefix(expr[2].name);
  final varname = "_test_${_counter++}";
  out.write("var $varname;\n");
  emit(expr[1], env, out, "$varname=");
  var args;
  if (match == null) args = emitArgsDeclaration(expr.getRange(3, expr.length), env, out);
  // FIXME : hack for handling special symbol like stdio
  out..write("$locus($varname.")..write((match == null) ? expr[2].name : match.group(1));
  if (match == null) {
    out.write("(");
    emitArgs(args, env, out);
    out.write(")");
  }
  out.write(");\n");
}

assoc(m, k, v) {
  final m1 = Map.of(m as Map);
  m1[k]=v;
  return m1;
}
lookup(m, k, [v]) {
  return m.containsKey(k) ? m[k] : v;
}
String munge(Symbol v) {
  return "${v.name}__${_counter++}";
}

void emitFn(List expr, env, StringSink out, String locus) {
  bool namedFn = expr[1] is Symbol;
  List paramsAlias = [];
  dynamic params = namedFn ? expr[2] : expr[1];
  env = params.fold(env, (env, elem) {
      String varname = munge(elem);
      paramsAlias.add(varname);
      return assoc(env, elem, varname);
  });
  out.write("$locus(");
  emitParams(paramsAlias, out);
  out.write("{\n");
  if (namedFn) {
    env = assoc(env, expr[1], munge(expr[1]));
    emitSymbol(expr[1], env, out);
    emitParams(paramsAlias, out);
    out.write("{\n");
  }
  for (var i = namedFn ? 3 : 2; i < expr.length-1; i++) {
    emit(expr[i], env, out, "");
  }
  emit(expr.last, env, out, "return ");
  if (namedFn) {
    out.write("}; return ");
    emitSymbol(expr[1], env, out);
    emitParams(paramsAlias, out);
    out.write(";\n");
  }
  out.write("});\n");
}

void emitIf(List expr, env, StringSink out, String locus) {
  final test = "_test_${_counter++}";
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
  final varname = munge(sym);
  out.write("var $varname;\n");
  emit(pair[1], env, out, "$varname=");
  return assoc(env, sym, varname);
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
  for (var i = 2; i < expr.length-1; i++) {
    emit(expr[i], env, out, ""); // pure effect
  }
  emit(expr.last, env, out, locus);
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
  for (var i = 2; i < expr.length-1; i++) {
    emit(expr[i], env, out, ""); // pure effect
  }
  emit(expr.last, env, out, locus);
  out.write("break;\n} while(true);\n");
}

void emitRecur(List expr, env, StringSink out, String locus) {
  var args=List();
  for(var i = 1; i < expr.length; i++) {
    final arg = "_arg${_counter++}";
    args.add(arg);
    out.write("var $arg;\n");
    emit(expr[i], env, out, "${arg}=");
  }
  out.write("continue;\n");
}

void emitSymbol(Symbol sym, env, StringSink out) {
  out.write(lookup(env, sym));
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

var _counter=0;

void emitFnCall(List expr, env, StringSink out, String locus) {
  var args=List();
  for(var i = 0; i < expr.length; i++) {
    final arg = "_arg${_counter++}";
    args.add(arg);
    out.write("var $arg;\n");
    emit(expr[i], env, out, "${arg}=");
  }
  out.write(locus);
  out.write(args[0]);
  out.write("(");
  out.write(args.getRange(1, args.length).join(","));
  out.write(");\n");
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
  if (expr is Symbol) {
    emitSymbol(expr, env, out);
  } else if (expr is String) {
    emitStr(expr, out);
  } else {
    out.write(expr.toString());
  }
  out.write(";\n");
}
