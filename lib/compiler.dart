import 'dart:io';
import 'dart:isolate';
import 'dart:developer' as dev;
import 'dart:collection';
import 'package:vm_service/vm_service_io.dart' as vms;
import 'package:vm_service/utils.dart' as vmutils;

import 'cljd.dart';
import 'evalexpr.dart' as evalexpr;

enum EmitLocus { RETURN, EXPR }

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
    emit(x, {}, out, EmitLocus.RETURN);
    out.write("\n}\n");
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

void emitArgs(Iterable args, m, StringSink out, [comma = false]) {
  // var comma = false;
  var key = true;
  var named = false;
  args.forEach((arg) {
    if (named) {
      if (key && comma) out.write(",");
      if (key) out..write(arg.name)..write(":");
      else emit(arg, m, out, EmitLocus.EXPR);
      comma=true;
      key=!key;
      return;
    }
    if ((arg is Symbol) && (arg.name == "&")) {
      named = key = true;
      return;
    }
    if (comma) out.write(",");
    emit(arg, m, out, EmitLocus.EXPR);
    comma=true;
  });
}

void emitParams(Iterable params, StringSink out) {
  out.write("(");
  var comma = false;
  params.forEach((param) {
      if (comma) out.write(",");
      out.write(param.name);
      comma=true;
  });
  out.write(")");
}

void emitNew(List expr, m, StringSink out, EmitLocus locus) {
  if (locus == EmitLocus.RETURN) out.write("return ");
  out..write("(")..write(expr[1].name)..write("(");
  emitArgs(expr.getRange(2, expr.length), m, out);
  out.write("))");
  if (locus == EmitLocus.RETURN) out.write(";");
}

void emitDot(List expr, m, StringSink out, EmitLocus locus) {
  if (locus == EmitLocus.RETURN) out.write("return ");
  final match = RegExp(r"^-(.*)").matchAsPrefix(expr[2].name);
  out.write("(");
  emit(expr[1], m, out, EmitLocus.EXPR);
  out..write(".")..write((match == null) ? expr[2].name : match.group(1));
  if (match == null) {
    out.write("(");
    emitArgs(expr.getRange(3, expr.length), m, out);
    out.write(")");
  }
  out.write(")");
  if (locus == EmitLocus.RETURN) out.write(";");
}

assoc(m, k, v) {
  final m1 = Map.of(m as Map);
  m1[k]=v;
  return m1;
}
Symbol lookup(m, k, [v]) {
  return m.containsKey(k) ? m[k] : v;
}
Symbol munge(Symbol v) {
  return v;
}
bind(m, k) {
  return assoc(m, k, (m.containsKey(k) ? Symbol(null, (m[k].toString() + "_")) : munge(k)));
}

// ((n) {
//   fact (n) { if (n > 0) return n*fact(n-1); return 1; };
//   return fact(n);
// })
void emitFn(List expr, m, StringSink out, EmitLocus locus) {
  if (locus == EmitLocus.RETURN) out.write("return ");
  bool namedFn = expr[1] is Symbol;
  List paramsAlias = [];
  dynamic params = namedFn ? expr[2] : expr[1];
  dynamic m1 = params.fold(m, (acc, elem) {
      dynamic newEnv = bind(acc, elem);
      paramsAlias.add(lookup(newEnv, elem));
      return newEnv;
  });
  out.write("(");
  emitParams(paramsAlias, out);
  out.write("{");
  if (namedFn) {
    emitSymbol(expr[1], m1, out);
    emitParams(paramsAlias, out);
    out.write("{");
  }
  for (var i = namedFn ? 3 : 2; i < expr.length; i++) {
    // FIXME
    emit(expr[i], m1, out, i == (expr.length - 1) ? EmitLocus.RETURN : EmitLocus.EXPR);
  }
  if (namedFn) {
    out.write("}; return ");
    emitSymbol(expr[1], m1, out);
    emitParams(paramsAlias, out);
    out.write(";");
  }
  out.write("})");
  if (locus == EmitLocus.RETURN) out.write(";");
}

void emitIf(List expr, m, StringSink out, EmitLocus locus) {
  if (locus == EmitLocus.RETURN) {
    out.write("if (");
    emit(expr[1], m, out, EmitLocus.EXPR);
    out.write("){");
    emit(expr[2], m, out, EmitLocus.RETURN);
    out.write("}");
    emit((expr.length == 4) ? expr[3] : null, m, out, EmitLocus.RETURN);
    return;
  }
  out.write("(");
  emit(expr[1], m, out, EmitLocus.EXPR);
  out.write(" ? ");
  emit(expr[2], m, out, EmitLocus.EXPR);
  out.write(" : ");
  emit((expr.length == 4) ? expr[3] : null, m, out, EmitLocus.EXPR);
  out.write(")");
}

void emitBinding(List pair, m, StringSink out) {
  out.write("final ");
  emitSymbol(pair[0], bind(m, pair[0]), out);
  out.write(" = ");
  emit(pair[1], m, out, EmitLocus.EXPR);
  out.write(";");
}

void emitLet(List expr, m, StringSink out, EmitLocus locus) {
  if (locus != EmitLocus.RETURN) out.write("(() {");
  dynamic bindings = expr[1];
  final bindings1 = [];
  for (var i = 0; i < bindings.length; i += 2) {
    bindings1.add(bindings.sublist(i, i+2 > bindings.length ? bindings.length : i + 2));
  }
  dynamic m1 = bindings1.fold(m, (acc, elem) {
      emitBinding(elem, acc, out);
      return bind(acc, elem.first);
  });
  for (var i = 2; i < expr.length-1; i++) {
    emit(expr[i], m1, out, EmitLocus.EXPR);
    out.write(";\n");
  }
  emit(expr.last, m1, out, EmitLocus.RETURN);
  if (locus != EmitLocus.RETURN) out.write("})()");
}

void emitSymbol(Symbol sym, m, StringSink out) {
  out.write(lookup(m, sym, sym));
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

void emitFnCall(List expr, m, StringSink out, EmitLocus locus) {
    if (locus == EmitLocus.RETURN) out.write("return ");
    out.write("(");
    emit(expr.first, m, out, EmitLocus.EXPR);
    out.write("(");
    emitArgs(expr.getRange(1, expr.length), m, out);
    out.write("))");
    if (locus == EmitLocus.RETURN) out.write(";");
}

void emit(dynamic expr, m, StringSink out, EmitLocus locus ) {
  expr=macroexpand(expr);
  if (expr is List) {
    if (expr.first == NEW) {
      emitNew(expr, m, out, locus);
      return;
    }
    if (expr.first == DOT) {
      emitDot(expr, m, out, locus);
      return;
    }
    if (expr.first == FN) {
      emitFn(expr, m, out, locus);
      return;
    }
    if (expr.first == LET) {
      emitLet(expr, m, out, locus);
      return;
    }
    if (expr.first == IF) {
      emitIf(expr, m, out, locus);
      return;
    }
    emitFnCall(expr, m, out, locus);
    return;
  }
  if (locus == EmitLocus.RETURN) out.write("return ");
  if (expr is Symbol) {
    emitSymbol(expr, m, out);
  } else if (expr is String) {
    emitStr(expr, out);
  } else {
    out.write(expr.toString());
  }
  if (locus == EmitLocus.RETURN) out.write(";");
}
