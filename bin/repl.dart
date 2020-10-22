import 'dart:async';
import 'dart:io';
import 'dart:convert';
import 'dart:isolate';
import 'dart:developer' as dev;
import 'package:vm_service/vm_service_io.dart' as vms;
import 'package:vm_service/utils.dart' as vmutils;

import '../lib/reader.dart';
import '../lib/cljd.dart';
import '../lib/evalexpr.dart' as evalexpr;

printSeparated(Iterable x, StringSink out, [delim=" "]) {
  if (x.isNotEmpty) {
    clj_print(x.first, out);
    x.skip(1).forEach((x) {
        out.write(delim);
        clj_print(x, out);
    });
  }
}

void printString(String s, StringSink out) {
  out..write('"')
    ..write(s.replaceAllMapped(RegExp("([\x00-\x1f])|[\"]"), (m) {
      if (m.group(1) == null) return r'\"';
      switch(m.group(1)) {
        case '\b': return "\\b";
        case '\n': return "\\n";
        case '\r': return "\\r";
        case '\t': return "\\t";
        case '\f': return "\\f";
      }
      return "\\u${m.group(1).codeUnitAt(0).toRadixString(16).padLeft(4,'0')}";
    }))
    ..write('"');
}

void clj_print(dynamic x, StringSink out) {
  if (x is List) {
    out.write("(");
    printSeparated(x, out);
    out.write(")");
    return;
  }
  if (x is Set) {
    out.write("#{");
    printSeparated(x, out);
    out.write("}");
    return;
  }
  if (x == null) return out.write("nil");
  if (x is BigInt) return (out..write(x.toString())).write("N");
  if (x is String) return printString(x, out);
  out.write(x.toString());
}

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
    out.write("import 'dart:io';\nFuture exec() async {\n  return ");
    emit(x, out);
    out.write(";\n}\n");
  } finally {
    await out.close();
  }
  if (!await reload()) {
    await File("lib/evalexpr.dart").writeAsString("Future exec() => null;\n");
    await reload(); // TODO throw or msg on false
  }
  return evalexpr.exec();
}


Future main() async {
  final rdr = Reader(stdin.transform(utf8.decoder)); //.transform(const LineSplitter()));
  print("Clojure Dart v0.0.ε");
  try {
    while(true) {
      stdout.write("=> ");
      final expr = await rdr.read();
      final ret = await eval(expr);
      clj_print(ret, stdout);
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
      if (name.endsWith(".") && name != ".") {
        return List()..add(NEW)..add(Symbol(null, name.substring(0,name.length-1)))..addAll(expr.getRange(1, expr.length));
      }
      if (name.startsWith(".") && name != ".") {
        return List()..add(DOT)..add(expr[1])..add(Symbol(null, name.substring(1)))..addAll(expr.getRange(2, expr.length));
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

void emitDot(List expr,  StringSink out) {
  out.write("(");
  emit(expr[1], out);
  out..write(".")..write(expr[2].name)..write("(");
  emitArgs(expr.getRange(3, expr.length), out);
  out.write("))");
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

void emit(dynamic expr,  StringSink out) {
  expr=macroexpand(expr);
  if (expr is List) {
    if (expr.first == NEW) {
      emitNew(expr, out);
      return;
    }
    if (expr.first == DOT) {
      emitDot(expr, out);
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
