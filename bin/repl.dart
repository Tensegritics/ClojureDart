import 'dart:async';
import 'dart:io';
import 'dart:convert';

import '../lib/reader.dart';
import '../lib/printer.dart';
import '../lib/compiler.dart';


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
