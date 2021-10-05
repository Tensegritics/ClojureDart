import 'dart:io';
import 'dart:convert';
import 'package:analyzer/dart/analysis/analysis_context_collection.dart';
import 'package:analyzer/dart/analysis/results.dart';
import 'package:analyzer/error/error.dart';
import 'package:analyzer/file_system/physical_file_system.dart';
import 'package:analyzer/file_system/overlay_file_system.dart';
import 'package:analyzer/dart/element/visitor.dart';
import 'package:analyzer/dart/element/element.dart';
import 'package:analyzer/dart/element/type.dart';
import 'package:analyzer/dart/element/nullability_suffix.dart';

final Set<String> libsToDo = {};

final Set<String> libsDone = {};

bool isPublic(Element e) => e.isPublic;

R fnil<R,E>(R f(E e), E? x, R fallback) => x != null ? f(x) : fallback;

String M(Map<String,dynamic> m) {
  final sb = StringBuffer("{");
  var first = true;
  m.forEach((k, v) {
      if (v == null || v == false || v is Iterable && v.isEmpty) return;
      if (first) first=false; else sb.write("\n");
      sb..write(k)..write(" ");
      if (v is Iterable) {
        sb..write("[")
        ..writeAll(v, " ")
        ..write("]");
        return;
      }
      sb.write(v);
  });
  sb.write("}");
  return sb.toString();
}

String emitType(DartType t) {
  final lib = t.element?.library?.identifier;
  if (lib != null) libsToDo.add(lib);
  if (t is FunctionType) {
    return M({':type': "\"Function\"",
        ':return-type': emitType(t.returnType),
        ':parameters': t.parameters.map(emitParameter),
        ':type-parameters': t.typeFormals.map(emitTypeParameter)});
  }
  var name = t.displayName;
  final i = name.indexOf("<");
  if (i >= 0) name = name.substring(0, i);
  final isParam = t is TypeParameterType;
  return M({':type': "\"${name}\"",
      ':nullable': t.isDartCoreNull || t.nullabilitySuffix == NullabilitySuffix.question,
      ':is-param': isParam,
      ':lib': (isParam || lib == null ? null : "\"$lib\""),
      ':type-parameters': t is ParameterizedType ? t.typeArguments.map(emitType) : null
  });
}

String emitTypeParameter(TypeParameterElement tp) =>
M({':name': "\"${tp.displayName}\"",
    ':bound': fnil(emitType,tp.bound,null)});

String emitParameter(ParameterElement p) {
  final name = p.displayName;
  return M({":name": name.isEmpty ? null : name, ":kind": p.isNamed ? ':named' : ':positional', ':type': emitType(p.type), ':optional': p.isOptional});
}

class TopLevelVisitor extends ThrowingElementVisitor {
  void visitImportElement(ImportElement e) {
    print(e.displayName);
    throw "coucou";
  }

  void visitClassElement(ClassElement e) {
    print("\"${e.displayName}\"");
    Map<String,dynamic> classData =
    {':kind': ':class',
      ':type-parameters': e.typeParameters.map(emitTypeParameter),
      ':super': fnil(emitType,e.supertype,null),
      ':mixins': e.mixins.map(emitType),
      ':interfaces': e.interfaces.map(emitType)};
    for(final m in e.methods.where(isPublic)) {
      final name = m.displayName;
      classData["\"${name == '-' && m.parameters.isEmpty ? 'unary-' : name}\""]=
      M({':kind': ':method', ':operator': m.isOperator,
          ':static': m.isStatic,
          ':return-type': emitType(m.returnType),
          ':parameters': m.parameters.map(emitParameter),
          ':type-parameters': m.typeParameters.map(emitTypeParameter)
      });
    }
    for(final c in e.constructors.where(isPublic)) {
      classData["\"${c.displayName}\""]=
      M({':kind': ':constructor',
          ':parameters': c.parameters.map(emitParameter),
          ':type-parameters': c.typeParameters.map(emitTypeParameter)
      });
    }
    for(final f in e.fields.where(isPublic)) {
      classData["\"${f.displayName}\""]=
      M({':kind': ':field',
          ':static': f.isStatic,
          ':getter': f.getter!=null,
          ':setter': f.setter!=null,
          ':type': emitType(f.type)
      });
    }
    print(M(classData));
  }
  void visitPropertyAccessorElement(PropertyAccessorElement e) {
    print("; getter/setter ${e.displayName}");
  }
  void visitTopLevelVariableElement(TopLevelVariableElement e) {
    print("; var ${e.displayName}");
  }
  void visitTypeAliasElement(TypeAliasElement e) {
    print("; typedef ${e.displayName}");
  }
  void visitFunctionElement(FunctionElement e) {
    print("; function ${e.displayName} ${e.type.typeFormals}");
  }
  void visitExtensionElement(ExtensionElement e) {
    print("; extension ${e.displayName}");
  }
}

// void main() async {
//   final entity = File("dummy.dart");
//   final resourceProvider = OverlayResourceProvider(PhysicalResourceProvider.INSTANCE);
//   //resourceProvider.setOverlay(entity.absolute.path, content: "", modificationStamp:0);
//   var path = "/Users/baptistedupuch/Projects/flutter/flutter/packages/flutter/lib";
//   var p = resourceProvider.pathContext;
//   print(p.isAbsolute(path));
//   print(p.normalize(path));
//   final collection = AnalysisContextCollection(
//     // includedPaths: ["/Users/baptistedupuch/Projects/flutter/flutter/packages/flutter/lib/foundation.dart",
//     //   "/Users/baptistedupuch/Projects/flutter/flutter/packages/flutter/lib/widgets.dart"
//     // ],
//     includedPaths: ["/Users/baptistedupuch/Projects/flutter/flutter/packages/flutter/lib"],
//     resourceProvider: resourceProvider);
//   //libsToDo.add("dart:core");
//   String basePath = "file:///Users/baptistedupuch/Projects/flutter/flutter/packages/flutter/lib/";
//   //libsToDo.add(basePath + "/foundation.dart");
//   //libsToDo.add(basePath + "widgets.dart");
//   libsToDo.add("package:flutter/widgets.dart");
//   // libsToDo.add("file://" + entity.absolute.path);
//   //print(resourceProvider.getFile(basePath + "widgets.dart").toUri());
//   print("{"); // open 2
//   //var f = "file:///Users/baptistedupuch/Projects/flutter/flutter/packages/flutter/lib/src/material/tooltip_theme.dart";
//   //print(p.fromUri(f)); /// Winner
//   var f = "/Users/baptistedupuch/Projects/flutter/flutter/packages/flutter/lib/src/material/tooltip_theme.dart";
//   // print(p.toUri(f)); /// WINNER

//   // for (final context in collection.contexts) {
//   //   final currentSession = context.currentSession;
//   //   print(context.contextRoot.analyzedFiles().toList());
//   //   // retourne sous cette forme /Users/baptistedupuch/Projects/flutter/flutter/packages/flutter/lib/src/widgets/sliver_fill.dart
//   //   while(libsToDo.isNotEmpty) {
//   //     //final libraryElementResult = await currentSession.getLibraryByUri2(lib);

//   //     final lib = libsToDo.first;
//   //     libsToDo.remove(lib);
//   //     if (libsDone.contains(lib)) continue;
//   //     libsDone.add(lib);

//   //     final libraryElementResult = await currentSession.getLibraryByUri("file:///Users/baptistedupuch/Projects/flutter/flutter/packages/flutter/lib/src/material/tooltip_theme.dart");
//   //     final libraryElement = (libraryElementResult as LibraryElementResult).element;

//   //     print(libraryElement.identifier);
//   //     print("\"${lib}\" {"); // open 1
//   //     // for (final top in libraryElement.topLevelElements) {
//   //     //   if (top.isPublic) top.accept(TopLevelVisitor());
//   //     // }
//   //     // for (final l in libraryElement.exportedLibraries) {
//   //     //   for (final top in l.topLevelElements) {
//   //     //     if (top.isPublic) top.accept(TopLevelVisitor());
//   //     //   }
//   //     // }
//   //     print("}"); // close 1
//   //   }
//   // }
//   print("}"); // close 2
// }

void main() async {
  final resourceProvider = OverlayResourceProvider(PhysicalResourceProvider.INSTANCE);
  var ctx = resourceProvider.pathContext;
  Uri projectDirectoryUri = Directory.current.uri;
  final collection = AnalysisContextCollection(
    includedPaths: [ctx.normalize(projectDirectoryUri.path)],
    resourceProvider: resourceProvider
  );
  final includedDependenciesPaths = <String>[];
  for (final context in collection.contexts) {
    final currentSession = context.currentSession;
    var packageFileJsonString = context.contextRoot.packagesFile?.readAsStringSync();
    // TODO throw when package file does not exist
    if (packageFileJsonString != null) {
      var jsonContent = json.decode(packageFileJsonString);
      for (final jc in jsonContent["packages"]) {
        var rootUri = jc["rootUri"];
        if (jc.containsKey("packageUri")) {
          rootUri = ctx.join(rootUri, jc["packageUri"]);
        }
        includedDependenciesPaths.add(ctx.normalize(ctx.fromUri(rootUri)));
      };
      // TODO see whether we remove current project or not
      includedDependenciesPaths.removeLast();
    }
  }
  // NOTE : deps analysis
  final collectionDeps = AnalysisContextCollection(
    includedPaths: includedDependenciesPaths as List<String>,
    resourceProvider: resourceProvider
  );
  print("{");
  for (final context in collectionDeps.contexts) {
    final currentSession = context.currentSession;
    var f = context.contextRoot.analyzedFiles().toList();
    for (final pathFile in f) {
      var p = ctx.toUri(pathFile).toString();
      final libraryElementResult = await currentSession.getLibraryByUri(p);
      if (libraryElementResult is LibraryElementResult) {
        final libraryElement = (libraryElementResult as LibraryElementResult).element;
        if (libsDone.contains(libraryElement.identifier)) continue;
        libsDone.add(libraryElement.identifier);
        print("\"${libraryElement.identifier}\" {"); // open 1
        for (final top in libraryElement.topLevelElements) {
          if (top.isPublic) top.accept(TopLevelVisitor());
        }
        // for (final l in libraryElement.exportedLibraries) {
        //   // TODO : only take show (or hide)
        //   for (final top in l.topLevelElements) {
        //     if (top.isPublic) top.accept(TopLevelVisitor());
        //   }
        // }
        for (final ex in libraryElement.exports) {
          for (final comb in ex.combinators) {
            if (comb is ShowElementCombinator) {
              print(comb.shownNames);

            }
          }
          if (ex.exportedLibrary != null) {
            print(ex.exportedLibrary?.identifier);
            print(libraryElement.identifier);
          }
        }
        throw "aa";
        print("}"); // close 1
      }
    }
  }
  print("}"); // close 2
}
