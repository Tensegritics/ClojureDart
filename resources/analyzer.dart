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

final Map<String, String> packages = {};

String libPathToPackageName(String path) {
  if (packages[path] != null) return packages[path] as String;
  for (final pname in packages.keys) {
    if (path.startsWith(pname)) {
      String pack = packages[pname] as String;
      String result = path.replaceRange(0, pname.length, pack);
      packages[path] = result;
      return result;
    }
  }
  return path;
}

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
  //if (!isParam || lib != null) addLibIdentifierIfNotContains(libsToDo, lib as String);
  return M({':type': "\"${name}\"",
      ':nullable': t.isDartCoreNull || t.nullabilitySuffix == NullabilitySuffix.question,
      ':is-param': isParam,
      ':lib': (isParam || lib == null ? null : "\"${libPathToPackageName(lib)}\""),
      ':type-parameters': t is ParameterizedType ? t.typeArguments.map(emitType) : null
  });
}

String emitTypeParameter(TypeParameterElement tp) =>
M({':type': "\"${tp.displayName}\"",
    ':is-param': true,
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
      ':lib': '"${libPathToPackageName(e.library.identifier)}"',
      ':private': e.isPrivate,
      ':internal': e.hasInternal,
      ':const': e.unnamedConstructor?.isConst,
      ':type-parameters': e.typeParameters.map(emitTypeParameter),
      ':super': fnil(emitType,e.supertype,null),
      ':mixins': e.mixins.map(emitType),
      ':interfaces': e.interfaces.map(emitType),
      ':on': e.superclassConstraints.map(emitType)};
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
          ':return-type': emitType(c.returnType),
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
    print("\"${e.displayName}\"");
    print(M({':kind': ':field',
             ':const': e.isConst,
             ':getter': e.getter!=null,
             ':setter': e.setter!=null,
             ':type': emitType(e.type)
    }));
  }
  void visitTypeAliasElement(TypeAliasElement e) {
    print("; typedef ${e.displayName}");
  }
  void visitFunctionElement(FunctionElement e) {
    print("\"${e.displayName}\"");
    Map<String,dynamic> classData =
    {':kind': ':function',
      ':lib': '"${libPathToPackageName(e.library.identifier)}"',
      ':parameters': e.parameters.map(emitParameter),
      ':return-type': emitType(e.returnType),
      ':type-parameters': e.typeParameters.map(emitTypeParameter)};
    print(M(classData));
  }
  void visitExtensionElement(ExtensionElement e) {
    print("; extension ${e.displayName}");
  }
}

Future<void> analyzePaths (session, List<String> paths) async {
  for (final p in paths) {
    final libraryElementResult = await session.getLibraryByUri(p);
    if (!libsDone.add(p)) continue;
    if (libraryElementResult is LibraryElementResult) {
      final libraryElement = (libraryElementResult as LibraryElementResult).element;
      final packageName = libPathToPackageName(libraryElement.identifier);
      if (packageName != p) {
        libsToDo.add(packageName);
        continue;
      }
      print("\"$p\" {"); // open 1
      for (final top in libraryElement.topLevelElements) {
        if (top.isPublic) top.accept(TopLevelVisitor());
      }
      var exs = libraryElement.exports;
      if (exs.isNotEmpty) {
        print(":exports [");
        for (final ex in libraryElement.exports) {
          if (ex.exportedLibrary != null) {
            var n = ex.exportedLibrary?.identifier as String;
            libsToDo.add(n);
            for (final comb in ex.combinators) {
              if (comb is ShowElementCombinator) {
                print(M({":lib": "\"${libPathToPackageName(n)}\"", ':shown': comb.shownNames.map((name) => "\"${name}\"").toList()}));
              }
              if (comb is HideElementCombinator) {
                print(M({":lib": "\"${libPathToPackageName(n)}\"", ':hidden': comb.hiddenNames.map((name) => "\"${name}\"").toList()}));
              }
            }
            if (ex.combinators.isEmpty) {
              print(M({":lib": "\"${n}\""}));
            }
          }
        }
        print("]");
      }
      print(":private ${libraryElement.isPrivate}");
      print(":internal ${libraryElement.hasInternal}");
      print("}"); // close 1
    }
  }
}

void main(args) async {
  final resourceProvider = OverlayResourceProvider(PhysicalResourceProvider.INSTANCE);
  var ctx = resourceProvider.pathContext;
  late Directory dir;
  if (args.isEmpty) dir = Directory.current;
  else dir = Directory(args.first);
  Uri projectDirectoryUri = dir.uri;
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
        packages[rootUri] = 'package:' + jc["name"] + '/';
        String normalizedPath = ctx.normalize(ctx.fromUri(rootUri));
        if (!ctx.isRelative(normalizedPath)) {
          includedDependenciesPaths.add(normalizedPath);
        } else {
          stderr.write("WARNING: could not analyze '" + jc["name"] + "' package \n");
          // TODO: better message on consequences
        }
      };
    }
  }
  // NOTE : deps analysis
  final collectionDeps = AnalysisContextCollection(
    includedPaths: includedDependenciesPaths as List<String>,
    resourceProvider: resourceProvider
  );
  print("{"); // open 2
  for (final context in collectionDeps.contexts) {
    final currentSession = context.currentSession;
    var files = context.contextRoot.analyzedFiles().map((n) => ctx.toUri(n).toString()).toList();
    files.removeWhere((p) => ctx.extension(p) != '.dart');
    await analyzePaths(currentSession, files);
  }
  /// Only necessary for dart:* libs
  do {
    var libs = libsToDo.difference(libsDone);
    libsToDo..clear()..addAll(libs);
    await analyzePaths(collection.contexts.first.currentSession, libsToDo.toList());
  }
  while(libsToDo.isNotEmpty);
  print("}"); // close 2
}
