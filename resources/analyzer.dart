import 'dart:io';
import 'dart:convert';
import 'dart:async';
import 'package:analyzer/dart/analysis/analysis_context_collection.dart';
import 'package:analyzer/dart/analysis/results.dart';
import 'package:analyzer/file_system/physical_file_system.dart';
import 'package:analyzer/file_system/overlay_file_system.dart';
import 'package:analyzer/dart/element/visitor.dart';
import 'package:analyzer/dart/element/element.dart';
import 'package:analyzer/dart/element/type.dart';
import 'package:analyzer/dart/element/nullability_suffix.dart';
import 'package:path/path.dart';

bool isPublic(Element e) => e.isPublic;

R fnil<R, E>(R f(E e), E? x, R fallback) => x != null ? f(x) : fallback;

String M(Map<String, dynamic> m) {
  final sb = StringBuffer("{");
  var first = true;
  m.forEach((k, v) {
    if (v == null || v == false || v is Iterable && v.isEmpty) return;
    if (first)
      first = false;
    else
      sb.write("\n");
    sb
      ..write(k)
      ..write(" ");
    if (v is Iterable) {
      sb.write("[");
      v.forEach((e) => sb
        ..write(M(e))
        ..write(" "));
      sb.write("]");
      return;
    }
    if (v is Map) {
      sb.write(M(v.cast<String, dynamic>()));
      return;
    }
    sb.write(v);
  });
  sb.write("}");
  return sb.toString();
}

Map<String, dynamic> emitType(LibraryElement rootLib, DartType t) {
  final lib = t.element2?.library?.identifier;
  if (t is FunctionType) {
    return {
      ':element-name': "\"Function\"",
      ":kind": ":function",
      ":qname": "dc.Function",
      ":canon-qname": "dc.Function",
      ":canon-lib": "\"dart:core\"",
      ":lib": "\"dart:core\"",
      ':nullable':
          t.isDartCoreNull || t.nullabilitySuffix == NullabilitySuffix.question,
      ':return-type': emitType(rootLib, t.returnType),
      ':parameters': t.parameters.map((e) => emitParameter(rootLib, e)),
      ':type-parameters':
          t.typeFormals.map((e) => emitTypeParameter(rootLib, e))
    };
  }
  if (t is DynamicType) {
    return {
      ":kind": ":class",
      ":qname": "dc.dynamic",
      ":canon-qname": "dc.dynamic",
      ":canon-lib": "\"dart:core\"",
      ":lib": "\"dart:core\"",
      ":element-name": "\"dynamic\"",
      ":type-parameters": []
    };
  }
  if (t is VoidType) {
    return {
      ":kind": ":class",
      ":qname": "void",
      ":canon-qname": "void",
      ":element-name": "\"void\""
    };
  }
  if (t is NeverType) {
    return {
      ":kind": ":class",
      ":qname": "dc.Never",
      ":canon-qname": "dc.Never",
      ":canon-lib": "\"dart:core\"",
      ":lib": "\"dart:core\"",
      ":element-name": "\"Never\""
    };
  }
  if (t is TypeParameterType) {
    var res = emitTypeParameter(rootLib, t.element2);
    res[":nullable"] =
        t.isDartCoreNull || t.nullabilitySuffix == NullabilitySuffix.question;
    return res;
  }
  var name = t.getDisplayString(withNullability: false);
  final i = name.indexOf("<");
  if (i >= 0) name = name.substring(0, i);
  //if (!isParam || lib != null) addLibIdentifierIfNotContains(libsToDo, lib as String);
  var canonLib = lib == null ? null : "\"${lib}\"";
  var exportingLib = canonLib == null
      ? null
      : (t.element2 == null
          ? null
          : (t.element2!.library == null
              ? null
              : (isExported(rootLib, t.element2!.library!)
                  ? "\"${rootLib.identifier}\""
                  : canonLib)));
  exportingLib = exportingLib ?? canonLib;
  return {
    ':kind': ':class',
    ':element-name': "\"${name}\"",
    ':nullable':
        t.isDartCoreNull || t.nullabilitySuffix == NullabilitySuffix.question,
    ':canon-lib': canonLib,
    ':lib': exportingLib,
    ':canon-qname-placeholder': canonLib == null ? null : true,
    ':type-parameters': t is ParameterizedType
        ? t.typeArguments.map(((e) => (emitType(rootLib, e))))
        : null
  };
}

Map<String, dynamic> emitTypeParameter(
    LibraryElement rootLib, TypeParameterElement tp) {
  var f = (t) => emitType(rootLib, t);
  return {
    ":kind": ":class",
    ":element-name": "\"${tp.displayName}\"",
    ":qname": "${tp.displayName}",
    ":canon-qname": "${tp.displayName}",
    ":is-param": true,
    ":bound": fnil(f, tp.bound, null)
  };
}

Map<String, dynamic> emitParameter(LibraryElement rootLib, ParameterElement p) {
  final name = p.displayName;
  return {
    ":name": name.isEmpty ? null : name,
    ":kind": p.isNamed ? ':named' : ':positional',
    ':type': emitType(rootLib, p.type),
    ':optional': p.isOptional
  };
}

bool isExported(LibraryElement rootLib, LibraryElement l) =>
    rootLib.exportedLibraries.contains(l);

class TopLevelVisitor extends ThrowingElementVisitor<Map<String, dynamic>> {
  final LibraryElement rootLib;
  TopLevelVisitor(this.rootLib);

  //void visitImportElement(ImportElement e) {
//    print(e.displayName);
  //  throw "coucou";
//  }

  Map<String, dynamic> _visitInterfaceElement(InterfaceElement e) {
    var f = (t) => emitType(rootLib, t);
    Map<String, dynamic> classData = {
      ':kind': ':class',
      ':canon-qname-placeholder': true,
      ':canon-lib': '"${e.library.identifier}"',
      ':lib': '"${rootLib.identifier}"',
      ':private': e.isPrivate,
      ':internal': e.hasInternal,
      ':type-parameters':
          e.typeParameters.map((e) => emitTypeParameter(rootLib, e)),
      ':super': fnil(f, e.supertype, null),
      ':mixins': e.mixins.map(((e) => (emitType(rootLib, e)))),
      ':interfaces': e.interfaces.map(((e) => (emitType(rootLib, e)))),
    };
    if (e is MixinElement)
      classData[':on'] =
          e.superclassConstraints.map(((e) => (emitType(rootLib, e))));
    for (final m in e.methods.where(isPublic)) {
      final name = m.displayName;
      classData[
          "\"${name == '-' && m.parameters.isEmpty ? 'unary-' : name}\""] = {
        ':kind': ':method',
        ':operator': m.isOperator,
        ':static': m.isStatic,
        ':return-type': emitType(rootLib, m.returnType),
        ':parameters': m.parameters.map((e) => emitParameter(rootLib, e)),
        ':type-parameters':
            m.typeParameters.map((e) => emitTypeParameter(rootLib, e))
      };
    }
    for (final c in e.constructors.where(isPublic)) {
      classData["\"${c.displayName}\""] = {
        ':kind': ':constructor',
        ':named': !c.isDefaultConstructor,
        ':const': c.isConst,
        ':return-type': emitType(rootLib, c.returnType),
        ':parameters': c.parameters.map((e) => emitParameter(rootLib, e)),
        ':type-parameters':
            c.typeParameters.map((e) => emitTypeParameter(rootLib, e))
      };
    }
    for (final f in e.fields.where(isPublic)) {
      classData["\"${f.displayName}\""] = {
        ':kind': ':field',
        ':static': f.isStatic,
        ':const': f.isConst,
        ':getter': f.getter != null,
        ':setter': f.setter != null,
        ':type': emitType(rootLib, f.type)
      };
    }
    return classData;
  }

  Map<String, dynamic> visitClassElement(ClassElement e) {
    return this._visitInterfaceElement(e);
  }

  Map<String, dynamic> visitPropertyAccessorElement(PropertyAccessorElement e) {
    return {
      ':kind': ':field',
      ':canon-qname-placeholder': true,
      ':canon-lib': '"${e.library.identifier}"',
      ':lib': '"${rootLib.identifier}"',
      ':const': e.variable.isConst,
      ':getter': e.variable.getter != null,
      ':setter': e.variable.setter != null,
      ':type': emitType(rootLib, e.variable.type)
    };
  }

  Map<String, dynamic> visitTopLevelVariableElement(TopLevelVariableElement e) {
    return {
      ':kind': ':field',
      ':canon-qname-placeholder': true,
      ':canon-lib': '"${e.library.identifier}"',
      ':lib': '"${rootLib.identifier}"',
      ':const': e.isConst,
      ':getter': e.getter != null,
      ':setter': e.setter != null,
      ':type': emitType(rootLib, e.type)
    };
  }

  Map<String, dynamic> visitMixinElement(MixinElement e) {
    return this._visitInterfaceElement(e);
  }

  Map<String, dynamic> visitEnumElement(EnumElement e) {
    return this._visitInterfaceElement(e);
  }

  // void visitTypeAliasElement(TypeAliasElement e) {
  //   print("; typedef ${e.displayName}");
  // }

  Map<String, dynamic> visitFunctionElement(FunctionElement e) {
    return {
      ':kind': ':function',
      ':canon-qname-placeholder': true,
      ':canon-lib': '"${e.library.identifier}"',
      ':lib': '"${rootLib.identifier}"',
      ':parameters': e.parameters.map((e) => emitParameter(rootLib, e)),
      ':return-type': emitType(rootLib, e.returnType),
      ':type-parameters':
          e.typeParameters.map((e) => emitTypeParameter(rootLib, e))
    };
  }

  // void visitExtensionElement(ExtensionElement e) {
  //   print("; extension ${e.displayName}");
  // }
}

void main(args) async {
  final resourceProvider =
      OverlayResourceProvider(PhysicalResourceProvider.INSTANCE);
  var pathContext = resourceProvider.pathContext;
  final String sep = pathContext.separator;
  late Directory dir;
  if (args.isEmpty)
    dir = Directory.current;
  else
    dir = Directory(args.first);
  Uri projectDirectoryUri = dir.uri;

  final coll = AnalysisContextCollection(
      includedPaths: [
        pathContext.normalize(projectDirectoryUri.path)
      ],
      excludedPaths: [
        "${pathContext.normalize(projectDirectoryUri.path)}${sep}lib${sep}cljd-out",
        "${pathContext.normalize(projectDirectoryUri.path)}${sep}lib${sep}cljd-out${sep}analyzer_pure_lib.dart"
      ],
      resourceProvider: resourceProvider,
      sdkPath: dirname(dirname(Platform.resolvedExecutable)));
  // (-> io/Platform.resolvedExecutable path/dirname path/dirname)
  await doesLibraryExist(resourceProvider, coll, "dart:core");
  await doesLibraryExist(resourceProvider, coll, "dart:async");
  await for (final line in stdin.transform(utf8.decoder).transform(const LineSplitter())) {
    final tokens = line.split(" ");
    switch(tokens[0]) {
      case "lib":
      var res = await doesLibraryExist(resourceProvider, coll, tokens[1]!);
      if (res) {
        print(true);
      } else {
        print("nil");
      }
      break;
      case "elt":
      var elem = await retrieveElement(resourceProvider, coll,
          projectDirectoryUri, tokens[1]!, tokens[2]!);
      if (elem != null)
        print(M(elem));
      else
        print("nil");
      break;
    }
  }
}

Future<Map<String, dynamic>?> retrieveElement(
    OverlayResourceProvider resourceProvider,
    AnalysisContextCollection coll,
    Uri projectDirectoryUri,
    String lib,
    String element,
    [bool reload = true]) async {
  var pathContext = resourceProvider.pathContext;
  final String sep = pathContext.separator;
  final String filePath = pathContext.normalize(
      "${projectDirectoryUri.path}${sep}lib${sep}cljdfuzzysearch.dart");
  resourceProvider.setOverlay(filePath,
      content: "import '${lib}' as libalias;\n",
      modificationStamp: DateTime.now().millisecondsSinceEpoch);
  var context = coll.contextFor(filePath);
  context.changeFile(filePath);
  await context.applyPendingFileChanges();
  var session = context.currentSession;
  var result =
      await session.getLibraryByUri(pathContext.toUri(filePath).toString());
  if (result is LibraryElementResult) {
    var rootLib = result.element.importedLibraries.first;
    // when rootLib is a local file under .../lib/myfile.dart , reload it everytime
    if (reload &
        isWithin(pathContext.normalize(projectDirectoryUri.path),
            pathContext.normalize(rootLib.librarySource.toString()))) {
      rootLib.session.analysisContext
          .changeFile(pathContext.normalize(rootLib.librarySource.toString()));
      await rootLib.session.analysisContext.applyPendingFileChanges();
      var res = await retrieveElement(
          resourceProvider, coll, projectDirectoryUri, lib, element, false);
      if (res != null) res[":local-lib"] = true;
      return res;
    }
    var e = rootLib.exportNamespace.get(element);
    if (e != null) {
      var res = e.accept(TopLevelVisitor(rootLib));
      if (res != null) {
        res[":element-name"] = "\"${element}\"";
        res[":toplevel"] = true;
        res[":canon-qname-placeholder"] = true;
        return res;
      }
    }
  }
  return null;
}

Future<bool> doesLibraryExist(OverlayResourceProvider resourceProvider,
    AnalysisContextCollection coll, String lib) async {
  var pathContext = resourceProvider.pathContext;
  final String sep = pathContext.separator;
  final String filePath = pathContext
      .normalize("${pathContext.current}${sep}lib${sep}cljdfuzzysearch.dart");
  resourceProvider.setOverlay(filePath,
      content: "import ${lib} as fuzzyalias;\n",
      modificationStamp: DateTime.now().millisecondsSinceEpoch);
  var context = coll.contextFor(filePath);
  context.changeFile(filePath);
  await context.applyPendingFileChanges();
  var session = context.currentSession;
  var errors = await session.getErrors(filePath);
  var analysisErrors = (errors as ErrorsResult).errors;
  if (analysisErrors.isEmpty ||
      (analysisErrors.first.errorCode.uniqueName ==
          "CompileTimeErrorCode.URI_DOES_NOT_EXIST")) {
    return false;
  }
  return true;
}
