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
