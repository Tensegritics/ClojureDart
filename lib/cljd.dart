import 'dart:collection';

int hashCombine(int seed, int hash) {
  seed ^= hash + 0x9e3779b9 + (seed << 6) + (seed >> 2);
  return seed;
}

class Symbol {
  final String namespace;
  final String name;
  const Symbol._internal(this.namespace, this.name);
  const factory Symbol(String namespace, String name) = Symbol._internal;
  String toString() { return namespace != null ? "$namespace/$name" : "$name"; }
  bool operator == (other) => other is Symbol && namespace == other.namespace && name == other.name;
  int get hashCode {
    var h = 775073750;
    hashCombine(h, namespace.hashCode);
    hashCombine(h, name.hashCode);
    return h;
  }
}

class Keyword {
  final String namespace;
  final String name;
  const Keyword._internal(this.namespace, this.name);
  const factory Keyword(String namespace, String name) = Keyword._internal;
  String toString() { return namespace != null ? ":$namespace/$name" : ":$name"; }
  bool operator == (other) => other is Keyword && namespace == other.namespace && name == other.name;
  int get hashCode {
    var h = 2030774975;
    hashCombine(h, namespace.hashCode);
    hashCombine(h, name.hashCode);
    return h;
  }
}

class PersistentVector<E> extends ListMixin<E> {
  List<E> _v;
  E operator [](int i) => _v[i];
  operator []=(int i, E _) => throw new UnsupportedError("Cannot add to an unmodifiable list");

  int get length => _v.length;
  set length(int _) => throw new UnsupportedError("Cannot add to an unmodifiable list");

  PersistentVector(this._v);
  PersistentVector.from(this._v);
  PersistentVector.empty() : this([]);
}


enum _Argument { missing }
const MISSING_ARG = _Argument.missing;
