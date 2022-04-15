# Differences with Clojure

## Dart

ClojureDart targets [Dart](https://dart.dev/) (surprise!) and, through Dart, [Flutter](https://flutter.dev/) a GUI framework for mobile, desktop and web.

Dart has three compilation targets:
 * its own VM which is mostly used at dev time because it allows for more tooling,
 * native code,
 * javascript.

## Missing features
 * [REPL](https://github.com/Tensegritics/ClojureDart/issues/6)
 * [multimethods](https://github.com/Tensegritics/ClojureDart/issues/3)
 * [sorted collections](https://github.com/Tensegritics/ClojureDart/issues/4)

## Divergent features
### ns, :require, :use and :import
In ClojureDart `:require` and `:use` supersedes `:import` and thus `:import` is rarely used.

To use a Dart library, just put its URI as a string in lieu of the symbol referring to a namespace. Then you can use `:as`, `:refer`, :rename` as with a regular Clojure(Dart) namespace.

```clj
(ns acme.main
  (:require ["package:flutter/material.dart" :as m :refer [Colors]]))
```

Like in Clojurescript "Naked `:use`" is not supported: you must always provide a `:only` list.

### Protocols
Unlike Clojure and like Clojurescript, ClojureDart is extensively based on protocols.

Like Clojure default extensions are provided by extending to `Object` and/or `Null`.

However instead of extending to `Object` or `Null`, it's often preferable to extend to the `fallback` pseudotype which has two distinctive qualities:
 * it has a lower priority than other extensions,
 * `satisfies?` returns `false` for objects which use a fallback implementation.

## try/catch

In Dart, when one catch an exception, the stacktrace isn't attached to the exception. Thus in ClojureDart if you want to capture the stacktrace you have to specify an extr name after the exception name in catch:

```clj
(try
  ...
  (catch io/HttpException e ; no stack trace binding
    ...)
  (catch Exception e st ; stack trace binding
    ...))
```

When porting some Dart code you may encounter "catch-alls": `catch` clauses with no type. They are syntactic sugar for the `dynamic` type, so in ClojureDart you would write:

```clj
(try
  ...
  (catch dynamic e
    ...))
```

## Macros
Until ClojureDart is self-hosted macros will be a bit special: they are evaluated on the JVM so if they need some support functions from your namespace then these functions must be tagged with `^:macro-support` to also be available to macros.

To be clear we are talking about cases like this:

```clj
(defn ^:macro-support do-expand [expr] ...)
(defmacro my-macro [expr]
  (do-expand expr))
```

And not like that:
```clj
(defmacro my-macro [& body]
  `(my-fn (fn [] ~@body))) ; it's ok, nothing special to do
```

## Interop
### Member names as strings
Dart considers operators calls to be syntactically sweetened methods calls (`a+b` is going to call the `+` method on the object `a` with argument `b`).

It follows that `(.+ a b)` or `(. a + b)` are valid ClojureDart expresions.

However while Dart is very conservative in which characters can appear in an identifier (`a-zA-Z0-9$_`) its operators names are not all valid Clojure symbols, for example: `^`, `[]`, `[]=`, `~/` ...

To work around this issue, **member names are allowed to be strigs**: `(. a "[]=" i v)` is the ClojureDart equivaelent of `a[i]=v`.

This also applies when implementing operators in `reify`, `deftype` or `defrecord`.

### Static members and libs aliases

When it comes to referring to classes **in Clojure** either you have imported the class and you can refer to it by its unqualified name (e.g. `Thread`) or you refer to it using it's fully qualified name (e.g. `java.io.File`).

**In ClojureDart** since lib names are URIs they usually don't make for legal symbols thus to refer to a class (or any toplevel of a lib) you either `:refer` it and use its unqualified name (e.g. `Future`) or you refer to it with the lib alias (e.g. `io/HttpException`).

However when you want to access a static member **in Clojure** you would write `(Thread/currentThread)` for a static method or `java.nio.charset.StandardCharsets/UTF_8` for a static field.

**In ClojureDart** you write `(painting.EdgeInsets/only :left 16)` for a static method and `material.InputBorder/none` for a static property. Note that in thes cases the alias and the class name are concatenated to make the namespace of the symbol.

### reify/deftype
#### `^:abstract`
**`deftype`**
A type name can have the `:abstract` metadata to indicate the generated class to be asbtract.

#### `:extends`
**`reify` and `deftype`**
One can derive from a super class by specifying a class (with a no-arg constructor) or a constructor expression. For `deftype` only fields can be used in the constructor expression.

```clj
(reify
  :extends material/StatelesWidget
  (build [_ ctx] ...))
```

#### `:type-only`
**deftype**
The `:type-only` option instructs `deftype` to not create factory function (`->MyType`).

#### `^:mixin`
**`reify`, `defrecord` and `deftype`**
This metadata on implemented classes specify these classes should be considered [mixins](https://dart.dev/guides/language/language-tour#adding-features-to-a-class-mixins) and not [interfaces](https://dart.dev/guides/language/language-tour#implicit-interfaces).

#### `^:getter`/`^:setter`
**`reify`, `defrecord` and `deftype`**
Method names can be tagged with `:getter` and/or `:setter` if the method is in fact a [property](https://dart.dev/guides/language/language-tour#getters-and-setters).

For a getter you must provide a 1-arg arity of the method (`[this]`) and for a setter a 2-arg arity (`[this new-value]`).

#### Calling `super`
When you must call the `super` implementation (since one can now extends a super type) you have to add metadata on the "this" parameter of a method. For example when implementing a [State](https://api.flutter.dev/flutter/widgets/State/initState.html) one can write:

```clj
(initState [^{:super papa} self]
  (.initState papa) ; here papa refers to super
  ...
  nil)
```

### Tests
Tests written with `cljd.test` can be run with `dart test` (or `flutter test`).

## Specific features
 * named parameters
 * dartlit
 * ^some
 * nullability
 * generics
 * async/await + dynamic bindings
 * dart/is
 * magicast
