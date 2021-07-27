# Differences with Clojure

This document is modeled after [ClojureScript's one](https://clojurescript.org/about/differences).

## Rationale

The rationale for ClojureDart is much the same as for Clojure, with Dart in the role of platform to [create native apps (mobile and desktop)](https://dart.dev/overview#platform).

## State and Identity

Same as Clojure. Clojure’s identity model is simpler and more robust than mutable state, even in message-passing environments.

## Dynamic Development

WIP: currently Dart's hot reload provide some dynamic development experience but the end goal is to hav a working REPL.

## Functional Programming

ClojureDart has the same immutable persistent collections as Clojure on the JVM.

## Lisp

Same as in Clojure.

WIP: as long as Clojuredart is not self-hosted, macros will be evaluated as Clojure/JVM code.

## Runtime Polymorphism

ClojureDart protocols have the same semantics as Clojure protocols.

## Concurrent Programming

Clojure’s model of values, state, identity, and time is valuable even in message-passing environments.

 * Atoms and volatiles work as in Clojure
 * No Agents, Refs nor STM
 * TODO: binding
 * Vars
   * not reified at runtime
   * def produces ordinary Dart top-levels

## Compiles to Dart

ClojureDart compiles cljd to Dart and then Dart compiles to either Dart VM (for development), to native (for deployment) or to JavaScript (for web target).

## Getting Started

TODO See Quick Start

## The Reader
* Numbers
  * ClojureDart currently only supports integer and floating point literals that map to Dart int and double types.
  * Dart ints are a bit special as their width varies between targets (js vs native+vm).
  * Ratio, BigDecimal, and BigInteger literals are currently not supported
  * Equality on numbers works like JavaScript, not Clojure: (= 0.0 0) ⇒ true (TODO is this the behavior we want?)
* Characters
  * Dart does not have character literals. Characters are single-codeunit strings like in ClojureScript. However Dart strings offer proper access to codepoints through its runes property.
  * Because there is no character type in Dart, \ produces a single-codeunit string.
* Dart lists literals `#dart [x y]`, `#dart ^:fixed [x y]`, `#dart ^int [x y]`
* Type-parametrized symbols: `#/(sym u v)` is read as `^{:type-params [u v]} sym` and is equivalent to Dart `sym<u,v>` so they are generally used for type hints and more rarely for method names whose parameters can't be inferred.

## Evaluation

* No local-clearing

## Special forms

* `dart/is?`: Dart being less dynamic than Java, ClojureDart lacks the `instance?` function. Instead it offers the `dart/is?` special form which takes the type as a literal argument (it can't be the result of an expression) like in `(dart/is? x String)`.
* TODO `dart/await`

## Macros

## Other functions

* no `instance?` function, see special `dart/is?`

## Data Structures

## Seqs

## Protocols

* protocols can be extended to the pseudo-class `fallback`. `fallback` differs from ClojureScript's `default` or Clojure's `Object` in that `satisfies?` does return `false` for objects which would get through the fallback extension.

## Metadata

Same as Clojure.

## Namespaces

* Namespaces are not reified.
* Unlike in Clojure there's no strict separation between requiring a namespace and importing a Dart lib (resp. Java package).
  * In addition to symbols to denote Clojure namespaces, `:require` and `:use` accept strings to identify Dart libs. `:refer`, `:only`, `:as` and `:rename` work with Dart libs too.
  * `:import` works too but it's recommended to stick to `:require` and `:use`.


## Refs and Transactions

Refs and transactions are not currently supported.

## Agents

Agents are not currently supported.

## Atoms

Atoms work as in Clojure.

## Host interop

* Property access: property names must be prefixed by `-` for example `(.-iterator obj)`.
* Optional parameters: Dart functions and methods can have optional parameters, either positional or named.
  * Call sites:
    * positional optionals: nothing special; for example calling the [fillRange](https://api.dart.dev/stable/2.12.4/dart-core/List/fillRange.html) method is just `(.fillRange some-list 5 6 "filler")`
    * named optionals: add a `.&` before the first named argument `(List/filled 3 "filler" .& :growable true)`
  * Declarations (only for methods or single-arity fns):
    * positional optionals: add `...` before the first optional parameter like this: `(fn [a b ... c d])`,
    * named optionals: add `.&` before the first optional parameter like this: `(fn [a b .& c d])`,
    * in both cases the optional parameters list is a mixed list where each symbol may be followed by a default value: `(fn [a b .& c 42 d])` here `c` will defaults to 42.

### Type hints
* `^some` is a pseudo-type which means "`Object?` but not `bool`" it helps simplifying boolean conversion in just a null check.
* In Dart, types are not nullable by default. If you type-hint something that can be null you should add a question mark at the end of the type symbol.
  * `List` is the type of a non-nullable list of dynamic values (`List` in Dart)
  * `#/(List String)` is the type of non-nullable list of non-nullable strings (`List<String>`)
  * `#/(List String?)` is the type of a non-nullable list of strings or nils (`List<String?>`)
  * `#/(List? String?)` is the the nullable type of a list of strings or nils (`List<String?>?`)
  * `#/(List? String)` is the nullable type of a list of non-nullable strings (`List<String>?`)
