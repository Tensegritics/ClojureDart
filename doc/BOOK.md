# The Book of ClojureDart

## Introduction

### Why ClojureDart?

Because Baptiste Dupuch wanted to do mobile development in Clojure, and Christophe Grand was foolish enough to follow.

More seriously, ClojureDart exists because we love Clojure: its simplicity, its power, its data-first mindset. We don't want to give that up just because we're building apps for phones, tablets, or the web.

Flutter provides an impressive cross-platform UI framework. With a single codebase, you can target Android, iOS, desktop, and even the web (yes, SPAs too). But for Clojure developers, Dart is not exactly a dream language.

ClojureDart bridges this gap. It lets you write idiomatic Clojure code while building high-performance Flutter apps. You get to keep your functional programming model, immutable data structures, and macros, while taking advantage of Flutter's rich widget ecosystem and smooth rendering.

This isn't about rewriting Flutter or replacing Dart. It's about giving Clojure developers a way to build modern apps *without switching mental models*. If you’ve ever dreamed of calling `(map inc xs)` inside your UI logic, or threading state updates with `->` instead of managing callbacks and setState, this is for you.

ClojureDart is both pragmatic and expressive. It's a way to stay in the language you love while building apps that run anywhere.

### Who this book is for

This book is for Clojure or ClojureScript developers who want to build mobile (desktop, web...) apps without leaving their Clojure reasoning and modeling skills behind.

You’ve probably looked at Flutter and thought: “Looks not bad, but Dart?” Or maybe you’ve written native apps before and missed Lisp.

This book doesn’t teach Clojure or Flutter from scratch. It shows how to use what you already know to build real apps with ClojureDart, in a way that stays true to the core Clojure mindset.

### What you’ll build and learn

TODO Provide a brief overview of the types of apps readers will build (e.g. task managers, multi-screen apps with data fetching) and the skills they’ll develop, such as using interop, managing state, structuring UIs, and compiling for release.

## Getting Started

### Project structure overview

A typical ClojureDart project is both a Clojure project (with a `deps.edn` file and everything where you’d expect it) and a Dart project (with a `pubspec.yaml` file and standard Flutter layout).

In Dart projects, source files live under `lib/`, with `lib/src/`  used for internal modules.

The ClojureDart compiler generates `.dart` files under `lib/cljd-out/`. This is where your Clojure code gets compiled to Dart. The directory is added to `.gitignore` by default when initializing a project, as it’s considered generated code.

### Setup and Tooling

TODO

### Hello Flutter World

TODO

## Interop with the Dart world

### Dart is more static than Java

Java, by virtue of the JVM, is more dynamic than it lets on. It has powerful reflection, supports dynamic bytecode injection, and erases generics at runtime — meaning there's no real distinction between, say, a `List<String>` and a `List<Object>` at the JVM level.

Dart, in contrast, leans heavily into static typing. It offers only limited reflection (and only in dev mode), doesn’t support dynamic code loading (outside of hot reload in dev mode), and its generics are reified: a list of strings at compile time is still a list of strings at runtime.

So you might wonder: how can ClojureDart still offer typeless interop in such a static world?

Fortunately, Dart has some dynamic roots, and a few features remain from that era — some useful, some less so:

- The special `dynamic` type tells the compiler to emit method calls even when the receiver’s type is unknown.
- The `runtimeType` field (think Java’s `.getClass()`) exists, but it can be overridden and can't be fully trusted. Combined with Dart’s limited reflection, it makes type comparison unreliable. The only reliable test is `instance?`.
- There's a `noSuchMethod` hook that lets a class catch calls to undefined methods — sort of like `method_missing` in Ruby.

ClojureDart leans heavily on `dynamic` by default. You can think of it as similar to how Clojure uses reflective calls when type information isn’t available. But because Dart is stricter, dynamic calls may sometimes pick the wrong method or behave in surprising ways.

That’s why, unlike `*warn-on-reflection*` in Clojure — which is optional — dynamic warnings in ClojureDart are always on, and you should take them seriously.

If something behaves weirdly, check for dynamic warnings. Don’t ignore them — fix them.

In fact, it’s best not to ship production builds with any dynamic warnings at all.

To enforce that, you can add `:no-dynamic true` to your namespace metadata. This will turn dynamic warnings into hard errors:

```clojure
(ns my.namespace
  "Wonderful core namespace where no dynamic calls are allowed."
  {:no-dynamic true}
  ...)
```

### Squashing "dynamic warnings"

Like reflection warnings in Clojure, dynamic warnings in ClojureDart should not be ignored — and like with reflection, you should *always fix the first one first*.

Why? Because dynamic calls often stem from type inference failures, and those tend to cascade. One missing type hint at the source can cause a whole chain of warnings downstream. Adding a single hint in the right spot might clean up several warnings at once.

So don’t go whack-a-moling from the bottom of the stack. Start at the top, add hints as needed, and you’ll often see multiple warnings disappear together.

### Requiring a Dart lib

Requiring Dart packages in ClojureDart looks just like requiring Clojure namespaces — with one small twist.

Instead of a namespace symbol, you pass a string that represents the Dart import path:

```clojure
(ns my.app
  (:require ["package:flutter/material.dart" :as m]))
```

### Collections

Just like Clojure collections are also Java collections, ClojureDart collections are also Dart collections.

And it goes both ways: Dart collections can be used (in a read-only way) with functions like `get`, `nth`, `seq`, and friends. So you can treat a Dart list much like a Clojure sequence — at least when reading from it.

Now, since Dart generics are not erased (unlike on the JVM), you might wonder: how can a Clojure vector — which can hold values of any type — be used in a place where Dart expects a `List<String>` or `List<Widget>`?

That’s where ClojureDart’s “magicast” kicks in. When the compiler sees that you’re passing a dynamically-typed value to a Dart method expecting a specific type, it automatically inserts checks and type conversions behind the scenes.

Let’s say you’re using Flutter’s `m/Column`, which expects a `.children` argument of type `List<Widget>`. But you have a Clojure vector of widgets — which is a Dart list, yes, but it defaults to being a `List<dynamic>`.

So what happens?

The compiler will check that what you’re passing is indeed a `List`. If it’s not already a `List<Widget>`, it will insert a `.cast<Widget>()` call on it — just like you might do manually in Dart. That way, Dart gets what it expects, and you don’t have to manually cast anything.

Here’s the clever bit: ClojureDart collections can be *cast* to any type. The root object changes, but the underlying structure is preserved and shared. In other words, the collection lies about its element types — and it works, *as long as you’re not lying too hard*.

If the collection claims to be a `List<Widget>`, but actually contains something that’s not a widget, Dart will throw a runtime exception when it tries to access that element.

So back to our `m/Column`: you can safely pass a Clojure vector of widgets as `children`, and it’ll Just Work™ — but only if it’s *really* a list of widgets.

```clojure
(m/Column
  .children
  [(m/Text "Hello")
   (m/Text "Magicast")])
```

### Functions

Simple ClojureDart functions — meaning: no multiple arities, no varargs — are also Dart functions.

That means you can pass them directly to Dart APIs expecting a function, without wrapping or conversion. It just works.

Functions are one of the areas where we’d really like to extend **magicast** in the future. Right now, interop works well with straightforward cases, but adding support for more complex Clojure function shapes (like multi-arity or rest args) would make things a lot smoother.

### Optional parameters (named or positional)

Sometimes interop means you need to implement a Dart function or method that takes optional parameters. Dart has two kinds: named and positional — and ClojureDart has syntax for both.

Here's how it works:

- `[a b c .d .e]`
  → Three required positional parameters (`a b c`) and two *named* optional parameters: `d` and `e`.

- `[a b c ... d e]`
  → Three required positional parameters, followed by two *optional positional* ones: `d` and `e`.

You can also specify default values:

- `[.a 42 .e]`
  → Two named parameters. `a` has a default value of `42`, `e` has no default.

- `[... a 42 b]`
  → Two optional positional parameters. `a` defaults to `42`, `b` has no default.

The dot (`.`) means “named” and the ellipsis (`...`) means “optional positional.” You’ll get used to it.


### Calling instance methods

Calling instance methods in ClojureDart is almost one-to-one with Dart — just with Clojure syntax.

```
obj.methodName(arg1, arg2, ...) // Dart
(.methodName obj arg1 arg2 ...) ; ClojureDart
```

Straightforward, right?

But Dart also has named arguments, and ClojureDart supports them too. The only catch: they have to come after all positional arguments, just like in Dart. You use dotted symbols to specify them:

```
obj.methodName(p1, p2, name3: p3, name4: p4) // Dart
(.methodName obj p1 p2 .name3 p3 .name4 p4) ; ClojureDart
```

It reads cleanly once you know the trick: dots introduce named argument keys.

For those wondering “why not keywords?” — the compiler needs to syntactically tell apart calls with named arguments from calls that happen to pass keywords as regular values. And the dot is already associated with anything interop.

### Accessing instance fields

Getting a property is simple:

```
obj.prop // Dart
(.-prop obj) ; ClojureDart
```

Setting one? Also easy:

```
obj.prop = x // Dart
(.-prop! obj x) ; ClojureDart sugar
(set! (.-prop obj) x) ; classic Clojure style
```

In Dart, properties are more than just fields — they often come with getters and setters behind the scenes. So while Java fields tend to be private and accessed through methods, Dart APIs commonly expose public properties directly.

That’s why (.-prop! obj x) is the preferred idiom in ClojureDart: it’s concise, idiomatic, and plays nicely with doto.

```clojure
(doto (m/Paint)
  (.-color! m/Colors.green)
  (.-style! m/PaintingStyle.stroke))
```

It keeps the code clean and expressive, especially when setting up objects with several properties in a row.

### Object destructuring

As we’ve seen, accessing properties is a big part of working with Dart APIs. So ClojureDart extends Clojure’s usual destructuring forms to support object property access too.

In addition to `:keys`, `:syms`, and `:strs`, you can use `:flds` to destructure fields:

```clojure
(let [{:flds [year month day]} (DateTime.now)]
  ...)
```

In this example, the compiler can infer the type of the object (`DateTime`) from context, so it knows which fields to pull out.

In more dynamic situations — say, if the type isn't obvious — you can give the compiler a hint, either directly on the destructuting map:

```clojure
^DateTime {:flds [year month day]}
```

Or as a hint on the alias within the binding map:

```clojure
{:flds [year month day] :as ^DateTime dt}
```

Either way, this helps the compiler insert the right property lookups safely and efficiently.

In addition to `:flds` you can also write :

```clojure
{y .-year m .-month d .-day}
```

### Tear-off methods

Surprisingly enough, in Dart you can access a method like a field — and what you get is a function that behaves just like the method, except it already knows its receiver (the object it belongs to). This is called a *tear-off*.

That means method calls can be treated like any other function call, which plays very nicely with Clojure’s functional style.

A common example is with the `Completer` class from `dart:async`, which is used to create promise-like futures.

Here's the typical approach:

```clojure
(let [completer (da/Completer)]
  (do something async and call (.complete completer v))
  (await (.-future completer)))
```

But thanks to tear-offs and object destructuring, you can make this cleaner:

```clojure
(let [{:flds [complete future] (da/Completer)]
  (do something async and call (complete v))
  (await future))
```

Much nicer, right? Tear-offs let you treat methods as first-class functions — just another thing to pass around.

### Constructors

In ClojureDart, there's no need to write `(new Object)` or `(Object.)`. Calling the default constructor is as simple as `(ClassName)`.

But wait — what *is* the default constructor? Here's something important to know about Dart: unlike Java or Clojure, Dart doesn't support method overloading. That means no multiple arities — not for regular methods, not for constructors. One method name, one signature.

To work around that, Dart uses *named constructors*. For example, the `DateTime` class has several: the default one, plus named constructors like `now`, `utc`, `fromMillisecondsSinceEpoch`, and `fromMicrosecondsSinceEpoch`.

Here’s how that looks in ClojureDart:

```clojure
(DateTime)
(DateTime.now) ;; or (DateTime/now)
(DateTime.fromMillisecondsSinceEpoch 1234567)
```

In Dart (and ClojureDart), constructor calls are syntactically indistinguishable from static method calls. And to make things more interesting, constructors don't even guarantee to return a new instance — they can be const or factory constructors.

That's why there's no new in ClojureDart: it wouldn’t really mean what you'd expect it to.

And yes, just like methods, constructors can be torn off and used as first-class functions:

```clojure
DateTime.fromMillisecondsSinceEpoch
DateTime.new ;; tear-off for the default constructor
```

### `const` Constructors

Dart has a notion of `const` values — and it's not just about making things immutable. A `const` value in Dart is a *compile-time constant*: it gets fully computed during compilation and is then memory-mapped into your app at runtime. This means no allocation, no instantiation — just reusing a shared value. It's efficient, but it has consequences.

In ClojureDart, `const` is used by default whenever possible. Most of the time, that’s exactly what you want. But sometimes it leads to surprising behavior.

For instance, suppose you're creating sentinel values using `(Object)`. In Dart, `Object`'s default constructor is marked as `const`. So if you write that expression multiple times, you’ll actually get *the same exact instance* every time. Not because of interning or caching, but because the compiler literally snapshots the value and reuses it.

If you're relying on object identity — say, for sentinel values or markers — this can break your logic. To force a fresh instance every time, use the `^:unique` metadata:

```clojure
^:unique (Object)
This tells the compiler: don’t treat this like a compile-time constant; I want a new instance each time.
```

Use ^:unique whenever identity matters.


### Calling static methods

There are two main ways to call a static method in ClojureDart:

```clojure
(ClassName/methodName ...)   ; old-school style
(ClassName.methodName ...)   ; modern style
```

The slash form (ClassName/methodName) is a bit of a legacy carryover — it only works if the class is local or explicitly imported. If you're using an alias, it won’t work.

That’s where the dot form comes in handy. It plays nice with aliases:

```clojure
(alias/ClassName.methodName ...)
```

Also worth knowing: Dart doesn't have fully qualified class names like Java does. Once imported, class names are just identifiers under an import prefix — no package-style nesting. This explains why the slash form while prevalent in Clojure feels old-school in ClojureDart.

### Static property access

Static properties in Dart — like `Colors.purple` — are straightforward to use in ClojureDart too.

Just write:

```clojure
m/Colors.purple
```

And yes, you can chain them just like in Dart:

```clojure
m/Colors.purple.shade900
```

Even method calls at the end of the chain work:

```clojure
(m/Colors.purple.shade900.withAlpha 128)
```

Of course you can also write `(.-purple m/Colors)` -- this can be easier when generating code in macros.

### Calling extension methods

[Extension methods](https://dart.dev/language/extension-methods) in Dart are a bit of syntactic sugar — and ClojureDart doesn’t have a great equivalent yet, mostly because they rely heavily on static typing.

Take `DateTime` for example. It has an extension called `DateTimeCopyWith`, which adds a `copyWith` method.

But here’s the trick: extension methods aren’t real instance methods. They’re just static methods dressed up to *look* like instance methods.

In ClojureDart, you can still call them — you just have to be a bit more explicit:

```clojure
;; assuming `dt` is a DateTime
(-> dt dart:core/DateTimeCopyWith (.copyWith .day 1))
```

One important caveat: the `(-> dt dart:core/DateTimeCopyWith)` part is not a value by itself. It only makes sense when followed by a method call. We’re piggybacking on Dart’s sugar here, not working with actual objects.

### `instance?`

In Clojure, `(instance? (identity String) "a")` works just fine — the class can be passed as a value, unwrapped, etc. But in ClojureDart, things are a bit stricter.

That’s because in Dart, the type used in an `is` check must be statically known — it has to appear *literally* in the code. So in ClojureDart, only something like `(instance? String "a")` is valid. You can’t sneak the class in through a variable or a function call.

In short: `instance?` exists, but it’s not a real function — it’s special syntax that must be fed a class name directly.


### Non-nullable types

Here’s another key difference with Java — and one to watch for when writing shared `cljc` code: Dart types are *not* nullable by default.

So if you write `^String x` in Clojure, `x` can still be `nil`. But in ClojureDart, that same type hint means `x` is *not allowed* to be `nil`.

If you want to allow `nil`, you need to say so explicitly with `^String? x`.

In short: nullable types must be marked with a `?`. No question mark, no `nil`.

### Generics

In Java, generics are erased at runtime, which is why Clojure doesn’t need to care about them.

But Dart *does* preserve generics at runtime, so ClojureDart has to deal with them — and the solution is a bit of a hack (a clever one?).

We piggyback on tagged literals: `#/(Map String Future)` is a tagged literal where the tag is `/`. It reads as `^{:type-params [String Future]} Map`. The key thing to note is that it’s parsed as a *symbol*, which means you can use this syntax *anywhere a symbol is valid* — method names, constructors, wherever.

For nested generics, no need to repeat the tag:

```clojure
#/(List (Map String Future)) ; equivalent to List<Map<String, Future>> in Dart
```

Simple and flexible, if a bit quirky.

We are considering leveraging the `^[]` shorthand  introduced in Clojure 1.12 as an alternative way to denote generics: `^[String Future] Map`, `^[^[String Future] Map] List`.

## UI with `cljd.flutter`: more Flutter, less clutter!

Not a framework — just a handy library to cut down on boilerplate and make Flutter more palatable to Clojurists.

Less ceremony, more joy.

### `f/widget`

The main macro, it evaluates to a `Widget`, and its body is a mix of expressions and directives.

Directives are always keywords followed by one form. Some are general-purpose like `:let`, others are Flutter-specific like `:vsync`.

Expressions? They’re automatically threaded through `.child`.

### `.child`-threading

Inside a `f/widget` body, expressions are threaded through the `.child` named parameter. So this:

```clojure
(f/widget
  m/Center
  (m/Text "hello"))
```

…expands to:

```clojure
(m/Center .child (m/Text "hello"))
```

You can also use dotted symbols to thread through other named parameters:

```clojure
(f/widget
  m/MaterialApp
  .home
  m/Scaffold
  .body
  (m/Text "Don't stop it now!"))
```

Which becomes:

```clojure
(m/MaterialApp .home
  (m/Scaffold .body
    (m/Text "Don't stop it now!")))
```



### State Management

## Data, I/O and Side Effects

## Advanced Topics

### FFI to C/ObjC/Java/Swift

### Testing

### Deploying Apps

### Performance and Debugging
