# All you never wanted to know about Dart generics

## Preamble
In Java, generics exist only at the language level, not at the JVM level (the loss of this information is known as type erasure).
Interop happening at the JVM level we Clojure people don't have to care for Generics.

However in Dart, generics are not erased so we ClojureDart people have to deal with them.

## Notation transcription
Every time you see `foo<bar,baz>` you can write `#/(foo bar baz)`, it's irrelevant to whether the `foo` symbol is a type, a method or whatever.

`#/(foo bar baz)` read as a symbol with metadata. It's just a shorthand for `^{:type-params [bar baz]} foo`.

## Covariance and contravariance
Let's not talk about function return types for now and consider all other cases.

A `List<String>` is a subtype of `List<Object>` because `String` is a subtype of `Object`. Dart generics are said to be covariant.

It's a common design choice even if it brings funny behaviors:

```dart
List<String> strings = [];
List<Object> objects = strings;
 // I must be able to add any object to objects since it's a list of Object, right?
objects.add(Object()); // right?
// ka💥boom! because objects points to strings which accepts only strings as items.
```

## `.cast<R>()` is a lie
It's a common pattern found for example on collections and streams to have a `.cast` method to change the type of the object.

This pattern is a runtime bandaid. For example when you have a string of objects which happens to only hold strings, you can't pass it as list of strings. See:

```dart
acceptOnlyStrings(List<String> strings) {
  // intentionally left blank
}

List<Object> objects = [];
acceptOnlyStrings(objects); // 👈 compilation error
```

However if you `.cast. it works:

```dart
acceptOnlyStrings(List<String> strings) {
  // intentionally left blank
}

List<Object> objects = [];
acceptOnlyStrings(objects.cast()); // 👌
```

You may wonder why we only typed `.cast()` and not `.cast<String>()`: it's because the compiler expects a `List<String>` and thus is able to infer the omitted `<String>`.

Let's try something not too different but more surprising:
```dart
acceptOnlyStrings(List<String> strings) {
  // intentionally left blank
}

List<Object> objects = [42];
acceptOnlyStrings(objects.cast()); // still 👌 despite 42 being obviously not a string!
```

`.cast` methods create wrappers whose purpose is only to lie on their static type. The lie will hold as long as we don't do anything that could betray it:

```dart
List<Object> objects = ["I'm a string", 42];
List<String> strings = objects.cast();
print(33); // print accepts any object
print(strings.first); // strings.first is 👌 because the first item is effectively a string 
print(strings[1]); // ka💥boom! because our lie can't be held any more 
```

Objects returned by `.cast` methods are only runtime type-enforcing views on the original object:

```dart
List<Object> objects = ["I'm a string", 42];
List<String> strings = objects.cast();
print(33); // print accepts any object
print(strings.first); // strings.first is 👌 because the first item is effectively a string 
objects[1]="Wait, I'm a string too!";
print(strings[1]); //  it's 👌 this time because the original object has been modified
```

## ClojureDart and magicast
### Magicast
Every time the CLJD compiler sees a value whose type is "not right but not completely wrong" it emits additional code to hopefuly make it right.

For example, let's suppose a method expecting a `List<Widget>` as argument and we pass it a local whose type is totally unknown then ClojureDart will do something a bit like that:

```dart
List<Widget> children;
if (myVar is List<Widget>)
  children=(List<Widget>) myVar;
else if (myVar is List)
  children=((List) myVar).cast();
else
  throw "ka💥boom!";
```

If the local had statically (type hint or inference) been known to be a `List` the compiler would have emitted:
```dart
List<Widget> children;
if (myVar is List<Widget>)
  children=(List<Widget>) myVar;
else
  children=((List) myVar).cast();
```

If the local has been known to be a `List<Widget>` nothing would have been emitted.

This is this behavior that we call "magicast" and this is what allows you to pass dynamic Clojure vectors where typed lists are expected.

And we magicast all types which happen to have a `.cast()` method.

### The Dart side of the colls
Unlike what has been described for Dart collections, ClojureDart collections returned by `.cast` are not views. It doesn't matter much because these are immutable collections.

Specifically a persistent collection is usually a wrapper object containing a tree of nodes (the wrapper holding information such as the element count, hash value etc.). When we call `.cast` on it we just create a new wrapper object matching the expected type but sharing the original tree node.

Thus `.cast` is cheap and the value returned by it is still a persistent collection and can still be used with no additional constraints on items types. It's only the Dart side which is typed.
