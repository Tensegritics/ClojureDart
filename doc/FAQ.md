# FAQ
## Dynamic Warnings
"Dynamic warnings" are similar to "reflection warnings" in Clojure but more serious and should be dealt with as soon as possible (starting by the first one, as they tend to cascade).

"Dynamic warnings" and "reflection warnings" are similar because they have the same cause: the compiler is not able to infer the type of the object upon which a method is called.

However Dart, by design, doesn't offer the relective features the JVM has. That's why the code emitted by ClojureDart in case of a dynamic warning has more chances to fail than the code emitted by Clojure in case of a reflective call.

So dynamic calls:
* are slower
* can fail at runtime because their arguments weren't properly casted to the correct type.
