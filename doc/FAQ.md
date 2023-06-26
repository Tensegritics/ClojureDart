# FAQ
## Dynamic Warnings
"Dynamic warnings" are similar to "reflection warnings" in Clojure but more serious and should be dealt with as soon as possible (starting by the first one, as they tend to cascade).

"Dynamic warnings" and "reflection warnings" are similar because they have the same cause: the compiler is not able to infer the type of the object upon which a method is called.

However Dart, by design, doesn't offer the relective features the JVM has. That's why the code emitted by ClojureDart in case of a dynamic warning has more chances to fail than the code emitted by Clojure in case of a reflective call.

So dynamic calls:
* are slower
* can fail at runtime because their arguments weren't properly casted to the correct type.
## Extension Methods
> Extension methods add functionality to existing libraries... Extensions can define not just methods, but also other members such as getter, setters, and operators. Also, extensions can have names, which can be helpful if an API conflict arises.

We currently do not correctly infer extension methods as methods, but you can still use them with our custom syntax.
Let's take the example of `DateTimeCopyWith` from `dart:core`, which can be found at https://api.flutter.dev/flutter/dart-core/DateTimeCopyWith.html .

This class exposes a new extension method called `copyWith`, which allows you to create a copy of a DateTime object and override its properties according to your needs.

In pure Dart, you would write the following code:
``` dart
var now = DateTime.now();
var nowPrecedentYear = now.copyWith(year: now.year - 1);
```
In ClojureDart for extensions methods you would write:

``` clojure
(let [now (DateTime/now)]
    (-> now dart:core/DateTimeCopyWith (.copyWith .year (dec (.-year now)))))
```
⚠️ Please note that we understand that being able to call extensions as regular method calls would be ideal, but we have not reached that point yet.

## Conditional reading, CLJC
Currently CLJD uses the Clojure reader so the `:clj` features in conditional is always on so **you have to put `:clj` last in your conditionals**.

Second, when you need a macro (which is currently compiled by Clojure) to use some Clojure path, it's better to use `:cljd/clj-host` so as to not confuse "clj-for-clj" and "clj-for-cljd-macros".
