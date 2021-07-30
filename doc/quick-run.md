# Quick Compile a ClojureDart file to Dart

1. [Install the Dart SDK](https://dart.dev/get-dart#install)

Make sure your version of the sdk >= 2.12 (Because of null safety)

2. [Install/Upgrade latest clj cli](https://clojure.org/guides/getting_started#_clojure_installer_and_cli_tools)

You need at least the [1.10.3.814](https://clojure.org/releases/tools#v1.10.3.814) version of the cli.

3. Create a new project

``` shell
# Root
mkdir foo
# Where your source files will go
mkdir -p foo/clj/src/acme
```

Note that you can put your files anywhere you want under your project
root as long as it's not under `foo/lib`. `/lib` is used by
dart/flutter to load it's files.

4. Create a `deps.edn` file

``` clojure
;; Your clojuredart files must be in (class)paths
{:paths ["clj/src" "clj/test" "resources"]
 :deps {org.clojure/clojure {:mvn/version "1.10.1"}
        io.github.dupuchba/clojuredart
        {:git/url "git@github.com:Tensegritics/ClojureDartPreview.git
         :sha "724fea858c0f0629f776910d442de2a2ca209dc8"}}}
```

5. Write some code in a new file

``` shell
touch foo/clj/src/acme/core.cljc
```

``` clojure
(ns acme.core)

(defn main []
  (dart:core/print "Hello World!"))
```

Note that you need a `main` function if you want to execute this file
in Dart VM.

6. Launch the compiler with auto-reload command

Command accepts multiple namespaces that are in your classpath.

``` shell
clj -M -m cljd.build watch acme.core
```

7. Execute your dart file

``` shell
# run in dart VM
dart foo/lib/cljd-out/acme/core.dart

```

You can also compile it to native executable.

``` shell
dart2native foo/lib/cljd-out/acme/core.dart
# Or even js if you are a baller
dart2js foo/lib/cljd-out/acme/core.dart
```

8. Repeat

Write more clojure code, new namespaces, have fun. Once your done
just *press enter* in your terminal where you launched the watcher.
Then execute you dart file again.
