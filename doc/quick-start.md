# ClojureDart Quick Start

> This document is about creating a CLI app written in Dart; for a mobile app follow our [Flutter Quick Start](flutter-quick-start.md). However it's recommended to first give a try to the current document.

## System requirements

ClojureDart needs at least Java 9.

## 1. [Install the Dart SDK](https://dart.dev/get-dart#install)

If you already have Dart installed, make sure your version of the sdk is at least 2.12 -- this version introduced a big change (types are not nullable by default) to the language, use `dart --version` to check. Code produced by ClojureDart wouldn't be compatible with previous versions of Dart.

## 2. [Install Clojure CLI Tools](https://clojure.org/guides/getting_started#_clojure_installer_and_cli_tools)

If you already have the `clj` command installed make sure to upgrade to at least the [1.10.3.814](https://clojure.org/releases/tools#v1.10.3.814). This release allows to easily use private git deps.

## 3. Create a new project

First, create a Clojure project, you need to specify it's a pure Dart (not Flutter) project and where is the `main` function (here `quickstart.helloworld`):

```shell
mkdir helloworld
cd helloworld
cat << EOF > deps.edn
{:paths ["src"] ; where your cljd files will live
 :deps {org.clojure/clojure {:mvn/version "1.10.1"}
        tensegritics/clojuredart
        {:git/url "git@github.com:tensegritics/ClojureDart.git"
         ; or  "https://github.com/tensegritics/ClojureDart.git"
         :sha "4518023cb5d0966ba9f7d34a488d799111fa8754"}}
 :aliases {:cljd {:main-opts ["-m" "cljd.build"]}}
 :cljd/opts {:kind :dart
             :main quickstart.helloworld}}
EOF
```

Then, you need to prepare this project to also be a Dart project:
```shell
clj -M:cljd init
```

And add the main namespace:

```shell
mkdir -p src/quickstart
cat << EOF > src/quickstart/helloworld.cljd
(ns quickstart.helloworld)

(defn main []
  (print "hello, world\n"))
EOF
```

The `src` directory isn't special, you are free to layout your project as you like, as long as you don't
interfere with [Dart's project layout](https://dart.dev/tools/pub/package-layout) (`bin` and `lib` especially).

## 4. Compiles to Dart

By default compilation starts from the main namespace (here `quickstart.helloworld`) and transitively compiles dependencies.

``` shell
clj -M:cljd compile
```

The above command compiles the project only once and exits. When you are actively working on a piece of code we recommend you use `watch` instead of `compile`:

``` shell
clj -M:cljd watch
```

## 5. Run your program

Compiled Dart files are found under `lib/cljd-out`; to execute the program, just type:

``` shell
dart run
```

By doing so you have run your program on the Dart VM. To get an actual executable, enter:

``` shell
dart compile exe -o helloworld bin/helloworld.dart
```

Without the `-o helloworld` option it would have created a `helloworld.exe` alongside `helloworld.dart`.

## 6. Enjoy!

Write more clojure code, new namespaces, have fun. The watcher will pick up your changes.
Then execute your dart file again.
