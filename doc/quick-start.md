# ClojureDart Quick Start

> If you want to use Flutter, follow this document first (not mandatory but recommended) then follow our [Flutter Quick Start](flutter-quick-start.md).

## 1. [Install the Dart SDK](https://dart.dev/get-dart#install)

Make sure your version of the sdk >= 2.12 -- this version introduced a big change (types are not nullable by default) to the language. Code produced by ClojureDart wouldn't be compatible by previous versions of Dart.

## 2. [Install Clojure CLI Tools](https://clojure.org/guides/getting_started#_clojure_installer_and_cli_tools)

If you already have the `clj` command installed make sure to upgrade to at least the [1.10.3.814](https://clojure.org/releases/tools#v1.10.3.814). This release allows to easily use private git deps.

## 3. Create a new project

### 3.1 Create a Dart project

``` shell
# create a Dart project
dart create helloworld
# move into it
cd helloworld
# create a directory for clojure source files
mkdir src
```

### 3.2 Edit `pubspec.yaml`

Change these lines:

``` yaml
environment:
  sdk: '>=2.10.0 <3.0.0'
```

into:

``` yaml
environment:
  sdk: '>=2.12.0 <3.0.0'
```

And then call `pub get` to update dependencies.

``` shell
pub get
```

(Again it's about ensuring the right language version, otherwise the Dart compiler enters compatibility mode.)

### 3.3 Create a `deps.edn` file

If your GitHub account is [configured for SSH access](https://docs.github.com/en/github/authenticating-to-github/connecting-to-github-with-ssh/adding-a-new-ssh-key-to-your-github-account):

``` shell
cat << EOF > deps.edn
{:paths ["src"] ; where your cljd files are
 :deps {org.clojure/clojure {:mvn/version "1.10.1"}
        tensegritics/clojuredart
        {:git/url "git@github.com:tensegritics/ClojureDartPreview.git"
         :sha "724fea858c0f0629f776910d442de2a2ca209dc8"}}}
EOF
```

Otherwise using HTTPS authentication:

``` shell
cat << EOF > deps.edn
{:paths ["src"] ; where your cljd files are
 :deps {org.clojure/clojure {:mvn/version "1.10.1"}
        tensegritics/clojuredart
        {:git/url "https://github.com/tensegritics/ClojureDartPreview.git"
         :sha "724fea858c0f0629f776910d442de2a2ca209dc8"}}}
EOF
```

The `src` directory isn't special, you are free to layout your project as you like, as long as you don't
interfere with [Dart's project layout](https://dart.dev/tools/pub/package-layout) (`bin` and `lib` especially).

## 4. Create an actual ClojureDart file

``` shell
mkdir -p src/quickstart
cat << EOF > src/quickstart/helloworld.cljd
(ns quickstart.helloworld)

(defn main []
  (dart:core/print "Hello World!"))
EOF
```

Note that you need a `main` function as the entry point of your program.

## 5. Compiles to Dart

You have to specify your main namespace (here `quickstart.helloworld`), dependencies will be transitively compiled.

``` shell
clj -M -m cljd.build compile quickstart.helloworld
```

The above command compiles the project only once and exits. When you are actively working on a piece of code
we recommend you use `watch` instead of `compile`:

``` shell
clj -M -m cljd.build watch quickstart.helloworld
```

## 6. Run your program

Compiled Dart files are found under `lib/cljd-out`; to execute the program, just type:

``` shell
dart lib/cljd-out/quickstart/helloworld.dart
```

By doing so you have ran your program on the Dart VM. To get an actual executable, enter:

``` shell
dart2native -o helloworld lib/cljd-out/quickstart/helloworld.dart
./helloworld
```

Without the `-o helloworld` option it would have created a `helloworld.exe` alongside `helloworld.dart`.

Last, if you feel playful you can compile it to js:

``` shell
dart2js -o helloworld.js lib/cljd-out/quickstart/helloworld.dart
node helloworld.js
```

## 7. Enjoy!

Write more clojure code, new namespaces, have fun. Once your done
just *press enter* in your terminal where you launched the watcher.
Then execute you dart file again.
