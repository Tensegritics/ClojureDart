# ClojureDart+Flutter Quick Start

> Even if Flutter bundles its own Dart it's better to first try [ClojureDart](quick-start.md) alone first.

## 1. [Install the latest stable Flutter](https://flutter.dev/docs/get-started/install)

It's a tad laborious as you have to install dependencies.

## 2. [Install Clojure CLI Tools](https://clojure.org/guides/getting_started#_clojure_installer_and_cli_tools)

If you already have the `clj` command installed make sure to upgrade to at least the [1.10.3.814](https://clojure.org/releases/tools#v1.10.3.814). This release allows to easily use private git deps.

## 3. Create your first Flutter project

``` shell
flutter create training
cd training
```

This creates a Dart project with (among other things) a `pubspec.yaml` with Flutter dependencies.

## 4. Remove existing lib/main.dart

``` shell
rm lib/main.dart
```

## 5. Create `deps.edn` file at the root of your project

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

## 6. Create a ClojureDart file with a main entry-point

First create a directory where clojure files live

``` shell
mkdir -p src/acme
cat << EOF > src/acme/main.cljd
(ns acme.main
  ;; pure dart package are imported using string
  (:require ["package:flutter/material.dart" :as material]
            ["package:flutter/widgets.dart" :as widgets]
            ["package:flutter/painting.dart" :as painting]))

(defn main []
  (material/runApp
    (reify :extends material/StatelessWidget
      ;; ^widgets/Widget is the return type of the build method
      ;; (defined in StatelessWidget). For now it's mandatory but we are not
      ;; far from being able to guess returns type from flutter
      (^widgets/Widget build [this context]
       ;; .& is for interop when you need named arguments when using dart libs
       ;; .& does not have to be first (see widgets/Text. ...)
       (material/MaterialApp. .&
         :title "Welcome to Flutter"
         :theme (material/ThemeData. .& :primarySwatch (.-pink material/Colors))
         :home (material/Scaffold. .&
                 :appBar (material/AppBar. .&
                           :title (widgets/Text. "Welcome to ClojureDart"))
                 :body (widgets/Center. .&
                         :child (widgets/Text. "This text is Centered." .&
                                  :style (painting/TextStyle. .&
                                           :color (.-red material/Colors)
                                           :fontSize 32.0)))))))))
EOF
```

## 7. Start the ClojureDart watcher

``` shell
clj -M -m cljd.build watch acme.main
```

## 8. Start a simulator

In another terminal

iOS:
``` shell
# In an other terminal window
open -a Simulator
```

Android:

## 9. Run the flutter hot-reload cli

In yet another terminal

``` shell
flutter run -t lib/cljd-out/acme/main.dart
```

You should get this kind of output:

```
Launching lib/cljd-out/acme/main.dart on iPhone 12 Pro Max in debug mode...
Running Xcode build...
 └─Compiling, linking and signing...                        51,0s
Xcode build done.                                           74,0s
Syncing files to device iPhone 12 Pro Max...                       150ms

Flutter run key commands.
r Hot reload. 🔥🔥🔥
R Hot restart.
h Repeat this help message.
d Detach (terminate "flutter run" but leave application running).
c Clear the screen
q Quit (terminate the application on the device).

💪 Running with sound null safety 💪
```

And the application should open in the simulator.

## 10. Enjoy!

 When you edit your cljd file, always follow these steps:
 1. press enter in the terminal tied to the ClojureDart "watcher",
 2. press R in the terminal tied to the flutter cli to restart the app.
