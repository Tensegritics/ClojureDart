# ClojureDart+Flutter Quick Start

> Even if Flutter bundles its own Dart it's better to first try [ClojureDart](quick-start.md) alone first.

## System requirements

ClojureDart needs at least Java 11.

## 1. [Install the latest stable Flutter](https://flutter.dev/docs/get-started/install)

It's a tad laborious as you have to install dependencies.

## 2. [Install Clojure CLI Tools](https://clojure.org/guides/getting_started#_clojure_installer_and_cli_tools)

If you already have the `clj` command installed make sure to upgrade to at least the [1.10.3.814](https://clojure.org/releases/tools#v1.10.3.814). This release allows to easily use private git deps.

## 3. Create your first ClojureDart/Flutter project

Creates a directory for the project with the following deps.edn:

``` shell
mkdir hello
cd hello
```

If your GitHub account is [configured for SSH access](https://docs.github.com/en/github/authenticating-to-github/connecting-to-github-with-ssh/adding-a-new-ssh-key-to-your-github-account):

``` shell
cat << EOF > deps.edn
{:paths ["src"] ; where your cljd files are
 :deps {org.clojure/clojure {:mvn/version "1.10.1"}
        tensegritics/clojuredart
        {:git/url "git@github.com:tensegritics/ClojureDart.git"
         :sha "19ba3c6280daf1898d27e07994a1911ad8f51f8d"}}}
EOF
```

Otherwise using HTTPS authentication:

``` shell
cat << EOF > deps.edn
{:paths ["src"] ; where your cljd files are
 :deps {org.clojure/clojure {:mvn/version "1.10.1"}
        tensegritics/clojuredart
        {:git/url "https://github.com/tensegritics/ClojureDart.git"
         :sha "19ba3c6280daf1898d27e07994a1911ad8f51f8d"}}}
EOF
```

## 4. Initialize the project

``` shell
clj -M -m cljd.build init acme.main
```

`acme.main` is the root namespace of the project where the `main` function is defined.

## 5. Create a ClojureDart file with a main entry-point

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
    (material/MaterialApp
      :title "Welcome to Flutter"
      :theme (material/ThemeData :primarySwatch material.Colors/pink)
      :home (material/Scaffold
              :appBar (material/AppBar
                        :title (widgets/Text "Welcome to ClojureDart"))
              :body (widgets/Center
                      :child (widgets/Text "This text is Centered."
                               :style (painting/TextStyle
                                        :color material.Colors/red
                                        :fontSize 32.0)))))))
EOF
```
## 7. Start a simulator

In another terminal

iOS:
``` shell
open -a Simulator
```

Android:

## 8. Start the ClojureDart watcher

``` shell
clj -M -m cljd.build flutter
```

## 9. Enjoy!

 When you edit your cljd file, the watcher recompiles cljd files and, on success, hot reloads the application. **Sometimes the application may not pick up your change so hit the return key to get the watcher to restart the application.**
