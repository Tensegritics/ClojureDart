<img src="logo1024.png" width="100%">

# What is ClojureDart?

ClojureDart is a Clojure dialect for Dart and Flutter.

Its primary goal is to leverage Dart and Flutter to extend Clojure's reach to
mobile and desktop apps.

# Status

ClojureDart is production-ready: you can ship applications right now.

It works with both Dart 2.13+ and Dart 3.

Some features are missing:
- no REPL yet, but we have excellent hot-reload so for UI work you get instant feedback
- multi-method (WIP)

In any case get in touch with us on [Clojurians #ClojureDart channel](https://clojurians.slack.com/app_redirect?channel=clojuredart) or open an issue.

# Cheatsheet

We have a [cheatsheet](doc/ClojureDart%20Cheatsheet.pdf) covering ClojureDart specifics on one side and Flutter programming on the other side.

# Conj 2023

Want to see what's the workflow like? Our talk at Clojure/Conj 2023 is mostly a live-coding session, starting from scratch and assuming no prior knowledge of Dart or Flutter!

https://www.youtube.com/watch?v=dqBeGpuedf0&list=PLZdCLR02grLpIQQkyGLgIyt0eHE56aJqd&index=24

# Links dump

[Slack](https://clojurians.slack.com/app_redirect?channel=clojuredart)
[Youtube](https://www.youtube.com/channel/UCCkvOkh6pXzYqkFKDgoyWRg)
[Twitter](https://twitter.com/clojuredart)

# Your first app!

Prerequisites: Clojure and Flutter installed and on your path.

Create a project directory with its `deps.edn`
``` shell
mkdir hello
cd hello
cat << EOF > deps.edn
{:paths ["src"] ; where your cljd files are
 :deps {tensegritics/clojuredart
        {:git/url "https://github.com/tensegritics/ClojureDart.git"
         :sha "8d5916c0dc87146dc2e8921aaa7fd5dc3c6c3401"}}
 :aliases {:cljd {:main-opts ["-m" "cljd.build"]}}
 :cljd/opts {:kind :flutter
             :main acme.main}}
EOF
```

(To update an existing project to the latest ClojureDart, just do `clj -M:cljd upgrade`)

Initialize project:

``` shell
clj -M:cljd init
```

Add some source code:

``` shell
mkdir -p src/acme
cat << EOF > src/acme/main.cljd
(ns acme.main
  (:require ["package:flutter/material.dart" :as m]
            [cljd.flutter :as f]))

(defn main []
  (f/run
    (m/MaterialApp
      .title "Welcome to Flutter"
      .theme (m/ThemeData .primarySwatch m.Colors/pink))
    .home
    (m/Scaffold
      .appBar (m/AppBar
                .title (m/Text "Welcome to ClojureDart")))
    .body
    m/Center
    (m/Text "Let's get coding!"
       .style (m/TextStyle
                .color m.Colors/red
                .fontSize 32.0))))
EOF
```

Compile, watch and run:

```
clj -M:cljd flutter
```

In most environments this will spawn a desktop app.

More details [there](doc/flutter-quick-start.md)

# Who is behind it?

Tensegritics, an itty-bitty Clojure consultancy by [Baptiste Dupuch](https://github.com/dupuchba)[🐦](https://twitter.com/BaptisteDupuch) and [Christophe Grand](https://github.com/cgrand)[🐦](https://twitter.com/cgrand).

ClojureDart is not a revenue source for us, even if we build it for fun and hopefully profit -- by being the proverbial rising tide which lifts all boats.

Sponsoring us is a good way to get the project moves faster. In the past sponsorship money allowed us to have an intern on the project writing samples and producing videos.

Obviously you can contract us for assistance or development with ClojureDart.

# Quick starts

- [For Flutter](doc/flutter-quick-start.md) to build GUIs
- [For Plain Dart](doc/quick-start.md) to build CLI apps.

# Examples

In the [samples directory](samples/) directory, you'll find original sample code and ports of [Flutter recipes](https://docs.flutter.dev/cookbook).

## How to run a sample project

Clone the ClojureDart repo.

```shell
git clone https://github.com/Tensegritics/ClojureDart.git
```

Go to the sample you want to try, let's say `fab`:

```shell
cd ClojureDart/samples/fab
```

Init the project:

```shell
clj -M:cljd init
```

Then launch the watcher:
```shell
clj -M:cljd flutter
```

You should get the sample running either in Chrome or as a desktop app.

To specify your exact target you must run `flutter devices` which outputs something like:

```shell
3 connected devices:
iPhone 6s (mobile) • D6707352-78D2-46BB-AB95-87355283FC82 • ios            •
com.apple.CoreSimulator.SimRuntime.iOS-15-5 (simulator)
macOS (desktop)    • macos                                • darwin-arm64   •
macOS 12.4 21F79 darwin-arm
Chrome (web)       • chrome                               • web-javascript •
Google Chrome 103.0.5060.114
```

The second column is the id of the target (here `D6707352-78D2-46BB-AB95-87355283FC82`, `macos` or `chrome`) that you pass to the watcher:

```shell
clj -M:cljd flutter -d D6707352-78D2-46BB-AB95-87355283FC82
```

Enjoy! 🧃

# `cljd.flutter`
`cljd.flutter` is an utility namespace to remove Flutter boilerplate and integrate more nicely with Clojure.

# `cljd.flutter.alpha`

Deprecated, use `cljd.flutter`.

# `cljd.flutter.alpha2`

Got out of alpha status and lives a happy life as `cljd.flutter`.

# Thanks!

To all individuals who blindly believed in our endeavor and sponsored our work.

To NuBank who approached us very early for sponsorship.

To Roam Research who bet their mobile apps development (now in the App Store and Play Store) on ClojureDart and allowed us to make steady progress since Summer 2021.

If you want to sponsor our work, you can sponsor either of us, we'll balance sponsorship. If you are a company you can also contact us directly.
