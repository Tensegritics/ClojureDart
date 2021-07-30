# Quick run flutter project

> Make sure you've done the [Quick Run](quick-run.md) before trying Flutter

1. Install clj cli dans dart (see Quick Run)

2. Install [Flutter](https://flutter.dev/docs/get-started/install)

- We advise you to choose a stable release of flutter.
- You might need to install XCode or Android Studio depending on which
  platform you are working on.
- You might need to install/update Cocoapods for iOS

3. Create your first Flutter project

``` shell
# In your terminal
flutter create training
cd training
```

4. Remove existing lib/main.dart

``` shell
# In your terminal
rm lib/main.dart
```

4. Create `deps.edn` file at the root of your project

``` clojure
{:paths ["clj/src" "clj/test" "resources"]
 :deps {org.clojure/clojure {:mvn/version "1.10.1"}
        io.github.dupuchba/clojuredart
        {:git/url "git@github.com:Tensegritics/ClojureDartPreview.git"
         :sha "dca46fa9f4b759b6a1ea2bd622ed887a7aca73c9"}}}
```

5. Create a clojuredart file with a main entry-point

First create a directory where clojure files live

``` shell
# In your terminal
mkdir -p clj/src/acme/
touch clj/src/acme/main.cljc
```

Try out some components...

``` clojure
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
```

Note : Flutter deps is specified in pubspec.yml file at the root of
the project.

6. Launch the clojuredart watcher

``` shell
# In your terminal
clj -M -m cljd.build watch acme.main
```

7. (iOS Only) Launch a simulator

``` shell
# In an other terminal window
open -a Simulator
```

8. Run the flutter hot-reload cli

``` shell
# In a terminal window
flutter run -t lib/cljd-out/acme/main.dart
```

9. Enjoy :-)

***

### When you edit your cljc file

- just press enter in your clj terminal to recompile
- press R in your flutter cli to restart the app
