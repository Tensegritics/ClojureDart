# Description

An example for [Video Player Plugin](https://pub.dev/packages/video_player)

OBS: This plugin does not support implementation for MacOS compilation!
like this: (```clj -M:cljd flutter -d macos```)
* [Flutter Issue for MacOS support on video_player](https://github.com/flutter/flutter/issues/41688)

# How to run

- Before running Clojure flutter, this demo needs to install the flutter dependencies with:

```bash
clj -M:cljd init
```

- And:

```bash
flutter pub add material
flutter pub add video_player
```

and after these two processes, you can do

```bash
clj -M:cljd flutter
```
