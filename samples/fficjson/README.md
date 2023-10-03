# cJson ClojureDart example

Demonstrates generation of bindings for a C library called
[cJson](https://github.com/DaveGamble/cJSON) and then using these bindings
to parse some json.

## Building the cJson dynamic library
From the root of this repository -
```
cd third_party/cjson_library
cmake .
make
cd ../../
```

## Generating bindings
At the root of this example (`example/c_json`), run -
```
clj -M:cljd init
dart pub get
dart pub add ffi
dart pub add path
dart pub add ffigen --dev
dart run ffigen --config config.yaml
```
This will generate bindings in a file: lib/cjson_generated_bindings.dart

## Running the example
```
clj -M:cljd compile
dart run
```
