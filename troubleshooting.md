# Troubleshooting

## Table of content
* [Project set-up](./Troubleshooting.md#project-set--up)

##  Project set up

### Error while host compiling [...] Can't resolve widgets/InheritedModel (no source location)

<details>
    <summary>Entire Traceback</summary>
    Something horrible happened! :scream: Error while host-compiling (ns samples.tables "Faithful port of https://docs.flutter.dev/cookbook/design/tabs" (:require ["package:flutter/material.dart" :as m] [cljd.flutter.alpha as f])) (ns samples.tables "Faithful port of https://docs.flutter.dev/cookbook/design/tabs" (:require ["package:flutter/material.dart" :as m] [cljd.flutter.alpha as f])) Can't resolve widgets/InheritedModel (no source location)
</details>


* Make sure you have the same error even when you remove the main and just keep your module declaration and the requirements. (the (ns ..) part).
    * If so, make sure you have:
        ```yaml
        # pubspec.yaml at the root of the project
        dependencies:
          flutter:
            sdk: flutter
        ```
> :bulb: In my case, I forgotten to add the `:` to the keyword `:as` to one of my requirements statements.

### Execution error at cljd.compiler/load-libs-info (compiler.cljc:126). EOF while reading

<details>
    <summary>Entire Traceback</summary>

```clojure
{:clojure.main/message
 "Execution error at cljd.compiler/load-libs-info (compiler.cljc:126).\nEOF while reading\n",
 :clojure.main/triage
 {:clojure.error/class java.lang.RuntimeException,
  :clojure.error/line 126,
  :clojure.error/cause "EOF while reading",
  :clojure.error/symbol cljd.compiler/load-libs-info,
  :clojure.error/source "compiler.cljc",
  :clojure.error/phase :execution},
 :clojure.main/trace
 {:via
  [{:type clojure.lang.EdnReader$ReaderException,
    :message "java.lang.RuntimeException: EOF while reading",
    :at [clojure.lang.EdnReader read "EdnReader.java" 180]}
   {:type java.lang.RuntimeException,
    :message "EOF while reading",
    :at [clojure.lang.Util runtimeException "Util.java" 221]}],
  :trace
  [[clojure.lang.Util runtimeException "Util.java" 221]
   [clojure.lang.EdnReader read "EdnReader.java" 130]
   [clojure.lang.EdnReader read "EdnReader.java" 111]
   [clojure.edn$read invokeStatic "edn.clj" 35]
   [clojure.edn$read invokeStatic "edn.clj" 14]
   [clojure.edn$read invoke "edn.clj" 14]
   [cljd.compiler$load_libs_info invokeStatic "compiler.cljc" 126]
   [cljd.compiler$load_libs_info invoke "compiler.cljc" 124]
   [cljd.build$compile_cli invokeStatic "build.clj" 180]
   [cljd.build$compile_cli doInvoke "build.clj" 175]
   [clojure.lang.RestFn invoke "RestFn.java" 457]
   [cljd.build$_main invokeStatic "build.clj" 373]
   [cljd.build$_main doInvoke "build.clj" 346]
   [clojure.lang.RestFn applyTo "RestFn.java" 137]
   [clojure.lang.Var applyTo "Var.java" 705]
   [clojure.core$apply invokeStatic "core.clj" 665]
   [clojure.main$main_opt invokeStatic "main.clj" 514]
   [clojure.main$main_opt invoke "main.clj" 510]
   [clojure.main$main invokeStatic "main.clj" 664]
   [clojure.main$main doInvoke "main.clj" 616]
   [clojure.lang.RestFn applyTo "RestFn.java" 137]
   [clojure.lang.Var applyTo "Var.java" 705]
   [clojure.main main "main.java" 40]],
  :cause "EOF while reading"}}
```

</details>

* When capturing the content of `./clojuredart/libs-info.edn` you shouldn't have one single line.
It may due to a wrong initialization of the project (`clj -M -m cljd.build init --dart folder.example` instead of simply `clj -M -m cljd.build init folder.example`)

> :bulb: You can restart the project the right way

> :bulb: Or, you can add content at `./clojuredart/libs-info.edn` by redirecting the output of `./clojuredart/analyzer.dart` to `./clojuredart/libs-info.edn`, by doing:
`dart ./clojuredart/analyzer.dart > ./clojuredart/libs-info.edn`



