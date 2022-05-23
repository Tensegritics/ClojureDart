# Troubleshooting

## Table of content
* [Project set-up](./Troubleshooting.md#project-set--up)

##  Project set up

### Error while host compiling [...] Can't resolve widgets/InheritedModel (no source location)

* Make sure you have the same error even when you re init the project.
    * If so, make sure you have:
        ```yaml
        # pubspec.yaml at the root of the project
        dependencies:
          flutter:
            sdk: flutter
        ```
> :bulb: You may have forgotten to add the `:` to the keyword `:as` to one of your requirements statements.

### Execution error at cljd.compiler/load-libs-info (compiler.cljc:126). EOF while reading

* When capturing the content of `./clojuredart/libs-info.edn` you shouldn't have one single line.
It may due to a wrong initialization of the project (`clj -M -m cljd.build init --dart folder.example` instead of simply `clj -M -m cljd.build init folder.example`)

> :bulb: You can restart the project the right way, by deleting everything except the content of your `src` folder and `deps.edn`
