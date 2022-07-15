# What's going on with ClojureDart?

A journal of random periodicity of what's going on with the ClojureDart project.

## July 15 2022

### Kondo and LSP (WIP)
We chose to prioritize developer experience and heretically enough it means postponing the REPL to first get better LSP support.

To this end, Baptiste is revisiting the "analyzer" (the offline process which dumps detailed class information about dependencies and which takes quite a lot of time on some builds) to be an on demand process (thus faster builds) to integrate with kondo and clojure-lsp.

He has put his recent REPL efforts on hold.

### Interop syntax changes (Done)

Dart supports named parameters and so does ClojureDart for interop. However current support relied on identifying paramters names with keywords (e.g. `(w/Center :child x)`). While nicely Clojury looking this has a big drawback: it's impossible to syntactically determine if we are calling a function (or method or constructor) with 1 named parameter or with 2 positional parameters (the first one being a keyword)!

To distinguish between the two we need type information which means that type-hinting was mandatory because non-hinted code would fail too often and with inscrutable messages.

Thus we decided to change the parameters names syntax from keywords to dotted symbols: `(w/Center .child x)`.

In the same vein, ClojureDart supports both `(.prop x)` and `(.-prop x)` for property access, however the first form (without `-`) suffers from syntactic ambiguity and requires type information. Now the compiler is going to push (through warnings) the habit of always prefixing properties with dash.

Both changes were made in the name of developper experience too: make code works more often and less abstruse errors.

### flutter.alpha (WIP)

We are working on better integration of the state lifecycle (didUpdateWidget), we have something done but are not quite happy with the mental model around all the options of `widget` and their interactions.

### Casting to dart functions (wishful thinking)

Currently ClojureDart relies on Dart to coerce Clojure functions into Dart functionsthis is some type-driven syntactic sugar not a dynamic behavior. We should replace it by our own explicit (at codegen) coercion.

This would allow us to better coerce the returned value (notably sparing us to add a `nil` when the return type is `void`). This would also allow to map named arguments to Clojure named arguments.
