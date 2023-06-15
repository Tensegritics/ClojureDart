# Generated names by CLJD

This document describes how dart names are generated.

We want name generation to be injective (no clash) and to support composite names.

## Munging
In Dart, names are very conservative: all characters must belong to `[a-zA-Z0-9_$]` and an identifier can't start by a number.
Plus if it starts by an underscore, it becomes private to the lib. Also some words are reserved or built-in identifiers (better to avoid both rather than being clever in the gray zone.)

When munging a clojure name the strategy is:
 * leave `[a-zA-Z0-9]` untouched
 * replace `-` by `_` -- unless if it's the first character in which case replace it by `$_`
 * escapes everything else (including `$` and `_`).

### Escape Sequences
Escape sequences starts by a dollar `$` and end with a `_`.

Common punctuation is escaped as `$ALLCAPS_` (e.g. `$COLON_`).

Reserved words are escaped as `$themselves_` (e.g. `$Function_`).

Other characters are escaped as a sequence of `$uXXXX_` (where `XXXX` is 1 to 4 uppercase hexadecimal digits) representing UTF-16 code units.

Once munged autogensyms tend bo very verbose (`x#` becomes `x__18920__auto__` which would be `x$UNDERSCORE_$UNDERSCORE_18920$UNDERSCORE_$UNDERSCORE_auto$UNDERSCORE_$UNDERSCORE_`) so there are two escape sequences for them: `$AUTO_` for `__auto__` and `$18920_` for `__18920`. Thus `x__18920__auto__` is actually munged to `x$18920_$AUTO_`.

### Type quoting
It's sometimes necessary to include a dart type in another name (eg for extensions). A dart type may contain characters outside of the identifiers characters: `.()<>[]{},` and space (before `Function`).

Type quoting should be injective (as munging in general). So it's escape sequences must not be mistaken for other escape sequences (especially the ALLCAPS one -- especially for all characters which are valid in clojure symbols too: `.<>`) and for `Function` itself.

If the type is foreign to CLJD it may contain a `$` which will mess up with our munging. But if the type is from CLJD it's safe.

Having two mechanisms is not great. So we should escape `$` and `_` too.

If we are set to escape everything then we should just re-munge the whole type.

With the notable exception we don't have to escape underscores since dashes are not valid in dart names.

### Invariant
A munged named must match this regexep: `([a-zA-Z0-9]|\$[a-zA-Z0-9]*_)([a-zA-Z0-9_]|\$[a-zA-Z0-9]*_)*`.

No leading underscore, all dollar signs are followed by alnums and closed by an underscore.

### Suffixes

In some contexts, suffixes may be appended to the munged name of a symbol. A suffixe matches `$[a-zA-Z0-9]`.

`$C` and `$D` are reserved suffixes (to not clash with composite names)

`$[0-9]+(tmp)?` is for locals: in the generated Dart code there's no shadowing. Those ending in `tmp` are used in recurs.

`$iface` is for the vdirect-implementation interface of a protocol.

`$iext` is for the extension implementation of a protocol.

`$iproto` is for the class implementing the IProtocol for a given protocol.

`$cext` is for a class implementing a class extension.

`$extension` is for the actual singleton of a `$cext`.

`$root` is for the root value of a dyn

## Composite names

A list of already munged names can be munged into a single unique identifier: the new composite name starts with `$C$` and ends with `$D$` (C and D look a bit like parenthesis) and its components are separated by `$$` when needed (that is when two adjacent components are not composite).

## Locals
To avoid shadowing locals are uniquely suffixed by `$nnn` where `nnn` is a decimal number.
Locals are guaranteed to be unique inside a top-level form.
