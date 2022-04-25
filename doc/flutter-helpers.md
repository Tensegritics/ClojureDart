# `cljd.flutter.alpha`

`cljd.flutter.alpha` strives to unclutter Flutter code 😜.

Its two goals are to cut on Flutter boilerplate and make it more Clojure-like.

## `nest` macro

In Flutter code, it's very common to have medium to long chains of widgets chained through `:child`.

For example, this Dart

```dart
IgnorePointer(
  ignoring: _open,
  child: AnimatedContainer(
    transformAlignment: Alignment.center,
    transform: Matrix4.diagonal3Values(
      _open ? 0.7 : 1.0,
      _open ? 0.7 : 1.0,
      1.0,
    ),
    duration: const Duration(milliseconds: 250),
    curve: const Interval(0.0, 0.5, curve: Curves.easeOut),
    child: AnimatedOpacity(
      opacity: _open ? 0.0 : 1.0,
      curve: const Interval(0.25, 1.0, curve: Curves.easeInOut),
      duration: const Duration(milliseconds: 250),
      child: FloatingActionButton(
        onPressed: _toggle,
        child: const Icon(Icons.create),
      ),
    ),
  ),
);
```

which translates directly to
```clj
(m/IgnorePointer.
  :ignoring (boolean @open)
  :child
  (m/AnimatedContainer.
    :transformAlignment m.Alignment/center
    :transform (m.Matrix4/diagonal3Values
                 (if @open 0.7 1.0)
                 (if @open 0.7 1.0)
                 1.0)
    :duration ^:const (m/Duration. :milliseconds 250)
    :curve ^:const (m/Interval. 0.0 0.5 :curve m.Curves/easeOut)
    :child
    (m/AnimatedOpacity.
      :opacity (if @open 0.0 1.0)
      :curve ^:const (m/Interval. 0.25 1.0 :curve m.Curves/easeInOut)
      :duration ^:const (m/Duration. :milliseconds 250)
      :child
      (m/FloatingActionButton.
        :onPressed toggle
        :child
        (m/Icon. m.Icons/create)))))
```

can be flattened with `nest` into:

```clj
(f/nest
  (m/IgnorePointer. :ignoring (boolean @open))
  (m/AnimatedContainer.
    :transformAlignment m.Alignment/center
    :transform (m.Matrix4/diagonal3Values
                 (if @open 0.7 1.0)
                 (if @open 0.7 1.0)
                 1.0)
    :duration ^:const (m/Duration. :milliseconds 250)
    :curve ^:const (m/Interval. 0.0 0.5 :curve m.Curves/easeOut))
  (m/AnimatedOpacity.
    :opacity (if @open 0.0 1.0)
    :curve ^:const (m/Interval. 0.25 1.0 :curve m.Curves/easeInOut)
    :duration ^:const (m/Duration. :milliseconds 250))
  (m/FloatingActionButton. :onPressed toggle)
  (m/Icon. m.Icons/create))
  ```

## `widget` macro

It's the Swiss army knife of Flutter in ClojureDart: it replaces instances of `StatelessWidget`, `StatefulWidget`, `State`, `Builder` and `StatefulBuilder`.

The general structure of `widget` is a body preceded by inlined `:option value` pairs.

The body always evaluates to a `Widget` and the whole `widget` form itself evaluates to a `Widget` too.

Supported options are `:key`, `:state`, `:watch`, `:with`, `:ticker` and `:tickers`.

### `:key k`

Specifiy the local key (a plain non-nil value) for this widget.

`nil` (default) means no key.

Local keys are used to identify siblings across reordering and updates.

### `:state [my-state init]`

`:state [my-state init]` creates an atom as per `(let [my-state (atom init)] ...)`. Any change to this atom will trigger an update of the widget.

### `:watch pre-existing-atom`

Any change to the atom named `pre-existing-atom` will trigger an update of the widget.

### `:with [resource init ...]`

This one is about resources management. For example if you need a `ScrollController` you can simply use `:with [controller (m/ScrollController.)]`, it will be initialized in [`initState`](https://api.flutter.dev/flutter/widgets/State/initState.html) and discarded in [`dispose`](https://api.flutter.dev/flutter/widgets/State/dispose.html).

By default a resource is disposed by calling its `.dispose` method. However if the resource must be freed differently you have to specify it like this:

```clj
:with [file (.openSync (io/File "log"))
       :dispose .closeSync]
```

The resource name is threaded (as per `->`) through the `:dispose` form. Most of the time it will be simply a method or a function.

Last, you can introduce intermediate values to use in resource initialization via `:let`:

```clj
:with [res1 init1
       :dispose .cancel
       :let [v expr]
       res2 (init2 v)]
```

### `:ticker name` or `:tickers name`

This will bind `name` to a [`TickerProvider`](https://api.flutter.dev/flutter/scheduler/TickerProvider-class.html) to use in `AnimationController`s. Use `:ticker` if you have a single `AnimationController` (the common case).
