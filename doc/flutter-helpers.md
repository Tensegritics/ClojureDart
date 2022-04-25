# `cljd.flutter.alpha`

`cljd.flutter.alpha` is a library which strives to unclutter Flutter code 😜.

Its two goals are to cut on Flutter boilerplate and make it more Clojure-like.

## `f/nest` macro

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

can be flattened with `f/nest` into:

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

## `f/widget` macro

It's the Swiss army knife of Flutter in ClojureDart: it replaces instances of `StatelessWidget`, `StatefulWidget`, `State`, `Builder` and `StatefulBuilder`.
