# Testing

# Writing tests
`cljd.test` is a `clojure.test` port built on `dart test` to leverage existing tooling.

By default, tests are compiled to the `test` directory (to match `dart test` defaults). This can be overriden on a namespace basis by putting the `:dart.test/dir` metadata in the `ns` form:

```clojure
(ns some.testing.namespace
  "Let's test!"
  {:dart.test/dir "integration_test"}
  (:require ...))
```

`deftest` supports inline options, specifically `:tags` and `:runner`:

```
(deftest whatever
  :tags [:widget]
  :runner (ft/testWidgets [tester])
  (let [^ft/WidgetTester {:flds [pumpWidget]} tester
        _ (await (pumpWidget (sw/my-widget "T" "M")))
        title-finder (ft/find.text "T")
        message-finder (ft/find.text "M")]
    (ft/expect title-finder ft/findsOneWidget)
    (ft/expect message-finder ft/findsOneWidget)))
```

`:tags` allows tests selection using `dart test` `-t` and `-x` flags (selecting by name is also possible).

```bash
$ clj -M:cljd test -- -t widget
```

Everything after `--` will be passed to `dart test`.

`:runner` allows to specify a specialized test runner, `[tester]` here is the binding vector for arguments provided by the runner.

# Running tests
Tests are run with `clj -M:cljd test`: it compiles all namespaces on the classpath and runs all tests found.

You can narrow the namespaces searched for tests by specifiying them after after `test`: `clj -M:cljd test ns.to.test1 ns.to.test2`.

If your tests live in extra source directories, you can use aliases as usual to include them like in `clj -M:test:cljd test` to enable the `:test` alias.

Tests selection using `dart test` flags can be controlled at the alias level. See this excerpt from a `deps.edn`:
```clojure
 :aliases {:cljd {:main-opts ["-m" "cljd.build"]}
           :test-widgets
           {:extra-paths ["test"]
            :cljd/opts {:dart-test-args ["-t" "widget"]}}}
```

When you combine several aliases, `:dart-test-args` are concatenated. However if you use `--` on the commande line, it will discard the computed `:dart-test-args`. Replace `--` by `++` to append instead.
