(ns cljd.fvm-test
  (:require [cljd.build :as build]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing run-tests]]))

(defn- temp-dir []
  (.toFile (java.nio.file.Files/createTempDirectory "cljd-fvm-test-"
             (make-array java.nio.file.attribute.FileAttribute 0))))

(defmacro with-project [[root] & body]
  `(let [~root (temp-dir)]
     (try ~@body (finally (build/del-tree ~root)))))

(defn- executable [root path]
  (let [file (io/file root path)]
    (.mkdirs (.getParentFile file))
    (spit file "")
    (.setExecutable file true)
    file))

(defn- command [root cwd windows? path bin & args]
  (#'build/exec-command root cwd windows? path bin args))

(deftest fvm-needs-project-config-and-path-executable
  (with-project [root]
    (let [bin (doto (io/file root "tools") .mkdirs)
          fvm (executable bin "fvm")
          flutter (executable bin "flutter")]
      (testing "FVM on PATH alone does not override Flutter"
        (is (= [(.getAbsolutePath flutter) "pub" "get"]
              (command root root false (str bin) "flutter" "pub" "get"))))
      (spit (io/file root ".fvmrc") "{}")
      (testing "The FVM wrapper suffices without any global Flutter executable"
        (.delete flutter)
        (is (= [(.getAbsolutePath fvm) "flutter" "pub" "get"]
              (command root root false (str bin) "flutter" "pub" "get"))))
      (testing "Config without FVM retains SDK fallback"
        (.delete fvm)
        (let [sdk (executable root ".fvm/flutter_sdk/bin/flutter")]
          (is (= [(.getAbsolutePath sdk) "pub" "get"]
                (command root root false (str bin) "flutter" "pub" "get")))
          (.delete sdk)
          (let [flutter (executable bin "flutter")]
            (is (= [(.getAbsolutePath flutter) "pub" "get"]
                  (command root root false (str bin) "flutter" "pub" "get")))))))))

(deftest project-sdk-and-fvm-survive-analyzer-working-directory
  (with-project [root]
    (let [cwd (doto (io/file root ".clojuredart/cache/sha/cljd_helper") .mkdirs)
          sdk (executable root ".fvm/flutter_sdk/bin/flutter")
          fvm (executable cwd "relative-tools/fvm")]
      (is (= [(.getAbsolutePath sdk) "pub" "run"]
            (command root cwd false "relative-tools" "flutter" "pub" "run")))
      (spit (io/file root ".fvmrc") "{}")
      (is (= [(.getAbsolutePath fvm) "flutter" "pub" "run"]
            (command root cwd false "relative-tools" "flutter" "pub" "run"))))))

(deftest explicit-binaries-and-dart-are-not-rewritten
  (with-project [root]
    (let [bin (doto (io/file root "tools") .mkdirs)
          _ (executable bin "fvm")
          flutter (executable root "chosen/flutter")
          dart (executable bin "dart")]
      (spit (io/file root ".fvmrc") "{}")
      (is (= [(.getAbsolutePath flutter) "test"]
            (command root root false (str bin) (.getAbsolutePath flutter) "test")))
      (is (= [(.getAbsolutePath flutter) "test"]
            (command root root false (str bin) "chosen/flutter" "test")))
      (is (= [(.getAbsolutePath dart) "pub" "get"]
            (command root root false (str bin) "dart" "pub" "get")))
      (is (thrown-with-msg? clojure.lang.ExceptionInfo #"Can't find missing"
            (command root root false "" "missing"))))))

(deftest windows-executable-and-shim-selection
  (with-project [root]
    (let [bin (doto (io/file root "tools") .mkdirs)
          fvm (executable bin "fvm.cmd")
          flutter (executable bin "flutter.bat")]
      (spit (io/file root ".fvmrc") "{}")
      (is (= [(.getAbsolutePath fvm) "flutter" "--version"]
            (command root root true (str bin) "flutter" "--version")))
      (is (= [(.getAbsolutePath flutter) "--version"]
            (command root root true (str bin) "flutter.bat" "--version")))
      (is (= [(.getAbsolutePath flutter) "--version"]
            (command root root true (str bin) (.getAbsolutePath flutter) "--version")))
      (.delete fvm)
      (is (= [(.getAbsolutePath flutter) "--version"]
            (command root root true (str bin) "flutter" "--version")))
      (let [exe (executable bin "flutter.exe")]
        (is (= [(.getAbsolutePath exe)]
              (command root root true (str bin) "flutter")))))))

(deftest fvm-dispatch-preserves-process-options
  (with-project [root]
    (let [windows? (.startsWith (System/getProperty "os.name") "Windows")
          cwd (doto (io/file root ".clojuredart/helper") .mkdirs)
          tools (doto (io/file root "tools with spaces") .mkdirs)
          fvm (executable tools (if windows? "fvm.cmd" "fvm"))
          input (io/file root "stdin.txt")
          output (io/file root "stdout.txt")
          error (io/file root "stderr.txt")
          original-dir (System/getProperty "user.dir")
          env-key "CLJD_FVM_TEST_VALUE"
          parent-env (System/getenv env-key)]
      (spit (io/file root ".fvmrc") "{}")
      (spit input "input-value\n")
      (spit fvm (if windows?
                  "@echo off\r\necho args:%*\r\necho env:%CLJD_FVM_TEST_VALUE%\r\necho cwd:%CD%\r\nset /p line=\r\necho stdin:%line%\r\necho error-value 1>&2\r\nexit /b 7\r\n"
                  "#!/bin/sh\nprintf 'args:%s\\n' \"$*\"\nprintf 'env:%s\\n' \"$CLJD_FVM_TEST_VALUE\"\nprintf 'cwd:%s\\n' \"$PWD\"\nread line\nprintf 'stdin:%s\\n' \"$line\"\nprintf 'error-value\\n' >&2\nexit 7\n"))
      (try
        (System/setProperty "user.dir" (str root))
        (let [options {:dir ".clojuredart/helper" :in input :out output :err error
                       :env {"PATH" (str tools) "Path" (str tools) env-key "child-value"}}
              process (build/exec (assoc options :async true) "flutter" "pub" "get")]
          (is (instance? Process process))
          (is (= 7 (.waitFor process)))
          (is (str/includes? (slurp output) "args:flutter pub get"))
          (is (str/includes? (slurp output) "env:child-value"))
          (is (str/includes? (slurp output) (str "cwd:" cwd)))
          (is (str/includes? (slurp output) "stdin:input-value"))
          (is (str/includes? (slurp error) "error-value"))
          (is (= parent-env (System/getenv env-key)))
          (is (= 7 (build/exec options "flutter" "pub" "get"))))
        (finally (System/setProperty "user.dir" original-dir))))))

(defn -main [& _]
  (let [{:keys [fail error]} (run-tests 'cljd.fvm-test)]
    (System/exit (if (zero? (+ fail error)) 0 1))))
