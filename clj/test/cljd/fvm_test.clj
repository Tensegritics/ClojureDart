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
  (#'build/exec-command root cwd windows? path bin args {}))

(defn- config! [root version]
  (spit (io/file root ".fvmrc") (str "{\"flutter\":\"" version "\"}")))

(defn- sdk-link! [root target]
  (let [link (io/file root ".fvm/flutter_sdk")]
    (.mkdirs (.getParentFile link))
    (cond
      (.startsWith (System/getProperty "os.name") "Windows")
      (let [process (.start (ProcessBuilder.
                             ["cmd.exe" "/c" "mklink" "/J" (str link) (str target)]))]
        (when-not (zero? (.waitFor process))
          (throw (ex-info "Unable to create test SDK junction." {}))))
      :else
      (java.nio.file.Files/createSymbolicLink (.toPath link) (.toPath target)
        (make-array java.nio.file.attribute.FileAttribute 0)))
    link))

(defn- delete-link! [link]
  ;; Delete the link itself, never traverse its target.
  (java.nio.file.Files/delete (.toPath link)))

(deftest sdk-is-used-directly-without-fvm-when-the-pin-matches
  (with-project [root]
    (let [sdk (executable root "versions/3.44.2/bin/flutter")
          target (.getParentFile (.getParentFile sdk))
          link (sdk-link! root target)
          cwd (doto (io/file root "helper") .mkdirs)]
      (try
        (config! root "3.44.2")
        (with-redefs [build/exec (fn [& _] (throw (Exception. "FVM must not run")))]
          (is (= [(str (io/file link "bin/flutter")) "pub" "get"]
                (command root cwd false "" "flutter" "pub" "get"))))
        (finally (delete-link! link))))))

(deftest pinned-sdk-batch-file-wins-over-a-global-windows-executable
  (with-project [root]
    (let [sdk (executable root "versions/3.44.2/bin/flutter.bat")
          target (.getParentFile (.getParentFile sdk))
          link (sdk-link! root target)
          tools (doto (io/file root "tools") .mkdirs)]
      (try
        (executable tools "flutter.exe")
        (config! root "3.44.2")
        (is (= [(str (io/file link "bin/flutter.bat")) "--version"]
              (command root root true (str tools) "flutter" "--version")))
        (finally (delete-link! link))))))

(deftest missing-stale-and-broken-links-are-repaired-once-at-project-root
  (doseq [state [:missing :stale :broken]]
    (with-project [root]
      (let [cwd (doto (io/file root "helper") .mkdirs)
            fvm (executable cwd "relative-tools/fvm")
            sdk (executable root "versions/3.44.2/bin/flutter")
            target (.getParentFile (.getParentFile sdk))
            old (doto (io/file root "versions/3.41.4") .mkdirs)
            calls (atom [])]
        (config! root "3.44.2")
        (when-not (= state :missing)
          (sdk-link! root old)
          (when (= state :broken) (.delete old)))
        (try
          (with-redefs [build/exec
                        (fn [opts & args]
                          (swap! calls conj [opts args])
                          (when-not (= state :missing)
                            (delete-link! (io/file root ".fvm/flutter_sdk")))
                          (sdk-link! root target)
                          nil)]
            (dotimes [_ 2]
              (is (= [(str (io/file root ".fvm/flutter_sdk/bin/flutter")) "pub" "get"]
                    (command root cwd false "relative-tools" "flutter" "pub" "get"))))
            (is (= [[{:dir root :env {}}
                     [(.getAbsolutePath fvm) "use" "3.44.2" "--skip-pub-get"]]] @calls)))
          (finally (delete-link! (io/file root ".fvm/flutter_sdk"))))))))

(deftest configuration-and-sync-failures-do-not-select-global-flutter
  (with-project [root]
    (let [bin (doto (io/file root "tools") .mkdirs)
          _ (executable bin "flutter")]
      (spit (io/file root ".fvmrc") "{broken")
      (is (thrown-with-msg? clojure.lang.ExceptionInfo #"Can't read" (command root root false (str bin) "flutter")))
      (spit (io/file root ".fvmrc") "{}")
      (is (thrown-with-msg? clojure.lang.ExceptionInfo #"nonblank" (command root root false (str bin) "flutter")))
      (config! root "3.44.2")
      (is (thrown-with-msg? clojure.lang.ExceptionInfo #"install FVM" (command root root false (str bin) "flutter")))
      (executable bin "fvm")
      (with-redefs [build/exec (constantly 7)]
        (is (thrown-with-msg? clojure.lang.ExceptionInfo #"fvm use failed" (command root root false (str bin) "flutter"))))
      (with-redefs [build/exec (constantly nil)]
        (is (thrown-with-msg? clojure.lang.ExceptionInfo #"without a usable SDK" (command root root false (str bin) "flutter")))))))

(deftest no-config-retains-sdk-and-path-fallback
  (with-project [root]
    (let [bin (doto (io/file root "tools") .mkdirs)
          global (executable bin "flutter")
          _ (executable bin "fvm")
          sdk (executable root ".fvm/flutter_sdk/bin/flutter")
          cwd (doto (io/file root "helper") .mkdirs)]
      (is (= [(str sdk) "pub" "run"] (command root cwd false (str bin) "flutter" "pub" "run")))
      (.delete sdk)
      (is (= [(str global)] (command root root false (str bin) "flutter"))))))

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
          flutter (executable bin "flutter.bat")]
      (is (= [(str flutter) "--version"] (command root root true (str bin) "flutter" "--version")))
      (is (= [(str flutter) "--version"] (command root root true (str bin) "flutter.bat" "--version")))
      (let [exe (executable bin "flutter.exe")]
        (is (= [(str exe)] (command root root true (str bin) "flutter")))))))

(deftest real-sync-runs-once-then-direct-sdk-preserves-process-options
  (with-project [root]
    (let [windows? (.startsWith (System/getProperty "os.name") "Windows")
          cwd (doto (io/file root "helper") .mkdirs)
          tools (doto (io/file root "tools with spaces") .mkdirs)
          fvm (executable tools (if windows? "fvm.cmd" "fvm"))
          flutter (executable root (if windows? "versions/3.44.2/bin/flutter.bat" "versions/3.44.2/bin/flutter"))
          target (.getParentFile (.getParentFile flutter))
          sync-log (io/file root "sync.txt")
          output (io/file root "stdout.txt")
          error (io/file root "stderr.txt")
          input (io/file root "stdin.txt")
          original-dir (System/getProperty "user.dir")
          parent-env (System/getenv "CLJD_FVM_TEST_VALUE")]
      (config! root "3.44.2")
      (.mkdirs (io/file root ".fvm"))
      (spit input "input-value\n")
      (spit fvm
        (cond
          windows?
          (str "@echo off\r\necho args:%*>>\"" sync-log "\"\r\n"
            "echo cwd:%CD%>>\"" sync-log "\"\r\n"
            "echo env:%CLJD_FVM_TEST_VALUE%>>\"" sync-log "\"\r\n"
            "mklink /J .fvm\\flutter_sdk \"" target "\" >nul\r\nexit /b %ERRORLEVEL%\r\n")
          :else
          (str "#!/bin/sh\necho args:$* >> \"" sync-log "\"\n"
            "echo cwd:$PWD >> \"" sync-log "\"\n"
            "echo env:$CLJD_FVM_TEST_VALUE >> \"" sync-log "\"\n"
            "/bin/ln -s \"" target "\" .fvm/flutter_sdk\n")))
      (spit flutter
        (cond
          windows?
          "@echo off\r\necho args:%*\r\necho cwd:%CD%\r\necho env:%CLJD_FVM_TEST_VALUE%\r\nset /p line=\r\necho stdin:%line%\r\necho error-value 1>&2\r\nexit /b 7\r\n"
          :else
          "#!/bin/sh\necho args:$*\necho cwd:$PWD\necho env:$CLJD_FVM_TEST_VALUE\nread line\necho stdin:$line\necho error-value >&2\nexit 7\n"))
      (try
        (System/setProperty "user.dir" (str root))
        (let [options {:dir "helper" :in input :out output :err error
                       :env {"PATH" (str tools) "Path" (str tools) "CLJD_FVM_TEST_VALUE" "child-value"}}
              process (build/exec (assoc options :async true) "flutter" "pub" "get")]
          (is (instance? Process process))
          (is (= 7 (.waitFor process)))
          (is (str/includes? (slurp sync-log) "args:use 3.44.2 --skip-pub-get"))
          (is (str/includes? (slurp sync-log) (str "cwd:" root)))
          (is (str/includes? (slurp sync-log) "env:child-value"))
          (is (str/includes? (slurp output) "args:pub get"))
          (is (str/includes? (slurp output) (str "cwd:" cwd)))
          (is (str/includes? (slurp output) "env:child-value"))
          (is (str/includes? (slurp output) "stdin:input-value"))
          (is (str/includes? (slurp error) "error-value"))
          (is (= parent-env (System/getenv "CLJD_FVM_TEST_VALUE")))
          (let [before (slurp sync-log)]
            (.delete fvm)
            (is (= 7 (build/exec options "flutter" "pub" "get")))
            (is (= before (slurp sync-log)))))
        (finally
          (System/setProperty "user.dir" original-dir)
          (when (.exists (io/file root ".fvm/flutter_sdk"))
            (delete-link! (io/file root ".fvm/flutter_sdk"))))))))

(defn -main [& _]
  (let [{:keys [fail error]} (run-tests 'cljd.fvm-test)]
    (System/exit (if (zero? (+ fail error)) 0 1))))
