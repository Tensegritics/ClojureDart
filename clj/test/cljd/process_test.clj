(ns cljd.process-test
  (:require [cljd.build :as build]
            [clojure.test :refer [deftest is run-tests]]))

(defn- compile-process-calls []
  (let [calls (atom [])]
    (binding [build/*deps* {:cljd/opts {:kind :dart}}]
      (with-redefs [build/ensure-cljd-analyzer! (constantly ".")
                    build/exec (fn [& args]
                                 (swap! calls conj args)
                                 (when (= 2 (count @calls))
                                   (throw (ex-info "Captured compile subprocess options" {:captured true}))))]
        (try
          (build/compile-cli :namespaces [])
          (catch clojure.lang.ExceptionInfo error
            (when-not (:captured (ex-data error))
              (throw error))))))
    @calls))

(defn- java-command [expression]
  ["java" "-cp" (System/getProperty "java.class.path") "clojure.main" "-e" expression])

(deftest synchronous-pub-output-does-not-block-compilation
  (let [[[opts & command]] (compile-process-calls)
        ;; 1 MiB exceeds common anonymous-pipe capacity on every supported OS.
        expression "(do (let [chunk (byte-array 8192)] (dotimes [_ 128] (.write System/out chunk))) (.flush System/out))"
        ^Process child (apply build/exec (assoc opts :async true) (java-command expression))]
    (try
      (is (= ["dart" "pub" "get"] (vec command)))
      (is (nil? (:in opts)))
      (is (not (contains? opts :err)) "stderr keeps the existing inherited default")
      (is (instance? java.io.File (:out opts)))
      (let [finished (.waitFor child 20 java.util.concurrent.TimeUnit/SECONDS)]
        (is finished "large pub-style stdout must finish without a reader")
        (when finished
          (is (zero? (.exitValue child)))))
      (finally
        (when (.isAlive child)
          (.destroyForcibly child)
          (.waitFor child 5 java.util.concurrent.TimeUnit/SECONDS))))))

(deftest silent-pub-output-preserves-failure-status
  (let [[[opts]] (compile-process-calls)]
    (is (= 7 (apply build/exec opts (java-command "(System/exit 7)"))))))

(deftest analyzer-keeps-its-live-stdio-pipes
  (let [[_ [opts & command]] (compile-process-calls)]
    (is (= ["dart" "pub" "run" "bin/analyzer.dart"] (vec (take 4 command))))
    (is (= true (:async opts)))
    (is (contains? opts :in))
    (is (nil? (:in opts)))
    (is (contains? opts :out))
    (is (nil? (:out opts)))))

(defn -main [& _]
  (let [{:keys [fail error]} (run-tests 'cljd.process-test)]
    (when (pos? (+ fail error))
      (System/exit 1))))
