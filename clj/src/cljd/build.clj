;   Copyright (c) Baptiste Dupuch & Christophe Grand . All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns cljd.build
  (:require [cljd.compiler :as compiler]
            [clojure.tools.cli :as ctc]
            [clojure.string :as str]
            [clojure.stacktrace :as st]
            [clojure.java.io :as io]
            [clojure.java.classpath :as cp]))

(defn compile-core []
  (compiler/compile-namespace 'cljd.core))

(defn compile-cli
  [& {:keys [watch namespaces] :or {watch false}}]
  (binding [compiler/*lib-path* (str (System/getProperty "user.dir") "/lib/")
            compiler/*hosted* true
            compiler/dart-libs-info (compiler/load-libs-info)]
    (println "== Compiling core.cljd -> core.dart ===")
    (compile-core)
    (loop []
        (doseq [n namespaces]
          (try (compiler/compile-namespace n)
               (catch Exception e
                 (st/print-stack-trace e))))
        (when watch
          (println "Press ENTER to recompile files :")
          (when (pos? (.read (System/in)))
            (recur))))))


;; TODO : handle errors of processes
(defn warm-up-libs-info! []
  (let [user-dir (System/getProperty "user.dir")
        lib-info-edn (java.io.File. (str user-dir "/.clojuredart/libs-info.edn"))
        dart-tools-json (java.io.File. (str user-dir "/.dart_tool/package_config.json"))]
    (when-not (.exists dart-tools-json)
      (throw (ex-message "Run flutter pub get at your project root before using ClojureDart.")))
    (when (or (.mkdir (.getParentFile lib-info-edn))
            (.createNewFile lib-info-edn)
            (< (.lastModified lib-info-edn) (.lastModified dart-tools-json)))
      ;; TODO : big hack... change this some day
      (if-some [compiler-root-file (some #(when (or (re-matches #"(.*)ClojureDartPreview\/resources$" (.getAbsolutePath %))
                                                  (re-matches #"(.*)tensegritics\/clojuredart\/(.*)\/resources" (.getAbsolutePath %)))
                                            (-> % .getParentFile)) (cp/classpath))]
        (let [pb (doto (ProcessBuilder. ["flutter" "pub" "get"])
                   (.directory compiler-root-file))
              pb-analyzer (doto (ProcessBuilder. ["flutter" "pub" "run" (str (.getAbsolutePath compiler-root-file) "/bin/analyzer.dart") user-dir])
                            (.directory compiler-root-file)
                            (.redirectOutput lib-info-edn))
              _ (prn "== Download Clojuredart deps... ===")
              process (.start pb)]
          (with-open [r (io/reader (.getInputStream process))]
            (loop []
              (when (doto (.readLine r) prn)
                (recur)))
            (.waitFor process))
          (.destroy process)
          (prn "== Analyze your project dependencies... ===")
          (doto (.start pb-analyzer)
            .waitFor
            .destroy))
        (throw (ex-info "Can't find ClojureDart on your classpath" {:classpath (cp/classpath)}))))))

(def cli-options
  [["-v" nil "Verbosity level; may be specified multiple times to increase value"
    :id :verbosity
    :default 0
    :update-fn inc]
   ["-h" "--help"]])

(defn usage [options-summary]
  (->> ["This program compiles Clojuredart files to dart files."
        "It compiles all required namespace."
        ""
        "Usage: program-name [options] action file1 file2 file3..."
        ""
        "Options:"
        options-summary
        ""
        "Actions:"
        "  compile Compile files"
        "  watch   Compile files and re-compile when user press enter."
        ""
        "Please refer to the manual page for more information."]
       (str/join \newline)))

(defn error-msg [errors]
  (str "The following errors occurred while parsing your command:\n\n"
       (str/join \newline errors)))

(defn exit [status msg]
  (println msg)
  (System/exit status))

(defn validate-args
  "Validate command line arguments. Either return a map indicating the program
  should exit (with a error message, and optional ok status), or a map
  indicating the action the program should take and the options provided."
  [args]
  (let [{:keys [options arguments errors summary] :as kk} (ctc/parse-opts args cli-options)]
    (cond
      (:help options) ; help => exit OK with usage summary
      {:exit-message (usage summary) :ok? true}
      errors ; errors => exit with description of errors
      {:exit-message (error-msg errors)}
      ;; custom validation on arguments
      (and (< 1 (count arguments))
        (#{"compile" "watch"} (first arguments)))
      {:action (first arguments)
       :options options
       :namespaces (map symbol (next arguments))}
      :else ; failed custom validation => exit with usage summary
      {:exit-message (usage summary)})))

(defn -main [& args]
  (let [{:keys [action options exit-message namespaces ok?]} (validate-args args)]
    (prn action)
    (if exit-message
      (exit (if ok? 0 1) exit-message)
      (do (println "== Warming up `.clojuredart/libs-info.edn` (helps us emit better code)")
          (warm-up-libs-info!)
          (case action
            "watch" (compile-cli :namespaces namespaces :watch true)
            "compile" (compile-cli :namespaces namespaces :watch false))))))
