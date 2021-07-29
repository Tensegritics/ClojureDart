(ns cljd.build
  (:require [cljd.compiler :as compiler]
            [clojure.tools.cli :as ctc]
            [clojure.string :as str]
            [clojure.stacktrace :as st]))

(defn compile-core []
  (binding [compiler/*lib-path* (str (System/getProperty "user.dir") "/lib")]
    (compiler/compile-namespace 'cljd.core)))

(defn compile-cli
  [& {:keys [watch namespaces] :or {watch false}}]
  (let [current-path (System/getProperty "user.dir")]
    (println "== Compiling core.cljd -> core.dart ===")
    (time (compile-core))
    (loop []
      (doseq [n namespaces]
        (try (compiler/compile-namespace n)
             (catch Exception e
               (st/print-stack-trace e))))
      (when watch
        (println "Press ENTER to recompile files :")
        (when (pos? (.read (System/in)))
          (recur))))))

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
      (case action
        "watch" (compile-cli :namespaces namespaces :watch true)
        "compile" (compile-cli :namespaces namespaces :watch false)))))
