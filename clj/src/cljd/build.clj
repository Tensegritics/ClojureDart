(ns cljd.build
  (:require [cljd.compiler :as compiler]
            [clojure.tools.cli :as ctc]
            [clojure.string :as str]))

(defn compile-core []
  (binding [compiler/*clj-path* ["clj/src"]
            compiler/*lib-path* (str (System/getProperty "user.dir") "/lib")]
    (compiler/compile-namespace 'cljd.core)))

(defn compile-files [files]
  (let [current-path (System/getProperty "user.dir")]
    (binding [compiler/*clj-path* [(str current-path "/clj/src") #_(str "/tmp/test/clj/src")]
              compiler/*lib-path* (str current-path "/lib")]
      (doseq [p compiler/*clj-path*
              f files
              :let [file-path (.getAbsolutePath f)
                    ns-name (-> (re-matches #"(.*)\.clj[cd]?" (subs file-path (inc (count p))))
                              second
                              (str/replace #"[\/_]" {"/" "." "_"  "-"})
                              symbol)]]
        (do
          (println (str "Compiling " file-path " to dart file."))
          (compiler/compile-namespace ns-name))))))

(defn compile-cli
  "Assumes valid cljd files."
  [& {:keys [watch files] :or {watch false}}]
  (let [current-path (System/getProperty "user.dir")]
    (println "== Compiling core.cljd -> core.dart ===")
    (time (compile-core))
    (compile-files files)
    (when watch
      (println "Press ENTER to recompile files :")
      (while (.read (System/in))
        (println "== Recompiling files")
        (compile-files files)
        (println "Press ENTER to recompile files :")))))

(def cli-options
  [["-v" nil "Verbosity level; may be specified multiple times to increase value"
    :id :verbosity
    :default 0
    :update-fn inc]
   ["-f" "--file NAME" "File names to read"
    :multi true
    :default []
    :update-fn conj]
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
       :files (when (every? #(let [f (java.io.File. %)
                                   curr-dir (System/getProperty "user.dir")]
                               (when-not (.exists f)
                                 (exit 1 (str "File `" curr-dir "/" % "` does not exists.")))
                               (when-not (.isFile f)
                                 (exit 1 (str "File `" curr-dir "/" % "` is not a file.")))
                               (when-not (re-matches #".*\.(clj[cd]?)" %)
                                 (exit 1 (str "File name `" curr-dir "/" % "` must be valid, e.g. : clj/src/a/b/c/name.clj[cd]?")))
                               true) (next arguments))
                (map #(java.io.File. %) (next arguments)))}
      :else ; failed custom validation => exit with usage summary
      {:exit-message (usage summary)})))

(defn -main [& args]
  (let [{:keys [action options exit-message files ok?]} (validate-args args)]
    (prn action)
    (if exit-message
      (exit (if ok? 0 1) exit-message)
      (case action
        "watch" (compile-cli :files files :watch true)
        "compile" (compile-cli :files files :watch false)))))
