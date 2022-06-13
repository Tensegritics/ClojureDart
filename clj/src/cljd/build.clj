;   Copyright (c) Baptiste Dupuch & Christophe Grand . All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns cljd.build
  (:require [cljd.compiler :as compiler]
            [clojure.edn :as edn]
            [clojure.tools.cli.api :as deps]
            [clojure.string :as str]
            [clojure.stacktrace :as st]
            [clojure.java.io :as io]
            [clojure.java.classpath :as cp]))

(def ^:dynamic *ansi* false)
(def ^:dynamic *config* {})

(defn compile-core []
  (compiler/compile 'cljd.core))

(defn watch-dirs [dirs reload]
  (let [watcher (.newWatchService (java.nio.file.FileSystems/getDefault))
        reg1
        (fn [^java.io.File dir]
          (when (.isDirectory dir)
            [(.register (.toPath dir) watcher (into-array [java.nio.file.StandardWatchEventKinds/ENTRY_CREATE
                                                           java.nio.file.StandardWatchEventKinds/ENTRY_DELETE
                                                           java.nio.file.StandardWatchEventKinds/ENTRY_MODIFY
                                                           java.nio.file.StandardWatchEventKinds/OVERFLOW])
               (into-array [com.sun.nio.file.SensitivityWatchEventModifier/HIGH]))
             (.toPath dir)]))
        reg*
        (fn [dir]
          (eduction (keep reg1) (tree-seq some? #(.listFiles ^java.io.File %) dir)))]
    (loop [ks->dirs (into {} (mapcat reg*) dirs) to-reload #{}]
      (if-some [k (.poll watcher (if (seq to-reload) 10 1000) java.util.concurrent.TimeUnit/MILLISECONDS)]
        (let [events (.pollEvents k)] ; make sure we remove them, no matter what happens next
          (if-some [^java.nio.file.Path dir (ks->dirs k)]
            (let [[ks->dirs to-reload]
                  (reduce (fn [[ks->dirs to-reload] ^java.nio.file.WatchEvent e]
                            (let [f (some->> e .context (.resolve dir) .toFile)]
                              (if (and (some? f) (not (.isDirectory f))
                                    (re-matches #"[^.].*\.clj[dc]" (.getName f)))
                                [(into ks->dirs (some->> f reg*))
                                 (conj to-reload f)]
                                [ks->dirs to-reload])))
                    [ks->dirs to-reload] events)]
              (recur (cond-> ks->dirs (not (.reset k)) (dissoc ks->dirs k)) to-reload))
            (do
              (.cancel k)
              (recur ks->dirs to-reload))))
        (do
          (reload to-reload)
          (recur ks->dirs #{}))))))

(defn title [s]
  (if *ansi*
    (str "\u001B[1m" s "\u001B[0m")
    (str "=== " s " ===")))

(defn bright [s]
  (if *ansi*
    (str "\u001B[1m" s "\u001B[0m")
    s))

(defn green [s]
  (if *ansi*
    (str "\u001B[1;32m" s "\u001B[0m")
    s))

(defn red [s]
  (if *ansi*
    (str "\u001B[1;31m" s "\u001B[0m")
    s))

(defn success []
  (rand-nth
    [(str (green "  All clear! ") "👌")
     (str (green "  You rock! ") "🤘")
     (str (green "  Bravissimo! ") "👏")
     (str (green "  Easy peasy! ") "😎")
     (str (green "  I like when a plan comes together! ") "👨‍🦳")]))

(defn print-exception [e]
  (println (rand-nth
             [(str (red "Oh noes! ") "😵")
              (str (red "Something horrible happened! ") "😱")
              (str (red "$expletives ") "💩")
              (str (red "Keep calm and fix bugs! ") "👑")
              (str (red "What doesn’t kill you, makes you stronger. ") "🤔")
              (str (red "You’re gonna need a bigger boat! ") "🦈")]))
  (if-some [exprs (seq (::compiler/emit-stack (ex-data e)))]
    (let [exprs (into [] exprs)]
      (println (ex-message e))
      (run! prn (pop exprs))
      (println (title (pr-str (peek exprs))))
      (println (ex-message (ex-cause e))))
    (do
      (println (ex-message e))
      (st/print-stack-trace e))))

(defn timestamp []
  (.format (java.text.SimpleDateFormat. "@HH:mm" java.util.Locale/FRENCH) (java.util.Date.)))

(defn exec [& args]
  (let [{:keys [async in err out env] :as opts
         :or {env {}
              in java.lang.ProcessBuilder$Redirect/INHERIT
              err java.lang.ProcessBuilder$Redirect/INHERIT
              out java.lang.ProcessBuilder$Redirect/INHERIT}}
        (when (map? (first args)) (first args))
        [bin & args] (cond-> args opts next)
        pb (doto (ProcessBuilder. [])
             (-> .environment (.putAll env))
             (cond->
                 in (.redirectInput in)
                 err (.redirectError err)
                 out (.redirectOutput out)))
        os-is-windows (.startsWith (System/getProperty "os.name") "Windows")
        PATH (if os-is-windows "Path" "PATH")
        path (-> pb .environment (get PATH))
        bins (if os-is-windows [(str bin ".exe") (str bin ".bat")] [bin])
        full-bin
        (or
          (first
            (for [bin bins
                  dir (.split path java.io.File/pathSeparator)
                  :let [file (java.io.File. dir bin)]
                  :when (and (.isFile file) (.canExecute file))]
              (.getAbsolutePath file)))
          (throw (ex-info (str "Can't find " (str/join " nor " bins) " on PATH.")
                   {:bin bin :path path})))
        process (.start (doto pb (.command (into [full-bin] args))))]
    (if-not async
      (let [exit-code (.waitFor process)]
        (when-not (zero? exit-code) exit-code))
      process)))

(defn warm-up-libs-info! []
  (let [bin (:bin *config*)
        user-dir (System/getProperty "user.dir")
        cljd-dir (-> user-dir (java.io.File. ".clojuredart") (doto .mkdirs))
        lib-info-edn (java.io.File. cljd-dir "libs-info.edn")
        dart-tools-json (-> user-dir (java.io.File. ".dart_tool") (java.io.File. "package_config.json"))]
    (when-not (and (.exists lib-info-edn) (.exists dart-tools-json)
                (< (.lastModified dart-tools-json) (.lastModified lib-info-edn)))
      (let [analyzer-dart (java.io.File. cljd-dir "analyzer.dart")]
        (with-open [out (java.io.FileOutputStream. analyzer-dart)]
          (-> (Thread/currentThread) .getContextClassLoader (.getResourceAsStream "analyzer.dart") (.transferTo out)))
        (or
          (and
            (do
              (newline)
              (println (title "Adding dev dependencies"))
              (exec bin "pub" "add" "-d" "analyzer:^3.3.1"))
            (do
              (newline)
              (println (title "Upgrading dev dependencies"))
              (exec bin "pub" "upgrade" "analyzer:^3.3.1")))
          (do
            (newline)
            (println (title "Fetching dependencies"))
            (exec bin "pub" "get"))
          (do
            (newline)
            (println (title "Dumping type information (it may take a while)"))
            (when (exec {:out (java.lang.ProcessBuilder$Redirect/to lib-info-edn)}
                    bin "pub" "run" (.getPath analyzer-dart))
              (.delete lib-info-edn)
              (System/exit 1))))))))

(defn compile-cli
  [& {:keys [watch namespaces flutter] :or {watch false}}]
  (println (title "Warming up `.clojuredart/libs-info.edn`") "(helps us emit better code)")
  (warm-up-libs-info!)
  (binding [compiler/*hosted* true
            compiler/dart-libs-info (compiler/load-libs-info)]
    (newline)
    (println (title "Compiling cljd.core to Dart"))
    (compile-core)
    (let [dirs (into #{} (map #(java.io.File. %)) (:paths (:basis (deps/basis nil))))
          dirty-nses (volatile! #{})
          compile-nses
          (fn [nses]
            (let [nses (into @dirty-nses nses)]
              (vreset! dirty-nses #{})
              (when (seq nses)
                (newline)
                (println (title "Compiling to Dart...") (timestamp))
                (run! #(println " " %) (sort nses))
                (try
                  (compiler/recompile nses)
                  (println (success))
                  true
                  (catch Exception e
                    (vreset! dirty-nses nses)
                    (print-exception e)
                    false)))))
          compile-files
          (fn [^java.io.Writer flutter-stdin]
            (fn [files]
              (when (some->
                      (for [^java.io.File f files
                            :let [fp (.toPath f)]
                            ^java.io.File d dirs
                            :let [dp (.toPath d)]
                            :when (.startsWith fp dp)
                            :let [ns (compiler/peek-ns f)]
                            :when ns]
                        ns)
                      seq
                      compile-nses)
                (when flutter-stdin
                  (locking flutter-stdin
                    (doto flutter-stdin (.write "r") .flush))))))]
      (compile-nses namespaces)
      (when (or watch flutter)
        (newline)
        (when flutter
          (println (title (str/join " " (into ["Lauching flutter run"] flutter)))))
        (let [p (some->> flutter
                  (apply exec {:async true :in nil :env {"TERM" ""}} "flutter" "run"))]
          (try
            (let [flutter-stdin (some-> p .getOutputStream (java.io.OutputStreamWriter. "UTF-8"))]
              (when flutter-stdin
                (doto (Thread. #(while true
                                  (let [s (read-line)]
                                    (doto flutter-stdin
                                      (.write (case s "" "R" s))
                                      .flush))))
                  (.setDaemon true)
                  .start))
              (watch-dirs dirs (compile-files flutter-stdin)))
            (finally
              (some-> p .destroy))))))))

(defn init-project [opts main-ns bin-opts]
  (let [deps-cljd-opts (:cljd/opts (:basis (deps/basis nil)))
        bin (or (some-> deps-cljd-opts :kind name) (:target opts))
        main-ns (or (:main deps-cljd-opts) main-ns
                  (throw (Exception. "A namespace must be specified in deps.edn under :cljd/opts :main or as argument to init.")))
        libdir (doto (java.io.File. compiler/*lib-path*) .mkdirs)
        dir (java.io.File. (System/getProperty "user.dir"))
        project-name (.getName dir)
        project_name (str/replace project-name #"[- ]" "_")
        entry-point (case bin
                      "flutter" (java.io.File. libdir "main.dart")
                      "dart"
                      (java.io.File. "bin" (str project_name ".dart")))
        lib (compiler/relativize-lib (.getPath entry-point) (compiler/ns-to-lib main-ns))]
    (println "Initializing" (bright project-name) "as a" (bright bin) "project!")
    (or
      (case bin
        "flutter"
        (apply exec bin "create" "--project-name" project_name
          (concat bin-opts [(System/getProperty "user.dir")]))
        "dart"
        (apply exec bin "create" "--force" (concat bin-opts [(System/getProperty "user.dir")])))
      (spit (java.io.File. "cljd.edn") (pr-str {:main main-ns :bin bin}))
      (spit entry-point (str "export " (compiler/with-dart-str (compiler/write-string-literal lib)) " show main;\n"))
      (println "👍" (green "All setup!") "Let's write some cljd in" main-ns))))

(defn exit [status msg]
  (println msg)
  (System/exit status))

(defn parse-args [{opt-specs :options :keys [defaults] :as commands :or {defaults {}}} args]
  (let [[options & args]
        (if (false? opt-specs)
          (cons defaults args)
          (loop [args (seq args) options defaults]
            (if-some [[arg & more-args] args]
              (cond
                (= "--" arg) (cons options more-args)
                (.startsWith ^String arg "--")
                (if-some [{:keys [long id parser rf init] :as opt-spec}
                          (some (fn [{:keys [long] :as spec}] (when (= long arg) spec)) opt-specs)]
                  (let [v (if parser
                            (parser (first more-args))
                            (:value opt-spec true))
                        more-args (cond-> more-args parser next)
                        id (or id (keyword (subs long 2)))
                        v (if rf (rf (options id init) v) v)]
                    (recur more-args (assoc options id v)))
                  (throw (Exception. (str "Unknown option: " arg))))
                (.startsWith ^String arg "-")
                (if-some [{:keys [short long id parser rf init] :as opt-spec}
                          (some (fn [{:keys [short] :as spec}] (when (.startsWith ^String arg short) spec)) opt-specs)]
                  (let [args (cond->> args (not= arg short)
                                      (cons (cond->> (subs arg 2) (not parser) (str "-"))))
                        v (if parser
                            (parser (first more-args))
                            (:value opt-spec true))
                        more-args (cond-> more-args parser next)
                        id (or id (keyword (subs long 2)))
                        v (if rf (rf (options id init) v) v)]
                    (recur more-args (assoc options id v)))
                  (throw (Exception. (str "Unknown option: " arg))))
                :else (cons options args))
              (cons options nil))))]
    (if (:help options)
      (list* options :help args)
      (if (some string? (keys commands))
        (let [[command & args] args]
          (if-some [subcommands (commands command)]
            (list* options command (parse-args subcommands args))
            (throw (Exception. (str "Unknown command: " command)))))
        (list* options args)))))

(defn print-missing-config-warning [f-exists cmd]
  (when-not (or f-exists
              (contains? #{:help "init"} cmd))
    (newline)
    (println (title "WARNING: No cljd.edn file"))
    (println "Did you forget to run the following command?")
    (println (bright "clj -M -m cljd.build init <project_main_namespace>"))
    (newline)))

(defn print-help [{:keys [doc options] :as spec}]
  (when doc
    (newline)
    (println doc))
  (let [cmds (keep (fn [[cmd {:keys [doc]}]] (when (string? cmd) [cmd doc])) spec)]
    (when (and options (seq options))
      (newline)
      (println "Options")
      (doseq [{:keys [short long doc]} (sort-by #(or (:long %) (:short %)) options)]
        (println " " (some-> short bright) (some-> long bright) doc)))
    (when (seq cmds)
      (newline)
      (println "Actions")
      (doseq [[cmd doc] (sort-by first cmds)]
        (println " " (bright cmd))
        (some->> doc (println "   "))))))

(def help-spec {:short "-h" :long "--help" :doc "Print this help."})

(def commands
  {:doc "This program compiles Clojuredart files to dart files."
   :options [help-spec]
   "init" {:doc "Take the main namespace as argument. Set up the current clojure project as a ClojureDart/Flutter."
           :options [help-spec
                     {:short "-f" :long "--flutter" :id :target :value "flutter"}
                     {:short "-d" :long "--dart" :id :target :value "dart"}
                     ; TODO should be stored in a cljd.local.edn
                     #_{:short "-p" :long "--path"
                      :doc "Path to the flutter or dart install."}]
           :defaults {:target "flutter"}}
   "compile" {:doc "Compile the specified namespaces (or the main one by default) to dart."}
   "watch" {:doc "Like compile but keep recompiling in response to file updates."}
   "flutter" {:options false
              :doc "Like watch but hot reload the application in the simulator or device. All options are passed to flutter run."}})

(defn -main [& args]
  (let [f (java.io.File. "cljd.edn")
        f-exists (.exists f)
        config (if f-exists
                 (with-open [rdr (-> f io/reader java.io.PushbackReader.)]
                   (edn/read rdr))
                 {})]
    (binding [*ansi* (and (System/console) (get (System/getenv) "TERM"))
              *config* config
              compiler/*lib-path*
              (str (.getPath (java.io.File. "lib")) "/")]
      (let [[options cmd cmd-opts & args] (parse-args commands args)]
        (print-missing-config-warning f-exists cmd)
        (case cmd
          :help (print-help commands)
          "init"
          (let [[args [_ & target-args]] (split-with (complement #{"--"}) args)]
            (init-project cmd-opts (some-> (first args) symbol) target-args))
          ("compile" "watch")
          (compile-cli
            :namespaces (or (seq (map symbol args))
                          (some-> *config* :main list))
            :watch (= cmd "watch"))
          "flutter"
          (let [[args [dash & flutter-args]] (split-with #(not= "--" %) args)
                flutter-args (if dash flutter-args args)
                args (if dash args nil)]
            (compile-cli
              :namespaces
              (or (seq (map symbol args))
                (some-> *config* :main list))
              :flutter (vec flutter-args))))))))
