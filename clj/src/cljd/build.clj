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
            [clojure.tools.deps :as deps]
            [clojure.string :as str]
            [clojure.stacktrace :as st]
            [clojure.java.io :as io]))

(def ^:dynamic *ansi* false)
(def ^:dynamic *deps*)

(defn compile-core []
  (compiler/compile 'cljd.core))

(defn watch-dirs-until [stop? init dirs reload]
  (with-open [watcher (.newWatchService (java.nio.file.FileSystems/getDefault))]
    (let [reg1
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
      (loop [ks->dirs (into {} (mapcat reg*) dirs) to-reload #{} state init]
        (if (stop? state)
          state
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
                  (recur (cond-> ks->dirs (not (.reset k)) (dissoc ks->dirs k)) to-reload state))
                (do
                  (.cancel k)
                  (recur ks->dirs to-reload state))))
            (recur ks->dirs #{} (reload state to-reload))))))))

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
  (if-some [[form & parents] (seq (::compiler/emit-stack (ex-data e)))]
    (let [toplevel (last (take-while #(not (and (seq? %) (= 'ns (first %)))) parents))]
      (println (ex-message e))
      (println "⛔️" (title (ex-message (ex-cause e))))
      (if toplevel
        (do
          (println (title "Faulty subform and/or expansion") (pr-str form))
          (println (title "While compiling") (pr-str toplevel)))
        (println (title "Faulty form") (pr-str form))))
    (do
      (println "⛔️" (title (ex-message e)))
      (st/print-stack-trace e))))

(defn timestamp []
  (.format (java.text.SimpleDateFormat. "@HH:mm:ss" (java.util.Locale/getDefault)) (java.util.Date.)))

(defn exec
  "If first arg is a map, it's an option map.
   Supported options:
   :in, :out and :err are either nil, a File or a ProcessBuilder$Redirect -- default to ProcessBuilder$Redirect/INHERIT
   :env a map of extra environment variables (strings to strings)
   :dir the current directory
   :async a boolean (defaults to false), when true exec returns immediatly.
   When async, exec returns a Process.
   When not async exec returns nil (for exit code 0) or non-zero exit code"
  [& args]
  (let [{:keys [async in err out env dir] :as opts
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
                 out (.redirectOutput out)
                 dir (.directory (io/file dir))))
        os-is-windows (.startsWith (System/getProperty "os.name") "Windows")
        PATH (if os-is-windows "Path" "PATH")
        ^String path (or
                       (-> pb .environment (get PATH))
                       (throw (Exception. (str "No " PATH " variable found in the environment. But PATH is: " (-> pb .environment (get "PATH")) ". Meanwhile System/getenv returns: " (System/getenv PATH) ))))
        bins (if os-is-windows [(str bin ".exe") (str bin ".bat")] [bin])
        full-bin
        (or
          (first
            (for [bin bins
                  dir (cons ".fvm/flutter_sdk/bin" (.split path java.io.File/pathSeparator))
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

(defn del-tree [^java.io.File f]
  (run! del-tree
    (when-not (java.nio.file.Files/isSymbolicLink (.toPath f))
      (.listFiles f)))
  (.delete f))

(defn ensure-cljd-analyzer! []
  (let [cljd-sha (get-in *deps* [:libs 'tensegritics/clojuredart :git/sha])
        parent-dir (-> (System/getProperty "user.dir") (java.io.File. ".clojuredart") (java.io.File. "cache")
                     (java.io.File. (or cljd-sha "dev")))
        parent-dir (doto parent-dir (cond-> (not cljd-sha) del-tree) .mkdirs)
        analyzer-dir (doto (java.io.File. parent-dir "cljd_helper") .mkdirs)
        analyzer-dart (-> analyzer-dir (java.io.File. "bin") (doto .mkdirs) (java.io.File. "analyzer.dart"))
        pubspec-yaml (-> analyzer-dir (java.io.File. "pubspec.yaml"))]
    (when-not (.exists pubspec-yaml)
      (with-open [out (java.io.FileOutputStream. pubspec-yaml)]
        (-> "name: cljd_helper\n\nenvironment:\n  sdk: '>=2.17.0 <4.0.0'\n"
          (.getBytes java.nio.charset.StandardCharsets/UTF_8)
          java.io.ByteArrayInputStream.
          (.transferTo out))))
    (when-not (.exists analyzer-dart)
      (exec {:dir analyzer-dir} (some-> *deps* :cljd/opts :kind name) "pub" "add" "analyzer:5.13.0")
      (with-open [out (java.io.FileOutputStream. analyzer-dart)]
        (-> (Thread/currentThread) .getContextClassLoader (.getResourceAsStream "analyzer.dart") (.transferTo out))))
    (.getPath analyzer-dir)))

(defmacro with-taps [fns & body]
  `(let [fns# [~@fns]]
     (try
       (run! add-tap fns#)
       ~@body
       (finally
         (run! remove-tap fns#)))))

(defn compile-cli
  [& {:keys [watch namespaces flutter] :or {watch false}}]
  (let [user-dir (System/getProperty "user.dir")
        analyzer-dir (ensure-cljd-analyzer!)]
    (exec {:in nil :out nil} (some-> *deps* :cljd/opts :kind name) "pub" "get")
    (with-taps
      [(fn [x]
         (case (::compiler/msg-kind x)
           :compile-ns
           (println " - compiling" (:ns x) (timestamp))
           :dump-ns
           (println " - writing compiled" (:ns x) "to" (:lib x) (timestamp))))]
      (binding [compiler/*hosted* true
                compiler/*dart-version* nil
                compiler/analyzer-info nil]
        (set! compiler/analyzer-info
          (compiler/mk-live-analyzer-info (exec {:async true :in nil :out nil :dir analyzer-dir}
                                            (some-> *deps* :cljd/opts :kind name)
                                            "pub" "run" "bin/analyzer.dart" user-dir)))
        (newline)
        (println (title (str "Compiling cljd.core to Dart "
                          (case compiler/*dart-version*
                            :dart2 "2"
                            :dart3 "3"))))
        (compile-core)
        (let [dirs (into #{} (map #(java.io.File. %))
                     (concat (:paths *deps*)
                       (mapcat (fn [{:keys [local/root paths]}]
                                 (when root paths))
                         (vals (:libs *deps*)))))
              dirty-nses (volatile! #{})
              recompile-count (volatile! 0)
              compile-nses
              (fn [nses]
                (let [nses (into @dirty-nses nses)]
                  (vreset! dirty-nses #{})
                  (when (seq nses)
                    (newline)
                    (println (title "Compiling to Dart...") (timestamp))
                    (run! #(println " " %) (sort nses))
                    (try
                      (compiler/recompile nses (vswap! recompile-count inc))
                      (println (success) (timestamp))
                      true
                      (catch Exception e
                        (vreset! dirty-nses nses)
                        (print-exception e)
                        false)))))
              compilation-success (compile-nses namespaces)]
          (if (or watch flutter)
            (let [compile-files
                  (fn [^java.io.Writer flutter-stdin]
                    (fn [_ files]
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
              (watch-dirs-until true? (or (boolean compilation-success) :first)
                dirs
                (fn [state changes]
                  (if (or (= :first state) (seq changes))
                    (boolean (compile-nses namespaces))
                    state)))
              (newline)
              (when flutter
                (println (title (str/join " " (into ["Launching flutter run"] flutter)))))
              (let [p (some->> flutter
                        (apply exec {:async true :in nil :out nil :env {"TERM" ""}} "flutter" "run"))]
                (try
                  (let [flutter-stdin (some-> p .getOutputStream (java.io.OutputStreamWriter. "UTF-8"))
                        flutter-stdout (some-> p .getInputStream (java.io.InputStreamReader. "UTF-8") java.io.BufferedReader.)
                        ansi *ansi*]
                    ; Unimplemented handling of missing static target
                    (when (and flutter-stdin flutter-stdout)
                      (doto (Thread. #(while true
                                        (let [s (read-line)]
                                          (locking flutter-stdin
                                            (doto flutter-stdin
                                              (.write (case s "" "R" s))
                                              .flush)))))
                        (.setDaemon true)
                        .start)
                      (doto (Thread.
                              #(binding [*ansi* ansi]
                                 (loop [state :idle]
                                   (when-some [line (.readLine flutter-stdout)]
                                     (when-not (= :reload-failed state)
                                       (println line))
                                     (let [line (.trim line)]
                                       (case state
                                         :idle
                                         (condp = line
                                           "Performing hot reload..." (recur :reloading)
                                           "Performing hot restart..." (recur :restarting)
                                           (recur state))
                                         :reloading
                                         (condp = line
                                           "Unimplemented handling of missing static target" (recur :reload-failed)
                                           (recur state))
                                         :reload-failed
                                         (if (re-matches #"Reloaded .+ of .+ libraries in .+." line)
                                           (do
                                             (newline)
                                             (println (bright "Hot reload failed, attempting hot restart!"))
                                             (locking flutter-stdin
                                               (doto flutter-stdin
                                                 (.write "R")
                                                 .flush))
                                             (recur :restarting))
                                           (recur state))
                                         :restarting
                                         (if (re-matches #"Restarted application .+" line)
                                           (recur :idle)
                                           (recur state))))))))
                        (.setDaemon true)
                        .start))
                    (watch-dirs-until (fn [_] (some-> p .isAlive not)) nil dirs (compile-files flutter-stdin))
                    (when p
                      (println (str "💀 Flutter sub-process exited with " (.exitValue p)))))
                  (finally
                    (some-> p .destroy)))))
            (or compilation-success (System/exit 1))))))))

(defn test-cli [& {:keys [namespaces dart-test-args]}]
  (when (compile-cli :namespaces namespaces)
    (newline)
    (println (title "Running tests..."))
    (let [bin (some-> *deps* :cljd/opts :kind name)]
      (System/exit (or (apply exec {:in nil #_#_:out nil} bin "test" dart-test-args) 0)))))

(defn gen-entry-point []
  (let [deps-cljd-opts (:cljd/opts *deps*)
        bin (some-> deps-cljd-opts :kind name)
        main-ns (or (:main deps-cljd-opts)
                  (throw (Exception. "A namespace must be specified in deps.edn under :cljd/opts :main or as argument to init.")))
        libdir (doto (java.io.File. compiler/*lib-path*) .mkdirs)
        dir (java.io.File. (System/getProperty "user.dir"))
        project-name (.getName dir)
        project_name (str/replace project-name #"[- ]" "_")
        entry-point (case bin
                      "flutter" (java.io.File. libdir "main.dart")
                      "dart" (java.io.File. "bin" (str project_name ".dart")))
        lib (compiler/relativize-lib (.getPath entry-point) (compiler/ns-to-lib main-ns))]
    (spit entry-point (str "export " (compiler/with-dart-str (compiler/write-string-literal lib)) " show main;\n"))))

(defn init-project [bin-opts]
  (let [deps-cljd-opts (:cljd/opts *deps*)
        bin (or (some-> deps-cljd-opts :kind name)
              (throw (Exception. "A project kind (:dart or :flutter) must be specified in deps.edn under :cljd/opts :kind.")))
        main-ns (or (:main deps-cljd-opts)
                  (throw (Exception. "A namespace must be specified in deps.edn under :cljd/opts :main.")))
        dir (java.io.File. (System/getProperty "user.dir"))
        project-name (.getName dir)
        ;; aggressive project_name munging TODO make this smarter at some point
        project_name (-> project-name str/lower-case (str/replace #"([^a-z0-9_]+)" "_") (->> (str "cljd_")))]
    (with-open [w (io/writer ".gitattributes" :append true)]
      (binding [*out* w]
        (newline)
        (println "# Syntax highlighting for ClojureDart files")
        (println "*.cljd linguist-language=Clojure")))
    (doto (java.io.File. compiler/*lib-path*) .mkdirs)
    (println "Initializing" (bright project-name) "as a" (bright bin) "project!")
    (or
     (case bin
       "flutter"
       (apply exec bin "create" "--project-name" project_name
              (concat bin-opts [(System/getProperty "user.dir")]))
       "dart"
       (apply exec bin "create" "--force" (concat bin-opts [(System/getProperty "user.dir")])))
     (gen-entry-point)
     (with-open [w (io/writer ".gitignore" :append true)]
      (binding [*out* w]
        (newline)
        (run! println ["# ClojureDart"
                       ".cpcache/"
                       ".clojuredart/"
                       "lib/cljd-out/"
                       "test/cljd-out/"])))
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
                (#{"--" "++"} arg) (cons options args)
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
        (if-some [[command & args] (seq args)]
          (if-some [subcommands (commands command)]
            (list* options command (parse-args subcommands args))
            (throw (Exception. (str "Unknown command: " command))))
          (list options :help))
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

(defn upgrade-cljd []
  (let [hashes (with-open [rdr (-> "https://raw.githubusercontent.com/Tensegritics/ClojureDart/main/.hashes" java.net.URL. io/reader)]
                 (into [] (comp (remove str/blank?) (map #(str \" % \"))) (line-seq rdr)))
        latest (peek hashes)
        pattern (re-pattern (str "(?<!#_)(?:" (str/join "|" hashes ) ")"))
        versions-found (atom 0)
        versions-replaced (atom 0)]
    (when-not (seq hashes)
      (throw (RuntimeException. "No past versions retrieved, can't update!")))
    (-> "deps.edn"
      slurp
      (str/replace pattern (fn [v]
                             (swap! versions-found inc)
                             (str latest
                               (when-not (= v latest)
                                 (swap! versions-replaced inc)
                                 (str " #_" v)))))
      (->> (spit "deps.edn")))
    (case @versions-replaced
      0 (if (= @versions-found @versions-replaced)
          (do
            (println "No known recommended versions found in deps.edn, please update manually to the latest recommended (next time it will work!):")
            (binding [*print-namespace-maps* false]
              (prn {'tensegritics/clojuredart
                    {:git/url "https://github.com/tensegritics/ClojureDart.git"
                     :sha (re-find #"[^\"]+" latest)}})))
          (println "Already up-to-date!"))
      1 (println  "1 version upgraded to" latest)
      (println  @versions-replaced "versions upgraded to" latest))))

(def help-spec {:short "-h" :long "--help" :doc "Print this help."})

(def commands
  {:doc "This program compiles Clojuredart files to dart files."
   :options [help-spec]
   "init" {:doc "Set up the current clojure project as a ClojureDart/Flutter project according to :cljd/opts in deps.edn."
           :options false
           #_#_#_#_
           :options [help-spec
                     {:short "-f" :long "--flutter" :id :target :value "flutter"}
                     {:short "-d" :long "--dart" :id :target :value "dart"}
                     ; TODO should be stored in a cljd.local.edn
                     #_{:short "-p" :long "--path"
                      :doc "Path to the flutter or dart install."}]
           :defaults {:target "flutter"}}
   "compile" {:doc "Compile the specified namespaces (or the main one by default) to dart."}
   "clean" {:doc "When there's something wrong with compilation, erase all ClojureDart build artifacts.\nConsider running flutter clean too."}
   "help" {:doc (:doc help-spec)}
   "test" {:doc "Run specified test namespaces (or all by default)."}
   "upgrade" {:doc "Upgrade cljd to latest version."}
   "watch" {:doc "Like compile but keep recompiling in response to file updates."}
   "flutter" {:options false
              :doc "Like watch but hot reload the application in the simulator or device. All options are passed to flutter run."}})

(defn sha256 [string]
  (let [digest (.digest (java.security.MessageDigest/getInstance "SHA-256") (.getBytes string "UTF-8"))]
    (apply str (map (partial format "%02x") digest))))

(defn find-pubspec [{:keys [:deps/root paths]}]
  (if root
    (let [f (java.io.File. root "pubspec.yaml")]
      (when (.exists f)
        (slurp f)))
    (some
      (fn [path]
        (let [f (java.io.File. path)]
          (if (.isDirectory f)
            (let [f (java.io.File. f "pubspec.yaml")]
              (when (.exists f)
                (slurp f)))
            (with-open [in (-> f io/input-stream java.util.jar.JarInputStream.)]
              (loop []
                (when-some [e (.getNextJarEntry in)]
                  (if (= "pubspec.yaml" (.getName e))
                    (slurp in) ; slurp closes `in` but it's ok it's the last action
                    (recur))))))))
      paths)))

(defn sync-pubspec! []
  (let [parser (org.yaml.snakeyaml.Yaml.)
        existing-deps (into {}
                        (for [[name {:strs [path]}] (get (.load parser (slurp "pubspec.yaml")) "dependencies")
                              :let [[_ sha] (some->> path (re-matches #"^\.clojuredart/deps/(.+)"))]
                              :when sha]
                          [[name sha] (-> path java.io.File. .exists)]))
        declared-deps (into {}
                        (for [pubspec (keep find-pubspec (vals (deps/resolve-deps *deps* {})))
                              :let [{:strs [name]} (.load parser pubspec)
                                    sha (sha256 pubspec)]]
                          [[name sha] pubspec]))
        ; the (filter existing-deps) is to remove "bridge" deps which don't exist on disk
        ; typically when getting an updated pubspec.yaml from scm (git)
        deps-to-remove (keys (transduce (filter existing-deps) dissoc existing-deps (keys declared-deps)))
        deps-to-add (transduce (filter existing-deps) dissoc declared-deps existing-deps)]

    (when-some [names (seq (map first deps-to-remove))]
      (apply exec {:in nil #_#_:out nil} (some-> *deps* :cljd/opts :kind name) "pub" "remove"
        names)
      (run! #(del-tree (java.io.File. ".clojuredart/deps" (second %))) deps-to-remove))

    (when-some [coords (seq (for [[name sha] (keys deps-to-add)]
                              (str name ":{\"path\":\".clojuredart/deps/" sha "\"}")))]
      (doseq [[[name sha] pubspec] deps-to-add
              :let [f (java.io.File. ".clojuredart/deps" sha)]]
        (.mkdirs f)
        (spit (java.io.File. f "pubspec.yaml") pubspec))
      (apply exec {:in nil #_#_:out nil} (some-> *deps* :cljd/opts :kind name) "pub" "add" "--directory=."
        coords))))

(defn ensure-test-dev-dep! []
  (let [parser (org.yaml.snakeyaml.Yaml.)]
    (when-not (get-in (.load parser (slurp "pubspec.yaml")) ["dev_dependencies" "test"])
      (exec {:in nil #_#_:out nil} (some-> *deps* :cljd/opts :kind name) "pub" "add" "--dev" "test"))))

(defn merge-cljd-opts [cljd-opts alias-cljd-opts]
  (reduce-kv
    (fn [cljd-opts k v]
      (if-some [[_ prefix base] (re-matches #"(replace|extra)-(.+)" (name k))]
        (let [k (keyword base)]
          (case prefix
            "replace" (assoc cljd-opts k v)
            "extra" (merge-with into cljd-opts {k v})))
        (let [v' (cljd-opts k v)]
          (when-not (= v' v)
            (throw (Exception. (str "Two different values provided in :cljd/opts for " k ": " (pr-str v) " and " (pr-str v')))))
          (assoc cljd-opts k v))))
    cljd-opts
    alias-cljd-opts))

(defn runtime-basis
  "Load the runtime execution basis context and return it."
  []
  (when-let [f (java.io.File. (System/getProperty "clojure.basis"))]
    (if (and f (.exists f))
      (let [{:keys [aliases basis-config] :as basis} (deps/slurp-deps f)]
        (assoc basis
          :cljd/opts
          (reduce merge-cljd-opts (:cljd/opts basis) (keep (comp :cljd/opts aliases) (:aliases basis-config)))))
      (throw (IllegalArgumentException. "No basis declared in clojure.basis system property")))))


(defn -main [& args]
  (binding [*ansi* (and (System/console) (get (System/getenv) "TERM"))
            compiler/*lib-path*
            (str (.getPath (java.io.File. "lib")) "/")]
    (binding [*deps* (runtime-basis)]
      (let [[options cmd cmd-opts & args] (parse-args commands args)]
        (case cmd
          ("compile" "watch" "flutter" "test") (sync-pubspec!)
          nil)
        (case cmd
          :help (print-help commands)
          "help" (print-help commands)
          "init" (init-project args)
          ("compile" "watch")
          (compile-cli
            :namespaces (or (seq (map symbol args))
                          (some-> *deps* :cljd/opts :main list))
            :watch (= cmd "watch"))
          "test"
          (let [[nses [delim & dart-test-args]] (split-with (complement #{"--" "++"}) args)
                nses (map symbol nses)
                opts-dart-test-args (-> *deps* :cljd/opts :dart-test-args)]
            (ensure-test-dev-dep!)
            (test-cli
              :dart-test-args (case delim
                                "++" (concat opts-dart-test-args dart-test-args)
                                (or dart-test-args opts-dart-test-args))
              :namespaces
              (or (seq nses)
                (let [wd (-> (java.io.File. ".") .getCanonicalFile .toPath)]
                  (for [root (:classpath-roots *deps*)
                        :when (-> root java.io.File. .getCanonicalFile .toPath (.startsWith wd))
                        ^java.io.File file (tree-seq
                                             some? #(.listFiles ^java.io.File %)
                                             (java.io.File. root))
                        :when (re-matches #"[^.].*\.clj[cd]" (.getName file))]
                    (compiler/peek-ns file))))))
          "upgrade"
          (upgrade-cljd)
          "flutter"
          (let [[args [delim & flutter-args]] (split-with (complement #{"--" "++"}) args)
                flutter-args (if delim flutter-args args)
                args (if delim args nil)
                opts-flutter-run-args (-> *deps* :cljd/opts :flutter-run-args)]
            (compile-cli
              :namespaces
              (or (seq (map symbol args))
                (some-> *deps* :cljd/opts :main list))
              :flutter (vec (case delim
                              "++" (concat opts-flutter-run-args flutter-args)
                              flutter-args))))
          "clean"
          (do
            (del-tree (java.io.File. "lib/cljd-out"))
            (del-tree (java.io.File. "test/cljd-out"))
            (del-tree (java.io.File. ".clojuredart"))
            (sync-pubspec!)
            (println "ClojureDart build state succesfully cleaned!")
            (case (some-> *deps* :cljd/opts :kind)
              :flutter (println "If problems persist, try" (bright "flutter clean"))
              nil)))))))
