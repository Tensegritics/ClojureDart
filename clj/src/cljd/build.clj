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
            [clojure.java.io :as io]
            [clojure.core.server :as server]))

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

(defn muted [s]
  (if *ansi*
    (str "\u001B[2m" s "\u001B[0m")
    s))

(defn inverted [s]
  (if *ansi*
    (str "\u001B[7m" s "\u001B[0m")
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
        path (if os-is-windows
               (or (-> pb .environment (get "Path")) (-> pb .environment (get "PATH")))
               (-> pb .environment (get "PATH")))
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

(defn try-ensure-cljd-analyzer! [{:keys [pubspec analyzer-dep resource-name]}]
  (let [cljd-sha (get-in *deps* [:libs 'tensegritics/clojuredart :git/sha])
        parent-dir (-> (System/getProperty "user.dir") (java.io.File. ".clojuredart") (java.io.File. "cache")
                     (java.io.File. (or cljd-sha "dev")))
        parent-dir (doto parent-dir (cond-> (not cljd-sha) del-tree) .mkdirs)
        analyzer-dir (doto (java.io.File. parent-dir "cljd_helper") .mkdirs)
        analyzer-dart (-> analyzer-dir (java.io.File. "bin") (doto .mkdirs) (java.io.File. "analyzer.dart"))
        pubspec-yaml (-> analyzer-dir (java.io.File. "pubspec.yaml"))]
    (when-not (.exists pubspec-yaml)
      (with-open [out (java.io.FileOutputStream. pubspec-yaml)]
        (-> ^String pubspec
          (.getBytes java.nio.charset.StandardCharsets/UTF_8)
          java.io.ByteArrayInputStream.
          (.transferTo out))))
    (when-not (.exists analyzer-dart)
      ; to move beyond analyzer 7.0.0 we need to move the min version to at least 6.5.2
      ; (changes to our analyzer.dart are required and they would break compat with 6.2.0)
      ; we are willing to support 6.2 for now as it means supporting dartlang < 3.3 (3.3 mandatory starting 6.3.0)
      ; maybe we should consider having multiple copies of our own analyzer.dart
      (if (exec {:dir analyzer-dir} (some-> *deps* :cljd/opts :kind name) "pub" "add" analyzer-dep)
        ; `pub add` failure
        (del-tree analyzer-dir)
        ; `pub add` worked
        (with-open [out (java.io.FileOutputStream. analyzer-dart)]
          (-> (Thread/currentThread) .getContextClassLoader (.getResourceAsStream ^String resource-name) (.transferTo out)))))
    (when (.exists analyzer-dir)
      (.getPath analyzer-dir))))

(defn ensure-cljd-analyzer! []
  (some try-ensure-cljd-analyzer!
    [{:pubspec "name: cljd_helper\n\nenvironment:\n  sdk: '>=3.0.0 <4.0.0'\n"
      :analyzer-dep "analyzer:'>=6.2.0 <7.0.0'"
      :resource-name "analyzer.dart"}
     {:pubspec "name: cljd_helper\n\nenvironment:\n  sdk: '>=2.17.0 <4.0.0'\n"
      :analyzer-dep "analyzer:5.13.0"
      :resource-name "analyzer_legacy_dart2.dart"}]))

(defmacro with-taps [fns & body]
  `(let [fns# [~@fns]]
     (try
       (run! add-tap fns#)
       ~@body
       (finally
         (run! remove-tap fns#)))))

(defn eval-to-repl [repltag expr-or-throwable *compiler-state trigger-reload p]
  (loop [expr-or-throwable expr-or-throwable
         throwable-phase (when (instance? Throwable expr-or-throwable)
                           :read-source)]

    (assert (= (some? throwable-phase)
              (instance? Throwable expr-or-throwable)))

    (let [expr (if throwable-phase
                 `(throw '~(-> expr-or-throwable
                             Throwable->map
                             (assoc :phase throwable-phase)))
                 expr-or-throwable)
          {N :recompile-count} (swap! *compiler-state update :recompile-count inc)
          throwable
          (try
            (compiler/recompile-form expr N repltag)
            (trigger-reload)
            nil
            (catch Throwable t t))]
      (when throwable
        (if throwable-phase
          ; failed handling error, throw it for good
          (deliver p throwable)
          (recur throwable :compilation))))))

(defn repl [*repl-states {:keys [dart-version analyzer-info *compiler-state trigger-reload
                                 repltag]}]
  (binding [compiler/*dart-version* dart-version
            compiler/*hosted* true
            compiler/analyzer-info analyzer-info
            compiler/dynamic-warning compiler/on-dynamic-warn
            *in* (clojure.lang.LineNumberingPushbackReader. *in*)]
    (compiler/with-cljd-reader
      (try
        (binding [compiler/*current-ns* 'cljd.core]
          (loop [expr-or-throwable '(ns cljd.user
                                      (:require #_[cljd.flutter.repl-impl]
                                                [cljd.flutter.repl :refer [pick! mount!]]
                                                [cljd.flutter :as f]
                                                ["package:flutter/material.dart" :as m]))]
            (locking *compiler-state
              (let [p (promise)
                    _ (swap! *repl-states assoc-in [repltag :ack!] #(deliver p %))
                    _ (eval-to-repl repltag expr-or-throwable *compiler-state trigger-reload p)
                    str-or-throwable @p]
                (if (instance? Throwable str-or-throwable)
                  (throw str-or-throwable)
                  (set! compiler/*current-ns* (symbol str-or-throwable)))))
            (let [x (try
                      (compiler/read {:eof *in* :read-cond :allow :features #{:cljd}} *in*)
                      (catch Throwable t t))]
              (when-not (identical? *in* x)
                (recur x)))))
        (catch java.io.IOException e (throw e))
        (catch Exception e
          (println "REPL session terminated." (.getMessage e))
          (st/print-stack-trace e))))))

(defmacro ^:private daemon [& forms]
  `(doto (Thread. (fn [] ~@forms))
    (.setDaemon true)
    .start))

(defn restartable-repl [*repl-states options]
  (let [{:keys [cnt]} (swap! *repl-states update :cnt inc)
        repltag (Long/toString cnt 36)
        true-in *in*
        true-out *out*
        reset-repl-state!
        (fn self []
          (let [reader (java.io.PipedReader.)
                writer (java.io.PipedWriter. reader)
                [{prev repltag} {curr repltag}] (swap-vals! *repl-states
                                                  assoc repltag
                                                  {:out true-out
                                                   :writer writer
                                                   :reader reader
                                                   :restart! self})]
            (some-> ^java.io.Writer (:writer prev) .close)
            curr))
        {:keys [reader writer]} (reset-repl-state!)]

    (daemon ; copy true-in to actual in of the repl
      (let [^chars buffer (make-array Character/TYPE 1024)]
        (loop [size 0 ^java.io.Writer writer writer]
          (case size
            -1 nil
            0 (recur (.read true-in buffer) writer)
            (if (try
                  (.write writer buffer 0 size)
                  true
                  (catch java.io.IOException e
                    (when-not (= "Pipe closed" (.getMessage e))
                      (throw e))))
              (recur 0 writer)
              (recur size (:writer (@*repl-states repltag))))))))

    (loop [reader reader]
      (binding [*in* reader]
        (repl *repl-states (assoc options :repltag repltag)))
      (let [reader' (:reader (@*repl-states repltag))]
        (when-not (identical? reader reader')
          ; pipe cycled by a restart
          (recur reader'))))))


(defn bsearch
  "pred is monotonic false -> true through v.
   Returns the last element for which pred is false"
  [v pred]
  (cond
    (empty? v) nil
    (pred (first v)) nil
    (not (pred (peek v))) (peek v)
    :else
    (loop [i 0 j (count v)]
      #_(assert (not (pred (nth v i))))
      (let [m (quot (+ i j) 2)
            x (nth v m)]
        (cond
          (= i m) x
          (pred x) (recur i m)
          :else (recur m j))))))

(defn smap-search [lib-smap line col]
  (when-some [[dart-line dart-col {:keys [slug smap]}]
              (bsearch lib-smap
                (fn [[dart-line dart-col smap]]
                  (or
                    (< line dart-line)
                    (and (= line dart-line)
                      (or (nil? col) (< col dart-col))))))]
    (let [line (- line (dec dart-line)) ; lines are 1-based
          col (if (and col (zero? line)) (- col dart-col) col)]
      (->
        (bsearch smap
          (fn [[dart-line dart-col smap]]
            (or
              (< line dart-line)
              (and (= line dart-line)
                (or (nil? col) (< col dart-col))))))
        peek
        (assoc :slug slug)))))

(defn excerpt-and-highlight [{:keys [line column end-line end-column url]}]
  (when (= line end-line)
    (let [r (io/reader url)
          _ (dotimes [_ (dec line)] (.readLine r))
          src (.readLine r)]
      (str "\n👉" (subs src 0 (dec column))
        (inverted (subs src (dec column) (dec end-column)))
        (subs src (dec end-column))))))

(def smap-line
  (let [mk-smap-line
        (fn mk-smap-line []
          (let [{:keys [libs]} @compiler/nses
                libspat
                (->> libs
                  (keep (fn [[libname {:keys [smap]}]]
                          (when smap
                            (-> libname
                              (str/replace #"^lib/" "/")
                              java.util.regex.Pattern/quote))))
                  (str/join "|"))
                pat (re-pattern (str "\\w+:[^:]+?(" libspat "):(\\d+)(?::(\\d+))"))]
            (fn [line]
              (when (identical? libs (:libs @compiler/nses))
                (let [*source-info (volatile! nil)
                      line
                      (str/replace line pat
                        (fn [[match lib line col]]
                          (let [{:keys [ns smap]} (get libs (str "lib" lib))
                                {:keys [line column slug end-line end-column file url]
                                 :as source-info}
                                (smap-search smap (parse-long line) (some-> col parse-long))]
                            (vreset! *source-info source-info)
                            (str (bright ns) (subs slug 0 1) (bright (subs slug 1)) " " (bright file) ":" (bright line) ":" (bright column) " " (muted match)))))]
                  (cond-> line
                    @*source-info (str (excerpt-and-highlight @*source-info))))))))
        *smap-line (atom (mk-smap-line))]
    (fn [line]
      (if-some [line (@*smap-line line)]
        line
        (do
          (reset! *smap-line (mk-smap-line))
          (recur line))))))

(defn ensure-no-existing!
  "exit if active REPL"
  []
  (let [f (java.io.File. "REPL.lock")]
    (when (.exists f)
      (let [[port pid] (re-seq #"\S+" (slurp f))]
        (when (some-> pid parse-long
                java.lang.ProcessHandle/of
                ^java.lang.ProcessHandle (.orElse nil)
                .isAlive)
          (println (str "Another ClojureDart process is running (PID " pid ")"))
          (System/exit 1))))))

(defn compile-cli
  [& {:keys [watch namespaces flutter] :or {watch false}}]
  (let [*repl-states (atom {:cnt 0})
        user-dir (System/getProperty "user.dir")
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
              *compiler-state (atom {:recompile-count 0
                                     :restart-count 0})
              compile-nses
              (fn [nses]
                (let [nses (into @dirty-nses nses)]
                  (vreset! dirty-nses #{})
                  (when (seq nses)
                    (newline)
                    (println (title "Compiling to Dart...") (timestamp))
                    (run! #(println " " %) (sort nses))
                    (try
                      (compiler/recompile nses (:recompile-count (swap! *compiler-state update :recompile-count inc)))
                      (println (success) (timestamp))
                      true
                      (catch Exception e
                        (vreset! dirty-nses nses)
                        (println e)
                        #_(print-exception e)
                        false)))))
              compilation-success (compile-nses namespaces)]
          (if (or watch flutter)
            (let [compile-files
                  (fn [^java.io.Writer flutter-stdin]
                    (fn [_ files]
                      (locking *compiler-state
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
                              (doto flutter-stdin (.write "r") .flush)))))))]
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
                        ansi *ansi*
                        q (java.util.concurrent.SynchronousQueue.)
                        true-out *out*
                        trigger-reload #(.put q {:kind :reload})
                        *repl-port (atom nil)]
                    ; Unimplemented handling of missing static target
                    (when (and flutter-stdin flutter-stdout)
                      (daemon
                        (while true
                          (let [s (read-line)]
                            (locking flutter-stdin
                              (doto flutter-stdin
                                (.write (case s "" "R" s))
                                .flush)))))

                      (daemon
                        (loop []
                          (when-some [line (some-> (.readLine flutter-stdout) smap-line)]
                            (.put q {:kind :line :line line})
                            (recur)))
                        (.put q {:kind :eof}))

                      (daemon
                        (binding [*ansi* ansi]
                          (loop [state :idle pending-reload false]
                            (cond
                              (and (= :idle state) pending-reload)
                              (do
                                (locking flutter-stdin
                                  (doto flutter-stdin
                                    (.write "r")
                                    .flush))
                                (recur :idle false))

                              (= :restarting state)
                              (do
                                (doseq [{:keys [restart! out]} (vals (dissoc @*repl-states :cnt))]
                                  (binding [*out* out]
                                    (println "\n\n;;;; App restarting. Abandon all state!\n"))
                                  (restart!))
                                (recur :waiting-end-of-restart pending-reload))

                              :else
                              (let [{:keys [kind line]} (.take q)]
                                (when (and line (not= :reload-failed state))
                                  (let [line (smap-line line)
                                        [_ repltag mode cont line']
                                        ; lines coming from flutter have this shape:
                                        ; flutter: [id mode)>actual content_
                                        ; where > is the continuation flag
                                        ; and _ is a sentinel to prevent line trimming
                                        ;
                                        ; the mismatched brackets [id) are there on purpose so that's it's unlikely
                                        ; to match some spurious output.
                                        ;
                                        ; The existing modes are: !, =, o, and e
                                        ; resp. acknowledge, evaluation result, stdout, stderr
                                        ;
                                        ; The continuation flag can be either: space (or newline), /, or >
                                        ; resp.:
                                        ; - end of line (the newline is part of the output and the output must be flushed),
                                        ; - flush (end of line is not part of the ouput and the output must be flushed)
                                        ; - multiline (end of line is not part of the ouput and the output should not
                                        ;   be flushed)
                                        (re-matches #"flutter: \[([^ )]+) ([^)]*)\)(?:([>/ ])(.*))?_" line)
                                        cont (or cont " ")]
                                    (when-not (and (= repltag "*") (= mode "RDY"))
                                      (if-some [{:keys [^java.io.Writer out ack!]}
                                                (@*repl-states repltag)]
                                        (case mode
                                          "!" (ack! line')
                                          ("=" "o" "e")
                                          (doto out
                                            (.write (or line' ""))
                                            (cond->
                                                (= cont " ") (doto (.write "\n"))
                                                (not= cont ">") (doto .flush))))
                                        (println line)))))

                                (case kind
                                  :reload (recur state true)
                                  :eof nil
                                  :line
                                  (let [line (.trim line)
                                        state'
                                        (case state
                                          :idle
                                          (condp = line
                                            "Performing hot reload..." :reloading
                                            "Performing hot restart..." (do
                                                                          (swap! *compiler-state
                                                                            update :restart-count inc)
                                                                          :restarting)
                                            state)
                                          :reloading
                                          (cond
                                            (re-matches #"Reloaded .+ of .+ libraries in .+." line) :idle
                                            (= "Unimplemented handling of missing static target" line) :reload-failed)
                                          :reload-failed
                                          (when (re-matches #"Reloaded .+ of .+ libraries in .+." line)
                                            (newline)
                                            (println (bright "Hot reload failed, attempting hot restart!"))
                                            (locking flutter-stdin
                                              (doto flutter-stdin
                                                (.write "R")
                                                .flush))
                                            :restarting)
                                          :waiting-end-of-restart
                                          (when (= "flutter: [* RDY)_" line) :idle))]
                                    (when (= "flutter: [* RDY)_" line)
                                      (when-some [port @*repl-port]
                                        (newline)
                                        (println (title "🤫 ClojureDart REPL")
                                          (cond->> "listening on port"
                                            (pos? (:restart-count @*compiler-state)) (str "still "))
                                          (title port))
                                        (newline)))
                                    (recur (or state' state) pending-reload)))))))))

                    (let [^java.net.ServerSocket socket
                          (server/start-server {:port 0 :name "CLJD repl" :accept 'cljd.build/restartable-repl
                                                :args [*repl-states
                                                       {:dart-version compiler/*dart-version*
                                                        :analyzer-info compiler/analyzer-info
                                                        :*compiler-state *compiler-state
                                                        :trigger-reload trigger-reload}]})
                          port (.getLocalPort socket)]
                      (reset! *repl-port port)
                      (doto (java.io.File. "REPL.lock")
                        .deleteOnExit
                        (spit (str port " " (-> (java.lang.ProcessHandle/current) .pid) "\n")))
                      #_#_(println (title "ClojureDart REPL (experimental 💥)") "listening on port" (title port))
                      (newline))
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
        lib (compiler/relativize-lib (str/join "/" (map #(.toString %) (.toPath entry-point))) (compiler/ns-to-lib main-ns))]
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
          (do
            (ensure-no-existing!)
            (compile-cli
             :namespaces (or (seq (map symbol args))
                           (some-> *deps* :cljd/opts :main list))
             :watch (= cmd "watch")))
          "test"
          (let [[nses [delim & dart-test-args]] (split-with (complement #{"--" "++"}) args)
                nses (map symbol nses)
                opts-dart-test-args (-> *deps* :cljd/opts :dart-test-args)]
            (ensure-no-existing!)
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
            (ensure-no-existing!)
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
