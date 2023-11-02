;   Copyright (c) Baptiste Dupuch & Christophe Grand. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns cljd.compiler
  (:refer-clojure :exclude [macroexpand macroexpand-1 munge compile])
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [clojure.edn :as edn]))

(def dc-void {:kind :class
              :element-name        "void"
              :canon-qname 'void
              :qname       'void})
(def dc-dynamic '{:kind :class
                  :qname           dc.dynamic,
                  :canon-qname     dc.dynamic
                  :canon-lib       "dart:core"
                  :lib             "dart:core",
                  :element-name            "dynamic",
                  :type-parameters []})
(def dc-Never '{:kind :class
                :qname       dc.Never,
                :canon-qname dc.Never
                :canon-lib   "dart:core"
                :lib             "dart:core",
                :element-name        "Never"})
(def dc-Object '{:kind :class
                 :qname           dc.Object,
                 :canon-qname     dc.Object
                 :canon-lib       "dart:core"
                 :lib             "dart:core",
                 :element-name            "Object",
                 :type-parameters []})
(def dc-Future '{:kind :class
                 :element-name            "Future",
                 :qname           dc.Future
                 :canon-qname     da.Future
                 :canon-lib       "dart:async"
                 :lib             "dart:core",
                 :type-parameters [{:element-name "T", :is-param true, :qname T :canon-qname T}],})
(def da-FutureOr '{:kind :class
                   :element-name            "FutureOr",
                   :canon-lib       "dart:async"
                   :lib             "dart:async",
                   :type-parameters [{:element-name "T", :is-param true, :qname T :canon-qname T}],
                   :qname           da.FutureOr
                   :canon-qname     da.FutureOr})
(def dc-Null '{:kind :class
               :qname           dc.Null,
               :canon-qname     dc.Null
               :canon-lib       "dart:core"
               :lib             "dart:core",
               :element-name            "Null",
               :type-parameters []})
(def dc-String '{:kind :class
                 :qname           dc.String,
                 :canon-qname     dc.String
                 :canon-lib       "dart:core"
                 :lib             "dart:core",
                 :element-name            "String",
                 :type-parameters []})
(def dc-Function '{:kind :class
                   :qname           dc.Function,
                   :canon-qname     dc.Function
                   :canon-lib       "dart:core"
                   :lib             "dart:core",
                   :element-name            "Function",
                   :type-parameters []})
(def dc-Record '{:kind            :class
                 :qname           dc.Record,
                 :canon-qname     dc.Record
                 :canon-lib       "dart:core"
                 :lib             "dart:core",
                 :element-name    "Record",
                 :type-parameters []})
(def dc-bool '{:kind :class
               :qname           dc.bool,
               :canon-qname     dc.bool
               :canon-lib       "dart:core"
               :lib             "dart:core",
               :element-name            "bool",
               :type-parameters []})
(def dc-int '{:kind :class
              :qname           dc.int,
              :canon-qname     dc.int
              :canon-lib       "dart:core"
              :lib             "dart:core",
              :element-name            "int",
              :type-parameters []})
(def dc-double '{:kind :class
                 :qname           dc.double,
                 :canon-qname     dc.double
                 :canon-lib       "dart:core"
                 :lib             "dart:core",
                 :element-name            "double",
                 :type-parameters []})
(def dc-num '{:kind :class
              :qname           dc.num,
              :canon-qname     dc.num
              :canon-lib       "dart:core"
              :lib             "dart:core",
              :element-name            "num",
              :type-parameters []})

(def pseudo-num-tower '{:kind :class
                        :qname       pseudo.num-tower
                        :canon-qname pseudo.num-tower})

(def pseudo-some '{:kind :class
                   :qname       dc.dynamic
                   :lib "dart:core"
                   :canon-qname pseudo.not-bool
                   :nullable true})

(def pseudo-super '{:kind :class
                    :qname       dc.dynamic
                    :lib "dart:core"
                    :canon-qname pseudo.super})

(def cljd-ifn '{:kind :class
                :qname lcoc_core.IFn$iface
                :canon-qname lcoc_core.IFn$iface
                :element-name "IFn$iface"})

(declare global-lib-alias)

(defn update-if
  [m k f]
  (if-some [v (get m k)]
    (assoc m k (f v))
    m))

(def ^:dynamic ^java.io.Writer *dart-out*)

(defmacro with-dart-str [& body]
  `(let [w# (java.io.StringWriter.)]
     (binding [*dart-out* w#]
       ~@body)
     (.toString w#)))

;; analyzer-info is a fn to query info about types and libs
;;
;; [lib element] returns a full map

(declare
  ^{:dynamic true
    :doc "dynamically bound fn to retrieve types and libs information
  [lib] returns nil if no such lib or a map with an optional
  :private key mapped to true when the lib is private.
  [lib element] returns a full map TBD."}
  analyzer-info)

(def ^:dynamic *dart-version* :dart3)

(defn mk-dead-analyzer-info [dart-libs-info]
  (fn
    ([lib] (some-> (dart-libs-info lib) (select-keys [:private])))
    ([lib element]
     (-> lib dart-libs-info (get element)))))

(defn mk-live-analyzer-info
  [^Process p]
  (let [stdin (java.io.OutputStreamWriter. (.getOutputStream p) "UTF-8")
        stdout (clojure.lang.LineNumberingPushbackReader.
                 (java.io.InputStreamReader. (.getInputStream p) "UTF-8"))
        {dart-version :dart} (edn/read stdout)
        cache (atom (-> {}
                      (assoc-in ["dart:core" "Never"] dc-Never)
                      (assoc-in ["dart:core" "dynamic"] dc-dynamic)
                      (assoc-in ["dart:_internal" :private] true)))
        qname (fn qname [m]
                (->> (cond-> m
                       (:canon-qname-placeholder m)
                       (-> (assoc
                             :canon-qname (-> (global-lib-alias (:canon-lib m) nil) (str "." (:element-name m)) symbol)
                             :qname (-> (global-lib-alias (:lib m) nil) (str "." (:element-name m)) symbol))
                         (dissoc :canon-qname-placeholder))
                       (= :record (:kind m)) (->> (into {:positional-fields [] :named-fields []})))
                  (into {} (map (fn [[k v]]
                                  (cond
                                    (map? v) [k (qname v)]
                                    (vector? v) [k (into [] (map qname) v)]
                                    :else [k v]))))))]
    (set! *dart-version*
      (cond
        (.startsWith dart-version "3.") :dart3
        (.startsWith dart-version "2.") :dart2
        :else (throw (Exception. (str "Unsupported Dart version: " dart-version)))))
    #_(fn [& args]
        (let [c @cache
              r (c args c)]
          (if-not (identical? r c)
            r
            (do
              (.write stdin (str (count args)))
              (doseq [arg args]
                (doto stdin (.write " ") (.write ^String arg)))
              (doto stdin (.write "\n") .flush)
              (let [r (edn/read stdout)]
                (swap! cache assoc args r)
                r)))))
    (fn
      ([lib]
       (when (some-> lib (str/starts-with? "lib/cljd-out") not)
         (let [c @cache
               ;; c as sentinel
               r (get c lib c)]
           (if-not (identical? r c)
             r
             (do (doto stdin (.write "lib ") (.write lib) (.write "\n") .flush)
                 ;; nil means lib does not exist
                 (when-some [r (edn/read stdout)]
                   (swap! cache assoc lib {})
                   true))))))
      ([lib element]
       (when (some-> lib (str/starts-with? "lib/cljd-out") not)
         (let [c @cache
               ;; c as sentinel
               r (get-in c [lib element] c)]
           (if-not (identical? r c)
             r
             (when (and lib element ; how does nil happen?
                     (not
                       (or (and (= (name lib) "dart:core")
                             (case (name element)
                               ("assert" "await" "is") true
                               false))
                         (<= 0 (.indexOf (name element) ".")))))
               (doto stdin (.write "elt ") (.write lib) (.write " ") (.write element) (.write "\n") .flush)
               ;; nil means elt does not exist
               (when-some [r (some-> (edn/read stdout) qname)]
                 (if (:local-lib r)
                   ;; We *don't* cache local-lib
                   (dissoc r :local-lib)
                   (do (swap! cache assoc-in [lib element] r)
                         r)))))))))))


(def ^:dynamic *hosted* false)
(def ^:dynamic *host-eval* false)

(def ^:dynamic ^String *lib-path* "lib/")
(def ^:dynamic ^String *test-path* "test/")

(def ^:dynamic *target-subdir*
  "Relative path to the lib directory (*lib-dir*) where compiled dart file will be put.
   Defaults to \"cljd-out/\"."
  "cljd-out/")

(def ^:dynamic *source-info* {})
(defn source-info []
  (let [{:keys [line column]} *source-info*]
    (if line
      (str " at line: " line ", column: " column ", file: " *file*)
      " (no source location)")))

(defmacro ^:private else->> [& forms]
  `(->> ~@(reverse forms)))

(defmacro ^:private expect-some
  "Like when-some but asserts on nil"
  [[binding expr] & body]
  `(let [v# ~expr]
     (assert (some? v#) ~(str (pr-str expr) "is not expexcted to be nil"))
     (let [~binding v#]
       ~@body)))

(defn- replace-all [^String s regexp f]
  #?(:cljd
     (.replaceAllMapped s regexp f)
     :clj
     (str/replace s regexp f)))

(def ns-prototype
  {:imports {"dart:core" {}}
   ; map from aliases found in clj code to dart libs
   :clj-aliases {"dart:core" "dart:core"}
   :mappings
   '{Record dart:core/Record
     Type dart:core/Type,
     BidirectionalIterator dart:core/BidirectionalIterator,
     bool dart:core/bool,
     UnimplementedError dart:core/UnimplementedError,
     Match dart:core/Match,
     Error dart:core/Error,
     Uri dart:core/Uri,
     Object dart:core/Object,
     IndexError dart:core/IndexError,
     MapEntry dart:core/MapEntry,
     DateTime dart:core/DateTime,
     StackTrace dart:core/StackTrace,
     Symbol dart:core/Symbol,
     String dart:core/String,
     Future dart:core/Future,
     StringSink dart:core/StringSink,
     Expando dart:core/Expando,
     BigInt dart:core/BigInt,
     num dart:core/num,
     Function dart:core/Function,
     TypeError dart:core/TypeError,
     StackOverflowError dart:core/StackOverflowError,
     Comparator dart:core/Comparator,
     double dart:core/double,
     Iterable dart:core/Iterable,
     UnsupportedError dart:core/UnsupportedError,
     Iterator dart:core/Iterator,
     Stopwatch dart:core/Stopwatch,
     int dart:core/int,
     dynamic dart:core/dynamic
     Invocation dart:core/Invocation,
     RuneIterator dart:core/RuneIterator,
     RegExpMatch dart:core/RegExpMatch,
     Deprecated dart:core/Deprecated,
     StateError dart:core/StateError,
     Map dart:core/Map,
     pragma dart:core/pragma,
     Sink dart:core/Sink,
     NoSuchMethodError dart:core/NoSuchMethodError,
     Set dart:core/Set,
     FallThroughError dart:core/FallThroughError,
     StringBuffer dart:core/StringBuffer,
     RangeError dart:core/RangeError,
     Comparable dart:core/Comparable,
     CyclicInitializationError dart:core/CyclicInitializationError,
     LateInitializationError dart:core/LateInitializationError,
     FormatException dart:core/FormatException,
     Null dart:core/Null,
     NullThrownError dart:core/NullThrownError,
     Exception dart:core/Exception,
     RegExp dart:core/RegExp,
     Stream dart:core/Stream,
     Pattern dart:core/Pattern,
     AbstractClassInstantiationError
     dart:core/AbstractClassInstantiationError,
     OutOfMemoryError dart:core/OutOfMemoryError,
     UriData dart:core/UriData,
     Runes dart:core/Runes,
     IntegerDivisionByZeroException
     dart:core/IntegerDivisionByZeroException,
     ConcurrentModificationError dart:core/ConcurrentModificationError,
     AssertionError dart:core/AssertionError,
     Duration dart:core/Duration,
     ArgumentError dart:core/ArgumentError,
     List dart:core/List}})

(declare ^:dynamic *current-ns*)

(def nses (atom {:libs {"dart:core" {:dart-alias "dc" :ns nil}
                        "dart:async" {:dart-alias "da" :ns nil}} ; dc can't clash with user aliases because they go through dart-global
                 ; map from dart aliases to libs
                 :dart-aliases {"dc" "dart:core"
                                "da" "dart:async"}
                 :ifn-mixins {}
                 #_#_'user ns-prototype}))

(defn global-lib-alias
  "ns may be nil, returns the alias for this lib"
  [lib ns]
  (or (-> @nses :libs (get lib) :dart-alias)
    (let [[_ trimmed-lib] (re-matches #"(?:package:)?(.+?)(?:\.dart)?" lib)
          segments (re-seq #"[a-zA-Z][a-zA-Z_0-9]*" trimmed-lib)
          prefix (apply str (map first (butlast segments)))
          base (cond->> (last segments) (not= prefix "") (str prefix "_"))
          nses (swap! nses
                 (fn [nses]
                   (if (-> nses :libs (get lib) :dart-alias)
                     nses
                     (let [alias (some #(when-not (get (:dart-aliases nses) %) %)
                                   (cons base (map #(str base "_" (inc %)) (range))))]
                       (-> nses
                         (assoc-in [:libs lib] {:dart-alias alias :ns ns})
                         (assoc-in [:dart-aliases alias] lib))))))]
      (-> nses :libs (get lib) :dart-alias))))

(declare resolve-type type-str actual-member)

(defn- resolve-clj-alias
  "Resolves the namespace part of a clojure symbol referencing a dart element to the dart lib
   of this element."
  [clj-alias]
  (when clj-alias
    (let [{:keys [libs dart-aliases] :as all-nses} @nses
          {:keys [clj-aliases]} (all-nses *current-ns*)]
      (or (get clj-aliases clj-alias)
        (some-> (re-matches #"\$lib:(.*)" clj-alias) second dart-aliases)))))

(defn- resolve-dart-type
  [clj-sym type-vars]
  (let [{:keys [libs] :as nses} @nses
        typename (name clj-sym)
        typens (namespace clj-sym)]
    (else->>
      (when-not (.endsWith (name clj-sym) "."))
      (if ('#{void dart:core/void} clj-sym)
        dc-void)
      (if (and (nil? typens) (contains? type-vars (symbol typename)))
        {:kind :class :canon-qname clj-sym :qname clj-sym :element-name typename :is-param true})
      (when-some [lib (resolve-clj-alias typens)]
        (or (-> (analyzer-info lib typename) (dissoc :members))
          (let [dart-alias (:dart-alias (libs lib))
                qname (symbol (str dart-alias "." typename))]
            (case clj-sym
              cljd.core/IFn$iface cljd-ifn
              nil)))))))

(defn non-nullable [tag]
  (when-some [[_ base] (re-matches #"(.+)[?]" (name tag))]
    (if (symbol? tag) ; is string support still desirable?
      (with-meta (symbol (namespace tag) base) (meta tag))
      base)))

(defn- type-env-from-map
  "Takes a map from type params names (as strings) to types (as maps).
   Returns a function from types (as maps) to types (as maps)."
  [type-map]
  #(if-some [t (when (:is-param %) (type-map (:element-name %)))]
     (cond-> t
       (when-not (:nullable t) (:nullable %)) (assoc :nullable true))
     %))

(defn- type-map-for
  [class-or-member type-args type-params]
  (let [nargs (count type-args)
        nparams (count type-params)]
    (cond
      (= nargs nparams)
      (zipmap (map :element-name type-params) type-args)
      (zero? nargs)
      (into {} (map (fn [p] [(:element-name p) (:bound p dc-dynamic)])) type-params)
      :else
      (throw (Exception. (str "Expecting " nparams " type arguments to " class-or-member ", got " nargs "."))))))

(def type-env-for (comp type-env-from-map type-map-for))

(defn specialize-type [dart-type nullable clj-sym type-vars]
  ; there's some serious duplication going on between here and actual-*
  (when dart-type
    (->
      (case (:canon-qname dart-type)
        dc.Function
        (if-some [type-parameters (seq (:type-parameters dart-type))]
          (let [type-env (type-env-for
                           (:canon-qname dart-type)
                           (map #(resolve-type % type-vars) (:type-params (meta clj-sym)))
                           type-parameters)
                type-vars (into type-vars (map :element-name) type-parameters)]
            (assoc dart-type
              :type-parameters
              (into []
                (keep (fn [p] (let [t (type-env (:element-name p))] (when (:is-param t) t))))
                type-parameters)
              :parameters (into [] (map #(update-in % :type type-env))
                            (:parameters dart-type))))
          dart-type)
        (assoc dart-type
          :type-parameters (into [] (map #(resolve-type % type-vars)) (:type-params (meta clj-sym)))))
      (assoc :nullable nullable))))

(defn specialize-function [function-info clj-sym type-vars]
  (actual-member
    [(type-env-for clj-sym
       (map #(resolve-type % type-vars) (:type-params (meta clj-sym)))
       (:type-parameters function-info))
     function-info]))

(defn- cljdize [ns]
  (if (symbol? ns)
    (some-> (cljdize (name ns)) symbol (with-meta (meta ns)))
    (when-some [[_ sub-ns] (some->> ns (re-matches #"clojure\.(.+)"))]
      (str "cljd." sub-ns))))

(defn- resolve-non-local-symbol [sym type-vars]
  (let [{:keys [libs dart-aliases] :as nses} @nses
        {:keys [mappings clj-aliases] :as current-ns} (nses *current-ns*)
        ensure-class (fn [v] (when (= :class (:type v)) v))
        resolve (fn [sym ensure-right-type]
                  (else->>
                    (if-some [v (ensure-right-type (get current-ns sym))] [:def v])
                    (if-some [v (get mappings sym)]
                      (recur (with-meta v (meta sym)) ensure-right-type))
                    (let [sym-ns (namespace sym)
                          lib-ns (when sym-ns
                                   (or (cljdize sym-ns)
                                     (some-> (get clj-aliases sym-ns) libs :ns name)
                                     (some-> (re-matches #"\$lib:(.*)" sym-ns)
                                       second dart-aliases libs :ns name)))])
                    (if (some-> lib-ns (not= sym-ns))
                      (recur (with-meta (symbol lib-ns (name sym)) (meta sym))
                        ensure-right-type))
                    (if-some [info (some-> sym-ns symbol nses (get (symbol (name sym))) ensure-right-type)]
                      [:def info])
                    (when-not (non-nullable sym))
                    (if-some [atype (resolve-dart-type sym type-vars)]
                      [:dart atype])))
        specialize (fn [[tag info] nullable]
                     [tag (case tag
                            :def (update info :dart/type specialize-type nullable sym type-vars)
                            :dart (case (:kind info)
                                    :function (specialize-function info  sym type-vars)
                                    :class (specialize-type info nullable sym type-vars)
                                    :field info))])]
    (or (some-> (resolve sym identity) (specialize false))
      (some-> (non-nullable sym) (resolve ensure-class) (specialize true)))))

(defn resolve-symbol
  "Returns either a pair [tag value] or nil when the symbol can't be resolved.
   tag can be :local, :def or :dart respectively indicating the symbol refers
   to a local, a global def (var-like) or a Dart global.
   The value depends on the tag:
   - for :local it's whatever is in the env,
   - for :def it's what's is in nses,
   - for :dart it's the aliased dart symbol."
  [sym env]
  (let [v (env sym env)] ; because v can be nil for consts
    (if-not (identical? v env)
      [:local v]
      (resolve-non-local-symbol sym (:type-vars env)))))

(defn dart-alias-for-ns [ns]
  (let [nses @nses
        lib (get-in nses [ns :lib])]
    (-> nses :libs (get lib) :dart-alias)))

(defn resolve-type
  "Resolves a type to map with keys :qname :lib :element-name and :type-parameters."
  ([sym type-vars]
   (or (resolve-type sym type-vars nil) (throw (ex-info (str "Can't resolve type: " sym) {:sym sym}))))
  ([sym type-vars not-found]
   (when-some [[tag info] (resolve-non-local-symbol sym type-vars)]
     (case tag
       :dart (do
               (some-> info :lib (global-lib-alias nil))
               (case (:canon-qname info)
                 dc.Record
                 (if-some [{:keys [fixed-fields-types named-fields-types]} (:fields-types (meta sym))]
                   (assoc info
                     :positional-fields (map (fn [t] {:type (resolve-type t type-vars)}) fixed-fields-types)
                     :named-fields (map (fn [[n t]]
                                          {:type (resolve-type t type-vars)
                                           :name n}) named-fields-types))
                   info)
                 dc.Function
                 (let [type-vars' (into (set type-vars) (:type-params (meta sym)))]
                   (if-some [{:keys [fixed-params-types opt-kind opt-params-types ret-type]} (:fn-type (meta sym))]
                     (assoc info
                       :parameters
                       (concat
                         (map (fn [t] {:kind :positional :type (resolve-type t type-vars')}) fixed-params-types)
                         (case opt-kind
                           :named
                           (map (fn [[n t]]
                                  {:name n
                                   :kind :named
                                   :optional true ; TODO handle required
                                   :type (resolve-type t type-vars')}) opt-params-types)
                           (map (fn [t]
                                  {:kind :positional
                                   :optional true
                                   :type (resolve-type t type-vars')}) opt-params-types)))
                       :return-type (resolve-type ret-type type-vars')
                       :type-parameters (map #(resolve-dart-type % type-vars') (:type-params (meta sym))))
                     info))
                 info))
       :def (case (:type info)
              :class (:dart/type info)
              :field (case (when (= 'cljd.core (:ns info)) (:name info))
                       int dc-int
                       int? (assoc dc-int :nullable true)
                       double dc-double
                       double? (assoc dc-double :nullable true)
                       not-found)
              not-found)
       not-found))))

;; #/[bool bool .foo String]
;; ^{:record-type [bool bool .foo String]} dc.Record

(defn unresolve-type
  "Takes a type map and returns a Clojure symbol (e.g. usable as a typehint or a class/iface name)
   that the compiler could resolve to this type map."
  [{:keys [is-param lib qname canon-qname element-name type-parameters nullable] :as x}]
  (if is-param
    (symbol (cond-> qname nullable (str "?")))
    (let [nses @nses]
      (when (nil? element-name)
        (throw (ex-info (pr-str x) {:x x})))
      (with-meta
        (symbol
          (or (get-in nses [*current-ns* :imports lib :clj-alias])
            (some-> lib (global-lib-alias nil) (->> (str "$lib:"))))
          (cond-> element-name nullable (str "?")))
        (let [m {:type-params (mapv unresolve-type type-parameters)}]
          (cond
            (and (= (:canon-qname dc-Function) canon-qname) (:parameters x))
            (let [opt-kind (if (some #(= :named (:kind %)) (:parameters x)) :named :positional)]
              ; the line above (determining opt-kind) and the two :when below (<1> and <2>)
              ; assumes that named implies optional and this isn't true anymore since Dart 2.12
              (assoc m :fn-type {:opt-kind opt-kind
                                 :fixed-params-types
                                 (for [{:keys [kind optional type]} (:parameters x)
                                       :when (and (not optional) (= :positional kind))] ; <1> see note above
                                   (unresolve-type type))
                                 :opt-params-types
                                 (case opt-kind
                                   :named
                                   (into {}
                                     (for [{:keys [name kind optional type]} (:parameters x)
                                           :when (or optional (= :named kind))] ; <2> see note above
                                       [name (unresolve-type type)]))
                                   (for [{:keys [optional type]} (:parameters x)
                                         :when optional]
                                     (unresolve-type type)))
                                 :ret-type (unresolve-type (or (:return-type x) dc-dynamic))}))

            (and (= (:canon-qname dc-Record) canon-qname) (:positional-fields x))
            (assoc m :fields-types {:fixed-fields-types (map #(unresolve-type (:type %)) (:positional-fields x))
                                    :named-fields-types
                                    (into {} (map (fn [{:keys [type name]}] [name (unresolve-type type)])
                                               (:named-fields x)))})
            :else m))))))

(defn emit-type
  [tag {:keys [type-vars] :as env}]
  (cond
    (= 'some tag) pseudo-some
    (= 'super tag) pseudo-super
    ('#{void dart:core/void} tag) dc-void
    :else
    (or (resolve-type tag type-vars nil) (when *hosted* (resolve-type (symbol (name tag)) type-vars nil))
      (throw (Exception. (str "Can't resolve type " tag "."))))))

(defn dart-type-truthiness [type]
  (case (:canon-qname type)
    (nil dc.Object dc.dynamic dc.Never) nil
    (dc.Null void) :falsy
    pseudo.not-bool :some
    dc.bool (when-not (:nullable type) :boolean)
    (if (:nullable type) :some :truthy)))

(defn dart-meta
  "Takes a clojure symbol and returns its dart metadata."
  [sym env]
  (let [{:keys [tag] :as m} (meta sym)
        type (some-> tag (emit-type env))]
    (cond-> {}
      (:async m) (assoc :dart/async true)
      (:getter m) (assoc :dart/getter true)
      (:setter m) (assoc :dart/setter true)
      (:const m) (assoc :dart/const true)
      (:unique m) (assoc :dart/const false)
      (:dart m) (assoc :dart/fn-type :native)
      (:clj m) (assoc :dart/fn-type :ifn)
      type (assoc :dart/type type)
      (= (:canon-qname dc-Function) (:canon-qname type)) (assoc :dart/fn-type :native))))

(def reserved-words ; and built-in identifiers for good measure
  #{"Function" "abstract" "as" "assert" "async" "await" "base" "break" "case" "catch"
    "class" "const" "continue" "covariant" "default" "deferred" "do" "dynamic"
    "else" "enum" "export" "extends" "extension" "external" "factory" "false"
    "final" "finally" "for" "get" "hide" "if" "implements" "import" "in"
    "interface" "is" "late" "library" "mixin" "new" "null" "on" "operator" "part"
    "rethrow" "return" "sealed" "set" "show" "static" "super" "switch" "sync" "this"
    "throw" "true" "try" "typedef" "var" "void" "while" "with" "yield"})

(def char-map
  {"-"    "_"
   "."    "$DOT_"
   "_"    "$UNDERSCORE_"
   "$"    "$DOLLAR_"
   ":"    "$COLON_"
   "+"    "$PLUS_"
   ">"    "$GT_"
   "<"    "$LT_"
   "="    "$EQ_"
   "~"    "$TILDE_"
   "!"    "$BANG_"
   "@"    "$CIRCA_"
   "#"    "$SHARP_"
   "'"    "$PRIME_"
   "\""   "$QUOTE_"
   "%"    "$PERCENT_"
   "^"    "$CARET_"
   "&"    "$AMPERSAND_"
   "*"    "$STAR_"
   "|"    "$BAR_"
   "{"    "$LBRACE_"
   "}"    "$RBRACE_"
   "["    "$LBRACK_"
   "]"    "$RBRACK_"
   "/"    "$SLASH_"
   "\\"   "$BSLASH_"
   "?"    "$QMARK_"
   " "    "$SPACE_"
   ","    "$COMMA_"})

(defn munge-str
  "Returns a dart-safe id as string."
  [x]
  (let [s (name x)]
    (or (when (reserved-words s) (str "$" s "_"))
      (replace-all s #"__(\d+)|__auto__|(^-)|[^a-zA-Z0-9]"
        (fn [[x n leading-dash]]
          (else->>
            (if leading-dash "$_")
            (if n (str "$" n "_"))
            (if (= "__auto__" x) "$AUTO_")
            (or (char-map x))
            (str "$u"
              ;; TODO SELFHOST :cljd version
              (str/join "_$u" (map #(-> % int Long/toHexString .toUpperCase) x))
              "_")))))))

(defn quote-dart-type
  [x]
  (let [s (name x)]
    (replace-all s #"__(\d+)|__auto__|(^-)|[^a-zA-Z0-9]"
      (fn [[x n leading-dash]]
        (else->>
          (if leading-dash "$_")
          (if n (str "$" n "_"))
          (if (= "__auto__" x) "$AUTO_")
          (or (case x
                "_" x
                (char-map x)))
          (str "$u"
            ;; TODO SELFHOST :cljd version
            (str/join "_$u" (map #(-> % int Long/toHexString .toUpperCase) x))
            "_"))))))

(defn munge
  ([sym env] (munge sym nil env))
  ([sym suffix env]
   (with-meta
     (or
       (-> sym meta :dart/name)
       (symbol
         (cond-> (munge-str sym)
           suffix (str "$" suffix))))
     (dart-meta sym env))))

(defn munge* [dart-names]
  (let [sb (StringBuilder. "$C$")]
    (reduce
      (fn [need-sep ^String dart-name]
        (when (and need-sep (not (.startsWith dart-name "$C$")))
          (.append sb "$$"))
        (.append sb dart-name)
        (not (.endsWith dart-name "$D$")))
      false (map name dart-names))
    (.append sb "$D$")
    (symbol (.toString sb))))

(defn- dont-munge [sym suffix]
  (let [m (meta sym)
        sym (cond-> sym suffix (-> name (str "$" suffix) symbol))]
    (with-meta sym (assoc m :dart/name sym))))

(defonce ^:private gens (atom 1))
(defn dart-global
  ([] (dart-global ""))
  ([prefix] (munge prefix (swap! gens inc) {})))

(def ^:dynamic *locals-gen*)
(def ^:dynamic *class-prefix* "")

(defn dart-local
  "Generates a unique (relative to the top-level being compiled) dart symbol.
   Hint is a string/symbol/keyword which gives a hint (duh) on how to name the
   dart symbol. Type tags when present are translated."
  ([env] (dart-local "" env))
  ([hint env]
   (let [dart-hint (munge hint env)
         {n dart-hint} (set! *locals-gen* (assoc *locals-gen* dart-hint (inc (*locals-gen* dart-hint 0))))
         {:dart/keys [type] :as dart-meta} (dart-meta hint env)]
     (with-meta (symbol (str dart-hint "$" n)) dart-meta))))

(defn anonymous-global
  [suffix]
  (let [basis (str *class-prefix* suffix)
        {n basis} (set! *locals-gen* (assoc *locals-gen* basis (inc (*locals-gen* basis 0))))]
    (symbol (str basis "$" n))))

(defn- dot-symbol? [x]
  (and (symbol? x)
    (.startsWith (name x) ".")))

(defn- parse-dart-params [params]
  (let [[fixed-params opt-params] (split-with (complement dot-symbol?) params)
        [delim & opt-params]
        (case (first opt-params)
          (... nil) opt-params
          .& (do
               (when-not (:cljd/compiler (meta (first opt-params))) ; don't warn users on internal sauce 🤫
                 (println "DEPRECATION WARNING: named parameters should now be declared like this: .name1 .name2 and no more like that .& name1 .name2 -- old syntax will not be actively maintained anymore. "))
               opt-params)
          ; new syntax for named params
          (cons '.&
            (for [p opt-params]
              (cond-> p
                (dot-symbol? p)
                (-> name (subs 1) symbol (with-meta (meta p)))))))]
    {:fixed-params fixed-params
     :opt-kind (case delim .& :named :positional)
     :opt-params
     (for [[p d] (partition-all 2 1 opt-params)
           :when (symbol? p)]
       [p (when-not (symbol? d) d)])}))

(defn expand-protocol-impl [{:keys [name impl iface iext extensions]}]
  (list `deftype impl []
    :type-only true
    'cljd.core/IProtocol
    (list 'satisfies '[_ x]
      (list* 'or (list 'dart/is? 'x iface)
        (concat (for [t (keys (dissoc extensions 'fallback))] (list 'dart/is? 'x t)) [false])))
    (list 'extensions '[_ x]
      ;; TODO SELFHOST sort types
      (cons 'cond
        (concat
          (mapcat (fn [[t ext]]
                    (assert (qualified-symbol? t) (str "Types in the :extensions map must be fully qualified, " t " is not."))
                    [(list 'dart/is? 'x t) ext]) (sort-by (fn [[x]] (case x Object 1 0)) (dissoc extensions 'fallback)))
          [:else (or ('fallback extensions) `(throw (dart:core/Exception. (.+ (.+ ~(str "No extension of protocol " name " found for type ") (.toString (.-runtimeType ~'x))) "."))))])))))

(defn- roll-leading-opts [body]
  (loop [[k v & more :as body] (seq body) opts {}]
    (if (and body (keyword? k))
      (recur more (assoc opts k v))
      [opts body])))

(defn resolve-protocol-mname-to-dart-mname*
  "Takes a protocol map and a method (as symbol) and the number of arguments passed
  to this method.
  Returns the name (as symbol) of the dart method backing this clojure method."
  [protocol mname args-count type-env]
  (or
    (let [mname' (get-in protocol [:sigs mname args-count :dart/name] mname)]
      (with-meta mname' (meta mname)))
    (throw (Exception. (str "No method " mname " with " args-count " arg(s) for protocol " (:name protocol) ".")))))

(defn resolve-protocol-mname-to-dart-mname
  "Takes two symbols (a protocol and one of its method) and the number
  of arguments passed to this method.
  Returns the name (as symbol) of the dart method backing this clojure method."
  [pname mname args-count type-env]
  (let [[tag protocol] (resolve-symbol pname {:type-vars type-env})]
    (when (and (= :def tag) (= :protocol (:type protocol)))
      (resolve-protocol-mname-to-dart-mname* protocol mname args-count type-env))))

(defn resolve-protocol-method [protocol mname args type-env]
  (some-> (resolve-protocol-mname-to-dart-mname* protocol mname (count args) type-env)
    (vector (into []
              (map-indexed #(if (symbol? %2)
                              (vary-meta %2 dissoc :tag)
                              (gensym (str "arg" %1))))
              args))))

(defn dart-method-sig
  "Returns either nil or [[fixed params types] opts type-parameters]
   where opts is either [opt-param1-type ... opt-paramN-type] or {opt-param-name type ...}."
  [member-info]
  (when-some [params (:parameters member-info)]
    (let [[fixed opts] (split-with (fn [p] (and (= :positional (:kind p)) (not (:optional p)))) params)
          opts (case (:kind (first opts))
                 :named (into {} (map (juxt (comp keyword :name) :type)) opts)
                 (into [] (map :type) opts))]
      [(into [] (map :type) fixed) opts (:type-parameters member-info)])))

(defn full-class-info
  "Takes a partial dart type (as map or discrete lib + element-name as strings).
   Returns a fully populated type map."
  ([dart-type]
   (let [fci (full-class-info (:lib dart-type) (:element-name dart-type))]
     (case (:canon-qname dart-type)
       dc.Record
       (let [{:keys [positional-fields named-fields]} dart-type]
         (into (assoc fci :positional-fields positional-fields :named-fields named-fields)
           (map (fn [x] [(name (:name x)) (-> x (assoc :kind :field :getter true) (dissoc :name))]))
           (concat
             (map-indexed #(assoc %2 :name (str "$" (inc %1))) positional-fields)
             named-fields)))
       fci)))
  ([lib element-name]
   (let [{:keys [libs] :as all-nses} @nses]
     (when-some [ci (or
                      (some-> (get all-nses (-> lib libs :ns)) (get (symbol element-name)) :dart/type)
                      (analyzer-info lib element-name))]
       ; TODO: finish refectoring, here we splice :members back in class info
       (merge (:members ci) (dissoc ci :members))))))

(defn dart-member-lookup
  "member is a symbol or a string"
  ([class member env]
   (dart-member-lookup class member (meta member) env))
  ([class member member-meta {:keys [type-vars] :as env}]
   (let [member-type-arguments (map #(resolve-type % type-vars) (:type-params member-meta))
         member (name member)
         member (if (and (.endsWith member ".new")
                      (= (str (:element-name class) ".new") member))
                  (:element-name class)
                  member)]
     (when-some [class-info (full-class-info class)]
       (when-some [[type-env member-info]
                   (or
                     (when-some [member-info (class-info member)]
                       [identity member-info])
                     (some #(dart-member-lookup % member env)
                       (concat (class-info :mixins) (some-> (:super class-info) list)
                         (mapcat class-info [:interfaces :on]))))]
         [(comp ; ordering matters
            (type-env-for member member-type-arguments (:type-parameters member-info))
            (type-env-for class (:type-parameters class) (:type-parameters class-info))
            type-env)
          member-info])))))

(declare actual-parameters)

(defn actual-type [analyzer-type type-env]
  (case (:canon-qname analyzer-type)
    nil nil
    dc.Function
    (-> analyzer-type
      (update :return-type actual-type type-env)
      (update :parameters actual-parameters type-env)
      (update :type-parameters (fn [ps] (map #(actual-type % type-env) ps))))
    (if (:is-param analyzer-type)
      (type-env analyzer-type)
      (update analyzer-type :type-parameters (fn [ps] (map #(actual-type % type-env) ps))))))

(defn actual-parameters [parameters type-env]
  (some->> parameters (map #(update % :type actual-type type-env))))

(defn actual-member [[type-env member-info]]
  (case (:kind member-info)
    (:function :method)
    (-> member-info
      (update :return-type actual-type type-env)
      (update :parameters actual-parameters type-env)
      (update :type-parameters (fn [ps] (map #(actual-type % type-env) ps))))
    :field
    (update member-info :type actual-type type-env)
    :constructor
    ;; TODO: what about type-parameters
    (-> member-info
      (update :return-type actual-type type-env)
      (update :parameters actual-parameters type-env))))

(defn dart-fn-lookup [function function-info]
  (let [type-args (:type-parameters function) ; TODO
        type-params (:type-parameters function-info)
        nargs (count type-args)
        nparams (count type-params)
        type-env (cond
                   (= nargs nparams)
                   (zipmap (map :element-name type-params) (:type-parameters class))
                   (zero? nargs)
                   (zipmap (map :element-name type-params)
                     (repeat (resolve-type 'dart:core/dynamic #{})))
                   :else
                   (throw (Exception. (str "Expecting " nparams " type arguments to " class ", got " nargs "."))))]
    (actual-member
      [#(or (when (:is-param %)
              (cond-> (type-env (:element-name %))
                (:nullable %) (assoc :nullable true))) %)
       function-info])))

(defn unresolve-params
  "Takes a list of parameters from the analyzer and returns a pair
   [fixed-args opt-args] where fixed-args is a vector of symbols (with :tag meta)
   and opt-args is either a set (named) or a vector (positional) of tagged symbols."
  [parameters]
  (let [[fixed optionals]
        (split-with (fn [{:keys [kind optional]}]
                      (and (= kind :positional) (not optional))) parameters)
        opts (case (:kind (first optionals))
               :named #{}
               [])
        as-sym (fn [{:keys [name type]}]
                 (with-meta (symbol name) {:tag (unresolve-type type)}))]
    [(into [] (map as-sym) fixed)
     (into opts (map as-sym) optionals)]))

(defn- transfer-tag [actual decl]
  (let [m (meta actual)]
    (cond-> actual
      (nil? (:tag m))
      (vary-meta assoc :tag (:tag (meta decl))))))

(defn resolve-dart-method
  [type mname args type-env]
  (if-some [member-info
            (some->
              (dart-member-lookup type mname
                {:type-vars (into (or type-env #{}) (:type-params (meta mname)))})
              actual-member)] ; TODO are there special cases for operators? eg unary-
    (case (:kind member-info)
      :field
      (do
        (when-not (:type member-info)
          (throw (ex-info (pr-str member-info) {:member-info member-info})))
        [(vary-meta mname assoc (case (count args) 1 :getter 2 :setter) true :tag (unresolve-type (:type member-info))) args])
      :method
      (let [[fixeds opts] (unresolve-params (:parameters member-info))
            {:as actual :keys [opt-kind opt-params] [this & fixed-and-opts] :fixed-params}
            (parse-dart-params args)
            _ (case opt-kind
                :named
                (do
                  (when-not (<= (count opt-params) (count opts))
                    (throw (Exception. (str "Too many optional named arguments (" (- (count opt-params) (count opts))  " extra(s)) for method " mname " for " (:element-name type) " of library " (:lib type)))))
                  (when-some [missed (seq (reduce disj opts (into #{} (map first) opt-params)))]
                    (throw (Exception. (str "Missing optional named arguments " missed  " for method " mname " for " (:element-name type) " of library " (:lib type))))))
                :positional
                (when-not (<= (count opt-params) (count opts))
                  (throw (Exception. (str "Too many optional positional arguments (" (- (count opt-params) (count opts))  " extra(s)) for method " mname " for " (:element-name type) " of library " (:lib type))))))
            _ (when-not (= (count fixeds) (count fixed-and-opts))
                (throw (Exception. (str args "Fixed arity mismatch (" (count fixeds) ", " (count fixed-and-opts) ") on " mname " for " (:element-name type) " of library " (:lib type)))))
            _ (when-not (case opt-kind :named (set? opts) (vector? opts))
                (throw (Exception. (str "Optional mismatch on " mname " for " (:element-name type) "of library " (:lib type)))))
            actual-fixeds
            (into [this] (map transfer-tag fixed-and-opts fixeds))
            actual-opts
            (case opt-kind
              :named (into [] (mapcat (fn [[p d]] [(transfer-tag p (opts p)) d])) opt-params)
              :positional
              (mapcat (fn [[p v] d]
                        (let [p (transfer-tag p d)
                              tag (:tag (meta p))]
                          (when (and (nil? v) (nil? tag))
                            (throw (Exception. (str "A non-nil default value must be provided for parameter " p " of type " tag))))
                          [p v]))
                (concat (:opt-params actual) (repeat '[_ nil])) opts))]
        (when (= (:return-type member-info) {:nullable true})
          (throw (ex-info (pr-str mname member-info) {:member-info member-info})))
        [(vary-meta mname assoc :tag (unresolve-type (:return-type member-info)))
         (cond-> actual-fixeds
           (seq actual-opts)
           (-> (conj (case opt-kind :named '^:cljd/compiler .& '...))
             (into actual-opts)))]))
    #_(TODO WARN)))

(defn- expand-defprotocol [proto & methods]
  ;; TODO do something with docstrings
  (let [proto (vary-meta proto assoc :tag 'cljd.core/IProtocol)
        [doc-string & methods] (if (string? (first methods)) methods (list* nil methods))
        method-mapping
        (into {} (map (fn [[m & arglists]]
                        (let [dart-m (munge m {})
                              [doc-string & arglists] (if (string? (last arglists)) (reverse arglists) (list* nil arglists))]
                          [(with-meta m {:doc doc-string})
                           (into {} (map #(let [l (count %)] [l {:dart/name (symbol (str dart-m "$" (dec l)))
                                                                 :args %}]))
                             arglists)]))) methods)
        iface (munge proto "iface" {})
        iface (with-meta iface {:dart/name iface})
        iext (munge proto "ext" {})
        iext (with-meta iext {:dart/name iext})
        impl (munge proto "iprot" {})
        impl (with-meta impl {:dart/name impl})
        the-ns (name *current-ns*)
        full-iface (symbol the-ns (name iface))
        full-iext (symbol the-ns (name iext))
        full-proto (symbol the-ns (name proto))
        proto-map
        {:name proto
         :iface iface
         :iext iext
         :impl impl
         :full-iext full-iext
         :sigs method-mapping}]
    (list* 'do
      (list* 'definterface iface
        (for [[method arity-mapping] method-mapping
              {:keys [dart/name args]} (vals arity-mapping)]
          (list name (subvec args 1))))
      (list* 'definterface iext
        (for [[method arity-mapping] method-mapping
              {:keys [dart/name args]} (vals arity-mapping)]
          (list name args)))
      (expand-protocol-impl proto-map)
      (list 'defprotocol* proto proto-map)
      (concat
        (for [[method arity-mapping] method-mapping]
          `(defn ~method
             {:inline-arities ~(into #{} (map (comp count :args)) (vals arity-mapping))
              :inline
              (fn
                ~@(for [{:keys [dart/name] all-args :args} (vals arity-mapping)
                        :let [[[_ this] & args :as locals] (map (fn [arg] (list 'quote (gensym arg))) all-args)]]
                    `(~all-args
                      `(let [~~@(interleave locals all-args)]
                         (if (dart/is? ~'~this ~'~full-iface)
                           (. ~'~(with-meta this {:tag full-iface}) ~'~name ~~@args)
                           (. ^{:tag ~'~full-iext} (.extensions ~'~full-proto ~'~this) ~'~name ~'~this ~~@args))))))}
             ~@(for [{:keys [dart/name] [this & args :as all-args] :args} (vals arity-mapping)]
                 `(~all-args
                   (if (dart/is? ~this ~full-iface)
                     (. ~(with-meta this {:tag full-iface}) ~name ~@args)
                     (. ^{:tag ~full-iext} (.extensions ~proto ~this) ~name ~@all-args))))))
        (list proto)))))

(defn- ensure-bodies [body-or-bodies]
  (cond-> body-or-bodies (vector? (first body-or-bodies)) list))

(defn expand-extend-type [type & specs]
  (let [proto+meths (reduce
                      (fn [proto+meths x]
                        (if (symbol? x)
                          (conj proto+meths [x])
                          (conj (pop proto+meths) (conj (peek proto+meths) x))))
                      [] specs)]
    (cons 'do
      (when-not *host-eval*
        (for [[protocol & meths] proto+meths
              :let [[tag info] (resolve-symbol protocol {})
                    _ (when-not (and (= tag :def) (= :protocol (:type info)))
                        (throw (Exception. (str protocol " isn't a protocol."))))
                    type (if (nil? type) 'dart:core/Null type)
                    quoted-type
                    (case type
                      fallback "fallback"
                      (-> type (emit-type {}) type-str quote-dart-type))
                    extension-base (munge* [quoted-type
                                            (-> info :dart/qname quote-dart-type)])
                    extension-name (dont-munge extension-base "cext")
                    extension-instance (dont-munge extension-base "extension")]]
          (list 'do
            (list* `deftype extension-name []
              :type-only true
              (:full-iext info)
              (for [[mname & body-or-bodies] meths
                    [[this & args] & body] (ensure-bodies body-or-bodies)
                    :let [mname (get-in info [:sigs mname (inc (count args)) :dart/name])]]
                `(~mname [~'_ ~this ~@args] (let* [~@(when-not (= type 'fallback)
                                                       [(vary-meta this assoc :tag type) this])] ~@body))))
            (list 'def extension-instance (list 'new extension-name))
            (list 'extend-type-protocol* type (:ns info) (:name info)
              (symbol (name *current-ns*) (name extension-instance)))))))))

(defn create-host-ns [ns-sym directives]
  (let [sym (symbol (str ns-sym "$host"))]
    (remove-ns sym)
    (binding [*ns* *ns*
              *err*
              (proxy [java.io.Writer] []
                (close [])
                (flush [])
                (write ([_]) ([_ _ _])))
              #_(java.io.Writer/nullWriter)]
      (binding [*reader-resolver* nil]
        ; all nses required are already loaded except thiose
        ; specified by :host-ns which should be read with the
        ; default resolver
        (eval (list* 'ns sym directives)))
      ;; NOTE: big trick here...
      (require '[clojure.core :as cljd.core])
      *ns*)))

(defn- hint-as [x tag]
  (cond-> x (and tag (or (seq? x) (symbol? x))) (vary-meta assoc :tag tag)))

(defn- reannotate-as [x annotations]
  (cond-> x (and annotations (or (seq? x) (symbol? x))) (vary-meta assoc :annotations annotations)))

(defn- propagate-hints [expansion form]
  (if (identical? form expansion)
    expansion
    (-> expansion
      (hint-as (:tag (meta form)))
      (reannotate-as (:annotations (meta form))))))

(defn inline-expand-1 [env form]
  (->
    (if-let [[f & args] (and (seq? form) (symbol? (first form)) form)]
      (let [f-name (name f)
            [f-type f-v] (resolve-symbol f env)
            {:keys [inline-arities inline tag]} (case f-type
                                                  :def (:meta f-v)
                                                  nil)]
        (cond
          (env f) form
          (and inline-arities (inline-arities (count args)))
          (hint-as (apply inline args) tag)
          :else form))
      form)
    (propagate-hints form)))

(defn resolve-static-member [sym]
  (or
    (when-some [[_ alias t] (some->> sym namespace (re-matches #"(?:(.+)\.)?(.+)"))]
      (when-some [type (resolve-type (symbol alias t) #{} nil)]
        (let [[_ alias t] (re-matches #"(.+)\.(.+)" (name (:qname type)))]
          [(with-meta (symbol (str "$lib:" alias) t) (meta sym)) (symbol (name sym))])))
    (when-some [[_ t members] (some->> sym name (re-matches #"(?:([^.]+)\.)(.+)"))]
      (when-some [type (resolve-type (symbol (namespace sym) t) #{} nil)]
        (let [[_ alias t] (re-matches #"(.+)\.(.+)" (name (:qname type)))]
          (into [(with-meta (symbol (str "$lib:" alias) t) (meta sym))]
                (map symbol) (str/split members #"[.]")))))))

(declare cljd-closed-overs)

(defn- blame-macro [f sym]
  (fn [& args]
    (try
      (apply f args)
      (catch Exception e
        (throw (Exception. (str "While expanding macro " sym ": " (.getMessage e)) e))))))

(defn macroexpand-1 [env form]
  (->
   (if-let [[f & args] (and (seq? form) (symbol? (first form)) form)]
     (let [f-name (name f)
           [f-type f-v] (resolve-symbol f env)
           macro-fn (case f-type
                      :def (case (:type f-v) ; TODO rename :type into :kind
                             :class (fn [form _ & _] (with-meta (cons 'new form) (meta form)))
                             (some-> f-v :meta :macro-host-fn (blame-macro f)))
                      :dart (case (:kind f-v)
                              :class (fn [form _ & _] (with-meta (cons 'new form) (meta form)))
                              nil)
                      nil)]
       (cond
         (env f) form
         (or (= 'cljd.core/defprotocol f) (= 'defprotocol f)) (apply expand-defprotocol args)
         (or (= 'cljd.core/extend-type f) (= 'extend-type f)) (apply expand-extend-type args)
         (= '. f) form
         macro-fn
         (apply macro-fn form (assoc env :nses (assoc @nses :current-ns *current-ns*)
                                :closed-overs #(cljd-closed-overs % env))
           (next form))
         (.endsWith f-name ".")
         (with-meta
           (list* 'new
                  (with-meta (symbol (namespace f) (subs f-name 0 (dec (count f-name)))) (meta f))
                  args)
           (meta form))
         (.startsWith f-name ".")
         (if (and (.startsWith f-name ".-") (.endsWith f-name "!"))
           (with-meta
             (list* 'set! (list (with-meta (symbol (subs f-name 0 (dec (count f-name)))) (meta f))
                            (first args))
               (next args))
             (meta form))
           (with-meta
             (list* '. (first args) (with-meta (symbol (subs f-name 1)) (meta f)) (next args))
             (meta form)))
         :else
         (if-some [[type :as r] (resolve-static-member f)]
           (with-meta
             (list* '.
               (reduce #(list '. %1 (str "-" %2)) type (next (pop r)))
               (peek r) args)
             (meta form))
           form)))
     (if-let [[type & members] (and (symbol? form) (resolve-static-member form))]
       (with-meta
         (reduce #(list '. %1 (str "-" %2)) type members)
         (meta form))
       form))
   (propagate-hints form)))

(defn macroexpand [env form]
  (let [ex (macroexpand-1 env form)]
    (cond->> ex (not (identical? ex form)) (recur env))))

(defn macroexpand-and-inline [env form]
  (let [ex (->> form (macroexpand-1 env) (inline-expand-1 env))]
    (cond->> ex (not (identical? ex form)) (recur env))))

(declare emit infer-type magicast)

(defn tree-some? [pred branch? children root]
  (letfn [(rf [_ x]
            (when (if (branch? x)
                    (reduce rf nil (children x))
                    (pred x))
              (reduced true)))]
    (unreduced (rf nil root))))

(defn has-recur?
  "Takes a dartsexp and returns true when it contains an open recur."
  [x]
  (tree-some? #(= 'dart/recur %) seq?
    (fn [[x :as s]]
      (when-not (or (= 'dart/loop x) (= 'dart/fn x)) s))
    x))

(defn has-await?
  "Takes a dartsexp and returns true when it contains an open await."
  [x]
  (tree-some? #(= 'dart/await %) sequential? #(when-not (= (first %) 'dart/fn) %) x))

(defn- dart-binding [hint dart-expr env]
  (let [tmp (dart-local hint env)
        dart-expr (magicast dart-expr (-> tmp meta (:dart/type dc-dynamic)) env)]
    [(with-meta tmp (infer-type dart-expr)) dart-expr]))

(defn lift-safe? [expr]
  (or (not (coll? expr))
    (and (vector? expr) (-> expr meta :dart/type :canon-qname (= 'dc.List)))
    (and (seq? expr)
      (or (= 'dart/fn (first expr)) (= 'dart/new (first expr))))))

(defn liftable
  "Takes a dartsexp and returns a [bindings expr] where expr is atomic (or a dart fn)
   or nil if there are no bindings to lift."
  [x env]
  (case (when (and (seq? x) (symbol? (first x))) (first x))
    dart/let
    (let [[_ bindings expr] x]
      (if-some [[bindings' expr] (liftable expr env)]
        [(concat bindings bindings') expr]
        (if (lift-safe? expr)
          [bindings expr]
          ;; this case should not happen
          (let [[tmp :as binding] (dart-binding "" expr env)]
            [(conj (vec bindings) binding) tmp]))))
    (dart/if dart/try dart/case dart/loop) ; no ternary for now
    (let [[tmp :as binding] (dart-binding (first x) x env)]
      [[binding]
       tmp])
    dart/set!
    (let [[_ target [tmp :as binding]] x]
      [[binding [nil (list 'dart/set! target [nil tmp])]] tmp])
    dart/assert
    [[nil x] nil]
    dart/as
    (let [[_ dart-expr dart-type] x
          must-lift (loop [x dart-expr]
                      (case (when (seq? x) (first x))
                        dart/let (seq (second x)) ; must-lift when let has bindings
                        (dart/if dart/try dart/case dart/loop dart/assert dart/set!) true
                        dart/as (recur (second x))
                        false))]
      (when must-lift
        (let [[tmp :as binding] (dart-binding (with-meta 'cast {:dart/type type}) x env)]
          [[binding] tmp])))
    nil))

(defmacro ^:private with-lifted
  "CAUTION: with-lifted is NOT appropriate for preventing double evaluation. See with-once.
   Its purpose is to turn expressions which are not suitable dart expressions
   into statement."
  [[name expr] env wrapped-expr]
  `(let [~name ~expr]
     (if-some [[bindings# ~name] (liftable ~name ~env)]
       (list 'dart/let bindings# ~wrapped-expr)
       ~wrapped-expr)))

(defmacro ^:private with-once
  [[name expr] env wrapped-expr]
  `(let [~name ~expr]
     (if (symbol? ~name)
       ~wrapped-expr
       (let [[~name :as binding#] (dart-binding "once" ~name ~env)]
         (list 'dart/let [binding#] ~wrapped-expr)))))

(defn- lift-arg [must-lift dart-expr hint env]
  (or (liftable dart-expr env)
    (cond
      (lift-safe? dart-expr) [nil dart-expr]
      (and must-lift (not (:dart/const (infer-type dart-expr))))
      (let [[tmp :as binding] (dart-binding hint dart-expr env)]
        [[binding] tmp])
      :else
      [nil dart-expr])))

(defn- ensure-kw [x]
  (cond
    (keyword? x)
    (let [s (symbol (str "." (name x)))]
      (binding [*out* *err*]
        (println "DEPRECATED use" s "instead of" x "for named parameters" (source-info)))
      x)
    (dot-symbol? x)
    (-> x name (subs 1) keyword)
    :else
    (throw (Exception. (str "Expected an argument name (.argname) but got " (pr-str x))))))

(defn- split-args
  "Returns a collection of triples [name dart-code expected-type]
   where name is nil for positional parameters, dart-code MAY NOT BE A DART EXPR
   and thus must be lifted (see lift-args)."
  [args [fixed-types opts-types :as method-sig] env]
  #_{:post [(or (every? (fn [[_ _ t]] (some? t)) %) (prn args method-sig))]}
  (cond
    (nil? method-sig)
    (let [[positional-args named-args]
          (split-with (complement dot-symbol?) args)
          named-args (cond-> named-args (= '.& (first named-args)) next)]
      (-> [] (into (map #(vector nil (emit % env) dc-dynamic) positional-args))
        (into (comp (partition-all 2) (map (fn [[name x]] [(ensure-kw name) (emit x env) dc-dynamic]))) named-args)))
    ; named parameters
    (map? opts-types)
    (let [args (remove '#{.&} args) ; temporary as .& in args is redundant with args starting by a dot and the fact that we know the expected params. It's just some cpmpatibility feature with some very early cljd code
          positional-args (mapv #(vector nil (emit %1 env) (or %2 dc-dynamic)) args fixed-types)
          rem-args (drop (count positional-args) args)
          all-args (into positional-args
                     (map (fn [[k expr]]
                            (let [k (ensure-kw k)]
                              (if-some [[_ type] (find opts-types k)]
                                [k (emit expr env) (or type dc-dynamic)]
                                (throw (Exception.
                                         (str "Not an expected argument name: ." (name k)
                                           ", valid names: " (str/join ", " (map #(str "." (name %)) (keys opts-types))))))))))
                     (partition 2 rem-args))]
      (when-not (= (count positional-args) (count fixed-types))
        (throw (Exception. (str "Not enough positional arguments: expected " (count fixed-types) " got " (count positional-args)))))
      (when-not (even? (count rem-args))
        (case (count rem-args)
          1 (throw (Exception. (str "Positional argument encountered: " (pr-str (last rem-args)) ", while expecting a parameter name amongst " (str/join ", " (map #(str "." (name %)) (keys opts-types))))))
          (when-not (even? (count rem-args))
            (throw (Exception. (str "Trailing argument: " (pr-str (last rem-args))))))))
      all-args)
    :else ; positional parameters
    (let [all-args (mapv #(vector nil (emit %1 env) (or %2 dc-dynamic)) args (concat fixed-types opts-types))]
      (when-not (<= 0 (- (count args) (count fixed-types)) (count opts-types))
        (throw (Exception. (str "Wrong argument count: expecting between " (count fixed-types) " and " (+ (count fixed-types) (count opts-types)) " but got " (count args)))))
      all-args)))

(defn- simple-types
  [{:keys [canon-qname nullable] :as type}]
  (case canon-qname
    dc.dynamic [dc-Object dc-Null]
    da.FutureOr (let [[{cqn :canon-qname :as t} :as tps] (:type-parameters type)]
                  (cond->
                      [(assoc dc-Future :type-parameters tps)
                       (case cqn dc.dynamic dc-Object (dissoc t :nullable))]
                      (or nullable (:nullable t) (= cqn 'dc.dynamic))
                      (conj dc-Null)))
    (if nullable
      [dc-Null (dissoc type :nullable)]
      [type])))

(defn- nullable-type? [type]
  (or (:nullable type)
    (case (:canon-qname type)
      dc.Null true
      dc.dynamic true
      da.FutureOr (recur (first (:type-parameters type)))
      false)))

(defn- positive-type
  [{:keys [canon-qname nullable] :as type}]
  (case canon-qname
    dc.Null nil
    dc.dynamic dc-Object
    da.FutureOr (let [[t] (:type-parameters type)]
                  (if-some [t+ (positive-type t)]
                    (assoc da-FutureOr :type-parameters [t+])
                    (assoc dc-Future :type-parameters [t])))
    (dissoc type :nullable)))

(defn is-assignable?
  "Returns true when a value of type value-type can be used as a value of type slot-type."
  [slot-type value-type]
  (letfn [(is-assignable? [slot-type value-type]
            (every? (apply some-fn (map (fn [s] (fn [v] (simply-assignable? s v))) (simple-types slot-type)))
              (simple-types value-type)))
          (simply-assignable?
            [{slot-canon-qname :canon-qname :as slot-type} value-type]
            ; simple because types are not nullable and not unions
            (letfn [(assignable-type [value-type]
                      (let [{:keys [canon-qname interfaces mixins super type-parameters]}
                            (or (analyzer-info (:lib value-type) (:element-name value-type))
                              ; TODO do we still need this fallback?
                              value-type)
                            type-args (:type-parameters value-type)
                            type-env (type-env-for value-type type-args type-parameters)]
                        (cond
                          (= canon-qname slot-canon-qname)
                          ; ensure type arguments are present
                          (actual-type (assoc value-type :type-parameters type-parameters) type-env)
                          (= 'dc.Null canon-qname) nil
                          :else
                          (some #(assignable-type (actual-type % type-env))
                            (cond->> (concat interfaces mixins) super (cons super))))))]
              (when-some [value-type (assignable-type value-type)]
                (let [slot-tp (:type-parameters slot-type)
                      value-tp (:type-parameters value-type)]
                  (every? #(is-assignable? (first %) (second %)) (map vector slot-tp (concat value-tp dc-dynamic)))))))]
    (or (nil? slot-type)
      (and (some? value-type)
        (is-assignable? slot-type value-type)))))

(defn- num-type [type]
  (case (:canon-qname type)
    dc.int dc-int
    dc.double dc-double
    dc-num))

(defn simple-cast
  ([dart-expr expected-type]
   (simple-cast dart-expr expected-type (:dart/type (infer-type dart-expr))))
  ([dart-expr expected-type actual-type]
   (cond
     (= (:canon-qname expected-type) 'pseudo.super)
     (let [super-type (:super (full-class-info actual-type) 'dc.Object)
           super (if (= 'this dart-expr) 'super (:that-super (meta dart-expr)))]
       (when-not super
         (throw (Exception. "Can't tag with pseudo-type super a local which isn't a this.")))
       (with-meta super {:dart/type super-type}))
     (is-assignable? expected-type actual-type) dart-expr
     (= (:canon-qname expected-type) 'void) dart-expr
     (= (:canon-qname expected-type) 'pseudo.num-tower)
     (recur dart-expr (num-type actual-type) actual-type)
     (= (:canon-qname expected-type) 'dc.double)
     (if (int? dart-expr)
       (double dart-expr)
       (list 'dart/as dart-expr expected-type)) ; double is specially treated by write
     :else
     (list 'dart/as dart-expr expected-type))))

(defn has-cast-method? [type]
  (if-some [member-info
            (some->
              (dart-member-lookup type "cast" {:type-vars #{}})
              actual-member)]
    ; TODO check the return type
    (case (:kind member-info)
      :method (and (zero? (count (:parameters member-info))))
      nil)))

(defn magicast
  "Note (magicast x nil env) is x."
  ([dart-expr expected-type env]
   (magicast dart-expr expected-type (:dart/type (infer-type dart-expr)) env))
  ([dart-expr expected-type actual-type env]
   (cond
     (= 'dc.dynamic (:canon-qname expected-type)) dart-expr ; TODO: should be covered by is-assignable?
     (is-assignable? expected-type actual-type) dart-expr ; <1>
     (and (nullable-type? expected-type) (nullable-type? actual-type))
     (if-some [expected-type+ (positive-type expected-type)]
       (with-once [dart-expr dart-expr] env
         `(dart/if (dart/. nil "!=" ~dart-expr)
            ; by construction expected-type can't be dynamic or FutureOr<dynamic>, otherwise
            ; it would have matched the assignability test <1> above
            ~(magicast dart-expr expected-type+ actual-type env)
            nil))
       (list 'dart/let [[nil dart-expr]] nil))
     ;; When inlined #dart[], we keep it inlines
     ;; TODO: don't like the (vector? dart-expr) check, it smells bad
     (and (= 'dc.List (:canon-qname expected-type) (:canon-qname actual-type))
       (vector? dart-expr)) dart-expr
     (and
       (has-cast-method? expected-type)
       #_(#{'dc.List 'dc.Map 'dc.Set} (:canon-qname expected-type))
       (when-some [tps (seq (:type-parameters expected-type))]
         (not-every? #(= (:canon-qname %) 'dc.dynamic) tps)))
     (let [[bindings dart-expr] (lift-arg true dart-expr "castable" env)
           casted (vary-meta (dart-local 'casted {} ) assoc :dart/type expected-type)]
       (list 'dart/let
         (conj (vec bindings)
           [casted
            (if (is-assignable? (dissoc expected-type :type-parameters) (dissoc actual-type :type-parameters))
              (list 'dart/. dart-expr (into ["cast"] (:type-parameters expected-type)))
              (list 'dart/if (list 'dart/is dart-expr expected-type) ; or expected-type?
                (list 'dart/as dart-expr expected-type)
                (list 'dart/. (list 'dart/as dart-expr (dissoc expected-type :type-parameters :nullable)) (into ["cast"] (:type-parameters expected-type)))))])
         casted))
     (and
       (= (:canon-qname expected-type) 'dc.Function)
       (:parameters expected-type))
     ; TODO : generics
     (let [{:keys [return-type parameters]} expected-type
           [fixed-types optionals] (dart-method-sig expected-type)
           fixed-params (into [] (map (fn [_] (dart-local env))) fixed-types)
           [dartf :as binding] (dart-binding 'maybe-f dart-expr env)
           cljf (gensym 'maybe-f)
           wrapper-env (into {cljf dartf} (zipmap fixed-params fixed-params))
           wrapper (vary-meta (dart-local 'wrapper-f {} ) assoc :dart/type expected-type)]
       (list 'dart/let
         [binding
          [wrapper
           (list 'dart/if (list 'dart/is dartf expected-type) ; or expected-type?
             dartf
             (list 'dart/fn fixed-params :positional () nil ; TODO opts
               (emit (cons (vary-meta cljf assoc :clj true :dart nil) fixed-params) wrapper-env)))]]
         wrapper))
     :else (simple-cast dart-expr expected-type actual-type))))

(defn lift-args
  "[bindings dart-args]"
  ([split-args env]
   (lift-args false split-args env))
  ([must-lift split-args env]
   (let [[bindings dart-args]
         (reduce (fn [[bindings dart-fn-args] [k dart-x t]]
                   (let [[bindings' dart-x']
                         (lift-arg (seq bindings) (magicast dart-x t env) (or k "arg") env)]
                     [(concat bindings' bindings)
                      (cond->> (cons dart-x' dart-fn-args) k (cons k))]))
           [(when must-lift (list 'sentinel)) ()]
           (rseq (vec split-args)))
         bindings (cond-> bindings must-lift butlast)]
     [bindings dart-args])))

(def ^:dynamic *threshold* 10)

(defn emit-fn-call [[f & args] env]
  ;; TO BE CONTINUED
  ; now we have :dart/parameters and :dart/return-type on meta of dart-f to guide casting
  (let [dart-f (emit f env)
        fn-type (if (some dot-symbol? args)
                  :native
                  (let [{:dart/keys [fn-type type]} (infer-type dart-f)]
                    (or fn-type (case (:canon-qname type)
                                  dc.Function :native
                                  (nil dc.Object dc.dynamic pseudo.not-bool) nil
                                  :ifn))))
        [bindings dart-args] (lift-args (nil? fn-type)
                               (split-args args (some-> dart-f meta :dart/signature dart-method-sig) env) env)
        [bindings' dart-f] (lift-arg (or (nil? fn-type) (seq bindings)) dart-f "f" env)
        bindings (concat bindings' bindings)
        native-call (when-not (= :ifn fn-type)
                      (cons
                        (case fn-type
                          :native dart-f
                          (list 'dart/as dart-f dc-Function))
                        dart-args))
        ifn-call (when-not (= :dart fn-type)
                   (let [dart-f (if (and fn-type (not (get-in (infer-type dart-f) [:dart/type :nullable])))
                                  dart-f
                                  ; cast when unknown
                                  (list 'dart/as dart-f (emit-type 'cljd.core/IFn$iface {})))]
                     (if (< (count dart-args) *threshold*)
                       (list* 'dart/. dart-f
                         (resolve-protocol-mname-to-dart-mname 'cljd.core/IFn '-invoke (inc (count dart-args)) (:type-vars env))
                         dart-args)
                       (list* 'dart/. dart-f
                         (resolve-protocol-mname-to-dart-mname 'cljd.core/IFn '-invoke-more (inc *threshold*) (:type-vars env))
                         (conj (subvec (vec dart-args) 0 (dec *threshold*))
                           (subvec (vec dart-args) (dec *threshold*)))))))
        dart-fn-call
        (case fn-type
          :native native-call
          :ifn ifn-call
          (list 'dart/if (list 'dart/is dart-f dc-Function)
            native-call
            (list 'dart/if (list 'dart/is dart-f (emit-type 'cljd.core/IFn$iface {}))
              ifn-call
              (let [[meth & args] (nnext ifn-call)] ; "callables" must be handled by an extesion on dynamic or Object
                (list* 'dart/. (list 'dart/. (emit 'cljd.core/IFn env) 'extensions dart-f) meth (cons dart-f args))))))]
    (cond->> dart-fn-call
      (seq bindings) (list 'dart/let bindings))))

(defn emit-dart-list-literal
  [maybe-quoted-emit x env]
  (else->>
    (let [item-tag (:tag (meta x) 'dart:core/dynamic)
          list-tag (vary-meta 'dart:core/List assoc :type-params [item-tag])])
    (if (:fixed (meta x))
      (if-some [[item & more-items] (seq x)]
        (let [lsym (dart-local (with-meta 'fl {:tag list-tag}) env)]
          (list 'dart/let
            (cons
              [lsym (with-lifted [item (maybe-quoted-emit item env)] env
                      (list 'dart/. (emit-type list-tag env) "filled" (count x) item))]
              (map-indexed (fn [i item]
                             [nil (with-lifted [item (maybe-quoted-emit item env)] env
                                    (list 'dart/. lsym "[]=" (inc i) item))])
                more-items))
            lsym))
        (emit (list '. list-tag 'empty) env)))
    (let [item-type (or (resolve-type item-tag (:type-vars env)) dc-dynamic)
          [bindings items] (lift-args (for [item x] [nil (emit item env) item-type]) env)]
      (cond->> #_(vec items)
               (with-meta (vec items) (meta (dart-local (with-meta 'fl {:tag list-tag}) env)))
               (seq bindings) (list 'dart/let bindings)))))

(defn emit-dart-record-literal
  [x env]
  (let [split-args+types (split-args x nil env)
        [bindings dart-record-args] (lift-args split-args+types env)
        sig (map #(if (keyword? %) % (infer-type %)) dart-record-args)
        positional-fields (into [] (take-while (complement keyword?)) sig)
        named-fields (into [] (comp (drop-while (complement keyword?)) (partition-all 2)
                                (map (fn [[k t]] {:name (name k) :type t}))) sig)]
    (cond->> (with-meta (list* 'dart/record dart-record-args)
               {:dart/type (assoc dc-Record :positional-fields positional-fields :named-fields named-fields)
                :dart/const (every? :dart/const (remove keyword? sig))
                :dart/inferred true})
      (seq bindings) (list 'dart/let bindings))))

(defn emit-dart-literal
  [x env]
  (cond
    (vector? x)
    (emit-dart-list-literal emit x env)

    (list? x)
    (emit-dart-record-literal x env)
    :else
    (throw (ex-info (str "Unsupported dart literal #dart " (pr-str x)) {:form x}))))

(defn emit-coll
  ([coll env] (emit-coll emit coll env))
  ([maybe-quoted-emit coll env]
   (cond
     (seq (meta coll))
     (with-lifted [data (maybe-quoted-emit (with-meta coll nil) env)] env
       (with-lifted [metadata (maybe-quoted-emit (meta coll) env)] env
         (list (emit 'cljd.core/with-meta env) data metadata)))
     (seq coll)
     (let [items (into [] (if (map? coll) cat identity) coll)
           fn-sym (cond
                    (map? coll) 'cljd.core/-map-lit
                    (vector? coll) (if (< 32 (count coll)) 'cljd.core/vec 'cljd.core/-vec-owning)
                    (set? coll) 'cljd.core/set
                    (seq? coll) 'cljd.core/-list-lit
                    :else (throw (ex-info (str "Can't emit collection " (pr-str coll)) {:form coll}))) ]
       (with-lifted [fixed-list (emit-dart-list-literal maybe-quoted-emit (with-meta (vec items) {:fixed true}) env)] env
         (list (emit fn-sym env) fixed-list)))
     :empty-coll
     (emit
       (cond
         (map? coll) 'cljd.core/-EMPTY-MAP
         (vector? coll) 'cljd.core/-EMPTY-VECTOR
         (set? coll) 'cljd.core/-EMPTY-SET
         (seq? coll) 'cljd.core/-EMPTY-LIST ; should we use apply list?
         :else (throw (ex-info (str "Can't emit collection " (pr-str coll)) {:form coll})))
       env))))

(defn emit-new [[_ class & args] env]
  (let [dart-type (emit-type class env)
        member-info (some-> (dart-member-lookup dart-type (:element-name dart-type) env) actual-member)
        _ (when-not (= :constructor (:kind member-info))
            (throw (Exception. (str "Can't resolve default constructor for type " (:element-name dart-type "dynamic") " of library " (:lib dart-type "dart:core") " " (source-info)))))
        method-sig (some-> member-info dart-method-sig)
        split-args+types (split-args args method-sig env)
        [bindings dart-args] (lift-args split-args+types env)]
    (cond->> (with-meta (list* 'dart/new dart-type dart-args)
               {:dart/type dart-type
                :dart/const (and (:const member-info)
                              (every? (comp :dart/const infer-type)
                                (remove keyword? dart-args)))
                :dart/inferred true})
      (seq bindings) (list 'dart/let bindings))))

(defn- fake-member-lookup [type! member n]
  (case (:canon-qname type!)
    dc.bool
    (case (name member)
      ("^^" "&&" "||") [identity
                        {:kind :method ; no need to gen :operator
                         :return-type dc-bool
                         :parameters
                         (repeat n {:kind :positional
                                    :type dc-bool})}]
      nil)
    nil))

(defn emit-dot [[_ obj member & args :as form] env]
  (if (seq? member)
    ;; legacy syntax of Clojure early days (. obj (meth arg1 arg2)))
    (recur (list* '. obj member) env)
    (let [[_ prop member-name] (re-matches #"(-)?(.+)" (name member))
          _ (when (and prop args) (throw (ex-info (str "Can't pass arguments to a property access" (pr-str form)) {:form form})))
          [dart-obj-bindings dart-obj]  (lift-arg nil (emit obj env) "obj" env)
          static (:dart/class (meta dart-obj))
          type (or static (:dart/type (infer-type dart-obj)))
          type! (dissoc type :nullable)
          member-name (cond-> member-name
                        (and prop (not (re-matches #"[a-zA-Z_$0-9]+" member-name)))
                        munge-str)
          num-only-member-name (when (.startsWith member-name "num:") (subs member-name 4))
          type! (cond-> type! num-only-member-name num-type)
          member-name (or num-only-member-name member-name)
          member-name (if (and (= "-" member-name) (empty? args)) "unary-" member-name)
          member-name+ (case member-name "!=" "==" member-name)
          member-info (some->
                        (else->>
                          (if-some [mi (fake-member-lookup type! member-name+ (count args))] mi)
                          (let [[_ member-info :as mi] (dart-member-lookup type! member-name+ (meta member) env)])
                          ;; in case a property/method has the same name of a named constructor
                          (or (when-not (and static (not (:static member-info))) mi))
                          (if static (dart-member-lookup type! (str (:element-name static) "." member-name) env))
                          (dart-member-lookup dc-Object member-name+ env))
                        actual-member)
          ; trying to munge name, just in case (and for records etc.)
          member-name' (when-not member-info (munge member-name {}))
          member-info (or member-info
                        (some-> (dart-member-lookup type! member-name' (meta member) env) actual-member))
          member-name (if (and member-name' member-info) member-name' member-name)
          ; end of trying to munge name
          member-name (:member-name member-info member-name)
          dart-obj (cond
                     (= "==" member-name+) dart-obj
                     (:type-params (meta obj)) ; static only
                     (emit-type obj env)
                     member-info
                     (simple-cast dart-obj type! type)
                     :else
                     (loop [dart-obj dart-obj]
                       (cond
                         (and (seq? dart-obj) (= 'dart/as (first dart-obj)))
                         (recur (second dart-obj))
                         (= 'dc.dynamic (:canon-qname (or (:dart/type (infer-type dart-obj)) dc-dynamic)))
                         dart-obj
                         :else
                         (list 'dart/as dart-obj dc-dynamic))))
          _ (when (and static (not (or (:static member-info) (= :constructor (:kind member-info)))))
              (throw (Exception. (str member-name " is neither a constructor nor a static member of " (:element-name type!) " " (source-info)))))
          _ (when (not member-info)
              (binding [*out* *err*]
                (println "DYNAMIC WARNING: can't resolve member" member-name "on target type" (:element-name type! "dynamic") "of library" (:lib type! "dart:core") (source-info))))
          special-num-op-sig (case (:canon-qname type!) ; see sections 17.30 and 17.31 of Dart lang spec
                               dc.int (case member-name
                                        ("-" "+" "%" "*")
                                        [[pseudo-num-tower]]
                                        nil)
                               nil)
          special-equality-sig (case member-name
                                 ("!=" "==") [[dc-dynamic]] ; see 17.26 Equality
                                 nil)
          method-sig (or special-num-op-sig special-equality-sig
                       (some-> member-info dart-method-sig))
          split-args+types (split-args args (when-not prop method-sig) env)
          [dart-args-bindings dart-args] (lift-args split-args+types env)
          prop (case (:kind member-info)
                 :field (do
                          (when-not (or prop static)
                            (binding [*out* *err*]
                              (println "INFO:" member-name "is a property and should be prefixed by a dash to work even when the type of the object is dynamic:" (str "-" member-name) (source-info))))
                          true)
                 ; DO NOT emit warning when prop and method/ctor because
                 ; it's valid: it's a tear-off
                 ; https://dart.dev/guides/language/effective-dart/usage#dont-create-a-lambda-when-a-tear-off-will-do
                 ; TODO: properly infer type of tear-offs
                 (nil :method :constructor) prop)
          name (if prop member-name (into [member-name] (map #(emit-type % env)) (:type-params (meta member))))
          op (if prop 'dart/.- 'dart/.)
          expr-type (or (if special-num-op-sig
                          (num-type (:dart/type (infer-type (first dart-args))))
                          (cond
                            (not prop) (:return-type member-info)
                            (= :method (:kind member-info)) nil ; TODO type delegate
                            :else (:type member-info)))
                      dc-dynamic)
          const (and (:const member-info)
                     (or prop
                         (every? (comp :dart/const infer-type)
                                 (remove keyword? dart-args))))
          expr (with-meta (list* op dart-obj name dart-args)
                 {:dart/type expr-type :dart/const const :dart/inferred true})
          bindings (concat dart-obj-bindings dart-args-bindings)
          ; peephole optimizations
          expr
          (case member-name
            "==" (let [dart-arg (first dart-args)]
                   (cond
                     (and (nil? dart-obj) (not (nullable-type? (:dart/type (infer-type dart-arg)))))
                     false
                     (and (nil? dart-arg) (not (nullable-type? (:dart/type (infer-type dart-obj)))))
                     false
                     :else expr))
            "!=" (let [dart-arg (first dart-args)]
                   (cond
                     (and (nil? dart-obj) (not (nullable-type? (:dart/type (infer-type dart-arg)))))
                     true
                     (and (nil? dart-arg) (not (nullable-type? (:dart/type (infer-type dart-obj)))))
                     true
                     :else expr))
            expr)]
      (cond->> expr
        (seq bindings) (list 'dart/let bindings)))))

(defn resolve-field-for-set! [obj member env]
  (let [[_ prop member-name] (re-matches #"(-)?(.+)" (name member))
        [dart-obj-bindings dart-obj]  (lift-arg nil (emit obj env) "obj" env)
        static (:dart/class (meta dart-obj))
        type (or static (:dart/type (infer-type dart-obj)))
        type! (dissoc type :nullable)
        member-info (some->
                      (else->>
                        (let [[_ member-info :as mi]
                              (dart-member-lookup type! member-name (meta member) env)])
                        ;; in case a property/method has the same name of a named constructor
                        (or (when-not (and static (not (:static member-info))) mi))
                        (if static (dart-member-lookup type! (str (:element-name static) "." member-name) env))
                        (dart-member-lookup dc-Object member-name env))
                      actual-member)
        dart-obj (cond
                   (:type-params (meta obj)) ; static only
                   (emit-type obj env)
                   member-info
                   (simple-cast dart-obj type! type)
                   :else
                   (loop [dart-obj dart-obj]
                     (cond
                       (and (seq? dart-obj) (= 'dart/as (first dart-obj)))
                       (recur (second dart-obj))
                       (= 'dc.dynamic (:canon-qname (or (:dart/type (infer-type dart-obj)) dc-dynamic)))
                       dart-obj
                       :else
                       (list 'dart/as dart-obj dc-dynamic))))]
    (when (and static (not (:static member-info)))
      (throw (Exception. (str member-name " is not a static member of " (:element-name type!) " " (source-info)))))
    (cond
      (not member-info)
      (binding [*out* *err*]
        (println "DYNAMIC WARNING: can't resolve member" member-name "on target type" (:element-name type! "dynamic") "of library" (:lib type! "dart:core") (source-info)))
      (not= (:kind member-info) :field)
      (throw (Exception. (str member-name " is not a field of " (:element-name type!) " " (source-info)))))
    [dart-obj-bindings dart-obj member-name (or (:type member-info) dc-dynamic)]))

(defn emit-set! [[_ target expr] env]
  (let [target (macroexpand env target)]
    (cond
      (symbol? target)
      (let [dart-sym (emit target env)
            [tag info] (resolve-symbol target env)]
        (case tag
          :local
          (when-not (-> info meta :dart/mutable)
            (throw (ex-info (str "Cannot assign to non-mutable: " target) {:target target})))
          :def (when-not (= :field (:type info))
                 (throw (ex-info (str "Cannot assign: " target) {:target target})))
          :dart (when-not (and (= :field (:kind info)) (:setter info))
                  (throw (ex-info (str "Cannot assign: " target) {:target target}))))
        (list 'dart/set! dart-sym
          (dart-binding 'setval (emit expr env) env)))
      (and (seq? target) (= '. (first target)))
      (let [[_ obj member] target
            [bindings dart-obj member dart-type] (resolve-field-for-set! obj member env)]
        (cond->> (list 'dart/set! (list 'dart/.- dart-obj member)
                   (dart-binding 'setval (emit expr env) env))
          bindings (list 'dart/let bindings)))
      :else
      (throw (ex-info (str "Unsupported target for assignment: " target) {:target target})))))

(defn emit-let* [[_ bindings & body] env]
  (let [[dart-bindings env]
        (reduce
         (fn [[dart-bindings env] [k v]]
           (let [[tmp dart-expr :as binding] (dart-binding k (emit v env) env)]
             (if (:dart/const (meta tmp))
               [dart-bindings (assoc env k dart-expr)]
               [(conj dart-bindings binding) (assoc env k tmp)])))
         [[] env] (partition 2 bindings))
        dart-bindings
        (into dart-bindings (for [x (butlast body)] [nil (emit x env)]))]
    (cond->> (emit (last body) env)
      ; wrap only when there are actual bindings
      (seq dart-bindings) (list 'dart/let dart-bindings))))

(defn emit-do [[_ & body] env]
  (emit (list* 'let* [] body) env))

(defn emit-loop* [[_ bindings & body] env]
  (let [[dart-bindings env]
        (reduce
         (fn [[dart-bindings env] [k v]]
           (let [dart-v (emit v env)
                 {:dart/keys [type]} (infer-type dart-v)
                 decl (vary-meta (dart-local k env) assoc :dart/loop-local true)
                 decl (cond-> decl
                        (nil? (:dart/type (meta decl)))
                        (vary-meta assoc :dart/type dc-dynamic))]
             [(conj dart-bindings [decl (magicast dart-v (-> decl meta :dart/type) env)])
              (assoc env k decl)]))
         [[] env] (partition 2 bindings))]
    (list 'dart/loop dart-bindings (emit (list* 'let* [] body) env))))

(defn emit-recur [[_ & exprs] env]
  (with-meta (cons 'dart/recur (map #(emit % env) exprs))
    {:dart/type     dc-Never
     :dart/inferred true}))

(defn emit-if [[_ test then else] env]
  (cond
    (or (coll? test) (symbol? test))
    (let [dart-test (emit test env)]
      (cond
        (true? dart-test) (emit then env)
        (false? dart-test) (emit else env)
        :else
        (let [truth (dart-type-truthiness (:dart/type (infer-type dart-test)))
              [bindings dart-test] (lift-arg (nil? truth) dart-test "test" env)
              dart-test (case truth
                          (:boolean :falsy :truthy) dart-test
                          :some (list 'dart/. dart-test "!=" nil)
                          (list 'dart/. (list 'dart/. dart-test "!=" false) "&&" (list 'dart/. dart-test "!=" nil)))]
          (case truth
            :falsy (list 'dart/let (conj (vec bindings) [nil dart-test]) (emit else env))
            :truthy (list 'dart/let (conj (vec bindings) [nil dart-test]) (emit then env))
            (cond->> (list 'dart/if dart-test (emit then env) (emit else env))
              (seq bindings) (list 'dart/let bindings))))))
    test
    (emit then env)
    :else
    (emit else env)))

(defn cljd-u32 [n] (bit-and 0xFFFFFFFF n))

(defn cljd-hash-combine [seed hash]
  (cljd-u32
    (bit-xor seed
      (+ hash 0x9e3779b9
        (bit-shift-left seed 6)
        (bit-shift-right seed 2)))))

(defn cljd-hash
  "Returns the hash for x in cljd."
  [x]
  (cljd-u32
    (cond
      (nil? x) 0
      (and (integer? x) (<= -0x100000000 x 0xFFFFFFFF)) (hash x) ; safe between -2^31 to 2^32-1 both inclusive (TODO check in dartjs)
      (string? x) (hash x) ; cljd hashes string like clj/jvm
      (boolean? x) (case x true 1231 false 1237)
      (char? x) (hash (str x))
      (symbol? x) (cljd-hash-combine
                    (cljd-u32 (clojure.lang.Murmur3/hashUnencodedChars (name x)))
                    (hash (namespace x)))
      (keyword? x)
      (cljd-hash-combine
        (or (some-> x namespace cljd-hash) 0)
        (cljd-hash (name x)))
      :else (throw (ex-info (str "cljd-hash not implemented for " (class x)) {:x x})))))

(defn emit-case* [[op expr clauses default] env]
  (cond
    (empty? clauses) (emit default env)
    (or (every? #(or (char? % ) (string? %)) (mapcat first clauses))
      (every? int? (mapcat first clauses)))
    (list 'dart/case (emit expr env)
      (for [[vs e] clauses]
        [(map #(emit % {}) vs) (emit e env)])
      (emit default env))
    :else
    (let [dart-local (env expr)
          by-hashes (group-by (fn [[v e]] (cljd-hash v)) (for [[vs e] clauses, v vs] [v e]))
          [tmp :as binding] (dart-binding 'hash (emit (list 'cljd.core/hash expr) env) env)]
      (list 'dart/let [binding]
        (list 'dart/case tmp
          (for [[h groups] by-hashes]
            (list
              [h]
              (reduce (fn [else [v e]]
                        ; TODO constant extraction
                        (list 'dart/if (emit (list 'cljd.core/= (list 'quote v) expr) env)
                          (emit e env)
                          else))
                '(dart/continue _default) (rseq groups))))
          (emit default env))))))

(defn- variadic? [[params]] (some #{'&} params))

(defn- resolve-invoke [n]
  (if (<= *threshold* n)
    (symbol (str "$_invoke$ext" n))
    (resolve-protocol-mname-to-dart-mname 'cljd.core/IFn '-invoke (inc n) #{})))

(defn- emit-ifn-arities-mixin [fixed-arities base-vararg-arity]
  (let [max-fixed-arity (some->> fixed-arities seq (reduce max))
        min-fixed-arity (some->> fixed-arities seq (reduce min))
        n (or base-vararg-arity max-fixed-arity)
        mixin-name (munge (str "IFnMixin-" (apply str (map (fn [i] (if (fixed-arities i) "X"  "u")) (range n))) (cond (= base-vararg-arity max-fixed-arity) "Y" base-vararg-arity "Z" :else "X")) {})
        synth-params (mapv #(symbol (str "arg" (inc %))) (range (max n (dec *threshold*))))
        more-param (gensym 'more)
        this 'this
        fixed-invokes (for [n fixed-arities
                            :when (< n *threshold* )]
                        `(~'-invoke [~this ~@(subvec synth-params 0 n)]))
        invoke-exts (for [n fixed-arities
                          :when (<= *threshold* n)]
                      `(~(symbol (str "$_invoke$ext" n)) [~this ~@(subvec synth-params 0 n)]))
        vararg-mname '$_invoke$vararg
        vararg-invokes
        (when base-vararg-arity
          (cons
            `(~vararg-mname [~this ~@(subvec synth-params 0 base-vararg-arity) ~'etc])
            (let [base-args (subvec synth-params 0 base-vararg-arity)
                  from (cond-> base-vararg-arity max-fixed-arity (max (inc max-fixed-arity)))]
              (for [n (range from *threshold*)
                    :let [rest-args (subvec synth-params base-vararg-arity n)]]
                `(~'-invoke [~this ~@base-args ~@rest-args]
                  (. ~this ~vararg-mname ~@base-args (seq ~(tagged-literal 'dart rest-args))))))))
        max-fixed-arity (or base-vararg-arity max-fixed-arity)
        invoke-more
        (when (or base-vararg-arity (seq invoke-exts))
          (let [more-params (subvec synth-params 0 (dec *threshold*))
                vararg-call
                (when base-vararg-arity
                  (let [above-threshold (- base-vararg-arity *threshold*)]
                    (if (neg? above-threshold)
                      `(. ~this ~vararg-mname
                          ~@(take base-vararg-arity more-params)
                          (seq (.+ ~(tagged-literal 'dart (vec (drop base-vararg-arity more-params)))
                                 ~more-param)))
                      (let [more-destructuring (conj (subvec synth-params (dec *threshold*)) '& 'etc)
                            bound-vars (remove #{'&} more-destructuring)]
                        `(if (.< ~above-threshold (count ~more-param))
                           (let [~more-destructuring ~more-param]
                             (. ~this ~vararg-mname ~@more-params ~@bound-vars))
                           (throw (dart:core/ArgumentError. "No matching arity")))))))]
            `(~'-invoke-more [~'this ~@more-params ~more-param]
              ~(if (seq invoke-exts)
                 `(~'case (count ~more-param)
                    ~@(mapcat (fn [[meth params]]
                                (let [ext-params (subvec params *threshold*)]
                                  [(count ext-params)
                                   `(let [~ext-params ~more-param]
                                      (. ~this ~meth ~@more-params ~@ext-params))])) invoke-exts)
                    ~@(some-> vararg-call list)) ; if present vararg is the default
                 vararg-call))))
        call+apply
        (let [call-args (cond->> synth-params (not base-vararg-arity) (take (inc max-fixed-arity)))
              fixed-args (cond->> call-args base-vararg-arity (take base-vararg-arity))
              base-arity (or min-fixed-arity base-vararg-arity)
              base-args (take base-arity call-args)
              opt-args (drop base-arity call-args)
              default-value `(dart:core/pragma "cljd:missing-param")
              fixed-arities-expr
              (for [args+1 (next (reductions conj (vec base-args)
                                   (cond->> opt-args base-vararg-arity (take (- base-vararg-arity base-arity)))))
                    :let [n+1 (count args+1)]]
                [args+1 `(. ~this
                           ~(resolve-invoke (dec n+1))
                           ~@(pop args+1))])]
          `((~'call [~this ~@base-args ... ~@(interleave opt-args (repeat default-value))]
             (cond
               ~@(mapcat (fn [[args+1 expr]] `((.== ~(peek args+1) ~default-value) ~expr)) fixed-arities-expr)
               :else ~(if base-vararg-arity
                        (if-some [[first-rest-arg :as rest-args] (seq (drop base-vararg-arity call-args))]
                          `(if (dart:core/identical ~first-rest-arg ~default-value)
                             (. ~this ~(resolve-invoke (count fixed-args))
                               ~@fixed-args)
                             (. ~this ~vararg-mname ~@fixed-args
                               (seq (.toList
                                      (.takeWhile ~(tagged-literal 'dart (vec rest-args))
                                        (fn [e#] (.!= e# ~default-value)))))))
                          `(. ~this ~vararg-mname ~@fixed-args nil))
                        `(. ~this ~(resolve-invoke (count fixed-args)) ~@fixed-args))))
            (~'-apply [~this ~more-param]
             (let [~more-param (seq ~more-param)]
               ~(reduce (fn [body [args+1 expr]]
                          `(if (nil? ~more-param)
                             ~expr
                             (let* [~(peek args+1) (first ~more-param)
                                    ~more-param (next ~more-param)]
                               ~body)))
                  (let [vararg-call (if base-vararg-arity
                                      `(. ~this ~vararg-mname ~@fixed-args ~more-param)
                                      `(throw (dart:core/ArgumentError. "No matching arity")))
                        fixed-method (when (fixed-arities base-vararg-arity) (resolve-invoke base-vararg-arity))]
                    (cond->> vararg-call
                      fixed-method (list `if `(nil? ~more-param) `(. ~this ~fixed-method ~@fixed-args))))
                  (reverse
                    (concat
                      (for [args+1 (next (reductions conj [] base-args))]
                        [args+1 `(throw (dart:core/ArgumentError. "No matching arity"))])
                      fixed-arities-expr)))))))]
    (binding [*current-ns* 'cljd.core]
      (emit
        `(deftype ~(vary-meta (dont-munge mixin-name nil) assoc :mixin true) []
           cljd.core/IFn
           ~@fixed-invokes
           ~@invoke-exts
           ~@vararg-invokes
           ~@(some-> invoke-more list)
           ~@call+apply)
        {}))
    mixin-name))

(defn- ensure-ifn-arities-mixin [fixed-arities base-vararg-arity]
  (let [fixed-arities (set fixed-arities)
        path [:ifn-mixins fixed-arities base-vararg-arity]]
    (or (when-some [mixin (get-in @nses path)]
          (when (get-in @nses [(symbol (namespace mixin)) (symbol (name mixin))])
            mixin))
      (let [mixin-name (emit-ifn-arities-mixin fixed-arities base-vararg-arity)
            mixin-name (symbol "cljd.core" (name mixin-name))]
        (swap! nses assoc-in path mixin-name)
        mixin-name))))

(defn- emit-ifn [async var-name name bodies env]
  (let [fixed-bodies (remove variadic? bodies)
        fixed-arities (some->> fixed-bodies seq (map first) (map count))
        [vararg-params & vararg-body] (some #(when (variadic? %) %) bodies)
        base-vararg-arity (some->> vararg-params (take-while (complement #{'&})) count)
        arities-mixin (ensure-ifn-arities-mixin fixed-arities base-vararg-arity)
        this (vary-meta (or name (gensym "this")) assoc :clj true)
        methods
        (cond->>
            (for [[params & body] fixed-bodies
                  :let [n (count params)
                        mname
                        (if (< n *threshold*)
                          '-invoke
                          (symbol (str "$_invoke$ext" n)))] ]
              `(~mname [~this ~@params] ~@body))
          vararg-params
          (cons
            `(~'$_invoke$vararg [~this ~@(-> vararg-params pop pop)
                                 ~(peek vararg-params)]
              ~@vararg-body)))
        methods
        (for [[mname [this & params] & body] methods]
          (list (cond-> mname async (with-meta {:async true}))
            (into [this] (map #(vary-meta % dissoc :tag) params))
            `(let [~@(mapcat (fn [p] (when (:tag (meta p)) [(vary-meta p dissoc :tag) p])) params)]
               ~@body)))
        dart-sexpr (emit `(~'reify
                           :var-name ~var-name
                           :name-hint ~name
                           ~(vary-meta arities-mixin assoc :mixin true)
                           cljd.core/Fn
                           cljd.core/IFn
                           ~@methods)
                     env)
        [tmp :as binding] (if name [(env name) dart-sexpr] (dart-binding '^:clj f dart-sexpr env))]
    (list 'dart/let [binding] tmp)))

(defn- no-future [type]
  (case (:canon-qname type)
    (da.FutureOr da.Future)
    (cond-> (-> type :type-parameters first) (nullable-type? type) (assoc :nullable true))
    nil dc-dynamic
    type))

(defn- ensure-future [type]
  (assoc dc-Future :type-parameters [(no-future (or type dc-dynamic))]))

(defn- leaves
  [branch? children tree]
  ; reduced not properly handled
  (reify clojure.lang.IReduceInit
    (reduce [this rf init]
      (letfn [(visit [acc x]
                (if (branch? x)
                  (reduce visit acc (children x))
                  (rf acc x)))]
        (visit init tree)))))

(defn closed-overs
  "Returns the set of dart locals (values of the env) referenced in the emitted code."
  [emitted env]
  (let [dart-locals
        (into #{}
          (keep
            (fn [[k dart-expr]]
              (cond
                (not (symbol? k)) nil
                (symbol? dart-expr) dart-expr
                (and (seq? dart-expr) (= 'dart/as (first dart-expr)))
                (recur [k (second dart-expr)])
                (:dart/const (infer-type dart-expr)) nil
                (and (seq? dart-expr) (= 'dart/fn (first dart-expr))) nil
                :else (throw (ex-info (str "Unexpected dart value in environment for key " k ": " (pr-str dart-expr)) {:dart-expr dart-expr})))))
          env)]
    (into #{} (comp (filter symbol?) (keep dart-locals))
      (leaves sequential? identity emitted)
      #_(tree-seq sequential? seq emitted))))

(defn cljd-closed-overs [expr env]
  (let [dart-locals (closed-overs (emit expr env) env)]
    (into #{} (keep (fn [[k v]] (when (dart-locals v) k))) env)))

(defn- emit-dart-fn [async fn-name [params & body] env]
  (let [ret-type (some-> (or (:tag (meta fn-name)) (:tag (meta params))) (emit-type env))
        ; there's definitely an overlap between parse-dart-params and dart-method-sig
        ; TODO we should strive to consolidate them as one if possible
        {:keys [fixed-params opt-kind opt-params]} (parse-dart-params params)
        [env dart-fixed-params]
        (reduce
          (fn [[env dart-fixed-params] p]
            (let [local (dart-local p env)
                  param (vary-meta local dissoc :dart/type)]
              [(assoc env p (simple-cast param (:dart/type (meta local) dc-dynamic) dc-dynamic))
               (conj dart-fixed-params param)]))
          [env []] fixed-params)
        [env dart-opt-params]
        (reduce
          (fn [[env dart-opt-params] [p d]]
            (let [param (with-meta p nil)] ; symbol name used as is, must be valid identifier
              [(assoc env p (simple-cast param (:dart/type (dart-meta p env) dc-dynamic) dc-dynamic))
               (conj dart-opt-params
                 [param (emit d env)])]))
          [env []] opt-params)
        function-type
        (assoc dc-Function
          :return-type (cond-> (or ret-type dc-dynamic) async ensure-future)
          :parameters (-> []
                        (into (map (fn [n]
                                     {:name n
                                      :kind :positional
                                      :type dc-dynamic})) dart-fixed-params)
                        (into (map (fn [[n _]] ; default value has no place in types
                                     {:name n
                                      :kind opt-kind
                                      :optional true
                                      :type dc-dynamic}))
                          dart-opt-params)))
        env
        (cond-> env
          fn-name
          (update fn-name vary-meta assoc :dart/type function-type :dart/ret-type (:return-type function-type)))
        ; mutable-locals : mutable-dart -> immutable-dart
        mutable-locals (into {}
                         (keep (fn [[clj dart]]
                                 (when (or (:dart/loop-local (meta dart)) (:dart/mutable (meta dart)))
                                   [dart
                                    (with-meta (dart-local (name clj) env)
                                      (dissoc (meta dart) :dart/mutable :dart/loop-local))])))
                         env)
        ; mutable-locals : immutable-dart -> mutable-dart
        safe-locals (into {} (map (fn [[k v]] [v k])) mutable-locals)
        safe-env ; no mutables
        (into env
          (keep (fn [[clj dart]]
                  (when-some [safe-dart (mutable-locals dart)]
                    [clj safe-dart])))
          env)
        dart-body (emit (cons 'do body) safe-env)
        async (or async (has-await? dart-body))
        mutable-closed-overs (into [] (keep #(find safe-locals %)) (closed-overs dart-body safe-env))
        recur-params (when (has-recur? dart-body) dart-fixed-params)
        dart-fixed-params (if recur-params
                            (map #(with-meta (dart-local % env) nil) fixed-params) ; regen new fixed params
                            dart-fixed-params)
        dart-body (cond->> dart-body
                    recur-params
                    (list 'dart/loop (map vector recur-params dart-fixed-params)))
        body-type (:dart/type (infer-type dart-body))
        dart-body (if ret-type
                    (with-lifted [dart-expr dart-body] env
                      (simple-cast dart-expr
                        (if async
                          (assoc da-FutureOr :type-parameters [ret-type])
                          ret-type)))
                    dart-body)
        ret-type (let [ret-type' (or ret-type body-type dc-dynamic)]
                   (cond-> ret-type'
                     ;; We don't want to emit a fn with dc.Never type as it would lead to subtle bugs
                     async ensure-future))
        dart-fn
        (-> (list 'dart/fn dart-fixed-params opt-kind dart-opt-params async dart-body)
          (vary-meta assoc
            :dart/ret-type  ret-type
            :dart/fn-type   :native
            :dart/type      (assoc function-type :return-type ret-type)))
        dart-fn
        (if fn-name
          (let [dart-fn-name (with-meta (env fn-name) (meta dart-fn))]
            (list 'dart/let [[dart-fn-name dart-fn]] dart-fn-name))
          dart-fn)]
    (if (seq mutable-closed-overs)
      (list 'dart/let mutable-closed-overs dart-fn)
      dart-fn)))

(defn emit-fn* [[fn* & bodies :as form] env]
  (let [{:keys [async]} (meta form)
        var-name (some-> fn* meta :var-name)
        name (when (symbol? (first bodies)) (first bodies))
        bodies (cond-> bodies name next)
        [body & more-bodies :as bodies] (ensure-bodies bodies)
        fn-type (if (or more-bodies (variadic? body)) :ifn :native)
        env (cond-> env
              name (assoc name (vary-meta (dart-local name env)
                                 #(assoc %
                                    :dart/fn-type   fn-type
                                    :dart/ret-type  (:dart/type %)))))]
    (case fn-type
      :ifn (emit-ifn async var-name name bodies env)
      (emit-dart-fn async name body env))))

(defn emit-letfn [[_ fns & body] env]
  (let [env (reduce (fn [env [name]] (assoc env name (dart-local name env))) env fns)
        fn*s (map #(macroexpand env (with-meta (cons 'cljd.core/fn %) (meta %))) fns)
        ifn? (fn [[_fn* _name & bodies]]
               (let [[body & more-bodies] (ensure-bodies bodies)]
                 (or more-bodies (variadic? body))))
        dart-fns (remove ifn? fn*s)
        ifns (filter ifn? fn*s)
        env (-> env
              (into (for [[_ name] ifns] [name (vary-meta (env name) assoc :dart/fn-type :ifn)]))
              (into (for [[_ name] dart-fns] [name (vary-meta (env name) assoc :dart/fn-type :native)])))
        bindings (into [] (mapcat (fn [[_ name & body :as form]]
                                    ; assumes emit-dart-fn returns a dart/let
                                    (second (emit-dart-fn (-> form meta :async) name (first (ensure-bodies body)) env)))) dart-fns)
        unset-env (into {}
                    (for [[_ name] ifns]
                      [name (vary-meta (env name) assoc :dart/mutable true)]))
        [_ bindings wirings]
        (reduce (fn [[unset-env bindings wirings] [_ name & bodies :as form]]
                  (let [unset-env (dissoc unset-env name)
                        ; assumes emit-ifn returns a dart/let
                        emitted (second (emit-ifn (-> form meta :async) nil name (ensure-bodies bodies)
                                          (into env unset-env)))
                        dart-name (env name)
                        deps (closed-overs emitted unset-env)
                        wirings (assoc wirings dart-name deps)
                        ; assumes emitted is a list of binding whose last binding is [dart-name (reify-ctor-call. ...)]
                        [dart-name ctor-call] (last emitted)
                        bindings (-> bindings
                                   (into (butlast emitted))
                                   (conj [dart-name (map #(if (deps %) nil %) ctor-call)]))]
                    [unset-env bindings wirings]))
          [unset-env bindings {}] ifns)]
    (list 'dart/let
          (vec
            (concat
             (for [[v] bindings] [:late v])
             (for [[v e] bindings] [nil (list 'dart/set! v [nil e])])
             (for [[obj deps] wirings
                   dep deps]
               [nil (list 'dart/set! (list 'dart/.- obj dep) [nil dep])])))
          (emit (list* 'let* [] body) env))))

(defn emit-method [class-name [mname {[this-param & fixed-params] :fixed-params :keys [opt-kind opt-params]} & body] env]
  ;; params destructuring will be added by a macro
  ;; opt-params need to have been fully expanded to a list of [symbol default]
  ;; by the macro
  (let [mtype-params (:type-params (meta mname))
        env (assoc env :type-vars
              (into (:type-vars env #{}) mtype-params))
        dart-fixed-params (map #(dart-local % env) fixed-params)
        dart-opt-params  (for [[p d] opt-params]
                           [(case opt-kind
                              :named (with-meta p (dart-meta p env)) ; here p must be a valid dart identifier
                              :positional (dart-local p env))
                            (emit d env)])
        _ (when (:super (meta this-param))
            (throw (Exception. "DEPRECATED super access has changed: use ^super on this at call sites. For example (.initState ^super self).")))
        env (into (assoc env this-param (with-meta 'this (dart-meta (vary-meta this-param assoc :tag class-name) env)))
                  (zipmap (concat fixed-params (map first opt-params))
                          (concat dart-fixed-params (map first dart-opt-params))))
        dart-body (emit (cons 'do body) env)
        recur-params (when (has-recur? dart-body) dart-fixed-params)
        dart-fixed-params (if recur-params
                            (map #(dart-local % env) fixed-params)
                            dart-fixed-params)
        dart-body (cond->> dart-body
                    recur-params
                    (list 'dart/loop (map vector recur-params dart-fixed-params)))
        mname (cond-> mname
                (has-await? dart-body) (vary-meta assoc :async true))
        mname (with-meta mname (dart-meta mname env))]
    [mname mtype-params dart-fixed-params opt-kind dart-opt-params (nil? (seq body)) dart-body]))

(defn extract-super-calls [dart-body this-super]
  ;; semantics of super are captured by dart closures, thus we use dart closures
  ;; to allow treating super as an almost regular value.
  (let [extract1
        (fn [x]
          (when (= this-super x) (throw (Exception. "Rogue reference to super.")))
          (let [op (when (seq? x) (first x))]
            (case (when (symbol? op) op)
              (dart/. dart/.-)
              (when (= (second x) this-super)
                (let [args (drop (case (first x) dart/. 4 3) x)
                      [bindings args]
                      (reduce (fn [[bindings args] a]
                                (let [[bindings' a'] (lift-arg true a 'super-arg {})]
                                  [(into bindings bindings')
                                   (conj args a')]))
                        [[] []] args)
                      params (vec (into #{} (filter symbol?) args))
                      meth (nth x 2)
                      dart-fn-name (dart-local (with-meta (symbol (str "super-" meth)) {:dart true}) {})
                      dart-fn (list 'dart/fn params :positional () false
                                (list* (first x) 'super meth args))]
                  [dart-fn-name dart-fn
                   (cond->> (cons dart-fn-name params)
                     (seq bindings) (list 'dart/let bindings))]))
              dart/set!
              (let [[_ target [_ dart-expr]] x]
                (when-some [fld (when-some [[op o fld] (when (seq? target) target)]
                                  (when (and (= 'dart/.- op) (= o this-super))
                                    fld))]
                  (let [dart-fn-name (dart-local (with-meta (symbol (str "super-set-" fld)) {:dart true}) {})
                        dart-fn (list 'dart/fn '[v] :positional () false
                                  (list 'dart/let
                                    [[nil (list 'dart/set! ('dart/.- 'super fld) [nil 'v])]]
                                    'v))]
                    [dart-fn-name dart-fn (list dart-fn-name dart-expr)])))
              nil)))
        extract
        (fn extract [form]
          (let [extraction (extract1 form)
                form (if extraction (peek extraction) form)
                extractions (into (if extraction {:extraction extraction} {})
                              (keep-indexed
                                (fn [i form]
                                  (when-some [m (extract form)]
                                    [i m])))
                              (when (sequential? form) form))]
            (when (seq extractions)
              extractions)))
        extraction (extract dart-body)
        apply-extraction
        (fn apply-extraction [form m]
          (if m
            (let [form (or (peek (:extraction m)) form)]
              (if (sequential? form)
                (let [items
                      (into []
                        (map-indexed
                          (fn [i x]
                            (apply-extraction x (m i))))
                        form)]
                  (with-meta (cond-> items (seq? form) sequence) (meta form)))
                form))
            form))]
    [(into [] (keep (comp pop :extraction)) (tree-seq map? #(vals (dissoc % :extraction)) extraction))
     (apply-extraction dart-body extraction)]))

(defn method-extract-super-call [method this-super]
  (let [[bindings dart-body] (extract-super-calls (peek method) this-super)]
    [bindings (conj (pop method) dart-body)]))

(declare write-class)

(defn do-def [nses sym m]
  (let [the-ns *current-ns*
        m (assoc m :ns the-ns :name sym)
        msym (meta sym)]
    (cond
      *host-eval* ; first def during host pass
      (let [{:keys [host-ns]} (nses the-ns)
            {:keys [inline-arities inline macro-host-fn bootstrap-def]} msym
            msym (cond-> msym (or inline-arities macro-host-fn)
                         (into (binding [*ns* host-ns]
                                 (eval {:inline inline
                                        :inline-arities inline-arities
                                        :macro-host-fn macro-host-fn}))))]
        (when bootstrap-def
          (binding [*ns* host-ns]
            (when-not (identical? (-> sym resolve meta :ns) *ns*)
              (ns-unmap *ns* sym)) ; get rid of warnings
            (eval bootstrap-def)))
        (assoc-in nses [the-ns sym] (assoc m :meta (merge msym (:meta m)))))
      *hosted* ; second pass
      (let [old-meta (select-keys (get-in nses [the-ns sym :meta])
                       [:inline :inline-arities :macro-host-fn :bootstrap-def])]
        (assoc-in nses [the-ns sym]
          (assoc m :meta (merge msym (:meta m) old-meta))))
      :else
      (assoc-in nses [the-ns sym] (assoc m :meta (merge msym (:meta m)))))))

(defn alter-def [nses sym f & args]
  (apply update-in nses [*current-ns* sym] f args))

(defn- resolve-method-spec [owner-spec type spec type-env]
  (let [[mname arglist & body] spec
        mname (cond-> mname (string? mname) symbol) ; this will make unreadable symbols for some operators thus beware of weird printouts.
        [mname' arglist']
        (case (:type type)
          :self [mname arglist]
          :protocol (resolve-protocol-method type mname arglist type-env)
          :class
          (let [dart-type (:dart/type type)]
            (or
              (resolve-dart-method dart-type mname arglist type-env)
              (throw (Exception. (str "In class " owner-spec ", can't resolve method " mname (vec arglist) " on class " (:element-name dart-type) " from lib " (:lib dart-type) (source-info)))))))
        mname (vary-meta mname' merge (meta mname))
        parsed-arglist' (parse-dart-params arglist')
        parsed-arglist (parse-dart-params arglist)
        #_{:fixed-params fixed-params
           :opt-kind (case delim .& :named :positional)
           :opt-params
           (for [[p d] (partition-all 2 1 opt-params)
                 :when (symbol? p)]
             [p (when-not (symbol? d) d)])}]
    (list* mname parsed-arglist'
      (when (seq body) ; don't emit a body if no explicit body
        [`(let [; a bunch of rebinding to take into account disagreeing type hints
                ; and/or destructuring
                ; first, fixed args, they may be destrctured
                ~@(mapcat (fn [a a']
                            (if (symbol? a)
                              (when-some [t (:tag (meta a))]
                                (when-not (= t (:tag (meta a')))
                                  [a a']))
                              ; destructuring case
                              [a a']))
                    (:fixed-params parsed-arglist)
                    (:fixed-params parsed-arglist'))
                ; second, optionals which can't be destructured because it would be syntactically
                ; ambiguous, so only checking for types disagreement
                ~@(case (:opt-kind parsed-arglist')
                    :named
                    ; named may appear out of order
                    (let [names' (into #{} (map first) (:opt-params parsed-arglist'))]
                      (mapcat (fn [[a]]
                                (when-some [t (:tag (meta a))]
                                  (let [a' (names' a)]
                                    (when-not (= t (:tag (meta a')))
                                      [a a']))))
                        (:opt-params parsed-arglist)))
                    :positional
                    (mapcat (fn [[a] [a']]
                              (when-some [t (:tag (meta a))]
                                (when-not (= t (:tag (meta a')))
                                  [a a'])))
                      (:opt-params parsed-arglist)
                      (:opt-params parsed-arglist')))]
            ~@body)]))))

(defn resolve-conflicting-meths [[ma :as a] [mb :as b]]
  (let [oa (-> ma meta :override boolean)
        ob (-> mb meta :override boolean)]
    (cond
      (= oa ob) (throw (Exception. (str "Conflicting method implementations for " ma ".")))
      oa a
      :else b)))

(defn- resolve-methods-specs [class-name specs type-env]
  (loop [specs-order [] resolved-specs {} specs specs]
    (if-some [[spec & more-specs :as specs] (seq specs)]
      (if (nil? spec) ; cgrand: I don't know why we have a leading nil
        (recur specs-order resolved-specs more-specs)
        (let [_ (when-not (symbol? spec)
                  (throw (Exception. (str "Encountered " (pr-str spec) " while expecting a symbol denoting a type or a protocol. " (source-info)))))
              [type spec] (cond
                            (= class-name spec) [{:type :self} spec]
                            (:mixin (meta spec)) [{:type :class
                                                   :dart/type (resolve-type spec type-env)}
                                                  spec]
                            :else
                            (let [[tag x] (resolve-non-local-symbol spec type-env)]
                              (case tag
                                :def [x (case (:type x)
                                          :class spec
                                          :protocol (symbol (name (:ns x)) (name (:iface x))))]
                                :dart [{:type :class :dart/type x} spec]
                                (throw (Exception. (str "Can't resolve " spec (source-info)))))))
              [meths more-specs] (split-with seq? more-specs)
              meths (into {} (comp (map #(resolve-method-spec spec type % type-env)) (map (fn [[mname :as meth]] [mname meth])))meths)]
          (recur
            (cond-> specs-order (not (resolved-specs spec)) (conj spec))
            (update resolved-specs spec #(merge-with resolve-conflicting-meths % meths))
            more-specs)))
      (mapcat (fn [spec] (cons spec (vals (resolved-specs spec)))) specs-order))))

(defn- parse-class-specs [class-name opts specs env]
  (let [{:keys [extends]} opts
        [ctor-op base & ctor-args :as ctor]
        (macroexpand env
          (cond
            (symbol? extends) (list 'new extends)
            (seq? extends) extends
            (nil? extends) '(new dart:core/Object)
            :else (throw (Exception. (str "Unexpected super constructor")))))
        specs (resolve-methods-specs class-name (cons (if extends base class-name) specs)
                (:type-vars env #{}))
        ctor-meth (when (= '. ctor-op) (first ctor-args))
        ctor-args (cond-> ctor-args (= '. ctor-op) next)
        methods (into [] (filter seq?) specs)  ; crude
        base-type (emit-type base env)
        classes (sequence
                  (comp (filter symbol?) (remove #{class-name})
                    (map (fn [x] [(-> x meta :mixin) (emit-type x env)]))
                    (remove #(= (:canon-qname (second %)) (:canon-qname base-type))))
                  specs) ; crude
        ifaces (into [] (comp (remove first) (map second)) classes)
        mixins (into [] (comp (filter first) (map second)) classes)
        need-nsm (and (seq ifaces) (not-any? (fn [[m]] (case m noSuchMethod true nil)) methods))]
    {:extends base-type
     :implements ifaces
     :with mixins
     :super-ctor
     {:method ctor-meth ; nil for new
      :args ctor-args}
     :methods methods
     :nsm need-nsm}))

(defn- emit-class-specs [class-name parsed-class-specs env]
  (update parsed-class-specs :methods (fn [methods] (map #(emit-method class-name % env) methods))))

(defn method-closed-overs [[mname type-params dart-fixed-params opt-kind dart-opt-params _ dart-body] env]
  (transduce (filter symbol?) disj (closed-overs dart-body env) (list* 'this 'super (concat dart-fixed-params (map second dart-opt-params)))))

(defn- new-dart-type
  ([mclass-name clj-type-params dart-fields env]
   (new-dart-type mclass-name clj-type-params dart-fields nil env))
  ([mclass-name clj-type-params dart-fields parsed-class-specs env]
   (let [all-nses @nses
         ctor-params (into []
                       (map (fn [field]
                              {:name field
                               :kind :positional
                               :type (or (:dart/type (meta field)) dc-dynamic)}))
                       dart-fields)
         qname (symbol (str (dart-alias-for-ns *current-ns*) "." mclass-name))
         lib (-> *current-ns* all-nses :lib)
         bare-type {:kind :class
                    :qname qname
                    :canon-qname qname
                    :lib lib
                    :canon-lib lib
                    :element-name (name mclass-name)
                    :type-parameters (into [] (map #(emit-type % env)) clj-type-params)}
         members
         (-> {}
           (into
             (map #(vector (name (:name %)) {:kind :field
                                             :getter true
                                             :type (:type %)}))
             ctor-params)
           (assoc (name mclass-name) {:kind :constructor
                                      :return-type bare-type
                                      :parameters ctor-params}
             :super (:extends parsed-class-specs)
             :kaboom (reify Object (hashCode [_] (/ 0)) (toString [_] "💥"))
             :interfaces (:implements parsed-class-specs)
             :mixins (:with parsed-class-specs))
           (into
             (map (fn [[mname {:keys [fixed-params opt-kind opt-params]} body]]
                    (let [{:keys [type-params getter setter]} (meta mname)
                          env (update env :type-vars (fnil into #{}) type-params)]
                      [(name mname)
                       (if (or getter setter)
                         {:kind :field
                          :getter getter
                          :setter setter
                          :type (:dart/type (dart-meta mname env) dc-dynamic)}
                         {:kind :method
                          :return-type (:dart/type (dart-meta mname env) dc-dynamic)
                          :type-parameters (map #(emit-type % env) type-params)
                          :parameters (concat
                                        (map
                                          (fn [p]
                                            {:name p
                                             :kind :positional
                                             :type (:dart/type (dart-meta p env) dc-dynamic)})
                                          (next fixed-params)) ; get rid of this
                                        (map
                                          (fn [[p]]
                                            {:name p
                                             :kind opt-kind
                                             :optional true
                                             :type (:dart/type (dart-meta p env) dc-dynamic)})
                                          opt-params))})])))
             (:methods parsed-class-specs)))]
     (assoc bare-type :members members))))

(defn emit-reify* [[_ opts & specs] env]
  (let [[outer-clj-this outer-dart-this] (some (fn [[clj dart :as e]] (when (= 'this dart) e)) env)
        that-super (dart-local "that-super" env)
        that-this (vary-meta (dart-local "that" env)
                             merge (meta outer-dart-this) {:that-super that-super})
        ;; when we have nested closures we must take care to not take a this relating
        ;; to the outermost clojure for a this relating to the innermost.
        ;; That's why we remap existing bindings to this and super.
        ;; Thus in the emitted code all references to this and super will relate to their
        ;; innermost closure.
        ;; And by checking the presence of that-this and that-super in the emitetd codeper
        ;; we know whether we need to close over the outermost values of this and super.
        env (cond-> env outer-clj-this (assoc outer-clj-this that-this))
        parsed-class-specs (parse-class-specs nil opts specs env)
        fingerprint (Integer/toUnsignedString
                     (hash [(:canon-qname (:extends parsed-class-specs))
                            (into #{} (map :canon-qname) (:implements parsed-class-specs))
                            (into #{} (map :canon-qname) (:mixins parsed-class-specs))]) 36)
        class-name (anonymous-global
                    (munge-str
                     (str (if-some [var-name (:var-name opts)] "ifn" (or (:name-hint opts) "Reify"))
                          fingerprint)))
        mclass-name (vary-meta class-name assoc :type-params (:type-vars env))
        dart-type (new-dart-type mclass-name (:type-vars env) [] parsed-class-specs env)
        ;; it's ok to predecl the class without fields because in a closure you don't have direct access to them nor to the constructor.
        _ (swap! nses do-def class-name {:dart/name mclass-name :dart/type dart-type :type :class})
        class (emit-class-specs mclass-name parsed-class-specs env)
        ; extract references to parent's super in dart closures
        [super-fn-bindings methods]
        (reduce (fn [[bindings meths] meth]
                  (let [[bindings' meth'] (method-extract-super-call meth that-super)]
                    [(into bindings bindings') (conj meths meth')]))
                [[] []] (:methods class))
        ; closed-overs are a seq of dart locals
        ; (they are found by walking the emitted dart sexp)
        ; super-fns extracted above are part of the closed overs
        closed-overs (concat
                      (transduce (map #(method-closed-overs % env)) into #{}
                                 methods)
                      (map first super-fn-bindings))
        super-ctor-split-args+types
        (when-some [dart-super-type (:extends class)]
          (let [meth (-> class :super-ctor :method)
                super-ctor-meth (cond-> (:element-name dart-super-type) meth (str "." meth))
                super-ctor-info (some-> (dart-member-lookup dart-super-type super-ctor-meth env) actual-member)]
            (when-not (= :constructor (:kind super-ctor-info))
              (throw (Exception. (str "Can't resolve constructor " super-ctor-meth " for type " (:element-name dart-super-type "dynamic") " of library " (:lib dart-super-type "dart:core") " " (source-info)))))
            (-> class :super-ctor :args (split-args (some-> super-ctor-info dart-method-sig) env))))
        super-ctor-split-params
        (into [] (map (fn [[name _]] [name (dart-local (or name "param") env)])) super-ctor-split-args+types)
        ;; when there are arguments to the super ctor, these arguments are lost
        ;; upon invocation and thus it's not possible to rebuild the object
        ;; thus it's impossible to implement with-meta which would require storing
        ;; suoer-ctor args.
        ;; Thus when you have super-ctor args you can't have meta support by default.
        ;; Note: one can also opt out of default meta by using the option :no-meta true
        no-meta (or (:no-meta opts) (seq super-ctor-split-args+types))
        meta-field (when-not no-meta (dart-local 'meta env))
        [parsed-class-meta-specs {meta-methods :methods meta-implements :implements}]
        (when meta-field
          (let [env-for-ctor-call ; hack where dart syms become clj syms
                (-> env
                    (into (map (fn [v] [v v])) closed-overs)
                    (assoc meta-field meta-field))
                parsed-class-meta-specs
                (parse-class-specs nil {} ; TODO
                                   `(cljd.core/IMeta
                                     (~'-meta [_#] ~meta-field)
                                     cljd.core/IWithMeta
                                     (~'-with-meta [_# m#] (new ~mclass-name m# ~@closed-overs)))
                                   env-for-ctor-call)]
            [parsed-class-meta-specs (emit-class-specs 'dart:core/Object parsed-class-meta-specs env-for-ctor-call)]))
        all-fields (cond->> closed-overs meta-field (cons meta-field))
        dart-type (if meta-field
                    (let [meta-members (:members (new-dart-type mclass-name (:type-vars env) all-fields parsed-class-meta-specs env))]
                      (-> dart-type
                        ; no mixins, no super, only interfaces
                        (update-in [:members :interfaces] (fnil into []) (:interfaces meta-members))
                        (update :members into (filter (fn [[k]] (string? k))) meta-members)))
                    dart-type)
        _ (swap! nses alter-def class-name assoc :dart/type dart-type)
        class (-> class
                  (assoc
                   :name (emit-type mclass-name env)
                   :ctor class-name
                   :fields all-fields
                   :methods (concat meta-methods methods)
                   :implements (concat meta-implements (:implements class))
                   :ctor-params
                   (concat
                    (map #(list '. %) all-fields)
                    (map second super-ctor-split-params)))
                  (assoc-in [:super-ctor :args]
                            (into [] (comp cat (remove nil?)) super-ctor-split-params)))]
    (swap! nses alter-def class-name assoc :dart/code (with-dart-str (write-class class)))
    (let [ctor-split-args (map #(assoc % 0 nil) super-ctor-split-args+types)
          [bindings dart-args] (lift-args ctor-split-args env)
          bindings (concat super-fn-bindings bindings)]
      (cond->>
       (with-meta (list* 'dart/new (emit-type class-name env)
                         (concat (when meta-field [nil])
                                 (map #(if (= that-this %) outer-dart-this %) closed-overs) dart-args))
         {:dart/type dart-type
          :dart/inferred true})
        (seq bindings)
        (list 'dart/let bindings)))))

(defn- ensure-dart-expr
  "If dart-expr is suitable as an expression (ie liftable returns nil),
   its emission is returned as is, otherwise a IIFE (thunk invocation) is returned."
  [dart-expr env]
  (if-some [[bindings dart-expr] (liftable dart-expr env)]
    (list (list 'dart/fn () :positional () false (list 'dart/let bindings dart-expr)))
    dart-expr))

(declare write-top-dartfn write-top-field write-dynamic-var-top-field write-annotations)

(defn dart-qualify [dartname]
  (with-meta (symbol (str (dart-alias-for-ns *current-ns*) "." dartname))
    (meta dartname)))

(defn emit-defprotocol* [[_ pname spec] env]
  (let [dartname (munge pname env)]
    (swap! nses do-def pname
           (assoc spec
                  :dart/name dartname
                  :dart/qname (dart-qualify dartname)
                  :dart/code (with-dart-str (write-top-field dartname (emit (list 'new (:impl spec)) {})))
                  :type :protocol))))

(defn emit-deftype* [[_ class-name fields opts & specs] env]
  (assert (and (re-matches #"[_$a-zA-Z][_$a-zA-Z0-9]*" (name class-name))
            (not (reserved-words (name class-name))))
    (str "class-names must be valid dart ids; infringing class name: " (name class-name)))
  (let [abstract (:abstract (meta class-name))
        mixin (:mixin (meta class-name))
        [class-name & type-params] (cons class-name (:type-params (meta class-name)))
        mclass-name (with-meta
                      (or (:dart/name (meta class-name)) class-name)
                      {:type-params type-params}) ; TODO shouldn't it be dne by munge?
        env {:type-vars (set type-params)}
        env (into env
                  (for [f fields
                        :let [{:keys [mutable]} (meta f)
                              {:keys [dart/type] :as m} (dart-meta f env)
                              m (cond-> m
                                  mutable (assoc :dart/mutable true)
                                  (or mixin abstract) (assoc :dart/late true))]]
                    [f (vary-meta (munge f env) merge m)]))
        dart-fields (map env fields)
        parsed-class-specs (parse-class-specs class-name opts specs env)
        _ (when-not *hosted*
            ;; necessary when a method returns a new instance of its parent class
            (swap! nses do-def class-name {:dart/name mclass-name
                                           :dart/type (new-dart-type mclass-name type-params dart-fields env)
                                           :type :class}))
        dart-type (new-dart-type mclass-name type-params dart-fields parsed-class-specs env)
        _ (swap! nses do-def class-name {:dart/name mclass-name :dart/type dart-type :type :class})]
    (binding [*class-prefix* (name mclass-name)]
      (let [class (emit-class-specs class-name parsed-class-specs env)
            dart-super-type (:extends class) ; should never
            meth (-> class :super-ctor :method)
            super-ctor-meth (cond-> (:element-name dart-super-type) meth (str "." meth))
            super-ctor-info (some-> (dart-member-lookup dart-super-type super-ctor-meth env) actual-member)
            _ (when (not super-ctor-info)
                (binding [*out* *err*]
                  (println "DYNAMIC WARNING: can't resolve constructor " super-ctor-meth "for type" (:element-name dart-super-type "dynamic") "of library" (:lib dart-super-type "dart:core") (source-info))))
            super-ctor-split-args+types
            (-> class :super-ctor :args (split-args (some-> super-ctor-info dart-method-sig) env))

        const (and (:const super-ctor-info) (not-any? #(:dart/mutable (meta %)) dart-fields)) ; TODO it's weak we should test super args for const while assuming params to be const
        dart-type (cond-> dart-type const (assoc-in [:members (name mclass-name) :const] true))
        class (-> class
                  (assoc :name dart-type
                         :ctor mclass-name
                         :abstract abstract
                         :mixin mixin
                         :fields dart-fields
                         :ctor-params (map #(list '. %) dart-fields))
                  (assoc-in [:super-ctor :args]
                            (into []
                                  (mapcat
                                   (fn [[name arg]]
                                     (if (nil? name)
                                       (-> arg (ensure-dart-expr env) list)
                                       [name (-> arg (ensure-dart-expr env))])))
                                  super-ctor-split-args+types)))]
    (swap! nses alter-def class-name assoc :dart/code (with-dart-str (write-class class)) :dart/type dart-type)
    (emit class-name env)))))

(defn relocatable-class-name
  "Returns a clojure (as in: usable in clojure code, not dart-sexp) symbol which works in
   any namespace, irrespective of the requires/imports"
  [class-name env]
  (case class-name
    fallback class-name
    (let [dart-type (resolve-type class-name #{})
          relocatable-type
          (binding [*current-ns* nil]
            ; no ns to force global aliases
            (unresolve-type dart-type))]
      (assert (qualified-symbol? relocatable-type) (str "oh noes " relocatable-type (resolve-type class-name #{})))
      relocatable-type)))

(defn ensure-import-ns [the-ns]
  (let [all-nses @nses
        the-lib (:lib (all-nses the-ns))
        dart-alias (some-> all-nses :libs (get the-lib) :dart-alias)]
    (when-not dart-alias
      (throw (ex-info (str "Namespace not required: " the-ns) {:ns the-ns})))
    (when-not (some-> *current-ns* all-nses :imports (get the-lib))
      (swap! nses assoc-in [*current-ns* :imports the-lib] {}))
    dart-alias))

(defn ensure-contrib [to-ns from-ns path]
  (swap! nses
    #(let [contrib-lib (:lib (% from-ns))]
       (-> %
         (assoc-in [to-ns :contribs contrib-lib] contrib-lib)
         (assoc-in [from-ns :contributions-paths path] path)))))

(defn emit-extend-type-protocol* [[_ class-name protocol-ns protocol-name extension-instance] env]
  (let [contrib-ns (-> extension-instance namespace symbol)
        class-name (relocatable-class-name class-name env)
        contrib-path [protocol-ns protocol-name :extensions class-name]
        {{proto-map protocol-name} protocol-ns}
        (swap! nses assoc-in contrib-path extension-instance)]
    (ensure-contrib protocol-ns contrib-ns contrib-path)
    (binding [*current-ns* protocol-ns]
      (emit (expand-protocol-impl proto-map) env))))

(defn- fn-kind [x]
  (when (and (seq? x) (= 'fn* (first x)))
    (let [[maybe-name :as body] (next x)
          [arglist-or-body :as body] (cond-> body (symbol? maybe-name) next)
          [[args] & more-bodies] (cond-> body (vector? arglist-or-body) list)]
      (if (or more-bodies (some '#{&} args)) :clj :dart))))

(defn- dart-fn-parameters [fn-expr]
  ;; FIXME doesn't take optionals into account
  (let [[_ name-or-args-or-body args-or-body] fn-expr
        args-or-body (if (symbol? name-or-args-or-body) args-or-body name-or-args-or-body)
        args (if (vector? args-or-body) args-or-body (first args-or-body))]
    (into [] (repeat (count args) {:kind :positional
                                   :type dc-dynamic}))))

(def ^:dynamic *recompile-count*)

(defn tack-version
  "When recompiling, adds a revision count to the name. Metadata is preserved."
  [dartqname]
  (if-not (bound? #'*recompile-count*)
    dartqname
    (-> (str dartqname "$v" *recompile-count*) symbol (with-meta (meta dartqname)))))

(defn emit-def [[_ sym & doc-string?+expr :as form] env]
  (let [[doc-string expr]
        (case (count doc-string?+expr)
          0 nil
          1 (cons (:doc (meta sym)) doc-string?+expr)
          2 (if (string? (first doc-string?+expr))
              doc-string?+expr
              (throw (ex-info "doc-string must be a string" {})))
          (throw (ex-info "Too many arguments to def" {})))
        sym (vary-meta sym assoc :doc doc-string)
        expr (macroexpand env expr)
        kind (case (:dart/fn-type (dart-meta sym env))
               :native :dart
               :ifn :clj
               (fn-kind expr))
        sym (cond-> sym
              kind (vary-meta assoc kind true)
              (:macro (meta sym)) (vary-meta assoc :macro-host-fn expr)
              (:macro-support (meta sym)) (vary-meta assoc :bootstrap-def (list 'def sym expr)))
        dartname (munge sym env)
        ret-type (:dart/type (meta dartname))
        dart-type (case kind
                    :dart
                    (cond-> dc-Function
                      ret-type (assoc
                                :return-type ret-type
                                :parameters (dart-fn-parameters expr)))
                    :clj (emit-type 'cljd.core/IFn$iface {})
                    nil)
        dartname (cond-> dartname
                   dart-type
                   (vary-meta (fn [{:dart/keys [type] :as m}]
                                (assoc m :dart/ret-type type
                                       :dart/type dart-type))))
        expr (when-not *host-eval*
               (if (and (seq? expr) (= 'fn* (first expr)))
                 (with-meta (cons (vary-meta (first expr) assoc :var-name sym) (next expr)) (meta expr))
                 expr))
        sub-type (cond
                   (:dynamic (meta sym)) :dynamic ; dynamic as in dynamic var, not dc.dynamic
                   (:no-reload (meta sym)) :defonce
                   dart-type :fn
                   :else :field)
        dartqname (let [dartqname (dart-qualify dartname)]
                    (case sub-type
                      :field (tack-version dartqname) ; only fields need to be versioned
                      dartqname))
        dart-fn
        (binding [*class-prefix* (str *class-prefix* dartname)]
          ; predecl so that the def is visible in recursive defs
          (swap! nses do-def sym {:dart/qname dartqname
                                  :dart/name dartname
                                  :type :field
                                  :dart/type dart-type})
          (emit expr env))
        dart-annotations
        (when-not *host-eval* (into [] (map #(emit % env)) (-> form meta :annotations)))
        dart-code
        (when-not *host-eval*
          (with-dart-str
            (write-annotations dart-annotations)
            (cond
              (:dynamic (meta sym))
              (let [k (symbol (name *current-ns*) (name sym))]
                (write-dynamic-var-top-field k dartname dart-fn))
              (and (seq? expr) (= 'fn* (first expr)) (not (symbol? (second expr))))
              (write-top-dartfn dartname
                                (or
                  ; peephole optimization: unwrap single let
                                 (and (seq? dart-fn) (= 'dart/let (first dart-fn))
                                      (let [[_ [[x e] & more-bindings] x'] dart-fn]
                                        (and (nil? more-bindings) (= x x') e)))
                                 (ensure-dart-expr dart-fn env)))
              :else
              (write-top-field dartqname dart-fn))))]
    (swap! nses alter-def sym assoc :dart/code dart-code)
    (emit sym env)))

(defn emit-symbol [x env]
  (let [[tag v] (resolve-symbol x env)]
    (case tag
      :local v
      :def
      (if-some [t (when (= :class (:type v)) (:dart/type v))]
        (list 'dart/type t)
        (let [{dartqname :dart/qname} v]
          dartqname))
      :dart (case (:kind v)
              :function
              ; this looks a LOT like specialize-function and dart-fn-lookup (has it ever been used?)
              (let [function-type (into dc-Function (select-keys v [:parameters :return-type :type-parameters]))
                    type-params (:type-parameters function-type)
                    ; type-parameters as returned by resolve-symbol are only names as strings
                    ; bounds are lost, so no check can be completed TOFIX
                    type-args (or (some->> (:type-params (meta x)) (map #(emit-type % env)))
                                (repeat (count type-params) dc-dynamic)) ; TODO correct bound
                    _ (when-not (= (count type-params) (count type-args))
                        (throw (ex-info (str "Function " v " expects " (count type-params) " but got " (count type-args) ": " (vec (:type-params (meta x))))
                                 {:symbol x :dart-info v})))
                    actual-function-type
                    (actual-type function-type (zipmap type-params type-args))] ; TODO add parameters to type

                (with-meta (:qname v) {:dart/type (or (some-> (meta x) :tag emit-type) actual-function-type) ; Why this or ?
                                       :dart/fn-type :native
                                       :dart/signature actual-function-type}))
              :class (with-meta (:qname v) {:dart/class v})
              :field (with-meta (:qname v) {:dart/type (:type v)}))
      (throw (Exception. (str "Unknown symbol: " x (source-info)))))))

(defn emit-var [[_ s] env]
  (let [[tag info] (resolve-symbol s {})]
    (case tag
      :def
      (emit (list 'quote (symbol (name (:ns info)) (name (:name info)))) {})
      (throw (Exception. (str "Not a var: " s (source-info)))))))

(defn emit-quoted [x env]
  (cond
    (coll? x) (emit-coll emit-quoted x env)
    (symbol? x) (emit (list 'cljd.core/symbol (namespace x) (name x)) env)
    :else (emit x env)))

(defn emit-quote [[_ x] env]
  (emit-quoted x env))

(defn ns-to-lib [ns-name]
  (str *lib-path* *target-subdir* (replace-all (name ns-name) #"[.]" {"." "/"}) ".dart"))

(declare compile-namespace)

(defn- import-to-require [spec]
  (cond
    (symbol? spec) (let [[_ ns id] (re-matches (name spec) #"(.+)\.(.+)")]
                     [(symbol ns) :refer [(symbol id)]])
    (sequential? spec) [(first spec) :refer (rest spec)]
    :else (throw (ex-info (str "Unsupported import spec: "
                               (pr-str spec)) {:spec spec}))))

(defn- use-to-require [spec]
  (or
    (when (sequential? spec)
      (let [lib (first spec)
            {:keys [only rename]} (apply hash-map (rest spec))]
        (when (or only rename)
          [lib :refer only :rename rename])))
    (throw (ex-info "use must have either a :rename or an :only option" {:spec spec}))))

(defn- refer-clojure-to-require [refer-spec]
  (let [{:keys [exclude only rename]} (reduce (fn [m [k v]] (merge-with into m {k v})) {} (partition 2 (next refer-spec)))
        exclude (into (set exclude) (keys rename))
        include (if only (set only) any?)
        refer (for [{:keys [type name] {:keys [private]} :meta} (vals (@nses 'cljd.core))
                    :when (and (= :field type) (not private)
                            (include name) (not (exclude name)))]
                name)]
    ['cljd.core
     :refer refer
     :rename (or rename {})]))

(defn emit-ns [[_ ns-sym & ns-clauses :as ns-form] _]
  (when (or (not *hosted*) *host-eval*)
    (let [ns-clauses (drop-while #(or (string? %) (map? %)) ns-clauses) ; drop doc and meta for now
          refer-clojures (if (= 'cljd.core ns-sym)
                           [[:refer-clojure :only []]]
                           (or (seq (filter #(= :refer-clojure (first %)) ns-clauses)) [[:refer-clojure]]))
          host-ns-directives (some #(when (= :host-ns (first %)) (next %)) ns-clauses)
          require-specs
          (->>
           (concat
            (map refer-clojure-to-require refer-clojures)
            (for [[directive & specs :as clause] ns-clauses
                  :when clause ; allow nils produced by conditionals
                  :let [f (case directive
                            :require #(if (sequential? %) % [%])
                            :import import-to-require
                            :use use-to-require
                            (:refer-clojure :host-ns) nil)]
                  :when f
                  spec specs
                  ; allow nils produced by conditionals
                  :when spec]
              (f spec)))
           (map (fn [[lib & more :as spec]]
                  (or (some-> (cljdize lib) (cons more))
                      spec))))
          ns-lib (ns-to-lib ns-sym)
          ns-map (-> ns-prototype
                     (assoc :lib ns-lib)
                     (assoc-in [:imports ns-lib] {:clj-alias (name ns-sym)}))
          ns-map
          (reduce #(%2 %1) ns-map
                  (for [[lib & {:keys [as refer rename]}] require-specs
                        :let [_ (when (and (string? lib) (nil? (analyzer-info lib)))
                                  (throw (Exception. (str "Can't find Dart lib: " lib))))
                              _ (when-not (or (nil? refer) (and (coll? refer) (every? symbol? refer)))
                                  (throw (ex-info ":refer expects a collection of symbols; :refer :all is not supported." {:refer refer})))
                              clj-ns (when-not (string? lib) lib)
                              dartlib (else->>
                                       (if (string? lib) lib)
                                       (if-some [{:keys [lib]} (@nses lib)] lib)
                                       (if (= ns-sym lib) ns-lib)
                                       (compile-namespace lib))
                              dart-alias (global-lib-alias dartlib clj-ns)
                              clj-alias (name (or as clj-ns (str "lib:" dart-alias)))
                              to-dart-sym (if clj-ns #(munge % {}) identity)]]
                    (fn [ns-map]
                      (-> ns-map
                          (cond-> (nil? (get (:imports ns-map) dartlib))
                            (assoc-in [:imports dartlib] {:clj-alias clj-alias}))
                          (assoc-in [:clj-aliases clj-alias] dartlib)
                          (update :mappings into
                                  (let [source (if (string? lib)
                                                 #(analyzer-info lib (name %))
                                                 (@nses lib))]
                                    (for [to refer :let [from (get rename to to)]]
                                      (if-not (source to)
                                        (throw (Exception. (str "Can't find " to " in " lib)))
                                        [from (symbol clj-alias (name to))]))))))))
          host-aliases (into #{}
                             (for [[op & more] host-ns-directives
                                   :when (= :require op)
                                   lib more
                                   :when (sequential? lib)
                                   :let [[_ & {:keys [as]}] lib]
                                   :when as]
                               as))
          host-ns-directives
          (concat
           host-ns-directives
           (for [[lib & {:keys [refer as] :as options}] require-specs
                 :when (not (host-aliases as))
                 :let [host-ns (some-> (get @nses lib) :host-ns ns-name)]
                 :when (and host-ns (not= ns-sym lib))
                 :let [options
                       (assoc options :refer (filter (comp :macro-support :meta (@nses 'cljd.core)) refer))]]
             (list :require (into [host-ns] cat options))))
          ns-map (cond-> ns-map
                   *host-eval* (assoc :host-ns (create-host-ns ns-sym host-ns-directives)))]
      (global-lib-alias ns-lib ns-sym)
      (set! *current-ns* ns-sym)
      (swap! nses assoc ns-sym ns-map))))

(defn- emit-no-recur [expr env]
  (let [dart-expr (emit expr env)]
    (when (has-recur? dart-expr)
      (throw (ex-info "Cannot recur across try." {:expr expr})))
    dart-expr))

(defn emit-try [[_ & body] env]
  (let [{body nil catches 'catch [[_ & finally-body] :as finallies] 'finally}
        (group-by #(when (seq? %) (#{'finally 'catch} (first %))) body)
        dart-expr (emit-no-recur (cons 'do body) env)]
    (if (or (seq catches) (seq finallies))
      (list 'dart/try
        dart-expr
        (for [[_ classname e & [maybe-st & exprs :as body]] catches
              :let [st (when (and exprs (symbol? maybe-st)) maybe-st)
                    exprs (if st exprs body)
                    env (cond-> (assoc env e (dart-local e env))
                          st (assoc st (dart-local st env)))]]
          [(emit-type classname env) (env e) (some-> st env) (emit-no-recur (cons 'do exprs) env)])
        (some-> finally-body (conj 'do) (emit-no-recur env)))
      dart-expr)))

(defn emit-throw [[_ expr] env]
  ;; always emit throw as a statement (in case it gets promoted to rethrow)
  (with-meta (list 'dart/let [[nil (list 'dart/throw (emit expr env))]] nil)
    {:dart/type     dc-Never
     :dart/inferred true}))

(defn emit-dart-is [[_ x type] env]
  #_(when (or (not (symbol? type)) (env type))
      (throw (ex-info (str "The second argument to dart/is? must be a literal type. Got: " (pr-str type)) {:element-name type})))
  (let [dart-x (emit x env)
        dart-type (emit-type type env)]
    (if (is-assignable? dart-type (:dart/type (infer-type dart-x)))
      true ; TODO should fix aggressive optim? if the expression is side-effecting, it's removal is not a no-op
      (with-lifted [x dart-x] env
        (with-meta (list 'dart/is x dart-type)
          {:dart/type     dc-bool
           :dart/inferred true})))))

(defn emit-dart-assert [[_ test msg] env]
  (list 'dart/assert (ensure-dart-expr (emit test env) env)
    (ensure-dart-expr (emit msg env) env)))

(defn emit-dart-await [[_ x] env]
  (with-lifted [x (emit x env)] env
    (with-meta (list 'dart/await x)
      (let [ret-type (:dart/type (infer-type x))]
        {:dart/type (no-future ret-type)
         :dart/inferred true}))))

(defn emit-dart-async-barrier [form env]
  (let [dart-expr (emit-do form env)]
    (if (has-await? dart-expr)
      (list (list 'dart/fn () :positional () true dart-expr))
      dart-expr)))

(defn emit-dart-run-zoned [[_ & opts+body] env]
  (let [opts (take-while (fn [[k v]] (keyword? k)) (partition 2 (butlast opts+body)))
        body (drop (* 2 (count opts)) opts+body)
        {:keys [values spec]} (into {} (map vec) opts)
        dart-expr (emit-no-recur (cons 'do body) env)
        async (has-await? dart-expr)
        thunk (list 'dart/fn () :positional () async dart-expr)
        thunksym (gensym "dart-run-zoned-thunk")]
    (emit
      (cond->>
          (list '$lib:da/runZoned
            thunksym '.zoneValues values '.zoneSpecification spec)
        async
        (list 'await))
      (assoc env thunksym thunk))))

(defn emit-dart-type-like [[_ x like] env]
  (simple-cast (emit x env) (:dart/type (infer-type (emit like env)))))

(defn emit
  "Takes a clojure form and a lexical environment and returns a dartsexp."
  [x env]
  (try
    (let [x (macroexpand-and-inline env x) ;; meta dc-nim
          dart-x
          (cond
            (symbol? x) (emit-symbol x env)
            #?@(:clj [(char? x) (str x)])
            (or (number? x) (boolean? x) (string? x)) x
            (instance? java.util.Date x)
            (let [[_ s] (re-matches #"#inst *\"(.*)\"" (pr-str x))]
              (emit (list 'dart:core/DateTime.parse s) env))
            (instance? java.util.regex.Pattern x)
            (emit (list 'new 'dart:core/RegExp
                        #_(list '. 'dart:core/RegExp 'escape (.pattern ^java.util.regex.Pattern x))
                        (.pattern ^java.util.regex.Pattern x)
                        #_#_#_'.& :unicode true) env)
            (keyword? x)
            (emit (list 'cljd.core/Keyword. (namespace x) (name x) (cljd-hash x)) env)
            (nil? x) nil
            (uuid? x)
            (emit (list 'cljd.core/UUID. (str x) (cljd-hash (str x))) env)
            (and (seq? x) (seq x)) ; non-empty seqs only
            (let [f (first x)
                  emit (if (symbol? f)
                         (case f
                           . emit-dot ;; local inference done
                           set! emit-set!
                           dart/is? emit-dart-is ;; local inference done
                           dart/await emit-dart-await
                           dart/async-barrier emit-dart-async-barrier
                           dart/run-zoned emit-dart-run-zoned
                           dart/assert emit-dart-assert
                           dart/type-like emit-dart-type-like
                           throw emit-throw
                           new emit-new ;; local inference done
                           ns emit-ns
                           try emit-try
                           case* emit-case*
                           quote emit-quote
                           do emit-do
                           var emit-var
                           let* emit-let*
                           loop* emit-loop*
                           recur emit-recur
                           if emit-if
                           fn* emit-fn*
                           letfn emit-letfn
                           def emit-def
                           reify* emit-reify*
                           deftype* emit-deftype*
                           defprotocol* emit-defprotocol*
                           extend-type-protocol* emit-extend-type-protocol*
                           emit-fn-call)
                         emit-fn-call)]
              (binding [*source-info* (let [{:keys [line column]} (meta x)]
                                        (if line
                                          {:line line :column column}
                                          *source-info*))]
                (emit x env)))
            (and (tagged-literal? x) (= 'dart (:tag x))) (emit-dart-literal (:form x) env)
            (coll? x) (emit-coll x env)
            :else (throw (ex-info (str "Can't compile " (pr-str x)) {:form x})))
          {:dart/keys [const type]} (dart-meta x env)
          inferred-const (:dart/const (meta dart-x))]
      (when (and const (not inferred-const))
        (println "🤔 Unexpected ^:const, if it passes Dart compilation, please open a ClojureDart issue" (pr-str x) (source-info)))
      (when (and (false? const) (not inferred-const))
        (println "🤔 Useless ^:unique. If you disagree, please open a ClojureDart issue" (source-info)))
      (when (and const inferred-const)
        (println "INFO: Useless ^:const, please remove" (source-info)))
      (cond-> dart-x
        (some? const) (vary-meta assoc :dart/const const)
        type (simple-cast type)))
    (catch Exception e
      (throw
       (if-some [stack (::emit-stack (ex-data e))]
         (ex-info (ex-message e) (assoc (ex-data e) ::emit-stack (conj stack x)) (ex-cause e))
         (ex-info (str "Error while compiling " (pr-str x)) {::emit-stack [x]} e))))))

(defn host-eval
  [x]
  (binding [*host-eval* true]
    (try
      (let [x (macroexpand {} x)]
        (when (seq? x)
          (case (when (symbol? (first x)) (first x))
            ns (emit-ns x {})
            def (emit-def x {})
            do (run! host-eval (next x))
            defprotocol* (emit-defprotocol* x {})
            deftype*
            (let [[_ class-name fields opts & specs] x
                  [class-name & type-params] (cons class-name (:type-params (meta class-name)))
                  mclass-name (with-meta
                                (or (:dart/name (meta class-name)) (munge class-name {}))
                                {:type-params type-params})
                  env {:type-vars (set type-params)}
                  def-me!
                  (fn def-me! [resolve-fully]
                    (let [dart-type
                          (if resolve-fully
                            (new-dart-type mclass-name type-params
                              (map #(with-meta % (dart-meta % env)) fields)
                              (parse-class-specs class-name opts specs env)
                              env)
                            (do
                              (swap! nses do-def class-name
                                {:dart/name mclass-name
                                 :dart/type (new-dart-type mclass-name type-params nil env)
                                 :type :class})
                              (new-dart-type mclass-name type-params nil
                                (parse-class-specs class-name opts
                                  (map #(cond->> % (seq? %) (take 2)) specs) env)
                                env)))]
                      (swap! nses do-def class-name
                        (cond->
                            {:dart/name mclass-name
                             :dart/type dart-type
                             :type :class}
                          (not resolve-fully)
                          (assoc :refresh! #(def-me! true))))))]
              (def-me! false))
            nil)))
      (catch Exception e
        (throw
          (if-some [stack (::emit-stack (ex-data e))]
            (ex-info (ex-message e) (assoc (ex-data e) ::emit-stack (conj stack x)) (ex-cause e))
            (ex-info (str "Error while host-compiling " (pr-str x)) {::emit-stack [x]} e)))))))

(defn emit-test [expr env]
  (binding [*locals-gen* {}
            *dart-out* *out*]
    ;; force lazy expresions
    (doto (emit expr env) hash)))

;; WRITING
(declare write-types)

(defn dart-print [arg & args]
  (.write *dart-out* (str arg))
  (doseq [arg args]
    (.write *dart-out* " ")
    (.write *dart-out* (str arg))))

(defn dart-newline []
  (.write *dart-out* "\n"))

(defn dart-println [& args]
  (apply dart-print args)
  (dart-newline))

(defn write-type
  [{:keys [lib qname type-parameters nullable] :as t}]
  (when lib ; lib may be empty for parameters or void
    (let [{:keys [libs] :as all-nses} @nses]
      (when-not (-> lib libs :dart-alias)
        (let [dart-alias (global-lib-alias lib nil)]
          (swap! nses #(-> %
                         (assoc-in [:libs lib :dart-alias] dart-alias)
                         (assoc-in [:dart-aliases dart-alias] lib)))))
      (when-not (-> *current-ns* all-nses :imports (get lib))
        (swap! nses assoc-in [*current-ns* :imports lib] {}))))
  (case qname
    nil (throw (ex-info "Invalid type representation" {:dart-type t}))
    dc.Function
    (if-some [ret (:return-type t)]
      (let [args (:parameters t)
            [fixed opts] (split-with (comp not :optional) args)]
        (write-type ret)
        (dart-print " Function")
        (write-types type-parameters "<" ">")
        (dart-print "(")
        (doseq [arg fixed] (write-type (:type arg)) (dart-print ", "))
        (case (:kind (first opts))
          :positional (do (dart-print "[") (doseq [arg opts] (write-type (:type arg)) (dart-print ", ")) (dart-print "]"))
          :named (do (dart-print "{") (doseq [arg opts] (write-type (:type arg)) (dart-print " ") (dart-print (:name arg)) (dart-print ", ")) (dart-print "}"))
          nil nil)
        (dart-print ")"))
      (dart-print qname)) ; TODO correct support of function and optionals
    dc.Record
    (if-some [pos-fields (:positional-fields t)]
      (let [named-fields (:named-fields t)]
        (dart-print "(")
        (doseq [arg pos-fields] (write-type (:type arg)) (dart-print ", "))
        (when (seq named-fields)
          (dart-print "{")
          (doseq [arg named-fields]
            (write-type (:type arg)) (dart-print " ") (dart-print (:name arg)) (dart-print ", "))
          (dart-print "}"))
        (dart-print ")"))
      (dart-print qname))
    (do
      (dart-print qname)
      (write-types type-parameters "<" ">")))
  (when nullable (dart-print "?")))

(defn write-types
  ([types] (write-types types "" ""))
  ([types before] (write-types types before ""))
  ([types before after]
   (when-some [[t & ts] (seq types)]
     (dart-print before)
     (write-type t)
     (doseq [t ts] (dart-print ", ") (write-type t))
     (dart-print after))))

(defn type-str [t] (when t (with-dart-str (write-type t))))

(defn declaration [locus] (:decl locus ""))
(defn declared [locus]
  ; merge to conserve custom attributes
  (merge  (dissoc locus :fork :decl) (:fork locus)))

(def statement-locus
  {:statement true
   ; Add extra parens https://github.com/dart-lang/sdk/issues/52904
   :pre "("
   :post ");\n"})

(defn named-fn-locus [name]
  {:pre (str (or (-> name meta :dart/ret-type type-str) "dc.dynamic") " " name)
   :post "\n"})

(def return-locus
  {:pre "return "
   :post ";\n"
   :exit true})

(def throw-locus
  {:pre "throw "
   :post ";\n"
   :exit true})

(def expr-locus
  {:pre ""
   :post ""})

(def arg-locus
  {:pre ""
   :post ", "})

(defn assignment-locus [left-value]
  {:pre (str left-value "=")
   :post ";\n"})

(defn var-locus [vartype varname]
  {:pre (str (or (some-> vartype type-str) "var") " " varname "=")
   :post ";\n"
   :decl (str (or (some-> vartype type-str) "var") " " varname ";\n")
   :fork (assignment-locus varname)})

(defn final-locus
  ([varname] (final-locus (-> varname meta :dart/type) varname))
  ([vartype varname]
   (let [vartype (or vartype dc-dynamic)]
     {:pre (str "final " (some-> vartype type-str (str " ")) varname "=")
      :post ";\n"
      :decl (str "late final " (some-> vartype type-str (str " ")) varname ";\n")
      :fork (assignment-locus varname)})))

(defn const-locus
  ([varname] (const-locus (-> varname meta :dart/type) varname))
  ([vartype varname]
   (let [vartype (or vartype dc-dynamic)]
     {:pre (str "const " (some-> vartype type-str (str " ")) varname "=")
      :post ";\n"})))

(def annotation-locus
  {:pre "@"
   :post "\n"})

(declare write)

(defn write-annotations [annotations]
  (run! #(write % annotation-locus) annotations))

(defn write-top-dartfn [sym x]
  (case (first x)
    dart/fn (write x (named-fn-locus sym))
    (write x (var-locus (emit-type 'cljd.core/IFn$iface {}) (name sym)))))

(defn strip-dart-alias
  "Returns the unqualified dart name."
  [dartname]
  (or (some-> (re-matches #"(?:.+\.)?(.+)" dartname) second) dartname))

(defn write-top-field [sym x]
  (write (ensure-dart-expr x {})
         (var-locus dc-dynamic (strip-dart-alias (name sym)))))

(defn write-dynamic-var-top-field [k dart-sym x]
  (let [root-sym (symbol (str dart-sym "$root"))
        type (-> dart-sym meta :dart/type (or dc-dynamic))]
    (write-top-field root-sym x)
    (write-type type)
    (dart-print " get ")
    (dart-print dart-sym)
    (dart-print " => ")
    (write (list 'dart/as
             (emit `(cljd.core/get-dynamic-binding '~k ~root-sym) {root-sym root-sym})
             type) expr-locus)
    (dart-print ";\nset ")
    (dart-print dart-sym)
    (dart-print "(dc.dynamic v) => ")
    (write (emit `(cljd.core/set-dynamic-binding! '~k ~'v) '{v v}) expr-locus)
    (dart-print ";\n")))

(defn- write-args [args]
  (let [[positionals nameds] (split-with (complement keyword?) args)]
    (dart-print "(")
    (run! #(write % arg-locus) positionals)
    (run! (fn [[k x]]
            (dart-print (str (name k) ": "))
            (write x arg-locus)) (partition 2 nameds))
    (dart-print ")")))

(defn write-string-literal [s]
  (dart-print
   (str \"
        (replace-all s #"([\x00-\x1f])|[$\\\"]"
                     (fn [match]
                       (let [[match control-char] (-> match #?@(:cljd [re-groups]))]
                         (if control-char
                           (case control-char
                             "\b" "\\b"
                             "\n" "\\n"
                             "\r" "\\r"
                             "\t" "\\t"
                             "\f" "\\f"
                             "\13" "\\v"
                             (str "\\x"
                                  #?(:clj
                                     (-> control-char (nth 0) long
                                         (+ 0x100)
                                         Long/toHexString
                                         (subs 1))
                                     :cljd
                                     (-> control-char
                                         (.codeUnitAt 0)
                                         (.toRadixString 16)
                                         (.padLeft 2 "0")))))
                           (str "\\" match)))))
        \")))

(defn write-literal [x]
  (cond
    (string? x) (write-string-literal x)
    (nil? x) (dart-print "null")
    (symbol? x) (let [x (name x)]
                  (when-some [[_ dart-alias] (re-matches #"(.+?)\..+" x)]
                    (let [{:keys [dart-aliases libs] :as all-nses} @nses
                          lib (dart-aliases dart-alias)]
                      (when-not (-> *current-ns* all-nses :imports (get lib))
                        (swap! nses assoc-in [*current-ns* :imports lib] {}))))
                  (dart-print x))
    (double? x)
    (cond
      (Double/isNaN x) (dart-print "dc.double.nan")
      (Double/isInfinite x) (dart-print (if (pos? x)
                                          "dc.double.infinity"
                                          "dc.double.negativeInfinity"))
      :else (dart-print (str x)))
    :else (dart-print (str x))))

(defn write-params [fixed-params opt-kind opt-params]
  (when-not (seqable? opt-params) (throw (ex-info "fail" {:k opt-params})))
  (dart-print "(")
  (doseq [p fixed-params :let [{:dart/keys [type]} (meta p)]]
    (write-type (or type dc-dynamic))
    (dart-print " ") (dart-print p) (dart-print ", "))
  (when (seq opt-params)
    (dart-print (case opt-kind :positional "[" "{"))
    (doseq [[p d] opt-params
            :let [{:dart/keys [type]} (meta p)
                  type (or type dc-dynamic)
                  required-meta (and (= :named opt-kind)
                                  (not (nullable-type? type))
                                  (nil? d))]]
      (when required-meta
        (dart-print "required "))
      (write-type type)
      (dart-print " ")
      (dart-print p)
      (when-not required-meta
        (dart-print " = ")
        (write d expr-locus))
      (dart-print ", "))
    (dart-print (case opt-kind :positional "]" "}")))
  (dart-print ")"))

(defn write-class [{class-name :name :keys [mixin abstract extends implements with fields ctor ctor-params super-ctor methods nsm]}]
  (when abstract (dart-print "abstract "))
  (let [[_ dart-alias local-class-name] (re-matches #"(?:([a-zA-Z0-9_$]+)\.)?(.+)" (type-str class-name))]
    ; TODO assert dart-alias is current alias
    (dart-print (if mixin "mixin" "class") local-class-name))
  (when extends (dart-print (if mixin " on " " extends ")) (write-type extends))
  (write-types with " with ")
  (write-types implements " implements ")
  (dart-print " {\n")
  (doseq [field fields
          :let [{:dart/keys [late mutable type]} (meta field)]]
    (when late (dart-print "late "))
    (some-> (cond (not mutable) "final " (not type) "var ") dart-print)
    (when type (write-type type) (dart-print " "))
    (dart-print field) (dart-print ";\n"))

  (when-not (or mixin abstract)
    (dart-newline)
    (when (-> class-name :members (get (name ctor)) :const) ; this :members is weird
      (dart-print "const "))
    (dart-print (str (or ctor class-name) "("))
    (doseq [p ctor-params]
      (dart-print (if (seq? p) (str "this." (second p)) p))
      (dart-print ", "))
    (dart-print "):super")
    (some->> super-ctor :method (str ".") dart-print)
    (write-args (:args super-ctor))
    (dart-print ";\n"))

  (doseq [[mname type-params fixed-params opt-kind opt-params no-explicit-body body] methods
          :let [{:dart/keys [getter setter type async]} (meta mname)]]
    (dart-newline)
    (when-not setter
      (write-type (or type dc-dynamic))
      (dart-print " "))
    (cond
      getter (dart-print "get ")
      setter (dart-print "set "))
    (when (#{">>" "[]=" "*" "%" "<=" "unary-" "|" "~" "/" "-" ">>>" "ˆ" "~/"
             "[]" ">=" "&" "<" "<<" "==" "+" ">"} (name mname))
      (dart-print "operator "))
    (dart-print mname)
    (when (seq type-params)
      (dart-print "<")
      (dart-print (str/join ", " type-params))
      (dart-print ">"))
    (when-not getter (write-params fixed-params opt-kind opt-params))
    (if (and abstract no-explicit-body)
      (dart-print ";\n")
      (do
        (when async (dart-print " async "))
        (dart-print "{\n")
        (write body return-locus)
        (dart-print "}\n"))))

  (when nsm
    (dart-newline)
    (dart-print "dc.dynamic noSuchMethod(i)=>super.noSuchMethod(i);\n"))

  (dart-print "}\n"))

(def ^:private ^:dynamic *caught-exception-symbol* nil)

(defn merge-dart-meta [inferred explicit]
  (if inferred
    (persistent!
      (reduce-kv
        (fn [m k ev]
          (assoc! m k
            (if-some [iv (m k)]
              (case k
                :dart/type (if (is-assignable? ev iv) iv ev)
                ev)
              ev)))
        (transient inferred) explicit))
    explicit))

(defn- inheritance-graph [dart-type]
  (loop [g {} todos [dart-type]]
    (if-some [{cqnt :canon-qname :as t} (peek todos)]
      (if (g cqnt)
        (recur g (pop todos))
        (let [{:keys [super mixins interfaces]} (full-class-info t)
              ts (cond->> (concat mixins interfaces) super (cons super))]
          (recur (assoc g cqnt {:type t :supers (into #{} (map :canon-qname) ts)})
            (-> todos pop (into ts)))))
      g)))

(defn- common-roots [ga gb]
  (let [g (select-keys ga (keys gb))
        g (transduce (keep (fn [[k v]] (when (:private (analyzer-info (:lib (:type v)))) k))) dissoc g g)]
    (keys (transduce (mapcat :supers) dissoc g (vals g)))))

(defn- merge-types [a b]
  ; this should take a list of types or returns an union type
  ; because currently the inferred type for a cond may change depending on the clauses order
  (let [cache (volatile! {})] ; for reentrance/fix point
    (letfn [(single-common-type [a b]
              (let [k [(:canon-qname a) (:canon-qname b)]]
                (or (@cache k)
                  (do
                    (vswap! cache assoc k dc-Object)
                    (let [ga (inheritance-graph (full-class-info a))
                          gb (inheritance-graph (full-class-info b))
                          [cqn & too-many] (common-roots ga gb)
                          a (:type (ga cqn))
                          b (:type (gb cqn))
                          c (if too-many
                              dc-Object
                              (merge-type-params a b))]
                      (vswap! cache assoc k c)
                      c)))))
            (merge-type-params [a b]
              (assoc a
                :type-parameters
                (into []
                  (map merge-types (:type-parameters a) (:type-parameters b)))))
            (merge-types [a b]
              (let [{qna :canon-qname :as a} (case (:canon-qname a)
                                               (dc.Null void) dc-Null
                                               nil dc-dynamic
                                               a)
                    {qnb :canon-qname :as b} (case (:canon-qname b)
                                               (dc.Null void) dc-Null
                                               nil dc-dynamic
                                               b)]
                (cond
                  ;; TODO: merging Functions can done better.
                  (= qna qnb 'dc.Function) dc-Function
                  ; fast path, also handles dc.Null dc.Null which should not set :nullable true
                  (= qna qnb) (assoc (merge-type-params a b) :nullable (or (:nullable a) (:nullable b))
                                :type-parameters
                                (into []
                                  (map merge-types (:type-parameters a) (:type-parameters b))))
                  (= 'dc.Never qna) b
                  (= 'dc.Never qnb) a
                  (= 'dc.dynamic qna) dc-dynamic
                  (= 'dc.dynamic qnb) dc-dynamic
                  (= 'dc.Null qna) (assoc b :nullable true)
                  (= 'dc.Null qnb) (assoc a :nullable true)
                  (= 'da.Future qna)
                  (assoc da-FutureOr
                    :type-parameters [(merge-types (first (:type-parameters a))
                                        (if (= 'da.FutureOr qnb)
                                          (first (:type-parameters b))
                                          b))]
                    :nullable (or (:nullable a) (:nullable b)))
                  (= 'da.Future qnb)
                  (assoc da-FutureOr
                    :type-parameters [(merge-types (first (:type-parameters b))
                                        (if (= 'da.FutureOr qna)
                                          (first (:type-parameters a))
                                          a))]
                    :nullable (or (:nullable a) (:nullable b)))
                  (= 'pseudo.not-bool qna)
                  (case qnb
                    (dc.Object dc.bool) dc-dynamic
                    a)
                  (= 'pseudo.not-bool qnb)
                  (case qna
                    (dc.Object dc.bool) dc-dynamic
                    b)
                  :else (assoc (single-common-type a b)
                          :nullable (or (:nullable a) (:nullable b))))))]
      (merge-types a b))))

(comment
  (write-type (binding [dart-libs-info li]
                (merge-types dc-num dc-String)))
  ; dc.Comparable<dc.Object>

  (write-type (binding [dart-libs-info li]
                (merge-types dc-int (assoc dc-String :nullable true))))
  ; dc.Comparable<dc.Comparable<dc.Object>>?
  ; I would have preferred dc.Comparable<dc.Object> as returned for String * num
  )

(defn infer-type [x]
  {:post [(or (:dart/type %) (do (binding [*print-meta* true]
                                   (prn (meta (second x)))
                                   (prn x))))]}
  (let [m (meta x)]
    (if (:dart/inferred m)
      m
      (let [inferred
            (cond
              (nil? x) {:dart/type dc-Null :dart/const true}
              (boolean? x) {:dart/type dc-bool :dart/const true}
              (string? x) {:dart/type dc-String :dart/const true}
              (double? x) {:dart/type dc-double :dart/const true}
              (integer? x) {:dart/type dc-int :dart/const true}
              (and (symbol? x) (-> x meta :dart/type :canon-qname (= 'dc.Function)))
              {:dart/ret-type (-> x meta :dart/signature :return-type)}
              (seq? x)
              (case (let [x (first x)] (when (symbol? x) x))
                dart/loop (infer-type (last x))
                dart/try
                (let [[_ dart-expr dart-catches-expr] x]
                  {:dart/type (reduce (fn [acc item]
                                        (if (= (:canon-qname (:dart/type acc)) 'dc.dynamic)
                                          (reduced acc)
                                          (merge-types acc item))) (:dart/type (infer-type dart-expr)) (map #(:dart/type (infer-type (last %))) dart-catches-expr))
                   :dart/const false})
                dart/if
                (let [[_ _ dart-then dart-else] x]
                  {:dart/type (merge-types (:dart/type (infer-type dart-then)) (:dart/type (infer-type dart-else)))
                   :dart/const false})
                dart/let (infer-type (last x))
                dart/.
                (let [[_ a meth & bs :as all] x ; TODO use type-params
                      {:dart/keys [fn-type ret-type]} (infer-type a)
                      [methname type-params] (if (sequential? meth) [(name (first meth)) (second meth)] [meth nil])]
                  (when (and (= :ifn fn-type) ret-type) {:dart/type ret-type}))
                ;; (dart/.- other$2 "ns")
                dart/as (let [[_ _ type] x]
                          {:dart/type type
                           :dart/const false})
                dart/set! (let [[_ target [_ expr]] x]
                            (infer-type expr))
                (let [{:keys [dart/ret-type]} (infer-type (first x))]
                  (when ret-type
                    {:dart/type ret-type})))
              :else nil)]
        (merge-dart-meta
          (cond-> (assoc inferred :dart/inferred true)
            (nil? (:dart/type inferred)) (assoc :dart/type dc-dynamic))
          m)))))

(defn print-pre [locus]
  (dart-print (:pre locus))
  (dotimes [_ (count (:casts locus))] (dart-print "(")))

(defn print-post [locus]
  (doseq [type (:casts locus)]
    (if (= 'dc.double (:canon-qname type))
      (dart-print
        (if (:nullable type)
          " as dc.num?)?.toDouble()"
          " as dc.num).toDouble()"))
      (do
        (dart-print " as ")
        (write-type type)
        (dart-print ")"))))
  (dart-print (:post locus)))

(defn write
  "Takes a dartsexp and a locus.
   Prints valid dart code.
   Returns true when the just-written code is exiting control flow (return throw continue) -- this enable some dead code removal."
  [x locus]
  (cond
    (vector? x)
    (do (print-pre locus)
        (some-> x meta :dart/type :type-parameters (write-types "<" ">"))
        (dart-print "[")
        (run! #(write % arg-locus) x)
        (dart-print "]")
        (print-post locus))
    (seq? x)
    (case (let [op (first x)] (when (symbol? op) op))
      dart/record
      (let [[_ & args] x]
        (print-pre locus)
        (write-args args)
        (print-post locus)
        (:exit locus))
      dart/fn
      (let [[_ fixed-params opt-kind opt-params async body] x]
        (print-pre locus)
        (write-params fixed-params opt-kind opt-params)
        (when async (dart-print " async "))
        (dart-print "{\n")
        (write body return-locus)
        (dart-print "}")
        (print-post locus))
      dart/let
      (let [[_ bindings expr] x]
        (or
         (some (fn [[v e]]
                 (if (= :late v)
                   (dart-print (declaration (final-locus e)))
                   (write e
                     (cond
                       (nil? v) statement-locus
                       (and (seq? e) (= 'dart/fn (first e))) (named-fn-locus v)
                       (-> v meta :dart/const) (const-locus v)
                       :else (final-locus v)))))
               bindings)
         (write expr locus)))
      dart/try
      (let [[_ body catches final] x
            decl (declaration locus)
            locus (declared locus)
            _  (some-> decl dart-print)
            _ (dart-print "try {\n")
            exit (write body locus)
            exit
            (transduce
             (map (fn [[classname e st expr]]
                    (dart-print "} on ")
                    (write-type classname)
                    (dart-print " catch (")
                    (dart-print e)
                    (some->> st (dart-print ","))
                    (dart-print ") {\n")
                    (binding [*caught-exception-symbol* e]
                      (write expr locus))))
             (completing (fn [a b] (and a b)))
             exit catches)]
        (when final
          (dart-print "} finally {\n")
          (write final statement-locus))
        (dart-print "}\n")
        exit)
      dart/as
      (let [[_ expr type] x]
        (write expr (update locus :casts conj type)))
      #_(let [[_ expr type] x]
        (print-pre locus)
        (dart-print "(")
        (write expr expr-locus)
        (dart-print " as ")
        (write-type type)
        (dart-print ")")
        (print-post locus))
      dart/is
      (let [[_ expr type] x]
        (print-pre locus)
        (dart-print "(")
        (write expr expr-locus)
        (dart-print " is ")
        (write-type type)
        (dart-print ")")
        (print-post locus))
      dart/assert
      (let [[_ condition msg-expr] x]
        (dart-print "assert(")
        (write condition expr-locus)
        (dart-print ", ")
        (write msg-expr expr-locus)
        (dart-print "); // assert\n\n"))
      dart/await
      (let [[_ expr] x]
        (print-pre locus)
        (dart-print "(await ")
        (write expr expr-locus)
        (dart-print ")")
        (print-post locus))
      dart/throw
      (let [[_ expr] x]
        (if (= expr *caught-exception-symbol*)
          (dart-print "rethrow;\n")
          (write expr throw-locus))
        true)
      dart/case
      (let [[_ expr clauses default-expr] x
            decl (declaration locus)
            locus (declared locus)
            _ (some-> decl dart-print)
            _ (dart-print "switch(")
            _ (write expr expr-locus)
            _ (dart-print "){\n")
            exit (reduce
                  (fn [exit [vals expr]]
                    (run! #(do (dart-print "case ") (write % expr-locus) (dart-print ":\n")) vals)
                    (if (write expr locus)
                      exit
                      (dart-print "break;\n")))
                  true
                  clauses)
            _ (dart-print "_default: default:\n")
            exit (and (write default-expr locus) exit)]
        (dart-print "}\n")
        exit)
      dart/continue
      (let [[_ label] x]
        (dart-print "continue ") (dart-print label) (dart-print ";\n")
        true)
      dart/if
      (let [decl (declaration locus)
            locus (declared locus)]
        (some-> decl dart-print)
        (loop [[_ test then else] x]
          (dart-print "if(")
          (write test expr-locus)
          (dart-print "){\n")
          (write then locus)
          (cond
            (:exit locus)
            (do
              (dart-print "}\n")
              (write else locus))
            (and (seq? else) (= 'dart/if (first else)))
            (do
              (dart-print "}else ")
              (recur else))
            :else
            (do
              (dart-print "}else{\n")
              (write else locus)
              (dart-print "}\n")))))
      dart/loop
      (let [[_ bindings expr] x
            decl (declaration locus)
            locus (-> locus declared (assoc :loop-bindings (map first bindings)))]
        (some-> decl dart-print)
        (doseq [[v e] bindings]
          (write e (var-locus (or (-> v meta :dart/type) dc-dynamic) v)))
        (dart-print "do {\n")
        (when-not (write expr locus)
          (dart-print "break;\n"))
        (dart-print "} while(true);\n"))
      dart/recur
      (let [[_ & exprs] x
            {:keys [loop-bindings]} locus
            expected (count loop-bindings)
            actual (count exprs)]
        (when-not loop-bindings
          (throw (ex-info "Can only recur from tail position." {:x x})))
        (when-not (= expected actual)
          (throw (ex-info (str "Mismatched argument count to recur, expected: "
                               expected " args, got: " actual) {})))
        (let [loop-rebindings (remove (fn [[v e]] (= v e)) (map vector loop-bindings exprs))
              vars (into #{} (map first) loop-rebindings)
              vars-usages (->>
                            (map (fn [[v e]]
                                   (into #{} (comp (filter symbol?) (keep (disj vars v)))
                                     (tree-seq sequential? seq e)))
                              loop-rebindings)
                           reverse
                           (reductions into)
                           reverse)
              tmps (into {}
                     (map (fn [[v e] vs]
                            (assert (re-matches #".*\$\d+" (name v)))
                            (when (vs v) [v (with-meta (symbol (str v "tmp")) (meta v))]))
                       loop-rebindings vars-usages))]
          (doseq [[v e] loop-rebindings]
            (write e (if-some [tmp (tmps v)] (final-locus tmp) (assignment-locus v))))
          (doseq [[v tmp] tmps]
            (write tmp (assignment-locus v)))
          (dart-print "continue;\n")
          true))
      dart/set!
      (let [[_ target [tmp val :as tmp-binding]] x]
        ; dart/set! like clojure set! evaluates to the set value
        ; when it occurs in non-lifted expression contexts (return, lhr of lets...)
        ; we have to avoid double evaluation of val, that's why dart/set! provides an
        ; "emergency binding" to provide a name for the resulting value.
        (cond
          (:statement locus)
          (write val
            (assignment-locus
              (binding [*dart-out* (java.io.StringWriter.)]
                (write target expr-locus)
                (str *dart-out*))))
          (nil? tmp) (throw (ex-info "dart/set! without temp in expression position"))
          :else
          (recur
            (list 'dart/let [tmp-binding
                             [nil (list 'dart/set! target [nil tmp])]]
              tmp) locus)))
      dart/.-
      (let [[_ obj fld] x]
        (print-pre locus)
        (if (= :class (:kind obj))
          (write-type obj)
          (write obj expr-locus))
        (dart-print (str "." fld))
        (print-post locus)
        (:exit locus))
      dart/.
      (let [[_ obj meth & args] x
            [meth & type-params] (cond-> meth (not (sequential? meth)) list)
            meth (name meth) ; sometimes meth is a symbol
            is-plain-method (re-matches #"^[a-zA-Z_$].*" meth)
            ; the :statement test below is not a gratuitous optimization meant to
            ; remove unnecessary parens.
            ; this :statement test is to handle the case of the []= operator which
            ; must not be put into parens and always occur in a statement locus.
            ; the plain-method & :this-position test however is purely aesthetic to
            ; avoid over-parenthizing chained method calls.
            must-wrap (not (or (:statement locus) (and is-plain-method (:this-position locus))))]
        (print-pre locus)
        (when must-wrap (dart-print "("))
        (case meth
          ;; operators, some are cljd tricks
          "[]" (do
                 (write obj expr-locus)
                 (dart-print "[")
                 (write (first args) expr-locus)
                 (dart-print "]"))
          "[]=" (do
                  (write obj expr-locus)
                  (dart-print "[")
                  (write (first args) expr-locus)
                  (dart-print "]=")
                  (write (second args) expr-locus))
          ("~" "!") (do
                      (dart-print meth)
                      (write obj expr-locus))
          "-" (if args
                (do
                  (write obj expr-locus)
                  (dart-print " ")
                  (dart-print meth)
                  (dart-print " ")
                  (write (first args) expr-locus))
                (do
                  (assert false "DEAD BRANCH")
                  (dart-print meth)
                  (write obj expr-locus)))
          "unary-" (do
                     (dart-print "(")
                     (dart-print "- ")
                     (write obj expr-locus)
                     (dart-print ")"))
          ("&&" "||" "^^" "+" "*" "&" "|" "^")
          (do
            (write obj expr-locus)
            (doseq [arg args]
              (dart-print " ")
              (dart-print meth)
              (dart-print " ")
              (write arg expr-locus)))
          ("<" ">" "<=" ">=" "==" "!=" "~/" "/" "%" "<<" ">>" #_">>>")
          (do
            (assert (= (count args) 1))
            (write obj expr-locus)
            (dart-print " ")
            (dart-print meth)
            (dart-print " ")
            (write (first args) expr-locus))
          ;; else plain method
          (do
            (assert is-plain-method (str "not a plain method: " meth))
            (when (:dart/const (meta x)) ; should only be on constructors, see #53
              (dart-print "const "))
            (cond
              (= :class (:kind obj))
              (write-type obj)
              (and (number? obj) (neg? obj))
              (do (dart-print "(")
                  (write obj expr-locus)
                  (dart-print ")"))
              :else
              (write obj (assoc expr-locus :this-position true)))
            (dart-print (str "." meth))
            (write-types type-params "<" ">")
            (write-args args)))
        (when must-wrap (dart-print ")"))
        (print-post locus)
        (:exit locus))
      dart/type
      (when-not (:statement locus)
        (print-pre locus)
        (write-type (second x))
        (print-post locus)
        (:exit locus))
      dart/new
      (let [[_ type & args] x]
        (print-pre locus)
        (when (:dart/const (meta x))
          (dart-print "const "))
        (write-type type)
        (write-args args)
        (print-post locus)
        (:exit locus))
      ;; native fn call
      (let [[f & args] x]
        (print-pre locus)
        (write f expr-locus)
        (write-types (some-> f meta :dart/signature :type-parameters) "<" ">")
        (write-args args)
        (print-post locus)
        (:exit locus)))
    :else (when-not (:statement locus)
            (print-pre locus)
            (write-literal x)
            (print-post locus)
            (:exit locus))))

(defn relativize-lib [^String src ^String target]
  (if (<= 0 (.indexOf target ":"))
    target
    (loop [[s & ss] (str/split src #"/") [t & ts :as all-ts] (str/split target #"/")]
      (if (and (some? ss) (= s t))
        (recur ss ts)
        (str/join "/" (concat (map (constantly "..") ss) all-ts))))))

;; Compile clj -> dart file
(defn dump-ns [ns-name {ns-lib :lib :as ns-map}]
  ;; used to silence the analyzer when people are using VS Code or other tools
  (dart-print "// ignore_for_file: type=lint, unnecessary_cast, unnecessary_type_check, unused_import, unused_local_variable, unused_label, unnecessary_question_mark, unused_catch_clause, type_check_with_null, dead_code\n")
  (doseq [lib (keys (:imports ns-map))
          :let [dart-alias (-> @nses :libs (get lib) :dart-alias)]]
    (dart-print "import ")
    (write-string-literal (relativize-lib ns-lib lib)) ;; TODO: relativize local libs (nses)
    (dart-print " as ")
    (dart-print dart-alias)
    (dart-print ";\n"))
  (doseq [[sym v] (->> ns-map (filter (comp symbol? key)) (sort-by key))
          :when (symbol? sym)
          :let [{:keys [dart/code]} v]
          :when code]
    (dart-println "\n// BEGIN" sym)
    (dart-println code)
    (dart-println "// END" sym)))

;; #/[bool bool .foo String]
;^{:record-type [bool bool .foo String]} dc.Record

(defn- parse-dart-params-types [params]
  (let [[fixed-params opt-params] (split-with (complement dot-symbol?) params)
        [delim & opt-params]
        (case (first opt-params)
          (... nil) opt-params
          (cons '.& opt-params))]
    {:fixed-params-types fixed-params
     :opt-kind (case delim .& :named ... :positional nil)
     :opt-params-types
     (case delim
       .& (into {} (comp (partition-all 2) (map (fn [[p t]] [(symbol (subs (name p) 1)) t]))) opt-params)
       opt-params)}))

(defn dart-type-params-reader [x]
  (cond
    (symbol? x) x
    (seq? x)
    (let [[args [_ ret slash & type-params :as rets]] (split-with (complement '#{->}) x)]
      (if (seq rets)
        ; fn type
        (let [params-types (parse-dart-params-types (map dart-type-params-reader args))]
          (assert (or (= 2 (count rets)) (= slash '/)))
          (with-meta 'dart:core/Function
            {:fn-type (assoc params-types :ret-type (dart-type-params-reader ret))
             :type-params type-params}))
        ; parametrized type
        (let [[type & params] x]
          (cond-> type
            params
            (vary-meta assoc :type-params (map dart-type-params-reader params))))))
    (vector? x)
    (let [{:keys [opt-kind fixed-params-types opt-params-types]}
          (parse-dart-params-types (map dart-type-params-reader x))]
      (when (= :positional opt-kind)
        (throw (Exception. (str "A record type can't have optional fields: " (pr-str x)))))
      (with-meta 'dart:core/Record {:fields-types {:fixed-fields-types fixed-params-types
                                                   :named-fields-types opt-params-types}}))
    :else
    (throw (Exception. (str "Unsupported type literal syntax: " (pr-str x))))))

(def dart-data-readers
  {'dart #(tagged-literal 'dart %)
   '/ dart-type-params-reader})

(def cljd-resolver
  (reify #?(:clj clojure.lang.LispReader$Resolver
            :cljd cljd.reader/IResolver)
    (currentNS [_] *current-ns*)
    (resolveClass [_ sym]
      (get-in @nses [*current-ns* :mappings sym]))
    (resolveAlias [_ sym]
      (let [{:keys [libs] :as nses} @nses
            {:keys [clj-aliases]} (nses *current-ns*)]
        (when-some [lib (some-> sym name clj-aliases libs)]
          (or (:ns lib)
            (symbol (str "$lib:" (:dart-alias lib)))))))
    (resolveVar [_ sym] nil)))

(defmacro with-cljd-reader [& body]
  `(binding [*data-readers* (into *data-readers* dart-data-readers)
             *reader-resolver* cljd-resolver]
     ~@body))

(defn load-input [in]
  #?(:clj
     (with-cljd-reader
       (let [in (clojure.lang.LineNumberingPushbackReader. in)]
         (loop []
           (let [form (read {:eof in :read-cond :allow :features #{:cljd}} in)]
             (when-not (identical? form in)
               (try
                 (binding [*locals-gen* {}] (emit form {}))
                 (catch Exception e
                   (throw (if (::emit-stack (ex-data e))
                            e
                            (ex-info "Compilation error." {:form form} e)))))
               (recur))))))))

(defn host-load-input [in]
  #?(:clj
     (with-cljd-reader
       (let [in (clojure.lang.LineNumberingPushbackReader. in)]
         (loop []
           (let [form (read {:eof in :read-cond :allow
                             :features #{:cljd :cljd/clj-host}} in)]
             (when-not (identical? form in)
               (binding [*locals-gen* {}] (host-eval form))
               (recur))))))))

(defn- rename-lib [{:keys [libs dart-aliases] :as nses} from to]
  (let [{:keys [ns dart-alias] :as m} (libs from)
        nses (assoc nses
               :libs (-> libs (dissoc from) (assoc to m))
               :dart-aliases (assoc dart-aliases dart-alias to))]
    (into nses (keep (fn [[ns ns-map]]
                       (when (symbol? ns)
                         (let [{:keys [clj-aliases imports lib contribs]} ns-map
                               imports (-> imports (dissoc from) (assoc to (imports from)))
                               clj-aliases (into {}
                                             (map (fn [[alias lib]]
                                                    [alias (if (= lib from) to lib)]))
                                             clj-aliases)
                               contribs (if (get contribs from) (-> contribs (dissoc from) (assoc to to)) contribs)]
                           [ns (assoc ns-map :lib (if (= lib from) to lib)
                                 :imports imports :clj-aliases clj-aliases
                                 :contribs contribs)])))) nses)))

(defn compile-input [in]
  (binding [*host-eval* false]
    (load-input in)
    (let [the-ns (@nses *current-ns*)
          libname (:lib the-ns)
          is-test-ns (-> 'main the-ns :meta :dart/test)
          libname' (if is-test-ns
                     (str *test-path* (str/replace (subs libname (count *lib-path*)) #"\.dart$" "_test.dart"))
                     libname)]
      (when is-test-ns
        (swap! nses rename-lib libname libname'))
      libname')))

(defn ns-to-paths [ns-name]
  (let [base (replace-all (name ns-name) #"[.-]" {"." "/" "-" "_"})]
    [(str base ".cljd") (str base ".cljc")]))

(defn find-resource
  "Search for a file on the clojure path."
  [filename]
  (io/resource filename))

(defn compile-url
  "Compiles the resource at the specified url. file-path is purely indicative.
   Returns the libname."
  [^String file-path ^java.net.URL url]
  (binding [*file* file-path]
    (when *hosted*
      (with-open [in (.openStream url)]
        (host-load-input (java.io.InputStreamReader. in "UTF-8")))
      (doseq [{:keys [refresh! name]} (sort-by (fn [{:keys [name]}] (case name (IProtocol SeqListMixin) 0 1)) (vals (@nses *current-ns*)))
              :when refresh!]
        (refresh!)))
    (with-open [in (.openStream url)]
      (compile-input (java.io.InputStreamReader. in "UTF-8")))))

(defn compile-namespace [ns-name]
  ;; iterate first on file variants then on paths, not the other way!
  (binding [*current-ns* ns-name]
      (let [file-paths (ns-to-paths ns-name)
            cljd-core (when-not (= ns-name 'cljd-core) (get @nses 'cljd.core))]
        (if-some [[file-path url] (some (fn [p] (some->> (find-resource p) (vector p))) file-paths)]
          (compile-url file-path url)
          (throw (ex-info (str "Could not locate "
                            (str/join " or " file-paths))
                   {:ns ns-name}))))))

(defmacro with-dump-modified-files
  "Dump modified Dart files upon succesful execution of the body."
  [& body]
  `(let [before# @nses]
     ~@body
     (doseq [[ns# ns-map#] @nses
             :when (and (symbol? ns#)
                     (not (identical? ns-map# (before# ns#))))]
       (with-open [out# (-> (java.io.File. ^String (:lib ns-map#))
                          (doto (-> .getParentFile .mkdirs))
                          java.io.FileOutputStream.
                          (java.io.OutputStreamWriter. "UTF-8")
                          java.io.BufferedWriter.)]
        (binding [*dart-out* out#]
          (dump-ns ns# ns-map#))))))

(defn compile
  [ns]
  (with-dump-modified-files
    (compile-namespace ns)))

(defn peek-ns
  [in]
  (with-open [in (io/reader in)]
    (with-cljd-reader
      (let [in (clojure.lang.LineNumberingPushbackReader. in)]
        (loop []
          (let [form (read {:eof in :read-cond :allow} in)]
            (cond
              (identical? form in) nil
              (and (seq? form) (= 'ns (first form))) (second form)
              :else (recur))))))))

(defn- transitive-closure [seeds f]
  (loop [closure #{} todo (vec seeds)]
    (if-some [x (peek todo)]
      (if (closure x)
        (recur closure (pop todo))
        (recur (conj closure x) (into (pop todo) (f x))))
      closure)))

(defn recompile
  "Takes a collection of nses to recompile."
  [nses-to-recompile recompile-count]
  (with-dump-modified-files
    (let [nses-before @nses
          dependants (fn [ns]
                       (let [lib (some-> nses-before ns :lib)]
                         (for [[ns {:keys [imports contribs]}] nses-before
                               :when (and (get imports lib) (not (get contribs lib)))]
                           ns)))
          nses-to-recompile
          (transitive-closure nses-to-recompile dependants)]
      (try

        (let [contributions
              (for [ns nses-to-recompile
                    path (keys (:contributions-paths (nses-before ns)))]
                path)]
          ; undo contributions
          (run! (fn [path]
                  (assert (vector? path))
                  (swap! nses update-in (pop path) dissoc (peek path)))
            contributions)

          ; remove nses to be recompiled -- but not their libs to keep aliases stable
          (apply swap! nses dissoc nses-to-recompile)

          (doseq [[protocol-ns protocol-name tag] contributions]
            (case tag
              :extensions
              (when-some [proto-map (get-in @nses [protocol-ns protocol-name])]
                (binding [*current-ns* protocol-ns
                          *locals-gen* {}]
                  (emit (expand-protocol-impl proto-map) {}))))))
        (binding [*recompile-count* recompile-count]
          (doseq [ns nses-to-recompile
                ; the ns may already have been transitively reloaded
                  :when (nil? (@nses ns))]
          ; recompiling via ns and not via url because cljd/cljc shadowing
            (compile-namespace ns)))
        (catch Exception e
          (reset! nses nses-before) ; avoid messy states
          (throw e))))))

(comment

  (do

    (require '[cljd.build :as build])

    (binding [build/*deps* {:cljd/opts {:kind :dart}}
              *dart-version* :dart3]
      (let [user-dir (System/getProperty "user.dir")
            analyzer-dir (build/ensure-cljd-analyzer!)]
        (build/exec {:in nil :out nil} "dart" "pub" "get")
        (def li (mk-live-analyzer-info
                 (build/exec {:async true :in nil :out nil :dir analyzer-dir}
                             (some-> build/*deps* :cljd/opts :kind name)
                             "pub" "run" "bin/analyzer.dart" user-dir))))))


  (binding [analyzer-info li
            *dart-out* *out*
            *locals-gen* {}
            *current-ns* 'cljd.core]
    #_(write
        (emit-test `~(tagged-literal 'dart '[1 2]) {})
        return-locus)
    (write (emit-test `(fn [~(with-meta 'ss
                               {:tag (dart-type-params-reader '(dart:core/Set dart:core/List))})]
                         (let [v (.lookup ss "key")]
                           (.-first! "val"))) {})
      return-locus))

  (binding [analyzer-info li
            *dart-out* *out*
            *locals-gen* {}
            *current-ns* 'cljd.core]
    (let [c `c#]
      (write (emit `(fn [~(with-meta 'ss {:tag (dart-type-params-reader '(dart:core/Set dart:core/List))})
                         f#]
                      (let [~c (.lookup ~'ss "key")]
                        (.-first! (let [x# ~(with-meta `(identity ~c) {:tag 'List})]
                                    x#) (let [v# (str "val")]
                                          (f#)
                                          (str v# "kiki")))
                        ;;(set! (.-first v#) "val")
                        )) {})
        return-locus)))


  (binding [analyzer-info li]
    (do
      (time
        (binding [*hosted* true
                  analyzer-info li]
          (compile 'cljd.core)))
      ;; XOXO
      (time
        (binding [analyzer-info li]
          (compile 'cljd.string)))

      #_(time
          (binding [*hosted* false
                    analyzer-info li]
            (compile 'cljd.multi)))

      (time
        (binding [analyzer-info li]
          (compile 'cljd.walk)))

      (time
        (binding [analyzer-info li
                  *hosted* true]
          (compile 'cljd.template)))

      (time
        (binding [analyzer-info li
                  *hosted* true]
          (compile 'cljd.test)))

      (time
        (binding [analyzer-info li
                  *hosted* true]
          (compile 'cljd.test-clojure.for)))

      (time
        (binding [*hosted* false
                  analyzer-info li]
          (compile 'cljd.test-clojure.core-test)))

      (time
        (binding [*hosted* false
                  analyzer-info li]
          (compile 'cljd.test-clojure.core-test-cljd)))

      (time
        (binding [analyzer-info li
                  *hosted* true]
          (compile 'cljd.test-clojure.string)))

      (time
        (binding [*hosted* true
                  analyzer-info li]
          (compile 'cljd.reader)))

      (time
        (binding [*hosted* false
                  analyzer-info li]
          (compile 'cljd.test-reader.reader-test)))))

  (binding [*locals-gen* {}
            analyzer-info li]
    (write (emit
             '(deftype Foo [x]
                  (bar [this])) {}) return-locus)
  )

  (binding [*locals-gen* {}
            analyzer-info li]
    (write (emit
             '(->Foo 14) {}) return-locus)
    )

  (binding [*locals-gen* {}
            analyzer-info li]
    (write (emit
             '(Foo 14) {}) return-locus)
    )

  (binding [*locals-gen* {}
            analyzer-info li]
    (write (emit ;-fn-call
             '(map 14) {}) return-locus)
    )

  (binding [*locals-gen* {}
            analyzer-info li]
    (keys (infer-type (emit ;-fn-call
                                     'Foo {})))
    )

  (binding [*dart-out* *out*
            analyzer-info li]
    (write
      (emit-test '(fn [] "dd" nil) {})
      return-locus))

  (write
    (binding [analyzer-info li]
      (emit-test '(deftype X [] (meuh [_]
                                  (.+ 1 1))) {}))
    return-locus)

  (write
    (binding [analyzer-info li]
      (emit-test '(let [cnt ^int (if (odd? 3) 1 nil)]) {}))
    return-locus)


  (write
    (binding [analyzer-info li]
      (emit-test '(fn [x] (loop [i x] (recur (inc i)))) {}))
    return-locus)

  (binding [*host-eval* true]
    (write
      (binding [analyzer-info li]
        (emit-test '(defmacro ddd [name [& fields] & b] 2) {}))
      return-locus))

  (binding [*host-eval* true]
    (write
      (binding [analyzer-info li]
        (emit-test '(let [re #"a" m (.matchAsPrefix re "abc")]
                      (.group m 0)) {}))
      return-locus))

  (write
    (binding [analyzer-info li]
      (emit-test '(.replaceAllMapped "abc" #"." assoc) {}))
    return-locus)

  (write
    (binding [analyzer-info li]
      (emit-test '(dart:core/Symbol (.toString ^Object x)) '{x x}))
    return-locus)


  (binding [analyzer-info li]
    (meta (emit-test 'dart:core/print {})))

  (binding [analyzer-info li]
    (resolve-symbol 'dart:core/print {}))

  (write
    (binding [dart-libs-info (assoc-in li ["dart:core" "print" :parameters 0 :type] dc-String)]
      (emit-test '(let [x (count 3)] x) '{}))
    return-locus)

  (write
    (binding [analyzer-info li]
      (emit-test '(== y 0) '{x x y y}))
    return-locus)

  (write
    (binding [analyzer-info li]
      (emit-test '(let [x 0] (inc (u32-bit-count x))) '{}))
    return-locus)

    (write
      (binding [analyzer-info li]
        (emit-test '(let [x ^num (u32-bit-count 0)] x) '{}))
      return-locus)

    (write
      (binding [analyzer-info li]
        (emit-test '(let [^List? x nil] (.add x 2)) '{}))
      return-locus)

    (write
      (binding [analyzer-info li]
        (emit-test '(. List filled 4 nil) '{}))
      return-locus)

    (binding [analyzer-info li]
      (:type (emit-type 'List {})))

    (write
      (binding [analyzer-info li]
        (emit '(.substring ^String x 2) '{x X}))
      return-locus)

    (binding [analyzer-info li
              *locals-gen* {}]
      (infer-type (emit '(let [n (VectorNode. nil (List/empty))] (.-arr n)) {})))

    (binding [analyzer-info li
              *locals-gen* {}]
      (keys (infer-type (emit '(let [n (VectorNode. nil (List/empty))] n) {}))))

    (binding [analyzer-info li
              *locals-gen* {}]
      (infer-type (emit '(let [n (VectorNode. nil (List/empty))] (.-arr n)) {})))

    (binding [analyzer-info li]
      (magicast 'xoxo dc-String (assoc dc-String :nullable true) {}))

  (write-type (binding [analyzer-info li]
                (emit-test (dart-type-params-reader '(String -> void)) {})))

  (get-in li ["dart:core" "print" :parameters 0 :type])
  (get-in (assoc-in li ["dart:core" "print" :parameters 0 :type] dc-String)
    ["dart:core" "print"])
  (get-in li ["dart:core" "String" "replaceAllMapped"])

  (get-in @nses '[:current-ns])

  )
