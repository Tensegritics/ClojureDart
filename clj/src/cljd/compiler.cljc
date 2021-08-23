;   Copyright (c) Baptiste Dupuch & Christophe Grand . All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns cljd.compiler
  (:refer-clojure :exclude [macroexpand macroexpand-1 munge])
  (:require [clojure.string :as str]
            [clojure.java.io :as io]))

(def dart-libs-info
  (-> "core-libs.edn" clojure.java.io/resource clojure.java.io/reader clojure.lang.LineNumberingPushbackReader. clojure.edn/read))

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
    (when line
      (str " at line: " line ", column: " column))))

(defmacro ^:private else->> [& forms]
  `(->> ~@(reverse forms)))

(defn- replace-all [^String s regexp f]
  #?(:cljd
     (.replaceAllMapped s regexp f)
     :clj
     (str/replace s regexp f)))

(def ns-prototype
  {:imports {"dart:core" {}}
   :aliases {"dart:core" "dart:core"}
   :mappings
   '{Type dart:core/Type,
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

(def nses (atom {:current-ns 'user
                 :libs {"dart:core" {:dart-alias "dc" :ns nil}} ; dc can't clash with user aliases because they go through dart-global
                 :aliases {"dc" "dart:core"}
                 :ifn-mixins {}
                 'user ns-prototype}))

(defn global-lib-alias [lib ns]
  (or (-> @nses :libs (get lib) :dart-alias)
    (let [[_ trimmed-lib] (re-matches #"(?:package:)?(.+?)(?:\.dart)?" lib)
          segments (re-seq #"[a-zA-Z][a-zA-Z_0-9]*" trimmed-lib)
          prefix (apply str (map first (butlast segments)))
          base (cond->> (last segments) (not= prefix "") (str prefix "_"))
          nses (swap! nses
                 (fn [nses]
                   (if (-> nses :libs (get lib) :dart-alias)
                     nses
                     (let [alias (some #(when-not (get (:aliases nses) %) %)
                                   (cons base (map #(str base "_" (inc %)) (range))))]
                       (-> nses
                         (assoc-in [:libs lib] {:dart-alias alias :ns ns})
                         (assoc-in [:aliases alias] lib))))))]
      (-> nses :libs (get lib) :dart-alias))))

(declare resolve-type)

(defn- ensure-import-lib [lib-or-alias]
  (when lib-or-alias
    (let [{:keys [libs current-ns] :as all-nses} @nses
          {:keys [aliases]} (all-nses current-ns)]
      (if-some [lib (get aliases lib-or-alias)]
        (some-> lib libs :dart-alias (vector lib))
        (when-some [[_ dart-alias] (re-matches #"\$lib:(.*)" lib-or-alias)]
          (when-some [lib (get (:aliases all-nses) dart-alias)]
            (swap! nses assoc-in [current-ns :imports lib] {})
            [dart-alias lib]))))))

(defn- resolve-dart-type
  [sym type-vars]
  (let [{:keys [libs current-ns] :as nses} @nses
        {:keys [aliases]} (nses current-ns)]
    (else->>
      (if ('#{void dart:core/void} sym)
        {:type "void"
         :qname 'void})
      (if (contains? type-vars sym)
        {:type (name sym) :is-param true})
      (if-some [[dart-alias lib] (ensure-import-lib (namespace sym))]
        {:qname (symbol (str dart-alias "." (name sym)))
         :lib lib
         :type (name sym)
         :type-parameters
         (vec
           (for [t (:type-params (meta sym))]
             (or (resolve-type t type-vars)
               (throw (Exception. (str "Can't resolve type parameter " t " on type " sym "."))))))})
      nil)))

(defn- resolve-non-local-symbol [sym type-vars]
  (let [{:keys [libs] :as nses} @nses
        {:keys [mappings aliases] :as current-ns} (nses (:current-ns nses))]
    (else->>
      (if-some [v (current-ns sym)] [:def v])
      (if-some [v (mappings sym)]
        (recur (with-meta v (meta sym)) type-vars))
      (let [sym-ns (namespace sym)
            lib-ns (if (= "clojure.core" sym-ns)
                     "cljd.core"
                     (some-> (get aliases sym-ns) libs :ns name))])
      (if (some-> lib-ns (not= sym-ns))
        (recur (with-meta (symbol lib-ns (name sym)) (meta sym)) type-vars))
      (if-some [info (some-> sym-ns symbol nses (get (symbol (name sym))))]
        [:def info])
      (if-some [atype (resolve-dart-type sym type-vars)]
        [:dart atype]))))

(defn resolve-symbol
  "Returns either a pair [tag value] or nil when the symbol can't be resolved.
   tag can be :local, :def or :dart respectively indicating the symbol refers
   to a local, a global def (var-like) or a Dart global.
   The value depends on the tag:
   - for :local it's whatever is in the env,
   - for :def it's what's is in nses,
   - for :dart it's the aliased dart symbol."
  [sym env]
  (if-some [v (env sym)]
    [:local v]
    (resolve-non-local-symbol sym (:type-vars env))))

(defn dart-alias-for-ns [ns]
  (let [{:keys [current-ns] :as nses} @nses
        lib (get-in nses [ns :lib])]
    (-> nses :libs (get lib) :dart-alias)))

(defn resolve-type
  "Resolves a type to map with keys :qname :lib :type and :type-parameters."
  [sym type-vars]
  (when-some [[tag info] (resolve-non-local-symbol sym type-vars)]
    (case tag
      :dart info
      :def (case (:type info)
             :class
             {:qname (symbol (str (dart-alias-for-ns (:ns info))
                               "." (:dart/name info)))
              :lib (-> @nses (get (:ns info)) :lib)
              :type (name sym)
              :type-parameters [#_TODO]})
      (throw (ex-info (str "Can't resolve type " sym) {:type-vars type-vars})))))

(defn unresolve-type [{:keys [is-param lib type type-parameters nullable]}]
  (if is-param
    (symbol (cond-> type nullable (str "?")))
    (let [{:keys [current-ns] :as nses} @nses]
      (with-meta
        (symbol
          (when-not (= lib (:lib (nses current-ns)))
            (get-in nses [current-ns :imports lib :clj-alias]))
          (cond-> type nullable (str "?")))
        {:type-params (mapv unresolve-type type-parameters)}))))

(defn non-nullable [tag]
  (if-some [[_ base] (re-matches #"(.+)[?]" (name tag))]
    (cond->> base (symbol? tag) (symbol (namespace tag)))
    tag))

(defn emit-type
  [tag {:keys [type-vars] :as env}]
  (cond
    (= 'some tag) "dc.dynamic"
    ('#{void dart:core/void} tag) "void"
    :else
    (let [tag! (non-nullable tag)
          atype (or (resolve-type tag! type-vars) (when *hosted* (resolve-type (symbol (name tag!)) type-vars))
                  (throw (Exception. (str "Can't resolve type " tag! "."))))
          typename
          (if (:is-param atype) (:type atype) (name (:qname atype)))
          type-params (->> tag meta :type-params (map #(emit-type % env)))]
      (cond->
          (if (seq type-params)
            (case typename
              "dc.Function"
              (str (first type-params) " Function(" (str/join ", " (next type-params)) ")") ; TODO correct support of function and optionals (stop conflating type params and params types)
              (str typename "<" (str/join ", " type-params) ">"))
            typename)
        (not= tag tag!) (str "?")))))

(defn dart-type-truthiness [type]
  (case type
    (nil "dc.Object" "dc.dynamic") nil
    "dc.bool" :boolean
    :some))

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
      (:dart m) (assoc :dart/fn-type :native)
      (:clj m) (assoc :dart/fn-type :ifn)
      type (assoc :dart/type type :dart/truth (dart-type-truthiness type))
      (= tag 'some) (assoc :dart/truth :some))))

(def reserved-words ; and built-in identifiers for good measure
  #{"Function" "abstract" "as" "assert" "async" "await" "break" "case" "catch"
    "class" "const" "continue" "covariant" "default" "deferred" "do" "dynamic"
    "else" "enum" "export" "extends" "extension" "external" "factory" "false"
    "final" "finally" "for" "get" "hide" "if" "implements" "import" "in"
    "interface" "is" "library" "mixin" "new" "null" "on" "operator" "part"
    "rethrow" "return" "set" "show" "static" "super" "switch" "sync" "this"
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
   "?"    "$QMARK_"})

(defn munge
  ([sym env] (munge sym nil env))
  ([sym suffix env]
   (let [s (name sym)]
     (with-meta
       (or
         (-> sym meta :dart/name)
         (symbol
          (cond->
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
                        "_")))))
              suffix (str "$" suffix))))
       (dart-meta sym env)))))

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

(defn dart-local
  "Generates a unique (relative to the top-level being compiled) dart symbol.
   Hint is a string/symbol/keyword which gives a hint (duh) on how to name the
   dart symbol. Type tags when present are translated."
  ([env] (dart-local "" env))
  ([hint env]
   (let [dart-hint (munge hint env)
         {n dart-hint} (set! *locals-gen* (assoc *locals-gen* dart-hint (inc (*locals-gen* dart-hint 0))))
         {:dart/keys [type] :as dart-meta} (dart-meta hint env)
         dart-meta (cond-> dart-meta type (assoc :dart/nat-type type))] ; nat type of a local is its type
     (with-meta (symbol (str dart-hint "$" n)) dart-meta))))

(defn- parse-dart-params [params]
  (let [[fixed-params [delim & opt-params]] (split-with (complement '#{.& ...}) params)]
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
          (mapcat (fn [[t ext]] [(list 'dart/is? 'x t) ext]) (sort-by (fn [[x]] (case x Object 1 0)) (dissoc extensions 'fallback)))
          [:else (or ('fallback extensions) `(throw (dart:core/Exception. (.+ ~(str "No extension of protocol " name " found for type ") (.toString (.-runtimeType ~'x)) "."))))])))))

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
    (vector (into [] (map #(cond-> % (symbol? %) (vary-meta dissoc :tag))) args))))

(defn dart-member-lookup [class member]
  (when-some [class-info (get-in dart-libs-info [(:lib class) (:type class)])]
    (when-some [[type-env' member-info]
                (or
                  (when-some [member-info (class-info member)]
                    [identity member-info])
                  (some #(dart-member-lookup % member) (:mixins class-info))
                  (some #(dart-member-lookup % member) (:interfaces class-info))
                  (some-> (:super class-info) (dart-member-lookup member)))]
      (let [type-args (:type-parameters class)
            type-params (:type-parameters class-info)
            nargs (count type-args)
            nparams (count type-params)
            type-env (cond
                       (= nargs nparams)
                       (zipmap (map :name type-params) (:type-parameters class))
                       (zero? nargs)
                       (zipmap (map :name type-params)
                         (repeat (resolve-type 'dart:core/dynamic #{})))
                       :else
                       (throw (Exception. (str "Expecting " nparams " type arguments to " class ", got " nargs "."))))]
          [#(let [v (type-env' %)]
              (or (when (:is-param v)
                    (cond-> (type-env (:type v))
                      (:nullable v) (assoc :nullable true))) v))
           member-info]))))

(declare actual-parameters)

(defn actual-type [analyzer-type type-env]
  (case (:type analyzer-type)
    "Function"
    (-> analyzer-type
      (update :return-type actual-type type-env)
      (update :parameters actual-parameters type-env))
    (if (:is-param analyzer-type)
      (type-env analyzer-type)
      (update analyzer-type :type-parameters (fn [ps] (map #(actual-type % type-env) ps))))))

(defn actual-parameters [parameters type-env]
  (map #(update % :type actual-type type-env) parameters))

(defn actual-member [[type-env member-info]]
  (case (:kind member-info)
    :method
    (-> member-info
      (update :return-type actual-type type-env)
      (update :parameters actual-parameters type-env))
    :field
    (update member-info :type actual-type type-env)))

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
     (conj (into opts (map as-sym) optionals))]))

(defn- transfer-tag [actual decl]
  (let [m (meta actual)]
    (cond-> actual
      (nil? (:tag m))
      (vary-meta assoc :tag (:tag (meta decl))))))

(defn resolve-dart-method
  [type mname args type-env]
  (if-some [member-info (some-> (dart-member-lookup type (name mname)) actual-member)] ; TODO are there special cases for operators? eg unary-
    (case (:kind member-info)
      :field
      [(vary-meta mname assoc (case (count args) 1 :getter 2 :setter) true :tag (unresolve-type (:type member-info))) args]
      :method
      (let [[fixeds opts] (unresolve-params (:parameters member-info))
            {:as actual :keys [opt-kind] [this & fixed-opts] :fixed-params}
            (parse-dart-params args)
            _ (when-not (= (count fixeds) (count fixed-opts))
                (throw (Exception. (str "Fixed arity mismatch on " mname " for " (:type type) " of library " (:lib type)))))
            _ (when-not (case opt-kind :named (set? opts) (vector? opts))
                (throw (Exception. (str "Optional mismatch on " mname " for " (:type type) "of library " (:lib type)))))
            actual-fixeds
            (into [this] (map transfer-tag fixed-opts fixeds))
            actual-opts
            (case opt-kind
              :named
              (into []
                (mapcat (fn [[p d]] [(transfer-tag p (opts p)) d]))
                (:opt-params actual))
              :positional
              (mapv transfer-tag (:opt-params actual) opts))]
        [(vary-meta mname assoc :tag (unresolve-type (:return-type member-info)))
         (cond-> actual-fixeds
           (seq actual-opts)
           (-> (conj (case opt-kind :named '.& '...))
             (into actual-opts)))]))
    #_(TODO WARN)))

(defn- assoc-ns [sym ns]
  (with-meta (symbol ns (name sym)) (meta sym)))

(defn- expand-defprotocol [proto & methods]
  ;; TODO do something with docstrings
  (let [[doc-string & methods] (if (string? (first methods)) methods (list* nil methods))
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
        proto-map
        {:name proto
         :iface iface
         :iext iext
         :impl impl
         :sigs method-mapping}
        the-ns (name (:current-ns @nses))
        full-iface (assoc-ns iface the-ns)
        full-proto (assoc-ns proto the-ns)]
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
                           (. ~'~(vary-meta this assoc :tag full-iface) ~'~name ~~@args)
                           (. (.extensions ~'~full-proto ~'~this) ~'~name ~'~this ~~@args))))))}
             ~@(for [{:keys [dart/name] [this & args :as all-args] :args} (vals arity-mapping)]
                 `(~all-args
                   (if (dart/is? ~this ~iface)
                     (. ~(vary-meta this assoc :tag iface) ~name ~@args)
                     (. (.extensions ~proto ~this) ~name ~@all-args))))))
        (list proto)))))

(defn- expand-case [expr & clauses]
  (cond
    (not (symbol? expr))
    `(let* [test# ~expr] (~'case test# ~@clauses))
    (even? (count clauses))
    `(~'case ~expr ~@clauses (throw (.value dart:core/ArgumentError ~expr nil "No matching clause.")))
    :else
    (let [clauses (vec (partition-all 2 clauses))
          [default] (peek clauses)
          clauses (pop clauses)]
      (list 'case* expr (for [[v e] clauses] [(if (seq? v) v (list v)) e]) default))))

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
                    extension-base (munge* [(str type) (str protocol)]) ; str to get ns aliases if any
                    extension-name (dont-munge extension-base "cext")
                    extension-instance (dont-munge extension-base "extension")]]
          (list 'do
            (list* `deftype extension-name []
              :type-only true
              (:iext info)
              (for [[mname & body-or-bodies] meths
                    [[this & args] & body] (ensure-bodies body-or-bodies)
                    :let [mname (get-in info [:sigs mname (inc (count args)) :dart/name])]]
                `(~mname [~'_ ~this ~@args] (let* [~@(when-not (= type 'fallback)
                                                       [(vary-meta this assoc :tag type) this])] ~@body))))
            (list 'def extension-instance (list 'new extension-name))
            (list 'extend-type-protocol* type (:ns info) (:name info)
              (symbol (name (:current-ns @nses)) (name extension-instance)))))))))

(defn create-host-ns [ns-sym directives]
  (let [sym (symbol (str ns-sym "$host"))]
    (remove-ns sym)
    (binding [*ns* *ns*]
      (eval (list* 'ns sym directives))
      *ns*)))

(defn- propagate-hints [expansion form]
  (if-let [tag (and (not (identical? form expansion))
                 (or (seq? expansion) (symbol? expansion))
                 (:tag (meta form)))]
    (vary-meta expansion assoc :tag tag)
    expansion))

(defn inline-expand-1 [env form]
  (->
    (if-let [[f & args] (and (seq? form) (symbol? (first form)) form)]
      (let [f-name (name f)
            [f-type f-v] (resolve-symbol f env)
            {:keys [inline-arities inline]} (case f-type
                                              :def (:meta f-v)
                                              nil)]
        (cond
          (env f) form
          (and inline-arities (inline-arities (count args))) (apply inline args)
          :else form))
      form)
    (propagate-hints form)))

(defn static-member? [sym]
  (when-let [ns-sym (some-> sym namespace symbol)]
    (let [{:keys [current-ns] :as nses} @nses]
      (get-in nses [current-ns :mappings ns-sym]))))

(defn macroexpand-1 [env form]
  (->
    (if-let [[f & args] (and (seq? form) (symbol? (first form)) form)]
      (let [f-name (name f)
            [f-type f-v] (resolve-symbol f env)
            macro-fn (case f-type
                       :def (-> f-v :meta :macro-host-fn)
                       nil)]
        (cond
          (env f) form
          #?@(:clj ; macro overrides
              [(= 'ns f) form
               (= 'letfn f) form
               (= 'reify f)
               (let [[opts specs] (roll-leading-opts args)]
                 (list* 'reify* opts specs))
               (= 'defprotocol f) (apply expand-defprotocol args)
               ('#{case clojure.core/case cljd.core/case} f) (apply expand-case args)
               (= 'extend-type f) (apply expand-extend-type args)])
          (= '. f) form
          macro-fn
          (apply macro-fn form (assoc env :nses @nses) (next form))
          (.endsWith f-name ".")
          (with-meta
            (list* 'new
              (symbol (namespace f) (subs f-name 0 (dec (count f-name))))
              args)
            (meta form))
          (.startsWith f-name ".")
          (with-meta
            (list* '. (first args) (symbol (subs f-name 1)) (next args))
            (meta form))
          (static-member? f)
          (with-meta
            (list* '. (symbol (namespace f)) (symbol f-name) args)
            (meta form))
          :else form))
      form)
    (propagate-hints form)))

(defn macroexpand [env form]
  (let [ex (macroexpand-1 env form)]
    (cond->> ex (not (identical? ex form)) (recur env))))

(defn macroexpand-and-inline [env form]
  (let [ex (->> form (macroexpand-1 env) (inline-expand-1 env))]
    (cond->> ex (not (identical? ex form)) (recur env))))

(declare emit infer-type)

(defn atomic?
  [x] (not (coll? x)))

(defn has-recur?
  "Takes a dartsexp and returns true when it contains an open recur."
  [x]
  (some {'dart/recur true} (tree-seq seq? #(case (first %) (dart/loop dart/fn) nil %) x)))

(defn- dart-binding [hint dart-expr env]
  (let [hint-hint (when (symbol? hint) (infer-type (with-meta 'SHOULD_NOT_APPEAR_IN_DART_CODE (dart-meta hint env))))
        tmp (-> hint (dart-local env) (vary-meta merge (infer-type dart-expr) hint-hint))]
    [tmp dart-expr]))

(defn liftable
  "Takes a dartsexp and returns a [bindings expr] where expr is atomic
   or nil if there are no bindings to lift."
  [x env]
  (case (when (seq? x) (first x))
    dart/let
    (let [[_ bindings expr] x]
      (if-some [[bindings' expr] (liftable expr env)]
         [(concat bindings bindings') expr]
         (if (atomic? expr)
           [bindings expr]
           ;; this case should not happen
           (let [[tmp :as binding] (dart-binding "" expr env)]
             [(conj (vec bindings) binding) tmp]))))
    (dart/if dart/try dart/case dart/loop) ; no ternary for now
    (let [[tmp :as binding] (dart-binding (first x) x env)]
      [[binding]
       tmp])
    dart/assert
    [[nil x] nil]
    nil))

(defmacro ^:private with-lifted [[name expr] env wrapped-expr]
  `(let [~name ~expr]
     (if-some [[bindings# ~name] (liftable ~name ~env)]
       (list 'dart/let bindings# ~wrapped-expr)
       ~wrapped-expr)))

(defn- lift-arg [must-lift x hint env]
  (or (liftable x env)
      (cond
        (atomic? x) [nil x]
        must-lift
        (let [[tmp :as binding] (dart-binding hint x env)]
          [[binding] tmp])
        :else
        [nil x])))

(defn- split-args [args]
  (let [[positional-args [_ & named-args]] (split-with (complement '#{.&}) args)]
    [positional-args named-args]))

(defn emit-args
  "[bindings dart-args]"
  ([args env]
   (emit-args false args env))
  ([must-lift args env]
   (let [[positionals nameds] (split-args args)
         [bindings dart-args]
         (as-> [(when must-lift (list 'sentinel)) ()] acc
           (reduce (fn [[bindings dart-fn-args] [k x]]
                     (let [[bindings' x'] (lift-arg (seq bindings) (emit x env) k env)]
                       [(concat bindings' bindings) (list* k x' dart-fn-args)]))
             acc (reverse (partition 2 nameds)))
           (reduce (fn [[bindings dart-fn-args] x]
                     (let [[bindings' x'] (lift-arg (seq bindings) (emit x env) "arg" env)
                           {:dart/keys [type nat-type]} (infer-type x')
                           here-type (or (some-> x meta :tag (emit-type env)) type)
                           x' (if (and here-type (not= here-type nat-type))
                                (list 'dart/as x' here-type)
                                x')]
                       [(concat bindings' bindings) (cons x' dart-fn-args)]))
             acc (reverse positionals)))
         bindings (cond-> bindings must-lift butlast)]
     [bindings dart-args])))

(def ^:dynamic *threshold* 10)

(defn emit-fn-call [[f & args] env]
  (let [dart-f (emit f env)
        fn-type (if (some '#{.&} args) :native (:dart/fn-type (meta dart-f)))
        [bindings dart-args] (emit-args (nil? fn-type) args env)
        [bindings' dart-f] (lift-arg (seq bindings) dart-f "f" env)
        bindings (concat bindings' bindings)
        native-call (cons dart-f dart-args)
        ifn-call (let [dart-f (if fn-type
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
                         (subvec (vec dart-args) (dec *threshold*))))))
        dart-fn-call
        (case fn-type
          :native native-call
          :ifn ifn-call
          (list 'dart/if (list 'dart/is dart-f "dc.Function")
            (cons (list 'dart/as dart-f 'dc.Function) dart-args)
            (list 'dart/if (list 'dart/is dart-f (emit-type 'cljd.core/IFn$iface {}))
              ifn-call
              (let [[meth & args] (nnext ifn-call)] ; "callables" must be handled by an extesion on dynamic or Object
                (list* 'dart/. (list 'dart/. (emit 'cljd.core/IFn env) 'extensions dart-f) meth (cons dart-f args))))))]
    (cond->> dart-fn-call
      (seq bindings) (list 'dart/let bindings))))

(defn emit-coll
  ([coll env] (emit-coll emit coll env))
  ([maybe-quoted-emit coll env]
   (if (seq coll)
     (let [items (into [] (if (map? coll) cat identity) coll)
           [bindings items]
           (reduce (fn [[bindings fn-call] x]
                     (let [[bindings' x'] (lift-arg (seq bindings) (maybe-quoted-emit x env) "item" env)]
                       [(concat bindings' bindings) (cons x' fn-call)]))
             [nil ()] (rseq items))
           fn-sym (cond
                    (map? coll) 'cljd.core/-map-lit
                    (vector? coll) 'cljd.core/vec
                    (set? coll) 'cljd.core/set
                    (seq? coll) 'cljd.core/-list-lit
                    :else (throw (ex-info (str "Can't emit collection " (pr-str coll)) {:form coll})))
           fn-call (list (emit fn-sym env) (vec items))]
       (cond->> fn-call (seq bindings) (list 'dart/let bindings)))
     (emit
       (cond
         (map? coll) 'cljd.core/-EMPTY-MAP
         (vector? coll) 'cljd.core/-EMPTY-VECTOR
         (set? coll) 'cljd.core/-EMPTY-SET
         (seq? coll) 'cljd.core/-EMPTY-LIST ; should we use apply list?
         :else (throw (ex-info (str "Can't emit collection " (pr-str coll)) {:form coll})))
       env))))

(defn emit-dart-literal [x env]
  (cond
    (not (vector? x)) (throw (ex-info (str "Unsupported dart literal #dart " (pr-str x)) {:form x}))
    (:fixed (meta x))
    (let [item-tag (:tag (meta x) 'dart:core/dynamic)
          list-tag (vary-meta 'dart:core/List assoc :type-params [item-tag])]
      (->
        (if-some [[item] (seq x)]
          (let [lsym `fl#]
            `(let [~lsym (. ~list-tag filled ~(count x) ~item)]
               ~@(map-indexed (fn [i x] `(. ~lsym "[]=" ~(inc i) ~x)) (next x))
               ~lsym))
          `(.empty ~list-tag))
        (vary-meta assoc :tag list-tag)
        (emit env)))
    :else
    (let [[bindings items]
          (reduce (fn [[bindings fn-call] x]
                    (let [[bindings' x'] (lift-arg (seq bindings) (emit x env) "item" env)]
                      [(concat bindings' bindings) (cons x' fn-call)]))
            [nil ()] (rseq x))]
     (cond->> (vec items) (seq bindings) (list 'dart/let bindings)))))

(defn emit-new [[_ class & args] env]
  (let [dart-type (emit-type class env)
        [bindings dart-args] (emit-args args env)]
    (cond->> (list* 'dart/new dart-type dart-args)
      (seq bindings) (list 'dart/let bindings))))

(def -interops (atom {}))

(defn emit-dot [[_ obj member & args :as form] env]
  (if (seq? member)
    (recur (list* '. obj member) env)
    (let [member (name member)
          [_ prop name] (re-matches #"(-)?(.+)" member)
          prop (and prop (nil? args))
          op (if prop 'dart/.- 'dart/.)
          [bindings [dart-obj & dart-args]] (emit-args (cons obj args) env)
          {:dart/keys [type nat-type]} (infer-type dart-obj)
          type (some-> type non-nullable)
          dart-obj (cond
                     ; static only
                     (:type-params (meta obj)) (symbol (emit-type obj env)) ; this symbol is not readable
                     ; non static
                     (not= type nat-type)
                     (list 'dart/as dart-obj type)
                     :else dart-obj)
          ;; TODO SELFHOST with mirrors one can check membership
          #_#_dart-obj (if (not= type nat-type)
                     (list 'dart/as dart-obj type)
                     (do
                       ;; Can't do it properly in crosscompilation
                       #_(when (or (nil? type) (= type "dc.dynamic")) ; TODO refine with mirrors and add *warn-on-dynamic*
                         (binding [*out* *err*]
                           ; TODO add file and line info (preserve meta through macroexpansion)
                           (println "Dynamic invocation warning. Reference to" (if prop "field" "method")
                             member "can't be resolved." obj)))
                       dart-obj))]
      (swap! -interops update-in [type member] (fnil conj #{}) (:line (meta form)))
      (cond->> (list* op dart-obj name dart-args)
        (seq bindings) (list 'dart/let bindings)))))

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
                 (throw (ex-info (str "Cannot assign: " target) {:target target}))))
        (list 'dart/let
          [[nil (list 'dart/set! dart-sym (emit expr env))]]
          dart-sym))
      (and (seq? target) (= '. (first target)))
      (let [[_ obj member] target]
        (if-some [[_ fld] (re-matches #"-(.+)" (name member))]
          (let [[bindings [dart-obj dart-val]] (emit-args true [obj expr] env)]
            (list 'dart/let
              (conj (vec bindings)
                [nil (list 'dart/set! (list 'dart/.- dart-obj fld) dart-val)])
              dart-val))
          (throw (ex-info (str "Cannot assign to a non-property: " member ", make sure the property name is prefixed by a dash.")
                          {:target target}))))
      :else
      (throw (ex-info (str "Unsupported target for assignment: " target) {:target target})))))

(defn emit-let* [[_ bindings & body] env]
  (let [[dart-bindings env]
        (reduce
         (fn [[dart-bindings env] [k v]]
           (let [[tmp :as binding] (dart-binding k (emit v env) env)]
             [(conj dart-bindings binding)
              (assoc env k tmp)]))
         [[] env] (partition 2 bindings))
        dart-bindings
        (into dart-bindings (for [x (butlast body)] [nil (emit x env)]))]
    (cond->> (emit (last body) env)
      ; wrap only when ther are actual bindings
      (seq dart-bindings) (list 'dart/let dart-bindings))))

(defn emit-do [[_ & body] env]
  (emit (list* 'let* [] body) env))

(defn emit-loop* [[_ bindings & body] env]
  (let [[dart-bindings env]
        (reduce
         (fn [[dart-bindings env] [k v]]
           (let [dart-v (emit v env)
                 {:dart/keys [type]} (infer-type dart-v)
                 decl (dart-local k env)
                 usage (if (-> decl meta :dart/type)
                         decl
                         (vary-meta decl merge (select-keys (infer-type dart-v) [:dart/type :dart/truth])))
                 decl (if (-> decl meta :dart/type) decl (vary-meta decl assoc :dart/type "dc.dynamic"))]
             [(conj dart-bindings [decl dart-v])
              (assoc env k usage)]))
         [[] env] (partition 2 bindings))]
    (list 'dart/loop dart-bindings (emit (list* 'let* [] body) env))))

(defn emit-recur [[_ & exprs] env]
  (cons 'dart/recur (map #(emit % env) exprs)))

(defn emit-if [[_ test then else] env]
  (cond
    (or (coll? test) (symbol? test))
    (let [dart-test (emit test env)
          {:keys [dart/truth]} (infer-type dart-test)
          [bindings test] (lift-arg (nil? truth) dart-test "test" env)
          test (case truth
                 :boolean test
                 :some (list 'dart/. test "!=" nil)
                 (list 'dart/. (list 'dart/. test "!=" false) "&&" (list 'dart/. test "!=" nil)))]
      (cond->> (list 'dart/if test (emit then env) (emit else env))
        (seq bindings) (list 'dart/let bindings)))
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
      (char? x) (hash (str x))
      (symbol? x) (cljd-hash-combine
                    (clojure.lang.Murmur3/hashUnencodedChars (name x))
                    (hash (namespace x)))
      (keyword? x)
      (cljd-hash-combine
        (or (some-> x namespace cljd-hash) 0)
        (cljd-hash (name x)))
      :else (throw (ex-info (str "cljd-hash not implemented for " (class x)) {:x x})))))

(defn emit-case* [[op expr clauses default] env]
  (assert (and (symbol? expr) (env expr)))
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
            [[h]
             (reduce (fn [else [v e]]
                       ; TODO constant extraction
                       (list 'dart/if (emit (list 'cljd.core/= (list 'quote v) expr) env)
                         (emit e env)
                         else))
               '(dart/continue _default) (rseq groups))])
          (emit default env))))))

(defn- variadic? [[params]] (some #{'&} params))

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
        (let [[this & call-args] (cond->> synth-params (not base-vararg-arity) (take (inc max-fixed-arity)))
              fixed-args (cond->> call-args base-vararg-arity (take base-vararg-arity))
              base-arity (or min-fixed-arity base-vararg-arity)
              base-args (take base-arity call-args)
              opt-args (drop base-arity call-args)
              default-value (str (gensym "default"))
              fixed-arities-expr
              (for [args+1 (next (reductions conj (vec base-args)
                                             (cond->> opt-args base-vararg-arity (take (- base-vararg-arity base-arity)))))]
                [args+1 `(. ~this ~(resolve-protocol-mname-to-dart-mname 'cljd.core/IFn '-invoke (count args+1) #{}) ~@(pop args+1))])]
          `((~'call [~this ~@base-args ... ~@(interleave opt-args (repeat default-value))]
             (cond
               ~@(mapcat (fn [[args+1 expr]] `((.== ~(peek args+1) ~default-value) ~expr)) fixed-arities-expr)
               true ~(if base-vararg-arity
                       (if-some [[first-rest-arg :as rest-args] (seq (drop base-vararg-arity call-args))]
                         `(if (.== ~first-rest-arg ~default-value)
                            (. ~this ~(resolve-protocol-mname-to-dart-mname 'cljd.core/IFn '-invoke (inc (count fixed-args)) #{})
                               ~@fixed-args)
                            (. ~this ~vararg-mname ~@fixed-args
                              (seq (.toList
                                     (.takeWhile ~(tagged-literal 'dart (vec rest-args))
                                       (fn [e#] (.!= e# ~default-value)))))))
                         `(. ~this ~vararg-mname ~@fixed-args nil))
                       `(. ~this ~(resolve-protocol-mname-to-dart-mname 'cljd.core/IFn '-invoke (inc (count fixed-args)) #{})
                               ~@fixed-args))))
            (~'-apply [~this ~more-param]
             (let [~more-param (seq ~more-param)]
               ~(reduce (fn [body [args+1 expr]]
                          `(if (nil? ~more-param)
                             ~expr
                             (let* [~(peek args+1) (first ~more-param)
                                    ~more-param (next ~more-param)]
                               ~body)))
                        `(if (nil? ~more-param)
                           (. ~this ~(resolve-protocol-mname-to-dart-mname 'cljd.core/IFn '-invoke (inc (count fixed-args)) #{})
                              ~@fixed-args)
                           ~(if base-vararg-arity
                              `(. ~this ~vararg-mname ~@fixed-args ~more-param)
                              `(throw (dart:core/ArgumentError. "No matching arity"))))
                        (reverse
                         (concat
                          (for [args+1 (next (reductions conj [] base-args))]
                            [args+1 `(throw (dart:core/ArgumentError. "No matching arity"))])
                          fixed-arities-expr)))))))
        [{:keys [current-ns]}] (swap-vals! nses assoc :current-ns 'cljd.core)]
    (emit
      `(deftype ~(vary-meta (dont-munge mixin-name nil) assoc :abstract true) []
         cljd.core/IFn
         ~@fixed-invokes
         ~@invoke-exts
         ~@vararg-invokes
         ~@(some-> invoke-more list)
         ~@call+apply)
      {})
    (swap! nses assoc :current-ns current-ns)
    mixin-name))

(defn- ensure-ifn-arities-mixin [fixed-arities base-vararg-arity]
  (let [fixed-arities (set fixed-arities)
        path [:ifn-mixins fixed-arities base-vararg-arity]]
    (or (get-in @nses path)
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
            `(let [~@(mapcat (fn [p] (when (:tag (meta p)) [p (vary-meta p dissoc :tag)])) params)]
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

(defn- dart-fn-param
  "Like dart-local but with no natural type as dart fns params must be dynamic."
  ([env] (dart-fn-param "" env))
  ([hint env]
   (vary-meta (dart-local hint env) dissoc :dart/nat-type)))

(defn- emit-dart-fn [async fn-name [params & body] env]
  (let [ret-type (some-> (or (:tag (meta fn-name)) (:tag (meta params))) (emit-type env))
        {:keys [fixed-params opt-kind opt-params]} (parse-dart-params params)
        dart-fixed-params (map #(dart-fn-param % env) fixed-params)
        dart-opt-params (for [[p d] opt-params]
                          [(case opt-kind
                             :named p ; here p must be a valid dart identifier
                             :positional (dart-fn-param p env))
                           (emit d env)])
        env (into env (zipmap (concat fixed-params (map first opt-params))
                              (concat dart-fixed-params (map first dart-opt-params))))
        dart-body (emit (cons 'do body) env)
        recur-params (when (has-recur? dart-body) dart-fixed-params)
        dart-fixed-params (if recur-params
                            (map #(dart-fn-param % env) fixed-params) ; regen new fixed params
                            dart-fixed-params)
        dart-body (cond->> dart-body
                    recur-params
                    (list 'dart/loop (map vector recur-params dart-fixed-params)))
        dart-body (if ret-type
                    (with-lifted [dart-expr dart-body] env
                      (list 'dart/as dart-expr ret-type))
                    dart-body)
        dart-fn
        (list 'dart/fn dart-fixed-params opt-kind dart-opt-params async dart-body)]
    (if-some [dart-fn-name (some-> fn-name env (vary-meta assoc :dart/ret-type ret-type))]
      (list 'dart/let [[dart-fn-name dart-fn]] dart-fn-name)
      dart-fn)))

(defn emit-fn* [[fn* & bodies :as form] env]
  (let [{:keys [async]} (meta form)
        var-name (some-> fn* meta :var-name)
        name (when (symbol? (first bodies)) (first bodies))
        bodies (cond-> bodies name next)
        [body & more-bodies :as bodies] (ensure-bodies bodies)
        fn-type (if (or more-bodies (variadic? body)) :ifn :native)
        env (cond-> env name (assoc name (vary-meta (dart-local name env) assoc :dart/fn-type fn-type)))]
    (case fn-type
      :ifn (emit-ifn async var-name name bodies env)
      (emit-dart-fn async name body env))))

(defn closed-overs
  "Returns the set of dart locals (values of the env) referenced in the emitted code."
  [emitted env]
  (into #{} (keep (set (vals env))) (tree-seq coll? seq emitted)))

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
    (list 'dart/letrec bindings wirings (emit (list* 'let* [] body) env))))

(defn emit-method [[mname {[this-param & fixed-params] :fixed-params :keys [opt-kind opt-params]} & body] env]
  ;; params destructuring will be added by a macro
  ;; opt-params need to have been fully expanded to a list of [symbol default]
  ;; by the macro
  (let [mtype-params (:type-params (meta mname))
        env (assoc env :type-vars
              (into (:type-vars env #{}) mtype-params))
        dart-fixed-params (map #(dart-local % env) fixed-params)
        dart-opt-params (for [[p d] opt-params]
                          [(case opt-kind
                             :named p ; here p must be a valid dart identifier
                             :positional (dart-local p env))
                           (emit d env)])
        super-param (:super (meta this-param))
        env (into (cond-> (assoc env this-param (with-meta 'this (dart-meta this-param env)))
                    super-param (assoc super-param 'super))
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
        mname (with-meta mname (dart-meta mname env))]
    [mname mtype-params dart-fixed-params opt-kind dart-opt-params (nil? (seq body)) dart-body]))

(defn extract-super-calls [dart-body this-super]
  (let [dart-fns (atom [])
        dart-body
        (clojure.walk/prewalk
          (fn [x]
            (or
              (case (when (seq? x) (first x))
                (dart/. dart/.-)
                (when (= (second x) this-super)
                  (let [args (drop 3 x)
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
                    (swap! dart-fns conj [dart-fn-name dart-fn])
                    (cond->> (cons dart-fn-name params)
                      (seq bindings) (list 'dart/let bindings))))
                dart/set!
                (when-some [fld (when-some [[op o fld] (when (seq? (second x)) (second x))]
                                  (when (and (= 'dart/.- op) (= o this-super))
                                    fld))]
                  (let [dart-fn-name (dart-local (with-meta (symbol (str "super-set-" fld)) {:dart true}) {})
                        dart-fn (list 'dart/fn '[v] :positional () false
                                  (list 'dart/set! (list 'dart/.- 'super fld) 'v))]
                    (swap! dart-fns conj [dart-fn-name dart-fn])
                    (list dart-fn-name (nth x 2))))
                nil)
              (when (= this-super x) (throw (Exception. "Rogue reference to super.")))
              x))
          dart-body)]
    [@dart-fns dart-body]))

(defn method-extract-super-call [method this-super]
  (let [[bindings dart-body] (extract-super-calls (peek method) this-super)]
    [bindings (conj (pop method) dart-body)]))

(declare write-class)

(defn do-def [nses sym m]
  (let [the-ns (:current-ns nses)
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
  (let [the-ns (:current-ns nses)]
    (apply update-in nses [the-ns sym] f args)))

(defn- resolve-methods-specs [specs type-env]
  (let [last-seen-type (atom nil)]
    (map
      (fn [spec]
        (cond
          (seq? spec)
          (let [[mname arglist & body] spec
                mname (cond-> mname (string? mname) symbol) ; this will make unreadable symbols for some operators thus beware of weird printouts.
                [mname arglist']
                (case (:type @last-seen-type)
                  :protocol (some-> @last-seen-type (resolve-protocol-method mname arglist type-env))
                  (or (some-> @last-seen-type (resolve-dart-method mname arglist type-env))
                    [mname arglist]))]
            `(~mname ~(parse-dart-params arglist')
              (let [~@(mapcat (fn [a a']
                                (when-some [t (:tag (meta a))]
                                  (when-not (= t (:tag (meta a')))
                                    [a a']))) arglist arglist')]
                ~@body)))
          (:mixin (meta spec)) (do (reset! last-seen-type (resolve-type spec type-env)) spec)
          :else
          (let [[tag x] (resolve-non-local-symbol spec type-env)]
            (reset! last-seen-type x)
            (case tag
              :def (case (:type x)
                     :class spec
                     :protocol (symbol (name (:ns x)) (name (:iface x))))
              :dart spec
              (throw (Exception. (str "Can't resolve " spec)))))))
      specs)))

(defn- emit-class-specs [opts specs env]
  (let [{:keys [extends] :or {extends 'Object}} opts
        specs (resolve-methods-specs specs (:type-vars env #{}))
        [ctor-op base & ctor-args :as ctor]
        (macroexpand env (cond->> extends (symbol? extends) (list 'new)))
        ctor-meth (when (= '. ctor-op) (first ctor-args))
        ctor-args (cond-> ctor-args (= '. ctor-op) next)
        classes (filter #(and (symbol? %) (not= base %)) specs) ; crude
        methods (remove symbol? specs)  ; crude
        mixins (filter (comp :mixin meta) classes)
        ifaces (remove (comp :mixin meta) classes)
        need-nsm (and (seq ifaces) (not-any? (fn [[m]] (case m noSuchMethod true nil)) methods))
        dart-methods (map #(emit-method % env) methods)]
    {:extends (emit-type base env)
     :implements (map #(emit-type % env) ifaces)
     :with (map #(emit-type % env) mixins)
     :super-ctor
     {:method ctor-meth ; nil for new
      :args ctor-args}
     :methods dart-methods
     :nsm need-nsm}))

(defn method-closed-overs [[mname type-params dart-fixed-params opt-kind dart-opt-params _ dart-body] env]
  (reduce disj (closed-overs dart-body env) (list* 'this 'super (concat dart-fixed-params (map second dart-opt-params)))))

(defn emit-reify* [[_ opts & specs] env]
  (let [this-this (dart-local "this" env)
        this-super (dart-local "super" env)
        env (into env (keep (fn [[k v]] (case v this [k this-this] super [k this-super] nil))) env)
        class (emit-class-specs opts specs env)
        [positional-ctor-args named-ctor-args] (-> class :super-ctor :args split-args)
        positional-ctor-params (repeatedly (count positional-ctor-args) #(dart-local "param" env))
        named-ctor-params (map #(dart-local % env) (take-nth 2 named-ctor-args))
        class-name (if-some [var-name (:var-name opts)]
                       (munge var-name "ifn" env)
                       (dart-global (or (:name-hint opts) "Reify")))
        mclass-name (vary-meta class-name assoc :type-params (:type-vars env))
        [super-fn-bindings methods]
        (reduce (fn [[bindings meths] meth]
                  (let [[bindings' meth'] (method-extract-super-call meth this-super)]
                    [(into bindings bindings') (conj meths meth')]))
          [[] []] (:methods class))
        closed-overs (concat
                       (transduce (map #(method-closed-overs % env)) into #{}
                         methods)
                       (map first super-fn-bindings))
        env-for-ctor-call
        (-> env
          (into (map (fn [v] [v v])) closed-overs)
          (into (map (fn [[f]] [f f])) super-fn-bindings)
          (assoc this-this 'this))
        no-meta (or (:no-meta opts) (seq positional-ctor-args) (seq named-ctor-args))
        meta-field (when-not no-meta (dart-local 'meta env))
        {meta-methods :methods meta-implements :implements}
        (when meta-field
          (emit-class-specs {}
            `[cljd.core/IMeta
              (~'-meta [_#] ~meta-field)
              cljd.core/IWithMeta
              (~'-with-meta [_# m#] (new ~mclass-name m# ~@closed-overs))]
              (assoc env-for-ctor-call meta-field meta-field)))
        _ (swap! nses do-def class-name {:dart/name mclass-name :type :class})
        class (-> class
                (assoc
                  :name (emit-type mclass-name env)
                  :ctor class-name
                  :fields (cond-> closed-overs meta-field (conj meta-field))
                  :methods (concat meta-methods methods)
                  :implements (concat meta-implements (:implements class))
                  :ctor-params
                  (concat
                    (map #(list '. %) (cond->> closed-overs meta-field (cons meta-field)))
                    positional-ctor-params
                    named-ctor-params))
                (assoc-in [:super-ctor :args]
                  (concat positional-ctor-params
                    (interleave (take-nth 2 named-ctor-args)
                      named-ctor-params))))
        reify-ctor-call (list*
                         'new class-name
                         (concat (cond->> closed-overs meta-field (cons nil))
                           positional-ctor-args
                           (take-nth 2 (next named-ctor-args))))]
    (swap! nses do-def class-name
           {:dart/name class-name
            :type :class
            :dart/code (with-out-str (write-class class))})
    (cond->> (emit reify-ctor-call env-for-ctor-call)
      (seq super-fn-bindings)
      (list 'dart/let super-fn-bindings))))

(defn- ensure-dart-expr
  "If dart-expr is suitable as an expression (ie liftable returns nil),
   its emission is returned as is, otherwise a IIFE (thunk invocation) is returned."
  [dart-expr env]
  (if-some [[bindings dart-expr] (liftable dart-expr env)]
    (list (list 'dart/fn () :positional () false (list 'dart/let bindings dart-expr)))
    dart-expr))

(declare write-top-dartfn write-top-field write-dynamic-var-top-field)

(defn emit-defprotocol* [[_ pname spec] env]
  (let [dartname (munge pname env)]
    (swap! nses do-def pname
      (assoc spec
        :dart/name dartname
        :dart/code (with-out-str (write-top-field dartname (emit (list 'new (:impl spec)) {})))
        :type :protocol))))

(defn emit-deftype* [[_ class-name fields opts & specs] env]
  (let [abstract (:abstract (meta class-name))
        [class-name & type-params] (cons class-name (:type-params (meta class-name)))
        mclass-name (with-meta
                      (or (:dart/name (meta class-name)) (munge class-name env))
                      {:type-params type-params}) ; TODO shouldn't it be dne by munge?
        env {:type-vars (set type-params)}
        env (into env
              (for [f fields
                    :let [{:keys [mutable]} (meta f)
                          {:keys [dart/type] :as m} (dart-meta f env)
                          m (cond-> m
                              mutable (assoc :dart/mutable true)
                              type (assoc :dart/nat-type type))]]
                [f (vary-meta (munge f env) merge m)]))
        dart-fields (map env fields)
        _ (swap! nses do-def class-name {:dart/name mclass-name :type :class})
        class (emit-class-specs opts specs env)
        [positional-ctor-args named-ctor-args] (-> class :super-ctor :args split-args)
        class (-> class
                (assoc :name (emit-type mclass-name env)
                  :ctor mclass-name
                  :abstract abstract
                  :fields dart-fields
                  :ctor-params (map #(list '. %) dart-fields))
                (assoc-in [:super-ctor :args]
                  (concat (map #(-> % (emit env) (ensure-dart-expr env)) positional-ctor-args)
                    (->> named-ctor-args (partition 2)
                      (mapcat (fn [[name arg]] [name (-> arg (emit env) (ensure-dart-expr env))]))))))]
    (swap! nses alter-def class-name assoc :dart/code (with-out-str (write-class class)))
    (emit class-name env)))

(defn emit-extend-type-protocol* [[_ class-name protocol-ns protocol-name extension-instance] env]
  (let [{:keys [current-ns] {proto-map protocol-name} protocol-ns}
        (swap! nses assoc-in [protocol-ns protocol-name :extensions class-name] extension-instance)]
    (swap! nses assoc :current-ns protocol-ns)
    (emit (expand-protocol-impl proto-map) env)
    (swap! nses assoc :current-ns current-ns)))

(defn- fn-kind [x]
  (when (and (seq? x) (= 'fn* (first x)))
    (let [[maybe-name :as body] (next x)
          [arglist-or-body :as body] (cond-> body (symbol? maybe-name) next)
          [[args] & more-bodies] (cond-> body (vector? arglist-or-body) list)]
      (if (or more-bodies (some '#{&} args)) :clj :dart))))

(defn emit-def [[_ sym & doc-string?+expr] env]
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
        kind (fn-kind expr)
        sym (cond-> sym
              kind (vary-meta assoc kind true)
              (:macro (meta sym)) (vary-meta assoc :macro-host-fn expr)
              (:macro-support (meta sym)) (vary-meta assoc :bootstrap-def (list 'def sym expr)))
        expr (when-not *host-eval*
               (if (and (seq? expr) (= 'fn* (first expr)))
                 (with-meta (cons (vary-meta (first expr) assoc :var-name sym) (next expr)) (meta expr))
                 expr))
        dartname (cond-> (munge sym env) kind (vary-meta (fn [{:dart/keys [type truth] :as m}]
                                                           (-> m (dissoc :dart/type :dart/nat-type :dart/truth)
                                                             (assoc :dart/ret-type type :dart/ret-truth truth)))))
        dart-fn
        (do
          (swap! nses do-def sym {:dart/name dartname :type :field}) ; predecl so that the def is visible in recursive defs
          (emit expr env))
        dart-code
        (when-not *host-eval*
          (with-out-str
            (cond
              (:dynamic (meta sym))
              (let [k (symbol (name (:current-ns @nses)) (name sym))]
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
              (write-top-field dartname dart-fn))))]
    (swap! nses alter-def sym assoc :dart/code dart-code)
    (emit sym env)))

(defn ensure-import-ns [the-ns]
  (let [{:keys [current-ns] :as all-nses} @nses
        the-lib (:lib (all-nses the-ns))
        dart-alias (some-> all-nses :libs (get the-lib) :dart-alias)]
    (when-not dart-alias
      (throw (ex-info (str "Namespace not required: " the-ns) {:ns the-ns})))
    (when-not (some-> current-ns all-nses :imports (get the-lib))
      (swap! nses assoc-in [current-ns :imports the-lib] {}))
    dart-alias))

(defn emit-symbol [x env]
  (let [[tag v] (resolve-symbol x env)]
    (case tag
      :local v
      :def
      (let [{dart-name :dart/name the-ns :ns} v]
          (if (= (:current-ns @nses) the-ns)
            dart-name
            (with-meta (symbol (str (ensure-import-ns the-ns) "." dart-name))
              (meta dart-name))))
      :dart (vary-meta (:qname v) assoc :dart/fn-type :native)
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
  (let [{:keys [exclude only rename]} (reduce (fn [m [k v]] (merge-with into m {k v})) {} (partition 2 refer-spec))
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
          refer-clojures (or (seq (filter #(= :refer-clojure (first %)) ns-clauses)) [[:refer-clojure]])
          host-ns-directives (some #(when (= :host-ns (first %)) (next %)) ns-clauses)
          require-specs
          (concat
            (map refer-clojure-to-require refer-clojures)
            (for [[directive & specs]
                  ns-clauses
                  :let [f (case directive
                            :require #(if (sequential? %) % [%])
                            :import import-to-require
                            :use use-to-require
                            (:refer-clojure :host-ns) nil)]
                  :when f
                  spec specs]
              (f spec)))
          ns-lib (ns-to-lib ns-sym)
          ns-map (-> ns-prototype
                   (assoc :lib ns-lib)
                   (assoc-in [:imports ns-lib] {:clj-alias ns-sym}))
          ns-map
          (reduce #(%2 %1) ns-map
            (for [[lib & {:keys [as refer rename]}] require-specs
                  :let [clj-ns (when-not (string? lib) lib)
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
                  (assoc-in [:aliases clj-alias] dartlib)
                  (update :mappings into
                    (for [to refer :let [from (get rename to to)]]
                      [from (with-meta (symbol clj-alias (name to))
                              {:dart (nil? clj-ns)})]))))))
          host-ns-directives
          (concat
            host-ns-directives
            (for [[lib & {:keys [refer as] :as options}] require-specs
                  :let [host-ns (some-> (get @nses lib) :host-ns ns-name)]
                  :when (and host-ns (not= ns-sym lib))
                  :let [options
                        (assoc options :refer (filter (comp :macro-support :meta (@nses 'cljd.core)) refer))]]
              (list :require (into [host-ns] cat options))))
          ns-map (cond-> ns-map
                   *host-eval* (assoc :host-ns (create-host-ns ns-sym host-ns-directives)))]
      (global-lib-alias ns-lib ns-sym)
      (swap! nses assoc ns-sym ns-map :current-ns ns-sym))))

(defn- emit-no-recur [expr env]
  (let [dart-expr (emit expr env)]
    (when (has-recur? dart-expr)
      (throw (ex-info "Cannot recur across try." {:expr expr})))
    dart-expr))

(defn emit-try [[_ & body] env]
  (let [{body nil catches 'catch [[_ & finally-body]] 'finally}
        (group-by #(when (seq? %) (#{'finally 'catch} (first %))) body)]
    (list 'dart/try
           (emit-no-recur (cons 'do body) env)
           (for [[_ classname e & [maybe-st & exprs :as body]] catches
                 :let [st (when (and exprs (symbol? maybe-st)) maybe-st)
                       exprs (if st exprs body)
                       env (cond-> (assoc env e (dart-local e env))
                             st (assoc st (dart-local st env)))]]
             [(emit-type classname env) (env e) (some-> st env) (emit-no-recur (cons 'do exprs) env)])
           (some-> finally-body (conj 'do) (emit-no-recur env)))))

(defn emit-throw [[_ expr] env]
  ;; always emit throw as a statement (in case it gets promoted to rethrow)
  (list 'dart/let [[nil (list 'dart/throw (emit expr env))]] nil))

(defn emit-dart-is [[_ x type] env]
  #_(when (or (not (symbol? type)) (env type))
    (throw (ex-info (str "The second argument to dart/is? must be a literal type. Got: " (pr-str type)) {:type type})))
  (with-lifted [x (emit x env)] env
    (list 'dart/is x (emit-type type env))))

(defn emit-dart-assert [[_ test msg] env]
  (list 'dart/assert (ensure-dart-expr (emit test env) env)
    (ensure-dart-expr (emit msg env) env)))

(defn emit-dart-await [[_ x] env]
  (with-lifted [x (emit x env)] env
    (list 'dart/await x)))

(defn emit
  "Takes a clojure form and a lexical environment and returns a dartsexp."
  [x env]
  (let [x (macroexpand-and-inline env x)
        dart-x
        (cond
          (symbol? x) (emit-symbol x env)
          #?@(:clj [(char? x) (str x)])
          (or (number? x) (boolean? x) (string? x)) x
          (instance? java.util.regex.Pattern x)
          (emit (list 'new 'dart:core/RegExp (.pattern ^java.util.regex.Pattern x) '.& :unicode true) env)
          (keyword? x)
          (emit (with-meta (list 'cljd.core/Keyword. (namespace x) (name x) (cljd-hash x)) {:const true}) env)
          (nil? x) nil
          (and (seq? x) (seq x)) ; non-empty seqs only
          (let [emit (case (first x)
                       . emit-dot
                       set! emit-set!
                       dart/is? emit-dart-is
                       dart/await emit-dart-await
                       dart/assert emit-dart-assert
                       throw emit-throw
                       new emit-new
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
                       emit-fn-call)]
            (binding [*source-info* (let [{:keys [line column file]} (meta x)]
                                      (if line
                                        {:line line :column column :file file}
                                        *source-info*))]
              (emit x env)))
          (and (tagged-literal? x) (= 'dart (:tag x))) (emit-dart-literal (:form x) env)
          (coll? x) (emit-coll x env)
          :else (throw (ex-info (str "Can't compile " (pr-str x)) {:form x})))]
    (cond-> dart-x
      (or (symbol? dart-x) (coll? dart-x)) (with-meta (infer-type (vary-meta dart-x merge (dart-meta x env)))))))

(defn host-eval
  [x]
  (binding [*host-eval* true]
    (let [x (macroexpand {} x)]
      (when (seq? x)
        (case (first x)
          ns (emit-ns x {})
          def (emit-def x {})
          do (run! host-eval (next x))
          defprotocol* (emit-defprotocol* x {})
          deftype*
          (let [[_ class-name fields opts & specs] x
                [class-name & type-params] (cons class-name (:type-params (meta class-name)))
                mclass-name (with-meta
                              (or (:dart/name (meta class-name)) (munge class-name {}))
                              {:type-params type-params})] ; TODO shouldn't it be dne by munge?
            (swap! nses do-def class-name {:dart/name mclass-name :type :class}))
          nil)))))

(defn emit-test [expr env]
  (binding [*locals-gen* {}]
    ;; force lazy expresions
    (doto (emit expr env) hash)))

;; WRITING
(defn declaration [locus] (:decl locus ""))
(defn declared [locus]
  ; merge to conserve custom attributes
  (merge  (dissoc locus :fork :decl) (:fork locus)))

(def statement-locus
  {:statement true
   :pre ""
   :post ";\n"})

(defn named-fn-locus [name]
  {:pre (str (-> name meta :dart/ret-type (or  "dc.dynamic")) " " name)
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

(def paren-locus
  {:pre "("
   :post ")"})

(def arg-locus
  {:pre ""
   :post ", "})

(defn assignment-locus [left-value]
  {:pre (str left-value "=")
   :post ";\n"})

(defn var-locus
  ([varname] (var-locus (-> varname meta :dart/type) varname))
  ([vartype varname]
   {:pre (str (or vartype "var") " " varname "=")
    :post ";\n"
    :decl (str (or vartype "var") " " varname ";\n")
    :fork (assignment-locus varname)}))

(defn final-locus
  ([varname] (final-locus (-> varname meta :dart/type) varname))
  ([vartype varname]
   {:pre (str "final " (some-> vartype (str " ")) varname "=")
    :post ";\n"
    :decl (str "late final " (some-> vartype (str " ")) varname ";\n")
    :fork (assignment-locus varname)}))

(declare write)

(defn write-top-dartfn [sym x]
  (case (first x)
    dart/fn (write x (named-fn-locus sym))
    (write x (var-locus (emit-type 'cljd.core/IFn$iface {}) (name sym)))))

(defn write-top-field [sym x]
  (write (ensure-dart-expr x {}) (var-locus (name sym))))

(defn write-dynamic-var-top-field [k dart-sym x]
  (let [root-sym (symbol (str dart-sym "$root"))
        type (-> dart-sym meta (:dart/type "dc.dynamic"))]
    (write-top-field root-sym x)
    (print type)
    (print " get ")
    (print dart-sym)
    (print " => ")
    (write (list 'dart/as
             (emit `(cljd.core/get-dynamic-binding '~k ~root-sym) {root-sym root-sym})
             type) expr-locus)
    (print ";\nset ")
    (print dart-sym)
    (print "(dc.dynamic v) => ")
    (write (emit `(cljd.core/set-dynamic-binding! '~k ~'v) '{v v}) expr-locus)
    (print ";\n")))

(defn- write-args [args]
  (let [[positionals nameds] (split-with (complement keyword?) args)]
    (print "(")
    (run! #(write % arg-locus) positionals)
    (run! (fn [[k x]]
            (print (str (name k) ": "))
            (write x arg-locus)) (partition 2 nameds))
    (print ")")))

(defn write-string-literal [s]
  (print
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
    (nil? x) (print "null")
    :else (print (str x))))

(defn write-params [fixed-params opt-kind opt-params]
  (when-not (seqable? opt-params) (throw (ex-info "fail" {:k opt-params})))
  (print "(")
  (doseq [p fixed-params :let [{:dart/keys [nat-type]} (meta p)]]
    (print (or nat-type "dc.dynamic")) (print " ") (print p) (print ", "))
  (when (seq opt-params)
    (print (case opt-kind :positional "[" "{"))
    (doseq [[p d] opt-params]
      (print p "= ")
      (write d arg-locus))
    (print (case opt-kind :positional "]" "}")))
  (print ")"))

(defn write-class [{class-name :name :keys [abstract extends implements with fields ctor ctor-params super-ctor methods nsm]}]
  (when abstract (print "abstract "))
  (let [[_ dart-alias local-class-name] (re-matches #"(?:([a-zA-Z0-9_$]+)\.)?(.+)" class-name)]
    ; TODO assert dart-alias is current alias
    (print "class" local-class-name))
  (some->> extends (print " extends"))
  (some->> with seq (str/join ", ") (print " with"))
  (some->> implements seq (str/join ", ") (print " implements"))
  (print " {\n")
  (doseq [field fields
          :let [{:dart/keys [mutable type]} (meta field)]]
    (print (str (cond (not mutable) "final " (not type) "var ") (some-> type (str " ")) field ";\n")))

  (when-not abstract
    (newline)
    (when (and (contains? #{nil "dc.Object"} extends) ;; TODO refine heuristiict  with analyzer
            (not-any? #(:dart/mutable (meta %)) fields))
      (print "const "))
    (print (str (or ctor class-name) "("))
    (doseq [p ctor-params]
      (print (if (seq? p) (str "this." (second p)) p))
      (print ", "))
    (print "):super")
    (some->> super-ctor :method (str ".") print)
    (write-args (:args super-ctor))
    (print ";\n"))

  (doseq [[mname type-params fixed-params opt-kind opt-params no-explicit-body body] methods
          :let [{:dart/keys [getter setter type async]} (meta mname)]]
    (newline)
    (when-not setter
      (print (or type "dc.dynamic"))
      (print " "))
    (cond
      getter (print "get ")
      setter (print "set "))
    (when (#{">>" "[]=" "*" "%" "<=" "unary-" "|" "~" "/" "-" ">>>" "ˆ" "~/"
             "[]" ">=" "&" "<" "<<" "==" "+" ">"} (name mname))
      (print "operator "))
    (print mname)
    (when (seq type-params)
      (print "<")
      (print (str/join ", " type-params))
      (print ">"))
    (when-not getter (write-params fixed-params opt-kind opt-params))
    (if (and abstract no-explicit-body)
      (print ";\n")
      (do
        (when async (print " async "))
        (print "{\n")
        (write body return-locus)
        (print "}\n"))))

  (when nsm
    (newline)
    (print "dc.dynamic noSuchMethod(i)=>super.noSuchMethod(i);\n"))

  (print "}\n"))

(def ^:private ^:dynamic *caught-exception-symbol* nil)

(defn- widen-num-op [a b]
  (case a
    "dc.int" (case b ("dc.int" "dc.double") b nil)
    "dc.double" (case b ("dc.int" "dc.double") a nil)
    nil))

(def inferences
  {"+" (fn [a b]
         (case a
           "dc.int" (case b ("dc.int" "dc.double") b nil)
           "dc.double" (case b ("dc.int" "dc.double") a nil)
           "dc.String" (case b ("dc.String") b nil)
           nil))
   "*" (fn [a b]
         (case a
           "dc.int" (case b ("dc.int" "dc.double") b nil)
           "dc.double" (case b ("dc.int" "dc.double") a nil)
           "dc.String" (case b ("dc.int") a nil)
           nil))
   "-" widen-num-op
   "/" widen-num-op})

(defn infer-type [x]
  (let [m (meta x)]
    (->
     (cond
       (:dart/inferred m) m
       ;; TODO use mirrors
       (= 'dc.identical x) {:dart/fn-type :native :dart/type "dc.Function" :dart/nat-type "dc.Function"
                            :dart/ret-type "dc.bool" :dart/ret-truth :boolean}
       (boolean? x) {:dart/type "dc.bool" :dart/nat-type "dc.bool" :dart/truth :boolean}
       (string? x) {:dart/type "dc.String" :dart/nat-type "dc.String" :dart/truth :some}
       (double? x) {:dart/type "dc.double" :dart/nat-type "dc.double" :dart/truth :some}
       (integer? x) {:dart/type "dc.int" :dart/nat-type "dc.int" :dart/truth :some}
       (seq? x)
       (case (first x)
         dart/let (infer-type (last x))
         dart/fn {:dart/fn-type :native :dart/type "dc.Function" :dart/nat-type "dc.Function"}
         dart/new {:dart/type (second x)
                   :dart/nat-type (second x)
                   :dart/truth (dart-type-truthiness (second x))}
         dart/.
         (let [[_ a meth b & bs] x
               {:dart/keys [fn-type ret-type ret-truth]} (infer-type a)]
           (if (= :ifn fn-type)
             {:dart/type ret-type :dart/truth ret-truth}
             (case (name meth)
               ("!" "<" ">" "<=" ">=" "==" "!=" "&&" "^^" "||")
               {:dart/type "dc.bool" :dart/nat-type "dc.bool" :dart/truth :boolean}
               ("~" "&" "|" "^" "<<" ">>" "~/")
               {:dart/type "dc.int" :dart/nat-type "dc.int" :dart/truth :some}
               ("+" "*" "-" "/")
               (let [type (reduce (inferences meth) (map (comp :dart/type infer-type) (list* a b bs)))]
                 {:dart/type type
                  :dart/nat-type type
                  :dart/truth (dart-type-truthiness type)})
               nil)))
         dart/is {:dart/type "dc.bool" :dart/nat-type "dc.bool" :dart/truth :boolean}
         dart/as (let [[_ _ type] x] {:dart/type type
                                      :dart/nat-type type
                                      :dart/truth (dart-type-truthiness type)})
         (when-some [{:keys [dart/ret-type dart/ret-truth]} (infer-type (first x))]
           {:dart/type ret-type
            :dart/truth (or ret-truth (dart-type-truthiness ret-type))}))
       :else nil)
     (merge m)
     (assoc :dart/inferred true))))

(defn write
  "Takes a dartsexp and a locus.
   Prints valid dart code.
   Returns true when the just-written code is exiting control flow (return throw continue) -- this enable some dead code removal."
  [x locus]
  (cond
    (vector? x)
    (do (print (:pre locus))
        (print "[")
        (run! #(write % arg-locus) x)
        (print "]")
        (print (:post locus)))
    (seq? x)
    (case (first x)
      dart/fn
      (let [[_ fixed-params opt-kind opt-params async body] x]
        (print (:pre locus))
        (write-params fixed-params opt-kind opt-params)
        (when async (print " async "))
        (print "{\n")
        (write body return-locus)
        (print "}")
        (print (:post locus)))
      dart/let
      (let [[_ bindings expr] x]

        (or
         (some (fn [[v e]] (write e (cond (nil? v) statement-locus
                                          (and (seq? e) (= 'dart/fn (first e))) (named-fn-locus v)
                                          :else (final-locus v))))
               bindings)
         (write expr locus)))
      dart/letrec
      (let [[_ bindings wirings expr] x
            loci+exprs (map (fn [[local expr]] [(final-locus local) expr]) bindings)]
        (run! print (keep (comp declaration first) loci+exprs))
        (doseq [[locus expr] loci+exprs]
          (write expr (declared locus)))
        (doseq [[obj deps] wirings
                dep deps]
          (print obj) (print ".") (print dep) (print "=") (print dep) (print ";\n"))
        (write expr locus))
      dart/try
      (let [[_ body catches final] x
            decl (declaration locus)
            locus (declared locus)
            _  (some-> decl print)
            _ (print "try {\n")
            exit (write body locus)
            exit
            (transduce
             (map (fn [[classname e st expr]]
                    (print "} on ")
                    (print classname) ;; TODO aliasing
                    (print " catch (")
                    (print e)
                    (some->> st (print ","))
                    (print ") {\n")
                    (binding [*caught-exception-symbol* e]
                      (write expr locus))))
             (completing (fn [a b] (and a b)))
             exit catches)]
        (when final
          (print "} finally {\n")
          (write final statement-locus))
        (print "}\n")
        exit)
      dart/as
      (let [[_ expr type] x]
        (print (:pre locus))
        (print "(")
        (write expr expr-locus)
        (print " as ")
        (print type)
        (print ")")
        (print (:post locus)))
      dart/is
      (let [[_ expr type] x]
        (print (:pre locus))
        (print "(")
        (write expr expr-locus)
        (print " is ")
        (print type)
        (print ")")
        (print (:post locus)))
      dart/assert
      (let [[_ condition msg-expr] x]
        (print "assert(")
        (write condition expr-locus)
        (print ", ")
        (write msg-expr expr-locus)
        (print ");\n"))
      dart/await
      (let [[_ expr] x]
        (print (:pre locus))
        (print "(await ")
        (write expr expr-locus)
        (print ")")
        (print (:post locus)))
      dart/throw
      (let [[_ expr] x]
        (if (= expr *caught-exception-symbol*)
          (print "rethrow;\n")
          (write expr throw-locus))
        true)
      dart/case
      (let [[_ expr clauses default-expr] x
            decl (declaration locus)
            locus (declared locus)
            _ (some-> decl print)
            _ (print "switch(")
            _ (write expr expr-locus)
            _ (print "){\n")
            exit (reduce
                  (fn [exit [vals expr]]
                    (run! #(do (print "case ") (write % expr-locus) (print ":\n")) vals)
                    (if (write expr locus)
                      exit
                      (print "break;\n")))
                  true
                  clauses)
            _ (print "_default: default:\n")
            exit (and (write default-expr locus) exit)]
        (print "}\n")
        exit)
      dart/continue
      (let [[_ label] x]
        (print "continue ") (print label) (print ";\n")
        true)
      dart/if
      (let [[_ test then else] x
            decl (declaration locus)
            locus (declared locus)]
        (some-> decl print)
        (print "if(")
        (write test expr-locus)
        (print "){\n")
        (if (write then locus)
          (do
            (print "}\n")
            (write else locus))
          (do
            (print "}else{\n")
            (write else locus)
            (print "}\n"))))
      dart/loop
      (let [[_ bindings expr] x
            decl (declaration locus)
            locus (-> locus declared (assoc :loop-bindings (map first bindings)))]
        (some-> decl print)
        (doseq [[v e] bindings]
          (write e (var-locus v)))
        (print "do {\n")
        (when-not (write expr locus)
          (print "break;\n"))
        (print "} while(true);\n"))
      dart/recur
      (let [[_ & exprs] x
            {:keys [loop-bindings]} locus
            expected (count loop-bindings)
            actual (count exprs)]
        (when-not loop-bindings
          (throw (ex-info "Can only recur from tail position." {})))
        (when-not (= expected actual)
          (throw (ex-info (str "Mismatched argument count to recur, expected: "
                               expected " args, got: " actual) {})))
        (let [vars (set loop-bindings)
              vars-usages (->>
                           (map #(into #{} (keep (disj vars %1))
                                       (tree-seq coll? seq %2))
                                loop-bindings exprs)
                           reverse
                           (reductions into)
                           reverse)
              tmps (into {}
                     (map (fn [v vs]
                            (assert (re-matches #".*\$\d+" (name v)))
                            (when (vs v) [v (with-meta (symbol (str v "tmp")) (meta v))]))
                       loop-bindings vars-usages))]
          (doseq [[v e] (map vector loop-bindings exprs)]
            (write e (if-some [tmp (tmps v)] (var-locus tmp) (assignment-locus v))))
          (doseq [[v tmp] tmps]
            (write tmp (assignment-locus v)))
          (print "continue;\n")
          true))
      dart/set!
      (let [[_ target val] x]
        ;; locus isn't used here because set! should always be lifted
        ;; into a side-effecting (nil-bound) let binding
        (write val (assignment-locus
                    (if (symbol? target)
                      target
                      (let [[op obj fld] target]
                        (case op
                          dart/.-
                          (if (symbol? obj)
                            (str obj "." fld)
                            (case (first obj)
                              dart/as
                              (let [[_ obj type] obj]
                                (str "(" obj " as " type ")." fld))))))))))
      dart/.-
      (let [[_ obj fld] x]
        (print (:pre locus))
        (write obj expr-locus)
        (print (str "." fld))
        (print (:post locus))
        (:exit locus))
      dart/.
      (let [[_ obj meth & args] x]
        (print (:pre locus))
        (case meth
          ;; operators, some are cljd tricks
          "[]" (do
                 (write obj expr-locus)
                 (print "[")
                 (write (first args) expr-locus)
                 (print "]"))
          "[]=" (do
                  (write obj expr-locus)
                  (print "[")
                  (write (first args) expr-locus)
                  (print "]=")
                  (write (second args) expr-locus))
          ("~" "!") (do
                      (print meth)
                      (write obj paren-locus))
          "-" (if args
                (do
                  (write obj paren-locus)
                  (print meth)
                  (write (first args) paren-locus))
                (do
                  (print meth)
                  (write obj paren-locus)))
          ("&&" "||" "^^" "+" "*" "&" "|" "^")
          (do
            (write obj paren-locus)
            (doseq [arg args]
              (print meth)
              (write arg paren-locus)))
          ("<" ">" "<=" ">=" "==" "!=" "~/" "/" "%" "<<" ">>" #_">>>")
          (do
            (write obj paren-locus)
            (print meth)
            (write (first args) paren-locus))
          ;; else plain method
          (do
            (write obj expr-locus)
            (print (str "." meth))
            (write-args args)))
        (print (:post locus))
        (:exit locus))
      dart/new
      (let [[_ type & args] x]
        (print (:pre locus))
        (when (:dart/const (meta x))
          (print "const "))
        (print type)
        (write-args args)
        (print (:post locus))
        (:exit locus))
      ;; native fn call
      (let [[f & args] x]
        (print (:pre locus))
        (write f expr-locus)
        (write-args args)
        (print (:post locus))
        (:exit locus)))
    :else (when-not (:statement locus)
            (print (:pre locus))
            (write-literal x)
            (print (:post locus))
            (:exit locus))))

(defn- relativize-lib [^String src ^String target]
  (if (<= 0 (.indexOf target ":"))
    target
    (loop [[s & ss] (str/split src #"/") [t & ts :as all-ts] (str/split target #"/")]
      (if (and (some? ss) (= s t))
        (recur ss ts)
        (str/join "/" (concat (map (constantly "..") ss) all-ts))))))

;; Compile clj -> dart file
(defn dump-ns [{ns-lib :lib :as ns-map}]
  (doseq [lib (keys (:imports ns-map))
          :let [dart-alias (-> @nses :libs (get lib) :dart-alias)]]
    (print "import ")
    (write-string-literal (relativize-lib ns-lib lib)) ;; TODO: relativize local libs (nses)
    (print " as ")
    (print dart-alias)
    (print ";\n"))
  (print "\n")
  (doseq [[sym v] (->> ns-map (filter (comp symbol? key)) (sort-by key))
          :when (symbol? sym)
          :let [{:keys [dart/code]} v]
          :when code]
    (println code)))

(defn dart-type-params-reader [x]
  (else->>
    (if (symbol? x) x)
    (let [[args [_ ret :as rets]] (split-with (complement '#{->}) x)])
    (if (seq rets) ; TODO correct support of optionals (stop conflating type params and params types)
      (with-meta 'dart:core/Function
        {:type-params (map dart-type-params-reader (cons ret args))}))
    (let [[type & params] x]
      (cond-> type
        params
        (vary-meta assoc :type-params (map dart-type-params-reader params))))))

(def dart-data-readers
  {'dart #(tagged-literal 'dart %)
   '/ dart-type-params-reader})

(def cljd-resolver
  (reify clojure.lang.LispReader$Resolver
    (currentNS [_] (:current-ns @nses))
    (resolveClass [_ sym]
      (let [{:keys [current-ns] :as nses} @nses]
         (get-in nses [current-ns :mappings sym])))
    (resolveAlias [_ sym]
      (let [{:keys [current-ns libs] :as nses} @nses
            {:keys [aliases]} (nses current-ns)]
        (when-some [lib (some-> sym name aliases libs)]
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
                     (throw (ex-info "Compilation error." {:form form} e))))
                 (recur))))))))

(defn host-load-input [in]
  #?(:clj
     (with-cljd-reader
       (let [in (clojure.lang.LineNumberingPushbackReader. in)]
         (loop []
           (let [form (read {:eof in :read-cond :allow} in)]
             (when-not (identical? form in)
               (binding [*locals-gen* {}] (host-eval form))
               (recur))))))))

(defn- rename-fresh-lib [{:keys [libs aliases] :as nses} from to]
  (let [{:keys [ns dart-alias] :as m} (libs from)
        nses (assoc nses
               :libs (-> libs (dissoc from) (assoc to m))
               :aliases (assoc aliases dart-alias to))
        {:keys [aliases imports] :as  the-ns} (nses ns)
        imports (-> imports (dissoc from) (assoc to (imports from)))
        aliases (into {}
                  (map (fn [[alias lib]]
                         [alias (if (= lib from) to lib)]))
                  aliases)]
    (assoc nses ns (assoc the-ns :lib to :imports imports :aliases aliases))))

(defn compile-input [in]
  (load-input in)
  (let [{:keys [current-ns] :as all-nses} @nses
        the-ns (all-nses current-ns)
        libname (:lib the-ns)
        is-test-ns (-> 'main the-ns :meta :dart/test)
        libname' (if is-test-ns
                   (str *test-path* (str/replace (subs libname (count *lib-path*)) #"\.dart$" "_test.dart"))
                   libname)]
    (when is-test-ns
      (swap! nses rename-fresh-lib libname libname'))
    (with-open [out (-> (java.io.File. ^String libname')
                      (doto (-> .getParentFile .mkdirs))
                      java.io.FileOutputStream.
                      (java.io.OutputStreamWriter. "UTF-8")
                      java.io.BufferedWriter.)]
      (binding [*out* out]
        (dump-ns (@nses current-ns))))
    libname'))

(defn ns-to-paths [ns-name]
  (let [base (replace-all (name ns-name) #"[.-]" {"." "/" "-" "_"})]
    [(str base ".cljd") (str base ".cljc")]))

(defn find-resource
  "Search for a file on the clojure path."
  [filename]
  (io/resource filename))

(defn compile-namespace [ns-name]
  ;; iterate first on file variants then on paths, not the other way!
  (let [file-paths (ns-to-paths ns-name)]
    (if-some [^java.net.URL url (some find-resource file-paths)]
      (do
        (when *hosted*
          (with-open [in (.openStream url)]
            (host-load-input (java.io.InputStreamReader. in "UTF-8"))))
        (with-open [in (.openStream url)]
          (compile-input (java.io.InputStreamReader. in "UTF-8"))))
      (throw (ex-info (str "Could not locate "
                        (str/join " or " file-paths))
               {:ns ns-name})))))

(comment
  (binding [*lib-path* "examples/hello-flutter/lib"]
    (compile-namespace 'hello-flutter.core))

  (time
    (binding [*hosted* true]
      (compile-namespace 'cljd.core)))

  (time
    (compile-namespace 'cljd.string))

  (time
    (compile-namespace 'cljd.walk))

  (time
    (binding [*hosted* true]
      (compile-namespace 'cljd.template)))

  (time
    (binding [*hosted* true]
      (compile-namespace 'cljd.test)))

  (time
    (binding [*hosted* true]
      (compile-namespace 'cljd.test-clojure.for)))

  (time
    (binding [*hosted* false]
      (compile-namespace 'cljd.test-clojure.core-test)))

  (time
    (binding [*hosted* true]
      (compile-namespace 'cljd.test-clojure.string)))

  (time
    (compile-namespace 'cljd.main))

  (time
    (compile-namespace 'cljd.user))

  )
