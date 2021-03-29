(ns cljd.compiler
  (:refer-clojure :exclude [macroexpand macroexpand-1 munge])
  (:require [clojure.string :as str]))

(def bootstrap-nses '#{cljd.compiler cljd.core})

(def ^:dynamic *bootstrap* false)
(def ^:dynamic *bootstrap-eval* false)

(def ^:dynamic *clj-path*
  "Sequential collection of directories to search for clj files."
  ["clj/"])

(def ^:dynamic *lib-path* "lib/")

(def ^:dynamic *target-subdir*
  "Relative path to the lib directory (*lib-dir*) where compiled dart file will be put.
   Defaults to \"cljd/\"."
  "cljd/")

(defmacro ^:private else->> [& forms]
  `(->> ~@(reverse forms)))

(defn- replace-all [^String s regexp f]
  #?(:cljd
     (.replaceAllMapped s regexp f)
     :clj
     (str/replace s regexp f)))

(def ns-prototype
  {:imports {"dc" {:lib "dart:core"}} ; dc can't clash with user aliases because they go through dart-global
   :aliases {"dart:core" "dc"}
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
                 'user ns-prototype}))

(declare munge)

(defn resolve-symbol
  "Returns either a pair [tag value] or nil when the symbol can't be resolved.
   tag can be :local, :def or :dart respectively indicating the symbol refers
   to a local, a global def (var-like) or a Dart global.
   The value depends on the tag:
   - for :local it's whatever is in the env,
   - for :def it's what's is in nses,
   - for :dart it's the aliased dart symbol."
  [sym env]
  (let [nses @nses
        {:keys [mappings aliases imports] :as current-ns} (nses (:current-ns nses))]
    (else->>
     (if-some [v (env sym)] [:local v])
     (if-some [v (-> sym current-ns)] [:def v])
     (if-some [v (mappings sym)] (recur v env))
     (if-some [alias (get aliases (namespace sym))]
       (let [{:keys [ns lib]} (imports alias)]
         (if ns
           (recur (with-meta (symbol (name ns) (name sym)) (meta sym)) env)
           [:dart (symbol (str alias "." (name sym)))])))
     (if-some [info (some-> sym namespace symbol nses (get (symbol (name sym))))]
       [:def info])
     nil)))

(defn emit-type [tag]
  (if (string? tag)
    (let [nses @nses
          {:keys [mappings aliases] :as current-ns} (nses (:current-ns nses))]
      (replace-all (str tag) #"(?:([^\s,()\[\]}<>]+)\.)?([a-zA-Z0-9_$]+)( +[a-zA-Z0-0_$]+)?" ; first group should match any clojure constituent char
        (fn [[_ alias type identifier]]
          (cond->
              (if (and (nil? alias) (#{"Function" "void" "dynamic"} type))
                type
                (name (emit-type (symbol alias type))))
            identifier (str identifier)))))
    (let [[t info] (resolve-symbol tag {})]
      (case t
        :dart (name info)
        :def (case (:type info)
               ; TODO should be "namespaced" by dart alias if needed
               :class (:dart/name info))))))

(defn dart-type-truthiness [type]
  (case type
    (nil "dc.Object" "dc.dynamic") nil
    "dc.bool" :boolean
    :some))

(defn dart-meta
  "Takes a clojure symbol and returns its dart metadata."
  [sym]
  (let [m (meta sym)
        type (some-> m :tag emit-type)]
    (cond-> {}
      (:getter m) (assoc :dart/getter true)
      (:setter m) (assoc :dart/setter true)
      (:dart m) (assoc :dart/fn-type :native)
      (:clj m) (assoc :dart/fn-type :ifn)
      type (assoc :dart/type type :dart/truth (dart-type-truthiness type)))))

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
   "'"    "$SINGLEQUOTE_"
   "\""   "$DOUBLEQUOTE_"
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

(defn munge [sym]
  (let [s (name sym)]
    (with-meta
      (symbol
       (or (when (reserved-words s) (str "$" s "_"))
           (replace-all s #"__(\d+)|__auto__|(^-)|[^a-zA-Z0-9]"
                        (fn [[x n leading-dash]]
                          (else->>
                           (if leading-dash "$_")
                           (if n (str "$" n "_"))
                           (if (= "__auto__" x) "$AUTO_")
                           (or (char-map x))
                           (str "$u"
                                ;; TODO :cljd version
                                (str/join "_$u" (map #(-> % int Long/toHexString .toUpperCase) x))
                                "_"))))))
      (dart-meta sym))))

(defn- dont-munge [& args]
  (let [sym (symbol (apply str args))]
    (with-meta sym {:dart/name sym})))

(defonce ^:private gens (atom 1))
(defn dart-global
  ([] (dart-global ""))
  ([prefix]
   (with-meta (symbol (str (munge prefix) "$" (swap! gens inc)))
     (dart-meta prefix))))

(def ^:dynamic *locals-gen*)
(defn dart-local
  "Generates a unique (relative to the top-level being compiled) dart symbol.
   Hint is a string/symbol/keyword which gives a hint (duh) on how to name the
   dart symbol. Type tags when present are translated."
  ([] (dart-local ""))
  ([hint]
   (let [dart-hint (munge hint)
         {n dart-hint} (set! *locals-gen* (assoc *locals-gen* dart-hint (inc (*locals-gen* dart-hint 0))))]
     (with-meta (symbol (str dart-hint "$" n))
       (dart-meta hint)))))

(defn- parse-dart-params [params]
  (let [[fixed-params [delim & opt-params]] (split-with (complement '#{.& ...}) params)]
    {:fixed-params fixed-params
     :opt-kind (case delim .& :named :positional)
     :opt-params
     (for [[p d] (partition-all 2 1 opt-params)
           :when (symbol? p)]
       [p (when-not (symbol? d) d)])}))

(defn expand-protocol-impl [{:keys [impl iface iext extensions]}]
  (list 'deftype impl []
    :type-only true
    'cljd.core/IProtocol
    (list 'satisfies '[_ x]
      (list* 'or (list 'dart-is? 'x iface)
        (concat (for [t (keys extensions)] (list 'dart-is? 'x t)) [false])))
    (list 'extensions '[_ x]
      ;; TODO sort types
      (list 'or
        (cons 'cond
          (mapcat (fn [[t ext]] [(list 'dart-is? 'x t) ext]) extensions))
        '(throw (dart:core/Exception. "No extension found."))))))

#?(:clj
   (do
     (defn- roll-leading-opts [body]
       (loop [[k v & more :as body] (seq body) opts {}]
         (if (and body (keyword? k))
           (recur more (assoc opts k v))
           [opts body])))

     (defn resolve-dart-mname
       "Takes two symbols (a protocol and one of its method) and the number
  of arguments passed to this method.
  Returns the name (as symbol) of the dart method backing this clojure method."
       [pname mname args-count]
       (let [[tag protocol] (resolve-symbol pname {})]
         (when (and (= :def tag) (= :protocol (:type protocol)))
           (or
             (get-in protocol [:sigs mname args-count :dart/name] mname)
             (throw (Exception. (str "No method " mname " with " args-count " arg(s) for protocol " pname ".")))))))

     (defn- expand-opts+specs [opts+specs]
       (let [[opts specs] (roll-leading-opts opts+specs)
             last-seen-type (atom nil)]
         (cons opts
               (map
                (fn [spec]
                  (if (seq? spec)
                    (let [[mname arglist & body] spec
                          mname (or (some-> @last-seen-type (resolve-dart-mname mname (count arglist)))
                                    mname)]
                      ;; TODO: mname resolution against protocol ifaces
                      (list* mname (parse-dart-params arglist) body))
                    (reset! last-seen-type spec)))
                specs))))

     #_(defmacro reify [& args]
       `(reify* ~@(expand-opts+specs args)))

     (defn-  expand-deftype [& args]
       (let [[class-name fields & args] args
             [opts & specs](expand-opts+specs args)]
         (list 'do
           (list* 'deftype* class-name fields opts specs)
           (when-not (:type-only opts)
             (list 'defn
               (symbol (str "->" class-name))
               (vec fields)
               (list* 'new class-name fields))))))

     (defn- expand-definterface [iface & meths]
       (list* 'deftype (vary-meta iface assoc :abstract true) []
         :type-only true
         (for [[meth args] meths]
           (list meth (into '[_] args)))))

     (defn- expand-defprotocol [proto & methods]
       ;; TODO do something with docstrings
       (let [[doc-string & methods] (if (string? (first methods)) methods (list* nil methods))
             method-mapping
             (into {} (map (fn [[m & arglists]]
                             (let [dart-m (munge m)
                                   [doc-string & arglists] (if (string? (last arglists)) (reverse arglists) (list* nil arglists))]
                               [(with-meta m {:doc doc-string})
                                (into {} (map #(let [l (count %)] [l {:dart/name (symbol (str dart-m "$" (dec l)))
                                                                      :args %}]))
                                      arglists)]))) methods)
             ; TODO check munging below
             iface (symbol (str (munge proto) "$iface"))
             iface (with-meta iface {:dart/name iface})
             iext (symbol (str (munge proto) "$extension"))
             iext (with-meta iext {:dart/name iext})
             impl (symbol (str (munge proto) "$impl"))
             impl (with-meta impl {:dart/name impl})
             proto-map
             {:iface iface
              :iext iext
              :impl impl
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
               (list* 'defn method
                 (for [{:keys [dart/name args]} (vals arity-mapping)]
                   (list args
                     (list 'if (list 'dart-is? (first args) iface)
                       (list* '. (first args) name (next args)) ; TODO cast to iface
                       `(. (.extensions ~proto ~(first args)) ~name ~@args)
                       #_TODO_EXTENSIONS)))))
             (list proto)))))

     (defn- expand-case [expr & clauses]
       (if (or (symbol? expr) (odd? (count clauses)))
         (let [clauses (vec (partition-all 2 clauses))
               last-clause (peek clauses)
               clauses (cond-> clauses (nil? (next last-clause)) pop)
               default (if (next last-clause)
                         `(throw (.value dart:core/ArgumentError ~expr nil "No matching clause."))
                         (first last-clause))]
           (list 'case* expr (for [[v e] clauses] [(if (seq? v) v (list v)) e]) default))
         `(let* [test# ~expr] (~'case test# ~@clauses))))))


(defn expand-extend-type [type & specs]
  (let [proto+meths (reduce
                      (fn [proto+meths x]
                        (if (symbol? x)
                          (conj proto+meths [x])
                          (conj (pop proto+meths) (conj (peek proto+meths) x))))
                      [] specs)]
    (cons 'do
      (when-not *bootstrap-eval*
        (for [[protocol & meths] proto+meths
              :let [[tag info] (resolve-symbol protocol {})
                    _ (when-not (and (= tag :def) (= :protocol (:type info)))
                        (throw (Exception. (str protocol " isn't a protocol."))))
                    extension-name (dont-munge (symbol (str/join "$$" (for [x [type protocol]
                                                                            f [namespace name]
                                                                            :let [x (some-> x f munge)]
                                                                            :when x]
                                                                               x))))
                    extension-instance (dont-munge (symbol (str extension-name "$instance")))]]
          (list 'do
            (list* 'deftype extension-name []
              :type-only true
              (:iext info)
              (for [[mname & body-or-bodies] meths
                    [args & body] (cond-> body-or-bodies (vector? (first body-or-bodies)) list)
                    :let [mname (get-in info [:sigs mname (count args) :dart/name])]]
                (list* mname (into '[_] args) body)))
            (list 'def extension-instance (list 'new extension-name))
            ;; TODO qualify class-name
            (list 'extend-type-protocol* type (:ns info) (:name info)
              (symbol (name (:current-ns @nses)) (name extension-instance)))))))))

(defn ghost-ns []
  (create-ns (symbol (str (:current-ns @nses) ".ghost"))))

(defn macroexpand-1 [env form]
  (if-let [[f & args] (and (seq? form) (symbol? (first form)) form)]
    (let [f-name (name f)
          f (if (= "clojure.core" (namespace f)) (symbol "cljd.core" f-name) f)
          [f-type f-v] (resolve-symbol f env)
          macro-fn (or
                     (when-some [v (when *bootstrap* (ns-resolve (ghost-ns) f))]
                       (when (-> v meta :macro) @v))
                     (case f-type ; wishful coding for the compiler running on dart
                       :def (when (-> f-v :meta :macro) (-> f-v :runtime-value))
                       nil))]
      ;; TODO add proper expansion here, before defaults
      (cond
        (env f) form
        #?@(:clj ; macro overrides
            [(= 'ns f) form
             (= 'reify f) (cons 'reify* (expand-opts+specs args))
             (= 'deftype f) (apply expand-deftype args)
             (= 'definterface f) (apply expand-definterface args)
             (= 'defprotocol f) (apply expand-defprotocol args)
             (= 'case f) (apply expand-case args)
             (= 'extend-type f) (apply expand-extend-type args)])
        (= '. f) form
        macro-fn
        (apply macro-fn form env (next form))
        (.endsWith f-name ".")
        (list* 'new
               (symbol (namespace f) (subs f-name 0 (dec (count f-name))))
               args)
        (.startsWith f-name ".")
        (list* '. (first args) (symbol (subs f-name 1)) (next args))
        :else form))
    form))

(defn macroexpand [env form]
  (let [ex (macroexpand-1 env form)]
    (cond->> ex (not (identical? ex form)) (recur env))))

(declare emit infer-type)

(defn atomic?
  [x] (not (coll? x)))

(def primitives?
  ;; TODO : handles other primitives (bytes ?)
  (some-fn string? float? int? boolean? char? double?))

(defn has-recur?
  "Takes a dartsexp and returns true when it contains an open recur."
  [x]
  (some {'dart/recur true} (tree-seq seq? #(case (first %) (dart/loop dart/fn) nil %) x)))

(defn liftable
  "Takes a dartsexp and returns a [bindings expr] where expr is atomic
   or nil if there are no bindings to lift."
  [x]
  (case (when (seq? x) (first x))
    dart/let
    (let [[_ bindings expr] x]
      (if-some [[bindings' expr] (liftable expr)]
         [(concat bindings bindings') expr]
         (if (atomic? expr)
           [bindings expr]
           ;; this case should not happen
           (let [tmp (dart-local)]
             [(conj (vec bindings) [tmp expr]) tmp]))))
    (dart/if dart/try dart/case) ; no ternary for now
    (let [tmp (dart-local (first x))]
      [[[tmp x]]
       tmp])
    nil))

(defn- lift-arg [must-lift x hint]
  (or (liftable x)
      (cond
        (atomic? x) [nil x]
        must-lift
        (let [tmp (dart-local hint)]
          [[[tmp x]] tmp])
        :else
        [nil x])))

(defn- split-args [args]
  (let [[positional-args [_ & named-args]] (split-with (complement '#{.&}) args)]
    [positional-args named-args]))

(defn emit-args
  "[bindings dart-args has-nameds]"
  ([args env]
   (emit-args false args env))
  ([must-lift args env]
   (let [[positionals nameds] (split-args args)
         [bindings dart-args]
         (as-> [(when must-lift (list 'sentinel)) ()] acc
           (reduce (fn [[bindings dart-fn-args] [k x]]
                     (let [[bindings' x'] (lift-arg (seq bindings) (emit x env) k)]
                       [(concat bindings' bindings) (list* k x' dart-fn-args)]))
             acc (reverse (partition 2 nameds)))
           (reduce (fn [[bindings dart-fn-args] x]
                     (let [[bindings' x'] (lift-arg (seq bindings) (emit x env) "arg")]
                       [(concat bindings' bindings) (cons x' dart-fn-args)]))
             acc (reverse positionals)))
         bindings (cond-> bindings must-lift butlast)]
     [bindings dart-args])))

(def ^:dynamic *threshold* 10)

(defn emit-fn-call [[f & args] env]
  (let [dart-f (emit f env)
        fn-type (if (some '#{.&} args) :native (:dart/fn-type (meta dart-f)))
        [bindings dart-args] (emit-args (nil? fn-type) args env)
        [bindings' dart-f] (lift-arg (seq bindings) dart-f "f")
        bindings (concat bindings' bindings)
        native-call (cons dart-f dart-args)
        ifn-call (let [ifn-cast (list 'dart/. (list 'dart/as dart-f (emit 'cljd.core/IFn$iface env)))]
                   (if (< (count dart-args) *threshold*)
                     (concat ifn-cast
                             [(resolve-dart-mname 'cljd.core/IFn '-invoke (inc (count dart-args)))]
                             dart-args)
                     (concat ifn-cast
                             [(resolve-dart-mname 'cljd.core/IFn '-invoke-more (inc *threshold*))]
                             (subvec (vec dart-args) 0 (dec *threshold*))
                             [(subvec (vec dart-args) (dec *threshold*))])))
        dart-fn-call
        (case fn-type
          :native native-call
          :ifn ifn-call
          (list 'dart/if (list 'dart/is dart-f 'dc.Function)
            (cons (list 'dart/as dart-f 'dc.Function) dart-args)
            (list 'dart/if (list 'dart/is dart-f (emit 'cljd.core/IFn$iface env))
              ifn-call
              (let [[meth & args] (nnext ifn-call)] ; "callables" must be handled by an extesion on dynamic or Object
                (list* 'dart/. (list 'dart/. (emit 'cljd.core/IFn env) 'extensions dart-f) meth (cons dart-f args)))

              #_(let [if-env {'ext (dart-local 'ext)}
                      ext (if-env 'ext)
                      [dart-if dart-test] (emit '(if ext) if-env)]
                  (list 'dart/let [[ext (list 'dart/. (emit 'cljd.core/IFn env) 'extensions dart-f)]]
                    (list dart-if dart-test
                      (let [[meth & args] (nnext ifn-call)] (list* 'dart/. ext meth (cons dart-f args)))
                      (cons (list 'dart/as (first native-call) 'dc.dynamic) (next native-call))))))))]
    (cond->> dart-fn-call
      (seq bindings) (list 'dart/let bindings))))

(defn emit-coll
  ([coll env] (emit-coll identity coll env))
  ([f coll env]
   (let [items (into [] (comp (if (map? coll) cat identity) (map f)) coll)
         [bindings items]
         (reduce (fn [[bindings fn-call] x]
                   (let [[bindings' x'] (lift-arg (seq bindings) (emit x env) "item")]
                     [(concat bindings' bindings) (cons x' fn-call)]))
                 [nil ()] (rseq items))
         fn-sym (cond
                  (map? coll) 'cljd.core/to-map ; is there a cljs equivalent?
                  (vector? coll) 'cljd.core/vec
                  (set? coll) 'cljd.core/set
                  (seq? coll) 'cljd.core/to-list ; should we use apply list?
                  :else (throw (ex-info (str "Can't emit collection " (pr-str coll)) {:form coll})))
         fn-call (list (emit fn-sym env) (vec items))]
     (cond->> fn-call (seq bindings) (list 'dart/let bindings)))))

(defn emit-dart-literal [x env]
  (if (vector? x)
    (let [[bindings items]
          (reduce (fn [[bindings fn-call] x]
                    (let [[bindings' x'] (lift-arg (seq bindings) (emit x env) "item")]
                      [(concat bindings' bindings) (cons x' fn-call)]))
                  [nil ()] (rseq x))]
     (cond->> (vec items) (seq bindings) (list 'dart/let bindings)))
    (throw (ex-info (str "Unsupported dart literal #dart " (pr-str x)) {:form x}))))

(defn emit-new [[_ class & args] env]
  (emit-fn-call (cons (vary-meta class assoc :dart true) args) env))

(defn emit-dot [[_ obj member & args] env]
  (if (seq? member)
    (recur (list* '. obj member) env)
    (let [member (name member)
          [_ prop name] (re-matches #"(-)?(.+)" member)
          prop (and prop (nil? args))
          op (if prop 'dart/.- 'dart/.)
          [bindings [dart-obj & dart-args]] (emit-args (cons obj args) env)]
      (cond->> (list* op dart-obj name dart-args)
        (seq bindings) (list 'dart/let bindings)))))

(defn emit-set! [[_ target expr] env]
  (let [target (macroexpand env target)]
    (cond
      (symbol? target)
      (if-some [dart-var (env target)]
        (if (-> dart-var meta :dart/mutable)
          (list 'dart/let
                [[nil (list 'dart/set! dart-var (emit expr env))]]
                dart-var)
          (throw (ex-info (str "Cannot assign to non-mutable: " target) {:target target})))
        (throw (ex-info (str "Unable to resolve symbol: " target " in this lexical context") {:target target})))
      (and (seq? target) (= '. (first target)))
      (let [[_ obj member] target]
        (if-some [[_ fld] (re-matches #"-(.+)" (name member))]
          (let [tmpobj (dart-local "objset")
                tmpval (dart-local fld)]
            (list 'dart/let
                  [[tmpobj (emit obj env)]
                   [tmpval (emit expr env)]
                   [nil (list 'dart/set! (list 'dart/.- tmpobj fld) tmpval)]]
                  tmpval))
          (throw (ex-info (str "Cannot assign to a non-property: " member ", make sure the property name is prefixed by a dash.")
                          {:target target}))))
      :else
      (throw (ex-info (str "Unsupported target for assignment: " target) {:target target})))))

(defn emit-let* [[_ bindings & body] env]
  (let [[dart-bindings env]
        (reduce
         (fn [[dart-bindings env] [k v]]
           (let [tmp (dart-local k)]
             [(conj dart-bindings [tmp (emit v env)])
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
           (let [tmp (dart-local k)]
             [(conj dart-bindings [tmp (emit v env)])
              (assoc env k tmp)]))
         [[] env] (partition 2 bindings))]
    (list 'dart/loop dart-bindings (emit (list* 'let* [] body) env))))

(defn emit-recur [[_ & exprs] env]
  (cons 'dart/recur (map #(emit % env) exprs)))

(defn emit-if [[_ test then else] env]
  (cond
    (or (coll? test) (symbol? test))
    (let [dart-test (emit test env)
          {:keys [dart/truth]} (infer-type dart-test)
          [bindings test] (lift-arg (nil? truth) dart-test "test")
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

(defn emit-case* [[op expr clauses default] env]
  (if (seq clauses)
    (list 'dart/case (emit expr env)
          (for [[vs e] clauses]
            [(map #(emit % {}) vs) (emit e env)])
          (emit default env))
    (emit default env)))

(defn- variadic? [[params]] (some #{'&} params))

(defn- emit-ifn [name bodies env]
  (let [synth-params (into [] (map (fn [_] (gensym "arg"))) (range *threshold*)) ; param names used when no user-specified
        more-param (gensym 'more)
        fixed-bodies (remove variadic? bodies)
        max-fixed-arity (some->> fixed-bodies seq (map first) (map count) (reduce max))
        min-fixed-arity (some->> fixed-bodies seq (map first) (map count) (reduce min))
        [vararg-params & vararg-body] (some #(when (variadic? %) %) bodies)
        base-vararg-arity (some->> vararg-params (take-while (complement #{'&})) count)
        this (or name (gensym "this"))
        invoke-exts (for [[params & body] fixed-bodies
                          :let [n (count params)]
                          :when (>= n *threshold*)]
                      `(~(dont-munge "_invoke$ext" n) [~this ~@params] ~@body))
        fixed-invokes (for [[params & body] fixed-bodies
                            :when (< (count params) *threshold*)]
                        `(~'-invoke [~this ~@params] ~@body))
        vararg-mname (dont-munge "_invoke$vararg")
        vararg-invokes
        (when vararg-params
          (cons
           `(~vararg-mname [~this ~@(drop-last 2 vararg-params) ~(peek vararg-params)] ~@vararg-body)
           (let [[this & base-args] (subvec synth-params 0 (inc base-vararg-arity))]
             (for [n (range (cond-> base-vararg-arity max-fixed-arity (max (inc max-fixed-arity))) *threshold*)
                   :let [rest-args (subvec synth-params (inc base-vararg-arity) (inc n))]]
               `(~'-invoke [~this ~@base-args ~@rest-args]
                 (. ~this ~vararg-mname ~@base-args ~(tagged-literal 'dart rest-args)))))))
        max-fixed-arity (or base-vararg-arity max-fixed-arity)
        invoke-more
        (when (or vararg-params (seq invoke-exts))
          (let [[this & more-params] synth-params
                vararg-call
                (when vararg-params
                  (let [above-threshold (- base-vararg-arity *threshold*)]
                    (if (neg? above-threshold)
                      `(. ~this ~vararg-mname
                          ~@(take base-vararg-arity more-params)
                          (.+ ~(tagged-literal 'dart (vec (drop base-vararg-arity more-params)))
                              ~more-param))
                      (let [more-destructuring (subvec vararg-params (dec *threshold*))
                            bound-vars (remove #{'&} more-destructuring)]
                        `(if (.< ~above-threshold (count ~more-param))
                           (let [~more-destructuring ~more-param]
                             (. ~this ~vararg-mname ~@more-params ~@bound-vars))
                           (throw (dart:core/ArgumentError. "No matching arity")))))))]
            `(~'-invoke-more [~@synth-params ~more-param]
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
        (let [[this & call-args] (cond->> synth-params (not vararg-params) (take (inc max-fixed-arity)))
              fixed-args (if vararg-params (take base-vararg-arity call-args) call-args)
              base-arity (or min-fixed-arity base-vararg-arity)
              base-args (take base-arity call-args)
              opt-args (drop base-arity call-args)
              default-value (str (gensym "default"))
              fixed-arities-expr
              (for [args+1 (next (reductions conj (vec base-args)
                                             (cond->> opt-args vararg-params (take (- base-vararg-arity base-arity)))))]
                [args+1 `(. ~this ~(resolve-dart-mname 'cljd.core/IFn '-invoke (count args+1)) ~@(pop args+1))])]
          `((~'call [~this ~@base-args ... ~@(interleave opt-args (repeat default-value))]
             (cond
               ~@(mapcat (fn [[args+1 expr]] `((.== ~(peek args+1) ~default-value) ~expr)) fixed-arities-expr)
               true ~(if vararg-params
                       (if-some [[first-rest-arg :as rest-args] (seq (drop base-vararg-arity call-args))]
                         `(if (.== ~first-rest-arg ~default-value)
                            (. ~this ~(resolve-dart-mname 'cljd.core/IFn '-invoke (inc (count fixed-args)))
                               ~@fixed-args)
                            (. ~this ~vararg-mname ~@fixed-args
                               (.toList
                                (.takeWhile ~(tagged-literal 'dart (vec rest-args))
                                            (fn [e#] (.!= e# ~default-value))))))
                         `(. ~this ~vararg-mname ~@fixed-args nil))
                       `(. ~this ~(resolve-dart-mname 'cljd.core/IFn '-invoke (inc (count fixed-args)))
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
                           (. ~this ~(resolve-dart-mname 'cljd.core/IFn '-invoke (inc (count fixed-args)))
                              ~@fixed-args)
                           ~(if vararg-params
                              `(. ~this ~vararg-mname ~@fixed-args ~more-param)
                              `(throw (dart:core/ArgumentError. "No matching arity"))))
                        (reverse
                         (concat
                          (for [args+1 (next (reductions conj [] base-args))]
                            [args+1 `(throw (dart:core/ArgumentError. "No matching arity"))])
                          fixed-arities-expr)))))))]
    (emit
     `(~'reify cljd.core/IFn
        ~@fixed-invokes
        ~@invoke-exts
        ~@vararg-invokes
        ~@(some-> invoke-more list)
        ~@call+apply)
     env)))

(defn- emit-dart-fn [dart-fn-name [params & body] env]
  (let [{:keys [fixed-params opt-kind opt-params]} (parse-dart-params params)
        dart-fixed-params (map dart-local fixed-params)
        dart-opt-params (for [[p d] opt-params]
                          [(case opt-kind
                             :named p ; here p must be a valid dart identifier
                             :positional (dart-local p))
                           (emit d env)])
        env (into env (zipmap (concat fixed-params (map first opt-params))
                              (concat dart-fixed-params (map first dart-opt-params))))
        dart-body (emit (cons 'do body) env)
        recur-params (when (has-recur? dart-body) dart-fixed-params)
        dart-fixed-params (if recur-params
                            (map dart-local fixed-params)
                            dart-fixed-params)
        dart-body (cond->> dart-body
                    recur-params
                    (list 'dart/loop (map vector recur-params dart-fixed-params)))
        dart-fn
        (list 'dart/fn dart-fixed-params opt-kind dart-opt-params dart-body)]
    (if dart-fn-name
      (list 'dart/let [[dart-fn-name dart-fn]] dart-fn-name)
      dart-fn)))

(defn emit-fn* [[_ & bodies] env]
  (let [name (when (symbol? (first bodies)) (first bodies))
        bodies (cond-> bodies name next)
        env (cond-> env name (assoc name (dart-local name)))
        [body & more-bodies :as bodies] (cond-> bodies (vector? (first bodies)) list)]
    (if (or more-bodies (variadic? body))
      (emit-ifn (some-> name env) bodies env)
      (emit-dart-fn (some-> name env) body env))))

(defn emit-method [[mname {[this-param & fixed-params] :fixed-params :keys [opt-kind opt-params]} & body] env]
  ;; params destructuring will be added by a macro
  ;; opt-params need to have been fully expanded to a list of [symbol default]
  ;; by the macro
  (let [dart-fixed-params (map dart-local fixed-params)
        dart-opt-params (for [[p d] opt-params]
                          [(case opt-kind
                             :named p ; here p must be a valid dart identifier
                             :positional (dart-local p))
                           (emit d env)])
        env (into (assoc env this-param 'this)
                  (zipmap (concat fixed-params (map first opt-params))
                          (concat dart-fixed-params (map first dart-opt-params))))
        dart-body (emit (cons 'do body) env)
        recur-params (when (has-recur? dart-body) dart-fixed-params)
        dart-fixed-params (if recur-params
                            (map dart-local fixed-params)
                            dart-fixed-params)
        dart-body (cond->> dart-body
                    recur-params
                    (list 'dart/loop (map vector recur-params dart-fixed-params)))
        mname (with-meta mname (dart-meta mname))]
    [mname dart-fixed-params opt-kind dart-opt-params (nil? (seq body)) dart-body]))

(defn closed-overs [emitted env]
  (into #{} (keep (set (vals env))) (tree-seq coll? seq emitted)))

(defn method-closed-overs [[mname dart-fixed-params opt-kind dart-opt-params _ dart-body] env]
  (reduce disj (closed-overs dart-body env) (cons 'this (concat dart-fixed-params (map second dart-opt-params)))))

(declare write-class)

(defn do-def [nses sym m]
  (let [the-ns (:current-ns nses)
        m (assoc m :ns the-ns :name sym :meta (merge (meta sym) (:meta m)))]
    (assoc-in nses [the-ns sym] m)))

(defn- emit-class-specs [opts specs env]
  (let [{:keys [extends] :or {extends 'Object}} opts
        [ctor-op base & ctor-args :as ctor]
        (macroexpand env (cond->> extends (symbol? extends) (list 'new)))
        ctor-meth (when (= '. ctor-op) (first ctor-args))
        ctor-args (cond-> ctor-args (= '. ctor-op) next)
        classes (filter #(and (symbol? %) (not= base %)) specs) ; crude
        methods (remove symbol? specs)  ; crude
        mixins(filter (comp :mixin meta) classes)
        ifaces-or-protocols (remove (comp :mixin meta) classes) ; TODO resolve protocol ifaces
        ifaces (map #(let [[tag x] (resolve-symbol % env)]
                       (case tag
                         :def (case (:type x)
                                :class %
                                :protocol (symbol (name (:ns x)) (name (:iface x)))) ; should be qualified?
                         :dart %)) ifaces-or-protocols)
        need-nsm (and (seq ifaces) (not-any? (fn [[m]] (case m noSuchMethod true nil)) methods))
        dart-methods (map #(emit-method % env) methods)]
    {:extends (emit base env)
     :implements (map #(emit % env) ifaces)
     :with mixins
     :super-ctor
     {:method ctor-meth ; nil for new
      :args ctor-args}
     :methods dart-methods
     :nsm need-nsm}))

(defn emit-reify* [[_ opts & specs] env]
  (let [class (emit-class-specs opts specs env)
        [positional-ctor-args named-ctor-args] (-> class :super-ctor :args split-args)
        positional-ctor-params (repeatedly (count positional-ctor-args) #(dart-local "param"))
        named-ctor-params (map dart-local (take-nth 2 named-ctor-args))
        class-name (dart-global "Reify")  ; TODO change this to a more telling name
        closed-overs (transduce (map #(method-closed-overs % env)) into #{}
                                (:methods class))
        class (-> class
                  (assoc :name class-name
                         :fields closed-overs
                         :ctor-params
                         (concat
                          (map #(list '. %) closed-overs)
                          positional-ctor-params
                          named-ctor-params))
                  (assoc-in [:super-ctor :args]
                            (concat positional-ctor-params
                                    (interleave (take-nth 2 named-ctor-args)
                                                named-ctor-params))))
        reify-ctor (concat ['new class-name] positional-ctor-args (take-nth 2 (next named-ctor-args)))
        reify-ctor-call (list*
                         'new class-name
                         (concat closed-overs
                                 positional-ctor-args
                                 (take-nth 2 (next named-ctor-args))))]
    (swap! nses do-def class-name
           {:dart/name class-name
            :type :class
            :dart/code (with-out-str (write-class class))})
    (emit reify-ctor-call (into env (zipmap closed-overs closed-overs)))))

(defn- ensure-dart-expr
  "If dart-expr is suitable as an expression (ie liftable returns nil),
   its emission is returned as is, otherwise a IIFE (thunk invocation) is returned."
  [dart-expr]
  (if-some [[bindings dart-expr] (liftable dart-expr)]
    (list (list 'dart/fn () :positional () (list 'dart/let bindings dart-expr)))
    dart-expr))

(defn- emit-strict-expr
  "If expr is suitable as an expression (ie liftable of its emission returns nil),
   its emission is returned as is, otherwise a IIFE (thunk invocation) is returned."
  [expr env]
  (let [dart-expr (emit expr env)]
    (if-some [[bindings dart-expr] (liftable dart-expr)]
      (list (list 'dart/fn () :positional () (list 'dart/let bindings dart-expr)))
      dart-expr)))

(declare write-top-dartfn write-top-field)

(defn emit-defprotocol* [[_ pname spec] env]
  (let [dartname (munge pname)]
    (swap! nses do-def pname
      (assoc spec
        :dart/name dartname
        :dart/code (with-out-str (write-top-field dartname (emit (list 'new (:impl spec)) {})))
        :type :protocol))))

(defn emit-deftype* [[_ class-name fields opts & specs] env]
  (let [mclass-name (with-meta
                      (or (:dart/name (meta class-name)) (munge class-name))
                      (select-keys (meta class-name) [:abstract])) ; TODO shouldn't it be dne by munge?
        env (into {} (for [f fields
                           :let [{:keys [mutable]} (meta f)]]
                       [f (vary-meta (munge f) assoc :dart/mutable mutable)]))
        _ (swap! nses do-def class-name {:dart/name mclass-name :type :class})
        class (emit-class-specs opts specs env)
        [positional-ctor-args named-ctor-args] (-> class :super-ctor :args split-args)
        class (-> class
                  (assoc :name mclass-name
                         :fields (vals env)
                         :ctor-params (map #(list '. %) (vals env)))
                  (assoc-in [:super-ctor :args]
                            (concat (map #(-> % (emit env) ensure-dart-expr) positional-ctor-args)
                                    (->> named-ctor-args (partition 2)
                                         (mapcat (fn [[name arg]] [name (-> arg (emit env) ensure-dart-expr)]))))))]
    (swap! nses do-def class-name
           {:dart/name mclass-name
            :type :class
            :dart/code (with-out-str (write-class class))})
    (emit class-name env)))

(defn emit-extend-type-protocol* [[_ class-name protocol-ns protocol-name extension-instance] env]
  ;; TODO qualify extension-instance
  (let [{:keys [current-ns] {proto-map protocol-name} protocol-ns}
        (swap! nses assoc-in [protocol-ns protocol-name :extensions class-name] extension-instance)]
    (swap! nses assoc :current-ns protocol-ns)
    (emit (expand-protocol-impl proto-map) env)
    (swap! nses assoc :current-ns current-ns)))

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
        dartname (munge sym)]
    (swap! nses do-def sym {:dart/name dartname :type :field}) ; predecl so that the def is visible in recursive defs
    (if (and (seq? expr) (= 'fn* (first expr)) (not (symbol? (second expr))))
      (let [dart-fn (emit expr env)]
        (swap! nses do-def sym
          {:dart/name (with-meta dartname
                        (into {:dart/fn-type
                               (case (first dart-fn)
                                 dart/fn :native
                                 :ifn)}
                          (meta dartname)))
           :type :dartfn
           :dart/code (with-out-str (write-top-dartfn dartname dart-fn))}))
      (swap! nses do-def sym
        {:dart/name dartname
         :type :field
         :dart/code (with-out-str (write-top-field dartname (emit expr env)))}))
    (emit sym env)))

(defn ensure-import [the-ns]
  (let [{:keys [current-ns] :as all-nses} @nses
        the-lib (:lib (all-nses the-ns))]
    (or
     (some (fn [[alias {:keys [lib]}]] (when (= lib the-lib) alias))
           (:imports (all-nses current-ns)))
     (let [[_ last-segment] (re-matches #".*?([^.]+)$" (name the-ns))
           alias (str (dart-global last-segment))]
       (swap! nses update-in [current-ns :imports] assoc alias {:lib the-lib :ns the-ns})
       alias))))

(defn emit-fully-qualified-symbol [x]
  (let [x-ns (some-> x namespace symbol)
        x-ns (case x-ns clojure.core 'cljd.core x-ns)
        ns-map (@nses x-ns)]
    (when-some [{dart-name :dart/name} (get ns-map (symbol (name x)))]
      (if (= (:current-ns @nses) x-ns)
        dart-name
        (symbol (str (ensure-import x-ns) "." dart-name))))))

(defn emit-symbol [x env]
  (let [x (if (= "clojure.core" (namespace x)) (symbol "cljd.core" (name x)) x)
        [tag v] (resolve-symbol x env)]
    (->
      (case tag
        :local v
        :def (:dart/name v) ; TODO qualify
        :dart v
        #_"TODO next form should throw"
        (munge (symbol nil (str "GLOBAL_" x))))
      (vary-meta merge (dart-meta x)))))

(defn emit-quoted [[_ x] env]
  (cond
    (coll? x) (emit-coll #(list 'quote %) x env)
    (symbol? x) (emit (list 'cljd.core/symbol (namespace x) (name x)) env)
    :else (emit x env)))

(defn ns-to-lib [ns-name]
  (str *target-subdir* (replace-all (name ns-name) #"[.]" {"." "/"}) ".dart"))

(declare compile-namespace)

(defn- import-to-require [spec]
  (cond
    (symbol? spec) (let [[_ ns id] (re-matches (name spec) #"(.+)\.(.+)")]
                     [(symbol ns) :refer [(symbol id)]])
    (sequential? spec) [(first spec) :refer (rest spec)]
    :else (throw (ex-info (str "Unsupported import spec: "
                               (pr-str spec)) {:spec spec}))))

(defn- use-to-require [spec]
  (if (sequential? spec)
    (let [lib (first spec)
          {:keys [only rename]} (apply hash-map (rest spec))]
      [lib :refer only :rename rename])))

(defn emit-ns [[_ ns-sym & ns-clauses] _]
  (let [ns-clauses (drop-while #(or (string? %) (map? %)) ns-clauses) ; drop doc and meta for now
        ns-map
        (reduce
         (partial merge-with into)
         (assoc ns-prototype :lib (ns-to-lib ns-sym))
         (for [[directive & specs] ns-clauses
               :let [f (case directive
                         :require #(if (sequential? %) % [%])
                         :import import-to-require
                         :use use-to-require
                         :refer-clojure nil)]
               :when f ; TODO fix refer-clojure
               spec specs
               :let [[lib & {:keys [as refer rename]}] (f spec)
                     alias (name (dart-global (or as "lib")))
                     clj-ns (when-not (string? lib) lib)
                     dartlib (else->>
                              (if (string? lib) lib)
                              (if-some [{:keys [lib]} (@nses lib)] lib)
                              (compile-namespace lib))
                     to-dart-sym (if clj-ns munge identity)]]
           (cond-> {:imports {alias {:lib dartlib :ns clj-ns}}
                    :mappings (into {} (for [[from to] (concat (zipmap refer refer) rename)]
                                         [from (with-meta (symbol (str alias "." (to-dart-sym to)))
                                                 {:dart/fn-type (when (nil? clj-ns) :native)})]))}
             as (assoc :aliases {(name as) alias}))))]
    (swap! nses assoc ns-sym ns-map :current-ns ns-sym)))

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
                       env (cond-> (assoc env e (dart-local e))
                             st (assoc st (dart-local st)))]]
             [classname (env e) (some-> st env) (emit-no-recur (cons 'do exprs) env)])
           (some-> finally-body (conj 'do) (emit-no-recur env)))))

(defn emit-throw [[_ expr] env]
  ;; always emit throw as a statement (in case it gets promoted to rethrow)
  (list 'dart/let [[nil (list 'dart/throw (emit expr env))]] nil))

(defn emit-dart-is [[_ x type] env]
  (when (or (not (symbol? type)) (env type))
    (throw (ex-info (str "The second argument to dart-is? must be a literal type. Got: " (pr-str type)) {:type type})))
  (let [x (emit x env)]
    (if-some [[bindings x] (liftable x)]
      (list 'dart/let bindings (list 'dart/is x (emit type env))) ; TODO emit-type that doesn't munge, only resolve
      (list 'dart/is x (emit type env)))))

(defn- ensure-new-special [x]
  (case (and (symbol? x) (name x))
    "dart-is?" 'dart-is?
    x))

(defn emit
  "Takes a clojure form and a lexical environment and returns a dartsexp."
  [x env]
  (let [x (macroexpand env x)
        dart-x
        (cond
          (symbol? x) (emit-symbol x env)
          #?@(:clj [(char? x) (str x)])
          (or (number? x) (boolean? x) (string? x)) x
          (keyword? x) (emit (list 'cljd.core/keyword (namespace x) (name x)) env)
          (nil? x) nil
          (and (list? x) (nil? (seq x))) (emit 'cljd.core/empty-list env)
          (and (vector? x) (nil? (seq x))) (emit 'cljd.core/empty-persistent-vector env)
          (seq? x)
          (let [emit (case (-> (first x) ensure-new-special)
                       . emit-dot
                       set! emit-set!
                                        ; have to think twice about hwo to handle namespacing of new specials by syntax quote -cgrand
                       (cljd.core/dart-is? dart-is?) emit-dart-is
                       throw emit-throw
                       new emit-new
                       ns emit-ns
                       try emit-try
                       case* emit-case*
                       quote emit-quoted
                       do emit-do
                       let* emit-let*
                       loop* emit-loop*
                       recur emit-recur
                       if emit-if
                       fn* emit-fn*
                       def emit-def
                       reify* emit-reify*
                       deftype* emit-deftype*
                       defprotocol* emit-defprotocol*
                       extend-type-protocol* emit-extend-type-protocol*
                       emit-fn-call)]
            (emit x env))
          (and (tagged-literal? x) (= 'dart (:tag x))) (emit-dart-literal (:form x) env)
          (coll? x) (emit-coll x env)
          :else (throw (ex-info (str "Can't compile " (pr-str x)) {:form x})))]
    (cond-> dart-x
      (or (symbol? dart-x) (coll? dart-x)) (with-meta (infer-type dart-x)))))

(defn bootstrap-eval
  [x]
  (let [x (binding [*bootstrap-eval* true] (macroexpand {} x))]
    (when (seq? x)
      (case (first x)
        ns (let [the-ns (second x)]
             (emit-ns x {})
             (remove-ns (ns-name (ghost-ns))) ; ensure we don't have a dirty ns
             (binding [*ns* (ghost-ns)]
               (refer-clojure)
               (alias the-ns (ns-name *ns*))))
        def (let [sym (second x)
                  {:keys [macro bootstrap inline]} (meta sym)]
              (cond
                (or macro bootstrap) (binding [*ns* (ghost-ns)] (eval x))
                inline (binding [*ns* (ghost-ns)] (eval (list 'def sym nil)))))
        nil))))

(defn emit-test [expr env]
  (binding [*locals-gen* {}]
    (emit expr env)))

;; WRITING
(defn declaration [locus] (:decl locus ""))
(defn declared [locus]
  ; merge to conserve custom attributes
  (merge  (dissoc locus :fork :decl) (:fork locus)))

(def statement-locus
  {:pre ""
   :post ";\n"})

(defn named-fn-locus [name]
  {:pre (str (:dart/type (meta name) "dc.dynamic") " " name)
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

(defn var-locus [varname]
  {:pre (str (-> varname meta (:dart/type "var")) " " varname "=")
   :post ";\n"
   :decl (str (-> varname meta (:dart/type "var")) " " varname ";\n")
   :fork (assignment-locus varname)})

(declare write)

(defn write-top-dartfn [sym x]
  (case (first x)
    dart/fn (do
              (print (-> sym meta (:dart/type "dc.dynamic")) (name sym))
              (write x expr-locus)
              (print "\n"))
    (write x (var-locus (name sym)))))

(defn write-top-field [sym x]
  (write (ensure-dart-expr x) (var-locus (name sym))))

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
        (replace-all s #"([\x00-\x1f])|[$\"]"
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
                                     :cld
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
  (doseq [p fixed-params] (print p) (print ", "))
  (when (seq opt-params)
    (print (case opt-kind :positional "[" "{"))
    (doseq [[p d] opt-params]
      (print p "= ")
      (write d arg-locus))
    (print (case opt-kind :positional "]" "}")))
  (print ")"))

(defn write-class [{class-name :name :keys [extends implements with fields ctor-params super-ctor methods nsm]}]
  (let [abstract (-> class-name meta :abstract)]
    (when abstract (print "abstract "))
    (print "class" class-name)
    (some->> extends (print " extends"))
    (some->> implements seq (str/join ", ") (print " implements"))
    (some->> with seq (str/join ", ") (print " with"))
    (print " {\n")
    (doseq [field fields
            :let [{:keys [dart/mutable]} (meta field)]]
      (print (str (if mutable "" "final ") (-> field meta (:dart/type (if mutable "var" ""))) " " field ";")))
    (newline)

    (print (str class-name "("))
    (doseq [p ctor-params]
      (print (if (seq? p) (str "this." (second p)) p))
      (print ", "))
    (print "):super")
    (some->> super-ctor :method (str ".") print)
    (write-args (:args super-ctor))
    (print ";\n")

    (doseq [[mname fixed-params opt-kind opt-params no-explicit-body body] methods
            :let [{:dart/keys [getter setter]} (meta mname)]]
      (newline)
      (when-not setter
        (print (:dart/type mname "dc.dynamic"))
        (print " "))
      (cond
        getter (print "get ")
        setter (print "set "))
      (print mname)
      (when-not getter (write-params fixed-params opt-kind opt-params))
      (if (and abstract no-explicit-body)
        (print ";\n")
        (do
          (print "{\n")
          (write body return-locus)
          (print "}\n"))))

    (when nsm
      (newline)
      (print "dc.dynamic noSuchMethod(i)=>super.noSuchMethod(i);\n"))

    (print "}\n")))

(def ^:private ^:dynamic *caught-exception-symbol* nil)

(defn infer-type [x]
  (let [m (meta x)]
    (->
     (cond
       (:dart/inferred m) m
       (boolean? x) {:dart/type "dc.bool" :dart/truth :boolean}
       (seq? x)
       (case (first x)
         dart/.
         (let [[_ a meth b] x]
           (case (name meth)
             ("!" "<" ">" "<=" ">=" "==" "!=" "&&" "|" "^")
             {:dart/type "dc.bool" :dart/truth :boolean}
             nil))
         dart/is {:dart/type "dc.bool" :dart/truth :boolean}
         dart/as (let [[_ _ type] x] {:dart/type type
                                      :dart/truth (dart-type-truthiness type)})
         (when-some [{:keys [dart/type dart/truth]} (infer-type (first x))]
           {:dart/type type
            :dart/truth (or truth (dart-type-truthiness type))}))
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
      (let [[_ fixed-params opt-kind opt-params body] x]
        (print (:pre locus))
        (write-params fixed-params opt-kind opt-params)
        (print "{\n")
        (write body return-locus)
        (print "}")
        (print (:post locus)))
      dart/let
      (let [[_ bindings expr] x]
        (or
         (some (fn [[v e]] (write e (cond (nil? v) statement-locus
                                          (and (seq? e) (= 'dart/fn (first e))) (named-fn-locus v)
                                          :else (var-locus v))))
               bindings)
          (write expr locus)))
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
        (write type expr-locus)
        (print ")")
        (print (:post locus)))
      dart/is
      (let [[_ expr type] x]
        (print (:pre locus))
        (print "(")
        (write expr expr-locus)
        (print " is ")
        (write type expr-locus)
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
            _ (print "default:\n")
            exit (and (write default-expr locus) exit)]
        (print "}\n")
        exit)
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
                         (map (fn [v vs] (when (vs v) [v (dart-local v)])) ; TODO using dart-local in write is going to cause double munging
                              loop-bindings vars-usages))]
          (doseq [[v e] (map vector loop-bindings exprs)]
            (write e (if-some [tmp (tmps v)] (var-locus tmp) (assignment-locus v))))
          (doseq [[v tmp] tmps]
            (write tmp (assignment-locus v)))
          (print "continue;\n")
          true))
      dart/set!
      (let [[_ target val] x] ; TODO it's dubious that locus isn't used
        (write val (assignment-locus
                    (if (symbol? target)
                      target
                      (let [[op obj fld] target]
                        (case op dart/.- (str obj "." fld)))))))
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
          ;; operators, ! and != are cljd tricks
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
          ("<" ">" "&" "&&" "<=" ">=" "==" "!=" "+" "~/" "/" "*" "%" "<<" ">>" ">>>")
          (do
            (write obj paren-locus)
            (print meth)
            (write (first args) paren-locus))
          ("|" "^" #_"&")
          (do
            (write obj paren-locus)
            (print (str meth meth))
            (write (first args) paren-locus))
          ;; else plain method
          (do
            (write obj expr-locus)
            (print (str "." meth))
            (write-args args)))
        (print (:post locus))
        (:exit locus))
      ;; native fn call
      (let [[f & args] x
            {:keys [dart/fn-type]} (meta f)]
        (print (:pre locus))
        (write f expr-locus)
        (write-args args)
        (print (:post locus))
        (:exit locus)))
    :else (do
            (print (:pre locus))
            (write-literal x)
            (print (:post locus))
            (:exit locus))))

;; Compile clj -> dart file
(defn dump-ns [ns-map]
  (doseq [[alias {:keys [lib]}] (:imports ns-map)]
    (print "import ")
    (write-string-literal lib) ;; TODO: relativize local libs (nses)
    (print " as ")
    (print alias)
    (print ";\n"))
  (print "\n")
  (doseq [[sym v] ns-map
          :when (symbol? sym)
          :let [{:keys [dart/code]} v]
          :when code]
    (print code)))

(defn load-input [in]
  #?(:clj
     (binding [*data-readers* (assoc *data-readers* 'dart #(tagged-literal 'dart %))
               *reader-resolver*
               (reify clojure.lang.LispReader$Resolver
                 (currentNS [_] (:current-ns @nses))
                 (resolveClass [_ sym] nil) ; should check for imported classes
                 (resolveAlias [_ sym]
                   (let [{:keys [current-ns] :as nses} @nses
                         aliases (:aliases (nses current-ns))]
                     (some-> (aliases (name sym)) symbol)))
                 (resolveVar [_ sym] nil))]
         (let [in (clojure.lang.LineNumberingPushbackReader. in)]
           (loop []
             (let [form (read {:eof in :read-cond :allow :features #{:cljd}} in)]
               (when-not (identical? form in)
                 (binding [*locals-gen* {}] (emit form {}))
                 (recur))))))))

(defn bootstrap-load-input [in]
  #?(:clj
     (binding [*data-readers* (assoc *data-readers* 'dart #(tagged-literal 'dart %))
               *reader-resolver*
               (reify clojure.lang.LispReader$Resolver
                 (currentNS [_] (:current-ns @nses))
                 (resolveClass [_ sym] nil) ; should check for imported classes
                 (resolveAlias [_ sym]
                   (let [{:keys [current-ns] :as nses} @nses
                         aliases (:aliases (nses current-ns))]
                     (some-> (aliases (name sym)) symbol)))
                 (resolveVar [_ sym] nil))]
         (let [in (clojure.lang.LineNumberingPushbackReader. in)]
           (loop []
             (let [form (read {:eof in :read-cond :allow} in)]
               (when-not (identical? form in)
                 (binding [*locals-gen* {}] (bootstrap-eval form))
                 (recur))))))))

(defn compile-input [in]
  (load-input in)
  (let [{:keys [current-ns] :as all-nses} @nses
        libname (:lib (all-nses current-ns))]
    (with-open [out (-> (java.io.File. *lib-path* libname)
                        (doto (-> .getParentFile .mkdirs))
                        java.io.FileWriter.)]
      (binding [*out* out]
        (dump-ns (all-nses current-ns))))
    (swap! nses assoc-in [current-ns :lib] libname)
    libname))

(defn ns-to-paths [ns-name]
  (let [base (replace-all (name ns-name) #"[.-]" {"." "/" "-" "_"})]
    [(str base ".cljd") (str base ".cljc")]))

(defn find-file
  "Search for a file on the clojure path."
  [filename]
  (first
   (for [dir *clj-path*
         :let [file (java.io.File. dir filename)]
         :when (.exists file)]
     file)))

(defn compile-namespace [ns-name]
  ;; iterate first on file variants then on paths, not the other way!
  (binding [*bootstrap* (bootstrap-nses ns-name)]
    (let [file-paths (ns-to-paths ns-name)]
      (if-some [file (some find-file file-paths)]
        (do
          (when *bootstrap*
            (with-open [in (java.io.FileInputStream. file)]
              (bootstrap-load-input (java.io.InputStreamReader. in "UTF-8"))))
          (with-open [in (java.io.FileInputStream. file)]
            (compile-input (java.io.InputStreamReader. in "UTF-8"))))
        (throw (ex-info (str "Could not locate "
                          (str/join " or " file-paths)
                          " on *clj-path*.")
                 {:ns ns-name}))))))

(comment
  (binding [*clj-path* ["examples/hello-flutter/src"]
            *lib-path* "examples/hello-flutter/lib"]
    (compile-namespace 'hello-flutter.core))

  (time
    (binding [*clj-path* ["clj/src"]
              *lib-path* "lib"]
      (compile-namespace 'cljd.core)))

  (-> @nses (get-in '[cljd.core]
              ))

  ;; 1/ compiler.clj JAVA compiles core.cljd -> core.dart
  ;; 2/ compiler.clj JAVA compiles compiler.clj (with core.dart deps) ->  compilier.dart
  ;; 3/ compilier.dart DART compiles core.cljd (with "old" core.dart as deps)
  ;; -> core'.dart
  ;; somehow it produces an executable cljd compiler
  )

(comment
  (emit-ns '(ns cljd.user

              (:require [cljd.bordeaux :refer [reviews] :as awesome]
                        [cljd.ste :as ste]
                        ["package:flutter/material.dart"]
                        clojure.string)) {})


  (emit '((((fn* [] (fn* [] (fn* [] 42)))))) {})
  ((((dart/fn () () (dart/let () (dart/fn () () (dart/let () (dart/fn () () (dart/let () 42)))))))))
  (write *1 (var-locus "DDDD"))

  (emit '(fn* [x] x) {})
  (dart/fn nil (_$7_) () (dart/let ([x_$8_ _$7_]) x_$8_))
  (write *1 return-locus)

  (emit '(fn* fname [x] 42) {})
  (dart/let [[nil (dart/fn _16623 (_16624) () (dart/let ([_16625 _16624]) 42))]] _16623)
  (write *1 return-locus)

  (emit '((fn* fname [x] 42)) {})
  (dart/let ([nil (dart/fn _16631 (_16632) () (dart/let ([_16633 _16632]) 42))]) (_16631))
  (write *1 return-locus)

  ()

  (emit '(def oo (fn* [x] 42)) {})
  (write *1 return-locus)



  (emit '(def oo1 42) {})


  (emit '(def oo (fn* [x] (if (.-isOdd x) (recur (. x + 1)) x ))) {})
  nses

  (emit '(def oo "caca\n") {})

  (emit '(def oo "docstring" (let [a "caca"] a)) {})

  (write *1 return-locus)

  (emit '(fn* aa [x] x) {})
  (dart/let [[nil (dart/fn _16717 (_16718) () (dart/let ([_16719 _16718]) _16719))]] _16717)

  (emit '(fn* [] (fn* aa [x] x)) {})
  (dart/fn nil () () (dart/let [[nil (dart/fn aa_$9_ (_$10_) () (dart/let ([x_$11_ _$10_]) x_$11_))]] aa_$9_))
  (dart/fn nil () () (dart/let ([nil (dart/fn _18396 (_18397) () (dart/let ([_18398 _18397]) (GLOBAL_do _18398)))]) (GLOBAL_do _18396)))

  (emit '(reify Object (boo [self x & y 33] (.toString self))) {})
  (GLOBAL__22982)

  (emit '(reify Object (boo [self x ... y 33] (.toString self))) {})
  (GLOBAL__22986)
  (write *1 return-locus)

  (emit '(let [x 42] (reify Object (boo [self] (str x "-" self)))) {})
  (dart/let ([_22991 42]) (GLOBAL__22992 _22991))

  (emit '(let [x 42] (reify Object (boo [self] (let [x 33] (str x "-" self))))) {})
  (dart/let ([x_$4_ 42]) (_reify_$5_))

  (emit '[1 2 3] {})
  (GLOBAL_cljd.core/vec [1 2 3])
  (write *1 expr-locus)

  (emit '[1 (inc 1) [1 1 1]] {})
  (GLOBAL_cljd.core/vec [1 (GLOBAL_inc 1) (GLOBAL_cljd.core/vec [1 1 1])])

  (emit ''[1 (inc 1) [1 1 1]] {})

  (GLOBAL_cljd.core/vec [1 (GLOBAL_inc 1) (GLOBAL_cljd.core/vec [1 1 1])])

  (emit '[1 (inc 1) [(let [x 3] x)]] {})
  (dart/let ([_24320 (GLOBAL_inc 1)] [_24318 3] [_24319 (GLOBAL_cljd.core/vec [_24318])]) (GLOBAL_cljd.core/vec [1 _24320 _24319]))
  (write *1 expr-locus)

  (emit '(let [x (try 1 2 3 4 (catch Exception e e1 (print e) 2 3))] x) {})
  (dart/let ([_17563 (dart/try (dart/let ([nil 1] [nil 2] [nil 3]) 4) (catch Exception [_17564 _17565] (dart/let ([nil (GLOBAL_print _17564)] [nil 2]) 3)))]) _17563)
  (write *1 return-locus)

  (emit '(if (try 1 2 3 4 (catch Exception e "noooo") (finally "log me")) "yeahhh") {})
  (write *1 return-locus)

  (emit '(try (catch E e st)) {})
  (dart/try nil ([E e_$19_ nil GLOBAL_st]) nil)
  (write *1 return-locus)

  (emit '(try 42 33 (catch E e st x) (finally (print "boo"))) {})
  (dart/try (dart/let ([nil 42]) 33) ([E e_$24_ st_$25_ GLOBAL_x]) (GLOBAL_print "boo"))
  (write *1 return-locus)

  (emit '[1 (let [x 2] x) 3] {})
  (dart/let ([__$3_ 2]) (GLOBAL_cljd.core/vec [1 __$3_ 3]))
  (dart/let ([_25768 2]) (GLOBAL_cljd.core/vec [1 _25768 3]))

  (emit '[(f) (let [x 2] x) 3] {})
  (dart/let ([_25772 (GLOBAL_f)] [_25771 2]) (GLOBAL_cljd.core/vec [_25772 _25771 3]))


  (emit '(try 1 2 3 4 (catch Exception e st 1 2)) {})
  (dart/try (dart/let ([nil 1] [nil 2] [nil 3]) 4) (catch Exception [e_$4_ st_$5_] (dart/let ([nil 1]) 2)) (catch Exception [e_$6_] GLOBAL_st))
  (write *1 return-locus)

  (emit '(throw 1) {})
  (dart/throw 1)
  (write *1 (var-locus "prout"))

  (emit '(throw (let [a 1] (. a + 3))) {})
  (dart/let ([a_$28_ 1] [_$29_ (dart/. a_$28_ "+" 3)]) (dart/throw _$29_))
  (write *1 return-locus)




  (emit '(let [a (throw 1)] a) {})
  (dart/let ([a_$50_ (dart/let [[nil (dart/throw 1)]] nil)]) a_$50_)
  (write *1 return-locus)

  (emit '(let [a (throw (if x y z))] a) {})
  (dart/let ([a_$41_ (dart/let [[nil (dart/throw (dart/if GLOBAL_x GLOBAL_y GLOBAL_z))]] nil)]) a_$41_)
  (write *1 return-locus)

  (emit '(try (catch E e (throw e))) {})
  (dart/try nil ([E e_$47_ nil (dart/let [[nil (dart/throw e_$47_)]] nil)]) nil)
  (write *1 return-locus)

  (emit '(loop [] (recur)) {})
  (dart/loop [] (dart/recur))
  (write *1 return-locus)

  (emit '(loop [] (if x (recur))) {})
  (dart/loop [] (dart/if GLOBAL_x (dart/recur) nil))
  (write *1 return-locus)
  (write *2 statement-locus)

  (emit '(reify Object (^:getter hashCode [] 42)
           (^:setter foo [this x] (println x))
           (meth [a b] "regular method")) {})
  (_reify_$8_)

  (emit '(deftype MyClass [^:mutable ^List a b ^Map c]
           :extends (ParentClass. (+ a b) (if 1 2 3))
           Object
           (meth [_ b] (set! a (if (rand-bool) 33 42)))
           (meth2 [this b] (set! (.-a this) "yup"))
           (^:getter hashCode [_] (let [^num n 42] n))) {})


  (emit '(defprotocol IProtocol_ (meth [a] [a b] [a b c]) (-coucou [a])) {})
  (dart/let [[nil IProtocol_$UNDERSCORE_] [nil meth] [nil _coucou]] IProtocol_$UNDERSCORE_)

  (emit '(defprotocol IMarker "This protocol is only a marker") {})
  (dart/let [[nil IMarker]] IMarker)

  (emit '(defprotocol IMarker2 "Docstring" (meth [one] [one two] "Docstring") (ops [one] [one two]) (opa [one] "Coucou")) {})
  (dart/let [[nil IMarker] [nil meth] [nil ops] [nil opa]] IMarker)

  (emit '(deftype MyClass [^:mutable ^List a b ^Map c]
           :extends (ParentClass. (+ a b) (if 1 2 3))
           IMarker
           IProtocol_
           (meth [a] "a")
           (meth [b c] "e")
           (meth [c d e] "oo")) {})
  (dart/let [[nil MyClass]] __$GT_MyClass)

  (get-in @nses '[cljd.core IProtocol])
  nses

  (macroexpand-1 {} '(defn aaa "docstring2" [ooo] "content"))

  (emit '(defn aaa "docstring2" [ooo] "content") {})
  (emit '(def ooo "docstirng" 42) {})

  (emit '(dart-is? 0 num) {})
  (dart/is 0 dart:core/num)

  (emit `(str (case x# 12 "hello" (13 14) "bye")) {})

  (write *1 return-locus)

  (emit `(case 12  12 "hello" (13 14) "bye") {})

  (macroexpand-1 {} '(case 12 12 "hello"))

  (clojure.core/let [test__6312__auto__ 12] (cljd.core/case test__6312__auto__ 12 "hello"))

    )


(comment


  (let [env  {}
        threshold 2 ; means up to (dec threshold) fn args incl
        f '(fn* ([] "no")
                ([a b c] "three")
                ([a b d e f g & c] "ahah" "hihi")
                #_([aa ab ac ad] "ohoh") )
        #_#_f '(fn* ([& more] "hahhahaha" "ohohohoh" more))
        #_f #_'(fn* ([a] body))
        #_'(fn* [a] body)
        #_'(fn* ([] "0") ([a] a))
        #_'(fn*  ([a] body) ([a & more] body))
        #_'(fn* ([] "oups" "coucou") ([a b c d e f & prefix] "e f g" prefix))
        #_'(fn* ([] "oups" "coucou") ([a] "coucou") ([a b c prefix] "bebe " prefix) ([a b c d e f prefix] "e f g" prefix))])


  (emit '(.add (List.) 1) {})
  (write *1 statement-locus)


  (emit-test '(defprotocol IFn
                "Protocol for adding the ability to invoke an object as a function.
  For example, a vecttor can also be used to look up a value:
  ([1 2 3 4] 1) => 2"
                (-invoke
                  [this]
                  [this a]
                  [this a b]
                  [this a b c]
                  [this a b c d]
                  [this a b c d e]
                  [this a b c d e f]
                  [this a b c d e f g]
                  [this a b c d e f g h]
                  [this a b c d e f g h i])
                (-invoke-more [this a b c d e f g h i rest])) {})


  (macroexpand-1 {} '(defprotocol IFn
                       "Protocol for adding the ability to invoke an object as a function.
  For example, a vector can also be used to look up a value:
  ([1 2 3 4] 1) => 2"
                       (-invoke
                         [this]
                         [this a]
                         [this a b]
                         [this a b c]
                         [this a b c d]
                         [this a b c d e]
                         [this a b c d e f]
                         [this a b c d e f g]
                         [this a b c d e f g h]
                         [this a b c d e f g h i]
                         [this a b c d e f g h i j])
                       (-invoke-more [this a b c d e f g h i j rest])))



  nses







  ;; S0 : sources du compilo qui interprète abstract et qui utilise abstract
  ;; C0 compilo résultatnt de S0
  ;; S1 : S0 sauf que le compilo a été modifié pour comprendre recondite et qui utilise abstract
  ;; C0(S1) -> C1
  ;; S2 : S1 sauf que les usages de asbtract ont ét traduits en recondite
  ;; C1(S2) -> C2

  ;; S0 : compilo c qui target x64
  ;; S1 : compilo c qui target ARM
  ;; C0(S1) -> C1 compilateur c (binaire x64) qui génère du ARM
  ;; S2 : tweaks sur S1 pour accomoder le changementde plateforme ()
  ;; C1(S2) -> C2 compilo c (binaire arm ) qui génère du arm


  (emit '(deftype MyClass [^:mutable ^List a b ^Map c]
           :extends (ParentClass. (+ a b) (if 1 2 3))
           IMarker
           IProtocol_
           (meth [a] "a")
           (meth [b c] "e")
           (meth [c d e] "oo")) {})

  (macroexpand-1 {} '(deftype MyClass [^:mutable ^List a b ^Map c]
                       :extends (ParentClass. (+ a b) (if 1 2 3))
                       IMarker
                       IProtocol_
                       (meth [a] "a")
                       (meth [b c] "e")
                       (meth [c d e] "oo")))

  (emit '(deftype*
           MyClass
           [^{:tag List, :mutable true} a b ^Map c]
           {:extends (ParentClass. (+ a b) (if 1 2 3))}
           IMarker
           IProtocol_
           (meth (a) :positional () "a")
           (meth (b c) :positional () "e")
           (meth (c d e) :positional () "oo")) {})

  (emit-test
   '(defprotocol IFn
      "Protocol for adding the ability to invoke an object as a function.
  For example, a vecttor can also be used to look up a value:
  ([1 2 3 4] 1) => 2"
      (-invoke
        [this]
        [this a]
        [this a b]
        [this a b c]
        [this a b c d]
        [this a b c d e]
        [this a b c d e f]
        [this a b c d e f g]
        [this a b c d e f g h]
        [this a b c d e f g h i])
      (-invoke-more [this a b c d e f g h i rest])))
  (dart/let [[nil IFn] [nil _invoke] [nil _invoke_more]] IFn)


  nses
  (emit-test '(let [a 42 b 43 c (fn ([] "coucou") ([d] b))] (c)))
  (dart/let [[a_$1_ 42] [b_$1_ 43] [c_$1_ (_reify_$35_ b_$1_)]] (c_$1_))
  (macroexpand-1 {} '(defprotocol IFn
                       "Protocol for adding the ability to invoke an object as a function.
  For example, a vecttor can also be used to look up a value:
  ([1 2 3 4] 1) => 2"
                       (-invoke
                         [this]
                         [this a]
                         [this a b]
                         [this a b c]
                         [this a b c d]
                         [this a b c d e]
                         [this a b c d e f]
                         [this a b c d e f g]
                         [this a b c d e f g h]
                         [this a b c d e f g h i]
 #_                        [this a b c d e f g h i j])
                       (-invoke-more [this a b c d e f g h i #_j rest])))


  (emit '(fn*
          ([thiss a b c d e f g & i]
           (if (dart-is? thiss IFn) (. thiss _invoke$10 a b c d e f g h i)))
          ([thiss a b c d e f g]
           (if (dart-is? thiss IFn) (. thiss _invoke$8 a b c d e f g)))) {})


  (emit-test '(if (f) then else) {})
  (dart/let [[_test_$1_ (GLOBAL_f)]] (dart/if _test_$1_ GLOBAL_then GLOBAL_else))
  (write *2 (var-locus 'V))

  (emit-test
   '(let [a 42]
      (reify Object (meth [_] a))))
(dart/let [[a_$1_ 42]] (_reify_$34_ a_$1_))

(run! #(emit-test % {}) '[
(defprotocol IFn
      "Protocol for adding the ability to invoke an object as a function.
  For example, a vecttor can also be used to look up a value:
  ([1 2 3 4] 1) => 2"
      (-invoke
        [this]
        [this a]
        [this a b]
        [this a b c]
        [this a b c d]
        [this a b c d e]
        [this a b c d e f]
        [this a b c d e f g]
        [this a b c d e f g h]
        [this a b c d e f g h i])
      (-invoke-more [this a b c d e f g h i rest]))
                          (defn < [a b] (.< a b))

(defn pos? [a] (.< 0 a))

(defn + [a b] (.+ a b))

(defn - [a b] (.- a b))

(defn nil? [x] (.== nil x))

(defn ^:dart fib [n]
  (if (< 1 n)
    (+ (fib (- n 1)) (fib (- n 2)))
    1))])
nses

  )
