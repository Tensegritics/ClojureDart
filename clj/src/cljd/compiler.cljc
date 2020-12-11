(ns cljd.compiler
  (:refer-clojure :exclude [macroexpand-all macroexpand-1 munge load-file])
  (:require [clojure.core :as cljhost]
            [clojure.string :as str]))

(defmacro ^:private else->> [& forms]
  `(->> ~@(reverse forms)))

(defn string-writer
  ([]
   #?(:clj (let [sb (StringBuilder.)]
             (fn
               ([] (.toString sb))
               ([x] (.append sb (str x)))))
      :cljd (let [sb (StringBuffer.)]
             (fn
               ([] (.toString sb))
               ([x] (.write sb (str x)))))))
  ([s]
   (let [write! (string-writer)]
     (write! s)
     write!)))

(defn out-writer [out]
  #?(:clj
     (fn [x]
       (.write ^java.io.Writer out (str x)))
     :cljd
     (fn [x]
       (.write ^StringSink out (str x)))))

(declare ^:dynamic *emitter*)

(defn open-prior!
  "Inserts a block of code before the current one and write"
  [& xs]
  (let [sb! (string-writer)]
    (run! sb! xs)
    (swap! (:blocks *emitter*) conj sb!)))

(defn close-prior! [& xs]
  (let [{:keys [blocks out!]} *emitter*
        sb! (-> blocks deref peek)]
    (out! (sb!))
    (run! out! xs)
    (swap! blocks pop)))

(defn flush! [& xs]
  (apply close-prior! xs)
  (open-prior!))

(defn emitter [print]
  {:out! print
   :blocks (atom [(string-writer)])})

(defmacro ^:private with-string-emitter [& body]
  `(let [sb!# (string-writer)]
     (binding [*emitter* (emitter sb!#)]
       ~@body
       (flush!))
     (sb!#)))

(defn write! [& xs]
  (let [sb! (-> *emitter* :blocks deref peek)]
    (run! sb! xs)))

(def nses (atom {:current-ns 'user
                 'user {}}))

(defn do-def [nses sym m]
  (assoc-in nses [(:current-ns nses) sym] m))

(defn- roll-leading-opts [body]
  (loop [[k v & more :as body] (seq body) opts {}]
    (if (and body (keyword? k))
        (recur more (assoc opts k v))
        [opts body])))

(defn expand-reify [&  opts+specs]
  (let [[opts specs] (roll-leading-opts opts+specs)]
    (list* 'reify* opts
           (map
            (fn [spec]
              (if (seq? spec)
                (let [[mname arglist & body] spec
                      [positional-args [_ & named-args]] (split-with (complement '#{&}) arglist)
                      triples
                      (loop [triples [] xs named-args]
                        (if-some [[sym & [k :as xs]] xs]
                          (if (symbol? sym)
                            (let [[k [v :as xs]] (if (keyword? k) [k (next xs)] [(keyword sym) xs])
                                  [v xs] (if (symbol? v) [nil xs] [v (next xs)])]
                              (recur (conj triples [k sym v]) xs))
                            (throw (ex-info "Unexpected form, expecting a symbol" {:form sym})))
                          triples))]
                  ; TODO: mname resolution against protocol ifaces
                  (list* mname (into (vec positional-args) triples) body))
                spec))
            specs))))

(defn macroexpand-1 [env form]
  (if-let [[f & args] (and (seq? form) (symbol? (first form)) form)]
    (let [name (name f)
          ;; TODO symbol resolution and macro lookup in cljd
          #?@(:clj [clj-var (ns-resolve (find-ns (:current-ns @nses)) f)])]
      ;; TODO add proper expansion here, before defaults
      (cond
        (env f) form
        #?@(:clj ; macro overrides
            [(= 'ns f) form
             (= 'reify f) (apply expand-reify args)])
        (= '. f) form
        #?@(:clj
            [(-> clj-var meta :macro)
             ; force &env to nil when cross-compiling, should be ok
             (apply @clj-var form nil (next form))]
            :cljd
            [TODO TODO])
        (.endsWith name ".")
        (list* 'new
          (symbol (namespace f) (subs name 0 (dec (count name))))
          args)
        (.startsWith name ".")
        (list* '. (first args) (symbol (subs name 1)) (next args))
        :else form))
    form))

(defn macroexpand-all [env form]
  (let [ex (macroexpand-1 env form)]
    (cond->> ex (not (identical? ex form)) (recur env))))

(defn symbol-literal [sym env]
  (or (env sym)
      (let [nses @nses
            {:keys [mappings aliases] :as current-ns} (nses (:current-ns nses))]
        (else->> (if (current-ns sym) (name sym))
                 (or (get mappings sym))
                 (if-some [alias (get aliases (namespace sym))] (str alias "." (name sym)))
                 #_"TODO next form should throw"
                 (str "XXX" (name sym))))))

(defn replace-all [^String s regexp f]
  #?(:cljd
     (.replaceAllMapped s regexp f)
     :clj
     (str/replace s regexp f)))

(defn emit-string [s]
  (write! \")
  (write! (replace-all s #"([\x00-\x1f])|[$\"]"
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
                             (str "\\" match))))))
  (write! \"))

(def NULL (DartExpr. "null"))

(defn emit-literal [expr env]
  (cond
    (symbol? expr) (symbol-literal expr env)
    (string? expr) (emit-string expr)
    #?@(:clj [(char? expr) (emit-string (str expr))])
    (nil? expr) NULL
    :else (DartExpr. (str expr))))

(def mungees (atom 0))

(deftype DartExpr [^String s]
  Object
  (toString [_] s))

(defn tmpvar [] (DartExpr. (str "_" (swap! mungees inc))))

(defn munge [sym env]
  (DartExpr. (if (env sym) (str (name sym) "__" (swap! mungees inc)) (name sym))))

(declare emit)

(defn lift-expr [expr env & {:keys [name name-env force]}]
  (let [value (not (coll? expr))]
    (if (and value (nil? name) (not force))
      expr
      (let [varname (if name (munge name (or name-env env)) (tmpvar))]
        (open-prior! "var " varname "=" (emit expr env))
        (close-prior! ";\n")
        varname))))

(defn write-args! [args env]
  (write! "(")
  (loop [named false keypos false xs (seq args)]
    (when-some [[x & xs] xs]
      (cond
        (= '& x) (recur true true xs)
        (and named keypos)
        (do
          (write! (name x) ": ") ; this name call is legit
          (recur true false xs))
        :else
        (do
          (write! (emit x env) ", ")
          (recur named true xs)))))
  (write! ")"))

(defmacro with-ret [& body]
  `(let [ret# (tmpvar)]
     (open-prior! ret# "=")
     ~@body
     (close-prior! ";\n")
     ret#))

(defn emit-new [[_ class & args] env]
  (with-ret
    (emit-literal class env)
    (write-args! args env)))

(defn emit-dot [[_ obj fld & args] env]
  (with-ret
    (let [[_ prop name] (re-matches #"(-)?(.*)" (name fld))]
      (write! (emit obj env)"." name)
      (when-not prop
        (emit-args args env)))))

(defn emit-body [expr env]
  (doseq [body (butlast expr)]
    (open-prior!)
    (write! emit body env)
    (close-prior! ";\n"))
  (emit (last expr) env))

(defn emit-let [[_ bindings & body] env]
  (let [env (loop [env env bindings (partition 2 bindings)]
              (if-some [[sym val] (first bindings)]
                (recur (assoc env sym (lift-expr val env :name sym)) (next bindings))
                env))]
    (emit-body body env)))

(defn emit-if [[_ test-expr then else] env]
  (let [ret (tmpvar)
        test (tmpvar)]
    (open-prior! "var " ret ";\nvar " test "=")
    (flush! (emit test-expr env) ";\nif (" test " != null && " test " != false) {\n")
    (write! ret "=" (emit then env))
    (flush! ";\n} else {")
    (write! ret "=" (emit else env))
    (close-prior! ";\n}\n")
    ret))

(defn emit-loop [[_ bindings & body] env]
  (let [env (loop [env env bindings (partition 2 bindings)]
              (if-some [[sym val] (first bindings)]
                (let [varname (lift-expr val env :name sym)]
                  (recur (-> env
                             (assoc sym varname)
                             (update ::loop-bindings (fnil conj []) varname))
                         (next bindings)))
                env))
        ret (tmpvar)]
    (open-prior! "var " ret ";\n")
    (flush "do {\n")
    (write! ret "=" (emit-body body env) ";")
    (close-prior! "break;\n} while(true);\n")
    ret))

(defn emit-recur [[_ & expr] env]
  (doseq [[expr binding] (map vector expr (::loop-bindings env))]
    (write! binding "=" (lift-expr expr env :force true) ";"))
  (flush! "continue;\n")
  'RECUR)

(defn emit-quoted [body env]
  (cond
    (map? body)
    (do (write! "{")
        (doseq [[k v] body]
          (emit-quoted k env)
          (write! ": ")
          (emit-quoted v env)
          (write! ", "))
        (write! "}"))
    (coll? body)
    (do (write! (cond (list? body) "List" (set? body) "Set" :else "Vector"))
        (write! ".from([")
        (doseq [expr body]
          (emit-quoted expr env)
          (write! ", "))
        (write! "])"))
    (symbol? body)
    (write! (str "Symbol(null, " (name body) ")"))
    :else (emit-literal body env)))

(defn emit-collection [body env]
  (if (map? body)
    (do
      (write! "{")
      (doseq [[k v] body]
        (write! (lift-expr k env) ": " (lift-expr v env) ", "))
      (write! "}"))
    (do
      (write! (if (set? body) "Set" "PersistentVector") ".from([")
      (doseq [expr body]
        (write! (lift-expr expr env) ", "))
      (write! "])")))) ; TODO: ret value?

(defn emit-fn-bodies [bodies env]
  (let [bodies (sort #(compare (count (first %1)) (count (first %2))) bodies)
        is-variadic (some #{'&} (first (last bodies)))
        smallest-arity (if (and (= 1 (count bodies)) is-variadic) (- (count (ffirst bodies)) 2) (count (ffirst bodies)))
        biggest-arity (count (first (last bodies)))]
    (write! "(")
    (let [params-alias (into [] (map #(let [tmpparam (tmpvar)]
                                        (when (= % smallest-arity) (write! "["))
                                        (write! tmpparam)
                                        (when (>= % smallest-arity) (write! "=MISSING_ARG"))
                                        (write! ", ")
                                        tmpparam))
                             (range (if is-variadic 8 biggest-arity)))]
      (when (or (< 1 (count bodies)) is-variadic) (write! "]"))
      (flush! ") {\n")
      (let [emit-fn-body
            (fn [[params & body]]
              (let [[params [_ vararg-param]] (split-with (complement #{'&}) params)
                    bodyenv (reduce (fn [bodyenv [param alias]]
                                      (let [varname (lift-expr alias env :name param :name-env bodyenv)]
                                        (-> bodyenv
                                            (assoc param varname)
                                            (update ::loop-bindings (fnil conj []) varname))))
                                    env
                                    (map vector params params-alias))
                      bodyenv (if vararg-param
                                (let [varname (lift-expr (DartExpr. (str "[" (str/join "," (subvec params-alias (count params))) "].takeWhile((e) => e != MISSING_ARG).toList()")) env :name vararg-param :name-env bodyenv)]
                                  (-> bodyenv
                                      (assoc vararg-param varname)
                                      (update ::loop-bindings (fnil conj []) varname)))
                                bodyenv)]
                (flush! "do {\n")
                (write! "return " (emit-body body bodyenv) ";\n} while(true);\n")
                (flush! "}\n")))]
        (doseq [body (butlast bodies)]
          (flush! (str "if (" (nth params-alias (count (first body))) " == MISSING_ARG) {\n"))
          (emit-fn-body body))
        (emit-fn-body (last bodies))
        nil)))) ; TODO ret value?

(defn emit-fn [[_ & sigs] env]
  (let [named (symbol? (first sigs))
        body (else->> (let [body (if named (next sigs) sigs)])
                      (if (vector? (first body)) (list body) body))
        fnname (when named (first sigs))
        munged (if named (munge fnname env) (tmpvar))]
    (open-prior! munged)
    (emit-fn-bodies body (cond-> env named (assoc fnname munged)))
    (close-prior!)
    munged))

(defn emit-const-expr [expr]
  (write! "TODO_CONST_EXPR"))

(defn emit-method [[mname [this-arg & other-args] & body] env]
  ;; args destructuring will be added by a macro
  (let [[fixed-args named-args] (split-with symbol? other-args)
        ;; named-args need to have been fully expanded to triples [keyword symbol default] by the macro
        dartargs (into [] (map (fn [arg] [arg (tmpvar)])) fixed-args) ; it's a vector and not map cause a binding may appear several times
        env (-> env
                (assoc this-arg (DartExpr. "this"))
                (into dartargs)
                (into
                 (map (fn [[k sym]] [sym (DartExpr. (name k))]))
                 named-args)
                (assoc ::loop-bindings
                       (into [] (map second) dartargs)))]
    (write! mname)
    (write! "(")
    (doseq [[_ arg] dartargs] (write! arg) (write! ", "))
    (when (seq named-args)
      (write! "{")
      (doseq [[k sym default] named-args]
        (write! (name k))
        (write! "=")
        (emit-const-expr default)
        (write! ", "))
      (write! "}"))
    (flush! "){\ndo {\n")
    (write! "return " (emit-body body env) ";\n")
    (flush! "} while(true);\n}\n\n")))

(defn emit-fn-call [[fnname & args] env]
  (with-ret
    (write! (emit (lift-expr fnname env) env))
    (write-args! args env)))

(defn extract-bodies [fn-expr]
  (let [bodies (if (symbol? (second fn-expr)) (nnext fn-expr) (next fn-expr))]
    (if (vector? (first bodies)) [bodies] bodies)))

(defn emit-top-fn [sym fn-expr env]
  (let [env (cond-> env (symbol? (second fn-expr)) (assoc (second fn-expr) (name sym)))]
    ;; @TODO :munged
    (write! (name sym))
    (emit-fn-bodies (extract-bodies fn-expr) env)
    (flush! "\n")))

(defn emit-def [[_ sym expr] env]
  (let [expr (macroexpand-all env expr)]
    (if (and (seq? expr) (= 'fn* (first expr)))
      (swap! nses do-def sym
             {:type :dartfn
              :code (with-string-emitter
                      (emit-top-fn sym expr env))})
      (swap! nses do-def sym
             {:type :field,
              :code
              (with-string-emitter
                ;; @TODO : munged
                (write! (name sym) "=")
                (if (coll? expr)
                  (do
                    (flush! "(){\n")
                    (write! "return " (emit expr env) ";\n")
                    (write! "}();\n"))
                  (write! (emit-literal expr env) ";\n")))}))
    (emit sym env)))

(defn emit-reify [[_ opts & specs] env out! locus]
  (let [{:keys [extends] :or {extends 'Object}} opts
        [ctor-op base & ctor-args :as ctor] (macroexpand-all env (cond->> extends (symbol? extends) (list 'new)))
        ctor-meth (when (= '. ctor-op) (first ctor-args))
        ctor-args (cond-> ctor-args (= '. ctor-op) next)
        [positional-ctor-args [_ & named-ctor-args]] (split-with (complement #{'&}) ctor-args)
        positional-ctor-params (repeatedly (count positional-ctor-args) tmpvar)
        named-ctor-params (repeatedly (quot (count named-ctor-args) 2) tmpvar)
        super-ctor (concat
                    (case ctor-op
                      . ['. (DartExpr. "super") ctor-meth]
                      new ['new (DartExpr. "super")])
                    positional-ctor-params '[&] (interleave (take-nth 2 named-ctor-args) named-ctor-params))
        class-name (tmpvar)  ; TODO change this to a more telling name
        classes (filter #(and (symbol? %) (not= base %)) specs) ; crude
        methods (remove symbol? specs)         ; crude
        mixins(filter (comp :mixin meta) classes)
        ifaces (remove (comp :mixin meta) classes)
        need-nsm (and (seq ifaces) (not-any? (fn [[m]] (case m noSuchMethod true nil)) methods))
        sb! (string-writer)]
    (sb! "class ")
    (sb! class-name)
    (when base
      (sb! " extends ")
      ;; @NOTE for @cgrand, does not take into account aliasing
      #_(sb! (name base))
      (symbol-literal base {} sb!))                ; TODO munge
    (when (seq ifaces)
      (sb! " implements ")
      (sb! (str/join ", " (map name ifaces)))) ; TODO aliasing
    (when (seq mixins)
      (sb! " with ")
      (sb! (str/join ", " (map name mixins)))) ; TODO aliasing
    (sb! " {\n")
    (let [env {} #_(closed-overs-recording-env env)
          ;; methods
          _ (when (seq methods)
              (doseq [m methods]
                (emit-method m env sb!)))
          closed-overs nil #_(get-closed-overs env)
          reify-ctor (concat ['new class-name] positional-ctor-args (take-nth 2 (next named-ctor-args)))]
      ;; closed overs
      (doseq [field (vals closed-overs)]
        (sb! "final ") ; revisit when support for :once
        (sb! field)
        (sb! ";\n"))
      ;; constructor
      (sb! class-name)
      (sb! "(")
      (sb! (str/join ", " (concat positional-ctor-params named-ctor-params
                                  (map #(str "this." (name %)) (vals closed-overs)))))
      (sb! ")")
      (if super-ctor
        (emit super-ctor {} sb! ": ")
        (sb! ";\n"))
      ;; noSuchMethod
      (when need-nsm
        (sb! "noSuchMethod(i)=>super.noSuchMethod(i);\n"))
      (sb! "}\n\n")
      (swap! nses do-def class-name {:type :class, :code (sb!)})
      (emit reify-ctor {} out! locus))))

;; The goal is to create the ns map that will go into nses
;; this map will contain keys :mappings :imports :aliases
;; :mappings a map from simple symbols to qualified dart literals
;; :imports a vector of pairs of dart lib strings and dart alias (will serve to produce `import "lib" as alias`)
;; :aliases a map of ns aliases (as strings) to lib aliases (as string)

(comment
  (:require [x.y.z :refer [foo-bar]])
  =>
  {:imports [["x/y/z.dart" "z1"]]
   :mappings {'foo-bar "z1.foo_bar"}}

  (:require [x.y.z :as z])
  =>
  {:imports [["x/y/z.dart" "z1"]]
   :aliases {"z" "z1"}}

  (:require foo.bar
            [clojure.string])

  (:require ["package:mobx/mobx.dart" :as mobx])
  =>
  {:imports [["package:mobx/mobx.dart" "mobx1"]]
   :aliases {"mobx" "mobx1"}}

  )

;; once it's done symbol resolution (in symbol-literal) must take them into account
;; I doubt it's enough -- we'll have to see how it interacts with macroexpansion for example.

(defn do-ns [[_ ns-sym & ns-clauses]]
  (let [ns-clauses (drop-while #(or (string? %) (map? %)) ns-clauses) ; drop doc and meta for now
        mappings
        (transduce
         (comp
          (mapcat (fn [[directive & args]] (map #(vector directive %) args)))
          (map
           (fn [[directive arg]]
             (case directive
               :require
               (let [arg (if (vector? arg) arg [arg])
                     alias (name (gensym "lib"))
                     clauses (into {} (partition-all 2) (next arg))]
                 (cond-> (assoc {} :imports [[(name (first arg)) alias]])
                   (:as clauses) (assoc-in [:aliases (name (:as clauses))] alias)
                   (:refer clauses) (assoc :mappings (into {} (map #(vector % (str alias "." (name %)))) (:refer clauses)))))
               :import (/ 0)
               :refer-clojure (/ 0)
               :use (/ 0)))))
         (partial merge-with into)
         {} ns-clauses)]
    (swap! nses assoc ns-sym mappings :current-ns ns-sym))
  #_(swap! nses assoc :current-ns (doto (second expr)
                                    #?@(:clj [create-ns]))) ;hacky
  )



(defn emit [expr env]
  (let [expr (macroexpand-all env expr)]
    (cond
      (seq? expr)
      (case (first expr)
        ns (do-ns expr)
        new (emit-new expr env)
        . (emit-dot expr env)
        let* (emit-let expr env)
        if (emit-if expr env)
        loop* (emit-loop expr env)
        recur (emit-recur expr env)
        quote (emit-quoted (second expr) env)
        fn* (emit-fn expr env)
        def (emit-def expr env)
        reify* (emit-reify expr env)
        (emit-fn-call expr env))
      (coll? expr) (emit-collection expr env)
      :else (emit-literal expr env))))

(defn dump-ns [ns-map out!]
  (doseq [[lib alias] (:imports ns-map)]
    (out! "import ")
    (emit-string lib out!)
    (out! " as ")
    (out! alias)
    (out! ";\n"))

  (doseq [[sym v] ns-map
          :when (or (symbol? sym) (instance? DartExpr sym))
          :let [{:keys [type code]} v]]
    (case type
      :class (out! code)
      :field (do (out! "var ") (out! code))
      :dartfn (out! code))))

(defn load-file [in]
  #?(:clj
     (let [in (clojure.lang.LineNumberingPushbackReader. in)]
       (loop []
         (let [form (read {:eof in} in)]
           (when-not (identical? form in)
             (emit form {} (constantly nil) "")
             (recur)))))))

(defn make-ns-to-out [^String target-dir]
  #?(:clj (let [out-dir (java.io.File. target-dir)]
            (.mkdirs out-dir)
            (fn [ns-sym]
              (let [ns-path (str (.replace (name ns-sym) "." "/") ".dart")
                    ns-file (doto (java.io.File. out-dir ns-path) (-> .getParentFile .mkdirs))
                    writer (java.io.FileWriter. ns-file java.nio.charset.StandardCharsets/UTF_8)]
                (fn
                  ([]
                   (.close writer))
                  ([x]
                   (.write writer (str x)))))))
     :cljd 'TODO))

(defn compile-file [in ns-to-out]
  (load-file in)
  (let [{:keys [current-ns] :as nses} @nses
        out! (ns-to-out current-ns)]
    (dump-ns (nses current-ns) out!)
    (out!)))

(comment

  (require '[clojure.java.io :as io])
  (compile-file (io/reader "test.cljd") (make-ns-to-out "targetdir/ohoh"))

  (set! *warn-on-reflection* true)

  (binding [*ns* *ns*]
    (ns cljd.bordeaux)
    (ns cljd.ste)
    (ns cljd.user))

  (doseq [form
          (concat
           '("hello"
             sym
             42
             \x
             (Foo. 1 & :bleh 32)
             (Foo. 1 & :bleh (Bar.))
             (.-fld a)
             (.meth a b c)
             (let [a 2 b 3] a a a a a a b)
             (if true "a" "b")
             (if true "a")
             (if (let [a true] a) "a")
             (loop [a 4] (if a a 5))
             (loop [a 4] (if a a (recur 5)))
             (loop [a 4 b 5] (if a a (recur (.-isOdd a) (.-isEven a))))
             '(fn [a] a)
             '{a b, c 3}
             {1 2, 3 4}
             {"a" {:a :b}, 3 4}
             [1 2 3 {1 2}]
             [1 2 (let [a 3] a)]
             #{1 (let [a 3] a) 2}
             [1 2 '(let [a])]
             (fn ([a] 1 a a))
             (fn ([a] 1 a) ([a b] b a) ([a b c] c))
             (fn ([a a a a a a] 1 a))
             (fn ([a & b] 1 a))
             (fn ([arg] (.-isOdd arg)) ([a & b] 1 a))
             (fn ([] 1) ([a & b] 1 a))
             (fn ([a] (recur a)) ([a & b] 1 a))
             (loop [a 4] (if a a (recur 5)))
             (fn unnom [a] (unnom a))
             ((fn [a] a) 42))

           ['(ns cljd.bordeaux)         ; nrepl or cider grrrrr
            '(def x 33)
            '(def reviews (fn* ([x] "The Best city Everrrr !")))
            '(ns cljd.ste)
            '(def surname "looser")
            '(ns cljd.user
               (:require [cljd.bordeaux :refer [reviews] :as awesome]
                         [cljd.ste :as ste]
                         ["package:flutter/material.dart"]
                         clojure.string))
            '(def x 42)
            '(def ste ste/surname)
            '(reviews awesome/x)
            '(def foo (fn foo [x] x))
            '(defn bar [z] z)
            '(or a b c)
            '(cond test-1 expr-1 test-2 expr-2 ":else" fallback)])
          :let [out! (string-writer)]]
    (print "#=> ") (prn form)
    (emit form {} out! "return ")
    (println (out!))
    (newline))

  (doseq [form
          (concat
           '(
             (reify :extends (StatelessWidget.origin 1 2) (build [_ ctx] "prout")
               IReduce
               (-reduce [coll f]
                 (iter-reduce coll f))
               #_(-reduce [coll f start]
                 (iter-reduce coll f start))

               IFn
               (-invoke [coll k]
                 (if (.-isOdd (.-length coll))
                   (recur (+ 1 k))
                   k)
                 (-lookup coll k))
               (-opts-meth [coll k & [:a a 42] [:b b 43]]
                 (+ a b))
               #_(-invoke [coll k not-found]
                 (-lookup coll k not-found)))
             ))
          :let [out! (string-writer)]]
    (print "#=> ") (prn form)
    (emit form {} out! "return ")
    (println (out!))
    (newline))

  @nses

  (let [sb! (string-writer)]
    (dump-ns (get @nses 'user) sb!)
    (print (str (sb!))))

  ;; Hello World ! flutter
  (doseq [form
          (concat
           ['(ns cljd.bordeaux
               (:require ["package:flutter/material.dart" :as material])) ; nrepl or cider grrrrr

            '(defn main []
               (material/runApp
                (reify :extends material/StatelessWidget
                  material/StatelessWidget
                  (build [_ ctx]
                    (material/MaterialApp.
                     & :title "Welcome to Flutter"
                     :home (material/Scaffold.
                            & :appBar (material/AppBar.
                                       & :title (material/Text. "Welcome to Flutter"))
                            :body (material/Center.
                                   & :child (material/Text. "Hello World!"))))))))

            ])
          :let [out! (string-writer)]]
    (print "#=> ") (prn form)
    (emit form {} out! "")
    (println (out!))
    (newline))

  (emit '(let [f (fn maFonction [x] (maFonction x))] f) {} (comp print str) "<>")
  (emit '(loop [f 1 a (inc f)] (recur a f)) {} (comp print str) "<>")

  (with-string-emitter
    (write! "locus " (emit '(let [a 1 b a] (+ a b)) {})))

  (with-string-emitter
    (write! "locus " (emit '(fn* [x] x) {})))


  )
