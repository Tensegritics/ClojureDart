;   Copyright (c) Baptiste Dupuch & Christophe Grand . All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns cljd.core
  (:require ["dart:math" :as math]
            ["dart:collection" :as dart-coll]))

(definterface IProtocol
  (extensions [x])
  (satisfies [x]))

(declare -EMPTY-LIST -EMPTY-MAP -EMPTY-SET -EMPTY-VECTOR)

(def ^{:clj true} =)
#_(def ^{:dart true} butlast)
(def ^{:dart true} contains?)
#_(def ^{:clj true} dissoc)
(def ^{:clj true} gensym)
(def ^{:dart true} ident?)
(def ^{:dart true} key)
(def ^{:dart true} keys)
(def ^{:clj true} keyword)
(def ^{:dart true} keyword?)
(def ^{:dart true} map?)
(def ^{:dart true} name)
(def ^{:dart true} namespace)
(def ^{:dart true} set)
(def ^{:dart true} string?)
(def ^{:clj true} subvec)
(def ^{:clj true} symbol)
(def ^{:dart true} symbol?)
#_(def ^{:dart true} take-nth)
#_(def ^{:dart true} val)
(def ^{:clj true} vary-meta)
#_(def ^{:dart true} vec)
(def ^{:clj true} vector)
(def ^{:dart true} vector?)

;; syntax quote support at bootstrap
;; the :cljd nil is most certainly going to bite us once we run the compiler on dart vm
(def ^:clj ^:bootstrap apply #?(:cljd nil :clj clojure.core/apply))
(def ^:clj ^:bootstrap concat #?(:cljd nil :clj clojure.core/concat))
(def ^:dart ^:bootstrap first #?(:cljd nil :clj clojure.core/first))
(def ^:clj ^:bootstrap hash-map #?(:cljd nil :clj clojure.core/hash-map))
(def ^:clj ^:bootstrap hash-set #?(:cljd nil :clj clojure.core/hash-set))
(def ^:clj ^:bootstrap list #?(:cljd nil :clj clojure.core/list))
(def ^:dart ^:bootstrap next #?(:cljd nil :clj clojure.core/next))
(def ^:clj ^:bootstrap nth #?(:cljd nil :clj clojure.core/nth))
(def ^:dart ^:bootstrap seq #?(:cljd nil :clj clojure.core/seq))
(def ^:clj ^:bootstrap vector #?(:cljd nil :clj clojure.core/vector))

(defprotocol IFn
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
    [this a b c d e f g h i])
  (-invoke-more [this a b c d e f g h i rest])
  (-apply [this more]))

(def ^:macro fn
  (fn* [&form &env & decl]
    (cons 'fn* decl)))

(def
  ^{:private true
    :bootstrap true}
 sigs
 (fn [fdecl]
   #_(assert-valid-fdecl fdecl)
   (let [asig
         (fn [fdecl]
           (let [arglist (first fdecl)
                 ;elide implicit macro args
                 arglist (if (= '&form (first arglist))
                           (subvec arglist 2 (count arglist))
                           arglist)
                 body (next fdecl)]
             (if (map? (first body))
               (if (next body)
                 (with-meta arglist (conj (if (meta arglist) (meta arglist) {}) (first body)))
                 arglist)
               arglist)))
         resolve-tag (fn [argvec]
                        (let [m (meta argvec)
                              tag (:tag m)]
                          argvec
                          ; TODO how to port to CLJD?
                          #_(if (symbol? tag)
                            (if (= (.indexOf ^String (name tag) ".") -1)
                              (if (nil? (clojure.lang.Compiler$HostExpr/maybeSpecialTag tag))
                                (let [c (clojure.lang.Compiler$HostExpr/maybeClass tag false)]
                                  (if c
                                    (with-meta argvec (assoc m :tag (symbol (name c))))
                                    argvec))
                                argvec)
                              argvec)
                            argvec)))]
     (if (seq? (first fdecl))
       (loop [ret [] fdecls fdecl]
         (if fdecls
           (recur (conj ret (resolve-tag (asig (first fdecls)))) (next fdecls))
           (seq ret)))
       (list (resolve-tag (asig fdecl)))))))

(def ^:bootstrap ^:private ^:dart argument-error
  (fn [msg]
    (new #?(:cljd ArgumentError :clj IllegalArgumentException) msg)))

(def
  ^{:macro true
    :doc "Same as (def name (fn [params* ] exprs*)) or (def
    name (fn ([params* ] exprs*)+)) with any doc-string or attrs added
    to the var metadata. prepost-map defines a map with optional keys
    :pre and :post that contain collections of pre or post conditions."
   :arglists '([name doc-string? attr-map? [params*] prepost-map? body]
                [name doc-string? attr-map? ([params*] prepost-map? body)+ attr-map?])
   :added "1.0"}
 defn (fn defn [&form &env fname & fdecl]
        ;; Note: Cannot delegate this check to def because of the call to (with-meta name ..)
        (if (symbol? fname)
          nil
          (throw (argument-error "First argument to defn must be a symbol")))
        (let [m (if (string? (first fdecl))
                  {:doc (first fdecl)}
                  {})
              fdecl (if (string? (first fdecl))
                      (next fdecl)
                      fdecl)
              m (if (map? (first fdecl))
                  (conj m (first fdecl))
                  m)
              fdecl (if (map? (first fdecl))
                      (next fdecl)
                      fdecl)
              fdecl (if (vector? (first fdecl))
                      (list fdecl)
                      fdecl)
              m (if (map? (last fdecl))
                  (conj m (last fdecl))
                  m)
              fdecl (if (map? (last fdecl))
                      (butlast fdecl)
                      fdecl)
              m (conj {:arglists (list 'quote (sigs fdecl))} m)
              m (let [inline (:inline m)
                      ifn (first inline)
                      iname (second inline)]
                  ;; same as: (if (and (= 'fn ifn) (not (symbol? iname))) ...)
                  (if (if (= 'fn ifn)
                        (if (symbol? iname) false true))
                    ;; inserts the same fn name to the inline fn if it does not have one
                    (assoc m :inline (cons ifn (cons (symbol (str (name fname) "__inliner"))
                                                     (next inline))))
                    m))
              m (conj (if (meta fname) (meta fname) {}) m)]
          (list 'def (with-meta fname m)
                ;;todo - restore propagation of fn name
                ;;must figure out how to convey primitive hints to self calls first
								;;(cons `fn fdecl)
								(with-meta (cons `fn fdecl) {:rettag (:tag m)})))))

(def
 ^{:macro true
   :doc "Like defn, but the resulting function name is declared as a
  macro and will be used as a macro by the compiler when it is
  called."
   :arglists '([name doc-string? attr-map? [params*] body]
               [name doc-string? attr-map? ([params*] body)+ attr-map?])
   :added "1.0"}
  defmacro (fn [&form &env
                name & args]
             (let [name (vary-meta name assoc :macro true)
                   prefix (loop [p (list name) args args]
                            (let [f (first args)]
                              (if (string? f)
                                (recur (cons f p) (next args))
                                (if (map? f)
                                  (recur (cons f p) (next args))
                                  p))))
                   fdecl (loop [fd args]
                           (if (string? (first fd))
                             (recur (next fd))
                             (if (map? (first fd))
                               (recur (next fd))
                               fd)))
                   fdecl (if (vector? (first fdecl))
                           (list fdecl)
                           fdecl)
                   add-implicit-args (fn [fd]
                             (let [args (first fd)]
                               (cons (vec (cons '&form (cons '&env args))) (next fd))))
                   add-args (fn [acc ds]
                              (if (nil? ds)
                                acc
                                (let [d (first ds)]
                                  (if (map? d)
                                    (conj acc d)
                                    (recur (conj acc (add-implicit-args d)) (next ds))))))
                   fdecl (seq (add-args [] fdecl))
                   decl (loop [p prefix d fdecl]
                          (if p
                            (recur (next p) (cons (first p) d))
                            d))]
               (cons `defn decl))))

(defmacro declare
  "defs the supplied var names with no bindings, useful for making forward declarations."
  [& names] `(do ~@(map #(list 'def % nil) names)))

(defn ^bool satisfies?
  {:inline (fn [protocol x] `(.satisfies ~protocol ~x))
   :inline-arities #{2}}
  [^IProtocol protocol x]
  (.satisfies protocol x))

(defn ^bool false?
  "Returns true if x is the value false, false otherwise."
  {:inline (fn [x] `(.== false ~x))
   :inline-arities #{1}}
  [x]
  (== false x))

(defn ^bool true?
  "Returns true if x is the value true, false otherwise."
  {:inline (fn [x] `(.== true ~x))
   :inline-arities #{1}}
  [x]
  (== true x))

(defn ^bool nil?
  {:inline-arities #{1}
   :inline (fn [x] `(.== nil ~x))}
  [x] (.== nil x))

(defn ^bool boolean
  "Coerce to boolean"
  [x]
  (if (or (nil? x) (false? x))
    false
    true))

(defn ^bool some?
  "Returns true if x is not nil, false otherwise."
  {:inline-arities #{1}
   :inline (fn [x] `(. ~x "!=" nil))}
  [x] (. x "!=" nil))

(defn ^:bootstrap destructure [bindings]
  (let [bents (partition 2 bindings)
        pb (fn pb [bvec b v]
             (let [pvec
                   (fn [bvec b val]
                     (let [gvec (gensym "vec__")
                           gseq (gensym "seq__")
                           gfirst (gensym "first__")
                           has-rest (some #{'&} b)]
                       (loop [ret (let [ret (conj bvec gvec val)]
                                    (if has-rest
                                      (conj ret gseq (list `seq gvec))
                                      ret))
                              n 0
                              bs b
                              seen-rest? false]
                         (if (seq bs)
                           (let [firstb (first bs)]
                             (cond
                              (= firstb '&) (recur (pb ret (second bs) gseq)
                                                   n
                                                   (nnext bs)
                                                   true)
                              (= firstb :as) (pb ret (second bs) gvec)
                              :else (if seen-rest?
                                      (throw (new Exception "Unsupported binding form, only :as can follow & parameter"))
                                      (recur (pb (if has-rest
                                                   (conj ret
                                                         gfirst `(first ~gseq)
                                                         gseq `(next ~gseq))
                                                   ret)
                                                 firstb
                                                 (if has-rest
                                                   gfirst
                                                   (list `nth gvec n nil)))
                                             (inc n)
                                             (next bs)
                                             seen-rest?))))
                           ret))))
                   pmap
                   (fn [bvec b v]
                     (let [gmap (gensym "map__")
                           defaults (:or b)]
                       (loop [ret (-> bvec (conj gmap) (conj v)
                                      (conj gmap) (conj `(if (seq? ~gmap) (-map-lit (seq ~gmap)) ~gmap))
                                      ((fn [ret]
                                         (if (:as b)
                                           (conj ret (:as b) gmap)
                                           ret))))
                              bes (let [transforms
                                          (reduce
                                            (fn [transforms mk]
                                              (if (keyword? mk)
                                                (let [mkns (namespace mk)
                                                      mkn (name mk)]
                                                  (cond (= mkn "keys") (assoc transforms mk #(keyword (or mkns (namespace %)) (name %)))
                                                        (= mkn "syms") (assoc transforms mk #(list `quote (symbol (or mkns (namespace %)) (name %))))
                                                        (= mkn "strs") (assoc transforms mk str)
                                                        :else transforms))
                                                transforms))
                                            {}
                                            (keys b))]
                                    (reduce
                                        (fn [bes entry]
                                          (reduce #(assoc %1 %2 ((val entry) %2))
                                                   (dissoc bes (key entry))
                                                   ((key entry) bes)))
                                        (dissoc b :as :or)
                                        transforms))]
                         (if (seq bes)
                           (let [bb (key (first bes))
                                 bk (val (first bes))
                                 local (if (ident? bb) (with-meta (symbol nil (name bb)) (meta bb)) bb)
                                 bv (if (contains? defaults local)
                                      (list `get gmap bk (defaults local))
                                      (list `get gmap bk))]
                             (recur (if (ident? bb)
                                      (-> ret (conj local bv))
                                      (pb ret bb bv))
                                    (next bes)))
                           ret))))]
               (cond
                (symbol? b) (-> bvec (conj b) (conj v))
                (vector? b) (pvec bvec b v)
                (map? b) (pmap bvec b v)
                :else (throw (new Exception (str "Unsupported binding form: " b))))))
        process-entry (fn [bvec b] (pb bvec (first b) (second b)))]
    (if (every? symbol? (map first bents))
      bindings
      (reduce process-entry [] bents))))

(defmacro let
  "binding => binding-form init-expr
  Evaluates the exprs in a lexical context in which the symbols in
  the binding-forms are bound to their respective init-exprs or parts
  therein."
  {:added "1.0", :special-form true, :forms '[(let [bindings*] exprs*)]}
  [bindings & body]
  #_(assert-args
      (vector? bindings) "a vector for its binding"
      (even? (count bindings)) "an even number of forms in binding vector")
  `(let* ~(destructure bindings) ~@body))

(defn ^{:private true :bootstrap true}
  maybe-destructured
  [params body]
  (if (every? symbol? params)
    (cons params body)
    (loop [params params
           new-params (with-meta [] (meta params))
           lets []]
      (if params
        (if (symbol? (first params))
          (recur (next params) (conj new-params (first params)) lets)
          (let [gparam (gensym "p__")]
            (recur (next params) (conj new-params gparam)
                   (-> lets (conj (first params)) (conj gparam)))))
        `(~new-params
          (let ~lets
            ~@body))))))

;redefine fn with destructuring and pre/post conditions
(defmacro fn
  "params => positional-params* , or positional-params* & next-param
  positional-param => binding-form
  next-param => binding-form
  name => symbol
  Defines a function"
  {:added "1.0", :special-form true,
   :forms '[(fn name? [params* ] exprs*) (fn name? ([params* ] exprs*)+)]}
  [& sigs]
    (let [name (if (symbol? (first sigs)) (first sigs) nil)
          sigs (if name (next sigs) sigs)
          sigs (if (vector? (first sigs))
                 (list sigs)
                 (if (seq? (first sigs))
                   sigs
                   ;; Assume single arity syntax
                   (throw (argument-error
                            (if (seq sigs)
                              (str "Parameter declaration "
                                   (first sigs)
                                   " should be a vector")
                              (str "Parameter declaration missing"))))))
          psig (fn* [sig]
                 ;; Ensure correct type before destructuring sig
                 (when (not (seq? sig))
                   (throw (argument-error
                            (str "Invalid signature " sig
                                 " should be a list"))))
                 (let [[params & body] sig
                       _ (when (not (vector? params))
                           (throw (argument-error
                                    (if (seq? (first sigs))
                                      (str "Parameter declaration " params
                                           " should be a vector")
                                      (str "Invalid signature " sig
                                           " should be a list")))))
                       conds (when (and (next body) (map? (first body)))
                                           (first body))
                       body (if conds (next body) body)
                       conds (or conds (meta params))
                       pre (:pre conds)
                       post (:post conds)
                       body (if post
                              `((let [~'% ~(if (.< 1 (count body))
                                            `(do ~@body)
                                            (first body))]
                                 ~@(map (fn* [c] `(assert ~c)) post)
                                 ~'%))
                              body)
                       body (if pre
                              (concat (map (fn* [c] `(assert ~c)) pre)
                                      body)
                              body)]
                   (maybe-destructured params body)))
          new-sigs (map psig sigs)]
      (with-meta
        (if name
          (list* 'fn* name new-sigs)
          (cons 'fn* new-sigs))
        (meta &form))))

(defn- ^:bootstrap roll-leading-opts [body]
  (loop [[k v & more :as body] (seq body) opts {}]
    (if (and body (keyword? k))
      (recur more (assoc opts k v))
      [opts body])))

(defmacro deftype [& args]
  (let [[class-name fields & args] args
        [opts specs] (roll-leading-opts args)]
    `(do
       (~'deftype* ~class-name ~fields ~opts ~@specs)
       ~(when-not (or (:type-only opts) (:abstract (meta class-name)))
          `(defn
             ~(symbol (str "->" class-name))
             [~@fields]
             (new ~(vary-meta class-name dissoc :type-params) ~@fields))))))

(defmacro definterface [iface & meths]
  `(deftype ~(vary-meta iface assoc :abstract true) []
     :type-only true
     ~@(map (fn [[meth args]] `(~meth [~'_ ~@args])) meths)))

(defmacro and
  "Evaluates exprs one at a time, from left to right. If a form
  returns logical false (nil or false), and returns that value and
  doesn't evaluate any of the other expressions, otherwise it returns
  the value of the last expr. (and) returns true."
  {:added "1.0"}
  ([] true)
  ([x] x)
  ([x & next]
   `(let [and# ~x]
      (if and# (and ~@next) and#))))

(defmacro when
  "Evaluates test. If logical true, evaluates body in an implicit do."
  [test & body]
  `(if ~test (do ~@body)))

(defmacro when-not
  "Evaluates test. If logical false, evaluates body in an implicit do."
  [test & body]
  `(if ~test nil (do ~@body)))

(defmacro when-some
  "bindings => binding-form test

   When test is not nil, evaluates body with binding-form bound to the
   value of test"
  [bindings & body]
  #_(assert-args
    (vector? bindings) "a vector for its binding"
    (= 2 (count bindings)) "exactly 2 forms in binding vector")
  (let [form (bindings 0) tst (bindings 1)]
    `(let [temp# ~tst]
       (if (nil? temp#)
         nil
         (let [~form temp#]
           ~@body)))))

(defmacro when-let
  "bindings => binding-form test

  When test is true, evaluates body with binding-form bound to the value of test"
  [bindings & body]
  ;; TODO assert-args
  #_(assert-args
      (vector? bindings) "a vector for its binding"
      (= 2 (count bindings)) "exactly 2 forms in binding vector")
  (let [form (bindings 0) tst (bindings 1)]
    `(let [temp# ~tst]
       (when temp#
         (let [~form temp#]
           ~@body)))))

(defmacro when-first
  "bindings => x xs

  Roughly the same as (when (seq xs) (let [x (first xs)] body)) but xs is evaluated only once"
  [bindings & body]
  (let [[x xs] bindings]
    `(when-let [xs# (seq ~xs)]
       (let [~x (first xs#)]
         ~@body))))

(defmacro or
  "Evaluates exprs one at a time, from left to right. If a form
  returns a logical true value, or returns that value and doesn't
  evaluate any of the other expressions, otherwise it returns the
  value of the last expression. (or) returns nil."
  {:added "1.0"}
  ([] nil)
  ([x] x)
  ([x & next]
      `(let [or# ~x]
         (if or# or# (or ~@next)))))

(defmacro cond
  "Takes a set of test/expr pairs. It evaluates each test one at a
  time.  If a test returns logical true, cond evaluates and returns
  the value of the corresponding expr and doesn't evaluate any of the
  other tests or exprs. (cond) returns nil."
  {:added "1.0"}
  [& clauses]
    (when clauses
      (list 'if (first clauses)
            (if (next clauses)
                (second clauses)
                (throw (argument-error
                         "cond requires an even number of forms")))
            (cons 'cljd.core/cond (next (next clauses))))))

(defmacro cond->
  "Takes an expression and a set of test/form pairs. Threads expr (via ->)
  through each form for which the corresponding test
  expression is true. Note that, unlike cond branching, cond-> threading does
  not short circuit after the first true test expression."
  [expr & clauses]
  #_(assert (even? (count clauses)))
  (let [g (gensym)
        steps (map (fn [[test step]] `(if ~test (-> ~g ~step) ~g))
                (partition 2 clauses))]
    `(let [~g ~expr
           ~@(interleave (repeat g) (butlast steps))]
       ~(if (empty? steps)
          g
          (last steps)))))

(defmacro cond->>
  "Takes an expression and a set of test/form pairs. Threads expr (via ->>)
  through each form for which the corresponding test expression
  is true.  Note that, unlike cond branching, cond->> threading does not short circuit
  after the first true test expression."
  [expr & clauses]
  #_(assert (even? (count clauses)))
  (let [g (gensym)
        steps (map (fn [[test step]] `(if ~test (->> ~g ~step) ~g))
                (partition 2 clauses))]
    `(let [~g ~expr
           ~@(interleave (repeat g) (butlast steps))]
       ~(if (empty? steps)
          g
          (last steps)))))

(defmacro loop
  "Evaluates the exprs in a lexical context in which the symbols in
  the binding-forms are bound to their respective init-exprs or parts
  therein. Acts as a recur target."
  {:added "1.0", :special-form true, :forms '[(loop [bindings*] exprs*)]}
  [bindings & body]
  #_(assert-args
     (vector? bindings) "a vector for its binding"
     (even? (count bindings)) "an even number of forms in binding vector")
  (let [db (destructure bindings)]
    (if (= db bindings)
      `(loop* ~bindings ~@body)
      (let [vs (take-nth 2 (drop 1 bindings))
            bs (take-nth 2 bindings)
            gs (map (fn [b] (if (symbol? b) b (gensym))) bs)
            bfs (reduce (fn [ret [b v g]]
                          (if (symbol? b)
                            (conj ret g v)
                            (conj ret g v b g)))
                        [] (map vector bs vs gs))]
        `(let ~bfs
           (loop* ~(vec (interleave gs gs))
                  (let ~(vec (interleave bs gs))
                    ~@body)))))))

(defmacro while
  "Repeatedly executes body while test expression is true. Presumes
  some side-effect will cause test to become false/nil. Returns nil"
  [test & body]
  `(loop []
     (when ~test
       ~@body
       (recur))))

(defmacro comment
  "Ignores body, yields nil"
  {:added "1.0"}
  [& body])

(defprotocol IPrint
  (-print [o string-sink]))

(extend-type fallback
  IPrint
  (-print [o sink]
    (.write ^StringSink sink (.toString o))))

(extend-type Null
  IPrint
  (-print [o sink]
    (.write ^StringSink sink "nil")))

;; TODO js does define infinite but not native & VM, handle theses cases
(extend-type num
  IPrint
  (-print [o sink]
    (cond
      (and (.-isInfinite o) (.-isNegative o)) (.write ^StringSink sink "##-Inf")
      (.-isInfinite o) (.write ^StringSink sink "##Inf")
      (.-isNaN o) (.write ^StringSink sink "##Nan")
      :else (.write ^StringSink sink (.toString o)))))

(deftype ^:abstract ToStringMixin []
  Object
  (toString [o]
    (let [sb (StringBuffer.)]
      (-print o sb)
      (.toString sb))))

(defprotocol INamed
  "Protocol for adding a name."
  (-name [x]
    "Returns the name String of x.")
  (-namespace [x]
    "Returns the namespace String of x."))

(defn namespace
  "Returns the namespace String of a symbol or keyword, or nil if not present."
  {:inline (fn [x] `(-namespace ~x))
   :inline-arities #{1}}
  [x]
  (-namespace x))

(defprotocol ISeqable
  "Protocol for adding the ability to a type to be transformed into a sequence."
  (-seq [o]
    "Returns a seq of o, or nil if o is empty."))

(defn seqable?
  "Return true if the seq function is supported for x."
  [x]
  (satisfies? ISeqable x))

(defn seq
  "Returns a seq on the collection. If the collection is
    empty, returns nil.  (seq nil) returns nil. seq also works on
    Strings, native Java arrays (of reference types) and any objects
    that implement Iterable. Note that seqs cache values, thus seq
    should not be used on any Iterable whose iterator repeatedly
    returns the same mutable object."
  {:inline (fn [coll] `(-seq ~coll))
   :inline-arities #{1}}
  [coll] (-seq coll))

(defprotocol ISeq
  "Protocol for collections to provide access to their items as sequences."
  (-first [coll]
    "Returns the first item in the collection coll.")
  (-rest [coll]
    "Returns a new collection of coll without the first item. It should
     always return a seq, e.g.
     (rest []) => ()
     (rest nil) => ()")
  (-next [coll]
    "Returns a new collection of coll without the first item. In contrast to
     rest, it should return nil if there are no more items, e.g.
     (next []) => nil
     (next nil) => nil"))

(defn ^bool seq?
  {:inline (fn [x] `(satisfies? ISeq ~x))
   :inline-arities #{1}}
  [x] (satisfies? ISeq x))

(extend-type Null
  ISeqable
  (-seq [coll] nil)
  ISeq
  (-first [coll] nil)
  (-rest [coll] ())
  (-next [coll] nil))

(defn first
  "Returns the first item in the collection. Calls seq on its
   argument. If coll is nil, returns nil."
  [coll]
  (-first (seq coll)))

(defn next
  "Returns a seq of the items after the first. Calls seq on its
  argument.  If there are no more items, returns nil."
  [coll]
  (-next (seq coll)))

(defn rest
  "Returns a possibly empty seq of the items after the first. Calls seq on its
  argument."
  [coll]
  (-rest (seq coll)))

(defprotocol ISequential
  "Marker interface indicating a persistent collection of sequential items")

(defn sequential?
  "Returns true if coll implements Sequential"
  {:inline-arities #{1}
   :inline (fn [coll] `(satisfies? ISequential ~coll))}
  [coll]
  (satisfies? ISequential coll))

(defn realized?
  "Returns true if a value has been produced for a promise, delay, future or lazy sequence."
  {:inline-arities #{1}
   :inline (fn [x] `(-realized? ~x))}
  [x]
  (-realized? x))

(defprotocol IPending
  "Protocol for types which can have a deferred realization. Currently only
  implemented by Delay and LazySeq."
  (-realized? [x]
    "Returns true if a value for x has been produced, false otherwise."))

(defn realized?
  "Returns true if a value has been produced for a promise, delay, future or lazy sequence."
  {:inline-arities #{1}
   :inline (fn [x] `(-realized? ~x))}
  [x]
  (-realized? x))

(defprotocol IList
  "Marker interface indicating a persistent list")

(defprotocol ICollection
  "Protocol for adding to a collection."
  (-conj [coll o]
    "Returns a new collection of coll with o added to it. The new item
     should be added to the most efficient place, e.g.
     (conj [1 2 3 4] 5) => [1 2 3 4 5]
     (conj '(2 3 4 5) 1) => '(1 2 3 4 5)"))

(defn conj
  "conj[oin]. Returns a new collection with the xs
    'added'. (conj nil item) returns (item).  The 'addition' may
    happen at different 'places' depending on the concrete type."
  {:inline-arities #{0 1 2}
   :inline (fn
             ([] [])
             ([coll] coll)
             ([coll x] `(-conj ~coll ~x)))}
  ([] [])
  ([coll] coll)
  ([coll x] (-conj coll x))
  ([coll x & xs]
   (if xs
     (recur (-conj coll x) (first xs) (next xs))
     (-conj coll x))))

(defprotocol IDeref
  "Protocol for adding dereference functionality to a reference."
  (-deref [o]
    "Returns the value of the reference o."))

(defn deref
  "Also reader macro: @ref/@agent/@var/@atom/@delay/@future/@promise. Within a transaction,
  returns the in-transaction-value of ref, else returns the
  most-recently-committed value of ref. When applied to a var, agent
  or atom, returns its current state. When applied to a delay, forces
  it if not already forced. When applied to a future, will block if
  computation not complete. When applied to a promise, will block
  until a value is delivered.  The variant taking a timeout can be
  used for blocking references (futures and promises), and will return
  timeout-val if the timeout (in milliseconds) is reached before a
  value is available. See also - realized?."
  {:added "1.0"
   :static true}
  ;; TODO: rethink
  ([ref] #_(if (instance? clojure.lang.IDeref ref)
           (.deref ^clojure.lang.IDeref ref)
           (deref-future ref))
   (-deref ref))
  #_([ref timeout-ms timeout-val]
   (if (instance? clojure.lang.IBlockingDeref ref)
     (.deref ^clojure.lang.IBlockingDeref ref timeout-ms timeout-val)
     (deref-future ref timeout-ms timeout-val))))

(defprotocol IReduce
  "Protocol for seq types that can reduce themselves.
  Called by cljs.core/reduce."
  (-reduce [coll f] [coll f start]
    "f should be a function of 2 arguments. If start is not supplied,
     returns the result of applying f to the first 2 items in coll, then
     applying f to that result and the 3rd item, etc."))

(deftype Reduced [val]
  IDeref
  (-deref [o] val))

(defn reduced
  "Wraps x in a way such that a reduce will terminate with the value x"
  [x]
  (Reduced. x))

(defn ^bool reduced?
  "Returns true if x is the result of a call to reduced"
  [r]
  (dart/is? r Reduced))

(defn ensure-reduced
  "If x is already reduced?, returns it, else returns (reduced x)"
  [x]
  (if (reduced? x) x (reduced x)))

(defn unreduced
  "If x is reduced?, returns (deref x), else returns x"
  [x]
  (if (reduced? x) (-deref x) x))

(extend-type fallback
  IReduce
  (-reduce [coll f]
    (if-some [[x & xs] (seq coll)]
      (if-some [[y & xs] xs]
        (let [val (f x y)]
          (if (reduced? val)
            (deref val)
            (-reduce xs f val)))
        x)
      (f)))
  (-reduce [coll f start]
    (loop [acc start xs (seq coll)]
      (if-some [[x & xs] xs]
        (let [val (f acc x)]
          (if (reduced? val)
            (deref val)
            (recur val xs)))
        acc))))

(defn reduce
  {:inline-arities #{2 3}
   :inline (fn
             ([f coll] `(-reduce ~coll ~f))
             ([f init coll] `(-reduce ~coll ~f ~init)))}
  ([f coll] (-reduce coll f))
  ([f init coll] (-reduce coll f init)))

(defprotocol IKVReduce
  "Protocol for associative types that can reduce themselves
  via a function of key and val."
  (-kv-reduce [coll f init]
    "Reduces an associative collection and returns the result. f should be
     a function that takes three arguments."))

(defn reduce-kv
  "Reduces an associative collection. f should be a function of 3
  arguments. Returns the result of applying f to init, the first key
  and the first value in coll, then applying f to that result and the
  2nd key and value, etc. If coll contains no entries, returns init
  and f is not called. Note that reduce-kv is supported on vectors,
  where the keys will be the ordinals."
  [f init coll]
  (-kv-reduce coll f init))

(defprotocol ICounted
  "Protocol for adding the ability to count a collection in constant time."
  (-count [coll]
    "Calculates the count of coll in constant time."))

(defn ^bool counted?
  "Returns true if coll implements count in constant time."
  [coll]
  (satisfies? ICounted coll))

(extend-type fallback
  ICounted
  (-count [coll]
    (reduce (fn [n _] (inc n)) 0 coll)))

(defn ^int count
  {:inline (fn [coll] `(-count ~coll))
   :inline-arities #{1}}
  [coll]
  (-count coll))

(defprotocol IChunk
  "Protocol for accessing the items of a chunk."
  (-drop-first [coll]
    "Return a new chunk of coll with the first item removed.")
  (-chunk-reduce [coll f init]
    "Internal reduce, doesn't unwrap reduced values returned by f."))

(defn chunk-reduce
  [f val coll]
  (-chunk-reduce coll f val))

(defprotocol IChunkedSeq
  "Protocol for accessing a collection as sequential chunks."
  (-chunked-first [coll]
    "Returns the first chunk in coll.")
  (-chunked-rest [coll]
    "Return a new collection of coll with the first chunk removed.")
  (-chunked-next [coll]
    "Returns a new collection of coll without the first chunk."))

(defn chunk-first [s]
  (-chunked-first s))

(defn chunk-rest [s]
  (-chunked-rest s))

(defn chunk-next [s]
  (-chunked-next s))

(defprotocol IVector
  "Protocol for adding vector functionality to collections."
  (-assoc-n [coll n val]
    "Returns a new vector with value val added at position n."))

(defprotocol IAssociative
  "Protocol for adding associativity to collections."
  (-contains-key? [coll k]
    "Returns true if k is a key in coll.")
  (-assoc [coll k v]
    "Returns a new collection of coll with a mapping from key k to
     value v added to it."))

(defn assoc
  {:inline (fn [map key val] `(-assoc ~map ~key ~val))
   :inline-arities #{3}}
  ([map key val] (-assoc map key val))
  ([map key val & kvs]
   (let [ret (-assoc map key val)]
     (if kvs
       (if (next kvs)
         (recur ret (first kvs) (second kvs) (nnext kvs))
         (throw (ArgumentError. "assoc expects even number of arguments after map/vector, found odd number")))
       ret))))

(defprotocol ITransientAssociative
  "Protocol for adding associativity to transient collections."
  (-assoc! [tcoll key val]
    "Returns a new transient collection of tcoll with a mapping from key to
     val added to it."))

(defn assoc!
  "When applied to a transient map, adds mapping of key(s) to
  val(s). When applied to a transient vector, sets the val at index.
  Note - index must be <= (count vector). Returns coll."
  ([coll key val] (-assoc! coll key val))
  ([coll key val & kvs]
   (let [ret (-assoc! coll key val)]
     (if kvs
       (recur ret (first kvs) (second kvs) (nnext kvs))
       ret))))

(defprotocol ITransientVector
  "Protocol for adding vector functionality to transient collections."
  (-assoc-n! [tcoll n val]
    "Returns tcoll with value val added at position n.")
  (-pop! [tcoll]
    "Returns tcoll with the last item removed from it."))

(defn pop!
  "Removes the last item from a transient vector. If
  the collection is empty, throws an exception. Returns coll"
  [coll]
  (-pop! coll))

(defprotocol IEquiv
  "Protocol for adding value comparison functionality to a type."
  (-equiv [o other]
    "Returns true if o and other are equal, false otherwise."))

(extend-type fallback
  IEquiv
  (-equiv [o other] (.== o other)))

(defn ^bool =
  #_"Equality. Returns true if x equals y, false if not. Same as
  Java x.equals(y) except it also works for nil, and compares
  numbers and collections in a type-independent manner.  Clojure's immutable data
  structures define equals() (and thus =) as a value, not an identity,
  comparison."
  ;; TODO put inline when tagging is working
  #_{:inline (fn [x y] `(. clojure.lang.Util equiv ~x ~y))
     :inline-arities #{2}
     :added "1.0"}
  ([x] true)
  ([x y] (-equiv x y))
  ([x y & more]
   (if (-equiv x y)
     (if (next more)
       (recur y (first more) (next more))
       (-equiv y (first more)))
     false)))

(defn ^bool not=
  "Same as (not (= obj1 obj2))"
  ([x] false)
  ([x y] (not (= x y)))
  ([x y & more]
   (not (apply = x y more))))

(defprotocol IIndexed
  "Protocol for collections to provide indexed-based access to their items."
  (-nth [coll n] [coll n not-found]
    "Returns the value at the index n in the collection coll.
     Returns not-found if index n is out of bounds and not-found is supplied."))

(extend-type List
  IIndexed
  (-nth [l n] (. l "[]" n))
  (-nth [l n not-found]
    (if (< -1 n (.-length l))
      (. l "[]" n)
      not-found)))

(extend-type String
  IIndexed
  (-nth [l n] (. l "[]" n))
  (-nth [l n not-found]
    (if (< n (.-length l))
      (. l "[]" n)
      not-found)))

(extend-type MapEntry
  IIndexed
  (-nth [me n]
    (cond
      (== 0 n) (.-key me)
      (== 1 n) (.-value me)
      :else (throw (IndexError. n me))))
  (-nth [me n not-found]
    (cond
      (== 0 n) (.-key me)
      (== 1 n) (.-value me)
      :else not-found)))

(extend-type Match
  IIndexed
  (-nth [m n]
    (.group m n))
  (-nth [m n not-found]
    (if (<= 0 n (.-groupCount m))
      (.group m n)
      not-found)))

(extend-type Null
  IIndexed
  (-nth [m n] nil)
  (-nth [m n not-found] not-found))

(extend-type fallback
  IIndexed
  (-nth [coll n]
    (when (neg? n) (throw (IndexError. n coll)))
    (loop [xs (seq coll) ^int i n]
      (cond
        (nil? xs) (throw (IndexError. n coll))
        (zero? i) (first xs)
        :else (recur (next xs) (.- i 1)))))
  (-nth [coll n not-found]
    (when (neg? n) not-found)
    (loop [xs (seq coll) ^int i n]
      (cond
        (nil? xs) not-found
        (zero? i) (first xs)
        :else (recur (next xs) (.- i 1))))))

(defn nth
  #_{:inline-arities #{2 3}
   :inline (fn
             ([coll n] `(-nth ~coll (.toInt ~^num n)))
             ([coll n not-found] `(-nth ~coll (.toInt ~^num n) ~not-found)))}
  ;; TODO : check performance of toInt...
  ([coll ^num n] (-nth coll (.toInt n)))
  ([coll ^num n not-found] (-nth coll (.toInt ^num n) not-found)))

(defprotocol ILookup
  "Protocol for looking up a value in a data structure."
  (-lookup [o k] [o k not-found]
    "Use k to look up a value in o. If not-found is supplied and k is not
     a valid value that can be used for look up, not-found is returned."))

(extend-type fallback
  ILookup
  (-lookup [o k]
    (when (and (dart/is? k int) (satisfies? IIndexed o))
      (-nth o k nil)))
  (-lookup [o k not-found]
    ;; `if` is used and not `when` - clj compliant e.g: (get [1 2] (int-array 1) :default) -> :default
    (if (and (dart/is? k int) (satisfies? IIndexed o))
      (-nth o k not-found)
      not-found)))

(defn get
  "Returns the value mapped to key, not-found or nil if key not present."
  ([map key]
   (-lookup map key))
  ([map key not-found]
   (-lookup map key not-found)))

(defprotocol IStack
  "Protocol for collections to provide access to their items as stacks. The top
  of the stack should be accessed in the most efficient way for the different
  data structures."
  (-peek [coll]
    "Returns the item from the top of the stack. Is used by cljs.core/peek.")
  (-pop [coll]
    "Returns a new stack without the item on top of the stack. Is used
     by cljs.core/pop."))

(extend-type fallback
  IStack
  (-peek [coll]
    (when-not (nil? coll)
      (throw (Exception. (str "Peek not supported on " (.-runtimeType coll))))))
  (-pop [coll]
    (when-not (nil? coll)
      (throw (Exception. (str "Pop not supported on " (.-runtimeType coll)))))))

(defn peek
  "For a list or queue, same as first, for a vector, same as, but much
  more efficient than, last. If the collection is empty, returns nil."
  [coll]
  (-peek coll))

(defn pop
  "For a list or queue, returns a new list/queue without the first
  item, for a vector, returns a new vector without the last item. If
  the collection is empty, throws an exception.  Note - not the same
  as next/butlast."
  [coll]
  (-pop coll))

(defprotocol IMap
  "Protocol for adding mapping functionality to collections."
  (-dissoc [coll k]
    "Returns a new collection of coll without the mapping for key k."))

(defn ^bool map?
  [x]
  (satisfies? IMap x))

(extend-type fallback
  IMap
  (-dissoc [coll k]
    (when-not (nil? coll)
      (throw (Exception. (str "Dissoc not supported on " (.-runtimeType coll)))))))

(defn dissoc
  "dissoc[iate]. Returns a new map of the same (hashed/sorted) type,
  that does not contain a mapping for key(s)."
  ([map] map)
  ([map key]
   (-dissoc map key))
  ([map key & ks]
   (when-some [ret (-dissoc map key)]
     (if ks
       (recur ret (first ks) (next ks))
       ret))))

(defprotocol IWithMeta
  "Protocol for adding metadata to an object."
  (-with-meta [o meta]
    "Returns a new object with value of o and metadata meta added to it."))

(defn with-meta
  {:inline (fn [obj m] `(-with-meta ~obj ~m))
   :inline-arities #{2}}
  [obj m]
  (when-not (map? m)
    (throw (Exception. (str "class " (.-runtimeType m) " cannot be cast to cljd.core/IMap"))))
  (-with-meta obj m))

(defprotocol IMeta
  "Protocol for accessing the metadata of an object."
  (-meta [o] "Returns the metadata of object o."))

(defn meta
  {:inline (fn [obj] `(-meta ~obj))
   :inline-arities #{1}}
  [obj]
  (-meta obj))

(defprotocol IHash
  "Protocol for adding hashing functionality to a type."
  (-hash [o] "Returns the hash code of o.")
  (-hash-realized? [o] "Returns whether the hash has already been realized or not."))

(extend-type bool
  IHash
  (-hash [o]
    (cond
      (true? o) 1231
      (false? o) 1237))
  (-hash-realized? [o] true))

(declare ^:dart m3-hash-int)

(extend-type int
  IHash
  (-hash [o]
    (m3-hash-int o)))

(extend-type double
  IHash
  (-hash [o]
    ; values taken from cljs
    (cond
      (.== (.-negativeInfinity double) o) -1048576
      (.== (.-infinity double) o) 2146435072
      (.== (.-nan double) o) 2146959360
      true (m3-hash-int (.-hashCode o))))
  (-hash-realized? [o] true))

(extend-type Null
  IHash
  (-hash [o] 0)
  (-hash-realized? [o] true))

(extend-type Object
  IHash
  (-hash [o] (m3-hash-int (.-hashCode o)))
  (-hash-realized? [o] true))

(defn ^int hash
  {:inline (fn [o] `^dart:core/int (-hash ~o))
   :inline-arities #{1}}
  [o] (-hash o))

(defn ^bool -equiv-sequential
  "Assumes x is sequential."
  [x y]
  (and (sequential? y)
    (or (not (counted? x)) (not (counted? y))
      (== (count x) (count y)))
    (or (not (-hash-realized? x)) (not (-hash-realized? y))
      (== (-hash x) (-hash y)))
    (loop [xs (seq x) ys (seq y)]
      (cond
        (nil? xs) (nil? ys)
        (nil? ys) false
        (= (first xs) (first ys)) (recur (next xs) (next ys))
        :else false))))

(defn ^bool -equiv-map
  "Test map equivalence. Returns true if x equals y, otherwise returns false."
  [x y]
  (boolean
    ;; TODO : add record? when there are records
    (when (and (map? y) #_(not (record? y)))
      ; assume all maps are counted
      (when (== (count x) (count y))
        (let [never-equiv (Object.)]
          (if (satisfies? IKVReduce x)
            (reduce-kv
              (fn [_ k v]
                (if (= (get y k never-equiv) v)
                  true
                  (reduced false)))
              true x)
            (every?
              (fn [xkv]
                (= (get y (first xkv) never-equiv) (second xkv)))
              x)))))))

(deftype ^:abstract EquivSequentialHashMixin []
  #_#_#_#_Object
  (^bool check_hash [x y]
    ;; TODO : use counted?
    ;; (and (counted? x) (counted? y) (== (count x) (count y)))
   )
  (^bool equiv_sequential [x y]
  )
  (^bool equiv_chunked [x y]
    (if (.check_hash x y)
      (let [other-seq (seq y)]
        (if (chunked-seq? other-seq)
          (loop [me-seq (seq x)
                 other-seq other-seq
                 me-cf (chunk-first me-seq)
                 other-cf (chunk-first other-seq)
                 ^int ime (count me-cf)
                 ^int iother (count other-cf)
                 j 0]
            (if (== ime iother)
              (if me-seq
                (if (== (-nth me-cf j) (-nth other-cf j))
                  (let [j (inc j)]
                    (if (< j ime)
                      (recur me-seq other-seq me-cf other-cf ime iother j)
                      (let [me-seq (chunk-next me-seq)
                            me-cf (some-> me-seq chunk-first)
                            other-seq (chunk-next other-seq)
                            other-cf (some-> other-seq chunk-first)]
                        (recur me-seq other-seq me-cf other-cf (count me-cf) (count other-cf) 0))))
                  false)
                true)
              false))
          (.equiv_sequential x y)))
      false))
  IEquiv
  (-equiv [x y] (-equiv-sequential x y)))

(deftype Keyword [^String? ns ^String name ^int _hash]
  ^:mixin ToStringMixin
  IPrint
  (-print [o ^StringSink sink]
    (.write sink ":")
    (when ns (.write sink ns) (.write sink "/"))
    (.write sink name))
  IEquiv
  (-equiv [this other]
    (or (identical? this other)
      (and (dart/is? other Keyword)
        ;; TODO : use `ns` instead of `(.-ns this)`; I used this form
        ;; because their is a bug in optional type emit
        (.== (.-ns this) (.-ns other)) (.== name (.-name other)))))
  IFn
  (-invoke [kw coll]
    (get coll kw))
  (-invoke [kw coll not-found]
    (get coll kw not-found))
  IHash
  (-hash [this] _hash)
  INamed
  (-name [_] name)
  (-namespace [_] ns))

(defn keyword
  "Returns a Keyword with the given namespace and name.  Do not use :
  in the keyword strings, it will be added automatically."
  ([s] (cond
         (keyword? s) s
         #_#_(symbol? s) (symbol (namespace s) (name s))
         (string? s) (Keyword. nil s (hash-combine 0 (hash-string* s)))))
  ([ns name]
   (Keyword. ns name (hash-combine (hash-string* ns) (hash-string* name)))))

(defn ^bool keyword?
  [x]
  (dart/is? x Keyword))

(defn ^bool simple-keyword?
  "Return true if x is a keyword without a namespace"
  [x]
  (and (keyword? x) (nil? (namespace x))))

(defn ^bool qualified-keyword?
  "Return true if x is a keyword with a namespace"
  [x]
  (and (keyword? x) (namespace x) true))

(defprotocol IFind
  "Protocol for implementing entry finding in collections."
  (-find [coll k] "Returns the map entry for key, or nil if key not present."))

(extend-type fallback
  IFind
  (-find [coll k]
    (when-not (nil? coll)
      (throw (Exception. (str "Find not supported on " (.-runtimeType coll)))))))

(defn find
  "Returns the map entry for key, or nil if key not present."
  [map key]
  (-find map key))

(defprotocol IMapEntry
  "Protocol for examining a map entry."
  (-key [coll]
    "Returns the key of the map entry.")
  (-val [coll]
    "Returns the value of the map entry."))

(defn ^bool map-entry?
  [x]
  (satisfies? IMapEntry x))

(defn key
  "Returns the key of the map entry."
  [^MapEntry e]
  (.-key e))

(defn val
  "Returns the key of the map entry."
  [^MapEntry e]
  (.-value e))

(defprotocol IEditableCollection
  "Protocol for collections which can transformed to transients."
  (-as-transient [coll]
    "Returns a new, transient version of the collection, in constant time."))

(defn transient
  "Returns a new, transient version of the collection, in constant time."
  [coll]
  (-as-transient coll))

(defprotocol ITransientCollection
  "Protocol for adding basic functionality to transient collections."
  (-conj! [tcoll val]
    "Adds value val to tcoll and returns tcoll.")
  (-persistent! [tcoll]
    "Creates a persistent data structure from tcoll and returns it."))

(defn conj!
  "Adds x to the transient collection, and return coll. The 'addition'
  may happen at different 'places' depending on the concrete type."
  ([] (transient []))
  ([coll] coll)
  ([coll x]
   (-conj! coll x)))

(defn persistent!
  "Returns a new, persistent version of the transient collection, in
  constant time. The transient collection cannot be used after this
  call, any such use will throw an exception."
  [coll]
  (-persistent! coll))

(defprotocol ITransientMap
  "Protocol for adding mapping functionality to transient collections."
  (-dissoc! [tcoll key]
    "Returns a new transient collection of tcoll without the mapping for key."))

(defn dissoc!
  "Returns a transient map that doesn't contain a mapping for key(s)."
  ([tcoll key] (-dissoc! tcoll key))
  ([tcoll key & ks]
   (let [ntcoll (-dissoc! tcoll key)]
     (if ks
       (recur ntcoll (first ks) (next ks))
       ntcoll))))

(defprotocol ISet
  "Protocol for adding set functionality to a collection."
  (-disjoin [coll v]
    "Returns a new collection of coll that does not contain v."))

(defn ^bool set?
  "Returns true if x satisfies ISet"
  [x]
  (satisfies? ISet x))

(extend-type Null
  ISet
  (-disjoin [coll v] nil))

(defn disj
  "disj[oin]. Returns a new set of the same (hashed/sorted) type, that
  does not contain key(s)."
  ([set] set)
  ([set key]
   (-disjoin set key))
  ([set key & ks]
   (let [ret (-disjoin set key)]
     (if ^some ks
       (recur ret (first ks) (next ks))
       ret))))

(defprotocol ITransientSet
  "Protocol for adding set functionality to a transient collection."
  (-disjoin! [tcoll v]
    "Returns tcoll without v."))

(defn disj!
  "disj[oin]. Returns a transient set of the same (hashed/sorted) type, that
  does not contain key(s)."
  ([set] set)
  ([set key]
   (-disjoin! set key))
  ([set key & ks]
   (let [ret (-disjoin! set key)]
     (if ^some ks
       (recur ret (first ks) (next ks))
       ret))))

(defprotocol IAtom
  "Marker protocol indicating an atom.")

(defprotocol IReset
  "Protocol for adding resetting functionality."
  (-reset! [o new-value]
    "Sets the value of o to new-value."))

(defprotocol ISwap
  "Protocol for adding swapping functionality."
  (-swap! [o f] [o f a] [o f a b] [o f a b xs]
    "Swaps the value of o to be (apply f current-value-of-atom args)."))

(defprotocol IWatchable
  "Protocol for types that can be watched. Currently only implemented by Atom."
  (-notify-watches [this oldval newval]
    "Calls all watchers with this, oldval and newval.")
  (-add-watch [this key f]
    "Adds a watcher function f to this. Keys must be unique per reference,
     and can be used to remove the watch with -remove-watch.")
  (-remove-watch [this key]
    "Removes watcher that corresponds to key from this."))

(deftype Atom [^:mutable state meta ^Function? validator ^:mutable ^PersistentHashMap watches]
  IAtom
  IEquiv
  (-equiv [o other] (identical? o other))
  IDeref
  (-deref [_] state)
  IMeta
  (-meta [_] meta)
  IWatchable
  (-notify-watches [this oldval newval]
    (when-some [s (seq watches)]
      (loop [[[key f] & r] s]
        (f key this oldval newval)
        (when r (recur r)))))
  (-add-watch [this key f]
    (set! watches (assoc watches key f))
    this)
  (-remove-watch [this key]
    (set! watches (dissoc watches key))
    this)
  IHash
  (-hash [this] (.-hashCode this))
  ISwap
  (-swap! [this f] (set-and-validate-atom-state! this (f state)))
  (-swap! [this f a] (set-and-validate-atom-state! this (f state a)))
  (-swap! [this f a b] (set-and-validate-atom-state! this (f state a b)))
  (-swap! [this f a b xs] (set-and-validate-atom-state! this (apply f state a b xs)))
  IReset
  (-reset! [this new-value] (set-and-validate-atom-state! this new-value)))

(defn- validate-atom-state [validator new-state]
  ;; TODO : maybe add some try/catch (see ARef.java)
  (when-not (validator new-state)
    (throw (Exception. "Validator rejected reference state"))))

(defn- set-and-validate-atom-state! [^Atom a new-state]
  (when-some [validator (.-validator a)]
    (validate-atom-state validator new-state))
  (let [old-state (.-state a)]
    (set! (.-state a) new-state)
    (-notify-watches a old-state new-state)
    new-state))

(defn ^Atom atom
  ([x]
   (Atom. x nil nil {}))
  ([x & {:keys [meta validator] :as o}]
   (when-not (or (nil? meta) (map? meta))
     (throw (Exception. "meta must satisfies IMap.")))
   (when validator
     (validate-atom-state validator x))
   (Atom. x meta validator {})))

(defn ^Atom add-watch
  "Adds a watch function to an atom reference. The watch fn must be a
  fn of 4 args: a key, the reference, its old-state, its
  new-state. Whenever the reference's state might have been changed,
  any registered watches will have their functions called. The watch
  fn will be called synchronously. Note that an atom's state
  may have changed again prior to the fn call, so use old/new-state
  rather than derefing the reference. Keys must be unique per
  reference, and can be used to remove the watch with remove-watch,
  but are otherwise considered opaque by the watch mechanism.  Bear in
  mind that regardless of the result or action of the watch fns the
  atom's value will change.  Example:

      (def a (atom 0))
      (add-watch a :inc (fn [k r o n] (assert (== 0 n))))
      (swap! a inc)
      ;; Assertion Error
      (deref a)
      ;=> 1"
  [^Atom reference key ^Function fn]
  (-add-watch reference key fn))

(defn ^Atom remove-watch
  "Removes a watch (set by add-watch) from a reference"
  [^Atom reference key]
  (-remove-watch reference key))

(defn swap!
  "Atomically swaps the value of atom to be:
  (apply f current-value-of-atom args). Note that f may be called
  multiple times, and thus should be free of side effects.  Returns
  the value that was swapped in."
  ([^Atom a f] (-swap! a f))
  ([^Atom a f x] (-swap! a f x))
  ([^Atom a f x y] (-swap! a f x y))
  ([^Atom a f x y & more] (-swap! a f x y more)))

(defn reset-vals!
  "Sets the value of atom to newval. Returns [old new], the value of the
   atom before and after the reset."
  [^Atom a newval]
  (let [old-state (.-state a)]
    [old-state (set-and-validate-atom-state! a newval)]))

(defn swap-vals!
  "Atomically swaps the value of atom to be:
  (apply f current-value-of-atom args). Note that f may be called
  multiple times, and thus should be free of side effects.
  Returns [old new], the value of the atom before and after the swap."
  ([^Atom a f]
   (reset-vals! a (swap! a f)))
  ([^Atom a f x]
   (reset-vals! a (swap! a f x)))
  ([^Atom a f x y]
   (reset-vals! a (swap! a f x y)))
  ([^Atom a f x y & more]
   (reset-vals! a (swap! a f x y more))))

(defprotocol IEmptyableCollection
  "Protocol for creating an empty collection."
  (-empty [coll]
    "Returns an empty collection of the same category as coll. Used
     by cljd.core/empty."))

(extend-type fallback
  IEmptyableCollection
  (-empty [coll] nil))

(defn empty
  "Returns an empty collection of the same category as coll, or nil"
  [coll]
  (-empty coll))

(defprotocol IVolatile
  "Protocol for adding volatile functionality."
  (-vreset! [o new-value]
    "Sets the value of volatile o to new-value without regard for the
     current value. Returns new-value."))

(defn ^bool volatile?
  "Returns true if x is a volatile."
  [x] (satisfies? IVolatile x))

(defn vreset!
  "Sets the value of volatile to newval without regard for the
   current value. Returns newval."
  [vol newval]
  (-vreset! vol newval))

(defmacro vswap!
  "Non-atomically swaps the value of the volatile as if:
   (apply f current-value-of-vol args). Returns the value that
   was swapped in."
  [vol f & args]
  `(-vreset! ~vol (~f (-deref ~vol) ~@args)))

(deftype Volatile [^:mutable state]
  IVolatile
  (-vreset! [_ new-state]
    (set! state new-state))
  IDeref
  (-deref [_] state))

(defn ^Volatile volatile!
  "Creates and returns a Volatile with an initial value of val."
  [val]
  (Volatile. val))

(defn ^bool not
  "Returns true if x is logical false, false otherwise."
  {:inline (fn [x] `^bool (if ~x false true))
   :inline-arities #{1}}
  [x] (if x false true))

(defmacro if-not
  "Evaluates test. If logical false, evaluates and returns then expr,
  otherwise else expr, if supplied, else nil."
  ([test then] `(if-not ~test ~then nil))
  ([test then else]
   `(if (not ~test) ~then ~else)))

;; op must be a string as ./ is not legal in clj/java so we must use the (. obj op ...) form
(defn ^:bootstrap ^:private nary-inline
  ([op] (nary-inline nil nil op))
  ([unary-fn op] (nary-inline nil unary-fn op))
  ([zero unary-fn op]
   (fn
     ([] zero)
     ([x] (unary-fn x))
     ([x y] `(. ~x ~op ~y))
     ([x y & more] (reduce (fn [a b] `(. ~a ~op ~b)) `(. ~x ~op ~y) more)))))

(defn ^:bootstrap ^:private nary-cmp-inline
  [op]
  (fn
    ([x] true)
    ([x y] `(. ~x ~op ~y))
    ([x y & more]
     (let [bindings (mapcat (fn [x] [(gensym op) x]) (list* x y more))]
       `(let [~@bindings]
          (.&&
            ~@(map (fn [[x y]] `(. ~x ~op ~y))
                (partition 2 1 (take-nth 2 bindings)))))))))

(defn ^:bootstrap ^:private >0? [n] (< 0 n))
(defn ^:bootstrap ^:private >1? [n] (< 1 n))

(defn ^bool ==
  {:inline (nary-cmp-inline "==")
   :inline-arities >0?}
  ([x] true)
  ([x y] (. x "==" y))
  ([x y & more]
   (if (== x y)
     (if (next more)
       (recur y (first more) (next more))
       (== y (first more)))
     false)))

(defn ^num *
  {:inline (nary-inline 1 identity "*")
   :inline-arities any?}
  ([] 1)
  ([x] x)
  ([x y] (.* x y))
  ([x y & more]
   (reduce * (* x y) more)))

(defn ^num /
  {:inline (nary-inline (fn [x] (list '. 1 "/" x)) "/")
   :inline-arities >0?}
  ([x] (. 1 "/" x))
  ([x y] (. x "/" y))
  ([x y & more]
   (reduce / (/ x y) more)))

;; TODO type hint
(defn rem
  {:inline (fn [num div] `(.remainder ~num ~div))
   :inline-arities #{2}}
  [num div]
  (.remainder num div))

(defn ^num +
  {:inline (nary-inline 0 identity "+")
   :inline-arities any?}
  ([] 0)
  ;; TODO: cast to num ??
  ([x] x)
  ([x y] (.+ x y))
  ([x y & more]
   (reduce + (+ x y) more)))

(defn ^num -
  {:inline (nary-inline (fn [x] (list '. x "-")) "-")
   :inline-arities >0?}
  ([x] (.- 0 x))
  ([x y] (.- x y))
  ([x y & more]
   (reduce - (- x y) more)))

(defn ^bool <=
  {:inline (nary-cmp-inline "<=")
   :inline-arities >0?}
  ([x] true)
  ([x y] (.<= x y))
  ([x y & more]
   (if (<= x y)
     (if (next more)
       (recur y (first more) (next more))
       (<= y (first more)))
     false)))

(defn ^bool <
  {:inline (nary-cmp-inline "<")
   :inline-arities >0?}
  ([x] true)
  ([x y] (.< x y))
  ([x y & more]
   (if (< x y)
     (if (next more)
       (recur y (first more) (next more))
       (< y (first more)))
     false)))

(defn ^bool >=
  {:inline (nary-cmp-inline ">=")
   :inline-arities >0?}
  ([x] true)
  ([x y] (.>= x y))
  ([x y & more]
   (if (>= x y)
     (if (next more)
       (recur y (first more) (next more))
       (>= y (first more)))
     false)))

(defn ^bool >
  {:inline (nary-cmp-inline ">")
   :inline-arities >0?}
  ([x] true)
  ([x y] (.> x y))
  ([x y & more]
   (if (> x y)
     (if (next more)
       (recur y (first more) (next more))
       (> y (first more)))
     false)))

(defn ^bool pos?
  {:inline-arities #{1}
   :inline (fn [n] `(< 0 ~n))}
  [n] (< 0 n))

(defn ^bool neg?
  {:inline-arities #{1}
   :inline (fn [n] `(> 0 ~n))}
  [n] (> 0 n))

(defn ^bool zero?
  {:inline (fn [num] `(.== 0 ~num))
   :inline-arities #{1}}
  [num]
  (== 0 num))

(defn ^num inc
  {:inline (fn [x] `(.+ ~x 1))
   :inline-arities #{1}}
  [x] (.+ x 1))

(defn ^num dec
  {:inline (fn [x] `(.- ~x 1))
   :inline-arities #{1}}
  [x]
  (.- x 1))

(defn quick-bench* [run]
  (let [sw (Stopwatch.)
        _ (dart:core/print "Calibrating")
        n (loop [n 1]
            (doto sw .reset .start)
            (run n)
            (.stop sw)
            (if (< (.-elapsedMicroseconds sw) 100000)
              (recur (* 2 n))
              n))]
    (dart:core/print (str "Running (batch size: " n ")"))
    (loop [cnt 0 mean 0.0 m2 0.0 rem (* 2 60 1000 1000)]
      (doto sw .reset .start)
      (run n)
      (.stop sw)
      (let [t (.-elapsedMicroseconds sw)
            rem (- rem t)
            cnt (inc cnt)
            delta (- t mean)
            mean (+ mean (/ delta cnt))
            delta' (- t mean)
            m2 (+ m2 (* delta delta'))]
        (if (or (< 0 rem) (< cnt 2))
          (recur cnt mean m2 rem)
          (let [sd (math/sqrt (/ m2 (dec cnt)))]
            ; pretty sure that dividing sd is wrong TODO find the right formula
            (dart:core/print (str (/ mean n) " (+/-" (/ sd n) ") us"))))))))

(defmacro quick-bench [& body]
  `(quick-bench* (fn [n#] (dotimes [_ n#] ~@body))))

;; array ops
(defn aget
  "Returns the value at the index/indices. Works on Java arrays of all
  types."
  {:inline (fn [array idx] `(. ~array "[]" ~idx))
   :inline-arities #{2}}
  ([^List array ^int idx]
   (. array "[]" idx))
  ([^List array ^int idx & idxs]
   (apply aget (aget array idx) idxs)))

(defn aset
  "Sets the value at the index/indices. Works on Java arrays of
  reference types. Returns val."
  {:inline (fn [a i v] `(let [v# ~v] (. ~a "[]=" ~i v#) v#))
   :inline-arities #{3}}
  ([^List array ^int idx val]
   (. array "[]=" idx val) val)
  ([^List array ^int idx ^int idx2 & idxv]
   (apply aset (aget array idx) idx2 idxv)))

(defn ^List aresize [^List a ^int from ^int to pad]
  (let [a' (.filled List to pad)]
    (dotimes [i from]
      (aset a' i (aget a i)))
    a'))

(defn ^List ashrink [^List a ^int to]
  (let [a' (.filled List to ^dynamic (do nil))]
    (dotimes [i to]
      (aset a' i (aget a i)))
    a'))

(defn ^int alength
  {:inline (fn [array] `(.-length ~array))
   :inline-arities #{1}}
  [^List array] (.-length array))

(defn ^List aclone
  {:inline (fn [arr] `(.from dart:core/List ~arr .& :growable false))
   :inline-arities #{1}}
  [^List arr]
  (.from List arr .& :growable false))

;; bit ops
(defn ^int bit-not
  "Bitwise complement"
  {:inline (fn [x] `(. ~x "~"))
   :inline-arities #{1}}
  [x] (. x "~"))

(defn ^int bit-and
  "Bitwise and"
  {:inline (nary-inline "&")
   :inline-arities >1?}
  ([x y] (. x "&" y))
  ([x y & more]
   (reduce bit-and (bit-and x y) more)))

(defn ^int bit-or
  "Bitwise or"
  {:inline (nary-inline "|")
   :inline-arities >1?}
  ([x y] (. x "|" y))
  ([x y & more]
   (reduce bit-or (bit-or x y) more)))

(defn ^int bit-xor
  "Bitwise exclusive or"
  {:inline (nary-inline "^")
   :inline-arities >1?}
  ([x y] (. x "^" y))
  ([x y & more]
   (reduce bit-xor (bit-xor x y) more)))

(defn ^int bit-and-not
  "Bitwise and with complement"
  {:inline (fn
              ([x y] `(bit-and ~x (bit-not ~y)))
              ([x y & more] (reduce (fn [a b] `(bit-and ~a (bit-not ~b))) `(bit-and ~x (bit-not ~y)) more)))
   :inline-arities >1?}
  ([x y] (bit-and x (bit-not y)))
  ([x y & more]
   (reduce bit-and-not (bit-and-not x y) more)))

(defn ^int bit-shift-left
  "Bitwise shift left"
  {:inline (fn [x n] `(. ~x "<<" (bit-and ~n 63)))
   :inline-arities #{2}}
  ; dart does not support negative n values. bit-and acts as a modulo.
  [x n] (. x "<<" (bit-and n 63)))

(defn ^int bit-shift-right
  {:inline (fn [x n] `(. ~x ">>" (bit-and ~n 63)))
   :inline-arities #{2}}
  ; dart does not support negative n values. bit-and acts as a modulo.
  [x n] (. x ">>" (bit-and n 63)))

(defn ^int bit-clear
  "Clear bit at index n"
  {:inline (fn [x n] `(bit-and ~x (bit-not (bit-shift-left 1 ~n))))
   :inline-arities #{2}}
  [x n] (bit-and x (bit-not (bit-shift-left 1 n))))

(defn ^int bit-set
  "Set bit at index n"
  {:inline (fn [x n] `(bit-or ~x (bit-shift-left 1 ~n)))
   :inline-arities #{2}}
  [x n] (bit-or x (bit-shift-left 1 n)))

(defn ^int bit-flip
  "Flip bit at index n"
  {:inline (fn [x n] `(bit-xor ~x (bit-shift-left 1 ~n)))
   :inline-arities #{2}}
  [x n] (bit-xor x (bit-shift-left 1 n)))

;; it might be faster to use (== 1 (bit-and 1 (bit-shift-right x n))) -> to benchmark
(defn ^bool bit-test
  "Test bit at index n"
  {:inline (fn [x n] `(.-isOdd (bit-shift-right ~x ~n)))
   :inline-arities #{2}}
  [x n] (.-isOdd (bit-shift-right x n)))

(defn ^int u32-bit-count [^int v]
  (let [v (- v (bit-and (bit-shift-right v 1) 0x55555555))
        v (+ (bit-and v 0x33333333) (bit-and (bit-shift-right v 2) 0x33333333))]
    (bit-and 63 (bit-shift-right (* (bit-and (+ v (bit-shift-right v 4)) 0xF0F0F0F) 0x1010101) 24))))

(defn ^int u32x2-bit-count [^int hi ^int lo]
  (let [hi (- hi (bit-and (bit-shift-right hi 1) 0x55555555))
        lo (- lo (bit-and (bit-shift-right lo 1) 0x55555555))
        v (+ (bit-and hi 0x33333333) (bit-and (bit-shift-right hi 2) 0x33333333)
            (bit-and lo 0x33333333) (bit-and (bit-shift-right lo 2) 0x33333333))]
    (bit-and 127 (bit-shift-right (* (+ (bit-and 0xF0F0F0F v) (bit-and 0xF0F0F0F (bit-shift-right v 4))) 0x1010101) 24))))

(defn ^int mod
  "Modulus of num and div. Truncates toward negative infinity."
  {:inline (fn [num div] `(. ~num "%" ~div))
   :inline-arities #{2}}
  [num div]
  (. num "%" div))

(defn ^int u32
  {:inline (fn [x] `(.& 0xFFFFFFFF ~x))
   :inline-arities #{1}}
  [x] (.& 0xFFFFFFFF x))

(defn ^int u32-add
  {:inline (fn [x y] `(u32 (.+ ~x ~y)))
   :inline-arities #{2}}
  [x y]
  (u32 (.+ x y)))

; can't work for dartjs (see Math/imul)
(defn ^int u32-mul
  {:inline (fn [x y] `(u32 (.* ~x ~y)))
   :inline-arities #{2}}
  [x y]
  (u32 (.* x y)))

(defn ^int u32-bit-shift-right
  {:inline (fn [x n] `(.>> ~x (.& 31 ~n)))
   :inline-arities #{2}}
  [x n]
  (.>> x (.& 31 n)))

(defn ^int u32-bit-shift-left
  {:inline (fn [x n] `(u32 (.<< ~x (.& 31 ~n))))
   :inline-arities #{2}}
  [x n]
  (u32 (.<< x (.& 31 n))))

(defn ^int u32-rol
  {:inline (fn [x n] `(let [x# ~x
                            n# ~n]
                        (.|
                         (u32-bit-shift-left x# n#)
                         (u32-bit-shift-right x# (.- n#)))))
   :inline-arities #{2}}
  [x n]
  (.|
   (u32-bit-shift-left x n)
   (u32-bit-shift-right x (.- n))))

;; murmur3
;; https://en.wikipedia.org/wiki/MurmurHash#Algorithm
(defn ^int m3-mix-k1 [k1]
  (u32-mul (u32-rol (u32-mul k1 0xcc9e2d51) 15) 0x1b873593))

(defn ^int m3-mix-h1 [h1 k1]
  (u32-add (u32-mul (u32-rol (bit-xor h1 k1) 13) 5) 0xe6546b64))

(defn ^int m3-fmix [h1 len]
  ;; TODO : rewrite with as-> when repeat is implemented
  (let [hash (bit-xor h1 len)
        hash (bit-xor hash (u32-bit-shift-right hash 16))
        hash (u32-mul hash 0x85ebca6b)
        hash (bit-xor hash (u32-bit-shift-right hash 13))
        hash (u32-mul hash 0xc2b2ae35)]
    (bit-xor hash (u32-bit-shift-right hash 16))))

(defn ^int m3-hash-u32 [in]
  (if (zero? in)
    0
    (let [k1 (m3-mix-k1 in)
          h1 (m3-mix-h1 0 k1)]
      (m3-fmix h1 4))))

(defn ^int m3-hash-int [in]
  (if (zero? in)
    in
    (let [upper (u32 (bit-shift-right in 32)) ; always 0 in js
          lower (u32 in)
          k (m3-mix-k1 lower)
          h (m3-mix-h1 0 k)
          k (m3-mix-k1 upper)
          h (m3-mix-h1 h k)]
      (m3-fmix h 8))))

(defn ^int hash-combine [^int seed ^int hash]
  ; a la boost
  (bit-xor seed
    (+ hash 0x9e3779b9
      (u32-bit-shift-left seed 6)
      (u32-bit-shift-right seed 2))))

;;http://hg.openjdk.java.net/jdk7u/jdk7u6/jdk/file/8c2c5d63a17e/src/share/classes/java/lang/String.java
(defn- ^int hash-string* [^String s]
  (let [len (.-length s)]
    (if (pos? len)
      (loop [i 0 hash 0]
        (if (< i len)
          (recur (inc i) (+ (u32-mul 31 hash) (.codeUnitAt s i)))
          (m3-hash-u32 hash)))
      0)))

(deftype HashCache [^:mutable ^#/(Map dynamic int) young
                    ^:mutable ^#/(Map dynamic int) old]
  :type-only true
  (insert [_ o ^int h]
    (when (.== 256 (.-length young))
      (let [bak old]
        (set! old young)
        (.clear bak)
        (set! young bak)))
    (. young "[]=" o h))
  (^int? lookup [this o]
   (or (. young "[]" o)
     (when-some [h (. old "[]" o)]
       (.insert this o h)
       h))))

(def ^HashCache -hash-string-cache (HashCache. (new #/(Map dynamic int)) (new #/(Map dynamic int))))

(defn hash-string [s]
  (or ^:some (.lookup -hash-string-cache s)
    (let [h (hash-string* s)]
      (.insert -hash-string-cache s h)
      h)))

(extend-type String
  IHash
  (-hash [o] (hash-string o)))

(defn ^int mix-collection-hash
  "Mix final collection hash for ordered or unordered collections.
   hash-basis is the combined collection hash, count is the number
   of elements included in the basis. Note this is the hash code
   consistent with =, different from .hashCode.
   See http://clojure.org/data_structures#hash for full algorithms."
  [hash-basis count]
  (let [k1 (m3-mix-k1 hash-basis)
        h1 (m3-mix-h1 0 k1)]
    (m3-fmix h1 count)))

(defn ^int hash-ordered-coll
  "Returns the hash code, consistent with =, for an external ordered
   collection implementing Iterable.
   See http://clojure.org/data_structures#hash for full algorithms."
  [coll]
  (loop [^int n 0 ^int hash-code 1 coll (seq coll)]
    (if-not (nil? coll)
      (recur (inc n)
        ;; TODO not sure about u32-add ?
        (u32-add (u32-mul 31 hash-code) ^int (hash (first coll)))
        (next coll))
      (mix-collection-hash hash-code n))))

(defn ^int hash-unordered-coll
  "Returns the hash code, consistent with =, for an external unordered
   collection implementing Iterable. For maps, the iterator should
   return map entries whose hash is computed as
     (hash-ordered-coll [k v]).
   See http://clojure.org/data_structures#hash for full algorithms."
  [coll]
  (loop [n 0 hash-code 0 coll (seq coll)]
    (if-not (nil? coll)
      ;; TODO not sure about u32-add
      (recur (inc n) (u32-add hash-code ^int (hash (first coll))) (next coll))
      (mix-collection-hash hash-code n))))

(defn ^bool identical?
  {:inline (fn [x y] `(dart:core/identical ~x ~y))
   :inline-arities #{2}}
  [x y]
  (dart:core/identical x y))

(defn ^bool true?
  {:inline (fn [x] `(dart:core/identical ~x true))
   :inline-arities #{1}}
  [x]
  (dart:core/identical x true))

(defn ^bool false?
  {:inline (fn [x] `(dart:core/identical ~x false))
   :inline-arities #{1}}
  [x]
  (dart:core/identical x false))

;; TODO : manage all bindings for printing
(defn- print-sequential [^String begin ^String end sequence ^StringSink sink]
  #_(binding [*print-level* (and (not *print-dup*) *print-level* (dec *print-level*))]
      (if (and *print-level* (neg? *print-level*))
        (.write w "#")
        (do
          (.write w begin)
          (when-let [xs (seq sequence)]
            (if (and (not *print-dup*) *print-length*)
              (loop [[x & xs] xs
                     print-length *print-length*]
                (if (zero? print-length)
                  (.write w "...")
                  (do
                    (print-one x w)
                    (when xs
                      (.write w sep)
                      (recur xs (dec print-length))))))
              (loop [[x & xs] xs]
                (print-one x w)
                (when xs
                  (.write w sep)
                  (recur xs)))))
          (.write w end))))
  (.write sink begin)
  (reduce (fn [need-sep x]
            (when need-sep
              (.write sink " "))
            (-print x sink)
            true) false sequence)
  (.write sink end))

(deftype ^:abstract #/(SeqListMixin E)
  []
  Object
  (toString [o]
    (let [sb (StringBuffer.)]
      (-print o sb)
      (.toString sb)))
  IPrint
  (-print [o sink] (print-sequential "(" ")" o sink))
  #/(List E)
  (length [coll ^int val]
    (throw (UnsupportedError. "lenght= not supported on Cons")))
  (add [coll _]
    (throw (UnsupportedError. "add not supported on Cons")))
  ("[]=" [coll ^int index ^E value]
   (throw (UnsupportedError. "[]= not supported on Cons")))
  ("[]" [coll idx] (-nth coll idx))
  (length [coll] (-count coll))
  IIndexed
  (-nth [coll n]
    (when (neg? n) (throw (IndexError. n coll)))
    (loop [xs (-seq coll) ^int i n]
      (cond
        (nil? xs) (throw (IndexError. n coll))
        (zero? i) (first xs)
        :else (recur (next xs) (.- i 1)))))
  (-nth [coll n not-found]
    (when (neg? n) not-found)
    (loop [xs (-seq coll) ^int i n]
      (cond
        (nil? xs) not-found
        (zero? i) (first xs)
        :else (recur (next xs) (.- i 1))))))

(defmacro ensure-hash [hash-key hash-expr]
  #_(core/assert (clojure.core/symbol? hash-key) "hash-key is substituted twice")
  `(let [h# ~hash-key]
     (if (< h# 0)
       (let [h# ~hash-expr]
         (set! ~hash-key h#)
         h#)
       h#)))

(deftype #/(Cons E)
  [meta _first rest ^:mutable ^int __hash]
  ^:mixin EquivSequentialHashMixin
  ^:mixin #/(dart-coll/ListMixin E)
  ^:mixin #/(SeqListMixin E)
  (^#/(Cons R) #/(cast R) [coll]
   (Cons. meta _first rest __hash))
  IList
  IWithMeta
  (-with-meta [coll new-meta]
    (if (identical? new-meta meta)
      coll
      (Cons. new-meta _first rest __hash)))
  IMeta
  (-meta [coll] meta)
  ISeq
  (-first [coll] _first)
  (-rest [coll] (if (nil? rest) () rest))
  (-next [coll] (if (nil? rest) nil (seq rest)))
  ICollection
  (-conj [coll o] (Cons. nil o coll -1))
  IEmptyableCollection
  (-empty [coll] ())
  ISequential
  IHash
  (-hash [coll] (ensure-hash __hash (hash-ordered-coll coll)))
  (-hash-realized? [coll] (.!= -1 __hash))
  ISeqable
  (-seq [coll] coll))

(defn spread
  {:private true}
  [arglist]
  (cond
    (nil? arglist) nil
    (nil? (next arglist)) (seq (first arglist))
    true (cons (first arglist) (spread (next arglist)))))

(defn list*
  "Creates a new seq containing the items prepended to the rest, the
  last of which will be treated as a sequence."
  ([args] (seq args))
  ([a args] (cons a args))
  ([a b args] (cons a (cons b args)))
  ([a b c args] (cons a (cons b (cons c args))))
  ([a b c d & more]
   (cons a (cons b (cons c (cons d (spread more)))))))

(deftype #/(PersistentList E)
  [meta _first rest ^int count ^:mutable ^int __hash]
  ^:mixin #/(dart-coll/ListMixin E)
  ^:mixin #/(SeqListMixin E)
  (^int ^:getter length [coll] count) ; TODO dart resolution through our own types
  (^#/(PersistentList R) #/(cast R) [coll]
   (PersistentList. meta _first rest count __hash))
  ;; invariant: _first is nil when count is zero
  IList
  IWithMeta
  (-with-meta [coll new-meta]
    (if (identical? new-meta meta)
      coll
      (PersistentList. new-meta _first rest count __hash)))
  IMeta
  (-meta [coll] meta)
  ISeq
  (-first [coll] _first)
  (-rest [coll]
    (if (<= count 1)
      ()
      rest))
  (-next [coll]
    (if (<= count 1)
      nil
      rest))
  IStack
  (-peek [coll] _first)
  (-pop [coll]
    (if (pos? count)
      rest
      (throw (ArgumentError. "Can't pop empty list"))))
  ICollection
  (-conj [coll o] (PersistentList. meta o coll (inc count) -1))
  IEmptyableCollection
  (-empty [coll] (-with-meta () meta))
  ISequential
  IHash
  (-hash [coll] (ensure-hash __hash (hash-ordered-coll coll)))
  (-hash-realized? [coll] (.!= -1 __hash))
  ISeqable
  (-seq [coll] (when (< 0 count) coll))
  ICounted
  (-count [coll] count))

(def ^PersistentList -EMPTY-LIST (PersistentList. nil nil nil 0 -1))

(defn list
  "Creates a new list containing the items."
  [& xs]
  ;; TODO : like to-array, find a more efficient way to not rebuild an intermediate array
  (let [arr (reduce (fn [acc item] (.add acc item) acc) #dart[] xs)]
    (loop [i (.-length arr) r ^PersistentList ()]
      (if (< 0 i)
        (recur (dec i) (-conj ^PersistentList r (. arr "[]" (dec i))))
        r))))

(defn cons
  "Returns a new seq where x is the first element and coll is the rest."
  [x coll]
  (cond
    (nil? coll)            (PersistentList. nil x nil 1 -1)
    (satisfies? ISeq coll) (Cons. nil x coll -1)
    true                   (Cons. nil x (seq coll) -1)))

(deftype #/(IteratorSeq E)
  [value ^Iterator iter ^:mutable ^some _rest]
  ^:mixin #/(dart-coll/ListMixin E)
  ^:mixin #/(SeqListMixin E)
  (^#/(IteratorSeq R) #/(cast R) [coll]
   (IteratorSeq. value iter _rest))
  ISeqable
  (-seq [this] this)
  ISeq
  (-first [coll] value)
  (-rest [coll] (or _rest (set! _rest (or (iterator-seq iter) ()))))
  (-next [coll] (-seq (-rest coll))))

(defn ^some iterator-seq [^Iterator iter]
  (when (.moveNext iter)
    (IteratorSeq. (.-current iter) iter nil)))

(extend-type Iterable
  ISeqable
  (-seq [coll] (iterator-seq (.-iterator coll))))

(deftype #/(StringSeq E)
  [string i meta ^:mutable ^int __hash]
  ^:mixin EquivSequentialHashMixin
  ^:mixin #/(dart-coll/ListMixin E)
  ^:mixin #/(SeqListMixin E)
  (^#/(StringSeq R) #/(cast R) [coll]
   (StringSeq. string i meta __hash))
  ISeqable
  (-seq [coll] (when (< i (.-length string)) coll))
  IMeta
  (-meta [coll] meta)
  IWithMeta
  (-with-meta [coll new-meta]
    (if (identical? new-meta meta)
      coll
      (StringSeq. string i new-meta -1)))
  ISeq
  (-first [this] (. string "[]" i))
  (-rest [_]
    (if (< (inc i) (.-length string))
      (StringSeq. string (inc i) nil -1)
      ()))
  (-next [_]
    (if (< (inc i) (.-length string))
      (StringSeq. string (inc i) nil -1)
      nil))
  ICounted
  (-count [_] (- (.-length string) i))
  IIndexed
  (-nth [coll n]
    (if (< n 0)
      (throw (ArgumentError. "Index out of bounds"))
      (let [i (+ n i)]
        (if (< i (.-length string))
          (. string "[]" i)
          (throw (ArgumentError. "Index out of bounds"))))))
  (-nth [coll n not-found]
    (if (< n 0)
      not-found
      (let [i (+ n i)]
        (if (< i (.-length string))
          (. string "[]" i)
          not-found))))
  ISequential
  ICollection
  (-conj [coll o] (cons o coll))
  IEmptyableCollection
  (-empty [coll] ())
  IReduce
  (-reduce [coll f]
    (let [l (.-length string)
          x (. string "[]" i)
          i' (inc i)]
      (if (< i' l)
        (loop [acc x idx i']
          (if (< idx l)
            (let [val (f acc (. string "[]" idx) )]
              (if (reduced? val)
                (deref val)
                (recur val (inc idx))))
            acc))
        x)))
  (-reduce [coll f start]
    (let [l (.-length string)]
      (loop [acc start idx i]
        (if (< idx l)
          (let [val (f acc (. string "[]" idx) )]
            (if (reduced? val)
              (deref val)
              (recur val (inc idx))))
          acc))))
  IHash
  (-hash [coll] (ensure-hash __hash (m3-hash-int (.-hashCode (.substring string i)))))
  (-hash-realized? [coll] (.!= -1 __hash))
  ; TODO : not reversible in clj (is in cljs)
  #_#_IReversible
  (-rseq [coll]
    (let [c (-count coll)]
      (if (pos? c)
        (RSeq. coll (dec c) nil)))))

(extend-type String
  ISeqable
  (-seq [coll] (when (.-isNotEmpty coll) (StringSeq. coll 0 nil -1))))

(defn ^String str
  ([] "")
  ([x] (if (nil? x) "" (.toString x)))
  ([x & xs]
   (let [sb (StringBuffer. (str x))]
     (loop [^some xs xs]
       (when xs
         (.write sb (str (first xs)))
         (recur (next xs))))
     (.toString sb))))

(defn ^String subs
  "Returns the substring of s beginning at start inclusive, and ending
  at end (defaults to length of string), exclusive."
  ([^String s start] (. s (substring start)))
  ([^String s start end] (. s (substring start end))))

(deftype #/(LazySeq E)
  [meta ^:mutable ^some fn ^:mutable s ^:mutable ^int __hash]
  ^:mixin EquivSequentialHashMixin
  ^:mixin #/(dart-coll/ListMixin E)
  ^:mixin #/(SeqListMixin E)
  (^#/(LazySeq R) #/(cast R) [coll]
   (LazySeq. meta fn s __hash))
  (sval [coll]
    (if (nil? fn)
      s
      (do
        (set! s (fn))
        (set! fn nil)
        s)))
  IPending
  (-realized? [coll]
    (not fn))
  IWithMeta
  (-with-meta [coll new-meta]
    (if (identical? new-meta meta)
      coll
      (LazySeq. new-meta #(-seq coll) nil __hash)))
  IMeta
  (-meta [coll] meta)
  ISeqable
  (-seq [coll]
    (.sval coll)
    (when-not (nil? s)
      (loop [ls s]
        (if (dart/is? ls LazySeq)
          (recur (.sval ls))
          (do (set! s ls)
              (seq s))))))
  ISeq
  (-first [coll]
    (-seq coll)
    (when-not (nil? s)
      (first s)))
  (-rest [coll]
    (-seq coll)
    (if-not (nil? s)
      (rest s)
      ()))
  (-next [coll]
    (-seq coll)
    (when-not (nil? s)
      (next s)))
  ICollection
  (-conj [coll o] (cons o coll))
  IEmptyableCollection
  ;; TODO understand why clj & cljs are different on that one
  (-empty [coll] ())
  ISequential
  IHash
  (-hash-realized? [coll] (.!= -1 __hash))
  (-hash [coll] (ensure-hash __hash (hash-ordered-coll coll))))

(defmacro lazy-seq
  "Takes a body of expressions that returns an ISeq or nil, and yields
  a ISeqable object that will invoke the body only the first time seq
  is called, and will cache the result and return it on all subsequent
  seq calls."
  [& body]
  `(new cljd.core/LazySeq nil (fn [] ~@body) nil -1))

;;; PersistentVector

(deftype VectorNode [edit ^List arr])

(defn- ^VectorNode new-path [^int level ^VectorNode node]
  (loop [^int ll level
         ^VectorNode ret node]
    (if (zero? ll)
      ret
      (recur (- ll 5) (VectorNode. nil #dart ^:fixed ^VectorNode [ret])))))

(defn- ^VectorNode push-tail [^PersistentVector pv ^int level ^VectorNode parent ^VectorNode tailnode]
  (let [subidx (bit-and (u32-bit-shift-right (dec (.-cnt pv)) level) 31)
        arr-parent (.-arr parent)
        level (- level 5)
        new-node (cond
                   (zero? level) tailnode ; fast path
                   (< subidx (.-length arr-parent)) ;some? is for transients
                   (if-some [child (. arr-parent "[]" subidx)]
                     (push-tail pv level child tailnode)
                     (new-path level tailnode))
                   :else
                   (new-path level tailnode))]
    (VectorNode. nil (aresize arr-parent subidx (inc subidx) new-node))))

(defn- ^List unchecked-array-for
  "Returns the array where i is located."
  [^VectorNode root ^int shift ^int i]
  (loop [^VectorNode node root
         ^int level shift]
    (if (< 0 level)
      (recur (aget (.-arr node) (bit-and (u32-bit-shift-right i level) 31)) (- level 5))
      (.-arr node))))

(defn- pop-tail [^PersistentVector pv ^int level ^VectorNode node]
  (let [n (- (.-cnt pv) 2)
        subidx (bit-and (u32-bit-shift-right n level) 31)]
    (cond
      (< 5 level)
      (if-some [new-child (pop-tail pv (- level 5) (aget (.-arr node) subidx))]
        (VectorNode. nil (aresize (.-arr node) subidx (inc subidx) new-child))
        (when (< 0 subidx) (VectorNode. nil (ashrink (.-arr node) subidx))))
      (< 0 subidx) (VectorNode. nil (ashrink (.-arr node) subidx)))))

(defn- ^VectorNode do-assoc [^int level ^VectorNode node ^int n val]
  (let [cloned-node (aclone (.-arr node))]
    (if (zero? level)
      (do (aset cloned-node (bit-and n 31) val)
          (VectorNode. nil cloned-node))
      (let [subidx (bit-and (u32-bit-shift-right n level) 31)
            new-child (do-assoc (- level 5) (aget (.-arr node) subidx) n val)]
        (aset cloned-node subidx new-child)
        (VectorNode. nil cloned-node)))))

(defn- pv-reduce
  ([^PersistentVector pv f ^int from]
   (let [cnt (.-cnt pv)
         tail (.-tail pv)
         root (.-root pv)
         shift (.-shift pv)]
     (if (<= cnt from)
       (f)
       (let [tail-off (bit-and-not (dec cnt) 31)
             arr (if (<= tail-off from) tail (unchecked-array-for root shift from))]
         (pv-reduce pv f (inc from) (aget arr (bit-and from 31)))))))
  ([^PersistentVector pv f ^int from init]
   (let [cnt (.-cnt pv)
         tail (.-tail pv)
         root (.-root pv)
         shift (.-shift pv)]
     (if (<= cnt from)
       init
       (let [tail-off (bit-and-not (dec cnt) 31)]
         (loop [acc init
                i from
                arr (if (<= tail-off from) tail (unchecked-array-for root shift from))]
           (let [acc (f acc (aget arr (bit-and i 31)))
                 i' (inc i)]
             (cond
               (reduced? acc) (deref acc)
               (< i' cnt)
               (recur acc i' (cond
                               (< 0 (bit-and i' 31)) arr
                               (== tail-off i') tail
                               :else (unchecked-array-for root shift i')))
               :else acc))))))))

(deftype #/(PVIterator E)
  [^PersistentVector v
   ^:mutable ^int i
   ^:mutable ^List curr]
  #/(Iterator E)
  (current [iter] (aget curr (bit-and (dec i) 31)))
  (moveNext [iter]
    (and (< i (.-cnt v))
      (do
        (when (zero? (bit-and i 31))
          (set! curr (if (<= (bit-and-not (dec (.-cnt v)) 31) i)
                       (.-tail v)
                       (unchecked-array-for (.-root v) (.-shift v) i))))
        (set! i (inc i))
        true))))

(deftype #/(PersistentVector E)
  [meta ^int cnt ^int shift ^VectorNode root ^List tail ^:mutable ^int __hash]
  ^:mixin EquivSequentialHashMixin
  ^:mixin #/(dart-coll/ListMixin E)
  (iterator [v] (PVIterator. v 0 tail)) ; tail assignment is only to pass a non-null list
  ^:mixin #/(SeqListMixin E)
  (^#/(PersistentVector R) #/(cast R) [coll]
   (PersistentVector. meta cnt shift root tail __hash))
  ^:mixin ToStringMixin
  IPrint
  (-print [o sink] (print-sequential "[" "]" o sink))
  IWithMeta
  (-with-meta [coll new-meta]
    (if (identical? new-meta meta)
      coll
      (PersistentVector. new-meta cnt shift root tail __hash)))
  IMeta
  (-meta [coll] meta)
  IStack
  (-peek [coll]
    (when (< 0 cnt) (aget tail (bit-and (dec cnt) 31))))
  (-pop [coll]
    (when (zero? cnt)
      (throw (ArgumentError. "Can't pop empty vector")))
    (let [cnt-1 (dec cnt)]
      (if (zero? cnt-1)
        (-with-meta [] meta)
        (let [new-tail-length (- cnt-1 (bit-and-not cnt-1 31))]
          (cond
            (< 0 new-tail-length)
            (PersistentVector. meta cnt-1 shift root (ashrink tail new-tail-length) -1)
            (== 5 shift)
            (let [new-root-length (dec (u32-bit-shift-right cnt-1 5))
                  arr (.-arr root)]
              (PersistentVector. meta cnt-1 5 (VectorNode. nil (ashrink arr new-root-length)) (.-arr (aget arr new-root-length)) -1))
            ;; root-underflow
            (== (- cnt-1 32) (u32-bit-shift-left 1 shift))
            (PersistentVector. meta cnt-1 (- shift 5) (aget (.-arr root) 0) (unchecked-array-for root shift (dec cnt-1)) -1)
            :else
            (PersistentVector. meta cnt-1 shift (pop-tail coll shift root) (unchecked-array-for root shift (dec cnt-1)) -1))))))
  ICollection
  (-conj [coll o]
    (let [tail-len (bit-and cnt 31)]
      (if (or (pos? tail-len) (zero? cnt))
        (PersistentVector. meta (inc cnt) shift root (aresize tail tail-len (inc tail-len) o) -1)
        (let [root-overflow? (< (u32-bit-shift-left 1 shift) (u32-bit-shift-right cnt 5))
              new-shift (if root-overflow? (+ shift 5) shift)
              new-root (if root-overflow?
                         (VectorNode. nil #dart ^:fixed ^VectorNode [root (new-path shift (VectorNode. nil tail))])
                         (push-tail coll shift root (VectorNode. nil tail)))]
          (PersistentVector. meta (inc cnt) new-shift new-root #dart ^:fixed [o] -1)))))
  IEmptyableCollection
  (-empty [coll] (-with-meta [] meta))
  ISequential
  IHash
  (-hash [coll] (ensure-hash __hash (hash-ordered-coll coll)))
  (-hash-realized? [coll] (.!= -1 __hash))
  ISeqable
  (-seq [coll]
    (cond
      (zero? cnt) nil
      (<= cnt 32) (-seq tail)
      :else (PVChunkedSeq. coll (unchecked-array-for root shift 0) 0 0 nil -1)))
  ICounted
  (-count [coll] cnt)
  IIndexed
  (-nth [coll n]
    (when (or (<= cnt n) (< n 0))
      (throw (ArgumentError. (str "No item " n " in vector of length " cnt))))
    (let [arr (if (<= (bit-and-not (dec cnt) 31) n) tail (unchecked-array-for root shift n))]
      (aget arr (bit-and n 31))))
  (-nth [coll n not-found]
    (if (or (<= cnt n) (< n 0))
      not-found
      (let [arr (if (<= (bit-and-not (dec cnt) 31) n) tail (unchecked-array-for root shift n))]
        (aget arr (bit-and n 31)))))
  ILookup
  (-lookup [coll k]
    (-lookup coll k nil))
  (-lookup [coll k not-found]
    (if (dart/is? k int)
      (-nth coll k not-found)
      not-found))
  IAssociative
  (-assoc [coll k v]
    (if (dart/is? k int)
      (-assoc-n coll k v)
      (throw (ArgumentError. "Vector's key for assoc must be a number."))))
  (-contains-key? [coll k]
    (if (dart/is? k int)
      (and (<= 0 k) (< k cnt))
      false))
  IFind
  (-find [coll n]
    (when-some [v' (-lookup coll n nil)]
      (PersistentMapEntry. n v' -1)))
  #_APersistentVector
  IVector
  (-assoc-n [coll n val]
    (when (or (< cnt n) (< n 0))
      (throw (ArgumentError. (str "Index " n " out of bounds  [0," cnt "]"))))
    (cond
      (== n cnt)
      (-conj coll val)
      (<= (bit-and-not (dec cnt) 31) n)
      (let [new-tail (aclone tail)]
        (aset new-tail (bit-and n 31) val)
        (PersistentVector. meta cnt shift root new-tail -1))
      :else
      (PersistentVector. meta cnt shift (do-assoc shift root n val) tail -1)))
  IReduce
  (-reduce [pv f] (pv-reduce pv f 0))
  (-reduce [pv f init] (pv-reduce pv f 0 init))
  IKVReduce
  (-kv-reduce [pv f init]
    (if (zero? cnt)
      init
      (let [tail-off (bit-and-not (dec cnt) 31)]
        (loop [acc init
               i 0
               arr (if (zero? tail-off) tail (unchecked-array-for root shift 0))]
          (if (< i cnt)
            (let [val (f acc i (aget arr (bit-and i 31)))
                  i' (inc i)]
              (if (reduced? val)
                (deref val)
                (recur val i' (cond
                                (< 0 (bit-and i' 31)) arr
                                (== tail-off i') tail
                                ;; stoppage
                                (== i' cnt) nil
                                :else (unchecked-array-for root shift i')))))
            acc)))))
  IFn
  (-invoke [coll k]
    (-nth coll k))
  (-invoke [coll k not-found]
    (-nth coll k not-found))
  IEditableCollection
  (-as-transient [coll]
    (TransientVector. cnt shift (Object.) root (aresize tail (.-length tail) 32 nil)))

  #_#_IReversible
  (-rseq [coll]
    (when (pos? cnt)
      (RSeq. coll (dec cnt) nil)))
  #_#_IIterable
  (-iterator [this]
    (ranged-iterator this 0 cnt)))

(defn ^bool vector? [x]
  (satisfies? IVector x))

(def -EMPTY-VECTOR (PersistentVector. nil 0 5 (VectorNode. nil (.empty List)) (.empty List) -1))

;; chunks

(defn ^bool chunked-seq?
  "Return true if x satisfies IChunkedSeq."
  [x] (satisfies? IChunkedSeq x))

(deftype ArrayChunk [^List arr ^int off ^int end]
  ICounted
  (-count [_] (- end off))
  IIndexed
  (-nth [coll i]
    (aget arr (+ off ^int i)))
  (-nth [coll i not-found]
    (if (< i 0)
      not-found
      (if (< i (- end off))
        (aget arr (+ off ^int i))
        not-found)))
  IChunk
  (-drop-first [coll]
    (if (== off end)
      (throw (ArgumentError. "-drop-first of empty chunk"))
      (ArrayChunk. arr (inc off) end)))
  (-chunk-reduce [coll f start]
    (loop [acc start ^int idx off]
      (if (< idx end)
        (let [val (f acc (aget arr idx))]
          (if (reduced? val)
            val
            (recur val (inc idx))))
        acc))))

(deftype ChunkBuffer [^:mutable ^List? arr ^:mutable ^int end]
  Object
  (add [_ o]
    (aset ^List arr end o)
    (set! end (inc end))
    nil)
  (chunk [_]
    (let [ret (ArrayChunk. ^List arr 0 end)]
      (set! arr nil)
      ret))
  ICounted
  (-count [_] end))

(defn ^ChunkBuffer chunk-buffer [capacity]
  (ChunkBuffer. (.filled #/(List dynamic) capacity nil) 0))

(deftype #/(ChunkedCons E)
  [chunk more meta ^:mutable ^int __hash]
  ^:mixin EquivSequentialHashMixin
  ^:mixin #/(dart-coll/ListMixin E)
  ^:mixin #/(SeqListMixin E)
  (^#/(ChunkedCons R) #/(cast R) [coll]
   (ChunkedCons. chunk more meta __hash))
  IWithMeta
  (-with-meta [coll new-meta]
    (if (identical? new-meta meta)
      coll
      (ChunkedCons. chunk more new-meta __hash)))
  IMeta
  (-meta [coll] meta)
  ISequential
  ISeqable
  (-seq [coll] coll)
  ISeq
  (-first [coll] (-nth chunk 0))
  (-rest [coll]
    (if (< 1 (-count chunk))
      (ChunkedCons. (-drop-first chunk) more nil -1)
      (if (nil? more)
        ()
        more)))
  (-next [coll]
    (if (< 1 (-count chunk))
      (ChunkedCons. (-drop-first chunk) more nil -1)
      (when-not (nil? more)
        (-seq more))))
  IChunkedSeq
  (-chunked-first [coll] chunk)
  (-chunked-rest [coll]
    (if (nil? more)
      ()
      more))
  (-chunked-next [coll]
    (if (nil? more)
      nil
      more))
  IReduce
  (-reduce [coll f]
    (let [val (-chunk-reduce (-drop-first chunk) f (-nth chunk 0))]
      (if (reduced? val)
        (deref val)
        (-reduce more f val))))
  (-reduce [coll f start]
    (let [val (-chunk-reduce chunk f start)]
      (if (reduced? val)
        (deref val)
        (-reduce more f val))))
  ICollection
  (-conj [this o] (cons o this))
  IEmptyableCollection
  (-empty [coll] ())
  IHash
  (-hash [coll] (ensure-hash __hash (hash-ordered-coll coll)))
  (-hash-realized? [coll] (.!= -1 __hash)))

(defn chunk-cons [chunk rest]
  (if (< 0 (count chunk))
    (ChunkedCons. chunk rest nil -1)
    rest))

(defn chunk-append [^ChunkBuffer b x]
  (.add b x))

(defn chunk [^ChunkBuffer b]
  (.chunk b))

(deftype #/(PVChunkedSeq E)
  [^PersistentVector vec ^List arr ^int i ^int off meta ^:mutable ^int __hash]
  ^:mixin EquivSequentialHashMixin
  ^:mixin #/(dart-coll/ListMixin E)
  ^:mixin #/(SeqListMixin E)
  (^#/(PVChunkedSeq R) #/(cast R) [coll]
   (PVChunkedSeq. vec arr i off meta __hash))
  IWithMeta
  (-with-meta [coll new-meta]
    (if (identical? new-meta meta)
      coll
      (PVChunkedSeq. vec arr i off new-meta -1)))
  IMeta
  (-meta [coll] meta)
  ISeqable
  (-seq [coll] coll)
  ISequential
  ISeq
  (-first [coll]
    (aget arr off))
  (-rest [coll]
    (if (< (inc off) (alength arr))
      (PVChunkedSeq. vec arr i (inc off) nil -1)
      (-chunked-rest coll)))
  (-next [coll]
    (if (< (inc off) (alength arr))
      (PVChunkedSeq. vec arr i (inc off) nil -1)
      (-chunked-next coll)))
  ICollection
  (-conj [coll o]
    (cons o coll))
  IEmptyableCollection
  (-empty [coll] ())
  IChunkedSeq
  (-chunked-first [coll]
    (ArrayChunk. arr off (alength arr)))
  (-chunked-rest [coll]
    (or ^some (-chunked-next coll) ()))
  (-chunked-next [coll]
    (let [end (+ i (alength arr))]
      (when (< end (-count vec))
        (PVChunkedSeq. vec (if (< end (bit-and-not end 31))
                             (unchecked-array-for (.-root vec) (.-shift vec) end)
                             (.-tail vec))
          end 0 nil -1))))
  IHash
  (-hash [coll] (ensure-hash __hash (hash-ordered-coll coll)))
  (-hash-realized? [coll] (.!= -1 __hash))
  IReduce
  (-reduce [coll f] (pv-reduce vec f (+ i off)))
  (-reduce [coll f start] (pv-reduce vec f (+ i off) start)))

;;; end chunks

;; transients

(defn- ^VectorNode tv-ensure-editable [edit ^VectorNode node]
  (if (identical? edit (.-edit node))
    node
    (let [arr (.-arr node)]
      (VectorNode. edit (aresize arr (.-length arr) 32 nil)))))

(defn- tv-editable-array-for
  "Returns the editable array where i is located."
  [^TransientVector tv ^int i]
  (loop [node (set! (.-root tv) (tv-ensure-editable (.-edit tv) (.-root tv)))
         ^int level (.-shift tv)]
    (if (< 0 level)
      (let [arr (.-arr node)
            j (bit-and (u32-bit-shift-right i level) 31)]
        (recur (aset arr j (tv-ensure-editable (.-edit tv) (aget arr j))) (- level 5)))
      (.-arr node))))

(defn- ^VectorNode tv-new-path [edit ^int level ^VectorNode node]
  (loop [^int ll level
         ^VectorNode ret node]
    (if (zero? ll)
      ret
      (let [arr (.filled #/(List dynamic) 32 nil)]
        (aset arr 0 ret)
        (recur (- ll 5) (VectorNode. edit arr))))))

(defn- ^VectorNode tv-push-tail [^TransientVector tv ^int level ^VectorNode parent tail-node]
  (let [edit (.-edit tv)
        ret (tv-ensure-editable edit parent)
        subidx (bit-and (u32-bit-shift-right (dec (.-cnt tv)) level) 31)
        level (- level 5)]
    (aset (.-arr ret) subidx
      (if (zero? level)
        tail-node
        (let [child (aget (.-arr ret) subidx)]
          (if-not (nil? child)
            (tv-push-tail tv level child tail-node)
            (tv-new-path edit level tail-node)))))
    ret))

(defn- tv-pop-tail! [^TransientVector tv ^int level ^VectorNode node]
  (let [n (- (.-cnt tv) 2)
        subidx (bit-and (u32-bit-shift-right n level) 31)]
    (cond
      (< 5 level)
      (or (tv-pop-tail! tv (- level 5) (aget (.-arr node) subidx))
        (when (< 0 subidx) (aset (.-arr node) nil) true))
      (< 0 subidx) (do (aset (.-arr node) subidx nil) true))))

(deftype TransientVector [^:mutable ^int cnt
                          ^:mutable ^int shift
                          ^:mutable ^some edit
                          ^:mutable ^VectorNode root
                          ^:mutable ^List tail]
  ITransientCollection
  (-conj! [tcoll o]
    (when-not edit
      (throw (ArgumentError. "conj! after persistent!")))
    (let [tail-len (bit-and cnt 31)]
      (if (or (pos? tail-len) (zero? cnt))
        (aset tail tail-len o)
        (let [tail-node (VectorNode. edit tail)
              new-tail (.filled #/(List dynamic) 32 nil)]
          (aset new-tail 0 o)
          (set! tail new-tail)
          (if (< (u32-bit-shift-left 1 shift) (u32-bit-shift-right cnt 5))
            (let [new-root-array (.filled #/(List dynamic) 32 nil)
                  new-shift (+ shift 5)]
              (aset new-root-array 0 root)
              (aset new-root-array 1 (tv-new-path edit shift tail-node))
              (set! root (VectorNode. edit new-root-array))
              (set! shift new-shift))
            (set! root (tv-push-tail tcoll shift root tail-node)))))
      (set! cnt (inc cnt))
      tcoll))
  (-persistent! [tcoll]
    (when-not edit
      (throw (ArgumentError. "persistent! called twice")))
    (set! edit nil)
    (let [cnt32 (bit-and cnt 31)]
      (cond
        (pos? cnt32)
        (PersistentVector. nil cnt shift root (ashrink tail cnt32) -1)
        (zero? cnt) []
        :else (PersistentVector. nil cnt shift root tail -1))))
  ITransientAssociative
  (-assoc! [tcoll key val]
    (when-not (dart/is? key int)
      (throw (ArgumentError. "TransientVector's key for assoc! must be a number.")))
    (-assoc-n! tcoll key val))
  ITransientVector
  (-assoc-n! [tcoll n val]
    (when-not edit
      (throw (ArgumentError. "assoc! after persistent!")))
    (when-not (and (<= 0 n) (<= n cnt))
      (throw (ArgumentError. (str "Index " n " out of bounds  [0," cnt "]"))))
    (cond
      (== n cnt) (-conj! tcoll val)
      (<= (bit-and-not (dec cnt) 31) n) (aset tail (bit-and n 31) val)
      :else
      (loop [arr (.-arr (set! root (tv-ensure-editable edit root)))
             level shift]
        (let [subidx (bit-and (u32-bit-shift-right n shift) 31)]
          (if (pos? level)
            (let [child (tv-ensure-editable edit (aget arr subidx))]
              (recur (.-arr (aset arr subidx child)) (- shift 5)))
            (aset arr (bit-and n 31) val)))))
    tcoll)
  (-pop! [tcoll]
    (when-not edit
      (throw (ArgumentError. "pop! after persistent!")))
    (when (zero? cnt)
      (throw (ArgumentError. "Can't pop empty vector")))
    (let [cnt-1 (dec cnt)
          subidx (bit-and cnt-1 31)]
      (if (or (pos? subidx) (zero? cnt-1))
        (aset tail subidx nil)
        ; pop tail
        (let [new-tail-length (- cnt-1 (bit-and-not cnt-1 31))]
          (set! tail (tv-editable-array-for tcoll cnt-1))
          (cond
            (== 5 shift)
            (aset (.-arr root) (u32-bit-shift-right (dec cnt-1) 5) nil)
            (== (- cnt-1 32) (u32-bit-shift-left 1 shift))
            (do (set! root (aget (.-arr root) 0))
                (set! shift (- shift 5)))
            :else
            (tv-pop-tail! tcoll shift root)))))
    (set! cnt (dec cnt))
    tcoll)
  ICounted
  (-count [coll]
    (when-not edit
      (throw (ArgumentError. "count after persistent!")))
    cnt)
  IIndexed
  (-nth [coll n]
    (when-not edit
      (throw (ArgumentError. "nth after persistent!")))
    (when (or (<= cnt n) (< n 0))
      (throw (ArgumentError. (str "No item " n " in vector of length " cnt))))
    (let [arr (if (<= (bit-and-not (dec cnt) 31) n) tail (unchecked-array-for root shift n))]
      (aget arr (bit-and n 31))))
  (-nth [coll n not-found]
    (when-not edit
      (throw (ArgumentError. "nth after persistent!")))
    (if (or (<= cnt n) (< n 0))
      not-found
      (let [arr (if (<= (bit-and-not (dec cnt) 31) n) tail (unchecked-array-for root shift n))]
        (aget arr (bit-and n 31)))))
  ILookup
  (-lookup [coll k] (-lookup coll k nil))
  (-lookup [coll k not-found]
    (when-not edit
      (throw (ArgumentError. "lookup after persistent!")))
    (if (dart/is? k int)
      (-nth coll k not-found)
      not-found))
  IFn
  (-invoke [coll k]
    (-lookup coll k))
  (-invoke [coll k not-found]
    (-lookup coll k not-found)))

;;;

;;; Mapentry

(deftype #/(PersistentMapEntry K V)
  [_k _v ^:mutable ^int __hash]
  ^:mixin EquivSequentialHashMixin
  ^:mixin ToStringMixin
  IPrint
  (-print [o sink] (print-sequential "[" "]" o sink))
  #/(MapEntry K V)
  (^K key [_] _k)
  (^V value [_] _v)
  IMapEntry
  (-key [node] _k)
  (-val [node] _v)
  IHash
  (-hash [coll] (ensure-hash __hash (hash-ordered-coll coll)))
  (-hash-realized? [coll] (.!= -1 __hash))
  IMeta
  (-meta [node] nil)
  IStack
  (-peek [node] _v)
  (-pop [node]
    (PersistentVector. meta 1 5 (.-root -EMPTY-VECTOR) #dart ^:fixed [_k] -1))
  ICollection
  (-conj [node o]
    (PersistentVector. meta 3 5 (.-root -EMPTY-VECTOR) #dart ^:fixed [_k _v o]  -1))
  IEmptyableCollection
  (-empty [node] nil)
  ISequential
  ISeqable
  (-seq [node] (-seq #dart ^:fixed [_k _v]))
  #_#_IReversible
  (-rseq [node] (IndexedSeq. 'js [val key] 0 nil))
  ICounted
  (-count [node] 2)
  IIndexed
  (-nth [node n]
    (cond (== n 0) _k
          (== n 1) _v
          :else    (throw (ArgumentError. "Index out of bounds"))))
  (-nth [node n not-found]
    (cond (== n 0) _k
          (== n 1) _v
          :else    not-found))
  ILookup
  (-lookup [node k] (-nth node k nil))
  (-lookup [node k not-found] (-nth node k not-found))
  IAssociative
  (-assoc [node k v]
    (->
      (PersistentVector. meta 2 5 (.-root -EMPTY-VECTOR) #dart ^:fixed [_k _v]  -1)
      (-assoc k v)))
  (-contains-key? [node k]
    (or (== k 0) (== k 1)))
  #_#_IFind
  (-find [node k]
    ;; TODO : replace with case
    (cond
      (== k 0) (MapEntry. 0 key -1)
      (== k 1) (MapEntry. 1 val -1)))
  IVector
  (-assoc-n [node n v]
    (->
      (PersistentVector. meta 2 5 (.-root -EMPTY-VECTOR)
        #dart ^:fixed [_k _v]  -1)
      (-assoc-n n v)))
  IReduce
  (-reduce [node f]
    (unreduced (f _k _v)))
  (-reduce [node f start]
    (let [r (f start _k)]
      (if (reduced? r)
        (deref r)
        (unreduced (f r _v)))))
  IFn
  (-invoke [node k]
    (-nth node k))
  (-invoke [node k not-found]
    (-nth node k not-found)))

; cgrand's
(deftype #/(BitmapIterator E)
  [^:mutable ^BitmapNode node
   ^:mutable ^int idx
   ^:mutable ^int mask
   ^:mutable ^int kvs
   ^:mutable ^int depth
   ^#/(List int) masks
   ^#/(List BitmapNode) nodes
   ^:dart mk-value]
  #/(Iterator E)
  (current [iter]
    (let [arr (.-arr node)]
      (mk-value (aget arr (- idx 2)) (aget arr (dec idx)))))
  (moveNext [iter]
    (cond
      (and (== depth 7) (< idx (* 2 (.-cnt node)))) ; collisions node
      (do (set! idx (+ 2 idx)) true)
      (not (zero? mask))
      (let [bit (bit-and mask (- mask))]
        (set! mask (bit-xor mask bit))
        (if (zero? (bit-and kvs bit))
          (let [^BitmapNode node' (aget (.-arr node) idx)
                hi (.-bitmap_hi node')
                lo (.-bitmap_lo node')]
            (aset nodes depth node)
            (aset masks depth mask)
            (set! node node')
            (set! idx 0)
            (set! mask (bit-or hi lo))
            (set! kvs (bit-and hi lo))
            (set! depth (inc depth))
            (recur))
          (do
            (set! idx (+ 2 idx))
            true)))
      (pos? depth)
      (let [^BitmapNode node' (aget nodes (set! depth (dec depth)))
            hi (.-bitmap_hi node')
            lo (.-bitmap_lo node')]
        (set! node node')
        (set! mask (aget masks depth))
        (set! idx (u32x2-bit-count (bit-and-not hi mask) (bit-and-not lo mask)))
        (set! kvs (bit-and hi lo))
        (recur))
      :else
      false)))

; Baptiste's
#_(deftype BitmapIterator [^:mutable ^int current-mask
                         ^:mutable ^BitmapNode current-bn
                         ^:mutable ^{:tag "List<int>"} mask-list
                         ^:mutable ^{:tag "List<BitmapNode>"} bn-list
                         ^:mutable ^int list-idx]
  Iterator
  (^:getter ^MapEntry current [iter]
   (let [bitmap-hi (.-bitmap_hi current-bn)
         bitmap-lo (.-bitmap_lo current-bn)
         kv-mask (bit-and current-mask bitmap-hi bitmap-lo)
         bit (bit-and kv-mask (- kv-mask))
         mask' (dec bit)
         idx (u32x2-bit-count (bit-and mask' bitmap-hi) (bit-and mask' bitmap-lo))
         arr (.-arr current-bn)]
     (set! current-mask (bit-xor current-mask bit))
     (MapEntry. (aget arr idx) (aget arr (inc idx)) -1)))
  (^bool moveNext [iter]
   (loop []
     (let [bitmap-hi (.-bitmap_hi current-bn)
           bitmap-lo (.-bitmap_lo current-bn)
           kv-mask (bit-and current-mask bitmap-hi bitmap-lo)]
       (cond
         (< 0 kv-mask) true
         (and (zero? current-mask) (zero? list-idx)) false
         (zero? current-mask)
         (do (set! list-idx (dec list-idx))
             (set! current-mask (aget mask-list list-idx))
             (set! current-bn (aget bn-list list-idx))
             (recur))
         :else
         (let [bn-mask (bit-and current-mask (bit-xor bitmap-hi bitmap-lo))
               bit (bit-and bn-mask (- bn-mask))
               mask (dec bit)
               idx (u32x2-bit-count (bit-and mask bitmap-hi) (bit-and mask bitmap-lo))
               ^BitmapNode next-bn (aget (.-arr current-bn) idx)]
           (aset mask-list list-idx (bit-xor current-mask bit))
           (aset bn-list list-idx current-bn)
           (set! list-idx (inc list-idx))
           (set! current-mask (bit-or (.-bitmap_hi next-bn) (.-bitmap_lo next-bn)))
           (set! current-bn next-bn)
           (recur)))))))

(deftype BitmapNode [^:mutable ^int cnt ^:mutable ^int bitmap-hi ^:mutable ^int bitmap-lo ^:mutable ^List arr]
  Object
  (inode_lookup [node k not-found]
    (let [h (hash k)]
      (loop [^BitmapNode node node
             ^int shift 0]
        (if (< shift 32)
          ; regular
          (let [bitmap-hi (.-bitmap_hi node)
                bitmap-lo (.-bitmap_lo node)
                n (bit-and (u32-bit-shift-right h shift) 31)
                bit (u32-bit-shift-left 1 n)
                mask (dec bit)
                idx (u32x2-bit-count (bit-and mask bitmap-hi) (bit-and mask bitmap-lo))
                hi (bit-and bitmap-hi bit)
                lo (bit-and bitmap-lo bit)]
            (cond
              (zero? (bit-or hi lo)) ; nothing
              not-found
              (zero? (bit-and hi lo))
              (recur (aget (.-arr node) idx) (+ 5 shift))
              :else ; kv
              (let [arr (.-arr node)
                    k' (aget arr idx)]
                (if (= k' k) (aget arr (inc idx)) not-found))))
          ; collisions node
          (let [n (* 2 cnt)
                arr (.-arr node)]
            (loop [^int i 0]
              (cond
                (== i n) not-found
                (= (aget arr i) k) (aget arr (inc i))
                :else (recur (+ 2 i)))))))))
  (inode_without [node shift h k]
    (if (< shift 32)
      (let [n (bit-and (u32-bit-shift-right h shift) 31)
            bit (u32-bit-shift-left 1 n)
            mask (dec bit)
            idx (u32x2-bit-count (bit-and mask bitmap-hi) (bit-and mask bitmap-lo))
            hi (bit-and bitmap-hi bit)
            lo (bit-and bitmap-lo bit)]
        (cond
          ; nothing
          (zero? (bit-or hi lo)) node
          ; a node
          (zero? (bit-and hi lo))
          (let [^BitmapNode child (aget arr idx)
                ^BitmapNode new-child (.inode_without child (+ shift 5) h k)]
            (cond
              (identical? child new-child) node
              ;; new-child is just made of a kv: inline it!
              (and (== 1 (.-cnt new-child)) (zero? (bit-xor (.-bitmap_hi new-child) (.-bitmap_lo new-child))))
              (let [k (aget (.-arr new-child) 0)
                    v (aget (.-arr new-child) 1)
                    size (inc (u32x2-bit-count bitmap-hi bitmap-lo))
                    new-arr (.filled #/(List dynamic) size v)]
                (dotimes [i idx] (aset new-arr i (aget arr i)))
                (aset new-arr idx k)
                (loop [j (inc idx) i (inc j)]
                  (when (< i size)
                    (aset new-arr i (aget arr j))
                    (recur (inc j) (inc i))))
                (BitmapNode. (dec cnt) (bit-or bitmap-hi bit) (bit-or bitmap-lo bit) new-arr))
              :else
              (BitmapNode. (dec cnt) bitmap-hi bitmap-lo (doto (aclone (.-arr node)) (aset idx new-child)))))
          ; a kv pair but not the right k
          (not (= k (aget arr idx))) node
          ; the right kv pair
          :else
          (let [size (- (u32x2-bit-count bitmap-hi bitmap-lo) 2)
                new-arr (.filled #/ (List dynamic) size nil)]
            (dotimes [i idx] (aset new-arr i (aget arr i)))
            (loop [i idx j (+ 2 idx)]
              (when (< i size)
                (aset new-arr i (aget arr j))
                (recur (inc i) (inc j))))
            (BitmapNode. (dec cnt) (bit-xor bitmap-hi bit) (bit-xor bitmap-lo bit) new-arr))))
      ; collisions node
      (let [n (* 2 cnt)]
        (loop [^int i 0]
          (cond
            (== i n) node
            (= (aget arr i) k)
            (let [n (- n 2)
                  new-arr (ashrink arr n)]
              (when-not (== i n)
                (aset new-arr i (aget arr n))
                (aset new-arr (inc i) (aget arr (inc n))))
              (BitmapNode. (dec cnt) 0 0 new-arr))
            :else (recur (+ 2 i)))))))
  (inode_assoc [node shift h k v]
    (if (< shift 32)
      ; regular node
      (let [n (bit-and (u32-bit-shift-right h shift) 31)
            bit (u32-bit-shift-left 1 n)
            mask (dec bit)
            idx (u32x2-bit-count (bit-and mask bitmap-hi) (bit-and mask bitmap-lo))
            hi (bit-and bitmap-hi bit)
            lo (bit-and bitmap-lo bit)]
        (cond
          (zero? (bit-or hi lo)) ; nothing
          (let [size (+ 2 (u32x2-bit-count bitmap-hi bitmap-lo))
                new-arr (.filled #/(List dynamic) size v)]
            (dotimes [i idx] (aset new-arr i (aget arr i)))
            (aset new-arr idx k)
            (loop [i (+ 2 idx) j idx]
              (when (< i size)
                (aset new-arr i (aget arr j))
                (recur (inc i) (inc j))))
            #_(BitmapNode. (inc cnt) (bit-xor (bit-and bitmap-hi bitmap-lo) bit) (bit-xor (bit-or bitmap-hi bitmap-lo) bitmap-lo bit) new-arr)
            (BitmapNode. (inc cnt) (bit-xor bitmap-hi bit) (bit-xor bitmap-lo bit) new-arr))
          (zero? (bit-and hi lo)) ; node
          (let [^BitmapNode child (aget arr idx)
                ^BitmapNode new-child (.inode_assoc child (+ shift 5) h k v)]
            (if (identical? child new-child)
              node
              (BitmapNode. (+ cnt (- (.-cnt new-child) (.-cnt child)))
                (bit-xor bitmap-hi bit) (bit-xor bitmap-lo bit)
                (doto (aclone (.-arr node)) (aset idx new-child)))
              #_(BitmapNode. (+ cnt (- (.-cnt new-child) (.-cnt child)))
                (bit-xor (bit-and bitmap-hi bitmap-lo) bit) (bit-xor (bit-or bitmap-hi bitmap-lo) bitmap-lo bit)
                (doto (aclone (.-arr node)) (aset idx new-child)))))
          :else ; kv
          (let [k' (aget arr idx)
                v' (aget arr (inc idx))]
            (cond
              ;; TODO not=
              (not (= k' k))
              (let [size (dec (u32x2-bit-count bitmap-hi bitmap-lo))
                    shift' (+ 5 ^int shift)
                    n' (bit-and (u32-bit-shift-right (hash k') shift') 31)
                    bit' (u32-bit-shift-left 1 n')
                    new-node (-> (BitmapNode. 1 bit' bit' #dart ^:fixed [k' v']) (.inode_assoc shift' h k v))
                    new-arr (.filled #/(List dynamic) size new-node)]
                (dotimes [i idx] (aset new-arr i (aget arr i)))
                (loop [i (inc idx) j (inc i)]
                  (when (< i size)
                    (aset new-arr i (aget arr j))
                    (recur (inc i) (inc j))))
                (BitmapNode. (inc cnt) bitmap-hi (bit-xor bitmap-lo bit) new-arr))
              (identical? v v') node
              :else
              (BitmapNode. cnt (bit-and bitmap-hi bitmap-lo) (bit-or bitmap-hi bitmap-lo)
                (doto (aclone arr) (aset (inc idx) v)))))))
      ; collisions node
      (let [n (* 2 cnt)]
        (loop [^int i 0]
          (cond
            (== i n)
            (BitmapNode. (inc cnt) 0 0 (doto (aresize arr n (+ 2 n) v) (aset n k)))
            (= (aget arr i) k)
            (let [i+1 (inc i)]
              (if (identical? (aget arr i+1) v)
                node
                (BitmapNode. cnt 0 0 (doto (aclone arr) (aset i+1 v)))))
            :else (recur (+ 2 i)))))))
  (inode_assoc_transient [node shift h k v]
    (if (< shift 32)
      ; regular node
      (let  [n (bit-and (u32-bit-shift-right h shift) 31)
             bit (u32-bit-shift-left 1 n)
             mask (dec bit)
             idx (u32x2-bit-count (bit-and mask bitmap-hi) (bit-and mask bitmap-lo))
             hi (bit-and bitmap-hi bit)
             lo (bit-and bitmap-lo bit)]
        (cond
          (zero? (bit-or hi lo)) ; nothing
          (let [net-size (u32x2-bit-count bitmap-hi bitmap-lo)
                net-size' (+ 2 net-size)
                idx' (inc idx)
                from-arr arr]
            (when (< (.-length arr) net-size')
              (set! arr (aresize arr net-size (inc (bit-or 7 (dec net-size'))) nil)))
            (loop [i (dec net-size') j (dec net-size)]
              (when (< idx' i)
                (aset arr i (aget from-arr j))
                (recur (dec i) (dec j))))
            (aset arr idx k)
            (aset arr idx' v)
            (set! cnt (inc cnt))
            (set! bitmap-hi (bit-or bitmap-hi bit))
            (set! bitmap-lo (bit-or bitmap-lo bit)))
          (zero? (bit-and hi lo)) ; node
          (let [^BitmapNode child (aget arr idx)]
            (if (zero? hi)
              ; if node is not owned
              (let [^BitmapNode child' (.inode_assoc child shift h k v)]
                (when-not (identical? child child')
                  (set! bitmap-hi (bit-xor hi bitmap-hi))
                  (set! bitmap-lo (bit-xor hi bitmap-lo))
                  (aset arr idx child')
                  (set! cnt (+ cnt (- (.-cnt child') (.-cnt child))))))
              ; if node is owned
              (let [old-cnt-child (.-cnt child)]
                (.inode_assoc_transient child (+ shift 5) h k v)
                (set! cnt (+ cnt (- (.-cnt child) old-cnt-child))))))
          :else ; kv
          (let [k' (aget arr idx)
                v' (aget arr (inc idx))]
            (cond
              ;; TODO not=
              (not (= k' k))
              (let [net-size (dec (u32x2-bit-count bitmap-hi bitmap-lo))
                    gross-size (inc (bit-or 7 (dec net-size)))
                    shift' (+ 5 ^int shift)
                    n' (bit-and (u32-bit-shift-right (hash k') shift') 31)
                    bit' (u32-bit-shift-left 1 n')
                    new-node (-> (BitmapNode. 1 bit' bit' (doto (.filled #/(List dynamic) 8 nil) (aset 0 k') (aset 1 v')))
                               (.inode_assoc_transient shift' h k v))
                    from-arr arr]
                (when (< gross-size (.-length arr))
                  (set! arr (aresize arr idx gross-size nil)))
                (aset arr idx new-node)
                (loop [i (inc idx) j (inc i)]
                  (when (< i net-size)
                    (aset arr i (aget from-arr j))
                    (recur (inc i) (inc j))))
                (when (< net-size gross-size) (aset arr net-size nil))
                (set! cnt (inc cnt))
                (set! bitmap-lo (bit-xor bitmap-lo lo)))
              (identical? v v') node
              :else
              (aset arr (inc idx) v))))
        node)
      ;; collisions node
      (let [n (* 2 cnt)]
        (loop [^int i 0]
          (cond
            (== i n)
            (do
              (set! cnt (inc cnt))
              (set! arr (doto (aresize arr n (+ 2 n) v) (aset n k))))
            (= (aget arr i) k)
            (let [i+1 (inc i)]
              (when-not (identical? (aget arr i+1) v)
                (set! arr (doto (aclone arr) (aset i+1 v)))))
            :else (recur (+ 2 i))))
        node)))
  (inode_without_transient [node shift h k]
    (if (< shift 32)
      (let [n (bit-and (u32-bit-shift-right h shift) 31)
            bit (u32-bit-shift-left 1 n)
            mask (dec bit)
            idx (u32x2-bit-count (bit-and mask bitmap-hi) (bit-and mask bitmap-lo))
            hi (bit-and bitmap-hi bit)
            lo (bit-and bitmap-lo bit)]
        (cond
          ; nothing
          (zero? (bit-or hi lo)) node
          ; a node
          (zero? (bit-and hi lo))
          (let [^BitmapNode child (aget arr idx)
                child-cnt (.-cnt child)
                ^BitmapNode? child'
                (if (zero? hi)
                  (let [child' (.inode_without child shift h k)]
                    (when-not (identical? child child') child'))
                  (.inode_without_transient child (+ shift 5) h k))]
            (when child'
              (if (and (== 1 (.-cnt child')) (zero? (bit-xor (.-bitmap_hi child') (.-bitmap_lo child'))))
                ; inline kv
                (let [k (aget (.-arr child') 0)
                      v (aget (.-arr child') 1)
                      net-size (inc (u32x2-bit-count bitmap-hi bitmap-lo))
                      gross-size (inc (bit-or 7 (dec net-size)))
                      from-arr arr]
                  (when (< (.-length arr) net-size)
                    (set! arr (aresize arr idx gross-size nil)))
                  (loop [j (inc idx) i (inc j)]
                    (when (< i net-size)
                      (aset arr i (aget from-arr j))
                      (recur (inc j) (inc i))))
                (aset arr idx k)
                (aset arr (inc idx) v)
                (set! cnt (dec cnt))
                (set! bitmap-hi (bit-or bitmap-hi bit))
                (set! bitmap-lo (bit-or bitmap-lo bit)))
                ; just update in place
                (aset arr idx child'))))
          ; the right kv pair
          (= k (aget arr idx))
          (let [net-size (- (u32x2-bit-count bitmap-hi bitmap-lo) 2)
                gross-size (inc (bit-or 7 (dec net-size)))
                from-arr arr]
            (when (< gross-size (.-length arr))
              (set! arr (aresize arr idx gross-size nil)))
            (loop [i idx j (+ 2 idx)]
              (when (< i net-size)
                (aset arr i (aget from-arr j))
                (recur (inc i) (inc j))))
            (when (identical? arr from-arr)
              (aset arr net-size nil)
              (aset arr (inc net-size) nil))
            (set! cnt (dec cnt))
            (set! bitmap-hi (bit-xor bitmap-hi bit))
            (set! bitmap-lo (bit-xor bitmap-lo bit))))
        node)
      ; collisions node
      (let [n (* 2 cnt)]
        (loop [^int i 0]
          (cond
            (== i n) nil
            (= (aget arr i) k)
            (let [n (- n 2)
                  new-arr (ashrink arr n)]
              (when-not (== i n)
                (aset new-arr i (aget arr n))
                (aset new-arr (inc i) (aget arr (inc n))))
              (set! arr new-arr)
              (set! cnt (dec cnt)))
            :else (recur (+ 2 i))))
        node))))

(comment
  (defn p-assoc [{:keys [nodes kvs] :as input}]
    (let [i (rand-int 32)]
      (cond
        (< i kvs) {:nodes (inc nodes) :kvs (dec kvs)}
        (< i (+ kvs nodes)) input
        :else {:nodes nodes :kvs (inc kvs)})))

  (defn p-size [{:keys [nodes kvs]}] (+ nodes kvs kvs))

  (defn max-sizes
    "Returns the distribution of the max size to which arr grows when inserting
     n random items. Default sample size: 10000."
    ([n] (max-sizes n 10000))
    ([n samples]
     (into (sorted-map)
       (frequencies
         (repeatedly samples
           #(transduce (comp (take n) (map p-size)) max 0 (iterate p-assoc {:nodes 0 :kvs 0})))))))

  (defn end-sizes
    "Returns the distribution of the end size to which arr grows when inserting
     n random items. Default sample size: 10000."
    ([n] (end-sizes n 10000))
    ([n samples]
     (into (sorted-map)
       (frequencies
         (repeatedly samples
           #(transduce (comp (take n) (map p-size)) (fn ([x] x) ([_ x] x)) 0 (iterate p-assoc {:nodes 0 :kvs 0})))))))

  )

(deftype TransientHashMap [^:mutable ^bool editable ^:mutable ^BitmapNode root]
  ITransientCollection
  (-conj! [tcoll o]
    (when-not editable
      (throw (ArgumentError. "conj! after persistent!")))
    (cond
      (map-entry? o)
      (-assoc! tcoll (-key o) (-val o))
      (vector? o)
      (-assoc! tcoll (-nth o 0) (-nth o 1))
      :else
      (reduce -conj! tcoll o)))
  (-persistent! [tcoll]
    (when-not editable
      (throw (ArgumentError. "persistent! called twice")))
    (set! editable false)
    (PersistentHashMap. nil root -1))
  ITransientAssociative
  (-assoc! [tcoll k v]
    (when-not editable
      (throw (ArgumentError. "assoc! after persistent!")))
    (set! root (.inode_assoc_transient root 0 (hash k) k v))
    tcoll)
  ITransientMap
  (-dissoc! [tcoll k]
    (when-not editable
      (throw (ArgumentError. "dissoc! after persistent!")))
    (set! root (.inode_without_transient root 0 (hash k) k))
    tcoll)
  ICounted
  (-count [coll]
    (when-not editable
      (throw (ArgumentError. "count after persistent!")))
    (.-cnt root))
  ILookup
  (-lookup [tcoll k]
    (when-not editable
      (throw (ArgumentError. "lookup after persistent!")))
    (-lookup tcoll k nil))
  (-lookup [tcoll k not-found]
    (when-not editable
      (throw (ArgumentError. "lookup after persistent!")))
    (.inode_lookup root k not-found))
  IFn
  (-invoke [tcoll k]
    (-lookup tcoll k))
  (-invoke [tcoll k not-found]
    (-lookup tcoll k not-found)))

;; TODO *configs*
(defn- print-map [m ^StringSink sink]
  #_(binding [*print-level* (and (not *print-dup*) *print-level* (dec *print-level*))]
      (if (and *print-level* (neg? *print-level*))
        (.write w "#")
        (do
          (.write w begin)
          (when-let [xs (seq sequence)]
            (if (and (not *print-dup*) *print-length*)
              (loop [[x & xs] xs
                     print-length *print-length*]
                (if (zero? print-length)
                  (.write w "...")
                  (do
                    (print-one x w)
                    (when xs
                      (.write w sep)
                      (recur xs (dec print-length))))))
              (loop [[x & xs] xs]
                (print-one x w)
                (when xs
                  (.write w sep)
                  (recur xs)))))
          (.write w end))))
  (.write sink "{")
  (if (satisfies? IKVReduce m)
    (reduce-kv (fn [need-sep k v]
                 (when need-sep
                   (.write sink ", "))
                 (-print k sink)
                 (.write sink " ")
                 (-print v sink)
                 true)
      false m)
    (reduce (fn [need-sep [k v]]
              (when need-sep
                (.write sink ", "))
              (-print k sink)
              (.write sink " ")
              (-print v sink)
              true)
      false m))
  (.write sink "}"))

(deftype #/(PersistentHashMap K V)
  [meta ^BitmapNode root ^:mutable ^int __hash]
  ^:mixin #/(dart-coll/MapMixin K V)
  (entries [coll]
    (reify ^:mixin #/(dart-coll/IterableMixin (MapEntry K V))
     (iterator [_]
      (BitmapIterator. root 0 0 0 1
        (List/filled 7 (bit-or (.-bitmap_hi root) (.-bitmap_lo root)))
        (List/filled 7 root)
        #(PersistentMapEntry. %1 %2 -1)))))
  ("[]" [coll k]
   (-lookup coll k nil))
  ("[]=" [coll key val]
   (throw (UnsupportedError. "[]= not supported on PersistentHashMap")))
  (remove [coll val]
    (throw (UnsupportedError. "remove not supported on PersistentHashMap")))
  (clear [coll]
   (throw (UnsupportedError. "clear not supported on PersistentHashMap")))
  (keys [coll]
   (reify ^:mixin #/(dart-coll/IterableMixin K)
     (iterator [_]
      (BitmapIterator. root 0 0 0 1
        (List/filled 7 (bit-or (.-bitmap_hi root) (.-bitmap_lo root)))
        (List/filled 7 root)
        (fn [k _] k)))))
  (values [coll]
   (reify ^:mixin #/(dart-coll/IterableMixin V)
     (iterator [_]
      (BitmapIterator. root 0 0 0 1
        (List/filled 7 (bit-or (.-bitmap_hi root) (.-bitmap_lo root)))
        (List/filled 7 root)
        (fn [_ v] v)))))
  (^#/(PersistentHashMap RK RV) #/(cast RK RV) [coll]
   (PersistentHashMap. meta root __hash))
  ^:mixin ToStringMixin
  IPrint
  ;; TODO : handle prefix-map & co
  (-print [o sink]
    (print-map o sink))
  IAssociative
  (-assoc [coll k v]
    (let [^BitmapNode new-root (.inode_assoc root 0 (hash k) k v)]
      (if (identical? new-root root)
        coll
        (PersistentHashMap. meta new-root -1))))
  (-contains-key? [coll k]
    (not (identical? (-lookup coll k coll) coll)))
  IWithMeta
  (-with-meta [coll new-meta]
    (if (identical? new-meta meta)
      coll
      (PersistentHashMap. new-meta root __hash)))
  IMeta
  (-meta [coll] meta)
  ICollection
  (-conj [coll entry]
    (if (and (satisfies? IVector entry) (== (-count entry) 2))
      (-assoc coll (-nth entry 0) (-nth entry 1))
      (loop [ret coll s (seq entry)]
        (if (nil? s)
          ret
          (let [e (first s)]
            (if (satisfies? IVector e)
              (recur (-assoc ret (-nth e 0) (-nth e 1)) (-next s))
              (throw (ArgumentError. "conj on a map takes map entries or seqables of map entries"))))))))
  IEmptyableCollection
  (-empty [coll] (-with-meta {} meta))
  IEquiv
  (-equiv [coll other] (-equiv-map coll other))
  IHash
  (-hash [coll] (ensure-hash __hash (hash-unordered-coll coll)))
  ISeqable
  (-seq [coll] (iterator-seq (.-iterator (.-entries coll))))
  ICounted
  (-count [coll] (.-cnt root))
  ILookup
  (-lookup [coll k]
    (-lookup coll k nil))
  (-lookup [coll k not-found]
    (.inode_lookup root k not-found))
  IFind
  (-find [coll k]
    (when-some [v (-lookup coll k nil)]
      (PersistentMapEntry. k v -1)))
  IMap
  (-dissoc [coll k]
    (let [new-root (.inode_without root 0 (hash k) k)]
      (if (identical? new-root root)
        coll
        (PersistentHashMap. meta new-root -1))))
  IKVReduce
  (-kv-reduce [coll f init]
    (if (zero? (.-cnt ^BitmapNode (.-root coll)))
      init
      (loop [s (-seq coll)
             acc init]
        (if (nil? s)
          acc
          (let [^PersistentMapEntry me (-first s)
                acc (f acc (-key me) (-val me))]
            (if (reduced? acc)
              (-deref acc)
              (recur (-next s) acc)))))))
  IFn
  (-invoke [coll k]
    (-lookup coll k))
  (-invoke [coll k not-found]
    (-lookup coll k not-found))
  IEditableCollection
  (-as-transient [coll]
    (let [bitmap-hi (.-bitmap_hi root)
          bitmap-lo (.-bitmap_lo root)
          net-size (u32x2-bit-count bitmap-hi bitmap-lo)
          gross-size (inc (bit-or 7 (dec net-size)))]
      (TransientHashMap. true (BitmapNode. (.-cnt root) (bit-and bitmap-hi bitmap-lo) (bit-or bitmap-hi bitmap-lo) (aresize (.-arr root) net-size gross-size nil))))))

(def -EMPTY-MAP
  (PersistentHashMap. nil (BitmapNode. 0 0 0 (.empty List)) -1))

(deftype #/(PersistentHashSet E)
  [meta ^PersistentHashMap hm ^:mutable ^int __hash]
  ^:mixin #/(dart-coll/SetMixin E)
  (contains [this e]
    (if-some [_ (-lookup hm e nil)]
      true false))
  (lookup [this e]
    (-lookup hm e nil))
  (add [this e]
    (throw (UnsupportedError. "add not supported on PersistentHashSet")))
  (remove [this e]
    (throw (UnsupportedError. "remove not supported on PersistentHashSet")))
  (clear [this]
    (throw (UnsupportedError. "clear not supported on PersistentHashSet")))
  (length [this] (-count hm))
  (iterator [this] (.-iterator ^#/(Iterable E) (.-keys hm)))
  (toSet [this] (dart:core/Set.of ^#/(Iterable E) (.-keys hm)))
  ;; TODO: not sure of this one
  (^#/(PersistentHashSet R) #/(cast R) [coll]
   (PersistentHashSet. meta hm __hash))
  ^:mixin ToStringMixin
  IPrint
  (-print [o sink]
    (print-sequential "#{" "}" o sink))
  IWithMeta
  (-with-meta [coll new-meta]
    (if (identical? new-meta meta)
      coll
      (PersistentHashSet. new-meta hm __hash)))
  IMeta
  (-meta [coll] meta)
  ICollection
  (-conj [coll o]
    (PersistentHashSet. meta (assoc hm o true) -1))
  IEmptyableCollection
  (-empty [coll] (-with-meta #{} meta))
  IEquiv
  (-equiv [coll other]
    (and
      (set? other)
      (== (-count hm) (-count (.-hm other)))
      (let [y (.-hm other)
            never-equiv (Object.)]
        (reduce-kv
          (fn [_ k v]
            (if (= (get y k never-equiv) v)
              true
              (reduced false)))
          true hm))))
  IHash
  (-hash [coll] (ensure-hash __hash (hash-unordered-coll coll)))
  ISeqable
  (-seq [coll] (iterator-seq (.-iterator (.-keys hm))))
  ICounted
  (-count [coll] (-count hm))
  ILookup
  (-lookup [coll v]
    (-lookup hm v nil))
  (-lookup [coll v not-found]
    (if-some [_ (-lookup hm v nil)]
      v
      not-found))
  ISet
  (-disjoin [coll v]
    (PersistentHashSet. meta (-dissoc hm v) -1))
  IFn
  (-invoke [coll k]
    (-lookup coll k))
  (-invoke [coll k not-found]
    (-lookup coll k not-found))
  IEditableCollection
  (-as-transient [coll] (TransientHashSet. (-as-transient hm))))

(deftype TransientHashSet [^:mutable ^TransientHashMap transient-map]
  ITransientCollection
  (-conj! [tcoll o]
    (when-not (.-editable transient-map)
      (throw (ArgumentError. "conj! after persistent!")))
    (set! transient-map (assoc! transient-map o true))
    tcoll)
  (-persistent! [tcoll]
    (when-not (.-editable transient-map)
      (throw (ArgumentError. "persistent! called twice")))
    (PersistentHashSet. nil (persistent! transient-map) -1))
  ITransientSet
  (-disjoin! [tcoll v]
    (when-not (.-editable transient-map)
      (throw (ArgumentError. "disj! called twice")))
    (set! transient-map (dissoc! transient-map v))
    tcoll)
  ICounted
  (-count [tcoll]
    (when-not (.-editable transient-map)
      (throw (ArgumentError. "count after persistent!")))
    (count transient-map))
  ILookup
  (-lookup [tcoll v]
    (when-not (.-editable transient-map)
      (throw (ArgumentError. "lookup after persistent!")))
    (-lookup tcoll v nil))
  (-lookup [tcoll v not-found]
    (when-not (.-editable transient-map)
      (throw (ArgumentError. "lookup after persistent!")))
    (if-some [_ (-lookup transient-map v nil)]
      v
      not-found))
  IFn
  (-invoke [tcoll k]
    (-lookup tcoll k nil))
  (-invoke [tcoll k not-found]
    (-lookup tcoll k not-found)))

(def -EMPTY-SET
  (PersistentHashSet. nil {} -1))

(defn ^List to-array
  [coll]
  ;; TODO : use more concrete implem of to-array for DS ?
  (let [ary #dart []]
    (loop [s (seq coll)]
      (if-not (nil? s)
        (do (.add ary (first s))
            (recur (next s)))
        ary))))

(defn apply
  ([f args]
   (if (satisfies? IFn f)
     (-apply f (seq args))
     (.apply Function f (to-array args))))
  ([f x args]
   (let [args (list* x args)]
     (if (satisfies? IFn f)
       (-apply f args)
       (.apply Function f (to-array args)))))
  ([f x y args]
   (let [args (list* x y args)]
     (if (satisfies? IFn f)
       (-apply f args)
       (.apply Function f (to-array args)))))
  ([f x y z args]
   (let [args (list* x y z args)]
     (if (satisfies? IFn f)
       (-apply f args)
       (.apply Function f (to-array args)))))
  ([f a b c d & args]
   (let [args (cons a (cons b (cons c (cons d (spread args)))))]
     (if (satisfies? IFn f)
       (-apply f args)
       (.apply Function f (to-array args))))))

(defn comp
  "Takes a set of functions and returns a fn that is the composition
  of those fns.  The returned fn takes a variable number of args,
  applies the rightmost of fns to the args, the next
  fn (right-to-left) to the result, etc."
  ([] identity)
  ([f] f)
  ([f g]
     (fn
       ([] (f (g)))
       ([x] (f (g x)))
       ([x y] (f (g x y)))
       ([x y z] (f (g x y z)))
       ([x y z & args] (f (apply g x y z args)))))
  ([f g & fs]
     (reduce comp f (cons g fs))))

(defmacro dotimes
  "bindings => name n

  Repeatedly executes body (presumably for side-effects) with name
  bound to integers from 0 through n-1."
  [bindings & body]
  #_(assert-args
     (vector? bindings) "a vector for its binding"
     (= 2 (count bindings)) "exactly 2 forms in binding vector")
  (let [i (first bindings)
        n (second bindings)]
    ;; TODO : re-think about `long`
    `(let [^int n# ~n]
       (loop [~i 0]
         (when (< ~i n#)
           ~@body
           (recur (inc ~i)))))))

(defn identity
  "Returns its argument."
  [x] x)

(defn ^bool every?
  "Returns true if (pred x) is logical true for every x in coll, else
  false."
  [pred coll]
  (cond
    (nil? (seq coll)) true
    (pred (first coll)) (recur pred (next coll))
    true false))

(defn ^bool empty?
  "Returns true if coll has no items - same as (not (seq coll)).
  Please use the idiom (seq x) rather than (not (empty? x))"
  {:inline (fn [coll] `^bool (not (seq ~coll)))
   :inline-arities #{1}}
  [coll]
  (not (seq coll)))

(defn complement
  "Takes a fn f and returns a fn that takes the same arguments as f,
  has the same effects, if any, and returns the opposite truth value."
  [f]
  (fn
    ([] (not (f)))
    ([x] (not (f x)))
    ([x y] (not (f x y)))
    ([x y & zs] (not (apply f x y zs)))))

(defn some
  "Returns the first logical true value of (pred x) for any x in coll,
  else nil.  One common idiom is to use a set as pred, for example
  this will return :fred if :fred is in the sequence, otherwise nil:
  (some #{:fred} coll)"
  [pred coll]
  (when-let [s (seq coll)]
    (or (pred (first s)) (recur pred (next s)))))

(defn second
  "Same as (first (next x))"
  [coll]
  (first (next coll)))

(defn ffirst
  "Same as (first (first x))"
  [coll]
  (first (first coll)))

(defn nfirst
  "Same as (next (first x))"
  [coll]
  (next (first coll)))

(defn fnext
  "Same as (first (next x))"
  [coll]
  (first (next coll)))

(defn nnext
  "Same as (next (next x))"
  [coll]
  (next (next coll)))

(defn last
  "Return the last item in coll, in linear time"
  [s]
  (let [sn (next s)]
    (if-not (nil? sn)
      (recur sn)
      (first s))))

(defn butlast
  "Return a seq of all but the last item in coll, in linear time"
  [s]
  (loop [ret [] s s]
    (if (next s)
      (recur (conj ret (first s)) (next s))
      (seq ret))))

(defn update
  "'Updates' a value in an associative structure, where k is a
  key and f is a function that will take the old value
  and any supplied args and return the new value, and returns a new
  structure.  If the key does not exist, nil is passed as the old value."
  ([m k f]
   (assoc m k (f (get m k))))
  ([m k f x]
   (assoc m k (f (get m k) x)))
  ([m k f x y]
   (assoc m k (f (get m k) x y)))
  ([m k f x y z]
   (assoc m k (f (get m k) x y z)))
  ([m k f x y z & more]
   (assoc m k (apply f (get m k) x y z more))))

(defn update-in
  "'Updates' a value in a nested associative structure, where ks is a
  sequence of keys and f is a function that will take the old value
  and any supplied args and return the new value, and returns a new
  nested structure.  If any levels do not exist, hash-maps will be
  created."
  [m ks f & args]
  (let [up (fn up [m ks f args]
             (let [[k & ks] ks]
               (if ks
                 (assoc m k (up (get m k) ks f args))
                 (assoc m k (apply f (get m k) args)))))]
    (up m ks f args)))

(defn drop
  "Returns a lazy sequence of all but the first n items in coll.
  Returns a stateful transducer when no collection is provided."
  ([n]
   (fn [rf]
     (let [nv (volatile! n)]
       (fn
         ([] (rf))
         ([result] (rf result))
         ([result input]
          (let [n @nv]
            (vswap! nv dec)
            (if (pos? n)
              result
              (rf result input))))))))
  ([n coll]
   (let [step (fn [n coll]
                (let [s (seq coll)]
                  (if (and (pos? n) s)
                    (recur (dec n) (rest s))
                    s)))]
     (lazy-seq (step n coll)))))

(defn drop-while
  "Returns a lazy sequence of the items in coll starting from the
  first item for which (pred item) returns logical false.  Returns a
  stateful transducer when no collection is provided."
  ([pred]
   (fn [rf]
     (let [dv (volatile! true)]
       (fn
         ([] (rf))
         ([result] (rf result))
         ([result input]
          (let [drop? @dv]
            (if (and drop? (pred input))
              result
              (do
                (vreset! dv nil)
                (rf result input)))))))))
  ([pred coll]
   (let [step (fn [pred coll]
                (let [s (seq coll)]
                  (if (and s (pred (first s)))
                    (recur pred (rest s))
                    s)))]
     (lazy-seq (step pred coll)))))

(defn drop-last
  "Return a lazy sequence of all but the last n (default 1) items in coll"
  ([coll] (drop-last 1 coll))
  ([n coll] (map (fn [x _] x) coll (drop n coll))))

(defn take
  "Returns a lazy sequence of the first n items in coll, or all items if
  there are fewer than n.  Returns a stateful transducer when
  no collection is provided."
  ([n]
   (fn [rf]
     (let [nv (volatile! n)]
       (fn
         ([] (rf))
         ([result] (rf result))
         ([result input]
          (let [n @nv
                nn (vswap! nv dec)
                result (if (pos? n)
                         (rf result input)
                         result)]
            (if (not (pos? nn))
              (ensure-reduced result)
              result)))))))
  ([n coll]
   (lazy-seq
    (when (pos? n)
      (when-let [s (seq coll)]
        (cons (first s) (take (dec n) (rest s))))))))

(defn take-while
  "Returns a lazy sequence of successive items from coll while
  (pred item) returns logical true. pred must be free of side-effects.
  Returns a transducer when no collection is provided."
  ([pred]
   (fn [rf]
     (fn
       ([] (rf))
       ([result] (rf result))
       ([result input]
        (if (pred input)
          (rf result input)
          (reduced result))))))
  ([pred coll]
   (lazy-seq
    (when-let [s (seq coll)]
      (when (pred (first s))
        (cons (first s) (take-while pred (rest s))))))))

(defn take-nth
  "Returns a lazy seq of every nth item in coll.  Returns a stateful
  transducer when no collection is provided."
  ([n]
   (fn [rf]
     (let [iv (volatile! -1)]
       (fn
         ([] (rf))
         ([result] (rf result))
         ([result input]
          (let [i (vswap! iv inc)]
            (if (zero? (rem i n))
              (rf result input)
              result)))))))
  ([n coll]
   (lazy-seq
     (when-let [s (seq coll)]
       (cons (first s) (take-nth n (drop n s)))))))

(defn take-last
  "Returns a seq of the last n items in coll.  Depending on the type
  of coll may be no better than linear time.  For vectors, see also subvec."
  [n coll]
  (loop [s (seq coll) lead (seq (drop n coll))]
    (if lead
      (recur (next s) (next lead))
      s)))

;; TODO : take time to implement the Repeat. type like in clj/cljs
(defn repeat
  "Returns a lazy (infinite!, or length n if supplied) sequence of xs."
  ([x] (lazy-seq (cons x (repeat x))))
  ([n x] (take n (repeat x))))

(defn repeatedly
  "Takes a function of no args, presumably with side effects, and
  returns an infinite (or length n if supplied) lazy sequence of calls
  to it"
  ([f] (lazy-seq (cons (f) (repeatedly f))))
  ([n f] (take n (repeatedly f))))

;; TODO : take time to implement the Iterate. type like in clj/cljs
(defn iterate
  "Returns a lazy sequence of x, (f x), (f (f x)) etc. f must be free of side-effects"
  {:added "1.0"}
  [f x] (cons x (lazy-seq (iterate f (f x)))))

;; TODO : same as iterate, maybe follow clj/cljs implementation of Range
(defn range
  "Returns a lazy seq of nums from start (inclusive) to end
  (exclusive), by step, where start defaults to 0, step to 1, and end to
  infinity. When step is equal to 0, returns an infinite sequence of
  start. When start is equal to end, returns empty list."
  ([] (range 0 (.-maxFinite dart:core/double) 1))
  ([end] (range 0 end 1))
  ([start end] (range start end 1))
  ([start end step]
   (lazy-seq
     (let [b (chunk-buffer 32)
           comp (cond (or (zero? step) (== start end)) not=
                      (pos? step) <
                      (neg? step) >)]
       (loop [i start]
         (if (and (< (count b) 32)
               (comp i end))
           (do
             (chunk-append b i)
             (recur (+ i step)))
           (chunk-cons (chunk b)
             (when (comp i end)
               (range i end step)))))))))

(defn nthrest
  "Returns the nth rest of coll, coll when n is 0."
  [coll n]
  (loop [^int n n xs coll]
    (if-let [xs (and (pos? n) (seq xs))]
      (recur (dec n) (rest xs))
      xs)))

(defn concat
  "Returns a lazy seq representing the concatenation of the elements in the supplied colls."
  ([] (lazy-seq nil))
  ([x] (lazy-seq x))
  ([x y]
   (lazy-seq
    (let [s (seq x)]
      (if s
        (if (chunked-seq? s)
          (chunk-cons (chunk-first s) (concat (chunk-rest s) y))
          (cons (first s) (concat (rest s) y)))
        y))))
  ([x y & zs]
   (let [cat (fn cat [xys zs]
               (lazy-seq
                (let [xys (seq xys)]
                  (if xys
                    (if (chunked-seq? xys)
                      (chunk-cons (chunk-first xys)
                                  (cat (chunk-rest xys) zs))
                      (cons (first xys) (cat (rest xys) zs)))
                    (when zs
                      (cat (first zs) (next zs)))))))]
     (cat (concat x y) zs))))

(defn map
  "Returns a lazy sequence consisting of the result of applying f to
  the set of first items of each coll, followed by applying f to the
  set of second items in each coll, until any one of the colls is
  exhausted.  Any remaining items in other colls are ignored. Function
  f should accept number-of-colls arguments. Returns a transducer when
  no collection is provided."
  ([f]
   (fn [rf]
     (fn
       ([] (rf))
       ([result] (rf result))
       ([result input]
        (rf result (f input)))
       ([result input & inputs]
        (rf result (apply f input inputs))))))
  ([f coll]
   (lazy-seq
     (when-let [s (seq coll)]
       (if (chunked-seq? s)
         (let [c (chunk-first s)]
           (chunk-cons
             (chunk (chunk-reduce #(doto %1 (chunk-append (f %2)))
                      (chunk-buffer (count c)) c))
             (map f (chunk-rest s))))
         (cons (f (first s)) (map f (rest s)))))))
  ([f c1 c2]
   (lazy-seq
    (let [s1 (seq c1) s2 (seq c2)]
      (when (and s1 s2)
        (cons (f (first s1) (first s2))
              (map f (rest s1) (rest s2)))))))
  ([f c1 c2 c3]
   (lazy-seq
    (let [s1 (seq c1) s2 (seq c2) s3 (seq c3)]
      (when (and  s1 s2 s3)
        (cons (f (first s1) (first s2) (first s3))
              (map f (rest s1) (rest s2) (rest s3)))))))
  ([f c1 c2 c3 & colls]
   (let [step (fn step [cs]
                (lazy-seq
                 (let [ss (map seq cs)]
                   (when (every? identity ss)
                     (cons (map first ss) (step (map rest ss)))))))]
     (map #(apply f %) (step (list* c1 c2 c3 colls))))))

(defmacro doto
  "Evaluates x then calls all of the methods and functions with the
  value of x supplied at the front of the given arguments.  The forms
  are evaluated in order.  Returns x."
  [x & forms]
  (let [gx (gensym)]
    `(let [~gx ~x]
       ~@(map (fn [f]
                (with-meta
                  (if (seq? f)
                    `(~(first f) ~gx ~@(next f))
                    `(~f ~gx))
                  (meta f)))
           forms)
       ~gx)))

(defmacro ->
  "Threads the expr through the forms. Inserts x as the
  second item in the first form, making a list of it if it is not a
  list already. If there are more forms, inserts the first form as the
  second item in second form, etc."
  [x & forms]
  (loop [x x, forms forms]
    (if forms
      (let [form (first forms)
            threaded (if (seq? form)
                       (with-meta `(~(first form) ~x ~@(next form)) (meta form))
                       (list form x))]
        (recur threaded (next forms)))
      x)))

(defmacro ->>
  "Threads the expr through the forms. Inserts x as the
  last item in the first form, making a list of it if it is not a
  list already. If there are more forms, inserts the first form as the
  last item in second form, etc."
  [x & forms]
  (loop [x x, forms forms]
    (if forms
      (let [form (first forms)
            threaded (if (seq? form)
                       (with-meta `(~(first form) ~@(next form)  ~x) (meta form))
                       (list form x))]
        (recur threaded (next forms)))
      x)))

(defmacro as->
  "Binds name to expr, evaluates the first form in the lexical context
  of that binding, then binds name to that result, repeating for each
  successive form, returning the result of the last form."
  [expr name & forms]
  `(let [~name ~expr
         ~@(interleave (repeat name) (butlast forms))]
     ~(if (empty? forms)
        name
        (last forms))))

(defmacro some->
  "When expr is not nil, threads it into the first form (via ->),
  and when that result is not nil, through the next etc"
  [expr & forms]
  (let [g (gensym)
        steps (map (fn [step] `(if (nil? ~g) nil (-> ~g ~step)))
                forms)]
    `(let [~g ~expr
           ~@(interleave (repeat g) (butlast steps))]
       ~(if (empty? steps)
          g
          (last steps)))))

(defmacro some->>
  "When expr is not nil, threads it into the first form (via ->>),
  and when that result is not nil, through the next etc"
  [expr & forms]
  (let [g (gensym)
        steps (map (fn [step] `(if (nil? ~g) nil (->> ~g ~step)))
                forms)]
    `(let [~g ~expr
           ~@(interleave (repeat g) (butlast steps))]
       ~(if (empty? steps)
          g
          (last steps)))))

(defn keep
  "Returns a lazy sequence of the non-nil results of (f item). Note,
  this means false return values will be included.  f must be free of
  side-effects.  Returns a transducer when no collection is provided."
  {:added "1.2"
   :static true}
  ([f]
   (fn [rf]
     (fn
       ([] (rf))
       ([result] (rf result))
       ([result input]
        (let [v (f input)]
          (if (nil? v)
            result
            (rf result v)))))))
  ([f coll]
   (lazy-seq
    (when-let [s (seq coll)]
      (if (chunked-seq? s)
        (let [c (chunk-first s)]
          (chunk-cons
            (chunk (chunk-reduce #(if-some [x (f %2)]
                                    (doto %1 (chunk-append x))
                                    %1)
                     (chunk-buffer (count c)) c))
            (keep f (chunk-rest s))))
        (let [x (f (first s))]
          (if (nil? x)
            (keep f (rest s))
            (cons x (keep f (rest s))))))))))

(defn ^:private preserving-reduced
  [rf]
  #(let [ret (rf %1 %2)]
     (if (reduced? ret)
       (reduced ret)
       ret)))

(defn cat
  "A transducer which concatenates the contents of each input, which must be a
  collection, into the reduction."
  [rf]
  (let [rrf (preserving-reduced rf)]
    (fn
      ([] (rf))
      ([result] (rf result))
      ([result input]
         (reduce rrf result input)))))

(defn mapcat
  "Returns the result of applying concat to the result of applying map
  to f and colls.  Thus function f should return a collection. Returns
  a transducer when no collections are provided"
  ([f] (comp (map f) cat))
  ([f & colls]
   (apply concat (apply map f colls))))

(defmacro lazy-cat
  "Expands to code which yields a lazy sequence of the concatenation
  of the supplied colls.  Each coll expr is not evaluated until it is
  needed.

  (lazy-cat xs ys zs) === (concat (lazy-seq xs) (lazy-seq ys) (lazy-seq zs))"
  {:added "1.0"}
  [& colls]
  `(concat ~@(map #(list `lazy-seq %) colls)))

(defn interleave
  "Returns a lazy seq of the first item in each coll, then the second etc."
  ([] ())
  ([c1] (lazy-seq c1))
  ([c1 c2]
   (lazy-seq
    (let [s1 (seq c1) s2 (seq c2)]
      (when (and s1 s2)
        (cons (first s1) (cons (first s2)
                               (interleave (rest s1) (rest s2))))))))
  ([c1 c2 & colls]
   (lazy-seq
    (let [ss (map seq (list* c1 c2 colls))]
      (when (every? identity ss)
        (concat (map first ss) (apply interleave (map rest ss))))))))

(defn filter
  "Returns a lazy sequence of the items in coll for which
  (pred item) returns logical true. pred must be free of side-effects.
  Returns a transducer when no collection is provided."
  ([pred]
   (fn [rf]
     (fn
       ([] (rf))
       ([result] (rf result))
       ([result input]
        (if (pred input)
          (rf result input)
          result)))))
  ([pred coll]
   (lazy-seq
    (when-let [s (seq coll)]
      (if (chunked-seq? s)
        (let [c (chunk-first s)]
          (chunk-cons
            (chunk (chunk-reduce #(if-let [x (pred %2)]
                                    (doto %1 (chunk-append x))
                                    %1)
                     (chunk-buffer (count c)) c))
            (filter pred (chunk-rest s))))
        (let [f (first s) r (rest s)]
          (if (pred f)
            (cons f (filter pred r))
            (filter pred r))))))))

(defn dorun
  "When lazy sequences are produced via functions that have side
  effects, any effects other than those needed to produce the first
  element in the seq do not occur until the seq is consumed. dorun can
  be used to force any effects. Walks through the successive nexts of
  the seq, does not retain the head and returns nil."
  ([coll]
   (when-let [s (seq coll)]
     (recur (next s))))
  ([n coll]
   (when (and (seq coll) (pos? n))
     (recur (dec n) (next coll)))))

(defn doall
  "When lazy sequences are produced via functions that have side
  effects, any effects other than those needed to produce the first
  element in the seq do not occur until the seq is consumed. doall can
  be used to force any effects. Walks through the successive nexts of
  the seq, retains the head and returns it, thus causing the entire
  seq to reside in memory at one time."
  ([coll]
   (dorun coll)
   coll)
  ([n coll]
   (dorun n coll)
   coll))

(defn partition
  "Returns a lazy sequence of lists of n items each, at offsets step
  apart. If step is not supplied, defaults to n, i.e. the partitions
  do not overlap. If a pad collection is supplied, use its elements as
  necessary to complete last partition upto n items. In case there are
  not enough padding elements, return a partition with less than n items."
  ([n coll]
   (partition n n coll))
  ([n step coll]
   (lazy-seq
    (when-let [s (seq coll)]
      (let [p (doall (take n s))]
        (when (== n (count p))
          (cons p (partition n step (nthrest s step))))))))
  ([n step pad coll]
   (lazy-seq
    (when-let [s (seq coll)]
      (let [p (doall (take n s))]
        (if (== n (count p))
          (cons p (partition n step pad (nthrest s step)))
          (list (take n (concat p pad)))))))))

(defn partition-all
  "Returns a lazy sequence of lists like partition, but may include
  partitions with fewer than n items at the end.  Returns a stateful
  transducer when no collection is provided."
  ([n]
   (fn [rf]
     (let [a #dart []]
       (fn
         ([] (rf))
         ([result]
          (let [result (if (.-isEmpty a)
                         result
                         (let [v (vec a)]
                           ;;clear first!
                           (.clear a)
                           (unreduced (rf result v))))]
            (rf result)))
         ([result input]
          (.add a input)
          (if (== n (.-length a))
            (let [v (vec a)]
              (.clear a)
              (rf result v))
            result))))))
  ([n coll]
   (partition-all n n coll))
  ([n step coll]
   (lazy-seq
    (when-let [s (seq coll)]
      (let [seg (doall (take n s))]
        (cons seg (partition-all n step (nthrest s step))))))))

(defn partition-by
  "Applies f to each value in coll, splitting it each time f returns a
   new value.  Returns a lazy seq of partitions.  Returns a stateful
   transducer when no collection is provided."
  ([f]
   (fn [rf]
     (let [a #dart []
           ;; TODO replace "none" by ::none
           pv (volatile! "none")]
       (fn
         ([] (rf))
         ([result]
          (let [result (if (.-isEmpty a)
                         result
                         (let [v (vec a)]
                           ;;clear first!
                           (.clear a)
                           (unreduced (rf result v))))]
            (rf result)))
         ([result input]
          (let [pval @pv
                val (f input)]
            (vreset! pv val)
            (if (or (identical? pval "none") (= val pval))
              (do
                (.add a input)
                result)
              (let [v (vec a)]
                (.clear a)
                (let [ret (rf result v)]
                  (when-not (reduced? ret)
                    (.add a input))
                  ret)))))))))
  ([f coll]
   (lazy-seq
    (when-let [s (seq coll)]
      (let [fst (first s)
            fv (f fst)
            run (cons fst (take-while #(= fv (f %)) (next s)))]
        (cons run (partition-by f (lazy-seq (drop (count run) s)))))))))

(defn remove
  "Returns a lazy sequence of the items in coll for which
  (pred item) returns logical false. pred must be free of side-effects.
  Returns a transducer when no collection is provided."
  ([pred] (filter (complement pred)))
  ([pred coll]
   (filter (complement pred) coll)))

(defn transduce
  "reduce with a transformation of f (xf). If init is not
  supplied, (f) will be called to produce it. f should be a reducing
  step function that accepts both 1 and 2 arguments, if it accepts
  only 2 you can add the arity-1 with 'completing'. Returns the result
  of applying (the transformed) xf to init and the first item in coll,
  then applying xf to that result and the 2nd item, etc. If coll
  contains no items, returns init and f is not called. Note that
  certain transforms may inject or skip items."
  ([xform f coll] (transduce xform f (f) coll))
  ([xform f init coll]
   (let [f (xform f)]
     (f (reduce f init coll)))))

(defn into
  "Returns a new coll consisting of to-coll with all of the items of
  from-coll conjoined. A transducer may be supplied."
  ([] [])
  ([to] to)
  ([to from]
     (if (satisfies? IEditableCollection to)
       (with-meta (persistent! (reduce conj! (transient to) from)) (meta to))
       (reduce conj to from)))
  ([to xform from]
     (if (satisfies? IEditableCollection to)
       (with-meta (persistent! (transduce xform conj! (transient to) from)) (meta to))
       (transduce xform conj to from))))

(defmacro for
  "List comprehension. Takes a vector of one or more
   binding-form/collection-expr pairs, each followed by zero or more
   modifiers, and yields a lazy sequence of evaluations of expr.
   Collections are iterated in a nested fashion, rightmost fastest,
   and nested coll-exprs can refer to bindings created in prior
   binding-forms.  Supported modifiers are: :let [binding-form expr ...],
   :while test, :when test.
  (take 100 (for [x (range 100000000) y (range 1000000) :while (< y x)] [x y]))"
  [seq-exprs body-expr]
  (letfn
      [(emit [seq-exprs ors]
         (let [[binding expr & seq-exprs] seq-exprs
               iter (gensym 'iter__)
               arg (gensym 'coll__)
               wrap
               (fn wrap [mods body]
                 (if-some [[mod expr & more-mods] (seq mods)]
                   (let [body (wrap more-mods body)]
                     (clojure.core/case mod
                       :let `(let ~expr ~body)
                       :while `(if ~expr ~body (or ~@ors))
                       :when `(if ~expr ~body (recur (next ~arg)))))
                   body))
               ors (cons `(~iter (next ~arg)) ors)
               nmods (* 2 (count (take-while keyword? (take-nth 2 seq-exprs))))
               mods (take nmods seq-exprs)
               seq-exprs (seq (drop nmods seq-exprs))
               body
               `(let [~binding (first ~arg)]
                  ~(wrap mods
                     (if seq-exprs
                       `(or ~(emit seq-exprs ors)
                          (recur (next ~arg)))
                       `(cons ~body-expr
                          (lazy-seq (or ~@ors))))))
               body
               (if seq-exprs
                 body
                 ; innermost, also check for chunked
                 `(if (chunked-seq? ~arg)
                    ~(emit-innermost-chunked arg ors binding
                       mods body-expr)
                    ~body))]
           `((fn ~iter [~arg] (when ~arg ~body))
             (seq ~expr))))
       (emit-innermost-chunked [arg ors binding mods body-expr]
         (let [buf `buf#]
           `(let [c# (chunk-first ~arg)
                  size# (count c#)
                  ~buf (chunk-buffer size#)
                  exit#
                  (loop [i# 0]
                    (when (< i# size#)
                      (or
                        (let [~binding (-nth c# i#)]
                          ~(chunked-wrap mods
                             `(chunk-append ~buf ~body-expr)))
                        (recur (inc i#)))))]
              (cond
                (pos? (count ~buf))
                (chunk-cons
                  (chunk ~buf)
                  (lazy-seq
                    (or (when-not exit#
                          (~(ffirst ors) (chunk-next ~arg)))
                      ~@(next ors))))
                exit# (or ~@(next ors))
                :else (recur (chunk-next ~arg))))))
       (chunked-wrap [mods body]
         (if-some [[mod expr & more-mods] (seq mods)]
           (let [body (chunked-wrap more-mods body)]
             (clojure.core/case mod
               :let `(let ~expr ~body)
               :while `(if ~expr ~body true)
               :when `(when ~expr ~body)))
           body))]
    `(lazy-seq ~(emit seq-exprs nil))))

;; TODO : test in cljd when `case` is ready
(defmacro doseq
  "Repeatedly executes body (presumably for side-effects) with
  bindings and filtering as provided by \"for\".  Does not retain
  the head of the sequence. Returns nil."
  [seq-exprs & body-expr]
  #_(assert-args
      (vector? seq-exprs) "a vector for its binding"
      (even? (count seq-exprs)) "an even number of forms in binding vector")
  (letfn [(emit [seq-exprs]
            (let [[binding expr & seq-exprs] seq-exprs
                  acc (gensym 'acc__)
                  wrap
                  (fn wrap [mods body]
                    (if-some [[mod expr & more-mods] (seq mods)]
                      (let [body (wrap more-mods body)]
                        (clojure.core/case mod
                          :let `(let ~expr ~body)
                          :while  `(if ~expr ~body (reduced ~acc))
                          :when `(when ~expr ~body)))
                      body))
                  nmods (* 2 (count (take-while keyword? (take-nth 2 seq-exprs))))
                  mods (take nmods seq-exprs)
                  seq-exprs (seq (drop nmods seq-exprs))
                  body
                  `(reduce (fn [~acc ~binding]
                             ~(wrap mods
                                (if seq-exprs
                                  (emit seq-exprs)
                                  `(do ~@body-expr nil))))
                     nil
                     ~expr)]
              body))]
    (emit seq-exprs)))

(defn vec [coll]
  (into [] coll))

(defn set [coll]
  (into #{} coll))

(defn hash-set
  "Returns a new hash set with supplied keys.  Any equal keys are
  handled as if by repeated uses of conj."
  ([] #{})
  ([& keys] (into #{} keys)))

(defn- -map-lit [^List kvs]
  (loop [^TransientHashMap tm (-as-transient {}) ^int i 0]
    (if (< i (.-length kvs))
      (recur (-assoc! tm (aget kvs i) (aget kvs (+ i 1))) (+ i 2))
      (-persistent! tm))))

(defn hash-map
  "keyval => key val
  Returns a new hash map with supplied mappings."
  [& keyvals]
  (when (.-isOdd (count keyvals))
    (throw (ArgumentError. (str "No value supplied for key: " (last keyvals)))))
  (loop [in (seq keyvals)
         out (transient {})]
    (if in
      (recur (nnext in) (assoc! out (first in) (second in)))
      (persistent! out))))

(defn- -list-lit [^List xs]
  (loop [^PersistentList l () ^int i (.-length xs)]
    (let [i (dec i)]
      (if (neg? i)
        l
        (recur (-conj l (aget xs i)) i)))))

(def ^:private ^math/Random RNG (math/Random.))

(defn rand-int
  "Returns a random integer between 0 (inclusive) and n (exclusive)."
  [n] (.nextInt RNG n))

(defn rand
  "Returns a random floating point number between 0 (inclusive) and
  n (default 1) (exclusive)."
  ([] (.nextDouble RNG))
  ([n] (* (.nextDouble RNG) n)))

(def d6 (inc (rand-int 6)))

#_(defn main []
  (dart:core/print d6)
  (dart:core/print {1 2 3 [4 5 6 7]})
  (dart:core/print [4 5 6 7])
  (dart:core/print '(4 5 6 7))
  (dart:core/print (cons 1 (cons 2 nil)))
  (dart:core/print (list 1 2 3))
  (dart:core/print (seq "aaa"))
  (dart:core/print (seq {:a :b :c :d}))
  (dart:core/print {:a :b :c :d})
  (dart:core/print {:root {:a :b :c :d}})
  (dart:core/print [(math/pow 10e3 1000)])
  (dart:core/print [1 2])
  (dart:core/print (fn [^int a] a))
  (dart:core/print (fn ([] 1) ([^int a] a)))


  (let [at (atom {:a {:b {:c "coucou"}}} :meta {:a :b} :validator (fn [one] (dart:core/print "one") true))]
    (add-watch at :kk (fn [key ref old-state new-state]
                        (dart:core/print old-state)
                        (dart:core/print new-state)))
    (remove-watch at :kk))


  (let [pv (into [] (take 1000 (map (fn [x] x) (iterate inc 0))))]
    (dart:core/print (iterator-seq (.-iterator pv))))

  (let [a #{1 2 3 4 5}]
    (dart:core/print (disj a 1 2 4 3 5 6)))

  (dart:core/print (hash-set :a :b 1 2 3 "d"))

  (dart:core/print (next (seq #{1 2 3 4})))

  (dart:core/print (vec (take 100 (range 0 100 3))))

  (dart:core/print (some-> {:a :b} :a))
  (dart:core/print (= 0 0.0))
  (dart:core/print (hash 0))
  (dart:core/print (hash 0.0))

  )
