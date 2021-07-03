(ns cljd.core
  (:require ["dart:math" :as math]
            ["dart:collection" :as dart-coll]))

(definterface IProtocol
  (extensions [x])
  (satisfies [x]))

(declare -EMPTY-LIST -EMPTY-MAP -EMPTY-VECTOR)

(def ^:dart to-map)
(def ^:dart to-list)

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

(defn ^bool nil?
  {:inline-arities #{1}
   :inline (fn [x] `(.== nil ~x))}
  [x] (.== nil x))

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
                                      (conj gmap) (conj `(if (seq? ~gmap) (to-map (seq ~gmap)) ~gmap))
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

(defprotocol IPending
  "Protocol for types which can have a deferred realization. Currently only
  implemented by Delay and LazySeq."
  (-realized? [x]
   "Returns true if a value for x has been produced, false otherwise."))

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

(defprotocol ICounted
  "Protocol for adding the ability to count a collection in constant time."
  (-count [coll]
    "Calculates the count of coll in constant time."))

(defn counted?
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
    "Return a new chunk of coll with the first item removed."))

(defprotocol IChunkedSeq
  "Protocol for accessing a collection as sequential chunks."
  (-chunked-first [coll]
    "Returns the first chunk in coll.")
  (-chunked-rest [coll]
    "Return a new collection of coll with the first chunk removed.")
  (-chunked-next [coll]
    "Returns a new collection of coll without the first chunk."))

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
  {:inline-arities #{2 3}
   :inline (fn
             ([coll n] `(-nth ~coll ~n))
             ([coll n not-found] `(-nth ~coll ~n ~not-found)))}
  ([coll n] (-nth coll n))
  ([coll n not-found] (-nth coll n not-found)))

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
    (when (and (dart/is? k int) (satisfies? IIndexed o))
      (-nth o k not-found))))

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
  (-hash [o] "Returns the hash code of o."))

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
        (.== ns (.-ns other)) (.== name (.-name other)))))
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

(defprotocol IVolatile
  "Protocol for adding volatile functionality."
  (-vreset! [o new-value]
    "Sets the value of volatile o to new-value without regard for the
     current value. Returns new-value."))

(defn volatile?
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

(defn volatile!
  "Creates and returns a Volatile with an initial value of val."
  [val]
  (Volatile. val))

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

;; TODO should use -equiv or equivalent
#_(defn ^bool = [a b] (.== a b))

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
  {:inline-arities #{1}
   :inline (fn [n] `(== 0 ~n))}
  [n] (== 0 n))

(defn ^num inc
  {:inline (fn [x] `(.+ ~x 1))
   :inline-arities #{1}}
  [x] (.+ x 1))

(defn ^num dec
  {:inline (fn [x] `(.- ~x 1))
   :inline-arities #{1}}
  [x]
  (.- x 1))

(defn ^bool zero?
  {:inline (fn [num] `(.== 0 ~num))
   :inline-arities #{1}}
  [num]
  (== 0 num))

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

(defn ^int alength
  {:inline (fn [array] `(.-length ~array))
   :inline-arities #{1}}
  [^List array] (.-length array))

(defn aclone
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
  (let [k1 (m3-mix-k1 in)
        h1 (m3-mix-h1 0 k1)]
    (m3-fmix h1 4)))

(defn ^int m3-hash-int [in]
  (if (zero? in)
    in
    (let [upper (u32 (bit-shift-right in 32))
          lower (u32 in)
          k (m3-mix-k1 lower)
          h (m3-mix-h1 0 k)
          k (m3-mix-k1 upper)
          h (m3-mix-h1 h k)]
      (m3-fmix h 8))))

(defn hash-combine [^int seed ^int hash]
  ; a la boost
  (bit-xor seed
    (+ hash 0x9e3779b9
      (u32-bit-shift-left seed 6)
      (u32-bit-shift-right seed 2))))

;;http://hg.openjdk.java.net/jdk7u/jdk7u6/jdk/file/8c2c5d63a17e/src/share/classes/java/lang/String.java
(defn hash-string* [^String? s]
  (if-not (nil? s)
    (let [len (.-length s)]
      (if (pos? len)
        (loop [i 0 hash 0]
          (if (< i len)
            (recur (inc i) (+ (u32-mul 31 hash) (.codeUnitAt s i)))
            hash))
        0))
    0))

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

(extend-type bool
  IHash
  (-hash [o]
    (cond
      (true? o) 1231
      (false? o) 1237)))

(extend-type double
  IHash
  (-hash [o]
    ; values taken from cljs
    (cond
      (.== (.-negativeInfinity double) o) -1048576
      (.== (.-infinity double) o) 2146435072
      (.== (.-nan double) o) 2146959360
      true (m3-hash-int (.-hashCode o)))))

(extend-type Null
  IHash
  (-hash [o] 0))

(extend-type Object
  IHash
  (-hash [o] (m3-hash-int (.-hashCode o))))

(defn ^int hash
  {:inline (fn [o] `(-hash ~o))
   :inline-arities #{1}}
  [o] (-hash o))

(defmacro ensure-hash [hash-key hash-expr]
  #_(core/assert (clojure.core/symbol? hash-key) "hash-key is substituted twice")
  `(let [h# ~hash-key]
     (if (< h# 0)
       (let [h# ~hash-expr]
         (set! ~hash-key h#)
         h#)
       h#)))

(deftype ^:abstract #/(SeqListMixin E)
  []
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

(deftype #/(Cons E)
  [meta _first rest ^:mutable ^int __hash]
  ^:mixin #/(dart-coll/ListMixin E)
  ^:mixin #/(SeqListMixin E)
  (^#/(Cons R) #/(cast R) [coll]
   (Cons. meta _first rest __hash))
  Object
  #_(^String toString [coll] "TODO" #_(pr-str* coll))
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
  #_#_IEmptyableCollection
  (-empty [coll] ())
  ISequential
  #_#_IEquiv
  (-equiv [coll other] (equiv-sequential coll other))
  #_#_IHash
  (-hash [coll] (caching-hash coll hash-ordered-coll __hash))
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
  Object
  #_(^String toString [coll]
     #_(pr-str* coll))
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
  #_#_IEmptyableCollection
  (-empty [coll] (-with-meta (.-EMPTY List) meta))
  ISequential
  #_#_IEquiv
  (-equiv [coll other] (equiv-sequential coll other))
  #_#_IHash
  (-hash [coll] (caching-hash coll hash-ordered-coll __hash))
  ISeqable
  (-seq [coll] (when (< 0 count) coll))
  ICounted
  (-count [coll] count)
  ;; TODO: do or not
  #_#_#_IReduce
  (-reduce [coll f] (seq-reduce f coll))
  (-reduce [coll f start] (seq-reduce f start coll)))

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
  ^:mixin #/(dart-coll/ListMixin E)
  ^:mixin #/(SeqListMixin E)
  (^#/(StringSeq R) #/(cast R) [coll]
   (StringSeq. string i meta __hash))
  Object
  #_(^String toString [coll]
     (pr-str* coll))
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
  #_#_IEquiv
  (-equiv [coll other] false)
  ICollection
  (-conj [coll o] (cons o coll))
  #_#_IEmptyableCollection
  (-empty [coll] (.-EMPTY List))
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

(defn ^bool not
  "Returns true if x is logical false, false otherwise."
  {:inline (fn [x] `^bool (if ~x false true))
   :inline-arities #{1}}
  [x] (if x false true))

(deftype #/(LazySeq E)
  [meta ^:mutable ^some fn ^:mutable s ^:mutable ^int __hash]
  ^:mixin #/(dart-coll/ListMixin E)
  ^:mixin #/(SeqListMixin E)
  (^#/(LazySeq R) #/(cast R) [coll]
   (LazySeq. meta fn s __hash))
  Object
  #_(^String toString [coll]
     (pr-str* coll))
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
  #_#_IEmptyableCollection
  (-empty [coll] (-with-meta (.-EMPTY List) meta))
  ISequential
  #_#_IEquiv
  (-equiv [coll other] (equiv-sequential coll other))
  #_#_IHash
  (-hash [coll] (caching-hash coll hash-ordered-coll __hash)))

(defmacro lazy-seq
  "Takes a body of expressions that returns an ISeq or nil, and yields
  a ISeqable object that will invoke the body only the first time seq
  is called, and will cache the result and return it on all subsequent
  seq calls."
  [& body]
  `(new cljd.core/LazySeq nil (fn [] ~@body) nil -1))

;;; PersistentVector

;; declarations
(deftype PersistentVector [])
(deftype TransientVector [])
(deftype PVChunkedSeq [])

(defn aresize [^List a ^int from ^int to pad]
  (let [a' (.filled List to pad)]
    (dotimes [i from]
      (aset a' i (aget a i)))
    a'))

(defn ashrink [^List a ^int to]
  (let [a' (.filled List to ^dynamic (do nil))]
    (dotimes [i to]
      (aset a' i (aget a i)))
    a'))

(deftype VectorNode [edit ^List arr])

(defn push-tail [^PersistentVector pv ^int level ^VectorNode parent ^VectorNode tailnode]
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

(defn ^VectorNode new-path [^int level ^VectorNode node]
  (loop [^int ll level
         ^VectorNode ret node]
    (if (zero? ll)
      ret
      (recur (- ll 5) (VectorNode. nil #dart ^:fixed ^VectorNode [ret])))))

(defn unchecked-array-for
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

(defn- do-assoc [^int level ^VectorNode node ^int n val]
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

(deftype #/(PersistentVector E)
  [meta ^int cnt ^int shift ^VectorNode root ^List tail ^:mutable ^int __hash]
  ^:mixin #/(dart-coll/ListMixin E)
  ^:mixin #/(SeqListMixin E)
  (^#/(PersistentVector R) #/(cast R) [coll]
   (PersistentVector. meta cnt shift root tail __hash))
  Object
  #_(toString [coll]
      (pr-str* coll))
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
                         #_(let [n-r (VectorNode. nil (.filled List 2 (new-path shift (VectorNode. nil tail))))]
                             (aset (.-arr n-r) 0 root)
                             n-r)
                         (VectorNode. nil #dart ^:fixed ^VectorNode [root (new-path shift (VectorNode. nil tail))])
                         (push-tail coll shift root (VectorNode. nil tail)))]
          (PersistentVector. meta (inc cnt) new-shift new-root #dart ^:fixed [o] -1)))))
  #_#_IEmptyableCollection
  (-empty [coll] (-with-meta (.-EMPTY PersistentVector) meta))
  ISequential
  #_#_IEquiv
  (-equiv [coll other]
    (if (instance? PersistentVector other)
      (if (== cnt (count other))
        (let [me-iter  (-iterator coll)
              you-iter (-iterator other)]
          (loop []
            (if ^boolean (.hasNext me-iter)
              (let [x (.next me-iter)
                    y (.next you-iter)]
                (if (= x y)
                  (recur)
                  false))
              true)))
        false)
      (equiv-sequential coll other)))
  #_#_IHash
  (-hash [coll] (caching-hash coll hash-ordered-coll __hash))
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
  #_#_IFind
  (-find [coll n]
    (when (and (<= 0 n) (< n cnt))
      (MapEntry. n (aget (unchecked-array-for coll n) (bit-and n 0x01f)) nil)))
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

(defn vector? [x]
  (satisfies? IVector x))

(def -EMPTY-VECTOR (PersistentVector. nil 0 5 (VectorNode. nil (.empty List)) (.empty List) -1))

;; chunks

(defn chunked-seq?
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
  IReduce
  (-reduce [coll f]
    (let [x (aget arr off)
          off' (inc off)]
      (if (< off' end)
        (loop [acc x ^int idx off']
          (if (< idx end)
            (let [val (f acc (aget arr idx))]
              (if (reduced? val)
                (deref val)
                (recur val (inc idx))))
            acc))
        x)))
  (-reduce [coll f start]
    (loop [acc start ^int idx off]
      (if (< idx end)
        (let [val (f acc (aget arr idx))]
          (if (reduced? val)
            (deref val)
            (recur val (inc idx))))
        acc))))

(deftype ChunkBuffer [^:mutable ^List? arr ^:mutable ^int end]
  Object
  (add [_ o]
    (aset ^List arr end o)
    (set! end (inc end)))
  (chunk [_]
    (let [ret (ArrayChunk. ^List arr 0 end)]
      (set! arr nil)
      ret))
  ICounted
  (-count [_] end))

(defn chunk-buffer [capacity]
  (ChunkBuffer. (.filled #/(List dynamic) capacity nil) 0))

(deftype #/(ChunkedCons E)
  [chunk more meta ^:mutable ^int __hash]
  ^:mixin #/(dart-coll/ListMixin E)
  ^:mixin #/(SeqListMixin E)
  (^#/(ChunkedCons R) #/(cast R) [coll]
   (ChunkedCons. chunk more meta __hash))
  Object
  #_(^String toString [coll]
     (pr-str* coll))
  IWithMeta
  (-with-meta [coll new-meta]
    (if (identical? new-meta meta)
      coll
      (ChunkedCons. chunk more new-meta __hash)))
  IMeta
  (-meta [coll] meta)
  ISequential
  #_#_IEquiv
  (-equiv [coll other] (equiv-sequential coll other))
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
  ICollection
  (-conj [this o] (cons o this))
  #_#_IEmptyableCollection
  (-empty [coll] (.-EMPTY List))
  #_#_IHash
  (-hash [coll] (caching-hash coll hash-ordered-coll __hash)))

(defn chunk-cons [chunk rest]
  (if (< 0 (-count chunk))
    (ChunkedCons. chunk rest nil -1)
    rest))

(defn chunk-append [b x]
  (.add b x))

(defn chunk [b]
  (.chunk b))

(defn chunk-first [s]
  (-chunked-first s))

(defn chunk-rest [s]
  (-chunked-rest s))

(defn chunk-next [s]
  (-chunked-next s))

(deftype #/(PVChunkedSeq E)
  [^PersistentVector vec ^List arr ^int i ^int off meta ^:mutable ^int __hash]
  ^:mixin #/(dart-coll/ListMixin E)
  ^:mixin #/(SeqListMixin E)
  (^#/(PVChunkedSeq R) #/(cast R) [coll]
   (PVChunkedSeq. vec arr i off meta __hash))
  Object
  #_(toString [coll]
      (pr-str* coll))
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
  #_#_IEquiv
  (-equiv [coll other] (equiv-sequential coll other))
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
  #_#_IEmptyableCollection
  (-empty [coll]
    ())
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
  #_#_IHash
  (-hash [coll] (caching-hash coll hash-ordered-coll __hash))
  IReduce
  (-reduce [coll f] (pv-reduce vec f (+ i off)))
  (-reduce [coll f start] (pv-reduce vec f (+ i off) start)))

;;; end chunks

;; transients

(defn ^VectorNode tv-ensure-editable [edit node]
  (if (identical? edit (.-edit node))
    node
    (let [arr (.-arr node)]
      (VectorNode. edit (aresize arr (.-length arr) 32 nil)))))

(defn tv-editable-array-for
  "Returns the editable array where i is located."
  [^TransientVector tv ^int i]
  (loop [node (set! (.-root tv) (tv-ensure-editable (.-edit tv) (.-root tv)))
         level (.-shift tv)]
    (if (< 0 level)
      (let [arr (.-arr node)
            j (bit-and (u32-bit-shift-right i level) 31)]
        (recur (aset arr j (tv-ensure-editable (.-edit tv) (aget arr j))) (- level 5)))
      (.-arr node))))

(defn tv-new-path [edit ^int level ^VectorNode node]
  (loop [ll level
         ret node]
    (if (zero? ll)
      ret
      (let [arr (.filled #/(List dynamic) 32 nil)]
        (aset arr 0 ret)
        (recur (- ll 5) (VectorNode. edit arr))))))

(defn tv-push-tail [^TransientVector tv ^int level ^VectorNode parent tail-node]
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

(deftype TransientVector [^int ^:mutable cnt
                          ^int ^:mutable shift
                          ^some ^:mutable edit
                          ^VectorNode ^:mutable root
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
        (PersistentVector. nil cnt shift root (ashrink tail (inc (bit-and cnt 31))) -1)
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

(deftype #/(PersistentMapEntry K V) [_k _v ^:mutable __hash]
  #/(MapEntry K V)
  (^K key [_] _k)
  (^V value [_] _v)
  IMapEntry
  (-key [node] _k)
  (-val [node] _v)
  #_#_IHash
  (-hash [coll] (caching-hash coll hash-ordered-coll __hash))
  #_#_IEquiv
  (-equiv [coll other] (equiv-sequential coll other))
  IMeta
  (-meta [node] nil)
  IStack
  (-peek [node] val)
  (-pop [node]
    (PersistentVector. meta 1 5 (VectorNode. nil (.empty List)) #dart ^:fixed [_k] -1))
  ICollection
  (-conj [node o]
    (PersistentVector. meta 3 5 (VectorNode. nil (.empty List))
      #dart ^:fixed [_k _v o]  -1))
  #_#_IEmptyableCollection
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
      (PersistentVector. meta 2 5 (VectorNode. nil (.empty List))
        #dart ^:fixed [_k _v]  -1)
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
      (PersistentVector. meta 2 5 (VectorNode. nil (.empty List))
        #dart ^:fixed [_k _v]  -1)
      (-assoc-n n v)))
  IReduce
  (-reduce [node f]
    (unreduced (f key val)))
  (-reduce [node f start]
    (let [r (f start key)]
      (if (reduced? r)
        (deref r)
        (unreduced (f r val)))))
  IFn
  (-invoke [node k]
    (-nth node k))
  (-invoke [node k not-found]
    (-nth node k not-found)))

(defn map-entry?
  [x]
  (satisfies? IMapEntry x))

;; decl
(deftype BitmapNode [])

; cgrand's
(deftype #/(BitmapIterator E) [^:mutable ^BitmapNode node
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
    ;; TODO collisions
    (let [h (hash k)]
      (loop [^BitmapNode node node
             ^int shift 0]
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
              ;; TODO use = instead of ==
              (if (== k' k) (aget arr (inc idx)) not-found)))))))
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
                (loop [i (+ idx 2) j (inc idx)]
                  (when (< i size)
                    (aset new-arr i (aget arr j))
                    (recur (inc i) (inc j))))
                (BitmapNode. (dec cnt) (bit-or bitmap-hi bit) (bit-or bitmap-lo bit) new-arr))
              :else
              (BitmapNode. (dec cnt) bitmap-hi bitmap-lo (doto (aclone (.-arr node)) (aset idx new-child)))))
          ; a kv pair but not the right k
          (not (== k (aget arr idx))) node
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
      (throw (Exception. "Collision!!!!"))))
  (inode_assoc [node shift h k v]
    (if (< shift 32)
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
            (BitmapNode. (inc cnt) (bit-or bitmap-hi bit) (bit-or bitmap-lo bit) new-arr))
          (zero? (bit-and hi lo)) ; node
          (let [^BitmapNode child (aget arr idx)
                ^BitmapNode new-child (.inode_assoc child (+ shift 5) h k v)]
            (if (identical? child new-child)
              node
              (BitmapNode. (+ cnt (- (.-cnt new-child) (.-cnt child))) bitmap-hi bitmap-lo
                (doto (aclone (.-arr node)) (aset idx new-child)))))
          :else ; kv
          (let [k' (aget arr idx)
                v' (aget arr (inc idx))]
            (cond
              ;; TODO not=
              (not (== k' k))
              (let [size (dec (u32x2-bit-count bitmap-hi bitmap-lo))
                    shift' (+ 5 ^int shift)
                    n' (bit-and (u32-bit-shift-right (hash k') shift') 31)
                    bit' (u32-bit-shift-left 1 n')
                    new-node (-> (BitmapNode. 1 bit' bit' #dart ^:fixed [k' v']) (.inode_assoc shift' h k v))
                    new-arr (.filled #/(List dynamic) size new-node)]
                (dotimes [i idx] (aset new-arr i (aget arr i)))
                (loop [i (inc idx) j (+ 2 idx)]
                  (when (< i size)
                    (aset new-arr i (aget arr j))
                    (recur (inc i) (inc j))))
                (BitmapNode. (inc cnt) (bit-xor bitmap-hi hi) bitmap-lo new-arr))
              (identical? v v') node
              :else
              (BitmapNode. cnt bitmap-hi bitmap-lo (doto (aclone arr) (aset (inc idx) v)))))))
      (throw (Exception. "Collision!!!!"))))
  (inode_assoc_transient [node shift h k v]
    (if (< shift 32)
      (let  [n (bit-and (u32-bit-shift-right h shift) 31)
             bit (u32-bit-shift-left 1 n)
             mask (dec bit)
             idx (u32x2-bit-count (bit-and mask bitmap-hi) (bit-and mask bitmap-lo))
             hi (bit-and bitmap-hi bit)
             lo (bit-and bitmap-lo bit)]
        (cond
          (zero? (bit-or hi lo)) ; nothing
          (let [idx' (inc idx)]
            (loop [i 63 j 61]
              (when (< idx' i)
                (aset arr i (aget arr j))
                (recur (dec i) (dec j))))
            (aset arr idx k)
            (aset arr idx' v)
            (set! cnt (inc cnt))
            (set! bitmap-hi (bit-or bitmap-hi bit))
            (set! bitmap-lo (bit-or bitmap-lo bit)))
          (zero? (bit-and hi lo)) ; node
          (let [^BitmapNode child (aget arr idx)
                ^BitmapNode child (if (zero? hi)
                                    ; if node is shared with a Persistent node
                                    (let [new-child-arr (.filled #/(List dynamic) 64 nil)
                                          child-arr (.-arr child)]
                                      (set! bitmap-hi (bit-xor bit bitmap-hi))
                                      (set! bitmap-lo (bit-xor bit bitmap-lo))
                                      (dotimes [i (alength child-arr)]
                                        (aset new-child-arr i (aget child-arr i)))
                                      (let [child-bitmap-hi (.-bitmap_hi child)
                                            child-bitmap-lo (.-bitmap_lo child)
                                            child-node
                                            (BitmapNode. (.-cnt child)
                                              (bit-and child-bitmap-hi child-bitmap-lo)
                                              (bit-or child-bitmap-hi child-bitmap-lo)
                                              new-child-arr)]
                                        (aset arr idx child-node)))
                                    child)
                ; we store cnt from child as mutability kicks in
                old-cnt-child (.-cnt child)]
            (.inode_assoc_transient child (+ shift 5) h k v)
            (set! cnt (+ cnt (- (.-cnt child) old-cnt-child))))
          :else ; kv
          (let [k' (aget arr idx)
                v' (aget arr (inc idx))]
            (cond
              ;; TODO not=u
              (not (== k' k))
              (let [size (dec (u32x2-bit-count bitmap-hi bitmap-lo))
                    shift' (+ 5 ^int shift)
                    n' (bit-and (u32-bit-shift-right (hash k') shift') 31)
                    bit' (u32-bit-shift-left 1 n')
                    new-node (-> (BitmapNode. 1 bit' bit' (doto (.filled #/(List dynamic) 64 nil) (aset 0 k') (aset 1 v')))
                               (.inode_assoc_transient shift' h k v))]
                (aset arr idx new-node)
                (loop [i (inc idx) j (+ 2 idx)]
                  (when (< j 64)
                    (aset arr i (aget arr j))
                    (recur (inc i) (inc j))))
                (aset arr 63 nil)
                (set! cnt (inc cnt))
                (set! bitmap-lo (bit-xor bitmap-lo lo)))
              (identical? v v') node
              :else
              (aset arr (inc idx) v))))
        node)
      (throw (Exception. "Collision!!!!"))))
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
                ^BitmapNode child (if (zero? hi)
                                    ; if node is shared with a Persistent node
                                    (let [new-child-arr (.filled #/(List dynamic) 64 nil)
                                          child-arr (.-arr child)]
                                      (set! bitmap-hi (bit-xor bit bitmap-hi))
                                      (set! bitmap-lo (bit-xor bit bitmap-lo))
                                      (dotimes [i (alength child-arr)]
                                        (aset new-child-arr i (aget child-arr i)))
                                      (let [child-bitmap-hi (.-bitmap_hi child)
                                            child-bitmap-lo (.-bitmap_lo child)
                                            child-node
                                            (BitmapNode. (.-cnt child)
                                              (bit-and child-bitmap-hi child-bitmap-lo)
                                              (bit-or child-bitmap-hi child-bitmap-lo)
                                              new-child-arr)]
                                        (aset arr idx child-node)))
                                    child)
                ^int old-child-cnt (.-cnt child)
                ^BitmapNode new-child (.inode_without_transient child (+ shift 5) h k)
                ^int new-child-cnt (.-cnt new-child)]
            (cond
              (== new-child-cnt old-child-cnt) node
              (and (== 1 new-child-cnt) (zero? (bit-xor (.-bitmap_hi new-child) (.-bitmap_lo new-child))))
              (let [k (aget (.-arr new-child) 0)
                    v (aget (.-arr new-child) 1)]
                (loop [i (+ idx 2) j (inc idx)]
                  (when (< i 64)
                    (aset arr i (aget arr j))
                    (recur (inc i) (inc j))))
                (aset arr idx k)
                (aset arr idx v)
                (set! cnt (dec cnt))
                (set! bitmap-hi (bit-or bitmap-hi bit))
                (set! bitmap-lo (bit-or bitmap-lo bit)))
              :else
              (do (set! cnt (dec cnt))
                  (aset arr idx new-child))))
          ; a kv pair but not the right k
          (not (== k (aget arr idx))) node
          ; the right kv pair
          :else
          (do (loop [i idx j (+ 2 idx)]
                (when (< j 64)
                  (aset arr i (aget arr j))
                  (recur (inc i) (inc j))))
              (aset arr 62 nil)
              (aset arr 63 nil)
              (set! cnt (dec cnt))
              (set! bitmap-hi (bit-xor bitmap-hi bit))
              (set! bitmap-lo (bit-xor bitmap-lo bit))))
        node)
      (throw (Exception. "Collision!!!!")))))

;; decl
(deftype PersistentHashMap [])

(deftype TransientHashMap [^:mutable ^bool editable ^:mutable ^BitmapNode root]
  ITransientCollection
  (-conj! [tcoll o]
    (when-not editable
      (throw (ArgumentError. "conj! after persistent!")))
    (cond
      (map-entry? o)
      (-assoc tcoll (-key o) (-val o))
      (vector? o)
      (-assoc tcoll (-nth o 0) (-nth o 1))
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
  #_#_IEmptyableCollection
  (-empty [coll] (-with-meta (.-EMPTY PersistentHashMap) meta))
  #_#_IEquiv
  (-equiv [coll other] (equiv-map coll other))
  #_#_IHash
  (-hash [coll] (caching-hash coll hash-unordered-coll __hash))
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
    (let [new-arr (.filled #/(List dynamic) 64 nil)
          arr (.-arr root)
          bitmap-hi (.-bitmap_hi root)
          bitmap-lo (.-bitmap_lo root)]
      (dotimes [i (alength arr)]
        (aset new-arr i (aget arr i)))
      (TransientHashMap. true (BitmapNode. (.-cnt root) (bit-and bitmap-hi bitmap-lo) (bit-or bitmap-hi bitmap-lo) new-arr)))))

(def -EMPTY-MAP
  (PersistentHashMap. nil (BitmapNode. 0 0 0 (.empty List)) -1))

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
            (chunk (reduce #(doto %1 (chunk-append (f %2)))
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
            (chunk (reduce #(if-some [x (f %2)]
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
            (chunk (reduce #(if-let [x (pred %2)]
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
            (if (or (identical? pval "none")
                  ;; TODO replace == by =
                  (== val pval))
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
            ;; TODO change == to =
            run (cons fst (take-while #(== fv (f %)) (next s)))]
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

(defn vec [coll]
  (into [] coll))

(defn -map-lit [^List kvs]
  (loop [^TransientHashMap tm (-as-transient {}) ^int i 0]
    (if (< i (.-length kvs))
      (recur (-assoc! tm (aget kvs i) (aget kvs (+ i 1))) (+ i 2))
      (-persistent! tm))))

(defn -list-lit [^List xs]
  (loop [^PersistentList l () ^int i (.-length xs)]
    (let [i (dec i)]
      (if (neg? i)
        l
        (recur (-conj l (aget xs i)) i)))))

(def ^math/Random RNG (math/Random.))

(defn rand-int
  "Returns a random integer between 0 (inclusive) and n (exclusive)."
  [n] (.nextInt RNG n))

(defn rand
  "Returns a random floating point number between 0 (inclusive) and
  n (default 1) (exclusive)."
  ([] (.nextDouble RNG))
  ([n] (* (.nextDouble RNG) n)))

(def d6 (inc (rand-int 6)))

(defn main []
  (dart:core/print d6)
  (dart:core/print {1 2 3 [4 5 6 7]})
  (dart:core/print [4 5 6 7])
  (dart:core/print '(4 5 6 7))
  (dart:core/print (into [] (partition-all 2) [1 2 3 4 5]))
    (dart:core/print (into [] (partition-by #(.-isOdd %)) [1 2 3 4 5]))
  #_(let [one (cons 1 (cons 2 (cons 3 nil)))]


    (^:dart dart:core/print
     (first one))


    (^:dart dart:core/print
     (rest one))

    (^:dart dart:core/print
     (first (rest one)))


    (^:dart dart:core/print
     (first (next one)))

    (^:dart dart:core/print
     (first (next (next one))))

    (^:dart dart:core/print
     (first (next (next (next one)))))

    (^:dart dart:core/print
    )

    )

  #_(let [coucou #dart [1 2]]

    (^:dart dart:core/print
     (first coucou))

    (^:dart dart:core/print
     ())

    (^:dart dart:core/print
     (first coucou))

    (^:dart dart:core/print
     (first (rest coucou)))

    (^:dart dart:core/print
     (first coucou))

    (^:dart dart:core/print
     (first (rest coucou)))

    #_(^:dart dart:core/print
     (first (rest (rest coucou))))

    )




  #_(^:dart dart:core/print
   (reduce #(str %1 " " %2) "START: " (seq "abc"))
   )

  #_(^:dart dart:core/print
   (reduce #(str %1 " " %2) "START: " (seq "a"))
   )

  #_(^:dart dart:core/print
   (reduce #(str %1 " " %2) (seq "a"))
   )

  #_(^:dart dart:core/print
   (reduce (fn [] "aa") "")
   )

  #_(^:dart dart:core/print
   (first (lazy-seq #dart [1 2 3])))

  #_(^:dart dart:core/print
   (first (next (lazy-seq #dart [1 2 3]))))

  #_(let [a (map #(do (^:dart dart:core/print %) (inc %)) #dart [1 2 3])]
    (^:dart dart:core/print
     "not realized")
    (^:dart dart:core/print
     (first a)))

  #_(let [a ]
      (dart:core/print (first (map #(+ %1 %2)  #dart [3 4 2 1]  #dart [1 2 3]))))

  #_(dart:core/print (seq #dart [1 2]))

  #_(dart:core/print (next (next (seq #dart [1 2]))))


  #_(dart:core/print
   (fnext (next (interleave (list 3 2) (list "a" "b") (list 10 11)))))


  #_(dart:core/print
   (first (drop-last (list 1 2))))

  #_(dart:core/print
   (fnext (drop-last (list 1 2))))

  #_(dart:core/print
   (first (second (partition 2 #dart[1 2 3 4]))))

  #_(dart:core/print
   (first (second (partition-all 2 #dart[1 2 3]))))

  #_(dart:core/print
   (next (partition 2 #dart[1 2 3])))

  #_(dart:core/print (count (cons 1 (seq #dart [2 3 4]))))
  #_(dart:core/print
   (first (last (partition-by #(< % 2) #dart[1 2 3 4 1]))))

  #_(dart:core/print
   (count (remove #(== 1 %) #dart[1 2 3 4 1])))

  #_(dart:core/print
   (last (remove #(== 1 %) #dart[1 2 3 4 1])))

  #_(dart:core/print
   (some #(if (== 1 %) %) #dart[ 2 3 4 5 1]))

  #_(dart:core/print
   (count (keep identity #dart[nil 2 3 nil 4 5 1])))

  #_(dart:core/print
   (reduce (fn [acc item]
             (if (== item 5)
               (reduced acc)
               (+ acc item)))
           0
           #dart[10 1 3 5 6]))

  #_(dart:core/print
   (reduce (fn [acc item]
             (if (== item "a")
               (reduced acc)
               (str acc " " item)))
           ""
           "bcdeafjdffd"))

  #_(dart:core/print
   (u32-bit-shift-right 33 5))


  (dart:core/print "Started PV test")
  #_(let [sw (Stopwatch.)
        _ (.start sw)
        pv (PersistentVector. nil 0 5 (VectorNode. nil (List/filled 0 nil)) (List/filled 0 nil) -1)
        pv1
        (loop [pv2 pv
               idx 0]
          (if (== idx 1000000)
            pv2
            (recur (-conj pv2 idx) (inc idx))))]
    (.stop sw)
    (dart:core/print (.-elapsedMilliseconds sw))
    (dart:core/print "ms")
    (dart:core/print "-nth with a default value")
    (dart:core/print (-nth pv1 10000))
    (dart:core/print (-nth pv1 100000000 "default")))

  #_(let [pv (PersistentVector. nil 0 5 (VectorNode. nil (.empty List)) (.empty List) -1)]
    (quick-bench
      (loop [pv pv, idx 1000000]
        (if (pos? idx)
          (recur (-conj pv idx) (dec idx))
          pv))))
  #_(dart:core/print "Ended PV test")

  #_(let [pv (PersistentVector. nil 0 5 (VectorNode. nil (List/filled 0 ^dynamic (do nil))) (List/filled 0 ^dynamic (do nil)) -1)]
    (quick-bench
      (loop [pv pv, idx 1000000]
        (if (pos? idx)
          (recur (-conj pv idx) (dec idx))
          pv))))

  #_#_(dart:core/print "Started TransientVector test")
  (let [tv (-as-transient [])]
    (quick-bench
      (loop [tv tv, idx 1000000]
        (if (pos? idx)
          (recur (-conj! tv idx) (dec idx))
          tv))))

  #_(let [sw (Stopwatch.)
        _ (.start sw)
        N 1000000
        pv (PersistentVector. nil 0 5 (VectorNode. nil (List/filled 0 nil)) (List/filled 0 nil) -1)
        pv1
        (loop [pv2 pv
               idx 0]
          (if (== idx N)
            pv2
            (recur (-conj pv2 idx) (inc idx))))]
    (.stop sw)
    (loop [pv pv1 expected (dec N)]
      (if (<= 0 expected)
        (if (== expected (-peek pv))
          (recur (-pop pv) (dec expected))
          (dart:core/print (str "fail " expected)))
        (if (== 0 (count pv))
          "ok"
          (dart:core/print "ouch"))))
    (dart:core/print (-peek (-pop pv1)))
    (dart:core/print (.-shift pv1))
    (dart:core/print (.-shift  (-pop pv1))))

  #_#_(dart:core/print "peek")
  (let [pv []]
    (dart:core/print (-peek (-assoc pv 0 1)))
    ; throw
    (dart:core/print (count (-conj (-conj pv 10) 11)))
    (dart:core/print (-peek (-conj (-conj pv 10) 11)))
    (dart:core/print (-peek (-assoc (-conj (-conj pv 10) 11) 1 1)))
    ; throw
    #_(dart:core/print (-assoc (-conj (-conj pv 10) 11) "a" 1)))

  #_(let [pv (loop [pv []
                  idx 0]
             (if (== idx 10000000)
               pv
               (recur (-conj pv idx) (inc idx))))]
    (dart:core/print (-nth (-assoc pv 11111 "coucou") 11111)))

  (dart:core/print "Start reduce +")
  (let [pv (loop [pv []
                  idx 0]
             (if (== idx 100000)
               pv
               (recur (-conj pv idx) (inc idx))))]
    (dart:core/print (reduce + 0 pv)))
  #_(dart:core/print "end reduce +")

  #_#_#_(dart:core/print "Start reduce on chunked-seq form []+")
  (let [pv (loop [pv []
                  idx 0]
             (if (== idx 100000)
               pv
               (recur (-conj pv idx) (inc idx))))]
    (dart:core/print (first (seq pv)))
    (dart:core/print (reduce + 0 (seq pv)))
    (dart:core/print (reduce + 0 (next (next (seq pv))))))
  (dart:core/print "end reduce +")

  #_(dart:core/print "map on chunked")
  #_(let [pv (loop [pv []
                  idx 0]
             (if (== idx 100000)
               pv
               (recur (-conj pv idx) (inc idx))))]
    (dart:core/print (first (map (fn [o] (dart:core/print o) o) (seq pv)))))

  #_#_(dart:core/print "keep on chunked")
  (let [pv (loop [pv []
                  idx 0]
             (if (== idx 100000)
               pv
               (recur (-conj pv idx) (inc idx))))]
    (dart:core/print (first (keep (fn [o] (dart:core/print o)
                                    (when (.-isOdd o) o)) (seq pv)))))

  #_(let [pv (loop [pv []
                  idx 0]
             (if (== idx 100000)
               pv
               (recur (-conj pv idx) (inc idx))))]
    (-kv-reduce pv (fn [acc i item]
                     (-conj acc #_(-conj (-conj [] i) item) i))
      [])
    (dart:core/print (-peek (-peek (-kv-reduce
                                     pv
                                     (fn [acc i item]
                                       (-conj acc (-conj (-conj [] i) item)))
                                     [])))))

  #_(let [pv (-conj [] 100)]
    (dart:core/print (pv 0)))

  #_(dart:core/print "Started Vector test")
  #_(let [sw (Stopwatch.)
        _ (.start sw)

        pv1
        (loop [v (List/filled 1000000 1)
               idx 0]
          (if (== idx 1000000)
            (.elementAt v 11111)
            (recur (do (. v "[]=" idx idx) v) (inc idx))))]
    (.stop sw)
    (dart:core/print (.-elapsedMilliseconds sw))
    (dart:core/print "ms"))

  (dart:core/print "PersistentHashMap")
  #_(let [node (BitmapNode. 0 0 0 (.empty List))
        h (PersistentHashMap. nil node -1)
        li (.generate List 100000 #(+ % 0))
        h1 (reduce (fn [acc item] (-assoc acc item item)) h li)
        h' (-seq h1)]

    (quick-bench (count h'))

    #_(let [ooo (-kv-reduce h1 (fn [acc k v] (-conj acc (-conj (-conj empty-persistent-vector k) v))) empty-persistent-vector)]
      (dart:core/print (count ooo))
      (dart:core/print (count h1))
      (dart:core/print (-count h')))

    #_(dart:core/print (-lookup h' 500 222))
    #_#_#_(dart:core/print (-lookup (-dissoc h' 500) 500 222))
    (dart:core/print (-peek (-first (-seq h'))))
    (dart:core/print (-peek (-first (-next (-seq h')))))
    #_(dart:core/print (count (-seq h')))
    #_(loop [h' (-seq h')]
      (when h'
        (dart:core/print (-peek (-first h')))
        (recur (-next h'))))
    #_(dart:core/print (-peek (last (-seq h'))))
    #_(dart:core/print (-lookup (-assoc (-assoc h 3342 1) 2 2) 2)))

  #_(let [node (BitmapNode. 0 0 0 (.empty List))
        h (PersistentHashMap. nil node -1)
        li (.generate List 100 #(+ % 0))
        h1 (reduce (fn [acc item] (-assoc acc item item)) h li)
        tran (-as-transient h #_h1)]
    #_  (dotimes [i 1000]
          (-assoc! tran i i))
    (let [tv (loop [tv tran, idx 100000]
               (if (pos? idx)
                 (recur (-assoc! tv idx idx) (dec idx))
                 tv))]
      (-> tv
        (-dissoc! 1000)
        (-dissoc! 999)
        (-dissoc! 999))
      (dart:core/print (-count tv))
      (dart:core/print (tv 999))
      (dart:core/print (-count (-persistent! tv)))

      (let [node (BitmapNode. 0 0 0 (.empty List))
            h (PersistentHashMap. nil node -1)
            h (reduce
                (fn [m i] (-assoc m i (- i)))
                h
                (.generate List 100000 #(+ % 0)))]
        (dart:core/print (count (seq h)))
        (quick-bench
          (let [it (.-iterator (.-entries h))]
            (while (.moveNext it))))
        (quick-bench
          (count (seq h))))

      #_#_#_#_(dart:core/print (-count hm))
      (dart:core/print (-count (seq hm)))
      (dart:core/print (-lookup hm 9999))
      (loop [hm hm, idx 10000]
        (if (pos? idx)
          (recur (-assoc hm idx 0) (dec idx))
          (dart:core/print (-count hm))))
      )

    #_(quick-bench
      (loop [phm h, idx 100000]
        (if (pos? idx)
          (recur (-assoc phm idx idx) (dec idx))
          phm)))



    #_(let [ooo (-kv-reduce h1 (fn [acc k v] (-conj acc (-conj (-conj empty-persistent-vector k) v))) empty-persistent-vector)]
        (dart:core/print (count ooo))
        (dart:core/print (count h1))
        (dart:core/print (-count h')))

    #_(dart:core/print (-lookup h' 500 222))
    #_#_#_(dart:core/print (-lookup (-dissoc h' 500) 500 222))
    (dart:core/print (-peek (-first (-seq h'))))
    (dart:core/print (-peek (-first (-next (-seq h')))))
    #_(dart:core/print (count (-seq h')))
    #_(loop [h' (-seq h')]
        (when h'
          (dart:core/print (-peek (-first h')))
          (recur (-next h'))))
    #_(dart:core/print (-peek (last (-seq h'))))
    #_(dart:core/print (-lookup (-assoc (-assoc h 3342 1) 2 2) 2)))

  (let [e (cljd.core/PersistentHashMap. nil (cljd.core/BitmapNode. 0 0 0 (.empty List)) -1)
        m (assoc e "items" (reduce conj [] #dart ^String ["one" "two" "three" "four"]))]
    (dart:core/print (val (first m))))

  (dart:core/print :hello/world)
  (dart:core/print (hash-string* "hello"))
  )
