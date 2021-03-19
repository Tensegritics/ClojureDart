(ns cljd.core
  (:require ["dart:core" :as dc :refer [print]]))

(definterface IProtocol
  (extension [x])
  (satisfies [x]))

(def ^:dart to-map)
(def ^:dart to-list)

(def ^{:clj true} =)
(def ^{:clj true} apply)
(def ^{:clj true} assoc)
(def ^{:dart true} butlast)
(def ^{:clj true} concat)
(def ^{:clj true} conj)
(def ^{:dart true} cons)
(def ^{:dart true} contains?)
(def ^{:dart true} count)
(def ^{:clj true} dissoc)
(def ^{:dart true} every?)
(def ^{:dart true} first)
(def ^{:clj true} gensym)
(def ^{:dart true} ident?)
(def ^{:dart true} inc)
(def ^{:dart true} key)
(def ^{:dart true} keys)
(def ^{:clj true} keyword)
(def ^{:dart true} keyword?)
(def ^{:dart true} last)
(def ^{:clj true} list)
(def ^{:clj true} list*)
(def ^{:clj true} map)
(def ^{:dart true} map?)
(def ^{:dart true} meta)
(def ^{:dart true} name)
(def ^{:dart true} namespace)
(def ^{:dart true} not)
(def ^{:dart true} next)
(def ^{:dart true} nil?)
(def ^{:dart true} nnext)
(def ^{:clj true} nth)
(def ^{:clj true} partition)
(def ^{:clj true} reduce)
(def ^{:dart true} second)
(def ^{:dart true} seq)
(def ^{:dart true} seq?)
(def ^{:dart true} set)
(def ^{:dart true} some)
(def ^{:clj true} str)
(def ^{:dart true} string?)
(def ^{:clj true} subvec)
(def ^{:clj true} symbol)
(def ^{:dart true} symbol?)
(def ^{:dart true} val)
(def ^{:clj true} vary-meta)
(def ^{:dart true} vec)
(def ^{:clj true} vector)
(def ^{:dart true} vector?)
(def ^{:dart true} with-meta)

;; syntax quote support at bootstrap
;; the :cljd nil is most certainly going to bite us once we run the compiler on dart vm
(def ^:dart ^:bootstrap seq #?(:cljd nil :clj clojure.core/seq))
(def ^:dart ^:bootstrap first #?(:cljd nil :clj clojure.core/first))
(def ^:dart ^:bootstrap next #?(:cljd nil :clj clojure.core/next))
(def ^:clj ^:bootstrap concat #?(:cljd nil :clj clojure.core/concat))
(def ^:clj ^:bootstrap list #?(:cljd nil :clj clojure.core/list))
(def ^:clj ^:bootstrap vector #?(:cljd nil :clj clojure.core/vector))
(def ^:clj ^:bootstrap hash-set #?(:cljd nil :clj clojure.core/hash-set))
(def ^:clj ^:bootstrap hash-map #?(:cljd nil :clj clojure.core/hash-map))
(def ^:clj ^:bootstrap apply #?(:cljd nil :clj clojure.core/apply))

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

(defn foobar [x] (let [[a b] x] (.+ a b)))

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

;;; NOT THE REAL THING BELOW

(defn ^num count [x] (if (.== nil x) 0 (.-length x)))

;; used by writer when encounter ()
(def empty-list nil)

#_(defn next [coll]
  (let [s (.sublist coll 1)]
    (when (.< 0 (.-length s))
      s)))

#_(defn first [coll] (if (.== coll nil) nil (.-first coll)))

(defn nth [x i default]
  (if (.< i (.-length x))
    (. x "[]" i)
    default))

(defn ^bool = [a b] (.== a b))

(defn ^bool zero? [num] (= 0 num))

(defprotocol ISeq
  "Protocol for collections to provide access to their items as sequences."
  (-first [coll]
    "Returns the first item in the collection coll.")
  (-rest [coll]
    "Returns a new collection of coll without the first item. It should
     always return a seq, e.g.
     (rest []) => ()
     (rest nil) => ()"))

(defprotocol ASeq
  "Marker protocol indicating an array sequence.")

(defprotocol INext
  "Protocol for accessing the next items of a collection."
  (-next [coll]
    "Returns a new collection of coll without the first item. In contrast to
     rest, it should return nil if there are no more items, e.g.
     (next []) => nil
     (next nil) => nil"))

(defprotocol ISequential
  "Marker interface indicating a persistent collection of sequential items")

(defprotocol ISeqable
  "Protocol for adding the ability to a type to be transformed into a sequence."
  (-seq [o]
    "Returns a seq of o, or nil if o is empty."))

(defprotocol IList
  "Marker interface indicating a persistent list")

(defprotocol ICollection
  "Protocol for adding to a collection."
  (-conj [coll o]
    "Returns a new collection of coll with o added to it. The new item
     should be added to the most efficient place, e.g.
     (conj [1 2 3 4] 5) => [1 2 3 4 5]
     (conj '(2 3 4 5) 1) => '(1 2 3 4 5)"))

(defprotocol ICloneable
  "Protocol for cloning a value."
  (-clone [value]
    "Creates a clone of value."))

(defprotocol ICounted
  "Protocol for adding the ability to count a collection in constant time."
  (-count [coll]
    "Calculates the count of coll in constant time."))

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

(defprotocol IIndexed
  "Protocol for collections to provide indexed-based access to their items."
  (-nth [coll n] [coll n not-found]
    "Returns the value at the index n in the collection coll.
     Returns not-found if index n is out of bounds and not-found is supplied."))

(defprotocol ILookup
  "Protocol for looking up a value in a data structure."
  (-lookup [o k] [o k not-found]
    "Use k to look up a value in o. If not-found is supplied and k is not
     a valid value that can be used for look up, not-found is returned."))

(defprotocol IStack
  "Protocol for collections to provide access to their items as stacks. The top
  of the stack should be accessed in the most efficient way for the different
  data structures."
  (-peek [coll]
    "Returns the item from the top of the stack. Is used by cljs.core/peek.")
  (-pop [coll]
    "Returns a new stack without the item on top of the stack. Is used
     by cljs.core/pop."))

(defn ^bool < [a b] (.< a b))

(defn ^bool <= [a b] (.<= a b))

(defn ^bool > [a b] (.> a b))

(defn ^bool >= [a b] (.>= a b))

(defn ^bool pos? [a] (.< 0 a))

(defn ^num + [a b] (.+ a b))

(defn ^num - [a b] (.- a b))

(defn ^num inc
  "Returns a number one greater than num."
  [x] (+ x 1))

(defn ^num dec [x] (- x 1))

#_(

  (defn count [x] (if (.== nil x) 0 (.-length x)))

   (defn seq [coll] coll)

   (defn next [coll]
     (let [s (.sublist coll 1)]
       (when (.< 0 (.-length s))
         s)))

   (defn first [coll] (if (.== coll nil) nil (.-first coll)))

   (defn nth [x i default]
     (if (.< i (.-length x))
       (. x "[]" i)
       default))

   (defn = [a b] (.== a b))

   (defn < [a b] (.< a b))

   (defn pos? [a] (.< 0 a))

   (defn + [a b] (.+ a b))

   (defn - [a b] (.- a b))

   (defn ^bool nil? [x] (.== nil x))

(defn ^bool not
  "Returns true if x is logical false, false otherwise."
  [x] (if x false true))

(defn ^:clj str
  ([] "")
  ([x] (if (nil? x)
         ""
         (.toString x)))
  ([x & ys]
   ;; TODO : maybe use writeAll ?
   (loop [sb (dc/StringBuffer. (str x)) more ys]
     (if more
       (recur (doto sb (.write (str (first more)))) (next more))
       (.toString sb)))))

(defn ^num alength [array] (.-length array))

(defn ^:clj aget
  ([array idx]
   (. array "[]" idx))
  ([array idx & idxs]
   (apply aget (aget array idx) idxs)))

(defn ^List aclone
  [arr]
  (.from dc/List arr))

(defn aset
  ([array idx val]
   (. array "[]=" idx val)
   val)
  ([array idx idx2 & idxv]
   (apply aset (aget array idx) idx2 idxv)))

(def cons nil)

(deftype IndexedSeq [arr i meta]
  #_Object
  #_(toString [coll]
    (pr-str* coll))
  #_(equiv [this other]
    (-equiv this other))
  #_(indexOf [coll x]
    (-indexOf coll x 0))
  #_(indexOf [coll x start]
    (-indexOf coll x start))
  #_(lastIndexOf [coll x]
    (-lastIndexOf coll x (count coll)))
  #_(lastIndexOf [coll x start]
    (-lastIndexOf coll x start))

  ICloneable
  (-clone [_] (IndexedSeq. arr i meta))

  ISeqable
  (-seq [this]
    (when (< i (alength arr))
      this))

  #_#_IMeta
  (-meta [coll] meta)
  #_#_IWithMeta
  (-with-meta [coll new-meta]
    (if (identical? new-meta meta)
      coll
      (IndexedSeq. arr i new-meta)))

  ASeq
  ISeq
  (-first [this] (aget arr i))
  (-rest [_] (if (< (inc i) (alength arr))
               (IndexedSeq. arr (inc i) nil)
               ()))

  INext
  (-next [_] (if (< (inc i) (alength arr))
               (IndexedSeq. arr (inc i) nil)
               nil))

  ICounted
  (-count [_]
    ; TODO : use max
    (let [l (- (alength arr) i)]
      (if (< 0 l) l 0))
    #_(max 0 (- (alength arr) i)))

  IIndexed
  (-nth [coll n]
    (let [i (+ n i)]
      (if (and (<= 0 i) (< i (alength arr)))
        (aget arr i)
        (throw (UnimplementedError. "Index out of bounds")))))
  (-nth [coll n not-found]
    (let [i (+ n i)]
      (if (and (<= 0 i) (< i (alength arr)))
        (aget arr i)
        not-found)))

  ISequential
  #_#_IEquiv
  (-equiv [coll other] (equiv-sequential coll other))

  #_#_IIterable
  (-iterator [coll]
    (IndexedSeqIterator. arr i))

  ICollection
  (-conj [coll o] (cons o coll))

  #_#_IEmptyableCollection
  (-empty [coll] (.-EMPTY List))

  #_#_#_IReduce
  (-reduce [coll f]
    (array-reduce arr f (aget arr i) (inc i)))
  (-reduce [coll f start]
    (array-reduce arr f start i))

  #_#_IHash
  (-hash [coll] (hash-ordered-coll coll))

  #_#_IReversible
  (-rseq [coll]
    (let [c (-count coll)]
      (if (pos? c)
        (RSeq. coll (dec c) nil)))))

(defn ^ISeq seq
  "Returns a seq on the collection. If the collection is
  empty, returns nil.  (seq nil) returns nil. seq also works on
  Strings."
  [coll]
  (when-not (nil? coll)
    (cond
      (dart-is? coll ISeqable)
      (-seq coll)

      (dart-is? coll dc/List)
      (when-not (zero? (alength coll))
        (IndexedSeq. coll 0 nil))

      (dart-is? coll String)
      (when-not (zero? (.-length coll))
        (IndexedSeq. coll 0 nil))

      #_#_(js-iterable? coll)
      (es6-iterator-seq
        (.call (gobject/get coll ITER_SYMBOL) coll))

      #_#_(native-satisfies? ISeqable coll)
      (-seq coll)

      true (throw (UnimplementedError. "not implemented yet.")))))

(defn first
  "Returns the first item in the collection. Calls seq on its
  argument. If coll is nil, returns nil."
  [coll]
  (when-not (nil? coll)
    (if (dart-is? coll ISeq)
      (-first coll)
      (let [s (seq coll)]
        (when-not (nil? s)
          (-first s))))))

(defn ^ISeq rest
  "Returns a possibly empty seq of the items after the first. Calls seq on its
  argument."
  [coll]
  (if-not (nil? coll)
    (if (dart-is? coll ISeq)
      (-rest coll)
      (let [s (seq coll)]
        (if s
          (-rest s)
          ())))
    ()))

(defn ^int count
  [coll]
  (if-not (nil? coll)
    (cond
      (dart-is? coll ICounted)
      (-count coll)

      (dart-is? coll dc/List)
      (alength coll)

      (dart-is? coll dc/String)
      ^number (.-length coll)

      true (throw (UnimplementedError. "not implemented yet."))
      #_#_(implements? ISeqable coll)
      (accumulating-seq-count coll)

      #_#_:else (-count coll))
    0))

(defn clone
  [value]
  (-clone value))

(defn next
  "Returns a seq of the items after the first. Calls seq on its
  argument.  If there are no more items, returns nil"
  [coll]
  (when-not (nil? coll)
    (if (dart-is? coll ISeq)
      (-next coll)
      (seq (rest coll)))))

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

(deftype LazySeq [meta ^:mutable ^:dart fn ^:mutable s ^:mutable __hash]
  Object
  #_#_(toString [coll]
        (pr-str* coll))
  (equiv [this other]
    (-equiv this other))
  (sval [coll]
    (if (nil? fn)
      s
      (do
        (set! s (fn))
        (set! fn nil)
        s)))
  #_#_#_#_(indexOf [coll x]
            (-indexOf coll x 0))
  (indexOf [coll x start]
    (-indexOf coll x start))
  (lastIndexOf [coll x]
    (-lastIndexOf coll x (count coll)))
  (lastIndexOf [coll x start]
    (-lastIndexOf coll x start))

  #_#_IPending
  (-realized? [coll]
    (not fn))

  #_#_IWithMeta
  (-with-meta [coll new-meta]
    (if (identical? new-meta meta)
      coll
      (LazySeq. new-meta #(-seq coll) nil __hash)))

  #_#_IMeta
  (-meta [coll] meta)

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

  INext
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
  (-hash [coll] (caching-hash coll hash-ordered-coll __hash))

  ISeqable
  (-seq [coll]
    (.sval coll)
    (when-not (nil? s)
      (loop [ls s]
        (if (dart-is? ls LazySeq)
          (recur (.sval ls))
          (do (set! s ls)
              (seq s))))))

  #_#_#_IReduce
  (-reduce [coll f] (seq-reduce f coll))
  (-reduce [coll f start] (seq-reduce f start coll)))

(deftype List [meta first rest count ^:mutable __hash]
  #_#_#_#_#_#_#_Object
  (toString [coll]
    (pr-str* coll))
  (equiv [this other]
    (-equiv this other))
  (indexOf [coll x]
    (-indexOf coll x 0))
  (indexOf [coll x start]
    (-indexOf coll x start))
  (lastIndexOf [coll x]
    (-lastIndexOf coll x count))
  (lastIndexOf [coll x start]
    (-lastIndexOf coll x start))

  IList

  ICloneable
  (-clone [_] (List. meta first rest count __hash))

  #_#_IWithMeta
  (-with-meta [coll new-meta]
    (if (identical? new-meta meta)
      coll
      (List. new-meta first rest count __hash)))

  #_#_IMeta
  (-meta [coll] meta)

  ASeq
  ISeq
  (-first [coll] first)
  (-rest [coll]
    (if (= count 1)
      ()
      rest))

  INext
  (-next [coll]
    (if (= count 1)
      nil
      rest))

  IStack
  (-peek [coll] first)
  (-pop [coll] (-rest coll))

  ICollection
  (-conj [coll o] (List. meta o coll (inc count) nil))

  #_#_IEmptyableCollection
  (-empty [coll] (-with-meta (.-EMPTY List) meta))

  ISequential
  #_#_IEquiv
  (-equiv [coll other] (equiv-sequential coll other))

  #_#_IHash
  (-hash [coll] (caching-hash coll hash-ordered-coll __hash))

  ISeqable
  (-seq [coll] coll)

  ICounted
  (-count [coll] count)

  #_#_#_IReduce
  (-reduce [coll f] (seq-reduce f coll))
  (-reduce [coll f start] (seq-reduce f start coll)))

(deftype EmptyList [meta]
  #_#_#_#_#_#_#_Object
  (toString [coll]
    (pr-str* coll))
  (equiv [this other]
    (-equiv this other))
  (indexOf [coll x]
    (-indexOf coll x 0))
  (indexOf [coll x start]
    (-indexOf coll x start))
  (lastIndexOf [coll x]
    (-lastIndexOf coll x (count coll)))
  (lastIndexOf [coll x start]
    (-lastIndexOf coll x start))

  IList

  ICloneable
  (-clone [_] (EmptyList. meta))

  #_#_IWithMeta
  (-with-meta [coll new-meta]
    (if (identical? new-meta meta)
      coll
      (EmptyList. new-meta)))

  #_#_IMeta
  (-meta [coll] meta)

  ISeq
  (-first [coll] nil)
  (-rest [coll] ())

  INext
  (-next [coll] nil)

  IStack
  (-peek [coll] nil)
  (-pop [coll] (throw (UnimplementedError. "Can't pop empty list")))

  ICollection
  (-conj [coll o] (List. meta o nil 1 nil))

  #_#_IEmptyableCollection
  (-empty [coll] coll)

  ISequential

  #_#_IEquiv
  (-equiv [coll other]
    (if (or (list? other)
            (sequential? other))
      (nil? (seq other))
      false))

  #_#_IHash
  (-hash [coll] empty-ordered-hash)

  ISeqable
  (-seq [coll] nil)

  ICounted
  (-count [coll] 0)

  #_#_#_IReduce
  (-reduce [coll f] (seq-reduce f coll))
  (-reduce [coll f start] (seq-reduce f start coll)))

(def ^IList empty-list (EmptyList. nil))

(defn list
  "Creates a new list containing the items."
  [& xs]
  (let [arr (if (and (dart-is? xs IndexedSeq) (zero? (.-i xs)))
              (.-arr xs)
              xs
              ;; TODO : for now, xs is a pure dart array, change when it will be a seq
              #_(let [arr #dart []]
                (loop [xs xs]
                  (if-not (nil? xs)
                    (do
                      (.push arr (-first xs))
                      (recur (-next xs)))
                    arr))))]
    (loop [i (alength arr) r ()]
      (if (> i 0)
        (recur (dec i) (-conj r (aget arr (dec i))))
        r))))

(deftype Cons [meta first rest ^:mutable __hash]
  #_#_#_#_#_#_#_Object
  (toString [coll]
    (pr-str* coll))
  (equiv [this other]
    (-equiv this other))
  (indexOf [coll x]
    (-indexOf coll x 0))
  (indexOf [coll x start]
    (-indexOf coll x start))
  (lastIndexOf [coll x]
    (-lastIndexOf coll x (count coll)))
  (lastIndexOf [coll x start]
    (-lastIndexOf coll x start))

  IList

  ICloneable
  (-clone [_] (Cons. meta first rest __hash))

  #_#_IWithMeta
  (-with-meta [coll new-meta]
    (if (identical? new-meta meta)
      coll
      (Cons. new-meta first rest __hash)))

  #_#_IMeta
  (-meta [coll] meta)

  ASeq
  ISeq
  (-first [coll] first)
  (-rest [coll] (if (nil? rest) () rest))

  INext
  (-next [coll]
    (if (nil? rest) nil (seq rest)))

  ICollection
  (-conj [coll o] (Cons. nil o coll nil))

  #_#_IEmptyableCollection
  (-empty [coll] (.-EMPTY List))

  ISequential

  #_#_IEquiv
  (-equiv [coll other] (equiv-sequential coll other))

  #_#_IHash
  (-hash [coll] (caching-hash coll hash-ordered-coll __hash))

  ISeqable
  (-seq [coll] coll)

  #_#_#_IReduce
  (-reduce [coll f] (seq-reduce f coll))
  (-reduce [coll f start] (seq-reduce f start coll)))

(defn cons
  "Returns a new seq where x is the first element and coll is the rest."
  [x coll]
  (cond
    (nil? coll)          (List. nil x nil 1 nil)
    (dart-is? coll ISeq) (Cons. nil x coll nil)
    true                 (Cons. nil x (seq coll) nil)))

(defn spread
  [arglist]
  (when-not (nil? arglist)
    (let [n (next arglist)]
      (if (nil? n)
        (seq (first arglist))
        (cons (first arglist)
              (spread n))))))

(defn list*
  ([args] (seq args))
  ([a args] (cons a args))
  ([a b args] (cons a (cons b args)))
  ([a b c args] (cons a (cons b (cons c args))))
  ([a b c d & more]
   (cons a (cons b (cons c (cons d (spread more)))))))

#_(defmacro lazy-seq
  "Takes a body of expressions that returns an ISeq or nil, and yields
  a ISeqable object that will invoke the body only the first time seq
  is called, and will cache the result and return it on all subsequent
  seq calls."
  [& body]
    `(new cljd.core/LazySeq nil (fn [] ~@body) nil nil))

(defn ^{:tag "dc.List"} to-array
  [coll]
  ;; TODO : reconsider #dart[] as it can only be used for one type
  ;; TODO : use count?
  (let [ary #dart []]
    (loop [s (seq coll)]
      (if-not (nil? s)
        (do (.add ary (first s))
            (recur (next s)))
        ary))))

(defn apply
  ([f args]
   (if (dart-is? f IFn)
     (-apply f (seq args))
     (.apply Function f (to-array args))))
  ([f x args]
   (let [args (list* x args)]
     (if (dart-is? f IFn)
       (-apply f args)
       (.apply Function f (to-array args)))))
  ([f x y args]
   (let [args (list* x y args)]
     (if (dart-is? f IFn)
       (-apply f args)
       (.apply Function f (to-array args)))))
  ([f x y z args]
   (let [args (list* x y z args)]
     (if (dart-is? f IFn)
       (-apply f args)
       (.apply Function f (to-array args)))))
  ([f a b c d & args]
   (let [args (cons a (cons b (cons c (cons d (spread args)))))]
     (if (dart-is? f IFn)
       (-apply f args)
       (.apply Function f (to-array args))))))

#_(defn ^macro defmacro [&form &env name args body] `(defn ~(vary-meta name assoc :macro true) args ~@body))

#_(defn make-array
  ([size]
     (dc/List. size))
  ([type size]
   (make-array size))
  ([type size & more-sizes]
    (let [dims more-sizes
          dimarray (make-array size)]
      (dotimes [i (alength dimarray)]
        (aset dimarray i (apply make-array nil dims)))
      dimarray)))

;; bit-shift-left - bit-shift-right-zero-fill

;; PersistentVector

(defn bit-shift-left [x n] (. x "<<" n))

(defn unsigned-bit-shift-right [x n] (. x ">>" n))

;; TODO : multi-arity
(defn bit-and [x y] (. x "&" y))

(deftype VectorNode [edit arr])

(defn- pv-fresh-node [edit]
  (VectorNode. edit (dc/List. 32)))

(defn- pv-aget [node idx]
  (.elementAt (.-arr node) idx))

(defn- pv-aset [node idx val]
  (. (.-arr node) "[]=" idx val)
  val)

(defn pv-clone-node [node]
  (VectorNode. (.-edit node) (aclone (.-arr node))))

(defn- tail-off [pv]
  (let [cnt (.-cnt pv)]
    (if (< cnt 32)
      0
      (bit-shift-left (unsigned-bit-shift-right (dec cnt) 5) 5))))

(defn- new-path [edit level node]
  (loop [ll level
         ret node]
    (if (zero? ll)
      ret
      (let [r (pv-fresh-node edit)]
        (pv-aset r 0 ret)
        (recur (- ll 5) r)))))

(defn unchecked-array-for [pv i]
  (if (>= i (tail-off pv))
    (.-tail pv)
    (loop [node (.-root pv)
           level (.-shift pv)]
      (if (pos? level)
        (recur (pv-aget node (bit-and (unsigned-bit-shift-right i level) 0x01f))
               (- level 5))
        (.-arr node)))))

(defn- array-for [pv i]
  (if (and (<= 0 i) (< i (.-cnt pv)))
    (unchecked-array-for pv i)
    (throw (UnimplementedError. (str "No item " i " in vector of length " (.-cnt pv))))))

(defn- push-tail [pv level parent tailnode]
  (let [ret (pv-clone-node parent)
        subdix (bit-and (unsigned-bit-shift-right (dec (.-cnt pv)) level) 0x01f)]
    (if (= 5 level)
      (do
        (pv-aset ret subdix tailnode)
        ret)
      (let [child (pv-aget parent subdix)]
        (if-not (nil? child)
          (let [node-to-insert (push-tail pv (- level 5) child tailnode)]
            (pv-aset ret subdix node-to-insert)
            ret)
          (let [node-to-insert (new-path nil (- level 5) tailnode)]
            (pv-aset ret subdix node-to-insert)
            ret))))))

(defn- do-assoc [pv level node i val]
  (let [ret (pv-clone-node node)]
    (if (zero? level)
      (do
        (pv-aset ret (bit-and i 0x01f) val)
        ret)
      (let [subidx (bit-and (unsigned-bit-shift-right i level) 0x01f)]
        (pv-aset ret subidx (do-assoc pv (- level 5) (pv-aget node subidx) i val))
        ret))))

(defn nth
  ([coll n]
   (cond
     (not (dart-is? n int))
     (throw (UnimplementedError. "Index argument to nth must be a number"))

     (nil? coll)
     coll

     (dart-is? coll IIndexed)
     (-nth coll n)

     (or (dart-is? coll dc/List) (dart-is? coll dc/String))
     (if (and (< -1 n) (< n (.-length coll)))
       (aget coll n)
       (throw (UnimplementedError. "Index out of bounds")))

     true (throw (UnimplementedError. "UnimplementedError nth"))

     #_#_#_#_#_#_(or (implements? ISeq coll)
         (implements? ISequential coll))
     (if (neg? n)
       (throw (UnimplementedError. "Index out of bounds"))
       (linear-traversal-nth coll n))

     (native-satisfies? IIndexed coll)
     (-nth coll n)

     :else
     (throw (js/Error. (str "nth not supported on this type "
                            (type->str (type coll)))))))
  ([coll n not-found]
   (cond
     (not (dart-is? n int))
     (throw (UnimplementedError. "Index argument to nth must be a number"))

     (nil? coll)
     not-found

     (dart-is? coll IIndexed)
     (-nth coll n not-found)

     (or (dart-is? coll dc/List) (dart-is? coll dc/String))
     (if (and (< -1 n) (< n (.-length coll)))
       (aget coll n)
       not-found)

     true (throw (UnimplementedError. "UnimplementedError nth"))

     #_#_#_#_#_#_(or (implements? ISeq coll)
         (implements? ISequential coll))
     (if (neg? n)
       not-found
       (linear-traversal-nth coll n not-found))

     (native-satisfies? IIndexed coll)
     (-nth coll n not-found)

     :else
     (throw (js/Error. (str "nth not supported on this type "
                            (type->str (type coll))))))))

(defprotocol APersistentVector
  "Marker protocol")

(defn- first-array-for-longvec [pv]
  (loop [node (.-root pv)
         level (.-shift pv)]
    (if (pos? level)
      (recur (pv-aget node 0) (- level 5))
      (.-arr node))))

(def ^:clj chunked-seq nil)

(defn- pop-tail [pv level node]
  (let [subidx (bit-and (unsigned-bit-shift-right (- (.-cnt pv) 2) level) 0x01f)]
    (cond
      (> level 5) (let [new-child (pop-tail pv (- level 5) (pv-aget node subidx))]
                    (if (and (nil? new-child) (zero? subidx))
                      nil
                      (let [ret (pv-clone-node node)]
                        (pv-aset ret subidx new-child)
                        ret)))
      (zero? subidx) nil
      true (let [ret (pv-clone-node node)]
             (pv-aset ret subidx nil)
             ret))))

(def ^VectorNode empty-vector-node (VectorNode. nil (dc/List. 32)))
(def empty-persistent-vector nil)

(deftype PersistentVector [meta cnt shift root tail ^:mutable __hash]
  #_#_#_#_#_#_#_Object
  (toString [coll]
    (pr-str* coll))
  (equiv [this other]
    (-equiv this other))
  (indexOf [coll x]
    (-indexOf coll x 0))
  (indexOf [coll x start]
    (-indexOf coll x start))
  (lastIndexOf [coll x]
    (-lastIndexOf coll x (count coll)))
  (lastIndexOf [coll x start]
    (-lastIndexOf coll x start))

  ICloneable
  (-clone [_] (PersistentVector. meta cnt shift root tail __hash))

  #_#_IWithMeta
  (-with-meta [coll new-meta]
    (if (identical? new-meta meta)
      coll
      (PersistentVector. new-meta cnt shift root tail __hash)))

  #_#_IMeta
  (-meta [coll] meta)

  IStack
  (-peek [coll]
    (when (> cnt 0)
      (-nth coll (dec cnt))))
  (-pop [coll]
    (cond
      (zero? cnt) (throw (UnimplementedError. "Can't pop empty vector"))
      (= 1 cnt) empty-persistent-vector ;; TODO : with-meta...
      (< 1 (- cnt (tail-off coll)))
      (PersistentVector. meta (dec cnt) shift root (doto #dart []
                                                     (.addAll (.sublist tail 0 (dec (alength tail))))) nil)
      :else (let [new-tail (unchecked-array-for coll (- cnt 2))
                  nr (pop-tail coll shift root)
                  new-root (if (nil? nr) empty-vector-node nr)
                  cnt-1 (dec cnt)]
              (if (and (< 5 shift) (nil? (pv-aget new-root 1)))
                (PersistentVector. meta cnt-1 (- shift 5) (pv-aget new-root 0) new-tail nil)
                (PersistentVector. meta cnt-1 shift new-root new-tail nil)))))

  ICollection
  (-conj [coll o]
    (if (< (- cnt (tail-off coll)) 32)
      ;; Check if it's better to fix dc/List length and iterate over old tail
      (let [new-tail (.from dc/List tail .& :growable true)]
        (.add new-tail o)
        (PersistentVector. meta (inc cnt) shift root new-tail nil))
      (let [root-overflow? (> (unsigned-bit-shift-right cnt 5) (bit-shift-left 1 shift))
            new-shift (if root-overflow? (+ shift 5) shift)
            new-root (if root-overflow?
                       (let [n-r (pv-fresh-node nil)]
                           (pv-aset n-r 0 root)
                           (pv-aset n-r 1 (new-path nil shift (VectorNode. nil tail)))
                           n-r)
                       (push-tail coll shift root (VectorNode. nil tail)))]
        (PersistentVector. meta (inc cnt) new-shift new-root #dart [o] nil))))

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
      (<= cnt 32) (IndexedSeq. tail 0 nil)
      true (chunked-seq coll (first-array-for-longvec coll) 0 0)))

  ICounted
  (-count [coll] cnt)

  IIndexed
  (-nth [coll n]
    (aget (array-for coll n) (bit-and n 0x01f)))
  (-nth [coll n not-found]
    (if (and (<= 0 n) (< n cnt))
      (aget (unchecked-array-for coll n) (bit-and n 0x01f))
      not-found))

  ILookup
  (-lookup [coll k] (-lookup coll k nil))
  (-lookup [coll k not-found] (if (dart-is? k dc/int)
                                (-nth coll k not-found)
                                not-found))

  IAssociative
  (-assoc [coll k v]
    (if (dart-is? k int)
      (-assoc-n coll k v)
      (throw (UnimplementedError. "Vector's key for assoc must be a number."))))
  (-contains-key? [coll k]
    (if (dart-is? k int)
      (and (<= 0 k) (< k cnt))
      false))

  #_#_IFind
  (-find [coll n]
    (when (and (<= 0 n) (< n cnt))
      (MapEntry. n (aget (unchecked-array-for coll n) (bit-and n 0x01f)) nil)))

  APersistentVector
  IVector
  (-assoc-n [coll n val]
    (cond
       (and (<= 0 n) (< n cnt))
       (if (<= (tail-off coll) n)
         (let [new-tail (aclone tail)]
           (aset new-tail (bit-and n 0x01f) val)
           (PersistentVector. meta cnt shift root new-tail nil))
         (PersistentVector. meta cnt shift (do-assoc coll shift root n val) tail nil))
       (= n cnt) (-conj coll val)
       true (throw (UnimplementedError. "Message that do no suck."))))

  #_#_#_IReduce
  (-reduce [v f]
    (pv-reduce v f 0 cnt))
  (-reduce [v f init]
    (loop [i 0 init init]
      (if (< i cnt)
        (let [arr  (unchecked-array-for v i)
              len  (alength arr)
              init (loop [j 0 init init]
                     (if (< j len)
                       (let [init (f init (aget arr j))]
                         (if (reduced? init)
                           init
                           (recur (inc j) init)))
                       init))]
          (if (reduced? init)
            @init
            (recur (+ i len) init)))
        init)))

  #_#_IKVReduce
  (-kv-reduce [v f init]
    (loop [i 0 init init]
      (if (< i cnt)
        (let [arr  (unchecked-array-for v i)
              len  (alength arr)
              init (loop [j 0 init init]
                     (if (< j len)
                       (let [init (f init (+ j i) (aget arr j))]
                         (if (reduced? init)
                           init
                           (recur (inc j) init)))
                       init))]
          (if (reduced? init)
            @init
            (recur (+ i len) init)))
        init)))

  IFn
  (-invoke [coll k]
    (-nth coll k))
  (-invoke [coll k not-found]
    (-nth coll k not-found))

  #_#_IEditableCollection
  (-as-transient [coll]
    (TransientVector. cnt shift (tv-editable-root root) (tv-editable-tail tail)))

  #_#_IReversible
  (-rseq [coll]
    (when (pos? cnt)
      (RSeq. coll (dec cnt) nil)))

  #_#_IIterable
  (-iterator [this]
    (ranged-iterator this 0 cnt)))

(def ^PersistentVector empty-persistent-vector (PersistentVector. nil 0 5 empty-vector-node #dart[] nil))

;; Transient should go here


;; PersistentQueue

(defn ^bool some? [x] (not (nil? x)))

(defn conj
  ([] [])
  ([coll] coll)
  ([coll x]
   (if-not (nil? coll)
     (-conj coll x)
     (list x)))
  ([coll x & xs]
   (if xs
     (recur (conj coll x) (first xs) (next xs))
     (conj coll x))))

(deftype PersistentQueueSeq [meta front rear ^:mutable __hash]
  #_#_#_#_#_#_#_Object
  (toString [coll]
    (pr-str* coll))
  (equiv [this other]
    (-equiv this other))
  (indexOf [coll x]
    (-indexOf coll x 0))
  (indexOf [coll x start]
    (-indexOf coll x start))
  (lastIndexOf [coll x]
    (-lastIndexOf coll x (count coll)))
  (lastIndexOf [coll x start]
    (-lastIndexOf coll x start))

  #_#_IWithMeta
  (-with-meta [coll new-meta]
    (if (identical? new-meta meta)
      coll
      (PersistentQueueSeq. new-meta front rear __hash)))

  #_#_IMeta
  (-meta [coll] meta)

  ISeq
  (-first [coll] (first front))
  (-rest  [coll]
    (if-let [f1 (next front)]
      (PersistentQueueSeq. meta f1 rear nil)
      (if (nil? rear)
        (throw (UnimplementedError. "Need to implement -empty"))#_(-empty coll)
        (PersistentQueueSeq. meta rear nil nil))))

  INext
  (-next [coll]
    (if-let [f1 (next front)]
      (PersistentQueueSeq. meta f1 rear nil)
      (when (some? rear)
        (PersistentQueueSeq. meta rear nil nil))))

  ICollection
  (-conj [coll o] (cons o coll))

  #_#_IEmptyableCollection
  (-empty [coll] (-with-meta (.-EMPTY List) meta))

  ISequential
  #_#_IEquiv
  (-equiv [coll other] (equiv-sequential coll other))

  #_#_IHash
  (-hash [coll] (caching-hash coll hash-ordered-coll __hash))

  ISeqable
  (-seq [coll] coll))

(deftype PersistentQueue [meta count front rear ^:mutable __hash]
  #_#_#_#_#_#_#_Object
  (toString [coll]
    (pr-str* coll))
  (equiv [this other]
    (-equiv this other))
  (indexOf [coll x]
    (-indexOf coll x 0))
  (indexOf [coll x start]
    (-indexOf coll x start))
  (lastIndexOf [coll x]
    (-lastIndexOf coll x (count coll)))
  (lastIndexOf [coll x start]
    (-lastIndexOf coll x start))

  ICloneable
  (-clone [coll] (PersistentQueue. meta count front rear __hash))

  #_#_IIterable
  (-iterator [coll]
    (PersistentQueueIter. front (-iterator rear)))

  #_#_IWithMeta
  (-with-meta [coll new-meta]
    (if (identical? new-meta meta)
      coll
      (PersistentQueue. new-meta count front rear __hash)))

  #_#_IMeta
  (-meta [coll] meta)

  ISeq
  (-first [coll] (first front))
  (-rest [coll] (rest (seq coll)))

  IStack
  (-peek [coll] (first front))
  (-pop [coll]
    (if front
      (if-let [f1 (next front)]
        (PersistentQueue. meta (dec count) f1 rear nil)
        (PersistentQueue. meta (dec count) (seq rear) [] nil))
      coll))

  ICollection
  (-conj [coll o]
    (if front
      (PersistentQueue. meta (inc count) front (conj (or rear []) o) nil)
      (PersistentQueue. meta (inc count) (conj front o) [] nil)))

  #_#_IEmptyableCollection
  (-empty [coll] (-with-meta (.-EMPTY PersistentQueue) meta))

  ISequential
  #_#_IEquiv
  (-equiv [coll other] (equiv-sequential coll other))

  #_#_IHash
  (-hash [coll] (caching-hash coll hash-ordered-coll __hash))

  ISeqable
  (-seq [coll]
    (let [rear (seq rear)]
      (if (or front rear)
        (PersistentQueueSeq. nil front (seq rear) nil))))

  ICounted
  (-count [coll] count))



(defn main []
  #_(let [a (LazySeq. nil (fn [] #dart [1 2 3 4]) nil nil)]
    (print (first (seq a)))
    (print (rest (seq a)))
    (print (last "coucou ma copine")))

  (let [a (PersistentVector. nil 0 5 (VectorNode. nil (dc/List. 32)) #dart [] nil)]
    (let [v (loop [v a
                   idx 0]
              (if (< idx 30)
                (recur (-conj v idx) (inc idx))
                v))]
      (print (-pop v))
      (print (-peek v))))

  (let [l (list 1 2 3 4 5)]
    (print (-pop l))
    (print (-peek l)))

  (let [queue (PersistentQueue.  nil 0 nil #dart [] nil)
        queue'
        (loop [v queue
               idx 0]
          (if (< idx 30)
            (recur (-conj v idx) (inc idx))
            v))]
    (print (first (-pop (-pop (-pop queue')))))
    (print (-peek queue')))

  )
 )
