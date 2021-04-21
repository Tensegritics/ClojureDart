(ns cljd.core)

(definterface IProtocol
  (extension [x])
  (satisfies [x]))

(def ^:dart to-map)
(def ^:dart to-list)

(def empty-persistent-vector nil)

(def ^{:clj true} =)
(def ^{:clj true} assoc)
(def ^{:dart true} butlast)
(def ^{:clj true} concat)
(def ^{:clj true} conj)
#_(def ^{:dart true} cons)
(def ^{:dart true} contains?)
(def ^{:clj true} dissoc)
(def ^{:clj true} drop)
#_(def ^{:dart true} first)
(def ^{:clj true} gensym)
(def ^{:clj true} get)
(def ^{:dart true} ident?)
(def ^{:clj true} interleave)
#_(def ^{:dart true} inc)
(def ^{:dart true} key)
(def ^{:dart true} keys)
(def ^{:clj true} keyword)
(def ^{:dart true} keyword?)
(def ^{:dart true} last)
(def ^{:clj true} list)
(def ^{:clj true} mapcat)
(def ^{:dart true} map?)
(def ^{:dart true} meta)
(def ^{:dart true} name)
(def ^{:dart true} namespace)
#_(def ^{:dart true} next)
(def ^{:dart true} nnext)
(def ^{:clj true} partition)
(def ^{:dart true} second)
#_(def ^{:dart true} seq)
(def ^{:dart true} seq?)
(def ^{:dart true} set)
(def ^{:dart true} some)
#_(def ^{:clj true} str)
(def ^{:dart true} string?)
(def ^{:clj true} subvec)
(def ^{:clj true} symbol)
(def ^{:dart true} symbol?)
(def ^{:dart true} take-nth)
(def ^{:dart true} val)
(def ^{:clj true} vary-meta)
(def ^{:dart true} vec)
(def ^{:clj true} vector)
(def ^{:dart true} vector?)
(def ^{:dart true} with-meta)

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

(defn ^bool satisfies?
  {:inline (fn [protocol x] `(.satisfies ~protocol ~x))
   :inline-arities #{2}}
  [^IProtocol protocol x]
  (.satisfies protocol x))

(defn ^bool nil?
  {:inline-arities #{1}
   :inline (fn [x] `(.== nil ~x))}
  [x] (.== nil x))

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
       ~(when-not (:type-only opts)
          `(defn
             ~(symbol (str "->" class-name))
             [~@fields]
             (new ~class-name ~@fields))))))

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
  {:added "1.0"}
  [test & body]
  `(if ~test (do ~@body)))

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

;; TODO
(def empty-list nil)

#_(defn nth [x i default]
  (if (.< i (.-length x))
    (. x "[]" i)
    default))

(defprotocol ISeqable
  "Protocol for adding the ability to a type to be transformed into a sequence."
  (-seq [o]
    "Returns a seq of o, or nil if o is empty."))

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

(extend-type Null
  ISeqable
  (-seq [coll] nil))

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

(defn first
  "Returns the first item in the collection. Calls seq on its
   argument. If coll is nil, returns nil."
  [coll]
  (let [s (seq coll)]
    (when s (-first s))))

(defn next
  "Returns a seq of the items after the first. Calls seq on its
  argument.  If there are no more items, returns nil."
  [coll]
  #_(some-> (seq coll) -next)
  (let [s (seq coll)]
    (when s (-next s))))

(defn rest
  "Returns a possibly empty seq of the items after the first. Calls seq on its
  argument."
  [coll]
  (if (satisfies? ISeq coll)
    (-rest coll)
    #_(some-> (seq coll) -rest)
    (let [s (seq coll)]
      (when s (-rest s)))))

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

(defprotocol IReduce
  "Protocol for seq types that can reduce themselves.
  Called by cljs.core/reduce."
  (-reduce [coll f] [coll f start]
    "f should be a function of 2 arguments. If start is not supplied,
     returns the result of applying f to the first 2 items in coll, then
     applying f to that result and the 3rd item, etc."))

;; TODO handle Reduced
(extend-type fallback
  IReduce
  (-reduce [coll f]
    (if-some [[x & xs] (seq coll)]
      (if-some [[y & xs] xs]
        (-reduce f (f x y) xs)
        x)
      (f)))
  (-reduce [coll f start]
    (loop [acc start xs (seq coll)]
      (if-some [[x & xs] xs]
        (recur (f acc x) xs)
        acc))))

(defn reduce
  ([f coll] (-reduce coll f))
  ([f init coll] (-reduce coll f init)))

(defprotocol ICounted
  "Protocol for adding the ability to count a collection in constant time."
  (-count [coll]
    "Calculates the count of coll in constant time."))

(extend-type fallback
  ICounted
  (-count [coll]
    (reduce (fn [n _] (inc n)) 0 coll)))

(defn ^int count [coll]
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

(defprotocol IEquiv
  "Protocol for adding value comparison functionality to a type."
  (-equiv [o other]
   "Returns true if o and other are equal, false otherwise."))

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

(defprotocol IWithMeta
  "Protocol for adding metadata to an object."
  (-with-meta [o meta]
    "Returns a new object with value of o and metadata meta added to it."))

(defprotocol IMeta
  "Protocol for accessing the metadata of an object."
  (-meta [o] "Returns the metadata of object o."))

(defprotocol IHash
  "Protocol for adding hashing functionality to a type."
  (-hash [o] "Returns the hash code of o."))

(defn ^bool pos? [a] (.< 0 a))

(def ^:clj ^:bootstrap apply #?(:cljd nil :clj clojure.core/apply))

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
             ([x y] `(bit-not (bit-and ~x ~y)))
             ([x y & more] (reduce (fn [a b] `(bit-not (bit-and ~a ~b))) `(bit-not (bit-and ~x ~y)) more)))
   :inline-arities >1?}
  ([x y] (bit-not (bit-and x y)))
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

(defn ^bool identical?
  ;; TODO inline does not work
  #_{:inline (fn [x y] `(dart:core/identical ~x ~y))
   :inline-arities #{2}}
  [x y]
  (dart:core/identical x y))

(defn ^bool true?
  #_{:inline (fn [x] `(dart:core/identical ~x true))
   :inline-arities #{1}}
  [x]
  (dart:core/identical x true))

(defn ^bool false?
  #_{:inline (fn [x] `(dart:core/identical ~x false))
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

(deftype Cons [meta first rest ^:mutable ^int __hash]
  Object
  (^String toString [coll] "TODO" #_(pr-str* coll))
  IList
  IWithMeta
  (-with-meta [coll new-meta]
    (if (identical? new-meta meta)
      coll
      (Cons. new-meta first rest __hash)))
  IMeta
  (-meta [coll] meta)
  ISeq
  (-first [coll] first)
  (-rest [coll] (if (nil? rest) empty-list rest))
  (-next [coll] (if (nil? rest) nil (seq rest)))
  ICollection
  (-conj [coll o] (Cons. nil o coll nil))
  #_#_IEmptyableCollection
  (-empty [coll] empty-list)
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

(deftype PersistentList [meta first rest ^int count ^:mutable ^int __hash]
  ;; invariant: first is nil when count is zero
  Object
  (^String toString [coll]
    ;; TODO
    #_(pr-str* coll))
  IList
  IWithMeta
  (-with-meta [coll new-meta]
    (if (identical? new-meta meta)
      coll
      (PersistentList. new-meta first rest count __hash)))
  IMeta
  (-meta [coll] meta)
  ISeq
  (-first [coll] first)
  (-rest [coll]
    (if (<= count 1)
      empty-list
      rest))
  (-next [coll]
    (if (<= count 1)
      nil
      rest))
  #_#_#_IStack
  (-peek [coll] first)
  (-pop [coll] (if (pos? count) rest (throw (js/Error. "Can't pop empty list"))))
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
  (-seq [coll] coll)
  ICounted
  (-count [coll] count)
  #_#_#_IReduce
  (-reduce [coll f] (seq-reduce f coll))
  (-reduce [coll f start] (seq-reduce f start coll)))

(def ^PersistentList empty-list (PersistentList. nil nil nil 0 -1))

(defn cons
  "Returns a new seq where x is the first element and coll is the rest."
  [x coll]
  (cond
    (nil? coll)            (PersistentList. nil x nil 1 -1)
    (satisfies? ISeq coll) (Cons. nil x coll -1)
    true                   (Cons. nil x (seq coll) -1)))

(def ^:dart seq-iterator nil)

(deftype IteratorSeq [value iter ^:mutable _rest]
  ISeqable
  (-seq [this] this)
  ISeq
  (-first [coll] value)
  (-rest [coll]
    (when (nil? _rest) (set! _rest (seq-iterator iter)))
    (if (nil? _rest) empty-list _rest))
  (-next [coll]
    (when (nil? _rest) (set! _rest (seq-iterator iter)))
    _rest))

(defn seq-iterator [^Iterator iter]
  (when (.moveNext iter)
    (IteratorSeq. (.-current iter) iter nil)))

(extend-type Iterable
  ISeqable
  (-seq [coll] (seq-iterator (.-iterator ^Iterable coll)))) ; TODO infer argument type in extend-type

(deftype StringSeq [string i meta ^:mutable ^int __hash]
  #_Object
  #_(^String toString [coll]
      (pr-str* coll))
  ISeqable
  (-seq [this] (when (< i (.-length string)) this))
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
      empty-list))
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
  ;; TODO handle reduced
  (-reduce [coll f]
    (let [l (.-length string)
          x (. string "[]" i)
          i' (inc i)]
      (if (< i' l)
        (loop [idx i' acc x]
          (if (< idx l)
            (recur (inc idx) (f acc (. string "[]" idx) ))
            acc))
        x)))
  (-reduce [coll f start]
    (let [l (.-length string)]
      (loop [acc start idx i]
        (if (< idx l)
          (recur (f acc (. string "[]" idx) ) (inc idx))
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

(defn ^bool not
  "Returns true if x is logical false, false otherwise."
  {:inline (fn [x] `(if ~x false true))
   :inline-arities #{1}}
  [x] (if x false true))

(deftype LazySeq [meta ^:mutable ^some fn ^:mutable s ^:mutable ^int __hash]
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
        (if (dart-is? ls LazySeq)
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
      empty-list))
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

;; chunks

(defn chunked-seq?
  "Return true if x satisfies IChunkedSeq."
  [x] (satisfies? IChunkedSeq x))

(deftype ArrayChunk [arr off end]
  ICounted
  (-count [_] (- end off))
  IIndexed
  (-nth [coll i]
    ;; TODO check out of bound exceptions
    (. arr "[]" (+ off i)))
  (-nth [coll i not-found]
    (if (< i 0)
      not-found
      (if (< i (- end off))
        (. arr "[]" (+ off i))
        not-found)))
  IChunk
  (-drop-first [coll]
    (if (== off end)
      (throw (ArgumentError. "-drop-first of empty chunk"))
      (ArrayChunk. arr (inc off) end)))
  IReduce
  (-reduce [coll f]
    ;; TODO check out of bound exceptions
    (let [x (. arr "[]" off)
          off' (inc off)]
      (if (< off' end)
        (loop [acc x idx off']
          (if (< idx end)
            (recur (f acc (. arr "[]" idx)) (inc idx))
            acc))
        x)))
  (-reduce [coll f start]
    (loop [acc start idx off]
      (if (< idx end)
        (recur (f acc (. arr "[]" idx)) (inc idx))
        acc))))

(defn array-chunk
  ([arr]
   (ArrayChunk. arr 0 (.-length arr)))
  ([arr off]
   (ArrayChunk. arr off (.-length arr)))
  ([arr off end]
   (ArrayChunk. arr off end)))

(deftype ChunkBuffer [^:mutable arr ^:mutable end]
  Object
  (add [_ o]
    (. arr "[]" end o)
    (set! end (inc end)))
  (chunk [_]
    (let [ret (ArrayChunk. arr 0 end)]
      (set! arr nil)
      ret))
  ICounted
  (-count [_] end))

(defn chunk-buffer [capacity]
  (ChunkBuffer. (.filled dart:core/List capacity nil) 0))

(deftype ChunkedCons [chunk more meta ^:mutable ^int __hash]
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
        empty-list
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
      empty-list
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
  ;; TODO : check when it is supposed to not be used in a chunk context
  (-chunked-next s)
  #_(if (implements? IChunkedNext s)
    (-chunked-next s)
    (seq (-chunked-rest s))))

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
    `(let [n# ^int ~n]
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
        (let [c (chunk-first s)
              size (count c)
              b (chunk-buffer size)]
          (dotimes [i size]
            (chunk-append b (f (-nth c i))))
          (chunk-cons (chunk b) (map f (chunk-rest s))))
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
        (let [c (chunk-first s)
              size (count c)
              b (chunk-buffer size)]
          (dotimes [i size]
            (let [v (-nth c i)]
              (when (pred v)
                (chunk-append b v))))
          (chunk-cons (chunk b) (filter pred (chunk-rest s))))
        (let [f (first s) r (rest s)]
          (if (pred f)
            (cons f (filter pred r))
            (filter pred r))))))))



#_(



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
  (^String toString [coll]
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
  (^String toString [coll]
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
  (^String toString [coll]
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


(defn main []

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
  (dart:core/print (reduce (fn [acc item] (+ acc item)) 0 #dart[1 2 3]))





  )
