(ns cljd.core
  (:require ["dart:core" :as dc :refer [print]]))

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
  (-invoke-more [this a b c d e f g h i rest])
  (-apply [this more]))

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

(defn ^bool < [a b] (.< a b))

(defn ^bool > [a b] (.> a b))

(defn ^bool pos? [a] (.< 0 a))

(defn ^num + [a b] (.+ a b))

(defn ^num - [a b] (.- a b))

(defn ^num inc
  "Returns a number one greater than num."
  [x] (+ x 1))

(defn ^num dec [x] (- x 1))

(defn ^bool nil? [x] (.== nil x))

(defn ^bool not
  "Returns true if x is logical false, false otherwise."
  [x] (if x false true))

(def ^:dart first nil)

(def ^:dart next nil)

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

(def ^:dart apply nil)

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

  #_#_#_IIndexed
  (-nth [coll n]
    (let [i (+ n i)]
      (if (and (<= 0 i) (< i (alength arr)))
        (aget arr i)
        #_(throw (js/Error. "Index out of bounds")))))
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

  #_#_#_IStack
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

  #_#_#_IStack
  (-peek [coll] nil)
  (-pop [coll] (throw (js/Error. "Can't pop empty list")))

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

(defprotocol APersistentVector
  "Marker protocol")

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

  #_#_#_IStack
  (-peek [coll]
    (when (> cnt 0)
      (-nth coll (dec cnt))))
  (-pop [coll]
    (cond
     (zero? cnt) (throw (js/Error. "Can't pop empty vector"))
     (== 1 cnt) (-with-meta (.-EMPTY PersistentVector) meta)
     (< 1 (- cnt (tail-off coll)))
      (PersistentVector. meta (dec cnt) shift root (.slice tail 0 -1) nil)
      :else (let [new-tail (unchecked-array-for coll (- cnt 2))
                  nr (pop-tail coll shift root)
                  new-root (if (nil? nr) (.-EMPTY-NODE PersistentVector) nr)
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

  #_#_ISeqable
  (-seq [coll]
    (cond
      (zero? cnt) nil
      (<= cnt 32) (IndexedSeq. tail 0 nil)
      :else (chunked-seq coll (first-array-for-longvec coll) 0 0)))

  ICounted
  (-count [coll] cnt)

  #_#_#_IIndexed
  (-nth [coll n]
    (aget (array-for coll n) (bit-and n 0x01f)))
  (-nth [coll n not-found]
    (if (and (<= 0 n) (< n cnt))
      (aget (unchecked-array-for coll n) (bit-and n 0x01f))
      not-found))

  #_#_#_ILookup
  (-lookup [coll k] (-lookup coll k nil))
  (-lookup [coll k not-found] (if (number? k)
                                (-nth coll k not-found)
                                not-found))

  #_#_#_IAssociative
  (-assoc [coll k v]
    (if (number? k)
      (-assoc-n coll k v)
      (throw (js/Error. "Vector's key for assoc must be a number."))))
  (-contains-key? [coll k]
    (if (integer? k)
      (and (<= 0 k) (< k cnt))
      false))

  #_#_IFind
  (-find [coll n]
    (when (and (<= 0 n) (< n cnt))
      (MapEntry. n (aget (unchecked-array-for coll n) (bit-and n 0x01f)) nil)))

  APersistentVector
  #_#_IVector
  (-assoc-n [coll n val]
    (cond
       (and (<= 0 n) (< n cnt))
       (if (<= (tail-off coll) n)
         (let [new-tail (aclone tail)]
           (aset new-tail (bit-and n 0x01f) val)
           (PersistentVector. meta cnt shift root new-tail nil))
         (PersistentVector. meta cnt shift (do-assoc coll shift root n val) tail nil))
       (== n cnt) (-conj coll val)
       :else (throw (js/Error. (str "Index " n " out of bounds  [0," cnt "]")))))

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

  #_#_#_IFn
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

(defn main []
  #_(let [a (LazySeq. nil (fn [] #dart [1 2 3 4]) nil nil)]
    (print (first (seq a)))
    (print (rest (seq a)))
    (print (last "coucou ma copine")))

  (let [a (PersistentVector. nil 5 5 (VectorNode. nil (dc/List. 32)) #dart [1 2 3 4 5] nil)]
    (print (.-tail (-conj a 110)))
    (print (.-tail a)))

  )
