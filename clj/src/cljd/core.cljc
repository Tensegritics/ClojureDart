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
    "Returns the first item in the collection coll. Used by cljs.core/first.")
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

  #_#_ICounted
  (-count [_]
    (max 0 (- (alength arr) i)))

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

  #_#_ICounted
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

  #_#_ICounted
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

(defn main []
  #_(let [a (LazySeq. nil (fn [] #dart [1 2 3 4]) nil nil)]
    (print (first (seq a)))
    (print (rest (seq a)))
    (print (last "coucou ma copine")))

  #_(let [sw (dc/Stopwatch.)
        v #dart []]
    (.start sw)
    (loop [i 0]
      (when (< i 10000000)
        (do (.add v i)
            (recur (inc i)))))
    (.stop sw)
    (print (.-elapsedMilliseconds sw))


    (.reset sw)
    (.start sw)
    (loop [i 0]
      (when (< i 10000000)
        (do (.elementAt v i)
            (recur (inc i)))))
    (.stop sw)
    (print (.-elapsedMilliseconds sw))
    )

  #_(let [a #dart [1 2 3 4 5]
        b (.from dc/List a)]
    (aset b 0 10)
    (print a)
    (print b))

  #_(let [sw (dc/Stopwatch.)
        v #dart []]
    (.start sw)
    (loop [i 0
           j 1000000]
      (when (< i 10000000)
        (< i j)
        (recur (inc i) j)))
    (.stop sw)
    (print (.-elapsedMilliseconds sw)))

  (let [a #dart [1 "ohoh"]
        b (aclone a)]
    (aset b 0 "ohoh")
    (print a)
    (print b))

  (let [a (seq #dart [1 2 3])
        b (-clone a)]

    (print (dc/identical a b))
    (print (dc/identical a a))
    (print (dc/identical b b)))





  )
