(ns cljd.core
  (:require ["dart:core" :refer [print]]))

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

(defn < [a b] (.< a b))

(defn pos? [a] (.< 0 a))

(defn + [a b] (.+ a b))

(defn inc
  "Returns a number one greater than num."
  [x] (+ x 1))

(defn - [a b] (.- a b))

(defn ^bool nil? [x] (.== nil x))

(defn ^bool not
  "Returns true if x is logical false, false otherwise."
  [x] (if x false true))

(defn ^num alength [array] (.-length array))

(defn aget [array idx] (. array "[]" idx))

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

  #_#_ICloneable
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
               #_(list)))

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

  #_#_ICollection
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



#_(defn ^:dart fib [n]
  (if (< n 2)
    1
    (+ (fib (- n 1)) (fib (- n 2)))))



(defn main []
  (let [a (IndexedSeq. #dart [1 2 3] 0 nil)]
    (print (-first a)))
  )
