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

(defn < [a b] (.< a b))

(defn pos? [a] (.< 0 a))

(defn + [a b] (.+ a b))

(defn - [a b] (.- a b))

(defn ^bool nil? [x] (.== nil x))

(defn ^:dart fib [n]
  (if (< n 2)
    1
    (+ (fib (- n 1)) (fib (- n 2)))))

(def toto (fn ([one two] one) ([one two & args] args)))


#_(def toto (fn ([a b c d e f g h i j k l] l) ([a b c d e f g h i j k l m n o & more] more)))


#_(def tata (fn ([one two] one) ([one two args] args)))

#_(def titi (fn ([& args] args)))

(defn main []
  (print (toto 1 1))
  (print (toto 1 1 2 2))
  (print (^:dart toto 1 2 3 4))
  (print (^:dart toto 1 2))
  (print (-apply toto #dart [4 5 6 7]))
  (print (-apply toto #dart [8 9]))
  (print (fib 5)))

#_
(defprotocol ISeqable
  "Protocol for adding the ability to a type to be transformed into a sequence."
  (-seq [o] ; TODO metadata for nil?
   "Returns a seq of o, or nil if o is empty."))

#_
(deftype LazySeq [^:mutable f ^:mutable s]
  ISeqable
  (-seq [_]
    (if (nil? f)
      s
      (loop [sv (f)]
        (if (dart-is? LazySeq sv)
          )
        )
      )))

#_
(defn my-range [from to step]
  (when (if (pos? step) (< from to) (< to from))
    (cons step (lazy-seq (my-range (+ from to) to step)))))
