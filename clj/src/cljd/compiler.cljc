(ns cljd.compiler
  (:refer-clojure :exclude [macroexpand-all macroexpand-1])
  (:require [clojure.core :as cljhost]))

(defmacro ^:private else->> [& forms]
  `(->> ~@(reverse forms)))

(defn string-writer
  ([]
   #?(:clj (let[sb (StringBuilder.)]
             (fn
               ([] (.toString sb))
               ([x] (.append sb (str x)))))
      :cljd (let[sb (StringBuffer.)]
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

(defn macroexpand-1 [env form]
  (if-some [[f & args] (when (seq? form) (seq form))]
    (if (symbol? f)
      (let [name (name f)]
        ;; TODO add proper expansion here, before defaults
        (cond
          (= name ".") form
          (.endsWith name ".")
          (list* 'new
                 (symbol (namespace f) (subs name 0 (dec (count name))))
                 args)
          (.startsWith name ".")
          (list* '. (first args) (symbol (subs name 1)) (next args))
          :else form))
      form)
    form))

(defn macroexpand-all [env form]
  (let [ex (macroexpand-1 env form)]
    (cond->> ex (not (identical? ex form)) (recur env))))

(defn emit-symbol [sym env out!]
  (out! (or (env sym) #_(TODO resolve ns sym) #_"TODO next form should throw" (name sym))))

(defn replace-all [^String s regexp f]
  #?(:cljd
     (.replaceAllMapped s regexp f)
     :clj
     (clojure.string/replace s regexp f)))

(defn emit-string [s out!]
  (out! \")
  (out! (replace-all s #"([\x00-\x1f])|[$\"]"
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
  (out! \"))

(defn emit-expr [expr env out!]
  (cond
    (symbol? expr) (emit-symbol expr env out!)
    (string? expr) (emit-string expr out!)
    #?@(:clj [(char? expr) (emit-string (str expr) out!)])
    :else (out! expr)))

(def mungees (atom 0))

(deftype DartExpr [^String s]
  Object
  (toString [_] s))

(defn tmpvar [] (DartExpr. (str "_" (swap! mungees inc))))

(defn munge [sym env]
  (DartExpr. (if (env sym) (str (name sym) "__" (swap! mungees inc)) (name sym))))

(defn lift-expr [expr env out! & {:keys [name name-env force]}]
  (let [value (not (coll? expr))]
    (if (and value (nil? name) (not force))
      expr
      (let [varname (if name (munge name (or name-env env)) (tmpvar))]
        (if value
          (emit expr env out! (str "var " varname "="))
          (do
            (out! (str "var " varname ";\n"))
            (emit expr env out! (str varname "="))))
        varname))))

(defn emit-args [expr env out! locus]
  (let [args
        (loop [named false keypos false xs (seq expr) args []]
          (if-some [[x & xs] xs]
            (cond
              (= '& x)
              (recur true true xs args)
              (and named keypos)
              (recur named false xs (conj args x))
              :else
              (recur named true xs (conj args (lift-expr x env out!))))
            args))]
    (out! (str locus "("))
    (loop [comma false named false keypos false xs (seq args)]
      (when-some [[x & xs] xs]
        (else->>
         (if (= '& x) (recur comma true true xs))
         (do (when comma (out! ", ")))
         (if (and named keypos)
           (do
             (out! (str (name x) ": "))
             (recur false true false xs)))
         (do
           (emit-expr x env out!)
           (recur true named true xs)))))
    (out! ");\n")))

(defn emit-new [[_ class & args] env out! locus]
  (let [sb! (string-writer locus)]
    (emit-expr class env sb!)
    (emit-args args env out! (sb!))))

(defn emit-dot [[_ obj fld & args] env out! locus]
  (let [[_ prop name] (re-matches #"(-)?(.*)" (name fld))
        sb! (string-writer locus)]
    (emit-expr (lift-expr obj env out!) env sb!)
    (sb! (str "." name))
    (if prop
      (do
        (sb! ";\n")
        (out! (sb!)))
      (emit-args args env out! (sb!)))))

(defn emit [expr env out! locus]
  (let [expr (macroexpand-all env expr)]
    (if (seq? expr)
      (case (first expr)
        new (emit-new expr env out! locus)
        . (emit-dot expr env out! locus))
      (do
        (out! locus)
        (emit-expr expr env out!)
        (out! ";\n")))))

(comment
  (let [out! (string-writer)]
    (emit "hello" {} out! "return ")
    (emit 'sym {} out! "return ")
    (emit 42 {} out! "return ")
    (emit \x {} out! "return ")
    (emit '(Foo. 1 & :bleh 32) {} out! "return ")
    (emit '(Foo. 1 & :bleh (Bar.)) {} out! "return ")
    (emit '(.-fld a) {} out! "return ")
    (emit '(.meth a b c) {} out! "return ")
    (println (out!)))
  )
