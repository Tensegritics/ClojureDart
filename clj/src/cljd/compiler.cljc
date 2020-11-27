(ns cljd.compiler
  (:refer-clojure :exclude [macroexpand-all macroexpand-1 munge load-file])
  (:require [clojure.core :as cljhost]))

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

(def nses (atom {:current-ns 'user}))

(defn do-def [nses sym m]
  (assoc-in nses [(:current-ns nses) sym] m))

(defn macroexpand-1 [env form]
  (if-let [[f & args] (and (seq? form) (symbol? (first form)) form)]
    (let [name (name f)
          #?@(:clj [clj-var (ns-resolve (find-ns (:current-ns @nses)) f)])]
      ;; TODO add proper expansion here, before defaults
      (cond
        (env f) form
        (= '. f) form
        (= 'ns f) form
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
    (nil? expr) (out! "null")
    :else (out! expr)))

(def mungees (atom 0))

(deftype DartExpr [^String s]
  Object
  (toString [_] s))

(defn tmpvar [] (DartExpr. (str "_" (swap! mungees inc))))

(defn munge [sym env]
  (DartExpr. (if (env sym) (str (name sym) "__" (swap! mungees inc)) (name sym))))

(declare emit)

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

(defn emit-body [body env out! locus]
  (loop [body body]
    (when-some [[x & xs] body]
      (if xs
        (do (emit x env out! "") (recur xs))
        (emit x env out! locus)))))

(defn emit-let [[_ bindings & body] env out! locus]
  (let [env (loop [env env bindings (partition 2 bindings)]
              (if-some [[sym val] (first bindings)]
                (recur (assoc env sym (lift-expr val env out! :name sym)) (next bindings))
                env))]
    (emit-body body env out! locus)))

(defn emit-if [[_ test then else] env out! locus]
  (let [test (lift-expr test env out! :force true)]
    (out! "if (")
    (emit-expr test env out!)
    (out! "!= null && false != ")
    (emit-expr test env out!)
    (out! ") {\n")
    (emit then env out! locus)
    (out! "}else{\n")
    (emit else env out! locus)
    (out! "}\n")))

(defn emit-loop [[_ bindings & body] env out! locus]
  (let [env (loop [env env bindings (partition 2 bindings)]
              (if-some [[sym val] (first bindings)]
                (let [varname (lift-expr val env out! :name sym)]
                  (recur (-> env
                             (assoc sym varname)
                             (update ::loop-bindings (fnil conj []) varname))
                         (next bindings)))
                env))]
    (out! "do {\n")
    (emit-body body env out! locus)
    (out! "break;\n} while(true);\n")))

(defn emit-recur [[_ & expr] env out! locus]
  (let [sb! (string-writer)]
    (doseq [[expr binding] (map vector expr (::loop-bindings env))]
      (emit (lift-expr expr env out! :force true) env sb! (str binding "=")))
    (out! (sb!))
    (out! "continue;\n")))

(defn emit-quoted [body env out!]
  (cond
    (map? body)
    (do (out! "{")
        (doseq [[k v] body]
          (emit-quoted k env out!)
          (out! ": ")
          (emit-quoted v env out!)
          (out! ", "))
        (out! "}"))
    (coll? body)
    (do (out! (cond (list? body) "List" (set? body) "Set" :else "Vector"))
        (out! ".from([")
        (doseq [e body]
          (emit-quoted e env out!)
          (out! ", "))
        (out! "])"))
    (symbol? body)
    (out! (str "Symbol(null, " (name body) ")"))
    :else (emit-expr body env out!)))

(defn emit-collection [body env out! locus]
  (let [sb! (string-writer locus)]
    (if (map? body)
      (do (sb! "{")
          (doseq [[k v] body]
            (if (not (coll? k)) (emit-expr k env sb!) (sb! (lift-expr k env out!)))
            (sb! ": ")
            (if (not (coll? v)) (emit-expr v env sb!) (sb! (lift-expr v env out!)))
            (sb! ", "))
          (sb! "}"))
      (do (sb! (if (set? body) "Set" "PersistentVector"))
          (sb! ".from([")
          (doseq [e body]
            (if (not (coll? e)) (emit-expr e env sb!) (sb! (lift-expr e env out!)))
            (sb! ", "))
          (sb! "])")))
    (out! (sb!))
    (out! ";\n")))

(defn emit-body [expr env out! locus]
  (doseq [body (butlast expr)]
    (emit body env out! ""))
  (emit (last expr) env out! locus))

(defn emit-bodies [bodies env out!]
  (let [bodies (sort #(compare (count (first %1)) (count (first %2))) bodies)
        is-variadic (some #{'&} (first (last bodies)))
        smallest-arity (if (and (= 1 (count bodies)) is-variadic) (- (count (ffirst bodies)) 2) (count (ffirst bodies)))
        biggest-arity (count (first (last bodies)))]
    (out! "(")
    (let [params-alias (into [] (map #(let [tmpparam (tmpvar)]
                                        (when (< 0 %) (out! ","))
                                        (when (= % smallest-arity) (out! "["))
                                        (out! tmpparam)
                                        (when (>= % smallest-arity) (out! "=MISSING_ARG"))
                                        tmpparam))
                             (range (if is-variadic 8 biggest-arity)))]
      (when (or (< 1 (count bodies)) is-variadic) (out! "]"))
      (out! ") {\n")
      (let [e (fn [[params & body] env out!]
                (let [[params [_ vararg-param]] (split-with (complement #{'&}) params)
                      bodyenv (reduce (fn [bodyenv [param alias]]
                                        (let [varname (lift-expr alias env out! :name param :name-env bodyenv)]
                                          (-> bodyenv
                                              (assoc param varname)
                                              (update ::loop-bindings (fnil conj []) varname))))
                                      env
                                      (map vector params params-alias))
                      bodyenv (if vararg-param
                                (let [varname (lift-expr (DartExpr. (str "[" (clojure.string/join "," (subvec params-alias (count params))) "].takeWhile((e) => e != MISSING_ARG).toList()")) env out! :name vararg-param :name-env bodyenv)]
                                  (-> bodyenv
                                      (assoc vararg-param varname)
                                      (update ::loop-bindings (fnil conj []) varname)))
                                bodyenv)]
                  (out! "do {\n")
                  (emit-body body bodyenv out! "return ")
                  (out! "break;\n} while(true);\n}\n")))]
        (doseq [body (butlast bodies)]
          (out! (str "if (" (nth params-alias (count (first body))) " == MISSING_ARG) {\n"))
          (e body env out!))
        (e (last bodies) env out!)))))

(defn emit-fn [[_ & sigs] env out! locus]
  (let [named (symbol? (first sigs))
        body (else->> (let [body (if named (next sigs) sigs)])
                      (if (vector? (first body)) (list body) body))]
    (if-some [fnname (when named (first sigs))]
      (let [munged (munge fnname env)]
        (out! munged)
        (emit-bodies body (assoc env fnname munged) out!)
        (out! (str locus munged)))
      (do (out! locus)
          (emit-bodies body env out!)))
    (out! ";\n")))

(defn emit-fn-call [[fnname & args] env out! locus]
  (let [sb! (string-writer locus)]
    (emit-expr (lift-expr fnname env out!) env sb!)
    (emit-args args env out! (sb!))))

(defn extract-bodies [fn-expr]
  (let [bodies (if (symbol? (second fn-expr)) (nnext fn-expr))]
    (if (vector? (first bodies)) [bodies] bodies)))

(defn emit-top-fn [sym fn-expr env out!]
  (let [env (cond-> env (symbol? (second fn-expr)) (assoc (second fn-expr) (name sym)))]
    (emit-symbol sym {} out!)
    (emit-bodies (extract-bodies fn-expr) env out!)
    (out! "\n")))

(defn emit-def [[_ sym expr] env out! locus]
  (let [expr (macroexpand-all env expr)
        sb! (string-writer)]
    (if (and (seq? expr) (= 'fn* (first expr)))
      (do
        (emit-top-fn sym expr env sb!)
        (swap! nses do-def sym {:type :dartfn, :code (sb!)}))
      (do
        (emit-symbol sym {} sb!)
        (sb! "=")
        (if (coll? expr)
          (do
            (sb! "(){\n")
            (emit expr env sb! "return ")
            (sb! "}()"))
          (emit-expr expr env sb!))
        (sb! ";\n")
        (swap! nses do-def sym {:type :field, :code (sb!)})))
    (emit sym env out! locus)))

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


  (:require ["package:mobx/mobx.dart" :as mobx])
  =>
  {:imports [["package:mobx/mobx.dart" "mobx1"]]
   :aliases {"mobx" "mobx1"}}

  )

;; once it's done symbol resolution (in emit-symbol) must take them into account
;; I doubt it's enough -- we'll have to see how it interacts with macroexpansion for example.

(defn do-ns [[_ ns-sym & ns-clauses]]
  (let [ns-clauses (drop #(or (string? %) (map? %)) ns-clauses) ; drop doc and meta for now
        #_(transduce
          (comp
            (mapcat (fn [[directive & args]] (map #(vector directive %) args)))
            (map
              (fn [[directive arg]]
                (case directive
                 :require
                 (let [[arg & {:keys [refer as]}] (if (vector? args) args [args])]
                   )
                 :import (/ 0)
                 :refer-clojure (/ 0)
                 :use (/ 0)
                 ))))
          (partial merge-with concat)
          {} ns-clauses)])
  (swap! nses assoc :current-ns (doto (second expr)
                                  #?@(:clj [create-ns]))) ; hacky)

(defn emit [expr env out! locus]
  (let [expr (macroexpand-all env expr)]
    (cond
      (seq? expr)
      (case (first expr)
        ns (do-ns expr)
        new (emit-new expr env out! locus)
        . (emit-dot expr env out! locus)
        let* (emit-let expr env out! locus)
        if (emit-if expr env out! locus)
        loop* (emit-loop expr env out! locus)
        recur (emit-recur expr env out! locus)
        quote (do (out! locus) (emit-quoted (second expr) env out!) (out! ";\n"))
        fn* (emit-fn expr env out! locus)
        def (emit-def expr env out! locus)
        (emit-fn-call expr env out! locus))
      (coll? expr) (emit-collection expr env out! locus)
      :else (do (out! locus) (emit-expr expr env out!) (out! ";\n")))))

(defn dump-ns [ns-map out!]
  (doseq [[lib alias] (:imports ns-map)]
    (out! "import ")
    (emit-string lib out!)
    (out! " as ")
    (out! alias)
    (out! ";\n"))

  (doseq [[sym {:keys [type code]}] ns-map]
    (case type
      :class 'TODO
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

            ['(ns cljd.bordeaux) ; nrepl or cider grrrrr
             '(def x 33)
             '(ns cljd.user)
             '(def x 42)
             '(def foo (fn foo [x] x))
             '(defn bar [z] z)
             '(or a b c)
             '(cond test-1 expr-1 test-2 expr-2 ":else" fallback)])
          :let [out! (string-writer)]]
    (print "#=> ") (prn form)
    (emit form {} out! "return ")
    (println (out!))
    (newline))

 )
