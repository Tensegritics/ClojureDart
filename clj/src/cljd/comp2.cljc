(ns cljd.comp2)

(declare emit)

(defn tmpvar [] (gensym "_"))

(defn atomic?
  [x] (not (coll? x)))

(defn dart-expr?
  "Takes a dartsexp and returns true if it can be emitted as a Dart expression."
  [x]
  (or (atomic? x)
      (when-some [[x] (when (seq? x) x)]
        (and (symbol? x) (= "dart" (namespace x))))
      (every? dart-expr? x)))

(defn liftable
  "takes a dartsexp and returns a [bindings expr] where expr is atomic
   or nil if there are no bindings to lift."
  [x]
  (case (when (seq? x) (first x))
    dart/let
    (if (atomic? (last x))
      (next x)
      (let [tmp (tmpvar)]
        [(concat (second x) [[tmp (last x)]])
         tmp]))
    dart/if ; no ternary for now
    (let [tmp (tmpvar)]
      [[[tmp x]]
       tmp])
    nil))

(defn lift-arg [must-lift x]
  (or (liftable x)
      (cond
        (atomic? x) [nil x]
        must-lift
        (let [tmp (tmpvar)]
          [[[tmp x]] tmp])
        :else
        [nil x])))

(defn emit-fn-call [fn-call env]
  (let [[positionals [_ & nameds]] (split-with (complement #{'&}) fn-call)
        [bindings fn-call]
        (as-> [nil ()] acc
          (reduce (fn [[bindings fn-call] [k x]]
                  (let [[bindings' x'] (lift-arg (seq bindings) (emit x env))]
                    [(concat bindings' bindings) (list* k x' fn-call)]))
                  acc (reverse (partition 2 nameds)))
          (reduce (fn [[bindings fn-call] x]
                    (let [[bindings' x'] (lift-arg (seq bindings) (emit x env))]
                      [(concat bindings' bindings) (cons x' fn-call)]))
                  acc (reverse positionals)))]
    (cond->> fn-call (seq bindings) (list 'dart/let bindings))))

(defn emit-let [[_ bindings & body] env]
  (let [[dart-bindings env]
        (reduce
         (fn [[dart-bindings env] [k v]]
           (let [tmp (tmpvar)]
             [(conj dart-bindings [tmp (emit v env)])
              (assoc env k tmp)]))
         [[] env] (partition 2 bindings))]
    (list 'dart/let
          (concat dart-bindings (for [x (butlast body)] [nil (emit x env)]))
          (emit (last body) env))))

(defn emit-if [[_ test then else] env]
  (let [test (emit test env)]
    (if-some [[_ bindings test] (liftable test)]
      (list 'dart/let bindings (list 'dart/if test (emit then env) (emit else env)))
      (list 'dart/if test (emit then env) (emit else env)))))

(defn emit [x env]
  (cond
    (symbol? x) (or (env x) (symbol (str "GLOBAL_" x)))
    (or (string? x) (number? x)) x
    (keyword? x) (recur (list 'cljd.Keyword/intern (namespace x) (name x)) env)
    (seq? x)
    (case (first x)
      let* (emit-let x env)
      if (emit-if x env)
      (emit-fn-call x env))))

(defn closed-overs [emitted env]
  (into #{} (keep (set (vals env))) (flatten emitted)))

;; WRITING
(defn declaration [locus] (:decl locus ""))
(defn declared [locus] (:fork locus locus))

(def statement-locus
  {:pre ""
   :post ";\n"})

(def return-locus
  {:pre "return "
   :post ";\n"})

(def expr-locus
  {:pre ""
   :post ""})

(def arg-locus
  {:pre ""
   :post ", "})

(defn declared-var-locus [varname]
  {:pre (str varname "=")
   :post ";\n"})

(defn var-locus [varname]
  {:pre (str "var " varname "=")
   :post ";\n"
   :decl (str "var " varname ";\n")
   :fork (declared-var-locus varname)})

(defn write [x locus]
  (cond
    (seq? x)
    (case (first x)
      dart/let
      (do
        (doseq [[v e] (second x)]
          (write e (if v (var-locus v) statement-locus)))
        (write (last x) locus))
      dart/if
      (let [[_ test then else] x
            decl (declaration locus)
            locus (declared locus)
            test-var (tmpvar)]
        (print decl)
        (write test (var-locus test-var))
        (print (str "if(" test-var "!=null && " test-var "!=false){\n"))
        (write then locus)
        (print "}else{\n")
        (write else locus)
        (print "}\n"))
      (let [f (first x)
            [positionals nameds] (split-with (complement keyword?) (next x))]
        (print (:pre locus))
        (write f expr-locus)
        (print "(")
        (run! #(write % arg-locus) positionals)
        (run! (fn [[k x]]
                (print (str (name k) ": "))
                (write x arg-locus)) (partition 2 nameds))
        (print ")")
        (print (:post locus))))
    :else (do (print (:pre locus)) (pr x) (print (:post locus)))))

(comment
  (emit '(a b c & :d e) {})
  (GLOBAL_a GLOBAL_b GLOBAL_c :d GLOBAL_e)
  (write *1 (var-locus 'RET))

  (emit '(let* [a 1] (println "BOOH") (a 2)) {})
  (dart/let ([_10174 1]
             [nil (GLOBAL_println "BOOH")])
    (_10174 2))
  (write (emit '(let* [a 1] (println "BOOH") (a 2)) {}) return-locus)

  (emit '(a (b c) (d e)) {})
  (GLOBAL_a (GLOBAL_b GLOBAL_c) (GLOBAL_d GLOBAL_e))

  (emit '(a (side-effect! 42) (let* [d 1] (d e)) (side-effect! 33)) {})
  (dart/let ([_10294 (GLOBAL_side-effect! 42)] [_10292 1] [_10293 (_10292 GLOBAL_e)]) (GLOBAL_a _10294 _10293 (GLOBAL_side-effect! 33)))
  (write *1 return-locus)

  (emit '(a (if b c d)) {})
  (dart/let ([_10299 (dart/if GLOBAL_b GLOBAL_c GLOBAL_d)]) (GLOBAL_a _10299))
  (write *1 return-locus)

  (emit '(if b c d) {})
  (dart/if GLOBAL_b GLOBAL_c GLOBAL_d)
  (write *1 (var-locus 'RET))

  (emit '(if b (let* [c 1] c) d) {})
  (dart/if GLOBAL_b (dart/let ([_10417 1]) _10417) GLOBAL_d)
  (write *1 return-locus)

  (emit '(if b (let* [c 1] (if c x y)) d) {})
  (dart/if GLOBAL_b (dart/let ([_10425 1]) (dart/if _10425 GLOBAL_x GLOBAL_y)) GLOBAL_d)
  (write *1 (var-locus 'RET))

  (emit '(if (let* [x 1] x) then else) {})
  (dart/let ([_10434 1]) (dart/if _10434 GLOBAL_then GLOBAL_else))
  (write *1 (var-locus 'RET))


  )
