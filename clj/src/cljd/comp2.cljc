(ns cljd.comp2)

(declare emit)

(defn tmpvar [] (gensym "_"))

(defn liftable
  "returns [bindings expr] where expr is atomic."
  [x]
  (case (when (seq? x) (first x))
    dart/let
    (if (coll? (last x))
      (let [tmp (tmpvar)]
        [(concat (second x) [[tmp (last x)]])
         tmp])
      [(second x)
       (last x)])
    dart/if ; no ternary for now
    (let [tmp (tmpvar)]
      [[[tmp x]]
       tmp])
    nil))

(defn emit-fn-call [fn-call env]
  (let [[bindings fn-call]
        (reduce (fn [[bindings fn-call] x]
                  (if-some [[bindings' x] (liftable x)]
                    [(concat bindings' bindings) (cons x fn-call)]
                    (cond
                      (not (coll? x)) [bindings (cons x fn-call)]
                      (seq bindings)
                      (let [tmp (tmpvar)]
                        [(cons [tmp x] bindings)
                         (cons tmp fn-call)])
                      :else
                      [nil (cons x fn-call)])))
                [nil ()] (map #(emit % env) (reverse fn-call)))]
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
    (if-some [[bindings test] (liftable test)]
      (list 'dart/let bindings (list 'dart/if test (emit then env) (emit else env)))
      (list 'dart/if test (emit then env) (emit else env)))))

(defn emit [x env]
  (cond
    (symbol? x) (or (env x) (symbol (str "GLOBAL_" x)))
    (or (string? x) (number? x)) x
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
      (do
        (print (:pre locus))
        (write (first x) expr-locus)
        (print "(")
        (run! #(write % arg-locus) (next x))
        (print ")")
        (print (:post locus))))
    :else (do (print (:pre locus)) (pr x) (print (:post locus)))))

(comment
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
