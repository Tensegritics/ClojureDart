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

(defn has-recur?
  "Takes a dartsexp and returns true when it contains an open recur."
  [x]
  (some {'dart/recur true} (tree-seq seq? #(case (first %) (dart/loop dart/fn) nil %) x)))

(defn liftable
  "Takes a dartsexp and returns a [bindings expr] where expr is atomic
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

(defn- lift-arg [must-lift x]
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

(defn emit-new [[_ & class+args] env]
  (emit-fn-call class+args env))

(defn emit-dot [[_ obj member & args] env]
  (let [member (name member)
        [_ prop name] (re-matches #"(-)?(.+)" member)
        prop (and prop (nil? args))
        op (if prop 'dart/.- 'dart/.)
        fn-call (emit-fn-call (cons obj args) env)]
    (case (first fn-call)
      dart/let (let [[_ bindings [obj & args]] fn-call]
                 (list 'dart/let bindings (list* op obj name args)))
      (list* op (first fn-call) name (next fn-call)))))

(defn emit-let [[_ bindings & body] env]
  (let [[dart-bindings env]
        (reduce
         (fn [[dart-bindings env] [k v]]
           (let [tmp (tmpvar)]
             [(conj dart-bindings [tmp (emit v env)])
              (assoc env k tmp)]))
         [[] env] (partition 2 bindings))
        dart-bindings
        (concat dart-bindings (for [x (butlast body)] [nil (emit x env)]))]
    (cond->> (emit (last body) env)
      ; wrap only when ther are actual bindings
      (seq dart-bindings) (list 'dart/let dart-bindings))))

(defn emit-loop [[_ bindings & body] env]
  (let [[dart-bindings env]
        (reduce
         (fn [[dart-bindings env] [k v]]
           (let [tmp (tmpvar)]
             [(conj dart-bindings [tmp (emit v env)])
              (assoc env k tmp)]))
         [[] env] (partition 2 bindings))]
    (list 'dart/loop dart-bindings (emit (list* 'let* [] body) env))))

(defn emit-recur [[_ & exprs] env]
  (cons 'dart/recur (map #(emit % env) exprs)))

(defn emit-if [[_ test then else] env]
  (let [test (emit test env)]
    (if-some [[_ bindings test] (liftable test)]
      (list 'dart/let bindings (list 'dart/if test (emit then env) (emit else env)))
      (list 'dart/if test (emit then env) (emit else env)))))

(defn emit
  "Takes a clojure form and a lexical environment and returns a dartsexp."
  [x env]
  (cond
    (symbol? x) (or (env x) (symbol (str "GLOBAL_" x)))
    (or (string? x) (number? x)) x
    (keyword? x) (recur (list 'cljd.Keyword/intern (namespace x) (name x)) env)
    (seq? x)
    (case (first x)
      . (emit-dot x env)
      new (emit-new x env)
      let* (emit-let x env)
      loop* (emit-loop x env)
      recur (emit-recur x env)
      if (emit-if x env)
      (emit-fn-call x env))))

(defn closed-overs [emitted env]
  (into #{} (keep (set (vals env))) (tree-seq coll? seq emitted)))

;; WRITING
(defn declaration [locus] (:decl locus ""))
(defn declared [locus]
  ; merge to conserve custom attributes
  (merge locus (:fork locus)))

(def statement-locus
  {:pre ""
   :post ";\n"})

(def return-locus
  {:pre "return "
   :post ";\n"})

(def expr-locus
  {:pre ""
   :post ""})

(def paren-locus
  {:pre "("
   :post ")"})

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

(defn- write-args [args]
  (let [[positionals nameds] (split-with (complement keyword?) args)]
    (print "(")
    (run! #(write % arg-locus) positionals)
    (run! (fn [[k x]]
            (print (str (name k) ": "))
            (write x arg-locus)) (partition 2 nameds))
    (print ")")))

(defn write
  "Takes a dartsexp and a locus.
   Prints valid dart code."
  [x locus]
  (cond
    (seq? x)
    (case (first x)
      dart/let
      (let [[_ bindings expr] x]
        (doseq [[v e] bindings]
          (write e (if v (var-locus v) statement-locus)))
        (write expr locus))
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
      dart/loop
      (let [[_ bindings expr] x
            decl (declaration locus)
            locus (-> locus declared (assoc :loop-bindings (map first bindings)))]
        (print decl)
        (doseq [[v e] bindings]
          (write e (var-locus v)))
        (print "do {\n")
        (write expr locus)
        (print "break;\n} while(true);\n"))
      dart/recur
      (let [[_ & exprs] x
            {:keys [loop-bindings]} locus
            expected (count loop-bindings)
            actual (count exprs)]
        (when-not loop-bindings
          (throw (ex-info "Can only recur from tail position." {})))
        (when-not (= expected actual)
          (throw (ex-info (str "Mismatched argument count to recur, expected: "
                               expected " args, got: " actual) {})))
        (let [vars (set loop-bindings)
              vars-usages (->>
                           (map #(into #{} (keep (disj vars %1))
                                       (tree-seq coll? seq %2))
                                loop-bindings exprs)
                           reverse
                           (reductions into)
                           reverse)
              tmps (into {}
                         (map (fn [v vs] (when (vs v) [v (tmpvar)]))
                              loop-bindings vars-usages))]
          (doseq [[v e] (map vector loop-bindings exprs)]
            (write e (if-some [tmp (tmps v)] (var-locus tmp) (declared-var-locus v))))
          (doseq [[v tmp] tmps]
            (write tmp (declared-var-locus v)))
          (print "continue;\n")))
      dart/.-
      (let [[_ obj fld] x]
        (print (:pre locus))
        (write obj expr-locus)
        (print (str "." fld))
        (print (:post locus)))
      dart/.
      (let [[_ obj meth & args] x]
        (print (:pre locus))
        (case meth
          ;; operators
          "[]" (do
                 (write obj expr-locus)
                 (print "[")
                 (write (first args) expr-locus)
                 (print "]"))
          "[]=" (do
                  (write obj expr-locus)
                  (print "[")
                  (write (first args) expr-locus)
                  (print "]=")
                  (write (second args) expr-locus))
          "~" (do
                (print meth)
                (write obj paren-locus))
          "-" (if args
                (do
                  (write obj paren-locus)
                  (print meth)
                  (write (first args) paren-locus))
                (do
                  (print meth)
                  (write obj paren-locus)))
          ("<" ">" "<=" ">=" "==" "+" "~/" "/" "*" "%" "|" "^" "&" "<<" ">>" ">>>")
          (do
            (write obj paren-locus)
            (print meth)
            (write (first args) paren-locus))
          ;; else plain method
          (do
            (write obj expr-locus)
            (print (str "." meth))
            (write-args args)))
        (print (:post locus)))
      ;; plain fn call
      (let [[f & args] x]
        (print (:pre locus))
        (write f expr-locus)
        (write-args args)
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

  (emit '(. a "[]" i) {})
  (dart/. GLOBAL_a "[]" GLOBAL_i)
  (write *1 (var-locus 'RET))

  (emit '(let* [b (new List)] (. b "[]=" 0 "hello") b) {})
  (dart/let ([_11752 (GLOBAL_List)] [nil (dart/. _11752 "[]=" 0 "hello")]) _11752)
  (write *1 (var-locus 'RET))

  (emit '(. obj meth) {})
  (dart/. GLOBAL_obj "meth")
  (emit '(. obj -prop) {})
  (dart/.- GLOBAL_obj "prop")

  (emit '(. obj meth a & :b :c) {})
  (dart/. GLOBAL_obj "meth" GLOBAL_a :b (GLOBAL_cljd.Keyword/intern nil "c"))

  (emit '(. (. a + b) * (. c + d)) {})
  (dart/. (dart/. GLOBAL_a "+" GLOBAL_b) "*" (dart/. GLOBAL_c "+" GLOBAL_d))
  (write *1 (var-locus 'RET))
  ;; var RET=((GLOBAL_a)+(GLOBAL_b))*((GLOBAL_c)+(GLOBAL_d));

  (emit '(. (. a + (if flag 0 1)) * (. c + d)) {})
  (dart/let ([_12035 (dart/if GLOBAL_flag 0 1)] [_12036 (dart/. GLOBAL_a "+" _12035)]) (dart/. _12036 "*" (dart/. GLOBAL_c "+" GLOBAL_d)))
  (write *1 (var-locus 'RET))
  ;; var _12035;
  ;; var _12039=GLOBAL_flag;
  ;; if(_12039!=null && _12039!=false){
  ;; _12035=0;
  ;; }else{
  ;; _12035=1;
  ;; }
  ;; var _12036=(GLOBAL_a)+(_12035);
  ;; var RET=(_12036)*((GLOBAL_c)+(GLOBAL_d));

  (emit '(. (let* [a (new Obj)] a) meth) {})
  (dart/let ([_11858 (GLOBAL_Obj)]) (dart/. _11858 "meth"))
  (write *1 (var-locus 'RET))
  ;; var _11858=GLOBAL_Obj();
  ;; var RET=_11858.meth();

  (emit '(loop* [a 1] (if test (recur (inc a)) a)) {})
  (dart/loop [[_12413 1]]
    (dart/if GLOBAL_test
      (dart/recur (GLOBAL_inc _12413))
      _12413))
  (write *1 return-locus)
  ;; var _12413=1;
  ;; do {
  ;; var _12416=GLOBAL_test;
  ;; if(_12416!=null && _12416!=false){
  ;; _12413=GLOBAL_inc(_12413, );
  ;; continue;
  ;; }else{
  ;; return _12413;
  ;; }
  ;; break;
  ;; } while(true);

  (emit '(loop* [a 1 b 2] (recur b a)) {})
  (dart/loop [[_12419 1] [_12420 2]] (dart/recur _12420 _12419))
  (write *1 return-locus)
  ;; var _12419=1;
  ;; var _12420=2;
  ;; do {
  ;; var _12423=_12420;
  ;; _12420=_12419;
  ;; _12419=_12423;
  ;; continue;
  ;; break;
  ;; } while(true);

  (emit '(loop* [a 1 b 2] (recur (inc a) (dec b))) {})
  (dart/loop [[_12693 1] [_12694 2]] (dart/recur (GLOBAL_inc _12693) (GLOBAL_dec _12694)))
  (write *1 return-locus)
  ;; var _12426=1;
  ;; var _12427=2;
  ;; do {
  ;; _12426=GLOBAL_inc(_12426, );
  ;; _12427=GLOBAL_dec(_12427, );
  ;; continue;
  ;; break;
  ;; } while(true);
  )
