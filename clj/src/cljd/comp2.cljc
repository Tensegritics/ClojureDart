(ns cljd.comp2
  (:refer-clojure :exclude [macroexpand macroexpand-1 munge load-file]))

(defmacro ^:private else->> [& forms]
  `(->> ~@(reverse forms)))

(def nses (atom {:current-ns 'user
                 'user {}}))

(defn macroexpand-1 [env form]
  (if-let [[f & args] (and (seq? form) (symbol? (first form)) form)]
    (let [name (name f)
          ;; TODO symbol resolution and macro lookup in cljd
          #?@(:clj [clj-var (ns-resolve (find-ns (:current-ns @nses)) f)])]
      ;; TODO add proper expansion here, before defaults
      (cond
        (env f) form
        #?@(:clj                        ; macro overrides
            [(= 'ns f) form
             (= 'reify f) nil #_(apply expand-reify args)]) ;; @TODO : change this
        (= '. f) form
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

(defn macroexpand [env form]
  (let [ex (macroexpand-1 env form)]
    (cond->> ex (not (identical? ex form)) (recur env))))

(declare emit)

(defn tmpvar [] (gensym "_"))

(defn atomic?
  [x] (not (coll? x)))

(comment
  ; TODO replace atomic? by something more finegrained
  (defn dart-expr?
    "Takes a dartsexp and returns true if it can be emitted as a Dart expression."
    [x]
    (cond
      (not (coll? x)) true
      (case (when (seq? x) (first x))
        (dart/if dart/let dart/loop dart/let-fn) true false) false
      :else (every? dart-expr? x))))

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

(defn emit-do [[_ & body] env]
  (list 'dart/let (for [x (butlast body)] [nil (emit x env)]) (emit (last body) env)))

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
    (if-some [[bindings test] (liftable test)]
      (list 'dart/let bindings (list 'dart/if test (emit then env) (emit else env)))
      (list 'dart/if test (emit then env) (emit else env)))))

(defn- variadic? [[params]] (some #{'&} params))

(defn- emit-non-variadic-body [[params & body] all-params env]
  (let [env (into env (zipmap params (repeatedly tmpvar)))
        body (emit (list* 'do body) env)]
    (list (if (has-recur? body) 'dart/loop 'dart/let) (map vector (map env params) all-params) body)))

(defn- emit-variadic-body [[params & body] all-params env]
  #_(list* 'let* (map vector params all-params) body))

(defn emit-fn [[_ & bodies] env]
  (let [name (when (symbol? (first bodies)) (first bodies))
        env (cond-> env name (assoc name (tmpvar))) ; TODO munge
        bodies (cond->> bodies name next)
        bodies (cond-> bodies (vector? (first bodies)) list)
        [variadic & too-many-variadics] (filter variadic? bodies)
        variadic-min
        (some-> variadic first (take-while (complement #{'&})) count)
        non-variadics
        (->> bodies (remove variadic?)
             (group-by (comp count first))
             (sort-by key)
             (into []
                   (map (fn [[n [body & too-many-bodies]]]
                          (when too-many-bodies
                            (throw (ex-info "Can't have 2 overloads with same arity" {})))
                          body))))
        non-variadics-max (-> non-variadics peek first count)
        params-min (or (some-> non-variadics ffirst count) variadic-min)
        params-max (if variadic 20 non-variadics-max)
        all-params (vec (repeatedly params-max tmpvar))
        last-body (or (some-> variadic (emit-variadic-body all-params env))
                      (some-> (peek non-variadics) (emit-non-variadic-body all-params env)))
        non-variadics (cond-> non-variadics (not variadic) pop)
        dart-fn
        (list 'dart/fn (env name) (take params-min all-params) (drop params-min all-params)
              (reduce
               (fn [else [params :as body]]
                 (let [next-p (nth all-params (count params))]
                   (list 'dart/if (list (emit 'cljd/missing-arg? env) next-p)
                         (emit-non-variadic-body body all-params env)
                         else)))
               last-body
               (rseq non-variadics)))]
    (when (some-> variadic-min (< non-variadics-max))
      (throw (ex-info "Can't have fixed arity function with more params than variadic function" {})))
    (if name ; systematically lift named functions
      (list 'dart/let [[nil dart-fn]] (env name))
      dart-fn)))

(defn emit-method [[mname [this-arg & fixed-args] named-args & body] env]
  ;; args destructuring will be added by a macro
  ;; named-args need to have been fully expanded to triples [keyword symbol default] by the macro
 #_ (let [dartargs (into [] (map (fn [arg] [arg (tmpvar)])) fixed-args) ; it's a vector and not map cause a binding may appear several times
        env (-> env
                (assoc this-arg (DartExpr. "this"))
                (into dartargs)
                (into
                 (map (fn [[k sym]] [sym (DartExpr. (name k))]))
                 named-args)
                (assoc ::loop-bindings
                       (into [] (map second) dartargs)))]
    (write! mname)
    (write! "(")
    (doseq [[_ arg] dartargs] (write! arg) (write! ", "))
    (when (seq named-args)
      (write! "{")
      (doseq [[k sym default] named-args]
        (write! (name k))
        (write! "=")
        (emit-const-expr default)
        (write! ", "))
      (write! "}"))
    (flush! "){\ndo {\n")
    (write! "return " (emit-body body env) ";\n")
    (flush! "} while(true);\n}\n\n"))
  nil)

(defn emit-reify [[_ opts & specs] env]
  #_(let [{:keys [extends] :or {extends 'Object}} opts
        [ctor-op base & ctor-args :as ctor] (macroexpand env (cond->> extends (symbol? extends) (list 'new)))
        ctor-meth (when (= '. ctor-op) (first ctor-args))
        ctor-args (cond-> ctor-args (= '. ctor-op) next)
        [positional-ctor-args [_ & named-ctor-args]] (split-with (complement #{'&}) ctor-args)
        positional-ctor-params (repeatedly (count positional-ctor-args) tmpvar)
        named-ctor-params (repeatedly (quot (count named-ctor-args) 2) tmpvar)
        super-ctor
        {:method ctor-meth ; nil for new
         :positional-args positional-ctor-params
         :named-args (map vector (take-nth 2 named-ctor-args) named-ctor-params)}
        class-name (tmpvar)  ; TODO change this to a more telling name
        classes (filter #(and (symbol? %) (not= base %)) specs) ; crude
        methods (remove symbol? specs)  ; crude
        mixins(filter (comp :mixin meta) classes)
        ifaces (remove (comp :mixin meta) classes)
        need-nsm (and (seq ifaces) (not-any? (fn [[m]] (case m noSuchMethod true nil)) methods))

        env {} #_(closed-overs-recording-env env)
        closed-overs nil #_(get-closed-overs env)
        reify-ctor (concat ['new class-name] positional-ctor-args (take-nth 2 (next named-ctor-args)))]
    (->>
     (with-string-emitter
       (open-prior! "class " class-name)
       (when base
         (write! " extends " (symbol-literal base {}))) ; TODO munge
       (when (seq ifaces)
         (write! " implements " (str/join ", " (map name ifaces)))) ; TODO aliasing
       (when (seq mixins)
         (write! " with " (str/join ", " (map name mixins)))) ; TODO aliasing
       (flush! " {\n")
       ;; closed overs
       (doseq [field (vals closed-overs)]
                                        ; revisit when support for :once
         (write! "final " field ";\n"))
       ;; methods
       (when (seq methods)
         (doseq [m methods]
           (write! (emit-method m env))))
       ;; constructor
       (flush! class-name "(" (str/join ", " (concat positional-ctor-params named-ctor-params
                                                     (map #(str "this." (name %)) (vals closed-overs)))))
       (when super-ctor
         (write! ": " (emit-literal super-ctor {}) ";\n"))
       ;; noSuchMethod
       (when need-nsm
         (write! "noSuchMethod(i)=>super.noSuchMethod(i);\n"))
       (close-prior! "}\n\n"))
     (assoc {:type :class} :code)
     (swap! nses do-def class-name))
    (with-ret (write! (emit reify-ctor {})))))

(defn write-top-dartfn [sym x]
  (write x (top-fn-locus (name sym))))

(defn do-def [nses sym m]
  (assoc-in nses [(:current-ns nses) sym] m))

(defn emit-def [[_ sym expr] env]
  (let [expr (macroexpand env expr)]
    (if (and (seq? expr) (= 'fn* (first expr)) (not (symbol? (second expr))))
      (swap! nses do-def sym
             {:type :dartfn
              :code (with-out-str (write-top-dartfn sym (emit expr env)))})
      (swap! nses do-def sym
             {:type :field
              :code (with-out-str
                      (write (emit (if (seq? expr) (list (list 'fn* [] expr)) expr) env)
                             (var-locus (name sym))))}))
    (emit sym env)))

(defn emit
  "Takes a clojure form and a lexical environment and returns a dartsexp."
  [x env]
  (let [x (macroexpand env x)]
    (cond
      (symbol? x) (or (env x)
                      (let [nses @nses
                            {:keys [mappings aliases] :as current-ns} (nses (:current-ns nses))]
                        (else->> (if (current-ns x) x)
                                 (or (get mappings x))
                                 (if-some [alias (get aliases (namespace x))] (str alias "." (name x)))
                                 #_"TODO next form should throw"
                                 (symbol (str "GLOBAL_" x)))))
      (or (string? x) (number? x) (boolean? x)) x
      (keyword? x) (recur (list 'cljd.Keyword/intern (namespace x) (name x)) env)
      (seq? x)
      (let [emit (case (first x)
                   . emit-dot
                   do emit-do
                   new emit-new
                   let* emit-let
                   loop* emit-loop
                   recur emit-recur
                   if emit-if
                   fn* emit-fn
                   def emit-def
                   emit-fn-call)]
        (emit x env)))))

(defn closed-overs [emitted env]
  (into #{} (keep (set (vals env))) (tree-seq coll? seq emitted)))

;; WRITING
(defn declaration [locus] (:decl locus ""))
(defn declared [locus]
  ; merge to conserve custom attributes
  (merge (dissoc locus :fork :decl) (:fork locus)))

(def statement-locus
  {:pre ""
   :post ";\n"})

(def return-locus
  {:pre "return "
   :post ";\n"})

(defn top-fn-locus [fnname]
  {:pre  fnname
   :post "\n"})

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

(declare write)

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
      dart/fn
      (let [[_ name fixed-params opt-params body] x]
        (print (:pre locus))
        (some-> name print) ; name is not nil only when locus is statement
        (print "(")
        (run! print (interleave fixed-params (repeat ", ")))
        (when (seq opt-params)
          (print "[")
          (run! print (interleave opt-params (repeat ", ")))
          (print "]"))
        (print "){\n")
        (write body return-locus)
        (print "}")
        (print (:post locus)))
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
        (some-> decl print)
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
        (some-> decl print)
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
  (dart/let ([_6612 1] [nil (GLOBAL_println "BOOH")]) (_6612 2))
  (write *1 return-locus)

  (emit '(a (b c) (d e)) {})
  (GLOBAL_a (GLOBAL_b GLOBAL_c) (GLOBAL_d GLOBAL_e))
  (write *1 return-locus)

  (emit '(a (side-effect! 42) (let* [d 1] (d e)) (side-effect! 33)) {})
  (dart/let ([_10294 (GLOBAL_side-effect! 42)] [_10292 1] [_10293 (_10292 GLOBAL_e)]) (GLOBAL_a _10294 _10293 (GLOBAL_side-effect! 33)))
  (write *1 return-locus)

  (emit '(a (if b c d)) {})
  (dart/let ([_10299 (dart/if GLOBAL_b GLOBAL_c GLOBAL_d)]) (GLOBAL_a _10299))
  (write *1 return-locus)

  (emit '(if b c d) {})
  (dart/if GLOBAL_b GLOBAL_c GLOBAL_d)
  (write *1 (var-locus 'RET))

  (emit '(if (if true "true") c d) {})
  (dart/let [[_9946 (dart/if true "true" nil)]] (dart/if _9946 GLOBAL_c GLOBAL_d))
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

  (emit '(. (let* [o obj] o) -prop) {})
  (write *1 (var-locus 'RET))


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


  (emit '(loop* [a 1 b 2] a b 3 4 (recur 1 2 )) {})
  (dart/loop [[_10053 1] [_10054 2]] (dart/let ([nil _10053] [nil _10054] [nil 3] [nil 4]) (dart/recur 1 2)))
  (write *1 return-locus)

  (emit-fn '(fn [x] x) {})
  (dart/fn (_12891) () (dart/let ([_12892 _12891]) _12892))

  (emit-fn '(fn ([x] x) ([x y] y)) {})
  (dart/fn (_12895) (_12896)
    (dart/if (GLOBAL_cljd/missing-arg? _12896)
      (dart/let ([_12897 _12895]) _12897)
      (dart/let ([_12898 _12895] [_12899 _12896]) _12899)))

  (emit-fn '(fn ([x] x) ([x y] y) ([u v w x y] u)) {})
  (dart/fn (_12902) (_12903 _12904 _12905 _12906)
    (dart/if (GLOBAL_cljd/missing-arg? _12903)
      (dart/let ([_12907 _12902]) _12907)
      (dart/if (GLOBAL_cljd/missing-arg? _12904)
        (dart/let ([_12908 _12902] [_12909 _12903]) _12909)
        (dart/let ([_12910 _12902] [_12911 _12903] [_12912 _12904] [_12913 _12905] [_12914 _12906]) _12910))))

  (emit-fn '(fn ([x] (recur x)) ([x y] y) ([u v w x y] u)) {})
  (dart/fn (_13991) (_13992 _13993 _13994 _13995)
    (dart/if (GLOBAL_cljd/missing-arg? _13992)
      (dart/loop ([_14005 _13991]) (dart/recur _14005))
      (dart/if (GLOBAL_cljd/missing-arg? _13993)
        (dart/let ([_14002 _13991] [_14003 _13992]) _14003)
        (dart/let ([_13996 _13991] [_13997 _13992] [_13998 _13993] [_13999 _13994] [_14000 _13995]) _13996))))
  (write *1 (var-locus "XXX"))

  (emit '(let* [inc (fn* [x] (. x "+" 1))] (inc 3)) {})
  (write *1 (var-locus "DDDD"))

  (emit '(loop* [a 4 b 5] a (recur b a)) {})
  (dart/loop [[_8698 4] [_8699 4]] (dart/let ([nil _8698]) _8699))
  (write *1 (var-locus "DDDD"))

  (emit '(do 1 2 3 4 (a 1) "ddd") {})
  (dart/let ([nil 1] [nil 2] [nil 3] [nil 4] [nil (GLOBAL_a 1)]) "ddd")
  (write *1 (var-locus "this"))


  (emit '(or 1 2 3 4 (a 1) "ddd") {})
  (dart/let ([_9757 1]) (dart/if _9757 _9757 (dart/let ([_9758 2]) (dart/if _9758 _9758 (dart/let ([_9759 3]) (dart/if _9759 _9759 (dart/let ([_9760 4]) (dart/if _9760 _9760 (dart/let ([_9761 (GLOBAL_a 1)]) (dart/if _9761 _9761 "ddd"))))))))))
  (write *1 return-locus)

  (macroexpand {} '(fn* nom [a] a))
  )


(defn do-ns [[_ ns-sym & ns-clauses]]
  (let [ns-clauses (drop-while #(or (string? %) (map? %)) ns-clauses) ; drop doc and meta for now
        mappings
        (transduce
         (comp
          (mapcat (fn [[directive & args]] (map #(vector directive %) args)))
          (map
           (fn [[directive arg]]
             (case directive
               :require
               (let [arg (if (vector? arg) arg [arg])
                     alias (name (gensym "lib"))
                     clauses (into {} (partition-all 2) (next arg))]
                 (cond-> (assoc {} :imports [[(name (first arg)) alias]])
                   (:as clauses) (assoc-in [:aliases (name (:as clauses))] alias)
                   (:refer clauses) (assoc :mappings (into {} (map #(vector % (str alias "." (name %)))) (:refer clauses)))))
               :import (/ 0)
               :refer-clojure (/ 0)
               :use (/ 0)))))
         (partial merge-with into)
         {} ns-clauses)]
    (swap! nses assoc ns-sym mappings :current-ns ns-sym)))

(comment

  (do-ns '(ns cljd.user
            (:require [cljd.bordeaux :refer [reviews] :as awesome]
                      [cljd.ste :as ste]
                      ["package:flutter/material.dart"]
                      clojure.string)))


  (emit '((((fn* [] (fn* [] (fn* [] 42)))))) {})
  ((((dart/fn () () (dart/let () (dart/fn () () (dart/let () (dart/fn () () (dart/let () 42)))))))))
  (write *1 (var-locus "DDDD"))

  (emit '(fn* [x] x) {})
  (dart/fn nil (_16639) () (dart/let ([_16640 _16639]) _16640))
  (write *1 return-locus)

  (emit '(fn* fname [x] 42) {})
  (dart/let [[nil (dart/fn _16623 (_16624) () (dart/let ([_16625 _16624]) 42))]] _16623)
  (write *1 return-locus)

  (emit '((fn* fname [x] 42)) {})
  (dart/let ([nil (dart/fn _16631 (_16632) () (dart/let ([_16633 _16632]) 42))]) (_16631))
  (write *1 return-locus)

  (emit '(def oo (fn* [x] 42)) {})
  (write *1 return-locus)



  (emit '(def oo1 42) {})


  (emit '(def oo (fn* [x] (if (.-isOdd x) (recur (. x + 1)) x ))) {})
  nses

  (emit '(def oo "caca\n") {})

  (write *1 return-locus)











  )
