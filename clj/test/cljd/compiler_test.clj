(ns cljd.compiler-test
  (:require
    [clojure.test :refer :all]
    [cljd.compiler :as comp]))

(defmacro test-emit-write [& body]
  `(do
     ~@(map (fn [[form# env# dart-sexpr# dart#]]
              `(let [f# (comp/emit-test '~form# '~env#)]
                 (is (= f# '~dart-sexpr#))
                 (is (= (with-out-str (comp/write f# (comp/var-locus "RET"))) ~dart#)))) body)))

(deftest dart-function-call-with-optional-named-param
  (test-emit-write
   [(a b c .& :d e) {a a b b c c e e}
    (a b c :d e)
    "var RET=a(b, c, d: e, );\n"]

   [(let [a 1] (println "BOOH") (a 2)) {println println}
    (dart/let [[a_$1_ 1] [nil (println "BOOH")]] (a_$1_ 2))
    "var a_$1_=1;
((println is IFn) ? (println as IFn).invoke(\"BOOH\", ) : println(\"BOOH\", ));
var RET=((a_$1_ is IFn) ? (a_$1_ as IFn).invoke(2, ) : a_$1_(2, ));\n"]

   [(a (b c) (d e)) {a a b b c c d d e e}
    (a (b c) (d e))
    "var RET=((a is IFn) ? (a as IFn).invoke(((b is IFn) ? (b as IFn).invoke(c, ) : b(c, )), ((d is IFn) ? (d as IFn).invoke(e, ) : d(e, )), ) : a(((b is IFn) ? (b as IFn).invoke(c, ) : b(c, )), ((d is IFn) ? (d as IFn).invoke(e, ) : d(e, )), ));\n"]

   [(a (side-effect! 42) (let [d 1] (d e)) (side-effect! 33)) {a a side-effect! side-effect! e e}
    (dart/let ([_arg_$1_ (side-effect! 42)] [d_$1_ 1] [_$1_ (d_$1_ e)]) (a _arg_$1_ _$1_ (side-effect! 33)))
    "var _arg_$1_=((side-effect! is IFn) ? (side-effect! as IFn).invoke(42, ) : side-effect!(42, ));
var d_$1_=1;
var _$1_=((d_$1_ is IFn) ? (d_$1_ as IFn).invoke(e, ) : d_$1_(e, ));
var RET=((a is IFn) ? (a as IFn).invoke(_arg_$1_, _$1_, ((side-effect! is IFn) ? (side-effect! as IFn).invoke(33, ) : side-effect!(33, )), ) : a(_arg_$1_, _$1_, ((side-effect! is IFn) ? (side-effect! as IFn).invoke(33, ) : side-effect!(33, )), ));\n"]

   [(. a "[]" i) {a a i i}
    (dart/. a "[]" i)
    "var RET=a[i];\n"]

   [(let [b (new List)] (. b "[]=" 0 "hello") b) {}
    (dart/let [[b_$1_ (dc.List)] [nil (dart/. b_$1_ "[]=" 0 "hello")]] b_$1_)
    "var b_$1_=dc.List();
b_$1_[0]=\"hello\";
var RET=b_$1_;\n"]

   [(. obj meth 1 2) {obj obj}
    (dart/. obj "meth" 1 2)
    "var RET=obj.meth(1, 2, );\n"]

   [(. obj meth 1 2 .& :pos "val" :pos1 "val2") {obj obj}
    (dart/. obj "meth" 1 2 :pos "val" :pos1 "val2")
    "var RET=obj.meth(1, 2, pos: \"val\", pos1: \"val2\", );\n"]

   [(. obj -prop) {obj obj}
    (dart/.- obj "prop")
    "var RET=obj.prop;\n"]

   [(. (let [o obj] o) -prop) {obj obj}
    (dart/let ([o_$1_ obj]) (dart/.- o_$1_ "prop"))
    "var o_$1_=obj;
var RET=o_$1_.prop;\n"]

   [(. (. a + b) * (. c + d)) {a a b b c c d d}
    (dart/let ([_f_$1_ (dart/. a "+" b)]) (dart/. _f_$1_ "*" (dart/. c "+" d)))
    "var _f_$1_=(a)+(b);
var RET=(_f_$1_)*((c)+(d));\n"]

   [(. (let [a (new Object)] a) meth) {}
    (dart/let ([a_$1_ (dc.Object)]) (dart/. a_$1_ "meth"))
    "var a_$1_=dc.Object();
var RET=a_$1_.meth();\n"]
   ))

(deftest fn-call-with-if-expr-as-argument
  (let [form (comp/emit-test '(a (if b c d)) '{a a b b c c d d})]
    (is (= form
           '(dart/let ([if$_$1_ (dart/if b c d)]) (a if$_$1_))))
    (is (= (with-out-str
             (comp/write form (comp/var-locus 'RET)))
           ""))))

(comment

  (run-tests)

  )



(comment

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
