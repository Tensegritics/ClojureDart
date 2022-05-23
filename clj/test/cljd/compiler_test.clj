;   Copyright (c) Baptiste Dupuch & Christophe Grand . All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

; @WARNING: Thu Jul 29 16:19:43 2021 : Tests are outdated but we keep
; them because we plan to fix them.

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

(deftest emit-tests
  (test-emit-write
   [(a b c .& :d e) {a a b b c c e e}
    (a b c :d e)
    "var RET=a(b, c, d: e, );\n"]

   [(let [a 1] (^:dart println "BOOH") (^:dart a 2)) {println println}
    (dart/let [[a$1 1] [nil (println "BOOH")]] (a$1 2))
    "var a$1=1;
println(\"BOOH\", );
var RET=a$1(2, );\n"]

   [(^:dart a (^:dart b c) (^:dart d e)) {a a b b c c d d e e}
    (a (b c) (d e))
    "var RET=a(b(c, ), d(e, ), );\n"]

   [(^:dart a (^:dart side-effect! 42) (let [d 1] (^:dart d e)) (^:dart side-effect! 33)) {a a side-effect! side-effect! e e}
    (dart/let ([arg$1 (side-effect! 42)] [d$1 1] [$1 (d$1 e)]) (a arg$1 $1 (side-effect! 33)))
    "var arg$1=side-effect!(42, );
var d$1=1;
var $1=d$1(e, );
var RET=a(arg$1, $1, side-effect!(33, ), );\n"]

   [(. a "[]" i) {a a i i}
    (dart/. a "[]" i)
    "var RET=a[i];\n"]

   [(let [b (new List)] (. b "[]=" 0 "hello") b) {}
    (dart/let [[b$1 (dc.List)] [nil (dart/. b$1 "[]=" 0 "hello")]] b$1)
    "var b$1=dc.List();
b$1[0]=\"hello\";
var RET=b$1;\n"]

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
    (dart/let ([o$1 obj]) (dart/.- o$1 "prop"))
    "var o$1=obj;
var RET=o$1.prop;\n"]

   [(. (. a + b) * (. c + d)) {a a b b c c d d}
    (dart/. (dart/. a "+" b) "*" (dart/. c "+" d))
    "var RET=((a)+(b))*((c)+(d));\n"]

   [(. (let [a (new Object)] a) meth) {}
    (dart/let ([a$1 (dc.Object)]) (dart/. a$1 "meth"))
    "var a$1=dc.Object();
var RET=a$1.meth();\n"]

   [(^:dart a (if b c d)) {a a b b c c d d}
    (dart/let ([$if_$1 (dart/if (dart/. (dart/. b "!=" false) "&" (dart/. b "!=" nil)) c d)]) (a $if_$1))
    "var $if_$1;
if(((b)!=(false))&&((b)!=(null))){
$if_$1=c;
}else{
$if_$1=d;
}
var RET=a($if_$1, );\n"]

   [(if b c d) {b b c c d d}
    (dart/if (dart/. (dart/. b "!=" false) "&" (dart/. b "!=" nil)) c d)
    "var RET;
if(((b)!=(false))&&((b)!=(null))){
RET=c;
}else{
RET=d;
}\n"]

   [(if (if true "true") c d) {c c d d}
    (dart/let [[$if_$1 (dart/if (dart/. (dart/. true "!=" false) "&" (dart/. true "!=" nil)) "true" nil)]]
      (dart/if (dart/. (dart/. $if_$1 "!=" false) "&" (dart/. $if_$1 "!=" nil)) c d))
    "var $if_$1;
if(((true)!=(false))&&((true)!=(null))){
$if_$1=\"true\";
}else{
$if_$1=null;
}
var RET;
if((($if_$1)!=(false))&&(($if_$1)!=(null))){
RET=c;
}else{
RET=d;
}\n"]

   [(if b (let [c 1] c) d) {b b d d}
    (dart/if (dart/. (dart/. b "!=" false) "&" (dart/. b "!=" nil)) (dart/let [[c$1 1]] c$1) d)
    "var RET;
if(((b)!=(false))&&((b)!=(null))){
var c$1=1;
RET=c$1;
}else{
RET=d;
}\n"]

   [(if b (let [c 1] (if c x y)) d) {b b x x y y d d}
    (dart/if (dart/. (dart/. b "!=" false) "&" (dart/. b "!=" nil))
      (dart/let [[c$1 1]]
        (dart/if (dart/. (dart/. c$1 "!=" false) "&" (dart/. c$1 "!=" nil))
          x y))
      d)
    "var RET;
if(((b)!=(false))&&((b)!=(null))){
var c$1=1;
if(((c$1)!=(false))&&((c$1)!=(null))){
RET=x;
}else{
RET=y;
}
}else{
RET=d;
}\n"]

   [(. (. a + (if flag 0 1)) * (. c + d)) {a a flag flag c c d d}
    (dart/let ([$if_$1 (dart/if (dart/. (dart/. flag "!=" false) "&" (dart/. flag "!=" nil)) 0 1)] [$1 (dart/. a "+" $if_$1)]) (dart/. $1 "*" (dart/. c "+" d)))
    "var $if_$1;
if(((flag)!=(false))&&((flag)!=(null))){
$if_$1=0;
}else{
$if_$1=1;
}
var $1=(a)+($if_$1);
var RET=($1)*((c)+(d));\n"]

   [(loop [a 1] (if test (recur (^:dart inc a)) a)) {test test inc inc a a}
    (dart/loop [[a$1 1]]
      (dart/if (dart/. (dart/. test "!=" false) "&" (dart/. test "!=" nil))
        (dart/recur (inc a$1))
        a$1))
    "var RET;
var a$1=1;
do {
if(((test)!=(false))&&((test)!=(null))){
a$1=inc(a$1, );
continue;
}else{
RET=a$1;
}
break;
} while(true);\n"]

   #_[(loop [a 1 b 2] (recur b a)) {}
    (dart/loop [[a$1 1] [b$1 2]] (dart/recur b$1 a$1))
    "var RET;
var a$1=1;
do {
if(((test)!=(false))&&((test)!=(null))){
a$1=inc(a$1, );
continue;
}else{
RET=a$1;
}
break;
} while(true);\n"]

   [(fn [x] x) {}
    (dart/fn (x$1) :positional () x$1)
    "var RET=(x$1, ){
return x$1;
};\n"]

   [(let [inc (fn [x] (. x "+" 1))] (inc 3)) {}
    (dart/let [[inc$1 (dart/fn (x$1) :positional () (dart/. x$1 "+" 1))]] (inc$1 3))
    "inc$1(x$1, ){
return (x$1)+(1);
}
var RET=inc$1(3, );\n"]

   [(do 1 2 3 4 (^:dart a 1) "ddd") {a a}
    (dart/let [[nil 1] [nil 2] [nil 3] [nil 4] [nil (a 1)]] "ddd")
    "1;
2;
3;
4;
a(1, );
var RET=\"ddd\";\n"]

   [(or 1 2 3 4 (^:dart a 1) "ddd") {a a}
    (dart/let [[or$5516_$AUTO_$1 1]]
      (dart/if (dart/. (dart/. or$5516_$AUTO_$1 "!=" false) "&" (dart/. or$5516_$AUTO_$1 "!=" nil))
        or$5516_$AUTO_$1
        (dart/let [[or$5516_$AUTO_$2 2]]
          (dart/if (dart/. (dart/. or$5516_$AUTO_$2 "!=" false) "&" (dart/. or$5516_$AUTO_$2 "!=" nil))
            or$5516_$AUTO_$2
            (dart/let [[or$5516_$AUTO_$3 3]]
              (dart/if (dart/. (dart/. or$5516_$AUTO_$3 "!=" false) "&" (dart/. or$5516_$AUTO_$3 "!=" nil))
                or$5516_$AUTO_$3
                (dart/let [[or$5516_$AUTO_$4 4]]
                  (dart/if (dart/. (dart/. or$5516_$AUTO_$4 "!=" false) "&" (dart/. or$5516_$AUTO_$4 "!=" nil))
                    or$5516_$AUTO_$4
                    (dart/let [[or$5516_$AUTO_$5 (a 1)]]
                      (dart/if (dart/. (dart/. or$5516_$AUTO_$5 "!=" false) "&" (dart/. or$5516_$AUTO_$5 "!=" nil))
                        or$5516_$AUTO_$5 "ddd"))))))))))
     "var or$5516_$AUTO_$1=1;
var RET;
if(((or$5516_$AUTO_$1)!=(false))&&((or$5516_$AUTO_$1)!=(null))){
RET=or$5516_$AUTO_$1;
}else{
var or$5516_$AUTO_$2=2;
if(((or$5516_$AUTO_$2)!=(false))&&((or$5516_$AUTO_$2)!=(null))){
RET=or$5516_$AUTO_$2;
}else{
var or$5516_$AUTO_$3=3;
if(((or$5516_$AUTO_$3)!=(false))&&((or$5516_$AUTO_$3)!=(null))){
RET=or$5516_$AUTO_$3;
}else{
var or$5516_$AUTO_$4=4;
if(((or$5516_$AUTO_$4)!=(false))&&((or$5516_$AUTO_$4)!=(null))){
RET=or$5516_$AUTO_$4;
}else{
var or$5516_$AUTO_$5=a(1, );
if(((or$5516_$AUTO_$5)!=(false))&&((or$5516_$AUTO_$5)!=(null))){
RET=or$5516_$AUTO_$5;
}else{
RET=\"ddd\";
}
}
}
}
}\n"]

   [((((fn [] (fn [] (fn [] 42)))))) {}
    (dart/let ([f$1 (dart/fn () :positional () (dart/fn () :positional () (dart/fn () :positional () 42)))]
               [$if_$1 (dart/if (dart/is f$1 GLOBAL_cljd.core/IFn) (dart/. (dart/as f$1 GLOBAL_cljd.core/IFn) "$_invoke$0") (f$1))]
               [$if_$2 (dart/if (dart/is $if_$1 GLOBAL_cljd.core/IFn) (dart/. (dart/as $if_$1 GLOBAL_cljd.core/IFn) "$_invoke$0") ($if_$1))])
      (dart/if (dart/is $if_$2 GLOBAL_cljd.core/IFn) (dart/. (dart/as $if_$2 GLOBAL_cljd.core/IFn) "$_invoke$0") ($if_$2)))
    ""])

  (comment

    (run-tests)

    ))






(comment

  (emit-ns '(ns cljd.user
              (:require [cljd.bordeaux :refer [reviews] :as awesome]
                        [cljd.ste :as ste]
                        ["package:flutter/material.dart"]
                        clojure.string)) {})


  (emit '((((fn* [] (fn* [] (fn* [] 42)))))) {})
  ((((dart/fn () () (dart/let () (dart/fn () () (dart/let () (dart/fn () () (dart/let () 42)))))))))
  (write *1 (var-locus "DDDD"))

  (emit '(fn* [x] x) {})
  (dart/fn nil (_$7_) () (dart/let ([x_$8_ _$7_]) x_$8_))
  (write *1 return-locus)

  (emit '(fn* fname [x] 42) {})
  (dart/let [[nil (dart/fn _16623 (_16624) () (dart/let ([_16625 _16624]) 42))]] _16623)
  (write *1 return-locus)

  (emit '((fn* fname [x] 42)) {})
  (dart/let ([nil (dart/fn _16631 (_16632) () (dart/let ([_16633 _16632]) 42))]) (_16631))
  (write *1 return-locus)

  ()

  (emit '(def oo (fn* [x] 42)) {})
  (write *1 return-locus)



  (emit '(def oo1 42) {})


  (emit '(def oo (fn* [x] (if (.-isOdd x) (recur (. x + 1)) x ))) {})
  nses

  (write *1 return-locus)

  (emit '(fn* aa [x] x) {})
  (dart/let [[nil (dart/fn _16717 (_16718) () (dart/let ([_16719 _16718]) _16719))]] _16717)

  (emit '(fn* [] (fn* aa [x] x)) {})
  (dart/fn nil () () (dart/let [[nil (dart/fn aa_$9_ (_$10_) () (dart/let ([x_$11_ _$10_]) x_$11_))]] aa_$9_))
  (dart/fn nil () () (dart/let ([nil (dart/fn _18396 (_18397) () (dart/let ([_18398 _18397]) (GLOBAL_do _18398)))]) (GLOBAL_do _18396)))

  (emit '(reify Object (boo [self x & y 33] (.toString self))) {})
  (GLOBAL__22982)

  (emit '(reify Object (boo [self x ... y 33] (.toString self))) {})
  (GLOBAL__22986)
  (write *1 return-locus)

  (emit '(let [x 42] (reify Object (boo [self] (str x "-" self)))) {})
  (dart/let ([_22991 42]) (GLOBAL__22992 _22991))

  (emit '(let [x 42] (reify Object (boo [self] (let [x 33] (str x "-" self))))) {})
  (dart/let ([x_$4_ 42]) (_reify_$5_))

  (emit '[1 2 3] {})
  (GLOBAL_cljd.core/vec [1 2 3])
  (write *1 expr-locus)

  (emit '[1 (inc 1) [1 1 1]] {})
  (GLOBAL_cljd.core/vec [1 (GLOBAL_inc 1) (GLOBAL_cljd.core/vec [1 1 1])])

  (emit ''[1 (inc 1) [1 1 1]] {})

  (GLOBAL_cljd.core/vec [1 (GLOBAL_inc 1) (GLOBAL_cljd.core/vec [1 1 1])])

  (emit '[1 (inc 1) [(let [x 3] x)]] {})
  (dart/let ([_24320 (GLOBAL_inc 1)] [_24318 3] [_24319 (GLOBAL_cljd.core/vec [_24318])]) (GLOBAL_cljd.core/vec [1 _24320 _24319]))
  (write *1 expr-locus)

  (emit '(let [x (try 1 2 3 4 (catch Exception e e1 (print e) 2 3))] x) {})
  (dart/let ([_17563 (dart/try (dart/let ([nil 1] [nil 2] [nil 3]) 4) (catch Exception [_17564 _17565] (dart/let ([nil (GLOBAL_print _17564)] [nil 2]) 3)))]) _17563)
  (write *1 return-locus)

  (emit '(if (try 1 2 3 4 (catch Exception e "noooo") (finally "log me")) "yeahhh") {})
  (write *1 return-locus)

  (emit '(try (catch E e st)) {})
  (dart/try nil ([E e_$19_ nil GLOBAL_st]) nil)
  (write *1 return-locus)

  (emit '(try 42 33 (catch E e st x) (finally (print "boo"))) {})
  (dart/try (dart/let ([nil 42]) 33) ([E e_$24_ st_$25_ GLOBAL_x]) (GLOBAL_print "boo"))
  (write *1 return-locus)

  (emit '[1 (let [x 2] x) 3] {})
  (dart/let ([__$3_ 2]) (GLOBAL_cljd.core/vec [1 __$3_ 3]))
  (dart/let ([_25768 2]) (GLOBAL_cljd.core/vec [1 _25768 3]))

  (emit '[(f) (let [x 2] x) 3] {})
  (dart/let ([_25772 (GLOBAL_f)] [_25771 2]) (GLOBAL_cljd.core/vec [_25772 _25771 3]))


  (emit '(try 1 2 3 4 (catch Exception e st 1 2)) {})
  (dart/try (dart/let ([nil 1] [nil 2] [nil 3]) 4) (catch Exception [e_$4_ st_$5_] (dart/let ([nil 1]) 2)) (catch Exception [e_$6_] GLOBAL_st))
  (write *1 return-locus)

  (emit '(throw 1) {})
  (dart/throw 1)
  (write *1 (var-locus "prout"))

  (emit '(throw (let [a 1] (. a + 3))) {})
  (dart/let ([a_$28_ 1] [_$29_ (dart/. a_$28_ "+" 3)]) (dart/throw _$29_))
  (write *1 return-locus)




  (emit '(let [a (throw 1)] a) {})
  (dart/let ([a_$50_ (dart/let [[nil (dart/throw 1)]] nil)]) a_$50_)
  (write *1 return-locus)

  (emit '(let [a (throw (if x y z))] a) {})
  (dart/let ([a_$41_ (dart/let [[nil (dart/throw (dart/if GLOBAL_x GLOBAL_y GLOBAL_z))]] nil)]) a_$41_)
  (write *1 return-locus)

  (emit '(try (catch E e (throw e))) {})
  (dart/try nil ([E e_$47_ nil (dart/let [[nil (dart/throw e_$47_)]] nil)]) nil)
  (write *1 return-locus)

  (emit '(loop [] (recur)) {})
  (dart/loop [] (dart/recur))
  (write *1 return-locus)

  (emit '(loop [] (if x (recur))) {})
  (dart/loop [] (dart/if GLOBAL_x (dart/recur) nil))
  (write *1 return-locus)
  (write *2 statement-locus)

  (emit '(reify Object (^:getter hashCode [] 42)
           (^:setter foo [this x] (println x))
           (meth [a b] "regular method")) {})
  (_reify_$8_)

  (emit '(deftype MyClass [^:mutable ^List a b ^Map c]
           :extends (ParentClass. (+ a b) (if 1 2 3))
           Object
           (meth [_ b] (set! a (if (rand-bool) 33 42)))
           (meth2 [this b] (set! (.-a this) "yup"))
           (^:getter hashCode [_] (let [^num n 42] n))) {})


  (emit '(defprotocol IProtocol_ (meth [a] [a b] [a b c]) (-coucou [a])) {})
  (dart/let [[nil IProtocol_$UNDERSCORE_] [nil meth] [nil _coucou]] IProtocol_$UNDERSCORE_)

  (emit '(defprotocol IMarker "This protocol is only a marker") {})
  (dart/let [[nil IMarker]] IMarker)

  (emit '(defprotocol IMarker2 "Docstring" (meth [one] [one two] "Docstring") (ops [one] [one two]) (opa [one] "Coucou")) {})
  (dart/let [[nil IMarker] [nil meth] [nil ops] [nil opa]] IMarker)

  (emit '(deftype MyClass [^:mutable ^List a b ^Map c]
           :extends (ParentClass. (+ a b) (if 1 2 3))
           IMarker
           IProtocol_
           (meth [a] "a")
           (meth [b c] "e")
           (meth [c d e] "oo")) {})
  (dart/let [[nil MyClass]] __$GT_MyClass)

  nses

  (macroexpand-1 {} '(defn aaa "docstring2" [ooo] "content"))

  (emit '(defn aaa "docstring2" [ooo] "content") {})
  (emit '(def ooo "docstirng" 42) {})

  (emit '(dart-is? 0 num) {})
  (dart/is 0 dc.num)

  (emit `(str (case x# 12 "hello" (13 14) "bye")) {})

  (write *1 return-locus)

  (emit `(case 12  12 "hello" (13 14) "bye") {})

  (macroexpand-1 {} '(case 12 12 "hello"))

  (clojure.core/let [test__6312__auto__ 12] (cljd.core/case test__6312__auto__ 12 "hello"))

    )

(comment




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



  (emit '(loop* [a 4 b 5] a (recur b a)) {})
  (dart/loop [[_8698 4] [_8699 4]] (dart/let ([nil _8698]) _8699))
  (write *1 (var-locus "DDDD"))


  )
