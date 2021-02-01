(ns cljd.core
  (:refer-clojure :exclude [reify deftype defprotocol definterface case])
  (:require [clojure.core :as clj]))

(comment

  (defmacro case [expr & clauses]
    (if (or (symbol? expr) (odd? (count clauses)))
      (let [clauses (vec (partition-all 2 clauses))
            last-clause (peek clauses)
            clauses (cond-> clauses (nil? (next last-clause)) pop)
            default (if (next last-clause)
                      `(throw (ex-info (str "No matching clause: " ~expr) {:value ~expr}))
                      (first last-clause))]
        (list 'case* expr (for [[v e] clauses] [(if (seq? v) v (list v)) e]) default))
      `(let [test# ~expr] (case test# ~@clauses))))

  )

(comment

  (defn- roll-leading-opts [body]
    (loop [[k v & more :as body] (seq body) opts {}]
      (if (and body (keyword? k))
        (recur more (assoc opts k v))
        [opts body])))



  (defn- expand-opts+specs [opts+specs]
    (let [[opts specs] (roll-leading-opts opts+specs)]
      (cons opts
            (map
             (fn [spec]
               (if (seq? spec)
                 (let [[mname arglist & body] spec
                       [positional-args [delim & opt-args]] (split-with (complement '#{.& ...}) arglist)
                       delim (clj/case delim .& :named :positional)
                       opt-params
                       (for [[p d] (partition-all 2 1 opt-args)
                             :when (symbol? p)]
                         [p (when-not (symbol? d) d)])]
                   ;; TODO: mname resolution against protocol ifaces
                   (list* mname positional-args delim opt-params body))
                 spec))
             specs))))

  (defmacro reify [& args]
    `(reify* ~@(expand-opts+specs args)))

  (defmacro deftype [class-name fields & args]
    `(do
       (deftype* ~class-name ~fields
         ~@(expand-opts+specs args))
       (defn ~(symbol (str "->" class-name))
         [~@fields]
         (new ~class-name ~@fields))))

  (defmacro definterface [iface & meths]
    `(deftype* ~(vary-meta iface assoc :abstract true) []
       ~@(expand-opts+specs (for [[meth args] meths]
                              (list meth (into '[_] args))))))

  (defmacro defprotocol [proto & methods]
    ;; TODO do something with docstrings
    (let [[doc-string & methods] (if (string? (first methods)) methods (list* nil methods))
          method-mapping
          (into {} (map (fn [[m & arglists]]
                          (let [dart-m (munge m)
                                [doc-string & arglists] (if (string? (last arglists)) (reverse arglists) (list* nil arglists))]
                            [(with-meta m {:doc doc-string}) (into {} (map #(let [l (count %)] [l {:dart/name (symbol (str dart-m "$" l))
                                                                                                   :args %}]))
                                                                   arglists)]))) methods)
          protocol-meta {:sigs method-mapping}
          class-name (vary-meta proto assoc :protocol protocol-meta)]
      `(do
         (definterface ~class-name
           ~@(for [[method arity-mapping] method-mapping
                   {:keys [dart/name args]} (vals arity-mapping)]
               (list name (subvec args 1))))
         ~@(for [[method arity-mapping] method-mapping]
             `(defn ~method
                ~@(for [{:keys [dart/name args]} (vals arity-mapping)]
                    `(~args
                      (if (dart-is? ~(first args) ~class-name)
                        (. ~(first args) ~name ~@(next args))
                        #_TODO_EXTENSIONS)))))
         ~class-name))))



(comment (defmacro sfsd [sdf] dsfd dsf sd)

         (defmacro defmaquereau [name args & body]
           `(defn ~(vary-meta name assoc :macro true) ~(into '[&form &env] args) ~@body))

         (defmaquereau nom [one two]
           `(list ~one ~two))

         (macroexpand '(nom 1 2))



         (defprotocol IFn
           (-invoke [this] [this a] [this a b]))

         (defn cons []))


;; S0 : sources du compilo qui interprète abstract et qui utilise abstract
;; C0 compilo résultatnt de S0
;; S1 : S0 sauf que le compilo a été modifié pour comprendre recondite et qui utilise abstract
;; C0(S1) -> C1
;; S2 : S1 sauf que les usages de asbtract ont ét traduits en recondite
;; C1(S2) -> C2

;; S0 : compilo c qui target x64
;; S1 : compilo c qui target ARM
;; C0(S1) -> C1 compilateur c (binaire x64) qui génère du ARM
;; S2 : tweaks sur S1 pour accomoder le changementde plateforme ()
;; C1(S2) -> C2 compilo c (binaire arm ) qui génère du arm
