(ns cljd.core
  (:refer-clojure :exclude [reify deftype defprotocol definterface]))

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
                     delim (case delim .& :named :positional)
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
                  `(~args (. ~(first args) ~name ~@(next args))))))
       ~class-name)))
