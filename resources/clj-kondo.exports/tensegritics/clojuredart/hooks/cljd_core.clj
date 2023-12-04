(ns hooks.cljd-core
  (:require [clj-kondo.hooks-api :as api]))

(defn rewrite-catch-block
  "Rewrites stacktrace-catches into lets:
   (catch Exception _ex st (println st)) ->
     (catch Exception _ex (let [st (StackTrace.)] (println st)))"
  [[head type ex-sym st-sym? & body]]
  (if (and (api/token-node? st-sym?)
           (simple-symbol? (api/sexpr st-sym?)))
    (list
     head type ex-sym
     (api/list-node
      (list*
       (api/token-node 'let)
       (api/vector-node
        (list
         st-sym?
         (api/list-node
          (list (api/token-node 'StackTrace)))))
       body)))
    (list*
     head type ex-sym
     st-sym?
     body)))

(defn stacktrace-catch
  [item]
  (update-in
   item [:node :children]
   (fn [children]
     (map
      (fn [child]
        (if (= "catch" (some-> child :children first api/sexpr name))
          (update child :children rewrite-catch-block)
          child))
      children))))
