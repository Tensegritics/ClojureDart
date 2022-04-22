(ns hooks.flutter
  (:require [clj-kondo.hooks-api :as api]
            [clojure.set :as set]
            [clojure.string :as str]))

(defn widget 
  "Hook for cljd.flutter.alpha/widget macro"
  [{:keys [:node]}]
    (let [args (rest (:children node))
          opts-nodes (take-while (comp api/keyword-node? first) (partition 2 args))
          pairs->value (fn [k] (first (filter #(-> % first (api/sexpr) (= k)) opts-nodes)))
          [_ state] (pairs->value :state)
          [s-name s-value] (:children state)
          [_ context] (pairs->value :context)
          [_ watch] (pairs->value :watch)
          [_ key] (pairs->value :key)

          bindings-count (* 2 (count opts-nodes))
          body (drop bindings-count args)
          error (fn [msg] (throw (ex-info msg {})))
          unknown-keys (set/difference 
                         (into #{} (map (comp api/sexpr first) opts-nodes))
                         #{:state :key :watch :context})]

      (cond
        (and (seq (filter api/keyword-node? args))   ; has top keywords
             (not (api/keyword-node? (first args)))) ; but first form is not a keyword
        (error (str "Unknown first form " (api/sexpr (first args))))

        (and state watch)
        (error ":state and :watch option keys are mutually exclusive")

        (and state (or (not (api/vector-node? state))
                       (not= (count (:children state)) 2)
                       (not (api/token-node? s-name))))
        (error ":state should be a vector [name initial-value]")

        (seq unknown-keys)
        (error (str "Unsupported option keys " (str/join " " unknown-keys)))

        (= 0 (- (count args) bindings-count)) 
        (error (str "No body provided"))

        :else 
        {:node (api/list-node 
                 (list
                   (api/token-node 'let)
                   (api/vector-node (conj (if context 
                                              [context (api/token-node 'identity)]
                                              []) 
                                            s-name 
                                            (api/list-node [(api/token-node 'atom) s-value])))
                   watch ;; to lint 'watch and 'key  
                   key
                   body))})))
