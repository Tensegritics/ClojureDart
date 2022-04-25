(ns hooks.flutter
  (:require [clj-kondo.hooks-api :as api :refer [sexpr token-node list-node]]
            [clojure.set :as set]
            [clojure.string :as str]))

(defn- with->bindings
  "Returns :with bindings vector, :let included"
  [with-node]
  (into []
        (mapcat (fn [[lhs rhs :as binding]]
                  (condp = (str lhs)
                    ":dispose" nil
                    ":let" (:children rhs)
                    binding)))
        (partition 2 (:children with-node))))

(defn- bind-identity 
  "Returns node-binding like [symbol identity] or nil"
  [symb]
  (when symb [(token-node symb) (api/token-node 'identity)]))

(defn prns [name node]
  (prn name ": " (api/sexpr node)))

(defn widget 
  "Hook for cljd.flutter.alpha/widget macro"
  [{:keys [:node]}]
  (let [args (rest (:children node))
        opts-nodes (take-while (comp api/keyword-node? first) (partition 2 args))
        pairs->value (fn [k] (first (filter #(-> % first (api/sexpr) (= k)) opts-nodes)))
        [_ with-node] (pairs->value :with)

        {[s-name s-value] :state :keys [state context ticker tickers watch key with] :as opts} 
        (into {} (map (fn [[k v]] (vector (sexpr k) (sexpr v)))) opts-nodes)

        bindings-count (* 2 (count opts-nodes))
        body (drop bindings-count args)
        error (fn [msg] (throw (ex-info msg {})))

        unknown-keys (set/difference 
                       (into #{} (keys opts))
                       #{:state :key :watch :context :with :ticker :tickers})]

    (cond
      (and (seq (filter api/keyword-node? args))   ; has top keywords
           (not (api/keyword-node? (first args)))) ; but first form is not a keyword
      (error (str "Unknown first form " (api/sexpr (first args))))

      (and state watch)
      (error ":state and :watch option keys are mutually exclusive")

      (and ticker tickers)
      (error "Both :ticker and :tickers are not allowed")

      (and with-node 
           (or (not (api/vector-node? with-node))
               (some #(not (or (api/token-node? %) (api/keyword-node? %))) 
                     (take-nth 2 (:children with-node)))))
      (error ":with left hand simbols should be simple simbols, :let, or :dispose forms")

      (and state (or (not (vector? state))
                     (not= (count state) 2)
                     (not (symbol? (first state)))))
      (error ":state should be a vector [name initial-value]")

      (seq unknown-keys)
      (error (str "Unsupported option keys " (str/join " " unknown-keys)))

      (= 0 (- (count args) bindings-count)) 
      (error (str "No body provided"))

      :else 
      (let [result {:node (api/list-node 
               (list*
                 (token-node 'let)
                 (api/vector-node 
                   (concat (bind-identity context) 
                           (bind-identity ticker)
                           (bind-identity tickers)
                           (when state ;; TODO when   
                             [(token-node s-name)
                              (list-node [(token-node 'atom) (token-node s-value)])])
                           (with->bindings with-node)))
                 (when watch (token-node watch))
                 (when key (token-node key))
                 body))}]
        #_(prns :result result)
        result))))
