(ns hooks.flutter
  (:require [clj-kondo.hooks-api :as api]
            [clojure.set :as set]
            [clojure.string :as str]))

(defn- with->bindings
  "Returns :with bindings vector, :let included"
  [with]
  (into []
        (mapcat (fn [[lhs rhs :as binding]]
                  (condp = (str lhs)
                    ":dispose" nil
                    ":let" (:children rhs)
                    binding)))
        (partition 2 (:children with))))

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
          [_ ticker] (pairs->value :ticker)
          [_ tickers] (pairs->value :tickers)
          [_ with] (pairs->value :with)
          bindings-count (* 2 (count opts-nodes))
          body (drop bindings-count args)
          error (fn [msg] (throw (ex-info msg {})))
          unknown-keys (set/difference 
                         (into #{} (map (comp api/sexpr first) opts-nodes))
                         #{:state :key :watch :context :with :ticker :tickers})]
      
      (cond
        (and (seq (filter api/keyword-node? args))   ; has top keywords
             (not (api/keyword-node? (first args)))) ; but first form is not a keyword
        (error (str "Unknown first form " (api/sexpr (first args))))

        (and state watch)
        (error ":state and :watch option keys are mutually exclusive")

        (and ticker tickers)
        (error "Both :ticker and :tickers are not allowed")

        (and with 
             (or (not (api/vector-node? with))
                 (some #(not (or (api/token-node? %) (api/keyword-node? %))) 
                       (take-nth 2 (:children with)))))
        (error ":with left hand simbols should be simple simbols, :let, or :dispose forms")

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
                   (api/vector-node 
                     (concat (if context [context (api/token-node 'identity)] []) 
                             (if ticker [ticker (api/token-node 'identity)] [])
                             (if tickers [tickers (api/token-node 'identity)] [])
                             (if state [s-name (api/list-node [(api/token-node 'atom) s-value])] [])
                             (with->bindings with)))
                   watch ;; to lint 'watch and 'key  
                   key
                   body))})))
