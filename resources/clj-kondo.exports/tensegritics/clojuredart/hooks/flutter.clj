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

(defn- ^:macro-support camel-kebab [s]
  (str/replace s #"(^[A-Z])|[A-Z]" 
               (fn [[s start]] (cond->> (str/lower-case s) (not start) (str "-")))))

(defn- bind-identity 
  "Returns node-binding like [symbol identity] or nil"
  [symb]
  (when symb [(token-node symb) (api/token-node 'identity)]))

(defn- inherit->bindings [form]
  (cond 
    ;; {:of [:a/b m/Theme]} -> [:a/b m/Theme] 
    (:of form)
    (inherit->bindings (:of form))


    ;; [:a/b m/Theme] -> {b :a/b, theme m/Theme}
    (vector? form)
    (->> form
         (map (fn [e]
                (if (keyword? e)
                  [(symbol (name e)) e] ;; using name to get rid of prefix: a.b/c -> c
                  [(symbol (camel-kebab (-> e name))) e])))
         (into {})
         inherit->bindings)

    :else (->> form keys (mapcat bind-identity))))

(defn- with->binding+dispose 
  "Returns seq with sequences like ([name value :dispose form] ...)"
  [with]
  (filter #(-> % (nth 2) (= :dispose)) (partition 4 1 (list* nil nil with))))

(defn- find! [condition node msg]
  (when condition 
    (api/reg-finding! (assoc (meta node) :message msg :type :flutter/widget))))

(defn widget 
  "Hook for cljd.flutter.alpha/widget macro"
  [{:keys [:node]}]
  (let [args (rest (:children node))
        opts-nodes (take-while (comp api/keyword-node? first) (partition 2 args))
        pairs->value (fn [k] (first (filter #(-> % first (api/sexpr) (= k)) opts-nodes)))
        [_ with-node] (pairs->value :with)

        {:keys [state context ticker tickers watch key with bind nested-in inherit] :as opts} 
        (into {} (map (fn [[k v]] (vector (sexpr k) (sexpr v)))) opts-nodes)

        bindings-count (* 2 (count opts-nodes))
        body (drop bindings-count args)

        unknown-keys (set/difference 
                       (into #{} (keys opts))
                       #{:state :key :watch :context :with :ticker 
                         :tickers :bind :nested-in :inherit})]

    (prn :inherit (inherit->bindings inherit))

    (find! (not= (count opts-nodes) (count (keys opts)))
           node
           "Duplicate keys")

    (find! (= 0 (- (count args) bindings-count)) 
           node
           (str "No body provided"))
    
    (find! (and (seq (filter api/keyword-node? args))   ; has top keywords
                (not (api/keyword-node? (first args)))) ; but first form is not a keyword
           (first args)
           (str "Unknown first form " (api/sexpr (first args))))

    (find! (and bind 
                (or (not (map? bind))
                    (some #(-> % keyword? not) (keys bind))))
           (first (pairs->value :bind))
           ":bind should be a map with toplevel keys as clojure keywords")

    (find! (and nested-in 
                (or (not (vector? nested-in)) (empty? nested-in)))
           (first (pairs->value :nested-in))
           ":nested-in should be a vector of widgets with :child binding omitted")

    (find! (and state watch)
           (first (pairs->value :state))
           ":state and :watch option keys are mutually exclusive")

    (find! (and with 
                (or (not (vector? with))
                    (some #(not (or (symbol? %) (= :let %) (= :dispose %))) (take-nth 2 with))))
           with-node 
           ":with left hand simbols should be simple simbols, :let, or :dispose")

    (find! (and ticker tickers)
           (first (pairs->value :ticker))
           "Both :ticker and :tickers are not allowed")

    (find! (some
             (fn [[disposable _ _ dispose-form]]
               (or (not (symbol? disposable))
                   (not (or (symbol? dispose-form) (seq? dispose-form)))))
             (with->binding+dispose with))
           with-node
           ":dispose should locate right after disposable pair and be an expression form")

    (find! (and state (or (not (vector? state))
                          (not= (count state) 2)
                          (not (symbol? (first state)))))
           (first (pairs->value :state))
           ":state should be a vector [name initial-value]")

    (find! (seq unknown-keys)
           node
           (str "Unsupported option keys " (str/join " " unknown-keys)))

    {:node (api/list-node 
               (list*
                 (token-node 'let)
                 (api/vector-node 
                   (concat (bind-identity context) 
                           (bind-identity ticker)
                           (bind-identity tickers)
                           (when (vector? state)  
                             [(with-meta (token-node (first state)) 
                                         (meta (-> :state pairs->value second :children first)))
                              (list-node [(token-node 'atom) (token-node (second state))])])
                           (with->bindings with-node)
                           (inherit->bindings inherit)))
                 (when watch (with-meta (token-node watch) (meta (second (pairs->value :watch)))))
                 (when key (with-meta (token-node key) (meta (second (pairs->value :key)))))
                 body))}))
