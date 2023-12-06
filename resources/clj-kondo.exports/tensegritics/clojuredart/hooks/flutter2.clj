(ns hooks.flutter2
  "Hook to support cljd.flutter.alpha2 macro"
  (:require [clj-kondo.hooks-api :as api
             :refer [sexpr token-node list-node vector-node]]
            [clojure.string :as str]))

(defn- ^:macro-support camel-kebab [s]
  (str/replace s #"(^[A-Z])|[A-Z]"
               (fn [[s start]] (cond->> (str/lower-case s) (not start) (str "-")))))

(defn get-node->let-binding-nodes
  "Examples:
  Args: `<vector: [:counters]`
  Return: `[ [<token: counters> <token: identity>] ] `"
  [get-node]
  (cond
    (api/vector-node? get-node)
    (map (fn [e]
           [(-> e sexpr name camel-kebab symbol token-node (with-meta (meta e)))
            (token-node 'identity)])
         (:children get-node))

    (api/map-node? get-node)
    (mapcat (fn [[k v :as kv]]
              (cond
                (api/keyword-node? k) (get-node->let-binding-nodes v)
                :else [kv]))
            (partition 2 (:children get-node)))

    :else []))

(comment
  (sexpr
    (list-node
      (list
        (token-node 'let)
        (vector-node
          (apply concat (get-node->let-binding-nodes (api/parse-string "{{{style displayLarge} textTheme} m/Theme :value-of [:counters]}")))))))
  ,)

(defn watch-vector-node->let-binding-nodes
  "Example:
  Args:  `<vector: [animator (m/AnimationController ...)]`
  Return: `[ [<token: animator> <List: (m/AnimationController .vsync vsync)>] ]`"
  ([node]
   (watch-vector-node->let-binding-nodes node :watch))
  ([node opt-name]
   (loop [pairs (partition 2 (:children node))
          last-binding-pair nil
          ;;  {:name hoge
          ;;   :value (rd/subscribe ,,,)
          ;;   :as true
          ;;   :dispose true
          ;;   :dedup true
          ;;   :> true}
          aggr []]
     (let [[k v :as pair] (first pairs)]
       (if pair
         (if (api/keyword-node? k)
           (let [kw (sexpr k)]
             (when (kw last-binding-pair)
               (api/reg-finding!
                (assoc (meta k)
                       :message (str "duplicate " kw " option in " opt-name " for `"
                                     (:name last-binding-pair) "`")
                       :type :flutter/widget)))
             (case kw
               :as
               (recur
                (rest pairs)
                (assoc last-binding-pair :as true)
                (conj aggr [v (:value last-binding-pair)]))

               (:dispose :>)
               (recur
                (rest pairs)
                (assoc last-binding-pair kw true)
                (conj aggr
                      [(api/token-node '_)
                       (api/list-node
                        (list (api/token-node '->) v))]))

               (:dedup :refresh-on)
               (recur
                (rest pairs)
                (assoc last-binding-pair kw true)
                (conj aggr [(api/token-node '_) v]))

              ;; else unknown kw
               (do
                 (api/reg-finding!
                  (assoc (meta k)
                         :message (str "unknown keyword option to " opt-name " `" kw "`")
                         :type :flutter/widget))
                 (recur
                  (rest pairs)
                  last-binding-pair
                  aggr)))) ;; to ignore unknown keywords
           (recur
            (rest pairs)
            {:name k
             :value v}
            (conj aggr
                  [k (let [node (api/list-node
                                 (list (api/token-node 'deref) v))]
                       (with-meta node (meta v)))])))
         aggr)))))

(defn managed-vector-node->let-binding-nodes
  [node]
  (loop [pairs (partition 2 (:children node))
         last-binding-pair nil
         ;;  {:name hoge
         ;;   :value (rd/subscribe ,,,)
         ;;   :refresh-on true
         ;;   :dispose true}
         aggr []]
    (let [[k v :as pair] (first pairs)]
      (if pair
        (if (api/keyword-node? k)
          (let [kw (sexpr k)]
            (when (kw last-binding-pair)
              (api/reg-finding!
               (assoc (meta k)
                      :message (str "duplicate " kw " option in :managed for `"
                                    (:name last-binding-pair) "`")
                      :type :flutter/widget)))
            (case kw
              :dispose
              (recur
               (rest pairs)
               (assoc last-binding-pair kw true)
               (conj aggr
                     [(api/token-node '_)
                      (api/list-node
                       (list (api/token-node '->) v))]))

              :refresh-on
              (recur
               (rest pairs)
               (assoc last-binding-pair kw true)
               (conj aggr [(api/token-node '_) v]))

              ;; else unknown kw
              (do
                (api/reg-finding!
                 (assoc (meta k)
                        :message (str "unknown keyword option to :managed `" kw "`")
                        :type :flutter/widget))
                (recur
                 (rest pairs)
                 last-binding-pair
                 aggr))))
          (recur
           (rest pairs)
           {:name k
            :value v}
           (conj aggr pair)))
        aggr))))

(defn vector-node->let-binding-nodes
  "Example:
  Args:  `<vector: [animator (m/AnimationController ...)]`
  Return: `[ [<token: animator> <List: (m/AnimationController .vsync vsync)>] ]`"
  [node]
  (mapv (fn [[k _ :as node-pair]]
          (if (api/keyword-node? k)
            [] ;; to ignore keywords like :dispose, :refresh-on
            node-pair))
        (partition 2 (:children node))))

(defn vector-node->bg-watcher-node
  [node]
  (update node :children
          (fn [[watcher & body]]
            (let [watch-args (watch-vector-node->let-binding-nodes watcher :bg-watcher)]
              (list*
               (api/token-node 'let)
               (vector-node (apply concat watch-args))
               body)))))

(defn bind-node->let-binding-nodes
  [node]
  (if (api/map-node? node)
    (mapv (fn [[_ v]]
            [(api/token-node '_) v])
          (partition 2 (:children node)))
    (do
      (api/reg-finding!
       (assoc (meta node)
              :message (str ":bind needs a map parameter")
              :type :flutter/widget))
      [])))

(defn top-key-value->let-binding-nodes
  "Examples:
  Args: `<token: :get>`, `<vector: [m/MediaQuery]>`
  Return: `[ [<token: media-query> <token: identity>], ... ]`"
  [node-key node-val]
  (case (sexpr node-key)
    :get (get-node->let-binding-nodes node-val)
    :bind (bind-node->let-binding-nodes node-val)
    (:key :keep-alive :spy :padding :color :width :height :visible) [[(token-node '_) node-val]]
    (:vsync :context) (vector [(with-meta (token-node (sexpr node-val)) (meta node-val))
                               (token-node 'identity)])
    :let (vector-node->let-binding-nodes node-val)
    :managed (managed-vector-node->let-binding-nodes node-val)
    :watch (watch-vector-node->let-binding-nodes node-val)
    :bg-watcher [[(token-node '_) (vector-node->bg-watcher-node node-val)]]
    ;; unsupported keys ignored
    []))

(defn nest-body
  [[head & tail]]
  (if (seq? tail)
    (cond
      (api/token-node? head)
      (nest-body
       (list* (api/list-node
               (list head))
              tail))

      (api/list-node? head)
      (let [[maybe-prop & followup] tail
            nested-body
            (if (and (api/token-node? maybe-prop)
                     (simple-symbol? (sexpr maybe-prop))
                     (.startsWith (name (sexpr maybe-prop)) "."))
              (list maybe-prop (nest-body followup))
              (list (api/token-node '.child)
                    (nest-body (list* maybe-prop followup))))]
        (update
         head :children
         (fn [children]
           (concat children nested-body))))

      :else
      (api/list-node
       (list head (nest-body tail))))
    (if (api/token-node? head)
      (api/list-node
       (list head))
      head)))

(defn widget
  "The idea is to have all of the bindings in let form and everything else in the body.

  ```
  (f/widget
    :let [a 1]
    :watch [n s1]
    (m/Text (str n a) .textAlign m/TextAlign.center .style text-style)) ;; =>

  (let [n s1, a 1]
    (m/Text (str n a) .textAlign m/TextAlign.center .style text-style))
  ```"
  [{:keys [node]}]
  (let [children
        (-> node :children rest)

        {:keys [node-keys node-values]}
        (->> children
             (map-indexed (fn [i r] [r i]))
             (group-by (fn [[n _]]
                         (if (api/keyword-node? n)
                           :node-keys
                           :node-values))))

        let-bindings
        (mapcat (fn [[n i]]
                  (top-key-value->let-binding-nodes n (nth children (inc i))))
                node-keys)

        body
        (->> node-values
             (remove (fn [[_ i]]
                       (when (> i 0)
                         (api/keyword-node? (nth children (dec i))))))
             (map first)
             nest-body)

        let-node
        (list-node
         (list
          (token-node 'let)
          (vector-node
           (apply concat let-bindings))
          body))]
    ;; (clojure.pprint/pprint (sexpr let-node))
    {:node let-node}))

(comment

  ;; Use REPL https://github.com/clj-kondo/clj-kondo/blob/master/doc/hooks.md#developing-hooks-in-the-repl

  ;; Or test with terminal:
  ;; clj-kondo --lint samples/fetch-data/src/sample/fetch_data.cljd --config-dir resources/clj-kondo.exports/tensegritics/clojuredart

  (sexpr
   (:node
     (widget
       {:node
        (api/parse-string
          "
          (f/widget
            :get [:counters]
            :let [_ (dart:core/print 1)]
            (m/ElevatedButton .onPressed #(swap! counters update k inc))
            (m/Text +))
          ")}))))

(defn analyze-build
  "Rewrite build into an fn with a f/widget call as the body"
  [{:keys [node]}]
  (let [[argsvec? & body :as children] (-> node :children rest)
        widget-node
        (->> (api/list-node
              (list*
               (api/token-node 'cljd.flutter/widget)
               (if (api/vector-node? argsvec?)
                 body
                 children)))
             (assoc {} :node)
             widget :node)
        result
        (api/list-node
         (list
          (api/token-node 'fn)
          (if (api/vector-node? argsvec?)
            argsvec?
            (api/vector-node []))
          widget-node))]
    {:node result}))
