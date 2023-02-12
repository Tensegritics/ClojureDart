(ns hooks.flutter2
  "Hook to support cljd.flutter.alpha2 macro"
  (:require [clj-kondo.hooks-api :as api
             :refer [sexpr token-node list-node map-node vector-node]]
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


(defn vector-node->let-binding-nodesr
  "Example:
  Args:  `<vector: [animator (m/AnimationController ...)]`
  Return: `[ [<token: animator> <List: (m/AnimationController .vsync vsync)>] ]`"
  [node]
  (mapv (fn [[k _ :as node-pair]]
          (if (api/keyword-node? k)
            [] ;; to ignore keywords like :dispose, :refresh-on
            node-pair))
        (partition 2 (:children node))))

(defn top-key-value->let-binding-nodes
  "Examples:
  Args: `<token: :get>`, `<vector: [m/MediaQuery]>`
  Return: `[ [<token: media-query> <token: identity>], ... ]`"
  [node-key node-val]
  (case (sexpr node-key)
    :get (get-node->let-binding-nodes node-val)
    (:vsync :context) (vector [(with-meta (token-node (sexpr node-val)) (meta node-val))
                               (token-node 'identity)])
    (:let :managed :watch) (vector-node->let-binding-nodesr node-val)
    :bg-watcher []
    ;; unsupported keys ignored
    []))

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
             (map first))

        let-node
        (list-node
          (list*
            (token-node 'let)
            (vector-node
              (apply concat let-bindings))
            body))]
    ; (clojure.pprint/pprint (sexpr let-node))
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
