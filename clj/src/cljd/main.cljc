(ns cljd.main
  (:require ["package:flutter/material.dart" :as material]
            ["package:flutter/foundation.dart" :as foundation]
            ["package:flutter/widgets.dart" :as widgets]
            ["package:flutter/painting.dart" :as painting]
            ["package:english_words/english_words.dart" :as en]

            ["dart:developer" :as dev]
            ["package:webview_flutter/webview_flutter.dart" :as webview]
            ["dart:async" :as async]
            ["dart:convert" :as convert]
            ["dart:io" :as io]))

;; 1 StatefulWidget which builds a InheritedModel en lui passant le state (l'atom)

(deftype RootStatefulWidgetState [])

(deftype RootStatefulWidget [child]
  :extends widgets/StatefulWidget
  (^RootStatefulWidgetState createState [this]
   (RootStatefulWidgetState. child
     {"items" ["clj" "cljs" "cljr" "cljd"]
      "favs" {}})))

(deftype RootInheritedModel [child state data]
  :extends (widgets/InheritedModel. .& :child child)
  (^bool updateShouldNotifyDependent [new-widget old-widget deps]
   (dart:core/print deps)
   (let [before (.-data ^RootInheritedModel old-widget)
         after (.-data new-widget)
         {items-before "items" favs-before "favs"} before
         {items-after "items" favs-after "favs"} after]
     (not (every? (fn [i] (== (get favs-before (nth items-before i nil))
                            (get favs-after (nth items-after i nil)))) deps))))
  (^bool updateShouldNotify [new-widget old-widget] true))

(deftype RootStatefulWidgetState [child ^:mutable data]
  :extends #/(widgets/State RootStatefulWidget)
  (^widgets/Widget build [this ^widgets/BuildContext context]
    (RootInheritedModel. child this data)))

(defn toggle-fav [^RootStatefulWidgetState state word]
  (.setState state
    #(let [data (.-data state)
           favs (get data "favs")
           favs (if (get favs word)
                  (dissoc favs word)
                  (assoc favs word true))
           data (assoc data "favs" favs)]
       (set! (.-data state) data)
       nil)))

(deftype ListWords []
  :extends widgets/StatelessWidget
  (^widgets/Widget build [this ^widgets/BuildContext context]
   (widgets/ListView.builder .&
     :padding (painting/EdgeInsets.all 16.0)
     :itemCount 7
     :itemBuilder
     (fn [^widgets/BuildContext inner-context i]
       (dart:core/print (str "building item #" i))
       (if (.-isOdd i)
         (material/Divider.)
         (widgets/Builder. .&
           :builder
           (fn [^widgets/BuildContext inner-context]
             (let [index (. i "~/" 2)
                   _ (dart:core/print (str "Rebuild word #" index))
                   ^RootInheritedModel? model (.dependOnInheritedWidgetOfExactType inner-context .& :aspect index)
                   ^RootStatefulWidgetState? state (when model (.-state model))
                   words (when model (get (.-data model) "items"))
                   word (nth words index)
                   saved? (get (get (when model (.-data model)) "favs") word false)]
               (material/ListTile. .&
                 :title (material/Text. word .& :style (painting/TextStyle. .& :fontSize 18))
                 :trailing (widgets/Icon. (if saved? (.-favorite material/Icons) (.-favorite_border material/Icons) )
                             .& :color (when saved? (.-red material/Colors)))
                 :onTap (fn []
                          (toggle-fav state word)
                          nil))))))))))

(deftype RandomWords []
  :extends widgets/StatelessWidget
  (^widgets/Widget build [this ^widgets/BuildContext context]
   (material/Scaffold. .&
     :appBar (material/AppBar. .&
               :title (widgets/Text "Clojure host list")
               #_#_:actions #dart [(material/IconButton. .&
                                     :icon (widgets/Icon. (.-list material/Icons))
                                     :onPressed (fn []
                                                  (.push (widgets/Navigator.of context)
                                                    (new #/(material/MaterialPageRoute void) .&
                                                      :builder
                                                      (fn [^widgets/BuildContext c] (SavedWordsWidget.))))
                                                  nil))])
     :body (ListWords.))))

(def main
  (fn* []
    (material/runApp
      (reify
        :extends material/StatelessWidget
        (^widgets/Widget build [_ context]
         (dart:core/print "BUILD OF THE APP")
         (material/MaterialApp. .&
           :title "Welcome to Flutter"
           :home (RootStatefulWidget. (RandomWords.))))))))

(comment

  ;; stream example

  (deftype MyAtom [^#/(async/StreamController List) stream-controller ^#/(Set en/WordPair) saved-suggestions]
    Object
    (addSuggestion [this ^en/WordPair word]
      (when (not (.contains saved-suggestions word))
        (.add saved-suggestions word)))
    (removeSuggestion [this ^en/WordPair word]
      (when (.contains saved-suggestions word)
        (.remove saved-suggestions word)))
    (^:getter ^#/(Set en/WordPair) suggestions [this] saved-suggestions)
    (^:getter ^#/(async/Stream List) stream [this] (.-stream stream-controller))
    (^:getter ^#/(async/StreamController List) streamController [this] stream-controller))

  (def my-atom (MyAtom.
                 (let [^#/(async/StreamController List) sc (async/StreamController.broadcast)]
                          sc)
                 (new #/(Set en/WordPair))))

  (deftype TileWidget [^en/WordPair word-pair ^String id]
    :extends widgets/StatelessWidget
    (^widgets/Widget build [this ^widgets/BuildContext context]
     (new #/(widgets/StreamBuilder List #_(List String en/WordPair bool)) .&
       :stream (.where (.-stream my-atom)
                 (fn [event]
                   (let [id' (.-first ^List event)]
                     (== id' id))))
       :initialData #dart [id word-pair false]
       :builder (fn [context snapshot]
                  (let [data (.-data snapshot)
                        word-pair (when data (aget data 1))
                        saved? (when data (aget data 2))]
                    (material/ListTile. .&
                      :title (material/Text. (.-asPascalCase word-pair) .& :style (painting/TextStyle. .& :fontSize 18))
                      :trailing (widgets/Icon. (if saved? (.-favorite material/Icons) (.-favorite_border material/Icons) )
                                  .& :color (when saved? (.-red material/Colors)))
                      :onTap (fn []
                               (if saved?
                                 (.removeSuggestion my-atom word-pair)
                                 (.addSuggestion my-atom word-pair))
                               (.add (.-sink (.-streamController my-atom)) #dart [id word-pair (if saved? false true)])
                               nil)))))))

  (deftype ListWordsState [])

  (deftype ListWords []
    :extends widgets/StatefulWidget
    (^ListWordsState createState [this]
     (ListWordsState. #dart [])))

  (deftype ListWordsState [^#/(List en/WordPair) suggestions]
    :extends #/(widgets/State ListWords)
    (^widgets/Widget build [this ^widgets/BuildContext context]
     (.buildSuggestions this))

    Object
    (^widgets/Widget buildSuggestions [this]
     (widgets/ListView.builder .&
       :padding (painting/EdgeInsets.all 16.0)
       :itemBuilder
       (fn [context i]
         (if (.-isOdd i)
           (material/Divider.)
           (let [index (. i "~/" 2)]
             (when (<= #_(.-length suggestions) (count suggestions) index)
               (.addAll suggestions (.take (en/generateWordPairs.) 10)))
             (.buildRow this (aget suggestions index)))))))

    (^widgets/Widget buildRow [this ^en/WordPair word-pair]
     (TileWidget. word-pair (.-asPascalCase word-pair))))

  (deftype RandomWords []
    :extends widgets/StatelessWidget
    (^widgets/Widget build [this ^widgets/BuildContext context]
     (material/Scaffold. .&
       :appBar (material/AppBar. .&
                 :title (widgets/Text "Startup Name Generator")
                 :actions #dart [(material/IconButton. .&
                                   :icon (widgets/Icon. (.-list material/Icons))
                                   :onPressed (fn []
                                                (.push (widgets/Navigator.of context)
                                                  (new #/(material/MaterialPageRoute void) .&
                                                    :builder
                                                    (fn [^widgets/BuildContext c] (SavedWordsWidget.))))
                                                nil))])
       :body (ListWords.))))

  (def main
    (fn* []
      (material/runApp
        (reify
          :extends material/StatelessWidget
          (^widgets/Widget build [_ context]
           (material/MaterialApp. .&
             :title "Welcome to Flutter"
             :home (RandomWords.)))))))

  )


















(comment (deftype RandomWordsState [])
         (deftype ListWordsState [])
         (deftype ListWords [])
         (deftype SavedWordsState [])
         (deftype MyInheritedWidgetState [])

         (deftype MyInherited [^foundation/Key key ^widgets/Widget child ^MyInheritedWidgetState data]
           :extends (widgets/InheritedWidget. .& :key key :child child)
           (^bool updateShouldNotify [this ^MyInherited old] true))

         (deftype MyInheritedWidget [^foundation/Key key ^widgets/Widget child]
           :extends (widgets/StatefulWidget. .& :key key)
           (^MyInheritedWidgetState createState [this]
            (MyInheritedWidgetState. (new #/(Set en/WordPair)))))

         (deftype MyInheritedWidgetState [^#/(Set en/WordPair) saved-suggestions]
           :extends #/(widgets/State MyInheritedWidget)
           (^widgets/Widget build [this ^widgets/BuildContext context]
            (MyInherited. (widgets/GlobalKey.) (.-child (.-widget this)) this))

           Object
           (addSuggestion [this ^en/WordPair pair]
             (when (not (.contains saved-suggestions pair))
               (.add saved-suggestions pair)))
           (removeSuggestion [this ^en/WordPair pair]
             (when (.contains saved-suggestions pair)
               (.remove saved-suggestions pair)))
           (^:getter savedSuggestions [this] saved-suggestions))

         (defn ^MyInheritedWidgetState? of [context]
           (let [^MyInherited? result (.dependOnInheritedWidgetOfExactType ^widgets/BuildContext context)]
             (when (not (nil? result))
               (.-data result))))

         (deftype SavedWordsWidget []
           :extends widgets/StatelessWidget
           (^widgets/Widget build [this ^widgets/BuildContext context]
            (let [^MyInheritedWidgetState? state (of context)
                  ^#/(Set en/WordPair) saved-suggestions (if state (.-savedSuggestions state) (new #/(Set en/WordPair)))
                  tiles (.map saved-suggestions (fn [^en/WordPair pair]
                                                  (material/ListTile. .&
                                                    :title (material/Text. (.-asPascalCase pair)
                                                             .& :style (painting/TextStyle. .& :fontSize 18))
                                                    :onTap (fn []
                                                             (.removeSuggestion state pair)))))
                  divided (if (.-isNotEmpty tiles)
                            (.toList (material/ListTile.divideTiles .&
                                       :context context :tiles tiles))
                            #dart ^:fixed ^widgets/Widget [])]
              (material/Scaffold. .&
                :appBar (material/AppBar. .&
                          :title (widgets/Text. "Savedd suggestions"))
                :body (widgets/ListView. .& :children divided)))))

         (deftype RandomWords [^foundation/Key? key]
           :extends (widgets/StatelessWidget. .& :key ^foundation/Key? key)
           (^widgets/Widget build [this ^widgets/BuildContext context]
            (material/Scaffold. .&
              :appBar (material/AppBar. .&
                        :title (widgets/Text "Startup Name Generator")
                        :actions #dart [(material/IconButton. .&
                                          :icon (widgets/Icon. (.-list material/Icons))
                                          :onPressed (fn []
                                                       (.push (widgets/Navigator.of context)
                                                         (new #/(material/MaterialPageRoute void) .&
                                                           :builder
                                                           (fn [^widgets/BuildContext c] (SavedWordsWidget.))))
                                                       nil))])
              :body (ListWords.))))

         (deftype ListWords []
           :extends widgets/StatefulWidget
           (^ListWordsState createState [this]
            (ListWordsState. #dart [])))

         (deftype TileWidget [^en/WordPair word-pair]
           :extends widgets/StatelessWidget
           (^widgets/Widget build [this ^widgets/BuildContext context]
            (let [^MyInheritedWidgetState? state (of context)
                  _ (dart:core/print context)
                  ^#/(Set en/WordPair) saved-suggestions (if state (.-savedSuggestions state) (new #/(Set en/WordPair)))
                  ^bool saved? (.contains saved-suggestions word-pair)]
              (material/ListTile. .&
                :title (material/Text. (.-asPascalCase word-pair) .& :style (painting/TextStyle. .& :fontSize 18))
                :trailing (widgets/Icon. (if saved? (.-favorite material/Icons) (.-favorite_border material/Icons) )
                            .& :color (when saved? (.-red material/Colors)))
                :onTap (fn []
                         (dart:core/print "coucou")
                         (dart:core/print state)
                         (dart:core/print saved-suggestions)
                         (when state
                           (if saved? (.addSuggestion state word-pair) (.removeSuggestion state word-pair)))
                         (.markNeedsBuild this)
                         nil)))))

         (deftype ListWordsState [^#/(List en/WordPair) suggestions]
           :extends #/(widgets/State ListWords)
           (^widgets/Widget build [this ^widgets/BuildContext context]
            (.buildSuggestions this))

           Object
           (^widgets/Widget buildSuggestions [this]
            (widgets/ListView.builder .&
              :padding (painting/EdgeInsets.all 16.0)
              :itemBuilder
              (fn [context i]
                (if (.-isOdd i)
                  (material/Divider.)
                  (let [index (. i "~/" 2)]
                    (when (<= #_(.-length suggestions) (count suggestions) index)
                      (.addAll suggestions (.take (en/generateWordPairs.) 10)))
                    (.buildRow this (aget suggestions index)))))))

           (^widgets/Widget buildRow [this ^en/WordPair word-pair]
            (TileWidget. word-pair)))

         (def main
           (fn* []
             (material/runApp
               (reify
                 :extends material/StatelessWidget
                 (^widgets/Widget build [_ context]
                  (material/MaterialApp. .&
                    :title "Welcome to Flutter"
                    :home (MyInheritedWidget. (foundation/Key. "prout") (RandomWords. (foundation/Key. "papa"))))))))))

#_(deftype RandomWords []
  :extends widgets/StatefulWidget
  (^RandomWordsState createState [this]
   (RandomWordsState.
     #dart []
     (painting/TextStyle. .& :fontSize 18)
     (dart:core/Set.))))

#_(deftype RandomWordsState [^#/(List en/WordPair) suggestions ^painting/TextStyle style ^#/(Set en/WordPair) already-saved]
  :extends #/(widgets/State RandomWords)
  (^widgets/Widget build [this ^widgets/BuildContext context]
   (material/Scaffold. .&
     :appBar (material/AppBar. .&
               :title (widgets/Text. "Yoyo Name Generator")
               :actions #dart [(material/IconButton. .&
                                 :icon (widgets/Icon. (.-list material/Icons))
                                 :onPressed (fn []
                                              (.push (widgets/Navigator.of context)
                                                (new #/(material/MaterialPageRoute void) .&
                                                  :builder
                                                  (fn [^widgets/BuildContext c]
                                                    (widgets/StatefulBuilder. .&
                                                      :builder (fn [c set-state]
                                                                 (let [tiles (.map already-saved (fn [^en/WordPair pair]
                                                                                                   (material/ListTile. .&
                                                                                                     :title (material/Text. (.-asPascalCase pair)
                                                                                                              .& :style style)
                                                                                                     :onTap (fn []
                                                                                                              (dev/log "coucou")
                                                                                                              (.setState this (fn [] (.remove already-saved pair)))
                                                                                                              (set-state (fn []))))))
                                                                       divided (if (.-isNotEmpty tiles)
                                                                                 (.toList (material/ListTile.divideTiles .&
                                                                                            :context c :tiles tiles))
                                                                                 #dart ^:fixed ^widgets/Widget [])]
                                                                   (material/Scaffold. .&
                                                                     :appBar (material/AppBar. .&
                                                                               :title (widgets/Text. "Savedd suggestions"))
                                                                     :body (widgets/ListView. .& :children divided))))))))))])
     :body (.buildSuggestions this)))

  Object
  (^widgets/Widget buildSuggestions [this]
   (widgets/ListView.builder .&
     :padding (painting/EdgeInsets.all 16.0)
     :itemBuilder
     (fn [context i]
       (if (.-isOdd i)
         (material/Divider.)
         (let [index (. i "~/" 2)]
           (when (<= #_(.-length suggestions) (count suggestions) index)
             (.addAll suggestions (.take (en/generateWordPairs.) 10)))
           (.buildRow this (aget suggestions index)))))))

  (^widgets/Widget buildRow [this ^en/WordPair word-pair]
   (let [saved? (.contains already-saved word-pair)]
     (material/ListTile. .&
       :title (material/Text. (.-asPascalCase word-pair) .& :style style)
       :trailing (widgets/Icon. (if saved? (.-favorite material/Icons) (.-favorite_border material/Icons) )
                   .& :color (when saved? (.-red material/Colors)))
       :onTap (fn [] (.setState this #(if saved? (.remove already-saved word-pair) (.add already-saved word-pair))))))))
