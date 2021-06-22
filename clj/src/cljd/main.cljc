(ns cljd.main
  (:require ["package:flutter/material.dart" :as material]
            ["package:flutter/widgets.dart" :as widgets]
            ["package:flutter/painting.dart" :as painting]
            ["package:english_words/english_words.dart" :as en]

            ["dart:developer" :as dev]))

(deftype RandomWordsState [])

(deftype RandomWords []
  :extends widgets/StatefulWidget
  (^RandomWordsState createState [this]
   (RandomWordsState.
     #dart []
     (painting/TextStyle. .& :fontSize 18)
     (dart:core/Set.))))

(deftype RandomWordsState [^#/(List en/WordPair) suggestions ^painting/TextStyle style ^#/(Set en/WordPair) already-saved]
  :extends #/(widgets/State RandomWords)
  (^widgets/Widget build [this ^widgets/BuildContext context]
   (material/Scaffold. .&
     :appBar (material/AppBar. .&
               :title (widgets/Text. "Yoyo Name Generator")
               :actions #dart [(material/IconButton. .&
                                 :icon (widgets/Icon. (.-list material/Icons))
                                 :onPressed #(.push (widgets/Navigator.of context)
                                               (new #/(material/MaterialPageRoute void) .&
                                                 :builder (fn [^widgets/BuildContext c]
                                                            (let [tiles (.map already-saved (fn [^en/WordPair pair]
                                                                                              (material/ListTile. .&
                                                                                                :title (material/Text. (.-asPascalCase pair)
                                                                                                         .& :style style)
                                                                                                :onTap (fn [] (dev/log "coucou")))))
                                                                  divided (if (.-isNotEmpty tiles)
                                                                            (.toList (material/ListTile.divideTiles .&
                                                                                       :context c :tiles tiles))
                                                                            #dart ^:fixed ^widgets/Widget [])]
                                                              (material/Scaffold. .&
                                                                :appBar (material/AppBar. .&
                                                                          :title (widgets/Text. "Saved suggestions"))
                                                                :body (widgets/ListView. .& :children divided)))))))])
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


(def main
  (fn* []
    (material/runApp
      (reify
        :extends material/StatelessWidget
        (^widgets/Widget build [_ context]
         (material/MaterialApp. .&
           :title "Welcome to Flutter"
           :home (RandomWords.)))))))
