(ns cljd.reader
  (:require ["dart:io" :as io]
            ["dart:async" :as async]))

;; ReaderInput class definition
(deftype ReaderInput [^#/(async/Stream String) in
                      ^:mutable ^#/(async/StreamSubscription? String) subscription
                      ^:mutable ^#/(async/Completer? String?) completer
                      ^:mutable ^String? buffer]
  (^void init_stream_subscription [this]
   (set! subscription
     (doto (.listen in
             (fn [^String s]
               (assert (nil? buffer))
               (when-not (== "" s)
                 (.complete completer s)
                 (.pause subscription))) .&
             :onDone
             (fn []
               (assert (nil? buffer))
               (.complete completer nil)
               (set! subscription nil)
               nil))
       (.pause)))
   nil)
  (^#/(async/FutureOr String?) read [this]
   (when-not (nil? subscription)
     (if-some [buf buffer]
       (do (set! buffer nil)
           (async.Future/value buf))
       (do (set! completer (async/Completer.))
           (.resume subscription)
           (.-future completer)))))
  (^void unread [this ^String s]
   (assert (nil? buffer))
   (assert (not (nil? subscription)))
   (set! buffer (when-not (== "" s) s))
   nil)
  (^#/(async/FutureOr void) close [this]
   (when-some [sub subscription]
     (let [cancel (.cancel sub)]
       (set! subscription nil)
       cancel))))

(defn ^ReaderInput make-reader-input [^#/(async/StreamController String) controller]
  (doto (ReaderInput. (.-stream controller) nil nil nil) (.init_stream_subscription)))

;; read-* functions
(declare ^:async ^:dart read)

(defn ^int cu0 [^String ch] (.codeUnitAt ch 0))

(defn ^#/(Future cljd.core/PersistentList) ^:async read-list [^ReaderInput rdr]
  (let [result #dart[]]
    (loop []
      (let [val (await (read rdr (cu0 ")")))]
        (if (== val rdr)
          (-list-lit result)
          (do (.add result val)
              (recur)))))))

(defn ^#/(Future cljd.core/PersistentHashMap) ^:async read-hash-map [^ReaderInput rdr]
  (let [result #dart[]]
    (loop []
      (let [val (await (read rdr (cu0 "}")))]
        (if (== val rdr)
          (if (zero? (bit-and 1 (.-length result)))
            (-map-lit result)
            (throw (FormatException. "Map literal must contain an even number of forms")))
          (do (.add result val)
              (recur)))))))

(defn ^#/(Future cljd.core/PersistentVector) ^:async read-vector [^ReaderInput rdr]
  (let [result #dart[]]
    (loop []
      (let [val (await (read rdr (cu0 "]")))]
        (if (== val rdr)
          (if (< 32 (.-length result))
            (vec result)
            (-vec-owning result))
          (do (.add result val)
              (recur)))))))

(def macros
  {"(" ^:async (fn [^ReaderInput rdr] (await (read-list rdr)))
   ")" ^:async (fn [_] (throw (FormatException. "EOF while reading, starting at line")))
   "{" ^:async (fn [^ReaderInput rdr] (await (read-hash-map rdr)))
   "}" ^:async (fn [_] (throw (FormatException. "EOF while reading, starting at line")))
   "[" ^:async (fn [^ReaderInput rdr] (await (read-vector rdr)))
   "]" ^:async (fn [_] (throw (FormatException. "EOF while reading, starting at line")))
   "'" ^:async (fn [^ReaderInput rdr] (list (symbol nil "quote") (await (read rdr -1))))})

(def ^RegExp SPACE-REGEXP #"[\s,]*")

(defn ^bool terminating? [^int code-unit]
  (let [ch (String/fromCharCode code-unit)]
    (cond
      (< -1 (.indexOf "'#" ch)) false
      (macros ch) true
      (< 0 (or (some-> (.matchAsPrefix SPACE-REGEXP ch) .end) 0)) true
      :else false)))

(defn ^#/(Future String) ^:async read-token [^ReaderInput rdr]
  (let [sb (StringBuffer.)]
    (loop [^int index 0
           ^String string ""]
      (if (== index (.-length string))
        (do (.write sb string)
            (when-some [s (await (.read rdr))]
              (recur 0 s)))
        (let [cu (.codeUnitAt string index)]
          (if (terminating? cu)
            (do (.write sb (.substring string 0 index))
                (.unread rdr (.substring string index)))
            (recur (inc index) string)))))
    (.toString sb)))

(defn interpret-token [^String token]
  (case token
    "nil" nil
    "true" true
    "false" false))

(defn ^#/(Future dynamic) ^:async read
  [^ReaderInput rdr ^int delim]
  (loop []
    (if-some [string (await (.read rdr))]
      (let [index (or (some-> (.matchAsPrefix SPACE-REGEXP string) .end) 0)]
        (if (== index (.-length string))
          (recur)
          (let [ch (.codeUnitAt string index)]
            (if (== delim ch)
              (doto rdr (.unread (.substring string (inc index))))
              (if-some [macro-reader (macros (aget string index))]
                (do (.unread rdr (.substring string (inc index)))
                    (let [val (await (macro-reader rdr))]
                      (if (== val rdr)
                        (recur)
                        val)))
                (do (.unread rdr (.substring string index))
                    (-> (await (read-token rdr)) interpret-token)))))))
      (if (< delim 0)
        rdr
        (throw (FormatException. (str "Unexpected EOF, expected " (String/fromCharCode delim))))))))

(defn ^#/(Future dynamic) ^:async read-string [^String s]
  (let [controller (new #/(async/StreamController String))
        rdr (make-reader-input controller)]
    (.add controller s)
    (await (read rdr -1))))

(defn ^:async main []
  (as-> (await (read-string "'(true false false)")) r (prn r (.-runtimeType r)))
  (as-> (await (read-string "(true true nil)")) r (prn r (.-runtimeType r)))
  (as-> (await (read-string "[true true nil]")) r (prn r (.-runtimeType r)))
  (as-> (await (read-string "{true true nil nil}")) r (prn r (.-runtimeType r)))
  (as-> (await (read-string "{true true nil [true true]}")) r (prn r (.-runtimeType r))))
