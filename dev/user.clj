(ns user)

;; NS to delete, serve as exploratory namespace for VM Service


(do
  (import '(org.dartlang.vm.service VmService))
  (import '(org.dartlang.vm.service.element VM))
  (import '(org.dartlang.vm.service.element BoundField))
  (import '(org.dartlang.vm.service.element FieldRef))
  (import '(org.dartlang.vm.service.element Field))
  (import '(org.dartlang.vm.service.element Obj))
  (import '(org.dartlang.vm.service.element Func))
  (import '(org.dartlang.vm.service.consumer VMConsumer))
  (import '(org.dartlang.vm.service.consumer GetObjectConsumer))
  (import '(org.dartlang.vm.service.element Version))
  (import '(org.dartlang.vm.service.consumer VersionConsumer))
  (import '(org.dartlang.vm.service.consumer GetIsolateConsumer))
  (import '( org.dartlang.vm.service VmService))

  (import '(org.dartlang.vm.service.element Isolate))

  (import '(org.dartlang.vm.service.consumer EvaluateConsumer))

  (import '(org.dartlang.vm.service.element InstanceRef))
  (import '(org.dartlang.vm.service.element Instance))
  (import '(org.dartlang.vm.service.element IsolateRef))

  (import '(org.dartlang.vm.service.element Library))
  (import '(org.dartlang.vm.service.element InstanceKind))
  (import '(org.dartlang.vm.service.consumer GetLibraryConsumer)))

;;

(defn version [^VmService vm-service cb]
  (.getVersion vm-service (reify VersionConsumer (^void received [_ ^Version response] (cb response)))))

(defn get-vm [^VmService vm-service cb]
  (.getVM vm-service (reify VMConsumer (^void received [_ ^VM response] (cb response)))))

(defn get-isolate [^VmService vm-service isolate-id cb]
  (.getIsolate vm-service ^int isolate-id (reify GetIsolateConsumer (^void received [_ ^Isolate response] (cb response)))))

(defn evaluate [^VmService vm-service isolate-id scope-id expr cb]
  (.evaluate vm-service isolate-id scope-id expr (reify EvaluateConsumer (^void received [_ ^InstanceRef response] (cb response)))))

(defn get-library [^VmService vm-service isolate-id library-id cb]
  (.getLibrary vm-service isolate-id library-id (reify GetLibraryConsumer (^void received [_ ^Library response] (cb response)))))

(defn get-object [^VmService vm-service isolate-id object-id cb]
  (.getObject vm-service isolate-id object-id (reify GetObjectConsumer (^void received [_ ^Obj response] (cb response)))))

(let [vm-service (VmService/connect "ws://127.0.0.1:55150/tOpvofKlliA=/")]
  (get-vm vm-service
    (fn [^VM vm]
      (when-some [isolate-id
                  (some (fn [^IsolateRef ir]
                          (when (= "main" (.getName ir))
                            (.getId ir))) (seq (.getIsolates vm)))]
        (get-isolate vm-service isolate-id
          (fn [^Isolate isolate]
            (evaluate vm-service isolate-id (.getId (.getRootLib isolate)) #_"((el) => [1, 2, el])(4)" "todos"
              (fn [^InstanceRef ir]
                (println (.getValueAsString ir))
                (case (doto (.ordinal (doto (.getKind ir) prn)) prn)
                  ;int
                  11 (println "int")
                  ; List
                  18 (get-object vm-service isolate-id (.getId ir)
                       (fn [^Instance obj]
                         (prn (map #(.getValueAsString ^Instance %) (.getElements obj)))))
                  ; closure
                  2 (let [func-ref (.getClosureFunction ir)]
                      (get-object vm-service isolate-id (.getId func-ref)
                        (fn [^Func obj]
                          (prn (.getName (.getTypeClass (.getReturnType (.getSignature obj))))))))
                  ; instance
                  22 (do (prn (.getName (.getClassRef ir)))
                         (get-object vm-service isolate-id (.getId ir)
                           (fn [^Instance obj]
                             (let [fields (.getFields obj)]
                               (->> fields
                                 (map #(vector (.getDecl ^BoundField %) (.getValue ^BoundField %)))

                                 (map (fn [[^FieldRef d v]]
                                        (get-object vm-service isolate-id (.getId d)
                                          (fn [^Field field]
                                            (prn 'here)
                                            (prn (.getName field))))))
                                 (into [])))))))))))))))

(let [vm-service (VmService/connect "ws://127.0.0.1:52341/cxXHMZ84YUE=/")]
  (let [p (promise)]
    (get-vm vm-service
      (fn [^VM vm]
        (when-some [isolate-id
                    (some (fn [^IsolateRef ir]
                            (when (= "main" (.getName ir))
                              (.getId ir))) (seq (.getIsolates vm)))]
          (get-isolate vm-service isolate-id
            (fn [^Isolate isolate]
              (evaluate vm-service isolate-id (.getId (.getRootLib isolate)) "(() {final dc.int a$1=42;\nreturn a$1;\n})()"
                (fn [^InstanceRef ir]
                  (prn "coucou")
                  (deliver p (.getValueAsString ir))
                  (println (.getValueAsString ir)))))))))
    (prn "aahh" @p)))
;; [w1, w2, w3]



;; Bool,
;; BoundedType,
;; Closure,
;; Double,
;; Float32List,
;; Float32x4,
;; Float32x4List,
;; Float64List,
;; Float64x2,
;; Float64x2List,
;; FunctionType,
;; Int,
;; Int16List,
;; Int32List,
;; Int32x4,
;; Int32x4List,
;; Int64List,
;; Int8List,
;; List,
;; Map,
;; MirrorReference,
;; Null,
;; PlainInstance,
;; ReceivePort,
;; RegExp,
;; StackTrace,
;; String,
;; Type,
;; TypeParameter,
;; TypeRef,
;; Uint16List,
;; Uint32List,
;; Uint64List,
;; Uint8ClampedList,
;; Uint8List,
;; WeakProperty,
;; Unknown
