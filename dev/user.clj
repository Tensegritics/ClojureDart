(ns user)

;; NS to delete, serve as exploratory namespace for VM Service

(import '(org.dartlang.vm.service.element VM))
(import '(org.dartlang.vm.service.consumer VMConsumer))
(import '(org.dartlang.vm.service.element Version))
(import '(org.dartlang.vm.service.consumer VersionConsumer))
(import '( org.dartlang.vm.service VmService))

(let [vm-service (VmService/connect "ws://127.0.0.1:63793/NGeYzpzHl9Y=/ws")]
  (.getVersion vm-service (reify VersionConsumer
                            (^void received [_ ^Version response]
                             (prn (str (.getMajor response) "." (.getMinor response))))))
  (.getVM vm-service (reify VMConsumer
                       (^void received [_ ^VM response]
                        (prn (.getId (.get (.getIsolates response) 0)))))))
