(ns build
  (:require [clojure.tools.build.api :as b]
            [deps-deploy.deps-deploy :as dd]))

(def lib 'org.clojuredart/java-host-lisp-reader)
(def version (format "1.11.%s" (b/git-count-revs nil)))
(def class-dir "target/classes")
(def jar-file (format "target/%s-%s.jar" (name lib) version))

;; delay to defer side effects (artifact downloads)
(def basis (delay (b/create-basis {:project "deps.edn"})))

(defn clean [_]
  (b/delete {:path "target"}))

(defn compile [_]
  (b/javac {:src-dirs ["src"]
            :class-dir class-dir
            :basis @basis
            :javac-opts ["--release" "11"]}))

(defn jar [_]
  (compile nil)
  (b/write-pom {:class-dir class-dir
                :lib lib
                :version version
                :basis @basis
                :src-dirs ["src"]
                :pom-data
                [[:licenses
                   [:license
                     [:name "Eclipse Public License"]
                     [:url "https://www.eclipse.org/legal/epl-v10.html"]]]]})
  #_(b/copy-dir {:src-dirs ["src" "resources"]
               :target-dir class-dir})
  (b/jar {:class-dir class-dir
          :jar-file jar-file}))

(defn install [_]
  (b/install {:basis      basis
              :lib        lib
              :version    version
              :jar-file   jar-file
              :class-dir  class-dir}))

(defn deploy
  "CLOJARS_PASSWORD and CLOJARS_USERNAME must be set; CLOJARS_PASSWORD being a demoyment token" [_]
  (dd/deploy {:installer :remote
              :artifact jar-file
              :pom-file (b/pom-path {:lib lib :class-dir class-dir})}))
