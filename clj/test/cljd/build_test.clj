(ns cljd.build-test
  (:require [cljd.build :as build]
            [clojure.edn :as edn]
            [clojure.test :refer [deftest is run-tests]]))

(def ^:private old-sha (apply str (repeat 40 "a")))
(def ^:private new-sha (apply str (repeat 40 "b")))

(defn- asset [tag sha]
  (str "{:paths [\"src\"]\n"
    " :deps {tensegritics/clojuredart\n"
    "        {:git/url \"https://github.com/tensegritics/ClojureDart.git\"\n"
    "         :tag \"" tag "\"\n"
    "         :sha \"" sha "\"}}\n"
    " :aliases {:cljd {:main-opts [\"-m\" \"cljd.build\"]}}}"))

(deftest parse-release-asset
  (is (= {:tag "0.9.20260822a" :sha new-sha}
        (build/parse-latest-deps (asset "0.9.20260822a" new-sha))))
  (is (thrown-with-msg? clojure.lang.ExceptionInfo
        #"Expected a UTC-dated tag"
        (build/parse-latest-deps (asset "1.0.0" new-sha))))
  (is (thrown-with-msg? clojure.lang.ExceptionInfo
        #"Expected a UTC-dated tag"
        (build/parse-latest-deps (asset "0.9.20260230" new-sha))))
  (is (thrown-with-msg? clojure.lang.ExceptionInfo
        #"Expected exactly one EDN form"
        (build/parse-latest-deps (str (asset "0.9.20260822a" new-sha) " {}")))))

(deftest upgrade-sha-only-coordinate
  (let [text (str "{:deps {tensegritics/clojuredart "
               "{:git/url \"https://github.com/tensegritics/ClojureDart.git\" "
               ":sha \"" old-sha "\"}}}")
        result (build/upgrade-deps-text text {:git/sha old-sha}
                 {:tag "0.9.20260822a" :sha new-sha})]
    (is (:changed? result))
    (is (.contains (:text result) (str ":sha \"" new-sha "\"")))
    (is (.contains (:text result) (str "#_\"" old-sha "\"")))
    (is (= "0.9.20260822a"
          (get-in (edn/read-string (:text result))
            [:deps 'tensegritics/clojuredart :tag])))))

(deftest upgrade-tagged-coordinate
  (let [text (str "{:deps {tensegritics/clojuredart "
               "{:git/url \"https://github.com/tensegritics/ClojureDart.git\" "
               ":tag \"0.9.20260822\" :sha \"" old-sha "\"}}}")
        result (build/upgrade-deps-text text
                 {:git/tag "0.9.20260822" :git/sha old-sha}
                 {:tag "0.9.20260822a" :sha new-sha})]
    (is (:changed? result))
    (is (.contains (:text result) ":tag \"0.9.20260822a\" #_\"0.9.20260822\""))
    (is (.contains (:text result) (str ":sha \"" new-sha "\" #_\"" old-sha "\"")))))

(deftest missing-tag-is-added-to-current-sha
  (let [text (str "{:deps {tensegritics/clojuredart {:sha \"" new-sha "\"}}}")
        result (build/upgrade-deps-text text {:sha new-sha}
                 {:tag "0.9.20260822a" :sha new-sha})]
    (is (:changed? result))
    (is (= {:sha new-sha :tag "0.9.20260822a"}
          (get-in (edn/read-string (:text result))
            [:deps 'tensegritics/clojuredart])))))

(deftest already-current-with-tag
  (let [text (str "{:deps {tensegritics/clojuredart "
               "{:tag \"0.9.20260822a\" :sha \"" new-sha "\"}}}")
        result (build/upgrade-deps-text text
                 {:tag "0.9.20260822a" :sha new-sha}
                 {:tag "0.9.20260822a" :sha new-sha})]
    (is (false? (:changed? result)))
    (is (= text (:text result)))))

(deftest stale-tag-is-updated-for-current-sha
  (let [text (str "{:deps {tensegritics/clojuredart "
               "{:tag \"0.9.20260822\" :sha \"" new-sha "\"}}}")
        result (build/upgrade-deps-text text
                 {:tag "0.9.20260822" :sha new-sha}
                 {:tag "0.9.20260822a" :sha new-sha})]
    (is (:changed? result))
    (is (= {:tag "0.9.20260822a" :sha new-sha}
          (get-in (edn/read-string (:text result))
            [:deps 'tensegritics/clojuredart])))
    (is (.contains (:text result) "#_\"0.9.20260822\""))))

(deftest namespaced-git-tag-is-added
  (let [text (str "{:deps {tensegritics/clojuredart {:git/sha \"" old-sha "\"}}}")
        result (build/upgrade-deps-text text {:git/sha old-sha}
                 {:tag "0.9.20260822a" :sha new-sha})]
    (is (= {:git/sha new-sha :git/tag "0.9.20260822a"}
          (get-in (edn/read-string (:text result))
            [:deps 'tensegritics/clojuredart])))))

(deftest ambiguous-coordinate-is-not-rewritten
  (let [text (str "{:deps {tensegritics/clojuredart {:sha \"" old-sha "\"} "
               "x/x {:sha \"" old-sha "\"}}}")]
    (is (thrown-with-msg? clojure.lang.ExceptionInfo
          #"occurs 2 times"
          (build/upgrade-deps-text text {:sha old-sha}
            {:tag "0.9.20260822a" :sha new-sha})))))

(deftest overridden-coordinate-is-not-rewritten
  (let [other-sha (apply str (repeat 40 "c"))
        text (str "{:deps {tensegritics/clojuredart {:sha \"" other-sha "\"}}}")]
    (is (thrown-with-msg? clojure.lang.ExceptionInfo
          #"does not match the running dependency"
          (build/upgrade-deps-text text {:sha old-sha}
            {:tag "0.9.20260822a" :sha new-sha})))))

(deftest newer-release-is-not-downgraded
  (let [text (str "{:deps {tensegritics/clojuredart "
               "{:tag \"0.9.20260822b\" :sha \"" old-sha "\"}}}")]
    (is (thrown-with-msg? clojure.lang.ExceptionInfo
          #"Refusing to downgrade"
          (build/upgrade-deps-text text {:tag "0.9.20260822b" :sha old-sha}
            {:tag "0.9.20260822a" :sha new-sha})))))

(deftest custom-git-url-is-not-rewritten
  (let [text (str "{:deps {tensegritics/clojuredart "
               "{:git/url \"https://github.com/acme/ClojureDart.git\" "
               ":sha \"" old-sha "\"}}}")]
    (is (thrown-with-msg? clojure.lang.ExceptionInfo
          #"custom Git URL"
          (build/upgrade-deps-text text {:sha old-sha}
            {:tag "0.9.20260822a" :sha new-sha})))))

(deftest upgrade-file-atomically
  (let [directory (.toFile (java.nio.file.Files/createTempDirectory
                             "cljd-upgrade-test-"
                             (make-array java.nio.file.attribute.FileAttribute 0)))
        file (java.io.File. directory "deps.edn")
        posix-view (java.nio.file.Files/getFileAttributeView (.toPath file)
                     java.nio.file.attribute.PosixFileAttributeView
                     (make-array java.nio.file.LinkOption 0))
        expected-permissions (java.nio.file.attribute.PosixFilePermissions/fromString "rw-------")
        text (str "{:deps {tensegritics/clojuredart "
               "{:git/url \"https://github.com/tensegritics/ClojureDart.git\" "
               ":sha \"" old-sha "\"}}}")]
    (try
      (spit file text)
      (when posix-view
        (java.nio.file.Files/setPosixFilePermissions (.toPath file) expected-permissions))
      (binding [build/*deps* {:libs {'tensegritics/clojuredart {:git/sha old-sha}}}
                build/*latest-deps-reader* #(asset "0.9.20260822a" new-sha)]
        (is (.contains (with-out-str (build/upgrade-cljd file))
              "ClojureDart upgraded to 0.9.20260822a")))
      (is (.contains (slurp file) (str ":sha \"" new-sha "\"")))
      (is (.contains (slurp file) (str "#_\"" old-sha "\"")))
      (when posix-view
        (is (= expected-permissions
              (java.nio.file.Files/getPosixFilePermissions (.toPath file)
                (make-array java.nio.file.LinkOption 0)))))
      (is (empty? (filter #(.startsWith (.getName ^java.io.File %) ".deps.edn-")
                    (.listFiles directory))))
      (finally
        (doseq [child (.listFiles directory)]
          (.delete child))
        (.delete directory)))))

(defn -main [& _]
  (let [{:keys [fail error]} (run-tests 'cljd.build-test)]
    (when (pos? (+ fail error))
      (System/exit 1))))
