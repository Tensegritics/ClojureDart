(ns release-test
  (:require [clojure.test :refer [deftest is run-tests testing]]
            [clojure.edn :as edn]
            [clojure.string :as str]
            [release :as release]))

(deftest version-selection
  (let [today (java.time.LocalDate/of 2026 8 22)
        sha-a (apply str (repeat 40 "a"))
        sha-b (apply str (repeat 40 "b"))]
    (testing "first release of the UTC day has no suffix"
      (is (= "0.9.20260822" (release/next-version "" today))))
    (testing "later releases on the same UTC day increment their suffix"
      (is (= "0.9.20260822c"
            (release/next-version
              (str sha-a " refs/tags/0.9.20260822b\n"
                sha-b " refs/tags/0.9.20260822\n")
              today))))
    (testing "a new UTC day starts without a suffix"
      (is (= "0.9.20260822"
            (release/next-version
              (str sha-a " refs/tags/0.9.20260821z\n")
              today))))
    (testing "unrelated versions are ignored"
      (is (= "0.9.20260822"
            (release/next-version
              (str sha-a " refs/tags/0.8.20260822\n"
                sha-b " refs/tags/1.0.20260822\n")
              today))))
    (testing "future-dated tags reveal a clock or release-history problem"
      (is (thrown-with-msg? clojure.lang.ExceptionInfo #"dated after"
            (release/next-version
              (str sha-a " refs/tags/0.9.20260823\n")
              today))))
    (testing "only the unsuffixed tag and a through z are available per day"
      (let [tags (apply str
                   (map-indexed
                     (fn [i suffix]
                       (str (format "%040x" (inc i)) " refs/tags/0.9.20260822" suffix "\n"))
                     (cons "" (map str (map char (range (int \a) (inc (int \z))))))))]
        (is (thrown-with-msg? clojure.lang.ExceptionInfo #"already used"
              (release/next-version tags today)))))))

(deftest asset-rendering
  (let [sha (apply str (repeat 40 "a"))
        asset (release/render-asset "0.9.20260822" sha)
        data (edn/read-string asset)]
    (is (= ["src"] (:paths data)))
    (is (= {:git/url "https://github.com/tensegritics/ClojureDart.git"
            :tag "0.9.20260822"
            :sha sha}
          (get-in data [:deps 'tensegritics/clojuredart])))
    (is (= {:kind :flutter :main 'change.me} (:cljd/opts data)))
    (is (.contains asset "; choose :flutter or :dart"))
    (is (.contains asset "; replace change.me with your main namespace"))
    (is (.endsWith asset "\n"))))

(deftest authentication-errors-are-actionable
  (let [ensure-gh! (ns-resolve 'release 'ensure-gh!)
        runner (fn [args]
                 (case (vec args)
                   ["gh" "--version"] {:exit 0 :out "gh version 2.74.0" :err ""}
                    ["gh" "release" "create" "--help"]
                    {:exit 0 :out "--verify-tag --fail-on-no-commits --latest" :err ""}
                    ["gh" "release" "upload" "--help"]
                    {:exit 0 :out "--clobber" :err ""}
                    ["gh" "release" "edit" "--help"]
                    {:exit 0 :out "--draft --latest" :err ""}
                    ["gh" "auth" "status" "--active" "-h" "github.com"]
                   {:exit 1 :out "" :err "invalid token"}))]
    (binding [release/*command-runner* runner]
      (try
        (ensure-gh!)
        (is false "Expected authentication failure")
        (catch clojure.lang.ExceptionInfo e
          (let [instructions (str/join "\n" (:instructions (ex-data e)))]
            (is (.contains instructions "gh auth login -h github.com"))
            (is (.contains instructions "gh auth status -h github.com"))))))))

(deftest incomplete-release-can-be-resumed
  (let [select-version! (ns-resolve 'release 'select-version!)
        sha (apply str (repeat 40 "a"))
        other-sha (apply str (repeat 40 "b"))
        tags (str sha " refs/tags/0.9.20260822\n")
        today #(java.time.LocalDate/of 2026 8 23)]
    (binding [release/*utc-today* today
              release/*command-runner*
              (fn [_] {:exit 1 :out "" :err "gh: Not Found (HTTP 404)"})
              *err* (java.io.StringWriter.)]
      (is (= {:version "0.9.20260822" :tag-exists? true :release-state :missing}
            (select-version! tags sha))))
    (binding [release/*utc-today* today
              release/*command-runner*
              (fn [_] {:exit 0 :out "{\"draft\":false}" :err ""})]
      (is (= {:version "0.9.20260823" :tag-exists? false :release-state :missing}
            (select-version! (str other-sha " refs/tags/0.9.20260822\n") sha)))
      (binding [*err* (java.io.StringWriter.)]
        (is (= {:version "0.9.20260822" :tag-exists? true :release-state :published}
              (select-version! tags sha)))))
    (binding [release/*utc-today* today
              release/*command-runner*
              (fn [_] {:exit 0 :out "{\"draft\":true}" :err ""})
              *err* (java.io.StringWriter.)]
      (is (= {:version "0.9.20260822" :tag-exists? true :release-state :draft}
            (select-version! tags sha))))))

(deftest release-version-is-refreshed-before-tagging
  (let [refresh-release! (ns-resolve 'release 'refresh-release!)
        sha (apply str (repeat 40 "a"))]
    (binding [release/*utc-today* #(java.time.LocalDate/of 2026 8 23)
              release/*command-runner*
              (fn [args]
                (if (= ["git" "ls-remote" "--tags" "--refs" "release" "refs/tags/0.9.*"]
                      (vec args))
                  {:exit 0 :out "" :err ""}
                  {:exit 1 :out "" :err "gh: Not Found (HTTP 404)"}))
              *err* (java.io.StringWriter.)]
      (is (= {:version "0.9.20260823"
              :tag-exists? false
              :release-state :missing
              :sha sha}
            (refresh-release! {:version "0.9.20260822" :sha sha}))))))

(deftest checked-release-is-passed-to-publisher
  (let [run-release! (ns-resolve 'release 'run-release!)
        preflight! (ns-resolve 'release 'preflight!)
        print-plan (ns-resolve 'release 'print-plan)
        publish! (ns-resolve 'release 'publish!)
        checked-release {:version "0.9.20260822"
                         :sha (apply str (repeat 40 "a"))
                         :tag-exists? false
                         :release-state :missing}
        calls (atom [])]
    (with-redefs-fn
      {preflight! (fn [] checked-release)
       print-plan (fn [release] (swap! calls conj [:print release]))
       publish! (fn [release] (swap! calls conj [:publish release]))}
      run-release!)
    (is (= [[:print checked-release] [:publish checked-release]] @calls))))

(deftest already-released-commit-is-rejected-before-tagging
  (let [ensure-new-commits! (ns-resolve 'release 'ensure-new-commits!)
        sha (apply str (repeat 40 "a"))
        runner (fn [args]
                 (case (last args)
                   "repos/Tensegritics/ClojureDart/releases/latest"
                   {:exit 0 :out "{\"tag_name\":\"0.9.20260822\"}" :err ""}
                   "repos/Tensegritics/ClojureDart/commits/0.9.20260822"
                   {:exit 0 :out (str "{\"sha\":\"" sha "\"}") :err ""}))]
    (binding [release/*command-runner* runner]
      (is (thrown-with-msg? clojure.lang.ExceptionInfo #"already published"
            (ensure-new-commits! sha))))))

(defn run-tests! []
  (let [{:keys [fail error]} (run-tests 'release-test)]
    (when (pos? (+ fail error))
      (throw (ex-info "Release-tool tests failed."
               {:release/error true
                :instructions ["Fix the failing tests before creating a release."]})))))
