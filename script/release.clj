(ns release
  (:require [babashka.process :as process]
            [cheshire.core :as json]
            [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]))

(def ^:private repository "Tensegritics/ClojureDart")
(def ^:private remote "release")
(def ^:private branch "main")
(def ^:private version-pattern #"^0\.9\.([0-9]{8})([a-z]?)$")
(def ^:private asset-name "deps.latest.edn")
(def ^:private latest-asset-url
  (str "https://github.com/" repository "/releases/latest/download/" asset-name))

(def ^:dynamic *command-runner*
  (fn [args]
    (try
      (let [{:keys [exit out err]}
            @(process/process args {:out :string :err :string})]
        {:exit exit :out out :err err})
      (catch java.io.IOException e
        {:exit 127 :out "" :err (.getMessage e) :exception e}))))

(defonce ^:private script-root (atom nil))

(def ^:dynamic *utc-today*
  #(java.time.LocalDate/now java.time.ZoneOffset/UTC))

(defn set-script-root! [root]
  (reset! script-root (.getCanonicalPath (io/file root))))

(defn- command [& args]
  (*command-runner* (vec args)))

(defn- output [result]
  (str/trim (:out result)))

(defn- fail! [message & instructions]
  (throw (ex-info message {:release/error true
                           :instructions (vec instructions)})))

(defn- warn! [message & instructions]
  (binding [*out* *err*]
    (println)
    (println "WARNING:" message)
    (doseq [instruction instructions]
      (when instruction
        (println instruction)))))

(defn- require-success! [result message & instructions]
  (when-not (zero? (:exit result))
    (apply fail! message
      (cond-> (vec instructions)
        (seq (str/trim (:err result)))
        (conj (str "Command output: " (str/trim (:err result)))))))
  result)

(defn- ensure-repository-root! []
  (let [result (command "git" "rev-parse" "--show-toplevel")]
    (require-success! result
      "This directory is not inside a Git repository."
      (str "Run the command from the ClojureDart checkout, for example:\n  cd " @script-root "\n  ./release --check"))
    (let [actual (.getCanonicalPath (io/file (output result)))]
      (when-not (= actual @script-root)
        (fail! "The release command must be run from the root of the ClojureDart repository."
          (str "Current repository root: " actual)
          (str "Expected repository root: " @script-root)
          (str "Run:\n  cd " @script-root "\n  ./release --check"))))))

(defn- ensure-main-branch! []
  (let [result (command "git" "branch" "--show-current")]
    (require-success! result "Unable to determine the current Git branch."
      "Check the repository with:\n  git status")
    (let [current (output result)]
      (when-not (= branch current)
        (fail! (str "Releases must be made from branch " branch ", but the current branch is "
                 (if (str/blank? current) "detached HEAD" current) ".")
          "Finish or preserve the current work, then switch with:\n  git switch main"
          "Verify with:\n  git branch --show-current")))))

(defn- ensure-tracked-worktree-clean! []
  (let [result (command "git" "status" "--porcelain" "--untracked-files=no")]
    (require-success! result "Unable to inspect the Git worktree."
      "Run:\n  git status")
    (when-not (str/blank? (:out result))
      (fail! "Tracked files have uncommitted changes, so HEAD does not describe the release contents."
        "Inspect them with:\n  git status --short"
        "Commit or stash the intended changes, then run:\n  ./release --check")))
  (let [result (command "git" "status" "--porcelain" "--untracked-files=all")
        untracked (filter #(str/starts-with? % "?? ") (str/split-lines (:out result)))]
    (when (seq untracked)
      (warn! (str (count untracked) " untracked path(s) will not be part of the release.")
        (str "Examples:\n  " (str/join "\n  " (take 5 untracked)))
        "Review all of them with:\n  git status --short"))))

(defn- head-sha! []
  (let [result (command "git" "rev-parse" "HEAD")]
    (require-success! result "Unable to determine the SHA of HEAD."
      "Run:\n  git rev-parse HEAD")
    (let [sha (output result)]
      (when-not (re-matches #"[0-9a-f]{40}" sha)
        (fail! (str "Git returned an invalid HEAD SHA: " (pr-str sha))
          "Verify the repository with:\n  git fsck"))
      sha)))

(defn- remote-main-sha! []
  (let [result (command "git" "ls-remote" remote (str "refs/heads/" branch))]
    (require-success! result
      (str "Unable to contact the Git remote named " remote ".")
      "Inspect configured remotes with:\n  git remote -v"
      (str "If the remote is missing, add it with:\n  git remote add " remote
        " git@github.com:Tensegritics/ClojureDart.git")
      "If SSH authentication is the problem, test it with:\n  ssh -T git@github.com")
    (if-some [[_ sha] (re-find #"(?m)^([0-9a-f]{40})\s+refs/heads/main$" (:out result))]
      sha
      (fail! (str "Remote " remote " has no " branch " branch.")
        (str "Inspect it with:\n  git ls-remote " remote " refs/heads/" branch)))))

(defn- ensure-head-is-pushed! [sha]
  (let [remote-sha (remote-main-sha!)]
    (when-not (= sha remote-sha)
      (fail! "Local HEAD and release/main do not point to the same commit."
        (str "Local HEAD:       " sha)
        (str "Remote main HEAD: " remote-sha)
        (str "Synchronize and inspect the branch with:\n  git fetch " remote " " branch
          "\n  git status")
        (str "If the local commits are ready to publish, push them explicitly with:\n  git push " remote " " branch)
        "The release command never pushes main automatically."))))

(defn- ensure-gh! []
  (let [version (command "gh" "--version")]
    (when (= 127 (:exit version))
      (fail! "GitHub CLI (gh) is not installed or cannot be found on PATH."
        "On macOS with Homebrew, install it with:\n  brew install gh"
        "Other installation methods:\n  https://cli.github.com/"
        "Verify the installation with:\n  gh --version"))
    (require-success! version "Unable to run GitHub CLI (gh)."
      "Verify the installation with:\n  gh --version"))
  (let [help (command "gh" "release" "create" "--help")]
    (require-success! help "Unable to inspect the GitHub CLI release command."
      "Verify the installation with:\n  gh release create --help")
    (when-not (every? #(.contains ^String (:out help) %)
                ["--verify-tag" "--fail-on-no-commits" "--latest"])
      (fail! "The installed GitHub CLI is too old for the safe release workflow."
        "Upgrade it with Homebrew using:\n  brew upgrade gh"
         "Other upgrade methods:\n  https://cli.github.com/"
         "Then verify the required options with:\n  gh release create --help")))
  (doseq [[subcommand required-options]
          [["upload" ["--clobber"]]
           ["edit" ["--draft" "--latest"]]]]
    (let [help (command "gh" "release" subcommand "--help")]
      (require-success! help (str "Unable to inspect gh release " subcommand ".")
        (str "Verify the installation with:\n  gh release " subcommand " --help"))
      (when-not (every? #(.contains ^String (:out help) %) required-options)
        (fail! "The installed GitHub CLI is too old to resume partial releases safely."
          "Upgrade it with Homebrew using:\n  brew upgrade gh"
          "Other upgrade methods:\n  https://cli.github.com/"))))
  (let [auth (command "gh" "auth" "status" "--active" "-h" "github.com")]
    (when-not (zero? (:exit auth))
      (fail! "GitHub CLI is not authenticated with a valid github.com token."
        "Authenticate interactively with:\n  gh auth login -h github.com"
        "Choose an account that can create releases in Tensegritics/ClojureDart."
        "Then verify the authentication with:\n  gh auth status -h github.com")))
  (let [permission (command "gh" "api" (str "repos/" repository)
                     "--jq" ".permissions.push")]
    (require-success! permission
      "GitHub CLI could not inspect repository permissions."
      "Verify authentication with:\n  gh auth status -h github.com"
      (str "Verify repository access with:\n  gh repo view " repository))
    (when-not (= "true" (output permission))
      (fail! (str "The authenticated GitHub account cannot push to " repository ".")
        "Authenticate with an account that has write access:\n  gh auth login -h github.com"
        "Verify the selected account with:\n  gh auth status -h github.com"))))

(defn parse-release-version [version]
  (when-some [[_ date suffix] (re-matches version-pattern version)]
    (try
      (let [day (java.time.LocalDate/parse date java.time.format.DateTimeFormatter/BASIC_ISO_DATE)
            suffix-index (if (str/blank? suffix)
                           0
                           (inc (- (int (first suffix)) (int \a))))]
        {:version version
         :date date
         :day day
         :suffix suffix
         :suffix-index suffix-index})
      (catch java.time.format.DateTimeParseException _ nil))))

(defn release-versions [ls-remote-output]
  (into []
    (keep (fn [line]
            (when-some [[_ sha version]
                        (re-find #"^([0-9a-f]{40})\s+refs/tags/(0\.9\.[0-9]{8}[a-z]?)$" line)]
              (some-> (parse-release-version version) (assoc :sha sha)))))
    (str/split-lines ls-remote-output)))

(defn- version-sort-key [{:keys [day suffix-index]}]
  [(.toEpochDay ^java.time.LocalDate day) suffix-index])

(defn- ensure-no-future-versions! [versions today]
  (let [future (filter #(.isAfter ^java.time.LocalDate (:day %) today) versions)]
    (when (seq future)
      (fail! "A release tag is dated after the current UTC date."
        (str "Current UTC date: " (.format today java.time.format.DateTimeFormatter/BASIC_ISO_DATE))
        (str "Future tag(s): " (str/join ", " (map :version (sort-by version-sort-key future))))
        "Check the machine clock before creating another release."))))

(defn next-version
  ([ls-remote-output] (next-version ls-remote-output (*utc-today*)))
  ([ls-remote-output today]
   (let [versions (release-versions ls-remote-output)]
     (ensure-no-future-versions! versions today)
     (let [date (.format today java.time.format.DateTimeFormatter/BASIC_ISO_DATE)
           suffix-index (inc (reduce max -1
                               (map :suffix-index (filter #(= date (:date %)) versions))))]
       (when (> suffix-index 26)
         (fail! (str "All release suffixes for UTC date " date " are already used.")
           "The supported sequence is the unsuffixed tag followed by a through z."
           "Wait until the next UTC day before creating another release."))
       (str "0.9." date
         (when (pos? suffix-index)
           (char (+ (int \a) (dec suffix-index)))))))))

(defn- remote-tags! []
  (let [result (command "git" "ls-remote" "--tags" "--refs" remote "refs/tags/0.9.*")]
    (require-success! result
      "Unable to list existing release tags."
      (str "Inspect the remote with:\n  git ls-remote --tags " remote " 'refs/tags/0.9.*'"))
    (:out result)))

(defn- github-api! [path]
  (let [result (command "gh" "api" path)]
    (cond
      (zero? (:exit result))
      (try
        (json/parse-string (:out result) true)
        (catch Exception e
          (fail! (str "GitHub returned malformed JSON for " path ": " (ex-message e))
            (str "Retry the request with:\n  gh api " path))))

      (re-find #"(?i)(HTTP 404|not found)" (:err result)) nil

      :else
      (fail! (str "GitHub API request failed: " path)
        "Verify authentication with:\n  gh auth status --active -h github.com"
        (str "Retry the request with:\n  gh api " path)
        (when (seq (str/trim (:err result)))
          (str "Command output: " (str/trim (:err result))))))))

(defn- release-state! [version]
  (if-some [release (github-api! (str "repos/" repository "/releases/tags/" version))]
    (if (:draft release) :draft :published)
    :missing))

(defn- ensure-new-commits! [sha]
  (when-some [release (github-api! (str "repos/" repository "/releases/latest"))]
    (let [tag (:tag_name release)
          commit (github-api! (str "repos/" repository "/commits/" tag))
          previous-sha (:sha commit)]
      (when-not (re-matches #"[0-9a-f]{40}" (or previous-sha ""))
        (fail! (str "Unable to resolve the commit for latest release " tag ".")
          (str "Inspect it with:\n  gh release view " tag " --repo " repository)))
      (when (= sha previous-sha)
        (fail! (str "Commit " sha " is already published as release " tag ".")
           "There are no new commits to release, so no tag was created."
           (str "Inspect recent commits with:\n  git log --oneline " tag "..HEAD")))
      (let [ancestor (command "git" "merge-base" "--is-ancestor" previous-sha sha)]
        (cond
          (zero? (:exit ancestor)) nil

          (= 1 (:exit ancestor))
          (fail! (str "Latest release " tag " is not an ancestor of the current HEAD.")
            (str "Latest release commit: " previous-sha)
            (str "Current HEAD:          " sha)
            "The main branch may have been rewound or diverged. Inspect the history before releasing:"
            (str "  git log --graph --oneline --decorate --all --boundary " previous-sha ".." sha))

          :else
          (fail! "Unable to compare HEAD with the latest release commit."
            (str "Run:\n  git merge-base --is-ancestor " previous-sha " " sha)
            (when (seq (str/trim (:err ancestor)))
              (str "Command output: " (str/trim (:err ancestor))))))))))

(defn- select-version! [tags-output current-sha]
  (let [versions (release-versions tags-output)]
    (ensure-no-future-versions! versions (*utc-today*))
    (if-some [{:keys [version] tag-sha :sha}
              (last (sort-by version-sort-key versions))]
      (case (release-state! version)
        :published
        (if (= current-sha tag-sha)
          (do
            (warn! (str "Commit " current-sha " is already published as release " version
                     "; verifying the completed release."))
            {:version version :tag-exists? true :release-state :published})
          {:version (next-version tags-output) :tag-exists? false :release-state :missing})

        :draft
        (if (= current-sha tag-sha)
          (do
            (warn! (str "GitHub Release " version " exists as a draft; resuming it.")
              (str "Inspect it with:\n  gh release view " version " --repo " repository))
            {:version version :tag-exists? true :release-state :draft})
          (fail! (str "Draft GitHub Release " version " points to another commit.")
            (str "Tag commit:     " tag-sha)
            (str "Current commit: " current-sha)
            (str "Inspect it with:\n  gh release view " version " --repo " repository)))

        :missing
        (if (= current-sha tag-sha)
          (do
            (warn! (str "Remote tag " version " exists without a published release; resuming it.")
              (str "Inspect the tag with:\n  git ls-remote " remote " refs/tags/" version)
              (str "Inspect drafts with:\n  gh release list --repo " repository))
            {:version version :tag-exists? true :release-state :missing})
          (fail! (str "Remote tag " version " exists without a GitHub Release and points to another commit.")
            (str "Tag commit:     " tag-sha)
            (str "Current commit: " current-sha)
            (str "Inspect it with:\n  git ls-remote " remote " refs/tags/" version)
            "Do not move or delete the tag automatically; resolve the partial release manually.")))
      {:version (next-version tags-output) :tag-exists? false :release-state :missing})))

(defn- ensure-version-available! [{:keys [version tag-exists? release-state]}]
  (when-not (re-matches version-pattern version)
    (fail! (str "The computed release version is invalid: " (pr-str version))
      "Expected a UTC-dated version such as 0.9.20260822 or 0.9.20260822a."))
  (let [local-tag (command "git" "rev-parse" "--verify" "--quiet" (str "refs/tags/" version))]
    (when (and (not tag-exists?) (zero? (:exit local-tag)))
      (fail! (str "A local tag named " version " already exists.")
        (str "Inspect it with:\n  git show " version)
        (str "If it is an accidental local-only tag, delete it explicitly with:\n  git tag -d " version)
        "Never move or overwrite a published release tag.")))
  (let [actual-state (release-state! version)]
    (when-not (= release-state actual-state)
      (fail! (str "GitHub Release " version " changed from " (name release-state)
               " to " (name actual-state) " while checks were running.")
        (str "Inspect it with:\n  gh release view " version " --repo " repository)
        "Run ./release --check again before creating the release."))))

(defn- report-ci! [sha]
  (let [result (command "gh" "run" "list" "--repo" repository
                 "--workflow" "Tests" "--commit" sha "--limit" "1"
                 "--json" "databaseId,status,conclusion,url")]
    (if-not (zero? (:exit result))
      (warn! "Unable to inspect CI for this commit; the release will remain allowed."
        (str "Inspect CI manually with:\n  gh run list --repo " repository " --commit " sha))
      (let [runs (json/parse-string (:out result) true)]
        (if-some [{:keys [databaseId status conclusion url]} (first runs)]
          (if (and (= "completed" status) (= "success" conclusion))
            (println (str "CI succeeded: " url))
            (warn! (str "CI is not green for " sha "; the release will remain allowed.")
              (str "Status: " status ", conclusion: " (or conclusion "pending"))
              (str "Inspect it with:\n  gh run view " databaseId " --repo " repository)
              (str "Watch it with:\n  gh run watch " databaseId " --repo " repository)
              (when (= "failure" conclusion)
                (str "Rerun failed jobs with:\n  gh run rerun " databaseId
                  " --failed --repo " repository))))
          (warn! "No CI run was found for this exact commit; the release will remain allowed."
            (str "Inspect recent runs with:\n  gh run list --repo " repository)))))))

(defn render-asset [version sha]
  (str "{:paths [\"src\"]\n"
    " :deps {tensegritics/clojuredart\n"
    "        {:git/url \"https://github.com/tensegritics/ClojureDart.git\"\n"
    "         :tag \"" version "\"\n"
    "         :sha \"" sha "\"}}\n"
    " :aliases {:cljd {:main-opts [\"-m\" \"cljd.build\"]}}\n"
    " :cljd/opts {:kind :flutter ; choose :flutter or :dart\n"
    "             :main change.me}} ; replace change.me with your main namespace\n"))

(defn- validate-asset! [text version sha]
  (let [data (edn/read-string text)
        coordinate (get-in data [:deps 'tensegritics/clojuredart])]
    (when-not (= {:git/url "https://github.com/tensegritics/ClojureDart.git"
                  :tag version
                  :sha sha}
                coordinate)
      (fail! "The generated deps.latest.edn asset failed internal validation."
        "No tag or release was created. Please report this as a release-tool bug."))
    text))

(defn- delete-tree! [file]
  (when (.exists ^java.io.File file)
    (doseq [child (or (seq (.listFiles ^java.io.File file)) [])]
      (delete-tree! child))
    (io/delete-file file true)))

(defn- preflight! []
  (ensure-repository-root!)
  (ensure-main-branch!)
  (ensure-tracked-worktree-clean!)
  (let [sha (head-sha!)]
    (ensure-head-is-pushed! sha)
    (ensure-gh!)
    (let [{:keys [release-state] :as release} (select-version! (remote-tags!) sha)]
      (when-not (= :published release-state)
        (ensure-new-commits! sha))
      (ensure-version-available! release)
      (report-ci! sha)
      (assoc release :sha sha))))

(defn- print-plan [{:keys [version sha]}]
  (println)
  (println "Release check succeeded.")
  (println (str "  Version: " version))
  (println (str "  Commit:  " sha))
  (println (str "  Asset:   " latest-asset-url)))

(defn- reserve-tag! [{:keys [version sha tag-exists?]}]
  (if tag-exists?
    (println (str "Using existing remote tag " version " at " sha "."))
    (let [refspec (str sha ":refs/tags/" version)
          result (command "git" "push" remote refspec)]
      (require-success! result
        (str "Unable to reserve release tag " version " atomically.")
        "Another release may have created it since the checks ran."
        (str "Inspect the remote tag with:\n  git ls-remote " remote " refs/tags/" version)
        "Run ./release --check again; never force-push a release tag.")))
  (let [result (command "git" "ls-remote" remote (str "refs/tags/" version))]
    (require-success! result (str "Unable to verify reserved tag " version ".")
      (str "Inspect it with:\n  git ls-remote " remote " refs/tags/" version))
    (when-not (str/starts-with? (:out result) sha)
      (fail! (str "Reserved tag " version " does not point to the expected commit.")
        (str "Expected: " sha)
        (str "Remote tag output: " (str/trim (:out result)))
        "Do not move the tag. Resolve the conflict manually."))))

(defn- http-read [url]
  (let [connection ^java.net.HttpURLConnection (.openConnection (java.net.URL. url))]
    (try
      (.setConnectTimeout connection 10000)
      (.setReadTimeout connection 15000)
      (.setInstanceFollowRedirects connection true)
      (.setRequestProperty connection "User-Agent" "ClojureDart-release-tool")
      (let [status (.getResponseCode connection)]
        (if (<= 200 status 299)
          (with-open [reader (io/reader (.getInputStream connection))]
            (slurp reader))
          (throw (ex-info (str "HTTP " status) {:status status}))))
      (finally
        (.disconnect connection)))))

(defn- read-latest-asset [version expected]
  (loop [attempt 1]
    (let [latest-tag (:tag_name (github-api! (str "repos/" repository "/releases/latest")))
          newer-release? (and (not= version latest-tag)
                           (parse-release-version version)
                           (parse-release-version latest-tag)
                           (pos? (compare
                                   (version-sort-key (parse-release-version latest-tag))
                                   (version-sort-key (parse-release-version version)))))
          result (when (= version latest-tag)
                   (try
                     {:text (http-read latest-asset-url)}
                     (catch Exception e {:error e})))]
      (cond
        newer-release?
        (warn! (str "Release " latest-tag " became latest while " version " was being verified.")
          (str "The immutable asset for " version " was verified directly; no latest redirect check is needed."))

        (= expected (:text result)) (:text result)
        (< attempt 10) (do (Thread/sleep 2000) (recur (inc attempt)))
        (:text result) (fail! "The public latest-release URL still serves a different deps.latest.edn."
                         "The release is published, but GitHub may still be updating its latest-release redirect."
                         (str "Retry the public URL with:\n  curl --fail --location " latest-asset-url))
        (:error result) (fail! "The release was published, but its public latest asset URL could not be verified."
                 (str "Retry the download with:\n  curl --fail --location " latest-asset-url)
                 (str "Underlying error: " (ex-message (:error result))))
        :else (fail! (str "GitHub still reports " (or latest-tag "no release")
                      " as latest instead of " version ".")
                (str "Inspect releases with:\n  gh release list --repo " repository))))))

(defn- refresh-release! [{:keys [sha version]}]
  (let [current (assoc (select-version! (remote-tags!) sha) :sha sha)]
    (when-not (= version (:version current))
      (warn! (str "The release version changed from " version " to " (:version current)
               " while checks were running.")
        "The UTC date or remote release state changed; the asset will use the refreshed version."))
    (ensure-version-available! current)
    current))

(defn- publish! [checked-release]
  (let [{:keys [version sha release-state] :as release} (refresh-release! checked-release)
        temp-dir (.toFile (java.nio.file.Files/createTempDirectory
                             "clojuredart-release-" (make-array java.nio.file.attribute.FileAttribute 0)))
        asset (io/file temp-dir asset-name)
        text (validate-asset! (render-asset version sha) version sha)]
    (try
      (spit asset text)
      ; Close the window between preflight and tag reservation.
      (when-not (= :published release-state)
        (ensure-new-commits! sha))
      (reserve-tag! release)
      (println)
      (case release-state
        :missing
        (do
          (println (str "Creating GitHub Release " version " for " sha "..."))
          (let [result (command "gh" "release" "create" version (.getPath asset)
                         "--repo" repository
                         "--verify-tag"
                         "--title" version
                         "--generate-notes"
                         "--fail-on-no-commits"
                         "--latest")]
            (require-success! result
              (str "GitHub CLI failed while creating release " version ".")
              "The operation may have created a tag or a draft before failing. Inspect both before retrying:"
              (str "  gh release view " version " --repo " repository)
              (str "  git ls-remote " remote " refs/tags/" version)
              "Do not delete or move a published tag automatically.")))

        :draft
        (do
          (println (str "Completing draft GitHub Release " version " for " sha "..."))
          (require-success!
            (command "gh" "release" "upload" version (.getPath asset)
              "--repo" repository "--clobber")
            (str "Unable to upload " asset-name " to draft release " version ".")
            (str "Inspect it with:\n  gh release view " version " --repo " repository))
          (require-success!
            (command "gh" "release" "edit" version "--repo" repository
              "--draft=false" "--latest")
            (str "The asset was uploaded, but draft release " version " could not be published.")
            (str "Retry with:\n  gh release edit " version " --repo " repository
              " --draft=false --latest")))

        :published
        (println (str "GitHub Release " version " is already published; verifying it.")))
      (let [download-dir (io/file temp-dir "download")
            _ (.mkdirs download-dir)
            result (command "gh" "release" "download" version "--repo" repository
                     "--pattern" asset-name "--dir" (.getPath download-dir))]
        (require-success! result "The release exists, but deps.latest.edn could not be downloaded for verification."
          (str "Retry the download with:\n  gh release download " version " --repo " repository
            " --pattern " asset-name))
        (when-not (= text (slurp (io/file download-dir asset-name)))
          (fail! "The published deps.latest.edn differs from the generated release asset."
            (str "Inspect assets with:\n  gh release view " version " --repo " repository))))
      (read-latest-asset version text)
      (println)
      (println (str "Release " version " published successfully."))
      (println (str "  https://github.com/" repository "/releases/tag/" version))
      (println (str "  " latest-asset-url))
      release
      (finally
        (delete-tree! temp-dir)))))

(defn- print-help []
  (println "ClojureDart release tool")
  (println)
  (println "Usage:")
  (println "  ./release --check   Validate the next release without creating it")
  (println "  ./release           Create the next UTC-dated 0.9.YYYYMMDD[a-z] GitHub Release")
  (println "  ./release --test    Run release-tool unit tests")
  (println "  ./release --help    Show this help"))

(defn- run-tests! []
  (load-file (str (io/file @script-root "script/release_test.clj")))
  ((ns-resolve 'release-test 'run-tests!))
  (let [result (command "clojure" "-M" "-m" "cljd.build-test")]
    (require-success! result
      "JVM upgrade tests failed."
      "Run them directly for complete diagnostics:\n  clojure -M -m cljd.build-test")
    (print (:out result))))

(defn- print-error! [e]
  (binding [*out* *err*]
    (println)
    (if (:release/error (ex-data e))
      (do
        (println "Release aborted:" (ex-message e))
        (doseq [instruction (:instructions (ex-data e))]
          (when instruction
            (println)
            (println instruction))))
      (do
        (println "Release aborted by an unexpected error:" (ex-message e))
        (println)
        (println "Run the non-mutating checks again with:")
        (println "  ./release --check")
        (println)
        (println "If the problem persists, report it with the complete output above.")))))

(defn- run-release! []
  (let [release (preflight!)]
    (print-plan release)
    (publish! release)))

(defn -main [& args]
  (try
    (case (vec args)
      [] (run-release!)
      ["--check"] (print-plan (preflight!))
      ["--test"] (run-tests!)
      (["--help"] ["-h"]) (print-help)
      (fail! (str "Unknown arguments: " (str/join " " args))
        "See the supported commands with:\n  ./release --help"))
    (catch Exception e
      (print-error! e)
      (System/exit 1))))
