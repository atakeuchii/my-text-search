(defproject my-text-search "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  :dependencies [[org.clojure/clojure "1.12.5"]
                 [atakeuchii/my-storage "0.1.0"]]
  :repositories [["github"
                  {:url "https://maven.pkg.github.com/atakeuchii/my-storage"
                   :username :env/github_actor
                   :password :env/github_token}]]
  :profiles {:dev {:dependencies [[org.clojure/test.check "1.1.1"]]}
             :bench {:source-paths ["bench"]}}
  :repl-options {:init-ns my-text-search.core})
