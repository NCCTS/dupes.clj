(defproject kircher "0.1.0-SNAPSHOT"
  :description "A tool for finding duplicate groupings of words in a large body of text"
  :license {:distribution :repo
            :comments "same as Clojure"
            :name "Eclipse Public License - v 1.0"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :url "https://github.com/NCCTS/kircher"

  :dependencies [[org.clojure/clojure "1.7.0-alpha5"]
                 [org.clojure/core.async "0.1.346.0-17112a-alpha"]
                 [org.clojure/math.numeric-tower "0.0.4"]]

  :main kircher

  :profiles {:dev
             {:dependencies [[com.cemerick/double-check "0.6.1"]]

              :plugins [[cider/cider-nrepl "0.9.0-SNAPSHOT"]
                        [com.jakemccrary/lein-test-refresh "0.5.5"]]}})
