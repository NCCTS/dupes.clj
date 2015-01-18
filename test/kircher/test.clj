(ns kircher.test
  (:require [clojure.string :as string]
            [clojure.test.check :as tc]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [clojure.test :refer :all]
            [kircher :as k]))

(deftest a-test
  (testing "FIXME, I fail."
    (is (= 0 1))))
;; ----------------------------------------------------------------------------

;; need to manually require w/in REPL context
;; ------------------------------------------
;; (require '[clojure.test.check :as tc]
;;          '[clojure.test.check.generators :as gen]
;;          '[clojure.test.check.properties :as prop])

