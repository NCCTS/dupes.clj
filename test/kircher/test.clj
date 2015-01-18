(ns kircher.test
  (:require [clojure.string :as string]
            [clojure.test.check :as tc]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [clojure.test :refer :all]
            [kircher :as k]))

;; ----------------------------------------------------------------------------

;; need to manually require w/in REPL context
;; ------------------------------------------
;; (require '[clojure.test.check :as tc]
;;          '[clojure.test.check.generators :as gen]
;;          '[clojure.test.check.properties :as prop])

(deftest norm-txt
  (testing "normalize text"
    (is (= (k/norm-txt "\n  \r\r   \tA   !@#\t$%^&*()-_=+\r[]{}\\|;:'\"<,>.?/b? \t\t !*@ C \n\n\n d  ")
           "a b c d"))))

