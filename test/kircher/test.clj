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

(deftest txt->vec
  (testing "text to vector"
    (is (= (k/txt->vec "a b c d")
           ["a" "b" "c" "d"]))))

(deftest max-steps
  (testing "step <= 0"
    (let [zero-throws
          (prop/for-all
           [coll (gen/vector gen/int)
            n gen/int
            ;; neg-int may gen 0 val, while s-neg-int would not
            step gen/neg-int]
           ;; wrapping in `boolean' is a hack
           ;;   http://dev.clojure.org/jira/browse/TCHECK-26
           (boolean (is (thrown? Error (k/max-steps coll n step))))
           (boolean (is (thrown? Error (k/max-steps coll n step ['pad])))))]
      (tc/quick-check 1000 zero-throws)))

  (testing "empty coll"
    (let [empty-always-zero
          (prop/for-all
           [n gen/int
            ;; s-pos-int never gens 0 val
            step gen/s-pos-int]
           (is (= 0 (k/max-steps [] n step)))
           (is (= 0 (k/max-steps [] n step ['pad]))))]
      (tc/quick-check 1000 empty-always-zero)))

  (testing "n < 0"
    (let [n-lte-0-always-0-or-1
          (prop/for-all
           [coll (gen/such-that not-empty (gen/vector gen/int))
            n gen/s-neg-int
            step gen/s-pos-int]
           (is (= 0 (k/max-steps coll n step)))
           (is (= 1 (k/max-steps coll n step ['pad]))))]
      (tc/quick-check 1000 n-lte-0-always-0-or-1))))

(def k-max-steps* @#'k/max-steps*)

(deftest max-steps*
  (testing "equiv counts for max-step* and partition"
    (let [equiv-count
          (prop/for-all
           [coll (gen/vector gen/int 1 10)
            n (gen/resize 15 gen/s-pos-int)
            step (gen/resize 15 gen/s-pos-int)]
           (let [cnt (count coll)
                 mx? 1
                 pad?-true true
                 pad?-false false
                 pnp (partition n step coll)
                 pwp (partition n step ['pad] coll)]
             (is (= (k-max-steps* cnt n step mx? pad?-false)
                    (count pnp)))
             (is (= (k-max-steps* cnt n step mx? pad?-true)
                    (count pwp)))))]
      (tc/quick-check 10000 equiv-count))))
