(ns kircher.test
  (:require [clojure.core.async :as async]
            [clojure.math.numeric-tower :as math]
            [clojure.string :as string]
            [clojure.test :refer :all]
            [kircher :as k]))

;; REPL context requires ""manual"" require, for some reason
;; ---------------------------------------------------------
(require '[clojure.test.check :as tc]
         '[clojure.test.check.generators :as gen]
         '[clojure.test.check.properties :as prop])

;; -----------------------------------------------------------------------------

(deftest norm-txt
  (testing "normalize text"
    (is (= (k/norm-txt (str "\n  \r\r   \tA   !@#\t$%^&*()-_=+\r[]{}\\|;:'\"<,>"
                            ".?/b? \t\t !*@ C \n\n\n d  "))
           "a b c d"))))

(deftest norm-txt->lazy-seq
  (testing "text to vector"
    (is (= (k/norm-txt->lazy-seq "a b c d")
           ["a" "b" "c" "d"]))))

(def dummy-pad ['pad])

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
           (boolean (is (thrown? Error (k/max-steps (count coll) n step))))
           (boolean (is (thrown? Error (k/max-steps coll n step dummy-pad))))
           (boolean (is (thrown? Error (k/max-steps (count coll)
                                                    n step dummy-pad)))))]
      (tc/quick-check 1000 zero-throws)))

  (testing "empty coll"
    (let [empty-always-zero
          (prop/for-all
           [n gen/int
            ;; s-pos-int never gens 0 val
            step gen/s-pos-int]
           (is (= 0 (k/max-steps [] n step)))
           (is (= 0 (k/max-steps 0 n step)))
           (is (= 0 (k/max-steps [] n step dummy-pad)))
           (is (= 0 (k/max-steps 0 n step dummy-pad))))]
      (tc/quick-check 1000 empty-always-zero)))

  (testing "n < 0"
    (let [n-lte-0-always-0-or-1
          (prop/for-all
           [coll (gen/such-that not-empty (gen/vector gen/int))
            n gen/s-neg-int
            step gen/s-pos-int]
           (is (= 0 (k/max-steps coll n step)))
           (is (= 0 (k/max-steps (count coll) n step)))
           (is (= 1 (k/max-steps coll n step dummy-pad)))
           (is (= 1 (k/max-steps (count coll) n step dummy-pad))))]
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
                 pad?-true true
                 pad?-false false
                 count-kms*-np (k-max-steps* cnt n step pad?-false)
                 count-kms*-wp (k-max-steps* cnt n step pad?-true)
                 count-kms-np (k/max-steps coll n step)
                 count-kms-np' (k/max-steps (count coll) n step)
                 count-kms-wp (k/max-steps coll n step dummy-pad)
                 count-kms-wp' (k/max-steps (count coll) n step dummy-pad)
                 pnp (partition n step coll)
                 pwp (partition n step dummy-pad coll)]

             ;; (when (or
             ;;        (not (= count-kms*-np
             ;;                count-kms-np
             ;;                count-kms-np'
             ;;                (bigint (count pnp))))
             ;;        (not (= count-kms*-wp
             ;;                count-kms-wp
             ;;                count-kms-wp'
             ;;                (bigint (count pwp)))))
             ;;   (println "coll: " coll)
             ;;   (println "[cnt n step]: " [cnt n step])
             ;;   (println "[ALL-NP-COUNTS]: " [count-kms*-np count-kms-np count-kms-np' (bigint (count pnp))])
             ;;   (println "[ALL-WP-COUNTS]: " [count-kms*-wp count-kms-wp count-kms-wp' (bigint (count pwp))])
             ;;   (println "pnp: " pnp)
             ;;   (println "pwp: " pwp))

             (is (= count-kms*-np
                    count-kms-np
                    count-kms-np'
                    (bigint (count pnp))))
             (is (= count-kms*-wp
                    count-kms-wp
                    count-kms-wp'
                    (bigint (count pwp))))
             (is (= clojure.lang.BigInt
                    (type count-kms*-np)
                    (type count-kms*-wp)
                    (type count-kms-np)
                    (type count-kms-np')
                    (type count-kms-wp)
                    (type count-kms-wp')))))]
      (tc/quick-check 10000 equiv-count))))
