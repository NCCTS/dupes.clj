(ns kircher
  (:require [clojure.core.async :as async]
            [clojure.math.numeric-tower :as math]
            [clojure.string :as string]))

(set! *warn-on-reflection* true)
;; (set! *unchecked-math* :warn-on-boxed)

;; -----------------------------------------------------------------------------

(def all-punc #"\p{Punct}")
(def all-white #"\s+")
(def empty-str "")
(def one-space " ")
(def one-space-plus-word #" \S+")

(def default-min-len 5)
(def default-max-len :max)

(defn chan?
  [c]
  (= clojure.core.async.impl.channels.ManyToManyChannel
     (type c)))

(defn bigint?
  [n]
  (= clojure.lang.BigInt
     (type n)))

(defn lazy?
  [xs]
  (or (= clojure.lang.LazySeq
         (type xs))
      (= clojure.lang.Cons
         (type xs))))

(defn norm-txt
  [txt]
  {:pre [(string? txt)]
   :post [(string? %)]}
  ;; ------------------
  (-> txt
      (string/replace all-punc empty-str)
      (string/replace all-white one-space)
      string/trim
      string/lower-case))

(defn txt->vec
  [txt]
  (string/split txt (re-pattern one-space)))
  {:pre [(string? n-txt)]
   :post [(lazy? %)]}
  ;; --------------------
  {:pre [(string? n-txt)]
   :post [(integer? %)]}
  ;; --------------------

(defn- max-steps*
  [cnt n step pad?]
  (let [[cnt n step] (map bigint [cnt n step])]
    (if (> n cnt)
      (if pad?
        1N
        0N)
      (if (>= step cnt)
        1N
        (let [mx (bigint (math/ceil (/ (- cnt (- n 1N)) step)))]
          (if (and (> (- cnt (* mx step)) 0N) pad?)
            (+ mx 1N)
            mx))))))

(defn max-steps
  [coll n step & args]
  {:pre [(or (coll? coll) (integer? coll))
         (integer? n)
         (integer? step)]
   :post [(bigint? %)]}
  ;; -------------------------------------
  (if (<= step 0)
    (throw (Error. "step value <= 0"))
    (let [cnt (count coll)]
      (if (= cnt 0)
        0
        (let [pad? (if (seq args) true false)]
          (if (< n 0)
            (if pad?
              1
              0)
            (max-steps* cnt (if (= n 0) 1 n) step 1 pad?)))))))

;; phrase-length in all-phrases should really denote the minimum phrase length,
;; not the max, i.e. the logic behind generating all-phrases should be more
;; complex, but may be enough to do an apply concat counting down from (length
;; of text - 1) to the min phrase length

;; should probably support both min and max, w/ default max being the length of
;; the orig text, and default min being 10 or 7 or whatever
  {:pre [(lazy? group-sizes)]
   :post [(lazy? %)]}
  ;; ------------------------

;; should support option for find-dups to indicate whether or not smaller
;; phrases should be checked for duplication inside larger phrases which are
;; already found to be duplicated; the default would be to not do such checks as
;; it's probably not suited to most use cases; need to think about implications
;; for checking and not checking when there is more than one input text

;; all-phrases could eventually handle multiple input texts, w/ "tagging" and
;; impl of find-dups which is tag-aware so can determine in which orig text a
;; dupe appears, which would be important for the report gen logic searching for
;; originals (i.e. pre-normalized dupe instances); for multiple inputs, need to
;; handle the case where one whole text matches another, but can possibly
;; generalize w/ test to the effect that in single-text case the test is a
;; trivial non-match

(defn norm-txt->all-phrases
  [n-txt min-len max-len & [txt-seq]]
  {:pre [(string? n-txt)
         (integer? min-len)
         (or (integer? max-len) (= :max max-len))]
   :post [(clojure.test/function? (:all-size %))
          (lazy? (:phrase-size-groups %))]}
  ;; ---------------------------------------------
  (let [txt-seq (or txt-seq (norm-txt->lazy-seq n-txt))]
    (let [cnt-txt-seq (count-norm-txt n-txt)
          max-len (if (= max-len :max) cnt-txt-seq max-len)
          max->min (range max-len (- min-len 1) -1)
          step-1 1
          group-sizes (map (fn [n] (max-steps cnt-txt-seq n step-1))
                           max->min)
          offsets (group-sizes->offsets group-sizes)
          all-size (fn [] (reduce + 0N group-sizes))]
      (struct all-phrases*
              all-size
              (map (fn [[group-size offset n]]
                     (let [phrase-length (bigint n)
                           p-s-g
                           (struct phrase-size-group*
                                   all-size
                                   group-size
                                   phrase-length
                                   (map (fn [[i phrase-vec]]
                                          (let [i (bigint i)
                                                p (struct phrase*
                                                          all-size
                                                          group-size
                                                          phrase-length
                                                          (+ i offset)
                                                          i
                                                          (string/join
                                                           one-space
                                                           phrase-vec))]
                                            (assert (bigint? (:all-index p)))
                                            (assert (bigint? (:group-index p)))
                                            p))
                                        (partition
                                         2
                                         (interleave (range group-size)
                                                     (partition n
                                                                step-1
                                                                txt-seq)))))]
                       (assert (bigint? (:group-size p-s-g)))
                       (assert (bigint? (:phrase-length p-s-g)))
                       (assert (lazy? (:phrases p-s-g)))
                       p-s-g))
                   (partition 3 (interleave group-sizes offsets max->min)))))))

;; eventually should write an algorithm to create a mapping from duplicates to
;; original phrases (any one dupe may have originals which are not equal to each
;; other); then should do a line,col search to map originals to their location
;; in the original text; then should generate a report (in edn format, giving
;; totals, etc. in addition to dupe data; provide options for edn -> html, etc.)
;; where the order is determined by earliest appearance (per line,col) with
;; variants grouped together with the normalized string on which they matched

;; when impl'ing the "report gen" ideas above, would want to switch println to
;; print to stderr, so it serves more as a visual indicator that progress is
;; being made, then should shunt final report through stdout, while providing
;; addt'l optional arg to -main which indicates name of file in which to instead
;; save report

(defn find-dups
  ([min-phrase-length txt]
   (find-dups min-phrase-length txt identity))
  ([min-phrase-length txt xform]
   (let [aps' (all-phrases min-phrase-length txt)
         aps (partition 2 (interleave (range) aps'))

         ;; maybe use a lazy-seq to return a variant of aps that grabs a random
         ;; index but track which ones have already been grabbed, e.g. in an
         ;; (atom #{}); pmap over the lazy-seq "shuf-aps", while the inner
         ;; loop-test of find-dups is over aps; the shuf should be "partitioned"
         ;; so that it shuffles within groups of phrases which are equal in
         ;; length, with larger phrases always being returned before smaller
         ;; phrases (e.g. with first); in that way, the inner loop-test can
         ;; check whether a phrase has a regex match inside a larger phrase
         ;; which is already tracked in dups set, and don't need to do another
         ;; elim of "smaller inside large" for the return val of doall (if they
         ;; were out-of-order would have to do that)

         ;; all-phrases SHOULD in fact use max-steps and group a map (or seq of
         ;; tuples) which indicate how big the partition group is; and the order
         ;; of the groups should be from largest to smallest phrase-size (not
         ;; group size); that will allow the lazy-seq described above to do its
         ;; job much easier; but then inner loop-test should be a lazy seq map
         ;; return concat'd val which flattens the groupings

         ;; shuf-aps (lazy-seq ...)
         shuf-aps '()

         dups (atom #{})
         in-dups (atom #{})]

     ;; when impl'ing ideas for report generation, first step will be to knock out nil members
     ;; in the return val of doall, using filter w/ #(not (nil? %))

     (doall
      (pmap (fn [[pos phrase]]
              (let [dups* @dups
                    in-dups* @in-dups
                    pl (count phrase)
                    rep (re-pattern phrase)]
                (when-not (or (dups* phrase)

                              ;; the in-dups test isn't in the right place; a
                              ;; shorter phrase might be a legit dupe apart from
                              ;; being a non-legit dupe w/in a larger dupe, so
                              ;; need to figure out how to move this logic to
                              ;; the inner loop-test, and figure out the proper
                              ;; scope of tracking an in-dups set, e.g. maybe
                              ;; the loop-test itself is a barrier, or maybe do
                              ;; want it across all runs (i.e. of pmap)

                              ;; may need to do a regex kind of thing where phrase is
                              ;; found in all larger-dupe texts and the latter
                              ;; are extracted (by regex) out of the original
                              ;; text and then phrase is tested for existence as a
                              ;; stand-alone smaller string in the original text

                              ;; which leads to the idea that maybe the inner
                              ;; loop-test can be dramatically simplified by
                              ;; instead using a regex match strategy, described
                              ;; above, which would be sufficient to determine
                              ;; whether one or more "legit" duplicates exist;
                              ;; but then should consider figuring out how many
                              ;; "legit" dupes exist (look into re-seq
                              ;; vs. re-find), so during "find original" steps
                              ;; of report generation can know how many to look
                              ;; for; will be important during report generation
                              ;; search for originals to knock out larger dupes
                              ;; from full text so don't find smaller dupes in
                              ;; larger ones

                              ;; dups and in-dups should maybe be maps with sets
                              ;; hanging off keys which are the length of
                              ;; phrases so can leverage that info to cache the
                              ;; "knock out" ops described above; may be able to
                              ;; dissoc larger key/sets in the cache while
                              ;; moving along, so as to not keep around multiple
                              ;; potentially large strings longer than
                              ;; necessary, but will need to think about how to
                              ;; coordinate w/ pmap and shuf-aps; rather than
                              ;; trying to coordinate could instead do an outer
                              ;; map around the pmap, where the outer map steps
                              ;; over the groups of same-length phrases
                              ;; (i.e. grouped by count of how many of same
                              ;; length); but this will require shuf-aps to to
                              ;; work a bit differently

                              (in-dups* phrase)
                              (some (fn [d]
                                      (let [tst (and (> (count d) pl)
                                                     (re-find rep d))]
                                        (if tst
                                          (do
                                            (swap! in-dups conj phrase)
                                            true)
                                          false)))
                                    dups*))
                  (when (let [aps-same-len (filter #(= (count %) pl) aps)]
                          (loop [tup (first aps-same-len)
                                 aps* (rest aps-same-len)]
                            (let [pos* (first tup)
                                  phrase* (second tup)]
                              (if (and (= phrase* phrase)
                                       (not= pos* pos))
                                phrase
                                (when-let [aps** (seq (rest aps*))]
                                  (recur (first aps**)
                                         aps**))))))
                    (let [dups** (swap! dups conj phrase)]
                      (when (not= dups** dups*)
                        [pos (xform phrase)]))))))
            shuf-aps)))))
  {:pre [(string? txt)
         (integer? min-len)
         (or (integer? max-len) (= :max max-len))]}
  ;; ----------------------------------------------
    (assert (chan? events))
;;   {:pre [(string? txt)
;;          (integer? min-len)
;;          (or (integer? max-len) (= :max max-len))]}
;;   ;; ----------------------------------------------
;;     (assert (chan? events))

(defn parse-int [s]
  (Integer. (re-find  #"\d+" s )))

(def print-chan (async/chan 100))
  {:pre [(string? s)]}
    (assert (chan? events-chan))

(defn print-and-return
  [v]
  (async/>!! print-chan v) v)

(defn print-listen
  []
  (async/thread
    (loop [c print-chan]
      (when-let [v (async/<!! c)]
        (println v "\n")
        (recur c)))))

;; ----------------------------------------------------------------------------

;; GUI IDEA
;; --------
;; look into fx-clj :: https://github.com/aaronc/fx-clj
;; also, revisit Launch4j :: http://launch4j.sourceforge.net/
;; and JarBundler :: http://informagen.com/JarBundler/index.html

(defn -main
  [& args]
  (let [txt (norm-txt (slurp (first args)))
        num (second args)
        len (if num (if (number? num) num (parse-int num)) 10)]
    (print-listen)
    (find-dups len txt print-and-return)
    (async/close! print-chan)
    ;; see comment re: shutdown-agents w.r.t. pmap
    ;; https://clojuredocs.org/clojure.core/pmap
    (shutdown-agents)))
