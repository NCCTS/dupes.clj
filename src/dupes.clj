(ns dupes
  (:require [clojure.core.async :as async]
            [clojure.string :as string]))

;; ----------------------------------------------------------------------------

(def all-punc #"\p{Punct}")

(def all-white #"\s+")

(def empty-str "")

(def one-space " ")

(defn norm-txt
  [txt]
  (string/lower-case
   (string/replace
    (string/replace txt all-punc empty-str)
    all-white
    one-space)))

(defn txt->vec
  [txt]
  (string/split txt (re-pattern one-space)))

(defn max-steps*
  [cnt n step mx? pad?]
  (if (< (- cnt (+ (* step (- mx? 1)) n)) 0)
    (+ (- mx? 1)

       ;; needs work, needs generative TESTS!!!
       ;; should work for n = 0
       ;; pad rules must be more sophisticated

       (if pad?
         (if (or (< step cnt) (= mx? 1))
           1
           0)
         0)

       )
    (recur cnt n step (inc mx?) pad?)))

(defn max-steps
  [coll n step & args]

  ;; needs generative TEST!!!

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

;; all-phrases could eventually handle multiple input texts, w/ "tagging" and
;; find-dups tagging aware impl so can determine in which orig text a dupe
;; appears, which would be important for the report gen logic searchin for
;; originals (i.e. pre-normalized dupe instances)

(defn all-phrases
  [min-phrase-length txt]
  (map (fn [phrase-vec] (string/join one-space phrase-vec))
       (partition min-phrase-length 1 (txt->vec txt))))

;; eventually should write an algorithm to create a mapping from duplicates to
;; original phrases (any one dupe may have originals which are not equal to each
;; other); then should do a line,col find to map originals to their location in
;; the original text; then should generate a report where the order is
;; determined by earliest appearance (per line,col) with variants grouped
;; together with the normalized string on which they matched

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

         ;; all-phrases SHOULD in fact use max-steps and group a map or seq of
         ;; tuples which indicate how big the partition group is; and the order
         ;; of the groups should be from largest to smallest phrase-size (not
         ;; group size); that will allow the lazy-seq described above to do its
         ;; just much easier; but then inner loop-test should be a lazy seq map
         ;; return concat'd val which flattens the groupings

         shuf-aps (lazy-seq ...)

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
                              ;; coordinate w/ pmap and shuf-aps

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

(defn parse-int [s]
  (Integer. (re-find  #"\d+" s )))

(def print-chan (async/chan 100))

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

;; see comment re: shutdown-agents w.r.t. pmap
;; https://clojuredocs.org/clojure.core/pmap
;; may need to invoke shutdown-agents after closing print-chan

(defn -main
  [& args]
  (let [txt (norm-txt (slurp (first args)))
        num (second args)
        len (if num (if (number? num) num (parse-int num)) 10)]
    (print-listen)
    (find-dups len txt print-and-return)
    (async/close! print-chan)))
