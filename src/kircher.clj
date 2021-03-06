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

(defn norm-txt->lazy-seq
  [n-txt]
  {:pre [(string? n-txt)]
   :post [(lazy? %)]}
  ;; --------------------
  (map string/triml (re-seq one-space-plus-word (str one-space n-txt))))

(defn count-norm-txt
  [n-txt]
  {:pre [(string? n-txt)]
   :post [(integer? %)]}
  ;; --------------------
  (+ 1
     (loop [cnt 0
            sdx 0]
       (let [fdx (. ^String n-txt (indexOf ^String one-space sdx))]
         (if (= -1 fdx)
           cnt
           (recur (inc cnt) (inc fdx)))))))

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
    (let [cnt (if (integer? coll) coll (count coll))]
      (if (= cnt 0)
        0N
        (let [pad? (if (seq args) true false)]
          (if (< n 0)
            (if pad?
              1N
              0N)
            (max-steps* cnt (if (= n 0) 1 n) step pad?)))))))

(defn group-sizes->offsets
  [group-sizes]
  {:pre [(lazy? group-sizes)]
   :post [(lazy? %)]}
  ;; ------------------------
  (reductions + 0 group-sizes))

(defstruct phrase* :all-size :group-size :phrase-length :all-index :group-index :phrase)
(defstruct phrase-size-group* :all-size :group-size :phrase-length :phrases)
(defstruct all-phrases* :all-size :phrase-size-groups)

;; norm-txt->all-phrases could eventually handle multiple input texts, w/ "tagging" and
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

;; should support option for find-dups to indicate whether or not smaller
;; phrases should be checked for duplication inside larger phrases which are
;; already found to be duplicated; the default would be to not do such checks as
;; it's probably not suited to most use cases; need to think about implications
;; for checking and not checking when there is more than one input text

(defn find-dups
  [txt min-len max-len & [events]]
  {:pre [(string? txt)
         (integer? min-len)
         (or (integer? max-len) (= :max max-len))]}
  ;; ----------------------------------------------
  (let [events (or events (async/close! (async/chan)))]
    (assert (chan? events))
    (let [n-txt (norm-txt txt)
          txt-seq (norm-txt->lazy-seq n-txt)
          aps (norm-txt->all-phrases n-txt min-len max-len txt-seq)
          n-txt* (atom n-txt)]

      ;; this is wrong, it needs to be a reduce w/ fast loops like finding " "
      ;; above for the count-norm-txt; also, overlapping dups in the same
      ;; phrase-size-group need to be combined before being substracted from the
      ;; string; it seems pmap isn't worthwhile either given high contention;
      ;; when switching to loops and reduce, should be able to avoid all use of
      ;; atoms

      (dorun
       (map
        (fn [p-s-g]
          (let [dups (atom #{})]
            (dorun
             (map
              (fn [dp]
                (swap! n-txt*
                       #(string/replace
                         %
                         (re-pattern (str one-space (:phrase dp)))
                         " |")))
              (filter
               #(not (nil? %))
               (doall
                (map
                 (fn [p]
                   (let [dups* @dups
                         p* (:phrase p)]
                     (when (not (dups* p*))
                       (let [one-space-plus-p* (re-pattern (str one-space p*))
                             n-txt*' @n-txt*
                             fnd (re-seq one-space-plus-p* (str one-space n-txt*'))]
                         (when (> (count fnd) 1)
                           (let [dups** @dups]
                             (when (not (dups** p*))
                               (async/>!! events p)
                               (swap! dups conj p*)
                               p)))))))
                 (:phrases p-s-g))))))))
        (:phrase-size-groups aps))))))

;; (defn find-dups
;;   [txt min-len max-len & [events]]
;;   {:pre [(string? txt)
;;          (integer? min-len)
;;          (or (integer? max-len) (= :max max-len))]}
;;   ;; ----------------------------------------------
;;   (let [events (or events (async/close! (async/chan)))]
;;     (assert (chan? events))
;;     (let [n-txt (norm-txt txt)
;;           txt-seq (norm-txt->lazy-seq n-txt)
;;           aps (norm-txt->all-phrases n-txt min-len max-len txt-seq)

;;           ;; maybe use a lazy-seq to return a variant of aps that grabs a random
;;           ;; index but track which ones have already been grabbed, e.g. in an
;;           ;; (atom #{}); pmap over the lazy-seq "shuf-aps", while the inner
;;           ;; loop-test of find-dups is over aps; the shuf should be "partitioned"
;;           ;; so that it shuffles within groups of phrases which are equal in
;;           ;; length, with larger phrases always being returned before smaller
;;           ;; phrases (e.g. with first); in that way, the inner loop-test can
;;           ;; check whether a phrase has a regex match inside a larger phrase
;;           ;; which is already tracked in dups set, and don't need to do another
;;           ;; elim of "smaller inside large" for the return val of doall (if they
;;           ;; were out-of-order would have to do that)

;;           ;; norm-txt->all-phrases SHOULD in fact use max-steps and group a map
;;           ;; (or seq of tuples) which indicate how big the partition group is;
;;           ;; and the order of the groups should be from largest to smallest
;;           ;; phrase-size (not group size); that will allow the lazy-seq
;;           ;; described above to do its job much easier; but then inner loop-test
;;           ;; should be a lazy seq map return concat'd val which flattens the
;;           ;; groupings

;;           ;; shuf-aps (lazy-seq ...)
;;           shuf-aps '()

;;           dups (atom #{})
;;           in-dups (atom #{})]

;;       ;; when impl'ing ideas for report generation, first step will be to knock out nil members
;;       ;; in the return val of doall, using filter w/ #(not (nil? %))

;;       (doall
;;        (pmap (fn [[pos phrase]]
;;                (let [dups* @dups
;;                      in-dups* @in-dups
;;                      pl (count phrase)
;;                      rep (re-pattern phrase)]
;;                  (when-not (or (dups* phrase)

;;                                ;; the in-dups test isn't in the right place; a
;;                                ;; shorter phrase might be a legit dupe apart from
;;                                ;; being a non-legit dupe w/in a larger dupe, so
;;                                ;; need to figure out how to move this logic to
;;                                ;; the inner loop-test, and figure out the proper
;;                                ;; scope of tracking an in-dups set, e.g. maybe
;;                                ;; the loop-test itself is a barrier, or maybe do
;;                                ;; want it across all runs (i.e. of pmap)

;;                                ;; may need to do a regex kind of thing where phrase is
;;                                ;; found in all larger-dupe texts and the latter
;;                                ;; are extracted (by regex) out of the original
;;                                ;; text and then phrase is tested for existence as a
;;                                ;; stand-alone smaller string in the original text

;;                                ;; which leads to the idea that maybe the inner
;;                                ;; loop-test can be dramatically simplified by
;;                                ;; instead using a regex match strategy, described
;;                                ;; above, which would be sufficient to determine
;;                                ;; whether one or more "legit" duplicates exist;
;;                                ;; but then should consider figuring out how many
;;                                ;; "legit" dupes exist (look into re-seq
;;                                ;; vs. re-find), so during "find original" steps
;;                                ;; of report generation can know how many to look
;;                                ;; for; will be important during report generation
;;                                ;; search for originals to knock out larger dupes
;;                                ;; from full text so don't find smaller dupes in
;;                                ;; larger ones

;;                                ;; dups and in-dups should maybe be maps with sets
;;                                ;; hanging off keys which are the length of
;;                                ;; phrases so can leverage that info to cache the
;;                                ;; "knock out" ops described above; may be able to
;;                                ;; dissoc larger key/sets in the cache while
;;                                ;; moving along, so as to not keep around multiple
;;                                ;; potentially large strings longer than
;;                                ;; necessary, but will need to think about how to
;;                                ;; coordinate w/ pmap and shuf-aps; rather than
;;                                ;; trying to coordinate could instead do an outer
;;                                ;; map around the pmap, where the outer map steps
;;                                ;; over the groups of same-length phrases
;;                                ;; (i.e. grouped by count of how many of same
;;                                ;; length); but this will require shuf-aps to to
;;                                ;; work a bit differently

;;                                (in-dups* phrase)
;;                                (some (fn [d]
;;                                        (let [tst (and (> (count d) pl)
;;                                                       (re-find rep d))]
;;                                          (if tst
;;                                            (do
;;                                              (swap! in-dups conj phrase)
;;                                              true)
;;                                            false)))
;;                                      dups*))
;;                    (when (let [aps-same-len (filter #(= (count %) pl) aps)]
;;                            (loop [tup (first aps-same-len)
;;                                   aps* (rest aps-same-len)]
;;                              (let [pos* (first tup)
;;                                    phrase* (second tup)]
;;                                (if (and (= phrase* phrase)
;;                                         (not= pos* pos))
;;                                  phrase
;;                                  (when-let [aps** (seq (rest aps*))]
;;                                    (recur (first aps**)
;;                                           aps**))))))
;;                      (let [dups** (swap! dups conj phrase)]
;;                        (when (not= dups** dups*)
;;                          [pos phrase]))))))
;;              shuf-aps)))))

(defn parse-int [s]
  {:pre [(string? s)]}
  (Integer. ^String (re-find  #"\d+" s)))

(defn events->console
  [& [c]]
  (let [events-chan (or c (async/chan 100))
        all-size (atom nil)
        nas "N/A"]
    (assert (chan? events-chan))
    (async/thread
      (loop [c events-chan]
        (when-let [msg (async/<!! c)]
          (let [a-s @all-size]
            (when (nil? a-s)
              (reset! all-size nas)
              (async/thread
                (reset! all-size ((:all-size msg)))))
            (apply println
                   [((fn [m] (if (not (integer? a-s))
                               nas
                               (let [a-i (:all-index m)]
                                 (str a-i
                                      "/"
                                      a-s
                                      " "
                                      (format "%.5f"
                                              (* 100
                                                 (float (/ a-i a-s))))))))
                     msg)
                    ((fn [m] (let [g-i (:group-index m)
                                   g-s (:group-size m)]
                               (str g-i
                                    "/"
                                    g-s
                                    " "
                                    (format "%.5f"
                                            (* 100
                                               (float (/ g-i g-s)))))))
                     msg)
                    (int (:phrase-length msg))
                    (:phrase msg)
                    "\n"]))
          (recur c))))
    events-chan))

;; -----------------------------------------------------------------------------

;; should support supplying of more than one file name, but wil need to think
;; about having min,max-len supplied as cli flags; there is probably some clojure
;; cli builder lib for

(defn -main
  [& [file-name min-len max-len]]
  (let [txt (slurp file-name)
        min-len (if min-len
                  (if (number? min-len) min-len (parse-int min-len))
                  default-min-len)
        max-len (if max-len
                  (if (number? max-len)
                    max-len
                    (if (or (= "max" max-len) (= ":max" max-len) (= :max max-len))
                      :max
                      (parse-int max-len)))
                  default-max-len)
        events-chan (events->console)]
    (find-dups txt min-len max-len events-chan)
    (async/close! events-chan)
    ;; see comment re: shutdown-agents w.r.t. pmap
    ;; https://clojuredocs.org/clojure.core/pmap
    (shutdown-agents)))

;; GUI IDEA
;; --------
;; look into fx-clj :: https://github.com/aaronc/fx-clj
;; also, revisit Launch4j :: http://launch4j.sourceforge.net/
;; and JarBundler :: http://informagen.com/JarBundler/index.html
