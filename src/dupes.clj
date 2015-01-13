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
  (string/replace (string/replace txt all-punc empty-str) all-white one-space))

(defn txt->vec
  [txt]
  (string/split txt #" "))

(defn all-phrases
  [phrase-length txt]
  (map (fn [phrase-vec] (string/join one-space phrase-vec))
       (partition phrase-length 1 (txt->vec txt))))

(defn find-dups
  ([phrase-length txt]
   (find-dups phrase-length txt identity))
  ([phrase-length txt xform]
   (let [aps' (all-phrases phrase-length txt)
         aps (shuffle (partition 2 (interleave (range (count aps')) aps')))
         dups (atom #{})]
     (filter
      #(not (nil? %))
      (doall
       (pmap (fn [[pos phrase]]
               (let [dups* @dups]
                 (when-not (dups* phrase)
                   (when (loop [tup (first aps)
                                aps* (rest aps)]
                           (let [pos* (first tup)
                                 phrase* (second tup)]
                             (if (and (= phrase* phrase)
                                      (not= pos* pos))
                               phrase
                               (when-let [aps** (seq (rest aps*))]
                                 (recur (first aps**)
                                        aps**)))))
                     (let [dups** (swap! dups conj phrase)]
                       (when (not= dups** dups*)
                         (xform phrase)))))))
             aps))))))

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

(defn -main
  [& args]
  (let [txt (norm-txt (slurp (first args)))
        num (second args)
        len (if num (if (number? num) num (parse-int num)) 10)]
    (print-listen)
    (find-dups len txt print-and-return)
    (async/close! print-chan)))
