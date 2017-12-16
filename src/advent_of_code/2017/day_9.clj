(ns advent-of-code.2017.day-9
  (:require [clojure.string :as string])
  (:use clojure.test))

(defn inpt []
  (as-> "resources/2017/day_9_input.txt" $
    (slurp $)
    (string/trim-newline $)))

(defn garbage-groups [data]
  (loop [i 0
         g []]
    (let [x (last g)]
      (cond (= (get data i) \<)
            ;; if we are already in a garbage group, ignore.
            ;; otherwise we encountered start of garbage group.
            (if (= 1 (count x)) ; we are in a garbage group, ignore
              (recur (inc i) g)
              (recur (inc i) (conj g [i])))
            (= (get data i) \!) ; ignore this character and the next
            (recur (inc (inc i)) g)
            (= (get data i) \>) ; we can close the group
            (recur (inc i) (conj (vec (butlast g)) (conj x (inc i))))
            (>= i (count data)) g
            :else (recur (inc i) g)))))

(defn remove-garbage [data]
  (let [gg (garbage-groups data)
        [a b] (first gg)
        parts (partition 2 (conj (vec (cons 0 (flatten gg))) (count data)))]
    (string/join (map #(subs data (first %) (second %)) parts))))

(defn
  ^{:test (fn []
            (is (= 1 (score "{<{o\"i!a,<{i<a>}")))
            (is (= 1 (score "{<<<<>}")))
            (is (= 1 (score "{<{!>}>}")))
            (is (= 1 (score "{<!!>}")))
            (is (= 1 (score "{<!!!>>}")))
            (is (= 1 (score "{}")))
            (is (= 6 (score "{{{}}}")))
            (is (= 5 (score "{{},{}}")))
            (is (= 16 (score "{{{},{},{{}}}}")))
            (is (= 1 (score "{<a>,<a>,<a>,<a>}")))
            (is (= 9 (score "{{<ab>},{<ab>},{<ab>},{<ab>}}")))
            (is (= 9 (score "{{<!!>},{<!!>},{<!!>},{<!!>}}")))
            (is (= 3 (score "{{<a!>},{<a!>},{<a!>},{<ab>}}"))))}
  score
  [data]
  (let [no-garbage (remove-garbage data)]
    (loop [score 0
           depth 0
           [x & ks] no-garbage]
      ;; each time we see an opening bracket, increase depth,
      ;; closing brackets add current depth and decrease it.
      ;; commas we just skip
      (cond (= x \{) (recur score (inc depth) ks)
            (= x \,) (recur score depth ks)
            (= x \}) (recur (+ score depth) (dec depth) ks)
            (nil? x) score))))

(test #'score)

(defn silver-star []
  (score (inpt)))

;; gold-star

;; Of course this could be done via classic map/reduce. But this required so
;; modification of the existing solution that I couldn't resist!
(defn
  ^{:test (fn []
            (is (= 0 (count-garbage "<>")))
            (is (= 17 (count-garbage "<random characters>")))
            (is (= 3 (count-garbage "<<<<>")))
            (is (= 2 (count-garbage "<{!>}>")))
            (is (= 0 (count-garbage "<!!>")))
            (is (= 0 (count-garbage "<!!!>>")))
            (is (= 10 (count-garbage "<{o\"i!a,<{i<a>"))))}
  count-garbage [data]
  (loop [i 0
         score 0
         inside false]
    (cond (= (get data i) \<)
          ;; if we are already in a garbage group, ignore.
          ;; otherwise we encountered start of garbage group.
          (if (true? inside) ; we are in a garbage group, ignore
            (recur (inc i) (inc score) inside)
            (recur (inc i) score true))
          (= (get data i) \!) ; ignore this character and the next
          (recur (inc (inc i)) score inside)
          (= (get data i) \>) ; we can close the group
          (recur (inc i) score false)
          (true? inside) (recur (inc i) (inc score) inside)
          (>= i (count data)) score
          :else (recur (inc i) score inside))))

(test #'count-garbage)

(defn gold-star []
  (count-garbage (inpt)))
