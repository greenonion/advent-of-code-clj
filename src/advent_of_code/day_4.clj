(ns advent-of-code.day-4
  (:require [pandect.algo.md5 :refer :all]))

(def input "ckczppom")

(defn valid? [hash zeroes]
  (and (> (count hash) zeroes)
       (= (subs hash 0 zeroes) (apply str (repeat zeroes "0")))))

(defn part_1 [input]
  (loop [answer 0]
    (let [cur (md5 (str input answer))]
      (if (valid? cur 5)
        answer
        (recur (inc answer))))))

(defn part_2 [input]
  (loop [answer 0]
    (let [cur (md5 (str input answer))]
      (if (valid? cur 6)
        answer
        (recur (inc answer))))))
