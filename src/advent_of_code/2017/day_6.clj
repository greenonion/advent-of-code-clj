(ns advent-of-code.2017.day-6
  (:require [clojure.string :as string])
  (:use clojure.test))

(defn inpt []
  (as-> "resources/2017/day_6_input.txt" $
    (slurp $)
    (string/split $ #"\t|\n")
    (map #(Integer/parseInt %) $)
    (apply vector $)))

(defn
  ^{:test (fn []
            (is (= 2 (largest-bank [0 2 7 0]))))}
  largest-bank
  [banks]
  (first (reduce
     (fn [[bank value] i]
       (let [current-value (get banks i)]
         (if (> current-value value)
           [i current-value]
           [bank value])))
     [0 (first banks)]
     (range 1 (count banks)))))

(test #'largest-bank)

(defn
  ^{:test (fn []
            (is (= [2 4 1 2] (distribute-largest-block 2 [0 2 7 0]))))}
  distribute-largest-block
  [banks]
  (let [from (largest-bank banks)
        v (get banks from)]
    (reduce
     (fn [b i] (update b i inc))
     (assoc banks from 0)
     (take v (drop (inc from) (cycle (range (count banks))))))))

(test #'distribute-blocks)

(defn
  ^{:test (fn []
            (is (= 5 (distribute-count [0 2 7 0]))))}
  distribute-count
  [banks]
  (reduce
   (fn [[current-banks seen-banks] i]
     (let [new-banks (distribute-largest-block current-banks)]
       (if (seen-banks new-banks)
         (reduced (inc i))
         [new-banks (conj seen-banks new-banks)])))
   [banks #{banks}]
   (range)))

(test #'distribute-count)

(defn silver-star []
  (distribute-count (inpt)))

;; gold star

(defn
  ^{:test (fn []
            (is (= [2 4 1 2] (first-seen-banks [0 2 7 0]))))}
  first-seen-banks
  [banks]
  (reduce
   (fn [[current-banks seen-banks] i]
     (let [new-banks (distribute-largest-block current-banks)]
       (if (seen-banks new-banks)
         (reduced new-banks)
         [new-banks (conj seen-banks new-banks)])))
   [banks #{banks}]
   (range)))

(test #'first-seen-banks)

(defn
  ^{:test (fn []
            (is (= 4 (cycle-count [0 2 7 0]))))}
  cycle-count
  [banks]
  (let [start (first-seen-banks banks)]
    (distribute-count start)))

(test #'cycle-count)

(defn gold-star []
  (cycle-count (inpt)))
