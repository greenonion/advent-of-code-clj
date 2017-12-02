(ns advent-of-code.2017.day-2
  (:require [clojure.string :as string]
            [clojure.math.combinatorics :as combinatorics]))

(defn inpt []
  (-> (slurp "resources/2017/day_2_input.txt")
      (string/split #"\n")))

(defn line-elements [line]
  (as-> line $
    (string/split $ #"\t")
    (map #(Integer/parseInt %) $)))

(defn max-min [elements]
  (- (apply max elements) (apply min elements)))

(defn silver-star []
  (reduce + (map #(max-min (line-elements %)) (inpt))))

;; gold star

(defn divisable-quot [x y]
  (if (> x y)
    (if (zero? (mod x y)) (quot x y) 0)
    (if (zero? (mod y x)) (quot y x) 0)))

(defn fetch-divisable-quot [elements]
  (reduce +
          (map #(apply divisible-quot %)
               (combinatorics/combinations elements 2))))

(defn gold-star []
  (reduce + (map #(fetch-divisable-quot (line-elements %)) (inpt))))
