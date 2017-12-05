(ns advent-of-code.2017.day-5
  (:require [clojure.string :as string])
  (:use clojure.test))r

(defn inpt []
  (as-> "resources/2017/day_5_input.txt" $
      (slurp $)
      (string/split $ #"\n")
      (map #(Integer/parseInt %) $)
      (apply vector $)))

(defn update-pos [[p l]]
  (when-let [steps (get l p)]
    [(+ p steps) (update l p inc)]))

(defn
  ^{:test (fn []
            (is (= 5 (escape-list [0 3 0 1 -3] update-pos)))
            (is (= 10 (escape-list [0 3 0 1 -3] update-pos-strange))))}
  escape-list
  [starting-list update-fn]
  (dec (count (take-while identity (iterate update-fn [0 starting-list])))))

(test #'escape-list)

(defn silver-star []
  (escape-list (inpt) update-pos))

;; gold star

(defn update-pos-strange [[p l]]
  (when-let [steps (get l p)]
    (let [f (if (> steps 2) dec inc)]
      [(+ p steps) (update l p f)])))

(defn gold-star []
  (escape-list (inpt) update-pos-strange))
