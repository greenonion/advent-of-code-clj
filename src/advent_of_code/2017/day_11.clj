(ns advent-of-code.2017.day-11
  (:require [clojure.string :as string])
  (:use clojure.test))

;; This is an excellent resource for hexagonal grids:
;; https://www.redblobgames.com/grids/hexagons/

(defn inpt []
  (as-> "resources/2017/day_11_input.txt" $
    (slurp $)
    (string/trim-newline $)
    (string/split $ #",")))

;; this is the "axial coordinate" system described in the article
;; each square is [column, row]
(defn update-coords [[col row] dir]
  (condp = dir
    "n" [col (dec row)]
    "s" [col (inc row)]
    "se" [(inc col) row]
    "ne" [(inc col) (dec row)]
    "sw" [(dec col) (inc row)]
    "nw" [(dec col) row]))

(defn follow-path [start path]
  (reduce
   (fn [location direction]
     (update-coords location direction))
   start
   path))

(defn distance-from-center [[col row]]
  (/ (+ (Math/abs col)
        (Math/abs (+ col row))
        (Math/abs row)) 2))

(defn
  ^{:test (fn []
            (is (= 3 (distance-from-center-after-path ["ne" "ne" "ne"])))
            (is (= 0 (distance-from-center-after-path ["ne" "ne" "sw" "sw"])))
            (is (= 2 (distance-from-center-after-path ["ne" "ne" "s" "s"])))
            (is (= 3 (distance-from-center-after-path ["se" "sw" "se" "sw" "sw"]))))}
  distance-from-center-after-path [path]
  (-> (follow-path [0 0] path)
      distance-from-center))

(test #'distance-from-center-after-path)

(defn silver-star []
  (distance-from-center-after-path (inpt)))

;; gold star

(defn furthest-distance-in-path [path]
  (:max-distance (reduce
                  (fn [{:keys [location max-distance]} direction]
                    (let [next-location (update-coords location direction)
                          next-distance (distance-from-center next-location)]
                      (if (> next-distance max-distance)
                        {:location next-location :max-distance next-distance}
                        {:location next-location :max-distance max-distance})))
                  {:location [0 0]
                   :max-distance 0}
                  path)))

(defn gold-star []
  (furthest-distance-in-path (inpt)))
