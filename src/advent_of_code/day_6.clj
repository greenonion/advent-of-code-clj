(ns advent-of-code.day-6
  (:require [clojure.string :as string]))

(defn create-grid [n]
  (vec (repeat n (vec (repeat n 0)))))

(defn turn-on [grid [x y]]
  (assoc-in grid [x y] 1))

(defn turn-off [grid [x y]]
  (assoc-in grid [x y] 0))

(defn toggle [grid [x y]]
  (update-in grid [x y] #(if (= 0 %) 1 0)))

(defn lights-affected [[a b] [c d]]
  (for [x (range a (inc c))
        y (range b (inc d))]
    [x y]))

(defn operate [grid action from to]
  (loop [g grid
         lights (lights-affected from to)]
    (if-let [light (first lights)]
      (recur (action g light) (next lights))
      g)))

(defn lit [grid]
  (reduce + (map #(reduce + %) grid)))

(defn parse-coords [coords]
  (map #(Integer. %) (clojure.string/split coords #",")))

(defn apply-toggle [grid [from _ to]]
  (operate grid toggle (parse-coords from) (parse-coords to)))

(defn apply-on [grid [from _ to]]
  (operate grid turn-on (parse-coords from) (parse-coords to)))

(defn apply-off [grid [from _ to]]
  (operate grid turn-off (parse-coords from) (parse-coords to)))

(defn parse-line [grid line]
  (let [line (string/split line #" ")]
    (cond (= "toggle" (first line)) (apply-toggle grid (rest line))
          (= "on" (second line)) (apply-on grid (rest (rest line)))
          (= "off" (second line)) (apply-off grid (rest (rest line))))))

(defn data [file]
  (line-seq (clojure.java.io/reader file)))

(defn part_1 [filename]
  (lit (reduce #(parse-line %1 %2) (create-grid 1000) (data filename))))
