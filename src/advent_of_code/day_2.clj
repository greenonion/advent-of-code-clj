(ns advent-of-code.day-2
  (:require [clojure.string :as string]))

(defn parse-line [line]
  (->> (string/split line #"x")
       (map #(Integer. %))
       sort))

(defn parcel-paper [[a b c]]
  (+ (* a b)
     (* 2 a b)
     (* 2 b c)
     (* 2 a c)))

(defn parcel-ribbon [[a b c]]
  (+ (* 2 (+ a b))
     (* a b c)))

(defn data [file]
  (line-seq (clojure.java.io/reader file)))

(defn paper-ops [line]
  (-> (parse-line line)
      parcel-paper))

(defn ribbon-ops [line]
  (-> (parse-line line)
      parcel-ribbon))

(defn part_1 []
  (->> (map paper-ops (data "resources/day_2_input.txt"))
       (reduce +)))

(defn part_2 []
  (->> (map ribbon-ops (data "resources/day_2_input.txt"))
       (reduce +)))
