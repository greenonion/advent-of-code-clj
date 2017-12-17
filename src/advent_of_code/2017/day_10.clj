(ns advent-of-code.2017.day-10
  (:require [clojure.string :as string])
  (:use clojure.test))

(defn inpt []
  (as-> "resources/2017/day_10_input.txt" $
    (slurp $)
    (string/trim-newline $)
    (string/split $ #",")
    (map #(Integer/parseInt %) $)
    (vec $)))

(defn reversed-part [data position length]
  (reverse (take length (drop position (cycle data)))))

(defn reverse-and-join [data position length]
  (let [start (take position data)
        mid  (reversed-part data position length)
        end (drop (+ position length) data)
        joined (concat start mid end)]
    (if (= (count data) (count joined))
      joined
      (let [extra-count (- (count joined) (count data))
            start (drop (count data) joined)
            end (drop extra-count (take (count data) joined))]
        (concat start end)))))

(defn silver-star []
  (let [res (reduce
             (fn [{:keys [skip-size position l]} length]
               {:skip-size (inc skip-size)
                :position (mod (+ position length skip-size) (count l))
                :l (reverse-and-join l position length)})
             {:skip-size 0
              :position 0
              :l (vec (range 256))}
             (inpt))]
    (* (first (:l res)) (second (:l res)))))

;; gold star

(defn chars->bytes [chars]
  (map int chars))

(defn inpt-gold []
  (as-> "resources/2017/day_10_input.txt" $
    (slurp $)
    (string/trim-newline $)))

(defn convert-input [data]
  (apply conj (vec (chars->bytes data)) [17 31 73 47 23]))

(defn rounds [data skip-size position n]
  (reduce
   (fn [{:keys [skip-size position l]} length]
     {:skip-size (inc skip-size)
      :position (mod (+ position length skip-size) (count l))
      :l (reverse-and-join l position length)})
   {:skip-size skip-size
    :position position
    :l (vec (range 256))}
   (take (* n (count data)) (cycle data))))

(defn
  ^{:test (fn []
            (is (= "a2582a3a0e66e6e86e3812dcb672a272") (hexify ""))
            (is (= "33efeb34ea91902bb2f59c9920caa6cd") (hexify "AoC 2017"))
            (is (= "3efbe78a8d82f29979031a4aa0b16a9d") (hexify "1,2,3"))
            (is (= "63960835bcdc130f0b66d7ff4f6a5a8e") (hexify "1,2,4")))}
  hexify [data]
  (let [hexes (:l (rounds (convert-input data) 0 0 64))]
    (string/join
     (map (fn [s] (->> (apply bit-xor s) (format "%02x"))) (partition 16 hexes)))))

(test #'hexify)

(defn gold-star []
  (hexify (inpt-gold)))
