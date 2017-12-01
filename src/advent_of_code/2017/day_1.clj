(ns advent-of-code.2017.day-1)

(def inpt
  (let [data (slurp "resources/2017/day_1_input.txt")]
    (take (dec (count data)) data)))

(defn cycle-plus-1 [a]
  (take (inc (count a)) (cycle a)))

(defn conseq-pairs [a]
  (partition 2 1 (cycle-plus-1 a)))

(defn add-if-conseq [a]
  (reduce
   (fn [n [a b]] (+ n (if (= a b) (Character/getNumericValue a) 0)))
   0
   a))

(defn first-star []
  (add-if-conseq (conseq-pairs inpt)))

(defn pair-nth [a]
  (map
   (fn [a b] [a b])
   a
   (drop (/ (count a) 2) (cycle a))))

(defn second-star []
  (add-if-conseq (pair-nth inpt)))
