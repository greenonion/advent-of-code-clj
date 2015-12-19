(ns advent-of-code.day-3)

(def rules
  {\^ [1 0]
   \v [-1 0]
   \> [0 1]
   \< [0 -1]})

(defn moves_1 [start directions]
  (loop [pos start
         houses #{start}
         d directions]
    (if (first d)
      (let [next-pos (map + pos (rules (first d)))]
        (recur next-pos (conj houses next-pos) (next d)))
      (count houses))))

(defn moves_2 [start directions]
  (loop [pos1 start
         pos2 start
         houses #{start}
         d directions]
    (if (first d)
      (let [next-pos1 (map + pos1 (rules (first d)))
            next-pos2 (map + pos2 (rules (second d)))]
        (recur next-pos1 next-pos2 (conj houses next-pos1 next-pos2) (next (next d))))
      (count houses))))

(defn part_1 []
  (let [directions (slurp "resources/day_3_input.txt")]
    (moves_1 [0 0] directions)))

(defn part_2 []
  (let [directions (slurp "resources/day_3_input.txt")]
    (moves_2 [0 0] directions)))
