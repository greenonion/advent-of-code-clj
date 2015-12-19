(ns advent-of-code.day-1)

(defn part_1 []
  (let [data (-> (slurp "resources/day_1_input.txt")
                 frequencies)]
    (- (data \() (data \)))))

(defn part_2 []
  (let [data (slurp "resources/day_1_input.txt")]
    (loop [cnt 0
           floor 0
           coll data]
      (cond (= floor -1) cnt
            (= (first coll) \() (recur (inc cnt) (inc floor) (next coll))
            (= (first coll) \)) (recur (inc cnt) (dec floor) (next coll))))))
