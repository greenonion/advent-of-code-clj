(ns advent-of-code.day-5)

(def vowels "aeiou")

(def forbidden #{"ab" "cd" "pq" "xy"})

(defn occurrences [char string]
  (count (filter #{char} string)))

(defn contains-three-vowels? [string]
  (->> (map #(occurrences % string) vowels)
       (reduce +)
       (<= 3)))

(defn contains-twice-in-a-row? [string]
  (loop [pairs (partition 2 1 string)]
    (if-let [[a b] (first pairs)]
      (if (= a b)
        true
        (recur (next pairs)))
      false)))

(defn not-contains-forbidden? [string]
  (loop [pairs (partition 2 1 string)]
    (if-let [[a b] (first pairs)]
      (if (forbidden (str a b))
        false
        (recur (next pairs)))
      true)))

(defn contains-pair-twice2? [string]
  (->> (partition 2 string)
       frequencies
       vals
       (some #(> % 1))
       boolean))

(defn contains-pair-twice? [string]
  (loop [pairs (partition 2 1 string)
         prev-pair nil
         freqs {}]
    (let [pair (first pairs)]
      (cond (nil? pair) false
            (= pair prev-pair) (recur (next pairs) pair freqs)
            (freqs pair) true
            :else (recur (next pairs) pair (assoc freqs pair 1))))))

(defn contains-repeating-trio? [string]
  (loop [pairs (partition 3 1 string)]
    (if-let [[a b c] (first pairs)]
      (if (= a c)
        true
        (recur (next pairs)))
      false)))

(defn nice? [string]
  (and (contains-three-vowels? string)
       (contains-twice-in-a-row? string)
       (not-contains-forbidden? string)))

(defn nice2? [string]
  (and (contains-pair-twice? string)
       (contains-repeating-trio? string)))

(defn data [file]
  (line-seq (clojure.java.io/reader file)))

(defn part_1 [filename]
  (count (filter nice? (data filename))))

(defn part_2 [filename]
  (count (filter nice2? (data filename))))
