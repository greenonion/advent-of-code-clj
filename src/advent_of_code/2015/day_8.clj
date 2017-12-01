(ns advent-of-code.day-8
  (:require [clojure.string :as string]))

(defn data [filename]
  (->> (line-seq (clojure.java.io/reader filename))
       (map string/trim)))

(defn characters-of-code [word]
  (count word))

(defn string-size [word]
  (-> (subs word 1 (- (count word) 1))
      (clojure.string/replace #"(\\x[0-9a-f]{2})" #(str (char (read-string (str "0" (subs (first %) 1))))))
      (clojure.string/replace #"\\\\" "\\'")
      (clojure.string/replace #"\\\"" "\"")
      count))

(defn string-size [word]
  (-> (count word)
      (- 2) ;; do not count quotes around string
      (- (* 3 (ascii-chars word)))
      (- (escaped-quotes word))
      (- (escaped-backslashes word))))

(defn part_1 [strings]
  (->> (map #(- (characters-of-code %) (string-size %)) strings)
       (reduce +)))
