(ns advent-of-code.day-7a
  (:require [clojure.string :as string]))

(def actions ["AND" "OR" "LSHIFT" "RSHIFT" "NOT"])

(defn parse-token [token lex]
  (if (re-find #"\d+" token)
    (Integer. token)
    (lex (keyword token))))

(defn try-and [tokens lex]
  (let [[sx _ sy sz] tokens
        x (parse-token sx lex)
        y (parse-token sy lex)
        z (keyword sz)]
    (if (and x y)
      [true (assoc lex z (bit-and x y))]
      [false lex])))

(defn try-or [tokens lex]
  (let [[sx _ sy sz] tokens
        x (parse-token sx lex)
        y (parse-token sy lex)
        z (keyword sz)]
    (if (and x y)
      [true (assoc lex z (bit-or x y))]
      [false lex])))

(defn try-lshift [tokens lex]
  (let [[sx _ sy sz] tokens
        x (parse-token sx lex)
        z (keyword sz)]
    (if x
      [true (assoc lex z (bit-shift-left x (Short. sy)))]
      [false lex])))

(defn try-rshift [tokens lex]
  (let [[sx _ sy sz] tokens
        x (parse-token sx lex)
        z (keyword sz)]
    (if x
      [true (assoc lex z (bit-shift-right x (Short. sy)))]
      [false lex])))

(defn try-not [tokens lex]
  (let [[_ sx sy] tokens
        x (parse-token sx lex)
        y (keyword sy)]
    (if x
      [true (assoc lex y (bit-and (bit-not x) 0xffff))]
      [false lex])))

(defn try-assign [tokens lex]
  (let [[sx sy] tokens
        x (parse-token sx lex)
        y (keyword sy)]
    (if x
      [true (assoc lex y x)]
      [false lex])))

(defn apply-action [tokens lex]
  (cond (= (nth tokens 1) "AND") (try-and tokens lex)
        (= (nth tokens 1) "OR") (try-or tokens lex)
        (= (nth tokens 1) "LSHIFT") (try-lshift tokens lex)
        (= (nth tokens 1) "RSHIFT") (try-rshift tokens lex)
        (= (first tokens) "NOT") (try-not tokens lex)
        (= (count tokens) 2) (try-assign tokens lex)))

(defn parse-line [line]
  (->> (string/split line #" ")
       (remove #(= "->" %))))

(defn do-a-pass [lines input]
  (loop [program lines
         lex input
         next_lines []]
    (if-let [l (first program)]
      (let [[status next_lex] (apply-action l lex)]
        (cond (false? status) (recur (next program) next_lex (conj next_lines l))
              (true? status) (recur (next program) next_lex next_lines)))
      [lex next_lines])))

(defn solve [lines]
  (loop [program lines
         lex {}
         cnt 0]
    (let [[next_lex next_lines] (do-a-pass program lex)]
      (cond (:a next_lex) (:a next_lex)
            (and (seq next_lines) (> 120 cnt)) (recur next_lines next_lex (inc cnt))
            :else [next_lex cnt]))))

(defn data [filename]
  (->> (line-seq (clojure.java.io/reader filename))
       (map parse-line)))

(defn part_1 []
  (solve (data "resources/day_7_input.txt")))

;; For part two just change the input line
;; that defines b
;; from 14146 -> b
;; to
;; 956 -> b
