(ns advent-of-code.2017.day-4
  (:require [clojure.string :as string])
  (:use clojure.test))

(defn inpt []
  (-> (slurp "resources/2017/day_4_input.txt")
      (string/split #"\n")))

(defn words [line]
  (string/split line #" "))

(defn
  ^{:test (fn []
            (is (= true (words (unique-words? "aa bb cc dd ee"))))
            (is (= false (words (unique-words? "aa bb cc dd aa"))))
            (is (= true (words (unique-words? "aa bb cc dd aaa")))))}
  unique-words? [words]
  (every? #(= % 1) (vals (frequencies words))))

(defn silver-star []
  (reduce + (map (fn [l] (if (unique-words? (words l)) 1 0)) (inpt))))

;; gold star

(defn
  ^{:test (fn []
            (is (= true (no-anagrams? (words "abcde fghij"))))
            (is (= false (no-anagrams? (words "abcde xyz ecdab"))))
            (is (= true (no-anagrams? (words "a ab abc abd abf abj"))))
            (is (= true (no-anagrams? (words "iiii oiii ooii oooi oooo"))))
            (is (= false (no-anagrams? (words "oiii ioii iioi iiio")))))}
  no-anagrams? [words]
  (let [sorted-words (set (map (comp #(apply str %) sort) words))]
    (= (count sorted-words) (count words))))

(defn gold-star []
  (reduce + (map (fn [l] (if (no-anagrams? (words l)) 1 0)) (inpt))))

