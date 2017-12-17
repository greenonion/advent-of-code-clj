(ns advent-of-code.2017.day-11
  (:require [clojure.string :as string])
  (:use clojure.test))

(def inpt
  (-> (slurp "resources/2017/day_12_input.txt")
      string/trim-newline))

(defn parse-input [data]
  (string/split data #"\n"))

(defn parse-line [line]
  (as-> line $
    (string/split $ #" <-> |, ")
    (map #(Integer/parseInt %) $)))

(defn data [in]
  (map parse-line (parse-input in)))

(defn pairs [parsed-line]
  (let [[x & xs] parsed-line]
    {x xs}))

(defn graph [data]
  (apply merge (map pairs data)))

;; See connected components theory for this exercise:
;; https://en.wikipedia.org/wiki/Connected_component_(graph_theory)

;; we need to do a depth-first traversal starting at 0
;; here's where I found this implementation of lazy graph DFS
;; http://hueypetersen.com/posts/2013/06/25/graph-traversal-with-clojure/
(defn seq-graph-dfs [g s]
  ((fn rec-dfs [explored frontier]
     (lazy-seq
       (if (empty? frontier)
         nil
         (let [v (peek frontier)
               neighbors (g v)]
           (cons v (rec-dfs
                     (into explored neighbors)
                     (into (pop frontier) (remove explored neighbors))))))))
   #{s} [s]))

(defn silver-star []
  (count (seq-graph-dfs (graph (data inpt)) 0)))

;; gold-star

;; To find all the connected components of a graph, loop through its vertices,
;; starting a new breadth first or depth first search whenever the loop reaches
;; a vertex that has not already been included in a previously found connected
;; component.
(defn connected-components [graph]
  (reduce
   (fn [cc node]
     ;; have we already included this node in a connected component?
     (if (some #(% node) cc)
       cc
       (conj cc (set (seq-graph-dfs graph node)))))
   []
   (keys graph)))

(defn gold-star []
  (count (connected-components (graph (data inpt)))))
