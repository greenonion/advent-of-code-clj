(ns advent-of-code.2017.day-7
  (:require [clojure.string :as string]
            clojure.set)
  (:use clojure.test))

(defn inpt []
  (as-> "resources/2017/day_7_input.txt" $
    (slurp $)
    (string/split $ #"\n")
    (apply vector $)))

(defn parse-weight [weight]
  (Integer/parseInt (string/replace weight #"\(|\)" "")))

(defn parse-line [line]
  (let [elements  (string/split line #", | ")
        weight (parse-weight (second elements))]
    (if (= (count elements) 2)
      {:name (first elements)
       :weight weight
       :type :leaf}
      {:name (first elements)
       :type :mid
       :weight weight
       :children (drop 3 elements)})))

(defn parents-children [data]
  (reduce
   (fn [coll program]
     (let [elements (parse-line program)]
       (if-let [children (:children elements)]
         (-> (update-in coll [:children] #(into % children))
             (update-in [:parents] #(conj % (:name elements))))
         (update-in coll [:parents] #(conj % (:name elements))))))
   {:children #{}
    :parents #{}}
   data))

(defn no-parents [data]
  (let [{:keys [parents children]} (parents-children data)]
    (clojure.set/difference parents children)))

(defn silver-star []
  (first (no-parents (inpt))))

;; gold-star

(def root (silver-star))

(defn parse-data [data]
  (reduce
   (fn [coll line]
     (let [{:keys [name] :as node} (parse-line line)]
       (assoc coll name node)))
   {}
   data))

(defn children [node nodes]
  (when-let [c (:children node)]
    (vals (select-keys nodes c))))

(defn weight [root nodes]
  (let [children-weight (fn children-weight [node]
                          (let [w (:weight node)]
                            (if (= :mid (:type node))
                              (reduce + (cons w (map children-weight
                                                     (children node nodes))))
                              w)))]
    (children-weight root)))

(defn follow-imbalanced [root nodes]
  (let [fw (fn fw [node]
             (let [cw (map (fn [n] [(weight n nodes) n]) (children node nodes))]
               (if (apply = (map first cw))
                 []
                 (reduce-kv
                  (fn [i k v]
                    (when (= (count v) 1)
                      (let [ov (first (remove #{k} (map first cw)))
                            mv (:weight (second (first v)))]
                        (cons (+ (- ov k) mv)
                              (follow-imbalanced (second (first v)) nodes)))))
                  []
                  (group-by first cw)))))]
    (fw root)))

(defn gold-star []
  (let [data (parse-data (inpt))]
    (last (follow-imbalanced (get data root) data))))
