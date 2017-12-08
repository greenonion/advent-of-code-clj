(ns advent-of-code.2017.day-8
  (:require [clojure.string :as string]
            clojure.set)
  (:use clojure.test))

(defn inpt []
  (as-> "resources/2017/day_8_input.txt" $
    (slurp $)
    (string/split $ #"\n")
    (apply vector $)))

(defn data []
  (map parse-line (inpt)))

(defn str->fn [str]
  (cond
    (= str "inc") +
    (= str "dec") -
    (= str "!=") not=
    :else (resolve (symbol str))))

(defn parse-line [line]
  (let [[r op v _ vr co vv] (string/split line #" ")]
    {:r r
     :op (str->fn op)
     :op-value (Integer/parseInt v)
     :cond-r vr
     :cond-op (str->fn co)
     :cond-value (Integer/parseInt vv)}))

(defn apply-instruction [env instruction]
  (if ((:cond-op instruction) (get env (:cond-r instruction) 0) (:cond-value instruction))
    (if (env (:r instruction))
      (update env (:r instruction) (:op instruction) (:op-value instruction))
      (assoc env (:r instruction) ((:op instruction) 0 (:op-value instruction))))
    env))

(defn max-val [env]
  (apply max (vals env)))

(defn silver-star []
  (max-val (reduce apply-instruction {} (data))))

;; gold star

(defn gold-star []
  (last
   (reduce
    (fn [[env mv] i]
      (let [new-env (apply-instruction env i)
            new-mv (max-val new-env)]
        (if (> new-mv mv)
          [new-env new-mv]
          [new-env mv])))
    [{} 0]
    (data))))

