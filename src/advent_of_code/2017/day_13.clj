(ns advent-of-code.2017.day-13
  (:require [clojure.string :as string])
  (:use clojure.test))

(def inpt
  (-> (slurp "resources/2017/day_13_input.txt")
      string/trim-newline))

(defn lines [input]
  (string/split input  #"\n"))

(defn parse-line [line]
  (let [[depth range] (->> (string/split line #": ")
                           (map #(Integer/parseInt %)))]
    {depth {:current 0 :range range :dir :asc}}))

(defn initial-state [input]
  {:firewall (apply merge (map parse-line (lines input)))
   :depth -1})

(defn move-packet [state]
  (update state :depth inc))

(defn caught? [state]
  (= 0 (:current ((:firewall state) (:depth state)))))

(defn move-scanner [scanner]
  (let [{:keys [current range dir]} scanner]
    (if (= dir :asc)
      (if (= current (dec range))
        {:current (dec current)
         :dir :desc
         :range range}
        {:current (inc current)
         :dir dir
         :range range})
      (if (= current 0)
        {:current (inc current)
         :dir :asc
         :range range}
        {:current (dec current)
         :dir :desc
         :range range}))))

(defn move-scanners [state]
  (let [f (reduce-kv
           (fn [coll d scanner]
             (assoc coll d (move-scanner scanner)))
           {}
           (:firewall state))]
    {:depth (:depth state)
     :firewall f}))

(defn move-through-firewall [start]
  (let [depth (apply max (keys (:firewall start)))]
    (reduce
     (fn [{:keys [state severity]} depth]
       (let [layer ((:firewall state) depth)]
         (let [mp (move-packet state)
               ms (move-scanners mp)]
           (prn (caught? mp))
           (if (and layer (caught? mp))
             {:state ms
              :severity (+ severity (* depth (:range layer)))}
             {:state ms
              :severity severity}))))
     {:state start
      :severity 0}
     (range (inc depth)))))

(defn silver-star []
  (:severity (move-through-firewall (initial-state inpt))))

;; gold star

;; version of the above that stops if we get caught
(defn can-move-through-firewall? [start]
  (let [depth (apply max (keys (:firewall start)))]
    (reduce
     (fn [state depth]
       (let [layer ((:firewall state) depth)]
         (let [mp (move-packet state)
               ms (move-scanners mp)]
           (if (and layer (caught? mp))
             (reduced false)
             ms))))
     start
     (range (inc depth)))))

(defn fewest-picos [input]
  (let [out (reduce
             (fn [state i]
               (if (false? (can-move-through-firewall? state))
                 (move-scanners state)
                 (reduced i)))
             (initial-state input)
             (range))]
    (if (number? out) out false)))

;; This is SLOW. It turns out in the case of my input the number of picoseconds
;; to wait is 3,970,918. This requires as many iterations and it takes several
;; minutes to complete.
;; It is obvious that there should be a more clever way to derive this, but I'll
;; leave it for another day :)
;; But it might be a nice playground of profiling code to understand the
;; bottleneck!
(defn gold-star []
  (fewest-picos inpt))
