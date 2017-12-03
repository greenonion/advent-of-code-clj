(ns advent-of-code.2017.day-3
  [:use clojure.test])

(def inpt 265149)

(defn
  ^{:test (fn []
            (is (= 0 (ring 1)))
            (is (= 2 (ring 12)))
            (is (= 2 (ring 23))))}
  ring
  [n]
  (loop [i 0
         s 1]
    (if (<= n s)
      i
      (recur (inc i) (+ s (* 8 (inc i)))))))

(defn ring-size [r]
  (if (= r 0) 1 (* r 8)))

(defn ring-position [r i]
  (let [rs (ring-size r)]
    (mod (+ i (dec r)) (/ rs 4))))

(defn
  ^{:test (fn []
            (is (= 0 (manh-dist 1)))
            (is (= 3 (manh-dist 12)))
            (is (= 2 (manh-dist 23))))}
  manh-dist
  [i]
  (let [r (ring i)]
    (+ r (ring-position r i))))

(defn silver-star []
  (manh-dist inpt))

;; gold star

(defn next-square-and-dir [[x y] d]
  (let [[x2 y2] (cond
                  (= d :w) [(inc x) y]
                  (= d :n) [x (inc y)]
                  (= d :e) [(dec x) y]
                  (= d :s) [x (dec y)])]
    (cond
      (and (= d :n) (= x2 y2)) [[x2 y2] :e]
      (and (= d :e) (= (- x2) y2)) [[x2 y2] :s]
      (and (= d :s) (= x2 y2)) [[x2 y2] :w]
      (and (= d :w) (= x2 (inc (- y2)))) [[x2 y2] :n]
      :else [[x2 y2] d])))

(defn square-value [s m]
  (if (= s [0 0])
    1
    (let [neighbors (square-neighbors s)]
      (reduce + ((comp vals select-keys) m neighbors)))))

(defn square-spiral-goal [goal]
  (loop [s [0 0]
         dir :w
         m {}]
    (let [v (square-value s m)]
      (if (> v goal)
        v
        (let [[s2 d2] (next-square-and-dir s dir)]
          (recur s2 d2 (assoc m s v)))))))

(defn gold-star []
  (square-spiral-goal inpt))
