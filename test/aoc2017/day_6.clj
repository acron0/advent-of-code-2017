(ns aoc2017.day-6
  (:require  [clojure.test :refer :all]))

(def input [4 1	15 12 0	9 9 5 5	8 7 3 14 5 12 3])

;;;

(defn step
  [banks]
  (let [start (.indexOf banks (apply max banks))
        v (nth banks start)
        alterations (frequencies (map #(mod (+ start %) (count banks)) (range 1 (inc v))))
        reset-banks (assoc banks start 0)]
    (reduce-kv (fn [b index amount]
                 (update b index + amount))
               reset-banks alterations)))

(defn redistribute-banks
  [f banks]
  (loop [seen []
         latest banks]
    (let [new-banks (step latest)]
      (if (contains? (set seen) new-banks)
        (conj seen new-banks)
        (recur (conj seen new-banks) new-banks)))))

(deftest examples-1-1
  (is (= [2 4 1 2] (step [0 2 7 0])))
  (is (= [3 1 2 3] (step [2 4 1 2])))
  (is (= [0 2 3 4] (step [3 1 2 3]))))

(deftest examples-1-2
  (is (= 5 (count (redistribute-banks step [0 2 7 0])))))

;;;
(deftest result-1
  (is (= 6681 (count (redistribute-banks step input)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn loop-seq
  [banks]
  (let [[final-bank & rbank-seq] (reverse banks)]
    (conj (take-while (complement #{final-bank}) rbank-seq) final-bank)))

(deftest examples-2-1
  (is (= 4 (count (loop-seq (redistribute-banks step [0 2 7 0]))))))

(deftest result-2
  (is (= 2392 (count (loop-seq (redistribute-banks step input))))))
