(ns aoc2017.day-8
  (:require  [clojure.test :refer :all]
             [clojure.string :as str]
             [clojure.java.io :as io]))

(def input
  (-> (io/resource "day-8-input")
      (slurp)
      (str/split-lines)))

;;

(defn instruction->fn
  [x]
  (case x
    "inc" +
    "dec" -))

(defn parse-instruction
  [line]
  (let [[n f v _ cn cx cv] (str/split line #" ")]
    {:register/name n
     :register/value (Integer/parseInt v)
     :instruction/fn (instruction->fn f)
     :instruction/condition {:register/name cn
                             :condition/statement cx
                             :register/value (Integer/parseInt cv)}}))

(defn resolve-condition
  [rs {:keys [register/name condition/statement register/value]}]
  (let [v (get rs name 0)]
    (case statement
      "==" (= v value)
      "!=" (not= v value)
      "<=" (<= v value)
      ">=" (>= v value)
      "<"  (< v value)
      ">"  (> v value))))

(defn resolve-instruction
  [rs ins]
  (if (resolve-condition rs (:instruction/condition ins))
    (let [{:keys [register/name register/value instruction/fn]} ins
          v (get rs name 0)]
      (assoc rs name (fn v value)))
    rs))

(defn resolve-instruction-set
  [input]
  (reduce resolve-instruction {}
          (map parse-instruction input)))

;;

(deftest parse-instructions-test
  (is (= {:register/name "a"
          :register/value 3
          :instruction/fn +
          :instruction/condition {:register/name "a"
                                  :condition/statement "<"
                                  :register/value 1}}
         (parse-instruction "a inc 3 if a < 1"))))

(deftest condition-statement
  (is (= #{"!=" "<=" ">=" "<" "==" ">"}
         (set (map (comp :condition/statement :instruction/condition parse-instruction)
                   input)))))

(deftest condition-tests
  (letfn [(c [x]
            {:register/name "a"
             :condition/statement x
             :register/value 3})]
    (is (resolve-condition {"a" 3} (c "==")))
    (is (resolve-condition {"a" 4} (c "!=")))
    (is (resolve-condition {"a" 3} (c "<=")))
    (is (resolve-condition {"a" 2} (c "<=")))
    (is (resolve-condition {"a" 3} (c ">=")))
    (is (resolve-condition {"a" 4} (c ">=")))
    (is (resolve-condition {"a" 4} (c ">")))
    (is (resolve-condition {"a" 2} (c "<")))))

(deftest example-1-1
  (let [test-input ["a inc 3 if a < 1"
                    "b inc 12 if a > 1"]]
    (is (= {"a" 3 "b" 12}
           (resolve-instruction-set test-input)))))

(deftest example-1-2
  (let [test-input ["b inc 5 if a > 1"
                    "a inc 1 if b < 5"
                    "c dec -10 if a >= 1"
                    "c inc -20 if c == 10"]]
    (is (= {"a" 1 "c" -10}
           (resolve-instruction-set test-input)))))

(deftest result-1
  (is (= 7296 (apply max (vals (resolve-instruction-set input))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn resolve-instruction*
  [h]
  (fn [rs ins]
    (if (resolve-condition rs (:instruction/condition ins))
      (let [{:keys [register/name register/value instruction/fn]} ins
            v (get rs name 0)
            new-v (fn v value)]
        (when (> new-v @h)
          (reset! h new-v))
        (assoc rs name new-v))
      rs)))

(defn resolve-instruction-set*
  [input]
  (let [h (atom 0)]
    (reduce (resolve-instruction* h) {}
            (map parse-instruction input))
    @h))

(deftest result-2
  (is (= 8186 (resolve-instruction-set* input))))
