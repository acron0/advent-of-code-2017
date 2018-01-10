(ns aoc2017.day-9
  (:require  [clojure.test :refer :all]
             [clojure.string :as str]
             [clojure.java.io :as io]))

(def input
  (-> (io/resource "day-9-input")
      (slurp)))

;;

(defn tokenize
  [line]
  (-> line
      (str/replace #"!." "")
      (str/replace #"<[^>]*>" "")
      (str/replace #"\{" " { ")
      (str/replace #"\}" " } ")
      (str/replace #"," "")
      (str/triml)
      (str/split #"\s+")))

(defn score
  [tokenized]
  (->> tokenized
       (reduce
        (fn [{:keys [out base] :as a} token]
          (assoc
           a
           :base (cond
                   (= "{" token) (inc base)
                   (= "}" token) (dec base)
                   :else base)
           :out (if (= "}" token)
                  (conj out base)
                  out)))
        {:out [] :base 0})
       :out
       (reduce + 0)))

(deftest tokenize-test
  (is (= ["{" "}"] (tokenize "{}")))
  (is (= ["{" "{" "}" "}"] (tokenize "{{}}")))
  (is (= ["{" "{" "}" "{" "}" "}"] (tokenize "{{},{}}")))
  (is (= ["{" "{" "{" "}" "{" "}" "{" "{" "}" "}" "}" "}"] (tokenize "{{{},{},{{}}}}")))
  (is (= ["{" "}"] (tokenize "{!}}"))))

(deftest garbage-test
  (is (= ["{" "}"] (tokenize "{<abc>}")))
  (is (= ["{" "}"] (tokenize "{<abc!>>}")))
  (is (= ["{" "}"] (tokenize "{<{{}}>}")))
  (is (= ["{" "{" "}" "{" "}" "}"] (tokenize "{{<ab>},{<ab>}}"))))

(deftest score-test
  (is (= 1 (score (tokenize "{}"))))
  (is (= 3 (score (tokenize "{{}}"))))
  (is (= 6 (score (tokenize "{{{}}}"))))
  (is (= 5 (score (tokenize "{{},{}}"))))
  (is (= 16 (score (tokenize "{{{},{},{{}}}}"))))
  (is (= 1 (score (tokenize "{<a>,<a>,<a>,<a>}"))))
  (is (= 9 (score (tokenize "{{<ab>},{<ab>},{<ab>},{<ab>}}"))))
  (is (= 9 (score (tokenize "{{<!!>},{<!!>},{<!!>},{<!!>}}"))))
  (is (= 3 (score (tokenize "{{<a!>},{<a!>},{<a!>},{<ab>}}")))))

(deftest result-1
  (is (= 14421 (score (tokenize input)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn garbage-only
  [line]
  (let [line (str/replace line #"!." "")
        line-ms (re-seq #"<([^>]*)>" line)]
    (->> line-ms
         (map second)
         (apply str))))

(deftest garbage-only-test
  (is (= "abc" (garbage-only "{<a>, <b>, {<c>}}") ))
  (is (= "<<<" (garbage-only "<<<<>") )))

(deftest result-2
  (is (= 6817 (count (garbage-only input)))))
