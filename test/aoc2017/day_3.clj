(ns aoc2017.day-3
  (:require  [clojure.test :refer :all]))

;; Hint: https://en.wikipedia.org/wiki/Centered_octagonal_number

;; 65  64  63  62  61  60  59  58  57
;; 66  37  36  35  34  33  32  31  56
;; 67  38  17  16  15  14  13  30  55
;; 68  39  18   5   4   3  12  29  54
;; 69  40  19   6   1   2  11  28  53
;; 70  41  20   7   8   9  10  27  52
;; 71  42  21  22  23  24  25  26  51
;; 72  43  44  45  46  47  48  49  50
;; 73  74  75  76  77  78  79  80  81

(def input 312051)

(defn circle-start
  "(2n+1)^2"
  [n]
  (int (Math/pow (inc (* 2 n)) 2)))

(defn circle
  "(sqrt(n)-1)/2"
  [n]
  (if (<= n 1)
    0
    (inc (int (/ (dec (Math/sqrt (dec n))) 2)))))

(defn distance
  [n]
  (if (<= n 1)
    0
    (let [c       (circle n)
          cs1     (inc (circle-start (dec c)))
          cs2     (circle-start c)
          len     (inc (- cs2 cs1))
          side    (inc (/ len 4))
          index   (mod (- len (- cs2 n)) len)
          centers (map (comp int #(+ % (int (/ side 2)))) (take 4 (take-nth (dec side) (range))))
          dst     (->> centers
                       (map (partial - index))
                       (map #(Math/abs %))
                       (apply min))]
      (+ dst c))))

(deftest circle-tests
  (is (= 0 (circle 1)))
  (is (every? (partial = 1) (map circle (range 2 10))))
  (is (every? (partial = 2) (map circle (range 11 26))))
  (is (every? (partial = 3) (map circle (range 27 50)))))

(deftest circle-start-tests
  (is (= 9 (circle-start 1)))
  (is (= 25 (circle-start 2)))
  (is (= 49 (circle-start 3))))

(deftest distance-tests
  (is (= 0 (distance 1)))
  (is (= 3 (distance 12)))
  (is (= 2 (distance 23)))
  (is (= 31 (distance 1024))))

;;;

(deftest result-1
  (is (= 430 (distance input))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn grid
  [d]
  {:pre [(odd? d)]}
  {:dim d
   :center [(int (/ d 2)) (int (/ d 2))]
   :cells (vec (repeat (* d d) 0))})

(defn gget
  [{:keys [dim cells] :as g} x y]
  (if (or (>= x dim) (>= y dim))
    nil
    (let [index (+ (* y dim) x)]
      (nth cells index))))

(defn gset
  [{:keys [dim cells] :as g} x y v]
  {:pre [(< x dim) (< y dim)]}
  (let [index (+ (* y dim) x)]
    (update g :cells assoc index v)))

(defn print-grid
  [{:keys [dim cells] :as g}]
  (for [r (range dim)]
    (println (apply str (map #(format "%6d" (gget g % r)) (range dim))))))

(defn get-surrounding-sum
  [g x y]
  (let [coords [[(dec x) (dec y)] [x (dec y)] [(inc x) (dec y)]
                [(dec x) y] [(inc x) y]
                [(dec x) (inc y)] [x (inc y)] [(inc x) (inc y)]]
        result (apply + (keep (partial apply gget g) coords))]
    (if (zero? result)
      1
      result)))

(defn walk-grid
  [input grd]
  (let [[start-x start-y] (:center grd)]
    (loop [g grd
           x start-x
           y start-y
           turns 1
           moves 1]
      (loop [m moves g' g x' x y' y]
        (let [surrounding-sum (get-surrounding-sum g' x y)
              _ (println "x=" x' "y=" y' "m=" m "sum="surrounding-sum)
              new-g (gset g' x' y' surrounding-sum)
              [new-x new-y]
              (case (mod turns 4)
                0 [(inc x) y]
                1 [x (inc y)]
                2 [(dec x) y]
                3 [x (dec y)])]
          (if (zero? m)
            {;; TODO ;;
             }
            (recur (dec m) new-g new-x new-y))))
      (if (> surrounding-sum input)
        new-g
        (recur new-g new-x new-y new-t new-m)))))

(deftest gget-tests
  (is (= 3  (gget {:dim 2,  :cells (range 0 4)}   1 1)))
  (is (= 20 (gget {:dim 20, :cells (range 0 400)} 0 1)))
  (is (= 21 (gget {:dim 20, :cells (range 0 400)} 1 1))))

(deftest example-1
  (walk-grid input (grid 3)))
