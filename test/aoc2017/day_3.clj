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
  (if (or (>= x dim) (>= y dim) (< x 0) (< y 0))
    nil
    (let [index (+ (* y dim) x)]
      (nth cells index))))

(defn gset
  [{:keys [dim cells] :as g} x y v]
  {:pre [(< x dim) (< y dim)]}
  (let [index (+ (* y dim) x)]
    (update g :cells assoc index v)))

(defn print-grid
  ([g]
   (print-grid g 8))
  ([{:keys [dim cells] :as g} n]
   (doall
    (for [r (range dim)]
      (println (apply str (map #(format (str "%" n "d") (gget g % r)) (range dim))))))
   nil))

(defn get-surrounding-sum
  [g x y]
  (let [coords [[(dec x) (dec y)] [x (dec y)] [(inc x) (dec y)]
                [(dec x) y] [(inc x) y]
                [(dec x) (inc y)] [x (inc y)] [(inc x) (inc y)]]
        result (apply + (keep (partial apply gget g) coords))]
    (if (zero? result)
      1
      result)))

(defn turn->process
  [turns]
  (case (mod turns 4)
    0 [1 0]
    1 [0 -1]
    2 [-1 0]
    3 [0 1]))

(defn plan-walk
  [grid]
  (let [cells (* (:dim grid) (:dim grid))
        all-moves (take cells (map (comp (partial repeat 2) inc) (range)))
        ;; there's probably an algorithm for this bit
        [_ limit] (reduce (fn [[s m :as a] i]
                            (let [s' (+ s i)]
                              (if (<= s' cells)
                                [s' (inc m)] a))) [0 0] (flatten all-moves))]
    (loop [moves all-moves
           turns 0
           agg []]
      (let [[new-turns new-agg]
            (loop [move (first moves)
                   turns turns
                   agg agg]
              (let [t (inc turns)
                    a (conj agg [(first move) (turn->process turns)])]
                (if (next move)
                  (recur (next move) t a)
                  [t a])))]
        (if (next moves)
          (recur (next moves) new-turns new-agg)
          (take limit new-agg))))))

(defn make-grid
  [n]
  (let [grid (grid n)
        plan (plan-walk grid)]
    (loop [plan plan
           grid grid
           [x y] (:center grid)]
      (let [[moves [diff-x diff-y]] (first plan)
            r (reduce
               (fn [{:keys [grid x y] :as a} _]
                 (let [sum (get-surrounding-sum grid x y)]
                   (assoc a
                          :grid (gset grid x y sum)
                          :x (+ x diff-x)
                          :y (+ y diff-y))))
               {:grid grid :x x :y y}
               (range moves))]
        (if (next plan)
          (recur (next plan) (:grid r) [(:x r) (:y r)])
          (:grid r))))))

(defn check-grid
  [input grid]
  (let [ordered (vec (reverse (sort (:cells grid))))]
    (last (take-while #(> % input) ordered))))

(deftest gget-tests
  (is (= 3  (gget {:dim 2,  :cells (range 0 4)}   1 1)))
  (is (= 20 (gget {:dim 20, :cells (range 0 400)} 0 1)))
  (is (= 21 (gget {:dim 20, :cells (range 0 400)} 1 1))))

(deftest result-2
  (is (= 312453 (check-grid input (make-grid 9)))))
