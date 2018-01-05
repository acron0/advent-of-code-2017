(ns aoc2017.day-7
  (:require  [clojure.test :refer :all]
             [clojure.string :as str]
             [clojure.java.io :as io]))

(def input
  (-> (io/resource "day-7-input")
      (slurp)
      (str/split-lines)))

;;

(def tower-regex
  #"^([a-z]+) \(([0-9]+)\)( -> ([a-z, ]+))?")

(defn parse-tower
  [s]
  (let [[_ name weight _ children] (re-matches tower-regex s)]
    (merge {:name name
            :weight (Integer/parseInt weight)}
           (when children
             {:children (set (str/split children #", "))}))))

(defn build-list
  [towers]
  (reduce #(conj %1 (parse-tower %2)) [] towers))

(defn find-root
  [towers]
  (let [g (filter :children (build-list towers))
        all-names (set (map :name g))
        all-kids (set (mapcat :children g))]
    (clojure.set/difference all-names all-kids)))

;;

(deftest result-1
  (is (= #{"vvsvez"} (find-root input))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn descend
  [g {:keys [name children weight] :as node}]
  (if children
    (let [new-children (reduce
                        (fn [a child]
                          (->> child
                               (get g)
                               (descend g)
                               (assoc a child))) {} children)
          tower-weight (reduce #(+ %1 (or (:tower-weight (second %2))
                                          (:weight (second %2)))) weight new-children)]
      (assoc node
             :children new-children
             :tower-weight tower-weight))
    node))

(defn build-graph
  [towers]
  (let [g (into {} (map #(vector (:name %) %) (build-list towers)))
        root-name (first (find-root towers))
        root (get g root-name)]
    (descend g root)))

(defn find-unbalanced-tower
  [towers]
  (letfn [(check-node [{:keys [name children]}]
            (let [weights (remove nil?
                                  (loop [remaining-children children
                                         out []]
                                    (if (empty? remaining-children)
                                      out
                                      (let [child (second (first remaining-children))
                                            r (check-node child)]
                                        (when-not r
                                          (recur (rest remaining-children) (conj out (:tower-weight child))))))))]
              (when (> (count (set weights)) 1)
                (println name))))]
    (let [g (build-graph towers)]
      (check-node (build-graph towers))
      g)))

;;; I gave up trying to write a test and just splunked the result in the repl:

;; (- (get-in (find-unbalanced-tower input) [:children "nbyij" :children "tdxow" :children "ghwgd" :weight]) 8)
;; => 362
