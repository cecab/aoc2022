(ns ccb.day2
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

;; Issue 11
(def points-db
  {"A" 10
   "B" 2
   "C" 3
   "X" :defeat
   "Y" :draw
   "Z" :defeated})

(def match-points-db
  {"A" {:draw "A"
        :defeat "C"
        :defeated "B"}
   "B" {:draw "B"
        :defeat "A"
        :defeated "C"}
   "C" {:draw "C"
        :defeat "B"
        :defeated "A"}})

(def winner-points {:draw 3
                    :defeat 0
                    :defeated 6})


(defn day-2-part-1
  [filename]
  (->>
   (line-seq
    (io/reader
     (io/resource filename)))
   (map
    (fn [str-line]
      (let [[oponent your-result] (-> str-line (str/split #" "))
            you (get-in match-points-db [oponent (points-db your-result)])]
        (+ (get points-db you) (winner-points (points-db your-result))))))
   (reduce + 0)))
(comment
  (def filename "day2-ex.txt")
  (day-2-part-1 filename) ;; 12
  (day-2-part-1 "day2.input.txt") ;; 11756
  ,)
