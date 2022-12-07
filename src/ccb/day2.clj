(ns ccb.day2
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))


(def points-db
  {"A" 1
   "B" 2
   "C" 3
   "X" 1
   "Y" 2
   "Z" 3})
(def match-points-db
  {"A" {
        :draw "X"
        :defeat "Z"
        :defeated "Y"}
   "B" {
        :draw "Y"
        :defeat "X"
        :defeated "Z"}
   "C" {
        :draw "Z"
        :defeat "Y"
        :defeated "X"}})
(defn lost?
  [oponent you]
  (if (= you (get-in match-points-db [oponent :defeat] ))
    0
    0))
(defn draw?
  [oponent you]
  (if (= you (get-in match-points-db [oponent :draw] ))
    3
    0))

(defn win?
  [oponent you]
  (if (= you (get-in match-points-db [oponent :defeated] ))
    6
    0))


(defn match-points
  [oponent you]
  (->> [lost? draw? win?]
       (map #(% oponent you) )
       (reduce + (get points-db you))))

(defn day-2-part-1
  [filename]
  (->>
   (line-seq
    (io/reader
     (io/resource filename)))
   (map
    (fn [str-line]
      (let [[oponent you] (-> str-line (str/split #" "))]
        (match-points oponent you))))
   (reduce + 0)))
(comment
  (def filename "day2-ex.txt")
  ;; define contest.
  
  
  
  (day-2-part-1 filename);; 15
  (day-2-part-1 "day2.input.txt") ;; 12645
  ,)
