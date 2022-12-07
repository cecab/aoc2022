(ns ccb.day1
  (:require [clojure.java.io :as io]))

(defn day-1 [filename]
    (loop [lines (line-seq (io/reader (io/resource filename)))
           max-calories [0 0 0]
           candidate 0]
      (let [line (first lines)]
        (cond
          (nil? line) (->> (conj max-calories candidate)
                          sort
                          (take-last 3)
                          (reduce + 0))
          ;; candidate is complete, contest?
          (empty? line)
          (recur (rest lines)
                 (->> (conj max-calories candidate)
                          sort
                          (take-last 3))
                 0)
          ;; candidate still in progress
          :else
          (recur (rest lines)
                 max-calories
                 (+ candidate (Integer/parseInt line)))))))
(comment
  (def filename "day1-ex.txt")
  (day-1 filename)
  ;; the challenge.
  (day-1 "day1.input.txt") ;; 75501
  ;; Part 2
  ;; Keep 3 top, not one.
  (day-1 "day1.input.txt") ;; 215594
  ,)
