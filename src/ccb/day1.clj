(ns ccb.day1
  (:require [clojure.java.io :as io]))

(defn day-1 [filename]
    (loop [lines (line-seq (io/reader (io/resource filename)))
           max-calories 0
           candidate 0]
      (let [line (first lines)]
        (cond
          (nil? line) (max max-calories candidate)
          ;; candidate is complete, contest?
          (empty? line)
          (recur (rest lines)
                 (max max-calories candidate)
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

  ,)
