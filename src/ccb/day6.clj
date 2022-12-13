(ns ccb.day6
  (:require [clojure.java.io :as io]))
(defn day-6-part-1 [line]
    (loop [
           solution 0
           input (seq line)]
      (let [marker (set (take 4 input))]
        (cond
          (= 4 (count marker)) (+ solution 4)
          :else
          (recur (inc solution)
                 (rest input))))))
(defn day-6-part-1-partition [filename]
  (let [line (slurp
              (io/resource filename))]
    (+ 14
       (count
        (take-while #(not= 14 (count (set %)))
                    (partition 14 1 line))))))

(comment
  (f1)
  (def filename "day6.input.txt")
  

  (day-6-part-1-partition "bvwbjplbgvbhsrlpgdmjqwftvncz")      ;; 5
  (day-6-part-1-partition "nppdvjthqldpwncqszvftbrmjlhg")      ;; 6
  (day-6-part-1-partition "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg") ;; 10
  (day-6-part-1-partition "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw")  ;; 11
  
  (def line
    (slurp "resources/day6-ex.txt"))
  "mjqjpqmgbljsphdztnvjfqwrcgsmlb\n"
  (day-6-part-1-partition "day6.input.txt") ;; 3837
  ,)
