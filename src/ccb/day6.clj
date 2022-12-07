(ns ccb.day6)
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

(comment
  

  (day-6-part-1 "bvwbjplbgvbhsrlpgdmjqwftvncz") ;; 5
  (day-6-part-1 "nppdvjthqldpwncqszvftbrmjlhg") ;; 6
  (day-6-part-1 "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg") ;; 10
  (day-6-part-1 "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw") ;; 11
  
  (def line
    (slurp "resources/day6-ex.txt"))
  "mjqjpqmgbljsphdztnvjfqwrcgsmlb\n"
  ,)
