(ns day3.clj
  (:require [clojure.java.io :as io]
            [clojure.set :as set]
            ;;[clojure.string :as str]
            ))

(comment
  (def filename "day3-ex-p2.txt")
  ;; for priority
  (defn build-priorities
    [start-char start-offset]
    (mapcat
     (fn [offset]
       [(char (+ start-char offset)) (+ 1 offset start-offset)])
     (range 0 26)))
  (def priorities 
    (apply (partial assoc {}) 
           (concat 
            (build-priorities 65 26)
            (build-priorities 97 0))))
  
  (def s (first lines))
  (defn find-wrong-item [s]
    (let [s-len (count s)
          first-half (subs s 0 (/ s-len 2))
          second-half (subs s (/ s-len 2) s-len)]
      (first (set/intersection (set first-half) (set second-half)))))

  (defn badge [group]
    (let [[g1 g2 g3] (map set group)]
      (first (set/intersection g1 g2 g3))))
  
  (defn day-3-part-2
    [filename]
    (let [groups
          (partition 3
                     (line-seq
                      (io/reader
                       (io/resource filename))))]
      (->> groups
           (map badge)
           (map priorities)
           (reduce + 0))))

  (day-3-part-1 filename);; 157
  (day-3-part-1 "day3.input.txt") ;; 7872

  ;; part 2
  (day-3-part-2 filename) ;; 70 
  (day-3-part-2 "day3.input.txt") ;; 2497
  (set/subset? #{1 2 3} #{5 6 7 1 2 3}) ;; true
  

  ,)
