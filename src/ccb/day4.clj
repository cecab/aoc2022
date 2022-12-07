(ns ccb.day4
  (:require [clojure.java.io :as io]
            [clojure.set :as set]
            [clojure.string :as str]))

(defn pair-sets
    [l]
    (let [[r-from r-to]
          (->> (str/split l #"-")
               (map #(Integer/parseInt %)))]
      (into #{} (range r-from (inc r-to)))))
  (defn is-subset?
    [s1 s2]
    (or (set/subset? s1 s2)
        (set/subset? s2 s1)))

(defn is-overlap? [s1 s2]
  (not (empty? (set/intersection s1 s2))))

(defn check-line [line]
  (apply is-overlap?
         (->> (str/split line #",")
              (map pair-sets))))
  (defn day-4-part-1 [filename]
    (let [lines 
          (line-seq
           (io/reader
            (io/resource filename)))]
      (->> lines
           (filter check-line)
           count)))

(defn day-4-part-2
    [filename]
    (let [lines 
          (line-seq
           (io/reader
            (io/resource filename)))]
      (->> lines
           (filter check-line)
           count)))
(comment
  (def filename "day4-ex.txt")
  
  (def line (last lines))
  

  (day-4-part-1 filename)         ;; 2
  (day-4-part-1 "day4.input.txt") ;; 494
  ;; part 2
  (day-4-part-2 filename)
  (day-4-part-2 "day4.input.txt") ;; 833
  ,)
