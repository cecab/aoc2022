(ns ccb.day8
  (:require [clojure.java.io :as io]))

(defn how-many-seen?
    "How many trees I can see in this collection, starting from index 0
     that are lower or equal that me UNTIL I find one that is equal or greater"
    [coll ref-value]
    (loop [not-seen coll
           blocker []
           already-seen []]
      (cond
        (or (empty? not-seen)
            (not (empty? blocker)))
        already-seen
        :else
        (recur
         (rest not-seen)
         (if (>= (first not-seen) ref-value)
           (conj blocker (first not-seen))
           blocker)
         (conj already-seen (first not-seen))))))

(defn count-seen? [coll ref-value]
  (count (how-many-seen? coll ref-value)))


(defn score-stream 
    "Get the score in coll for the element idx"
    [coll idx]
    (let [up-stream (reverse (range 0 idx))
          down-stream (range (inc idx) (count coll))]
      [(count-seen? (mapv #(get coll %) up-stream) (get coll idx))
       (count-seen? (mapv #(get coll %) down-stream) (get coll idx))]))

(defn scenic-score [matrix row-idx col-idx]
  ;; Check from all angles.
  (let [my-row (get matrix row-idx)
        my-column (get-column matrix row-idx col-idx)]
    (apply * 
           (concat (score-stream my-row col-idx)
                   (score-stream my-column row-idx)))))



(comment
  (def filename "day8-ex.txt")
  (def filename "day8.input.txt")
  
  (def matrix
    (->>
     (line-seq
      (io/reader
       (io/resource filename)))
     (mapv seq)
     (mapv (fn [row] (mapv #(Integer/parseInt (str %)) row)))))
  ;; Navigate
  (defn get-element [matrix row col]
    (-> matrix (get row) (get col)))
  (defn m-size [matrix]
    [(count matrix) (count (first matrix))])
  ;; tests..
  (assert (= 5 (get-element matrix 1 1)))
  (assert (= [5 5] (m-size matrix)))

  ;; A generic 'walker' over each element.
  (defn walker [matrix w-fn]
    (vec
     (map-indexed
      (fn [ row-idx row]
        (vec (map-indexed (fn [col-idx element] (w-fn element row-idx col-idx)) row)))
      matrix)))
  
  (def w-fn (fn [e r c] e))
  ;; Test
  (assert (= matrix 
             (walker matrix w-fn)))
  (assert (=
           (mapv (partial * 2) (first matrix))
           (first 
            (walker matrix (fn [e r c] (* 2 e))))))
  (defn get-column
    [matrix row-idx col-idx]
    (mapv (fn [row]
            (get row col-idx))
          matrix))
  (defn make-visible?
    [coll ref-val]
    (not-any? #(<= ref-val %) coll))
  
  (assert (= false (make-visible? [1 8 3] 7)))
  (assert (= true (make-visible? [4 3 2] 8)))
  
  (defn element-visible?
    " In coll we check if element idx is visible from any of edges"
    [coll idx]
    (let [up-stream (range 0 idx)
          down-stream (range (inc idx) (count coll))]
      (or (make-visible? (mapv #(get coll %) up-stream) (get coll idx))
          (make-visible? (mapv #(get coll %) down-stream) (get coll idx)))))
  ;; tests
  (assert (= true (element-visible? [2 5 5 1 2] 1)))
  (assert (= false (element-visible? [6 5 3 3 2] 2)))
  ;; ----
  ;; Is it visible?
  (defn is-visible? [matrix row-idx col-idx]
    ;; Check from all angles.
    (let [my-row (get matrix row-idx)
          my-column (get-column matrix row-idx col-idx)]
      (or (element-visible? my-row col-idx)
          (element-visible? my-column row-idx))))

  (assert (= [true true false
              true false true
              false true false
              ]
             [(is-visible? matrix 1 1)
              (is-visible? matrix 1 2)
              (is-visible? matrix 1 3)
              (is-visible? matrix 2 1)
              (is-visible? matrix 2 2)
              (is-visible? matrix 2 3)
              (is-visible? matrix 3 1)
              (is-visible? matrix 3 2)
              (is-visible? matrix 3 3)]))
  ;;
  (defn day-8-part-1 [matrix]
    (->> (walker matrix
                 (fn [element row-idx col-idx]
                   (is-visible? matrix row-idx col-idx)))
         flatten
         (filter identity)
         count))

  (day-8-part-1 matrix) ;; 1690
  21
  
  
  (let [[n-rows n-cols] (m-size matrix)
        ;; Borders are always visible
        always-visible [#{0 (dec n-rows)} #{0 (dec n-cols)}]]
    
    
    )
  ;; Part 2
  ;; The new part is about the ' trees I can see' which includes
  ;; those trees <= height than me, including the last one which
  ;; stops me to see beyond.
  (assert (= [[3] [5] [1 2] [3 5]]
             [(how-many-seen? [3] 5)
              (how-many-seen? [5 2] 5)
              (how-many-seen? [1 2] 5)
              (how-many-seen? [3 5 3] 5)]))
  ;; other example
  (assert (= [[3 5] [3 3 ] [3] [4 9]]
             [(how-many-seen? [3 5 3] 5)
              (how-many-seen? [3 3] 5)
              (how-many-seen? [3] 5)
              (how-many-seen? [4 9] 5)]))

  

  (assert (= 2 (score-stream [3 5 3 5 3] 1)))
  (score-stream [2 5 5 1 2] 2)
  
  (assert (= 4 
             (scenic-score matrix 1 2))) ;;
  (assert (= 8 (scenic-score matrix 3 2)))
  ;; Get the highest
  (->
   (walker matrix (fn [e r c] (scenic-score matrix r c)))
   flatten
   sort
   last)
  535680 ;; Correct! 
  ;; with input 
  
  

  ,)


