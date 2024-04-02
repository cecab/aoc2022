(ns ccb.day9
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))


(defn parse-command
    [line]
    (let [ [cmd size] (-> line (str/split #" "))]
      [(keyword cmd) (Integer/parseInt size)]))

(defn rope-tour
    [lines start-tail start-head]
    (loop [[p-tail p-head] [start-tail start-head]
           commands (expand-commands lines)
           tail-visited #{start-tail}]
      (cond
        (empty? commands)
        tail-visited
        :else
        (let [next-command (-> commands first cmd-dispatcher)
              next-head (-> p-head next-command)
              next-tail (follow-head p-tail next-head)]
          (recur
           ;; next posititions for tail and head.
           ;; HEAD always can move freely
           [next-tail next-head]
           ;; rest of commands..
           (rest commands)
           ;; Annotated visited coords for fail.
           (conj tail-visited next-tail))))))

(comment
  (def filename "day9-ex.txt")
  (def filename "day9-ex-2.txt")
  (def filename "day9.input.txt")
  
  (def lines
    (line-seq
     (io/reader
      (io/resource filename))))
  ;; Let's model the basic move of the tail given the head's coordinates
  ;; [row col] is a coordinate pair
  
  (def tail-point [4 0])
  (def head-point [4 0])
  (defn distance-cord [from-point to-point]
    (mapv (fn [from to] (- to from)) from-point to-point))

  (assert (= [0 0] (distance-cord [4 0] [4 0])))
  (assert (= [0 1] (distance-cord [4 1] [4 2])))
  (assert (= [0 -1] (distance-cord [4 2] [4 1])))
  (assert (= [-1 1] (distance-cord [4 3] [3 4])))
  ;; What happen when HEAD move 'too far'  from TAIL?
  ;; like here? 
  (distance-cord [4 3] [2 4]) ;; [-2 1]
  (distance-cord [1 4] [0 2]) ;; [-1 -2]
  ;; We define too-far? when steps is greater than 2
  (defn distance-steps [dx dy]
    (+ (abs dx) (abs dy)))
  (assert (= 2 (distance-steps -1 1)))
  (assert (= 3 (distance-steps -2 1)))
  (defn too-far? [dx dy]
    (cond
      (or (zero? dx) (zero? dy))
      (>= (distance-steps dx dy) 2)
      :else
      (>= (distance-steps dx dy) 3)))
  (assert (= false (too-far? -1 1)))
  (assert (= true (too-far? -2 1)))
  (assert (= true (too-far? 0 -2)))

  (defn short-step
    [delta]
    (let [coef (if (< delta 0) -1 1)]
      (cond
        (zero? delta) delta
        (= 1 (abs delta))
        delta
        (= 2 (abs delta))
        coef)))
  (assert (zero? (short-step 0)))
  (assert (= -1 (short-step -1)))
  (assert (= 1 (short-step 1)))
  (assert (= 1 (short-step 2)))
  (assert (= -1 (short-step -2)))

  (defn move-step [[px py] [dx dy]]
    [(+ px dx) (+ py dy)])
  
  (assert (= [0 3] (move-step [1 4] [-1 -1])))
  
  (defn follow-head
    [tail-point head-point]
    (let [distance (distance-cord tail-point head-point)]
      (cond
        (not (apply too-far? distance)) tail-point
        :else
        (let [step-cord (mapv short-step distance)]
          (move-step tail-point step-cord)))))
  ;; Modeling commands
  (defn move-right [point]
    (move-step point [0 1]))
  (assert (= [0 3] (move-right [0 2])))
  (defn move-left [point]
    (move-step point [0 -1]))
  (defn move-up [point]
    (move-step point [-1 0]))
  (defn move-down [point]
    (move-step point [1 0]))
  ;; A circle movement.
  (assert (= [4 4] (-> [4 4] move-right move-up move-left move-down)))
  (follow-head [1 1] [1 3]) ;; [1 2]
  (follow-head [1 1] [3 1]) ;; [2 1]
  (follow-head [3 1] [1 2]) ;; [2 2]
  (follow-head [3 1] [2 3]) ;; [2 2]
  (follow-head [1 4] [0 2]) ;; [0 3]
  (follow-head [0 3] [0 1]) ;; [0 2]
  
  
  (assert (= [:D 3] (parse-command "D 3")))
  
  (defn parse-commands [lines]
    (mapv parse-command lines))
  
  (assert (= [[:D 3] [:U 5]] (parse-commands ["D 3" "U 5"])))
  ;; Better expand commands where eaxh one is ONE-STEP only!
  (defn expand-commands
    [lines]
    (mapcat #(apply repeat (reverse %)) (parse-commands lines)))
  
  (def cmd-dispatcher
    {:U move-up :L move-left :D move-down :R move-right})
  
  ;; ----
  
  ;; Tests...
  (count (rope-tour lines [4 0] [4 0])) ;; 13

  ;; With input data.
  (count (rope-tour lines [0 0] [0 0])) ;; 6563 correct!
  
  ;; 6562
  ;;That's not the right answer; your answer is too low. If you're stuck, make sure you're using the full input data; there are also some general tips on the about page, or you can ask for hints on the subreddit. Please wait one minute before trying again. (You guessed 6562.) [Return to Day 9]

  ;; Part 2
  ;; Many knots: H 1 2 3... 9 moving with similar rules but in a 'cascading' fashion
  (def multirope (vec (take 10 (repeat [4 0]))))
  ;; Moving it, means take the H and move, then follow the rest
  (defn move-multi
    [multirope cmd]
    (let [next-head (cmd (first multirope))
          next-tails
          (loop [leading-knot next-head
                 knots (rest multirope)
                 new-knots []]
            (cond
              (empty? knots) new-knots
              :else
              (let [next-leading-knot (follow-head (first knots) leading-knot)]
                (recur
                 ;; first knot is the next leading..
                 next-leading-knot
                 ;; Remove first knot to continue
                 (rest knots)
                 ;; Add new position of the next leader
                 (conj new-knots next-leading-knot)))))]
      (concat [next-head] next-tails)))

  ;; Tests..
  (-> multirope
      (move-multi  move-right)
      (move-multi  move-right)
      (move-multi  move-right)
      (move-multi  move-right))
  ;;
  (defn multi-tour
    [multirope lines]
    (let [commands (expand-commands lines)]
      (reduce
       (fn [acc next-cmd]
         (let [next-rope
               (update acc :rope move-multi next-cmd)]
           (update next-rope :tail-visited conj (-> next-rope :rope last))))
       {:rope multirope
        :tail-visited #{(last multirope)}}
       (map cmd-dispatcher commands))))
  ;; Test
  (-> multirope
      (multi-tour lines)
      :tail-visited
      count);; 1

  ;; Test 2
  (-> multirope
      (multi-tour lines)
      :tail-visited
      count) ;; 36
  ;; Input
  (-> multirope
      (multi-tour lines)
      :tail-visited
      count) ;; 2653
  ;; ok
  
  ,)



