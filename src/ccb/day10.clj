(ns ccb.day10
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))


(defn crt
  [lines]
    (map-indexed
     (fn [idx x]
       (let [sprite #{(dec x) x (inc x)}
             led (if (sprite (rem idx 40)) :lit :dark)]
         {:led led
          :sprite sprite
          :idx  idx
          :x x}))
     (:cycles (execute-commands lines))))
(comment
  ;;
  (def filename "day10-ex.txt")
  (def filename "day10-ex-2.txt")
  (def filename "day10.input.txt")
  
  (def lines
    (line-seq
     (io/reader
      (io/resource filename))))

  ;; Define operations represent the changes in SIGNAL
  (def A {:during identity
          :after identity})
  (def B {:during identity
          :after :adder})
  ;; parse commands
  (defn parse-commands
    [lines]
    (mapcat
     (fn [line]
       (if (= "noop" line) [A]
           (let [adder (-> line (str/split #" ") last Integer/parseInt)]
             [A (assoc B :after #(+ adder %))])))
     lines))
  ;; test
  (assert (= 5 (count (parse-commands lines))))
  

  ;; The way X changes is by reducing over X
  
  (defn execute-commands
    [lines]
    (reduce
     (fn [acc cmd]
       (let [x-value (:X acc)
             x-during-cycle ((:during cmd) x-value)
             x-end-cycle ((:after cmd) x-during-cycle)]
         (-> acc
             (assoc :X x-end-cycle)
             (update :cycles conj x-during-cycle))))
     {:X 1
      :cycles []}
     (parse-commands lines)))
  ;; But we want the calculate the strengh signal at positions 19,59,99,139,179,219
  (def signal-cycles (range 19 220 40 ))
  ;;
  (defn day-10-part-1
    [lines]
    (->>  (select-keys (:cycles (execute-commands lines)) signal-cycles)
          (map (fn [[k v]] (* (inc k) v)))
          (reduce + 0)))
  ;;
  (day-10-part-1 lines) ;; 13860 correct
  ;; 13140
  ;; PART 2
  (def idx-cycles (range 0 220))
  (def crt-leds
    (sort-by :idx
             (partition 40 (map :led (crt lines)))))
  ;; Draw
  (def chars {:lit "#" :dark "."})
  ;;
  (defn draw
    [crt-leds]
    (print
     (->>
      (map #(-> (map chars  %)
                (str/join))  crt-leds)
      (str/join "\n"))))
  ;;print
  (draw crt-leds)
   
  

  ,)
