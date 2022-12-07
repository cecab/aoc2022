(ns ccb.day5
  (:require [clojure.java.io :as io]
            [clojure.set :as set]
            [clojure.string :as str]
            ))

(defn parse-input
  [lines]
  (->> (filter #(re-find #"^\[.*|^ .*" %) lines)
       (mapcat (fn [l]
                 (map-indexed
                  (fn [idx i]
                    {:pos (inc idx)
                     :content (str/join i)})
                  (partition-all 4 l))))
       (filter
        (fn [st]
          (re-find #"^\[[a-zA-Z]\].*" (:content st))))
       (map (fn [st]
              (update st :content
                      (fn [str-cct]
                        (last (re-find #"^\[([a-zA-Z])\].*" str-cct))))))
       (group-by :pos)
       (reduce (fn [acc [k v]]
                 (assoc acc k (mapv :content v)))
               {})))
(defn parse-commands [lines]
  (map
   (fn [str-cmd]
     (let [[qty from to]
           (rest (re-find #"^move ([0-9]+) from ([0-9]+) to ([0-9]+)" str-cmd))]
       (try
         {:qty (Integer/parseInt qty) :from (Integer/parseInt  from) :to (Integer/parseInt  to)}
         (catch Exception e
           (throw 
            (ex-info "ups.."
                     {:line str-cmd
                      :error (.getMessage e)}))))))
   (filter #(re-find #"^move .*" %) lines)))


(defn expand-commands [cmds]
  (mapcat
   (fn [cmd]
     (take (:qty cmd)
           (repeat (assoc (select-keys cmd [:from :to])
                          :qty 1))))
   cmds))


(defn exe-cmd
  [db cmd]
  (let [{:keys [from to]} cmd
        item-to-move (get-in db [from 0])]
    (-> db
        (update from (comp vec rest))
        (update to #(vec (conj (apply list %) item-to-move))))))

(defn day-5-part-1
  [filename]
  (let [lines 
        (parse-lines filename)
        all-cmds (expand-commands (parse-commands lines))
        final-db 
        (->> all-cmds
             (reduce exe-cmd (parse-input lines)))
        answer (str/join
                (map #(first (get final-db %))
                     (sort (keys final-db))))]
    {:all-cmds all-cmds
     :answer answer
     :final-db final-db}
    answer))

(comment
  ;;
  (def filename "day5-ex.txt")
  
  
  ;; Parse commands.
  (def db (parse-input lines))
  (def cmds (parse-commands lines))
  (def cmd (first cmds))

  
  
  ;; But commands with qty > 1 needs to be expanded...
  

  (defn parse-lines [filename]
    (line-seq
     (io/reader
      (io/resource filename))))


  
  ;; check
  (day-5-part-1 "day5-ex.txt")    ;; "CMZ"
  ;; my input
  (day-5-part-1 "day5.input.txt") ;; "PTWLTDSJV"
  "ZPTHRJZSC"
  ;;"ZPTHRJZSC"


  ;; Some tests..
  (parse-input (parse-lines "day5-1.test") )
  {2 ["D" "C" "M"], 1 ["Z" "N"], 3 ["a" "P"], 4 ["Q"]}
  {2 ["D" "C" "M"], 1 ["N" "Z"], 3 ["a" "P"], 4 ["Q"]}
  ;;
  (def db (parse-input (parse-lines "day5-2.test") ))
  (reduce exe-cmd db [{:from 9 :to 7} {:from 9 :to 7}])
  
  
  ;; Testing commands..
  (expand-commands 
   (parse-commands (parse-lines "day5-2.test")))
  ;; Manual check.
  (day-5-part-1 "day5-2.test")
  "NJVZJJDST"
  ,)
