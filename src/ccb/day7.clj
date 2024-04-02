(ns ccb.day7
  (:require [clojure.java.io :as io]
            [clojure.set :as set]
            [clojure.zip :as zip]
            [clojure.string :as str]))

(defn is-dir? [ls-item]
  (nil? (:size ls-item)))

(defn get-children [node]
  (if (not (map? node)) []
      (map identity node)))

(defn is-file? [ls-item]
  (:size ls-item))



(defn is-command? [line]
  (= \$(first line)))

(defn parse-comand [line]
  (str/split line #" "))

(defn parse-1 [lines]
  (reduce
   (fn [acc next-line]
     (cond
       (is-command? next-line)
       (conj acc 
             (let [cmd (first
                        (rest (parse-comand next-line)))
                   args (rest (rest (parse-comand next-line)) )]
               {:cmd cmd
                :args args
                :output []}))
       :else
       (update-in acc [(dec (count acc)) :output] conj next-line)))
   []
   lines))

(defn eval-cd
  [acc next-cmd]
  (update acc :current-path
          (fn [c-path]
            (let [cd-dir (first (:args next-cmd))]
              (cond
                (= ".." cd-dir)
                (vec (drop-last c-path))
                (= "/" cd-dir)
                []
                :else
                (conj c-path cd-dir))))))

(defn make-node [nodes]
  (reduce
   (fn [acc [name value]]
     (assoc acc name value))
   {}
   nodes))


(defn eval-ls
  [acc next-cmd]
  (let [ls-content (->> (:output next-cmd)
                        (map (fn [output-item]
                               (let [[child-type-or-size child-name] (str/split output-item #" ")]
                                 (if (= "dir" child-type-or-size)
                                   [child-name {}]
                                   [child-name {:size (Integer/parseInt child-type-or-size)}]))))
                        make-node)]
    (assoc-in acc (concat [:tree] (:current-path acc)) ls-content)))
(def dd0 (atom []))
(defn build-fs
  [lines]
  (reduce
   (fn [acc next-cmd]
     (cond
       ;; cd
       (= "cd" (:cmd next-cmd))
       (eval-cd acc next-cmd)
       ;; ls
       (= "ls" (:cmd next-cmd))
       (eval-ls acc next-cmd)
       :else acc))
   {:tree {}
    :current-path []}
   (parse-1 lines)))

(defn update-content
  [results parent-path node]
  (loop [total-results results
         ancestors parent-path]
    (cond
      (empty? ancestors) (update total-results [] (fnil + 0) (:size node))
      :else
      (recur (update total-results ancestors (fnil + 0) (:size node))
             (drop-last ancestors)))))
;; ---
(update-content {[] 3000 ["a"] 3000} ["a" "b" "c"] {:size 29116})
;; {[] 32116, ["a"] 32116, ["a" "b" "c"] 29116, ("a" "b") 29116}
;; {["a"] 32116, ["a" "b" "c"] 29116, ("a" "b") 29116}
;; {("a") 32116, ["a" "b" "c"] 29116, ("a" "b") 29116}
;; {["a" "b" "c"] 29116, ("a" "b") 29116, ("a") 29116}




(defn calculate-size
  [fs]
  (loop [
         results {[] 0}
         pending-paths (mapv vector (keys fs))]
    (let [a-path (first pending-paths)
          parent-path (-> a-path drop-last vec)]
      (cond
        ;; end?
        (empty? pending-paths)
        results
        ;; file 
        (is-file? (get-in fs a-path))
        (recur
         ;; For every file, we update across all ancestors (including root)
         (update-content results parent-path (get-in fs a-path))
         (rest pending-paths))
        ;; dir?
        (is-dir? (get-in fs a-path))
        (recur results
               (concat
                (map (partial conj a-path) (keys (get-in fs a-path)))
                (rest pending-paths)))))))


(defn day7-part-1
    [fs-size]
    (reduce
     (fn [acc [path content]]
       (+ acc content))
     0
     (filter
      (fn [[path content]]
        (and
         (<= content 100000)
         (not (empty? path))))
      fs-size)))

(comment
  (def filename "day7-ex.txt")
  (def filename "day7.input.txt")
  (def filename "day7-debug.txt")
  
  (def lines 
    (line-seq
     (io/reader
      (io/resource filename))))

  (def fs-0 (build-fs lines))
  (def fs (:tree fs-0))
  (def top-roots (set (keys fs)))
  ;; How to calculate the size?
  (def fs-size
    (calculate-size fs))
  

  ;; Identify those with 'at most 100000'
  
  
  ;; test...
  (day7-part-1 fs-size);; 1297683 !! (correct one)
  6694691

  ;; 95437  as expected.
  ;; pART 2
  ;; The total ocupied space is 
  (get fs-size []) ;; 44804833
  (def capacity 70000000)
  (def min-free-space 30000000)
  ;; But the total available 70000000
  ;; Unused space >= 30000000
  ;; but now is
  (- 70000000 44804833) ;; 25195167
  ;; We need to find a directory ('a!!!') only one.
  ;; to Free up enough space...
  ;; That 'directory'  must be AT LEAST
  (- 30000000 25195167) ;; 4804833

  ;; From the example data
  fs-size
  {[] 48381165, ["a" "e"] 584, ("a") 94853, ["d"] 24933642}
  (def unused-space (- capacity (get fs-size [])))  ;; 21618835
  (def required-to-delete (- min-free-space unused-space))
  ;; The solution, find the smallest one to achieve the free space
  (->>
   (sort-by (fn [[k v]] v) fs-size)
   (filter (fn [[k v]] (>= v required-to-delete)))
   first
   last)
  ,)
