(ns tsp
  (:use matrix)
  (:use util))


(defstruct result :distance :generation :path)

(defn create-single-chromosome 
  "Creates random vector of given size that represent single salesman path."
  [size]
  (vec (shuffle (range size))))
	
(defn generate-initial-population 
  "Generates population of given size that contains random salesman paths.
  Salesman path is a vector of ints beetwen 0 and chrom-size."
  [pop-size chrom-size]
  (repeatedly pop-size (partial create-single-chromosome chrom-size)))
  
(defn calculate-distance 
  "Calculates the distance for given path based on the values in matrix."
  [mtx salesman-path]
  (loop [sum 0 p salesman-path]
    (if (> (count p) 1) 
      (recur (+ sum (int (get-value mtx (first p) (second p)))) (rest p))
      (+ sum (int (get-value mtx (last salesman-path) (first salesman-path)))))))
      
(defn sort-by-distance 
  "Sorts paths in the population by minimal distance."
  [mtx population]
  (sort-by (partial calculate-distance mtx) population))
  
(defn mutate 
  "Swaps two random elements in the chromosome."
  [chromosome]
  (let [chrom-size (count chromosome)]
    (swap chromosome (rand-int chrom-size) (rand-int chrom-size))))

(def use-f (ref true))
(def use-s (ref true))

(defn- iter [res el]
 (do 
   (if (or (= nil (first el)) (el-in-vct? (first el) res))
     (dosync (ref-set use-f false)))
   (if (or (= nil (second el)) (el-in-vct? (second el) res) (= (first el) (second el)))
     (dosync (ref-set use-s false))))
 (if @use-f
   (if @use-s
     (vec (cons (first el) (conj res (second el))))
     (vec (cons (first el) res)))
   (if @use-s
     (conj res (second el))
     res)))

(defn crossover 
  "Returns new offspring by combining parents using 'Greedy crossover'."
  [parent1 parent2]
  (let [initial (parent1 (rand-int (count parent1)))
        idx-p1 (index-of #{initial} parent1)
        idx-p2 (index-of #{initial} parent2)
        to-front (reverse (subvec parent1 0 idx-p1))
        to-end (subvec parent2 (inc idx-p2))
        coll (by-pairs to-front to-end)]
        (dosync (ref-set use-f true))
        (dosync (ref-set use-s true))
        ;(println initial)
    (fill-random (reduce iter [initial] coll) (count parent1))))
    
(defn next-generation 
  "Generates new generation using crossover."
  [population]
  (letfn 
    [(iter [p]
      (lazy-seq
        (when (> (count p) 1)
          (cons (crossover (first p) (second p)) (iter (rest p))))))]
    (cons (crossover (first population) (last population)) (iter population))))
			
(defn next-mutated-gen 
  "Generates new generation using crossover and mutation."
  [population]
  (let [best (first population)
        p (next-generation (rest population))]
    (concat (map mutate p) (cons best p))))

(defn- solve 
  "Used by solve-tsp function to solve the TSP problem."
  [mtx population generation no-improvement-for best-path]
  (let [sorted-population (sort-by-distance mtx population)
        current-best-path (first sorted-population)
        current-best-path-val (calculate-distance mtx current-best-path)
        best-path-val (calculate-distance mtx best-path)]
    (if (or (== generation 500) (== no-improvement-for 30)) 
      (if (< current-best-path-val best-path-val) 
        (struct result current-best-path-val generation current-best-path)
        (struct result best-path-val generation best-path))
      (recur mtx (next-mutated-gen (half-of sorted-population)) (inc generation) 
        (if (< current-best-path-val best-path-val) 0 (inc no-improvement-for)) 
        (if (< current-best-path-val best-path-val) current-best-path best-path)))))

(defn solve-tsp 
  "Solves the TSP problem for given matrix.
  The result is a map that contains keys :distance, :generation and :path."
  [mtx]
  (let [initial-population (generate-initial-population 500 (count mtx))]
    (solve mtx initial-population 0 0 (create-single-chromosome (count mtx)))))

