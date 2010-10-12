(ns tsp
  (:use matrix)
  (:use util))


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
      (recur (+ sum (get-value mtx (first p) (second p))) (rest p))
      (+ sum (get-value mtx (last salesman-path) (first salesman-path))))))
      
(defn sort-by-distance 
  "Sorts paths in the population by minimal distance."
  [mtx population]
  (sort-by (partial calculate-distance mtx) population))
  
(defn mutate 
  "Swaps two random elements in the chromosome."
  [chromosome]
  (let [chrom-size (count chromosome)]
    (swap chromosome (rand-int chrom-size) (rand-int chrom-size))))
	
(defn crossover 
  "Returns new offspring only if its distance is lower than first parent's distance.
  Otherwise it returns first parent (because it always has lower distance than second parent)."
  [parent1 parent2 mtx]
  (let [idx (/ (count parent1) 2)
        p1 (subvec parent1 0 idx)
        child (reduce insert-if-different p1 parent2)]
    (if (< (calculate-distance mtx child) (calculate-distance mtx parent1)) child parent1)))
    
(defn next-generation 
  "Generates new generation using crossover."
  [population m]
  (loop [res (lazy-seq) p population]
    (if (> (count p) 1)
      (recur (conj res (crossover (first p) (second p) m)) (rest p))
      (conj res (crossover (first population) (last population) m)))))
			
(defn next-mutated-gen 
  "Generates new generation using crossover and mutation."
  [population m]
  (let [p (next-generation population m)]
    (interleave (map mutate p) p)))

(defn- solve 
  "Used by solve-tsp function to solve the TSP problem."
  [mtx population generation no-prosperity-for best-path]
  (let [sorted-population (sort-by-distance mtx population)
        current-best-path (first sorted-population)
        current-best-path-val (calculate-distance mtx current-best-path)
        best-path-val (calculate-distance mtx best-path)]
    (if (or (= generation 1000) (= no-prosperity-for 20)) 
      (if (< current-best-path-val best-path-val) 
        {:cost current-best-path-val, :path current-best-path, :generation generation} 
        {:cost best-path-val, :path best-path, :generation generation})
      (recur mtx (next-mutated-gen (half-of sorted-population) mtx) (inc generation) 
        (if (< current-best-path-val best-path-val) 0 (inc no-prosperity-for)) 
        (if (< current-best-path-val best-path-val) current-best-path best-path)))))

(defn solve-tsp 
  "Solves the TSP problem for given matrix.
  The result is a map that contains keys :cost, :generation and :path."
  [mtx]
  (let [initial-population (generate-initial-population 500 (count mtx))]
    (solve mtx initial-population 0 0 (create-single-chromosome (count mtx)))))

