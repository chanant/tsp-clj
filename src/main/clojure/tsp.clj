(ns tsp
  (:use matrix))


(defn create-single-chromosome [size]
  (vec (shuffle (range size))))
	
(defn generate-initial-population [pop-size chrom-size]
  (repeatedly pop-size (partial create-single-chromosome chrom-size)))
  
(defn calculate-distance [mtx salesman-path]
  (loop [sum 0 p salesman-path]
    (if (> (count p) 1) 
      (recur (+ sum (get-value mtx (first p) (second p))) (rest p))
      (+ sum (get-value mtx (last salesman-path) (first salesman-path))))))

