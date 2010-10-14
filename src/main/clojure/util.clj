(ns util
  (:use clojure.set))


(defn rand-pos-int 
  "Returns random positive integer lower than rng. 
  If rng is not specified result is lower than 100."
  ([] (rand-pos-int 100))
  ([rng] 
  (let [i (rand-int rng)]
    (if (> i 0) i
      (rand-pos-int rng)))))
      
(defn swap 
  "Swaps elements in vector at given positions."
  [vct i j]
  (assoc (assoc vct i (vct j)) j (vct i)))
  
(defn el-in-vct?
  "Checks if elements is already in the vector."
  [el vct]
  (not= nil (some #{el} vct)))
  
(defn insert-if-different 
  "Inserts element in vector if vector does not contain the element already."
  [vct el]
  (if (not= nil (some #{el} vct)) vct (conj vct el)))
  
(defn half-of 
  "Returns the first half of collection."
  [coll]
  (take (/ (count coll) 2) coll))
  
(defn indexed
  "Returns index - element pairs."
  [coll]
  (map vector (iterate inc 0) coll))
  
(defn index-filter
  "Returns indices of elements for which the predicate returns true."
  [pred coll]
  (when pred
    (for [[idx el] (indexed coll) :when (pred el)] idx)))
    
(defn index-of
  "Returns index of element in collection or nil if not found."
  [pred coll]
  (first (index-filter pred coll)))
  
(defn- fill-nil
  "Adds n nils to the end of vector if vector size is lower than size."
  [vct n]
  (if (< (count vct) n)
    (vec (concat vct (repeat n nil)))
    vct))
  
(defn by-pairs 
  "Returns collection of paired elements from c1 and c2.
  If collection are not of the same size, smaller one is filled with nils."
  [c1 c2]
	(let [c1-size (count c1)
	      c2-size (count c2)
	      c1 (fill-nil c1 (max c1-size c2-size))
	      c2 (fill-nil c2 (max c1-size c2-size))]
    (map vector c1 c2)))
    
(defn fill-random
  "Adds elements from (range n) that are not in coll."
  [coll n]
  (if (< (count coll) n)
    (vec (concat coll (shuffle (difference (set (range n)) (set coll)))))
    coll))