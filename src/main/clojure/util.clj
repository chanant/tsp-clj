(ns util)


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
  
(defn insert-if-different 
  "Inserts element in vector if vector does not contain the element already."
  [vct el]
  (if (not= nil (some #{el} vct)) vct (conj vct el)))
  
(defn half-of 
  "Returns the first half of collection."
  [coll]
  (take (/ (count coll) 2) coll))