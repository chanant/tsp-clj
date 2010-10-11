(ns matrix
  (:use util))

(defn generate-nil-row [size]
  (ref (vec (repeat size nil))))

(defn generate-nil-matrix [size]
  (vec (repeatedly size (partial generate-nil-row size))))
	
(defn get-value-at [matrix i j]
  (get (deref (get matrix i)) j))

(defn set-value-at [matrix i j v]
  (dosync (alter (get matrix i) assoc j v))) 
	
(defn generate-initial-matrix [size]
  (let [matrix (generate-nil-matrix size)]
    (dotimes [i size]
      (dotimes [j size]
        (if (= nil (get-value-at matrix i j))
          (if (= 0 (- i j))
            (set-value-at matrix i j 0)
            (let [v (rand-pos-int 1000)]
              (do (set-value-at matrix i j v) (set-value-at matrix j i v)))))))
  matrix))
		
(defn matrix [size]
  (let [m (generate-initial-matrix size)]
    (vec (for [row m] @row))))
		
(defn get-value [matrix i j]
  ((matrix i) j))

(defn print-matrix [mtx]
  (dotimes [i (count mtx)]
    (println (apply str (interpose "	" (mtx i))))))
	
	