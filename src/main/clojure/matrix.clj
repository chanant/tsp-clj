(ns matrix
  (:use util))


(defn- generate-nil-row 
  "Generates a vector ref of given size with all its elements being nil."
  [size]
  (ref (vec (repeat size nil))))

(defn- generate-nil-matrix 
  "Generates square matrix of given dimension with all its elements being nil."
  [size]
  (vec (repeatedly size (partial generate-nil-row size))))
	
(defn- get-value-at 
  "Returns a value in the given matrix with given coordinates."
  [matrix i j]
  (get (deref (get matrix i)) j))

(defn- set-value-at 
  "Sets value at the given matrix at given coordinates."
  [matrix i j v]
  (dosync (alter (get matrix i) assoc j v))) 
	
(defn- generate-initial-matrix 
  "Generates random symmetric matrix of given dimension with 0s on its diagonal.
  Rows in matrix are represented as vector refs."
  [size]
  (let [matrix (generate-nil-matrix size)]
    (dotimes [i size]
      (dotimes [j size]
        (if (= nil (get-value-at matrix i j))
          (if (= 0 (- i j))
            (set-value-at matrix i j 0)
            (let [v (rand-pos-int 1000)]
              (do (set-value-at matrix i j v) (set-value-at matrix j i v)))))))
  matrix))
		
(defn create-matrix 
  "Generates random symmetric matrix of given dimension with 0s on its diagonal.
  Matrix is represented as a vector of vectors."
  [size]
  (let [m (generate-initial-matrix size)]
    (vec (for [row m] @row))))
		
(defn get-value 
  "Returns the value in the matrix at given position."
  [matrix i j]
  ((matrix i) j))

(defn print-matrix 
  "Prints a matrix that is represented as a vector of vectors."
  [mtx]
  (dotimes [i (count mtx)]
    (println (apply str (interpose "	" (mtx i))))))
	
	