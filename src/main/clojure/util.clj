(ns util)

(defn rand-pos-int 
	"Returns random positive integer lower than rng. 
	If rng is not specified result is lower than 100."
	([] (rand-pos-int 100))
	([rng] 
	(let [i (rand-int rng)]
		(if (> i 0) i
				(rand-pos-int rng)))))