(ns web
  (:use compojure)
  (:use matrix)
  (:use tsp)) 

(defn index-html 
  [title & body] 
  (html 
    (doctype :html4) 
      [:html 
        [:head 
          [:title title]
        ]
        [:body
          [:div [:h1 title]]
          body]])) 


(def index-form 
  (index-html "Genetic Algorithm for TSP written in Clojure" 
  	(html [:h2 "Intro:"]
  	      [:p [:b "Traveling Salesman Problem: "] "Given a list of cities and their pairwise distances, the task is
  	       to find a shortest possible tour that visits each city exactly once (Wikipedia)."
  	      [:br]
  	      "Cities distances are defined in a random generated symmetric matrix, which will be shown
  	      on the results page. Solution is represented as a sequence of numbers, where each number
  	      represents particular city."
  	      ]
  	       
  	      [:p "Enter the number of cities and click \"Solve\" to start the algorithm."]
  	)
    (form-to [:post "/*"] 
      (label :x "Number of cities: ") (text-field {:size 3} :x)
      (html [:br]) 
      (submit-button "Solve"))))

(defn format-result [result m elapsed-time]
  (html
    [:html
      (doctype :html4)
      [:head
        [:title "Solution"]
      ]
      [:body
        [:h2 "Solution for TSP problem with " (count m) " cities: "]
        [:p "Solution found in " elapsed-time " msecs."]
        [:p "Generation: " (:generation result)]
        [:p "Optimal distance: " (:distance result)]
        [:p "Optimal path: " (interpose " " (:path result))]
        [:p]
        [:p "Cost matrix: "]
        (html-matrix m)
      ]
    ]))
    
(defn handle [x] 
  (let [start-time (. System (nanoTime))
    m (create-matrix (Integer/parseInt x))
  	rs(solve-tsp m)
  	elapsed-time (/ (double (- (. System (nanoTime)) start-time)) 1000000.0)] 
  	(format-result rs m elapsed-time)))
    
(defroutes webservice
  (GET "/*" index-form) 
  (POST "/*" (handle (params :x)))) 

(defn start-app []
  (run-server {:port 8080} "/*" (servlet webservice)))

