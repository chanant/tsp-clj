(ns web
  (:use hiccup.core)
  (:use hiccup.page-helpers)
  (:use hiccup.form-helpers)
  (:use compojure.core, ring.adapter.jetty)
  (:require [compojure.route :as route])
  (:use matrix)
  (:use tsp))

(defn layout 
  [title & body] 
  (html
    (doctype :xhtml-strict)
    [:html
      [:head
        (javascript-tag 
          "function visible(id) {
            if (document.getElementById(id).style.display == 'none')
              document.getElementById(id).style.display = 'block'
            else
              document.getElementById(id).style.display = 'none'
           }")
        [:title title]
      ]
      [:body
        [:h1 title]
        body]]))

(def start-page
  (layout "Genetic Algorithm for TSP written in Clojure" 
    (html 
      [:h2 "Intro"]
      [:p 
        [:b "Traveling Salesman Problem: "] "Given a list of cities and their pairwise distances, 
        the task is to find a shortest possible tour that visits each city exactly once and 
        returns to starting city."
        [:br]
          "Cities distances are defined in a random generated symmetric matrix, which will be shown 
          on the results page. Solution is represented as a sequence of numbers, where each number 
          represents particular city."
  	  ]
  	  
  	  [:p "Enter the number of cities and click \"Solve\" to start the algorithm."]
  	  (form-to [:POST "/solution"] 
        (label :x "Number of cities: ") (text-field :x)
        (html [:br]) 
        (submit-button "Solve")))))
        
(defn solution-page [result m elapsed-time]
  (layout (str "Solution for TSP problem with " (count m) " cities: ")
    (html
      [:p "Solution found in " elapsed-time " msecs."]
      [:p "Generation: " (:generation result)]
      [:p "Optimal distance: " (:distance result)]
      [:p "Optimal path: " (interpose " " (:path result))]
      [:p]
      [:a {:href "javascript:visible('matrix');"} "Show/hide distances matrix"]
      [:div {:id "matrix" :style "display: none;"}
        [:p "Distances matrix: "]
        (html-matrix m)
      ])))

(defn validate-param [param]
  (try
    (> (Integer/parseInt param) 0)
    (catch NumberFormatException _ false)))

(defn handle-start-page [x]
  (if (not (validate-param x)) (layout "Error" (html [:p "Illegal value for number of cities: " x]))
    (let 
      [start-time (. System (nanoTime))
       m (create-matrix (Integer/parseInt x))
  	   rs (solve-tsp m)
  	   elapsed-time (/ (double (- (. System (nanoTime)) start-time)) 1000000.0)] 
  	   (solution-page rs m elapsed-time))))

(defroutes main-routes
  (GET "/" [] start-page)
  (POST "/solution" [x] (handle-start-page x))
  (route/not-found (html "<h1>Requested resource not found</h1>")))

(defn start-app []
  (defonce server 
    (run-jetty main-routes {:port 8080 :join? false})))
