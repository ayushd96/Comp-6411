(defn readcustfile[]
  (def cust-file (clojure.string/split-lines (slurp "cust.txt"))) 
  (def cust {})
  (doseq [line cust-file]
    (def cust-id (nth (clojure.string/split line #"\|") 0))
    (def cust-name (str (nth (clojure.string/split line #"\|") 1)))
    (def cust-add (str (nth (clojure.string/split line #"\|") 2)))
    (def cust-phone (str (nth (clojure.string/split line #"\|") 3)))
    (def cust-data (str cust-name "," cust-add "," cust-phone ))
    (def cust(merge-with into cust {(Integer/parseInt cust-id) cust-data}))
    )
  )
(readcustfile)

(defn readprodfile[]
  (def prod-file (clojure.string/split-lines (slurp "prod.txt")))
  (def prod{})
  (doseq [line prod-file]
    (def prod-id (nth (clojure.string/split line #"\|") 0))
    (def item-name (nth (clojure.string/split line #"\|") 1))
    (def item-cost (nth (clojure.string/split line #"\|") 2))
    (def prod-data (str  item-name "," item-cost ))
    (def prod (merge-with into prod {(Integer/parseInt prod-id) prod-data})))
  )
(readprodfile)

(defn readsalesfile[]
  (def sale-file(clojure.string/split-lines (slurp "sales.txt")))
  (def sales{})
  (doseq [line sale-file]
    (def sales-id (nth (clojure.string/split line #"\|") 0))
    (def cust-id (nth (clojure.string/split line #"\|") 1))
    (def prod-id (nth (clojure.string/split line #"\|") 2))
    (def item-count (nth (clojure.string/split line #"\|") 3))
    (def sales-data (str cust-id "," prod-id "," item-count))
    (def sales (merge-with into sales {(Integer/parseInt sales-id) sales-data})))
  )
(readsalesfile)

(defn loadcustdata[]
  (doseq [i (into (sorted-map) cust)]
    (def c-id (str (nth i 0)))
    (print c-id)
    (def data (str (nth i 1)))
    (def c-name (str (nth (clojure.string/split data #",") 0)))
    (def c-add (str (nth (clojure.string/split data #",") 1)))
    (def c-phone (str (nth (clojure.string/split data #",") 2)))
    (println (str ": [""\""c-name"\"" " " "\"" c-add"\"" " " "\""c-phone"\"""]"))
    )
  )
;(loadcustdata)

(defn loadproddata[]
  (doseq [i (into (sorted-map) prod)]
    (def p-id (str (nth i 0)))
    (print p-id)
    (def data (str (nth i 1)))
    (def p-name (str (nth (clojure.string/split data #",") 0)))
    (def p-cost (str (nth (clojure.string/split data #",") 1)))
    (println (str ": [""\""p-name"\"" " " "\""p-cost"\"""]"))
    )
  )
;(loadproddata)

(defn loadsalesdata[]
  (doseq [i (into (sorted-map) sales)]
    (def sales-id (str (nth i 0)))
    (print sales-id)
    ;(println (str (nth i 1)))
    (def cust-id (str (nth (nth i 1) 0)))
    (def int-cust-id (Integer/parseInt cust-id))
    (def cust-data (str(get cust int-cust-id)))
    (def cust-name (nth (clojure.string/split cust-data #",")0))
    ;(println cust-name)
    (def prod-id (str (nth (nth i 1) 2)))
    (def int-prod-id (Integer/parseInt prod-id))
    ;(println int-prod-id)
    (def prod-data (str (get prod int-prod-id)))
    (def prod-name (nth (clojure.string/split prod-data #",") 0))
    ;(println prod-name)
    (def item-count (str(nth (nth i 1) 4)))
    ;(println item-count)
    (println (str ": [""\""cust-name"\"" " " "\""prod-name"\"" " " "\""item-count"\"""]"))
    ;(println final-string)
    )
  )
;(loadsalesdata)

(defn loadcustomerpurchase[]
  (println "Enter Name of Customer:")
  (def input-cust-name(read-line))
  ;(print input-cust-name)
  (def prod-cost [])
  (def does-exist "false")
  (doseq [i (into (sorted-map) cust)]
    ;(print (str (nth i 0)":"))
    ;(println (str (nth i 1)))
    (def data (str (nth i 1)))
    (def c-name (str (nth (clojure.string/split data #",") 0)))
    ;(println c-name)
    (cond (= c-name input-cust-name)
          (do
            (def does-exist "true")
            (def c-id(str(nth i 0)))
            (def i-c-id (Integer/parseInt c-id))
             ;(println i-c-id)
             (doseq [j (into(sorted-map) sales)]
               (def s-data(str (nth j 1)))
               (def s-id(str (nth (clojure.string/split s-data #",") 0)))
               (def i-s-id (Integer/parseInt s-id))
               (cond (= i-c-id i-s-id)
                 (do
                   (def prod-id (str (nth (clojure.string/split s-data #",")1)))
                   (def i-prod-id (Integer/parseInt prod-id))
                   ;(println "Prod ID:" i-prod-id)
                   (def p-data (get prod i-prod-id))
                   ;(println p-data)
                   (def p-cost (Double/parseDouble (nth (clojure.string/split p-data #",")1)))
                   ;(println "Prod Cost :" p-cost)
                   (def item-count (str (nth (clojure.string/split s-data #",")2)))
                   (def i-item-count (Double/parseDouble item-count))
                   ;(println "Item Count: "i-item-count)
                   (def prod-cost (conj prod-cost (* i-item-count p-cost)))
                   )))
             (cond (empty? prod-cost)
                   (do (println (str c-name": $0.00")))
             :else(do
                       (print (str c-name": $"))
                       (println (format "%.2f" (reduce + prod-cost))))))
      )
    )
  (cond (= does-exist "false")
        (do
             (println "Customer Not Found"))
        )
  )
;(loadcustomerpurchase)

(defn totalcountofproduct[]
  (println "Enter Name of Product:")
  (def prod-name (read-line))
  ;(println prod-name)
  (def prod-exists "false")
  (def total [])
  (doseq [i (into (sorted-map) prod)]
    (def prod-id (str (nth i 0)))
    (def i-prod-id (Integer/parseInt prod-id))
    ;(println i-prod-id)
    (def p-data (str (get prod i-prod-id)))
    (def p-name (str (nth (clojure.string/split p-data #",")0)))
    ;(println p-name)
    (cond (= prod-name p-name)
      (do
        (def prod-exists "true")
        (doseq [j (into (sorted-map) sales)]
          (def s-data (str (nth j 1)))
          ;(println s-data)
          (def s-p-id (str (nth (clojure.string/split s-data #",")1)))
          (def int-s-p-id (Integer/parseInt s-p-id))
          ;(println "s-prod-id" s-p-id)
          (cond (= i-prod-id int-s-p-id)
                (do
                  (def item-count (str (nth (clojure.string/split s-data #",")2)))
                  (def int-item-count (Integer/parseInt item-count))
                  ;(println int-item-count)
                  (def total (conj total (+ int-item-count)))
                    ))    
          )
        (print (str (clojure.string/capitalize prod-name)":"))
        (print (str(reduce + total)))
        ;(println (reduce + total))
        )
      :else())
    )
  (cond (= prod-exists "false")
        (do
          (println "Product Not Found"))
  ))

;(totalcountofproduct "milk")
(defn goodbye[]
  (println "Good Bye")
  (System/exit 0))

(defn printMenu []
	(println " ")
	(println "***  Sales Menu  ***")
	(println "----------------------------")
	(println "1. Display Customer Table")
	(println "2. Display Product Table")
	(println "3. Display Sales Table")
	(println "4. Total Sales for Customer")
	(println "5. Total Count for Product")
	(println "6. Exit")
	(println " ")
	(println "Enter Choice:"))

(defn main[]
  (printMenu)
  
  (let [choice (str(read-line))]
    (cond 
      (= "1" choice) (do 
                       (loadcustdata) 
                       (recur)) 
      (= "2" choice) (do 
                       (loadproddata) 
                       (recur))
      (= "3" choice) (do 
                       (loadsalesdata)
                       (recur))
      (= "4" choice) (do 
                       (loadcustomerpurchase) 
                       (recur))
      (= "5" choice) (do 
                       (totalcountofproduct) 
                       (recur))
      (= "6" choice) 
      (goodbye)
      :else (do 
              (println "Enter Correct Choice.")
              (recur))
      )))
(main)

;---- References ----- 
; https://clojuredocs.org/