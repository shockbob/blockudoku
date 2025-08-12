(ns blockudoku.core
  (:gen-class)
  (:require [clojure.string :as str]))

(defn gen-row-line [len row] (mapv (fn [col] [row col]) (range len)))
(defn gen-col-line [len col] (mapv (fn [row] [row col]) (range len)))
(defn gen-line [len id] {:id id :coords (gen-row-line len 0)})


(def sing (gen-line 1 "dot"))
(def doub (gen-line 2 "line-2"))
(def trip (gen-line 3 "line-3"))
(def quad (gen-line 4 "line-4"))
(def quint (gen-line 5 "line-5"))
(def cross {:id "cross" :coords (vec (concat [[0 1] [2 1]] (gen-row-line 3 1)))})
(def weird {:id "weird" :coords [[0 0] [0 1] [1 1][1 2]]})
(def square {:id "square" :coords (vec (concat (gen-row-line 2 0) (gen-row-line 2 1)))})
(def ushape {:id "u" :coords (vec (concat [[0 0] [0 2]] (gen-row-line 3 1)))})
(def l2 {:id "l2" :coords (vec (concat [[1 0]] (gen-row-line 2 0)))})
(def l3 {:id "l3" :coords (vec (concat [[1 0] [2 0] ] (gen-row-line 3 0)))})
(def diag-2 {:id "diag-2" :coords [[0 0][1 1]]})
(def diag-3 {:id "diag-3" :coords [[0 0][1 1][2 2]]})

(defn r90 [[row col]] [col (- row)])
(defn r180 [[row col]] [(- row) (- col)])
(defn r270 [[row col]] [(- col) row])
(defn rot90 [piece] {:id (str (piece :id) "-r90") :coords (mapv r90 (piece :coords))}) 
(defn rot180 [piece] {:id (str (piece :id) "-r180") :coords (mapv r180 (piece :coords))}) 
(defn rot270 [piece] {:id (str (piece :id) "-r270") :coords (mapv r270 (piece :coords))}) 
(def lines [sing doub (rot90 doub) trip (rot90 trip) quad (rot90 quad) quint (rot90 quint) diag-2 (rot90 diag-2) diag-3 (rot90 diag-3)])
(defn rotates [piece]
    [piece (rot90 piece) (rot180 piece) (rot270 piece)])
(def rotateds (mapcat rotates [ushape l2 l3 weird])) 
(def pieces (conj lines cross))
(def pieces (conj pieces square))
(def pieces (concat pieces rotateds))

(def seed (for [row (range 3) col (range 3)] 
            [row col]))
(def grids (for [row (range 0 9 3) col (range 0 9 3)] 
             (mapv (fn [[gr gc]] [(+ gr row) (+ gc col)]) seed)))


(def row-lines  (mapv (fn [col] (gen-row-line 9 col)) (range 9)))
(def col-lines  (mapv (fn [row] (gen-col-line 9 row)) (range 9)))
(def all-groups (concat row-lines col-lines grids))

(def EMPTY -1)
(def FULL 1)

(defn minrc [coords] 
      (reduce (fn [[min-row min-col] [row col]] [(min row min-row) (min col min-col)]) 
              (first coords) 
              (rest coords)))

(defn norm-coords [coords]
      (let [[min-row min-col] (minrc coords)
            normalize [(- min-row) (- min-col)]] 
           (map (partial map + normalize) coords)))

(defn set-grid-coords [grid coords value]
    (reduce (fn [grid coord] (assoc-in grid coord value)) grid coords))
   

(defn add-piece [piece grid coord value]
   (let [coords (map (partial map + coord) (piece :coords))]
         (set-grid-coords grid coords value))) 

(defn all-match [coords grid value]
   (every? #{value}  (map (partial get-in grid) coords)))

(defn full [coords grid]
   (all-match coords grid FULL)) 
    
(defn fits [piece grid offset]
   (let [coords (map (partial map + offset) (piece :coords))]
        (all-match coords grid EMPTY)))

(defn filled-grid [value]
  (vec (repeat 9 (vec (repeat 9 value))))) 

(def empty-grid 
  (filled-grid EMPTY))

(def full-grid 
  (filled-grid FULL))

(defn get-full-groups [grid]
   (filter (fn [group] (all-match group grid FULL)) all-groups))


(defn fix-grid [grid]
   (let [full-groups (get-full-groups grid)]
         (reduce (fn [grid coords] (set-grid-coords grid coords EMPTY)) grid full-groups)))  

(defn find-starts [grid piece]
   (for [row (range (count grid)) col (range (count (grid 0))) 
         :when (fits piece grid [row col]) ]
          [row col]))

(def combos [[0 1 2][0 2 1][1 2 0][1 0 2][2 0 1][2 1 0]])
(defn add-n-fix [grid piece loc]
      (let [new-grid (add-piece piece grid loc FULL)
            new-grid (fix-grid new-grid)]
         new-grid))

(defn rand-move [grid piece starts]
   (rand-nth starts))

(defn count-empties [grid] (count (filter #{EMPTY} (flatten grid))))

(defn best-move [grid piece starts]
    (let [scores (map (fn [loc] (count-empties (add-n-fix grid piece loc))) starts)
          loc-map (zipmap starts scores) 
          besty (first (apply max-key val))]
      besty)) 

(defn all-moves [grid piece starts]
    (let [scores (map (fn [loc] (add-n-fix grid piece loc)) starts)
          grid-map (zipmap starts grids)] 
      grid-map)) 

(defn best-move [grid piece starts]
    (let [grid-map (all-moves grid piece starts)
          best-move (reduce (fn [[coord score] grid] 
                             (let [this-score (count-empties grid)]
                                 (if (> this-score score)
                                   [this-score coord]
                                   [coord score]))) [nil -1] grid
                                    
          besty (first (apply max-key val))]
      besty)) 

(defn all-moves [grid piece starts]
    (let [scores (map (fn [loc] (add-n-fix grid piece loc)) starts)
          grid-map (zipmap starts grids)] 
      grid-map)) 

(defn next-move [grid move-strategy]
   (let [pieces [(rand-nth pieces)(rand-nth pieces)(rand-nth pieces)]]
            (let [bestmove (move-strategy grid pieces)]
               (if (nil? bestmove) 
                 {:pieces pieces :grid grid :done true }
                 (let [[new-grid piece loc]  bestmove]
                     {:grid new-grid :piece piece :loc loc :done false})))))


(defn to-str [grid] (map (fn [row] (apply str (map {-1 \space 1 \# } row))) grid))

(defn from-str [str-grid] (map (fn [row] (map {\space -1 \# 1} row)) str-grid))

(defn print-grid [grid] (println (str/join "\n" (to-str grid) )))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
