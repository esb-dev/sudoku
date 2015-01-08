; Sudoku solver by more or less brute force

; Copyright © 2014 Burkhardt Renz, THM. All rights reserved.
; The use and distribution terms for this software are covered by the
; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php).
; By using this software in any fashion, you are agreeing to be bound by
; the terms of this license.

(ns sudoku
  (:require [clojure.set :refer [difference] :as set])
  (:require [clojure.java.io :refer (reader)]))

;; Constants
(def ^:const n1
  "Order of the grid"
  3)
(def ^:const n2
  "Size of the units"
  (* n1 n1))
(def ^:const n4
  "Number of cells"
  (* n2 n2))

(def digits
  "Digits for cells"
  (map #(char (+ (int \0) %)) (range 1 (inc n2))))

;; A puzzle is represented as a vector of digits

(defn rows
  "Matrix of the chars in the rows."
  [puzzle]
  (mapv vec (partition n2 puzzle)))

(defn cols
 "Matrix of the chars in the columns."
 [rows]
 (apply mapv vector rows))

#_(defn blocks'
  "Matrix of the chars in the blocks."
  [puzzle]
  (let [chunks-c3 (partition n1 puzzle)      ; chunks of 3 cells
        chunks-r3 (partition n2 chunks-c3)   ; chunks of 3 groups of 3 cells
        transp-r3 (map #(apply map list (partition n1 %)) chunks-r3) ; transpose of these
        transp-r3' (apply concat transp-r3)]
    (mapv #(vec (apply concat %)) transp-r3')))
;; about 3 times faster:

(defn blocks
  "Matrix of the chars in the blocks."
  [rows]
  (let [get-block (fn [rows x y]
                      (for [x (range x (+ x 3))
                            y (range y (+ y 3))]
                        (get-in rows [x y])))]
    (for [x (range 0 9 3)
          y (range 0 9 3)]
      (vec (get-block rows x y)))))

(defn idx-vec
  "Vector of the indexes in rows, cols, and blocks for a given idx in the puzzle."
  [idx]
  (let [row-idx (quot idx n2), col-idx (rem  idx n2)]
    [row-idx, col-idx, (+ (* 3 (quot row-idx n1)) (quot col-idx n1))]))

(defn candidate-set
  "Set of candidates at idx of the puzzle, precondition: idx is not already set!"
  [idx rows cols blocks]
  (let [[row-idx col-idx, blk-idx] (idx-vec idx)
        already-there (into #{} (concat (nth rows row-idx) (nth cols col-idx) (nth blocks blk-idx)))]
    (set/difference (set digits) already-there)))

(defn candidates
  "Sequence of indexes with candidates."
  [puzzle]
  (let [r (rows puzzle)
        c (cols r)
        b (blocks r)
        d (map-indexed #(vector %1 %2) puzzle)
        f (filter #(= (second %) \.) d)]
   (map #(vector (first %) (candidate-set (first %) r c b)) f)))

(defn singletons
  "Sequences of indexes with one single candidate"
  [candidates]
  (not-empty (filter #(= (count (second %)) 1) candidates)))

(defn solved?
  "A puzzle is solved, if the sequence of candidates is empty."
  [candidates]
  (empty? candidates))

(defn unsolvable?
  "A puzzle is unsolvable if there is an open cell without a candidate."
  [candidates]
  (not-every? #(not-empty (second %)) candidates))

(defn assoc-singletons
  "Assocs the singletons to the corresponding indexes"
  [puzzle singles]
  (let [kvs (flatten (map #(vector (first %) (first (second %))) singles))]
    (apply assoc puzzle kvs)) )

(defn solve
  "More or less naïve algorithm to solve the puzzle"
  [puzzle]
  #_(println (apply str puzzle)) ;; observe the recursion
  (let [cand (candidates puzzle)]
    (cond
      (solved? cand)
        puzzle
      (unsolvable? cand)
        nil
      :else
      (if-let [singles (singletons cand)]  ;; assoc singletons
        (solve (assoc-singletons puzzle singles))
        ;else backtracking
        (let [[idx c-set] (first cand)]
          (first (drop-while nil? (map #(solve (assoc puzzle idx %)) c-set))))))))

;; ## Pretty-printing puzzles and solutions
(defn pretty-print
  "Prints puzzle of order 3 decoded as a vector of digits."
  [puzzle]
  (let [rule "+-------+-------+-------+\n"]
    (doseq [[row col ch] (map-indexed #(vector (inc (quot %1 9)) (inc (rem %1 9)) %2) puzzle)]
      (if (and (= 1 col) (= 1 (mod row 3))) (print rule))
      (cond (= 1 (mod col 3)) (print (str "| " ch ))
            (= 2 (mod col 3)) (print (str " " ch ))
            (= 0 (mod col 3)) (print (str " " ch " ")))
      (if (= 9 col) (print "|\n")) )
    (print rule)))

stop -- the following is the interactive part

;; ## Example for a puzzle

(def puzzle (vec ".24...38.6.72.91.481.7.3.96.48...97...........69...51.75.9.8.414.16.57.9.96...83."))

(pretty-print puzzle)

(pretty-print (solve puzzle))

(def ambiguous (apply vector "34..7659878934512656..89347413762859625891734897453612978534261256918473134627985"))

(pretty-print ambiguous)

(pretty-print (solve ambiguous))

;; ## Parser for files containing puzzles
;; The parsers looks for lines with 81 characters, the digits 1-9 and the character .
;; Other lines in the file are ignored

(defn parse
  "Parses file with filename and returns a list of puzzles."
  [filename]
  (with-open [rdr (reader filename)]
    (into () (map vec (filter #(re-matches #"^([1-9]|\.){81}$" %) (line-seq rdr))))))

;; ## Benchmarks
(defn bench
  [puzzles]
  (time
    (do
      (dorun (map solve puzzles))
      :done)))

;; easy50.txt
(def easy50 (parse "resources/sudoku/easy50.txt"))

easy50

(bench easy50)
;=> 1.1 secs

;; top95.txt
(def top95 (parse "resources/sudoku/top95.txt"))

top95

(bench top95)
;=> 7.1 secs

;; hardest.txt
(def hardest (parse "resources/sudoku/hardest.txt"))

hardest

(bench hardest)
;=> 0.8 secs
