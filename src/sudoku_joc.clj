; Sudoku solver from Chap 16.6 of
; The Joy of Clojure 2nd edition by Michael Fogus and Chris Houser

; Copyright © 2014 Fogus and Houser
; Distributed under the Eclipse Public License, the same as Clojure.

; The example is adapted to our representation of the puzzle

(ns sudoku-joc
  (:require [clojure.set :refer [difference] :as set])
  (:require [clojure.java.io :refer (reader)]))


(def n1
  "Order of the grid"
  3)
(def n2
  "Size of the units"
  (* n1 n1))
(def n4
  "Number of cells"
  (* n2 n2))

(defn rows [board sz]
  (partition sz board))

(defn row-for [board index sz]
  (nth (rows board sz) (/ index 9)))

(defn column-for [board index sz]
  (let [col (mod index sz)]
    (map #(nth % col)
         (rows board sz))))

(defn subgrid-for [board i]
  (let [rows (rows board 9)
        gc (/ (mod i 9) 3)
        gr (/ (/ i 9) 3)
        grp-col (column-for (mapcat #(partition 3 %) rows) gc 3)
        grp (take 3 (drop (* 3 (int gr)) grp-col))]
    (flatten grp)))

(defn numbers-present-for [board i]
  (set
   (concat (row-for board i 9)
           (column-for board i 9)
           (subgrid-for board i))))

(defn possible-placements [board index]
  (set/difference #{\1 \2 \3 \4 \5 \6 \7 \8 \9}
                  (numbers-present-for board index)))

(defn index [coll]
  (cond
   (map? coll) (seq coll)
   (set? coll) (map vector coll coll)
   :else (map vector (iterate inc 0) coll)))

(defn pos [pred coll]
  (for [[i v] (index coll) :when (pred v)] i))

(defn solve
  [board]
  (println board)
  (if-let [[i & _] (and (some '#{\.} board)
                        (pos '#{\.} board))]
   (flatten (map #(solve (assoc board i %)) (possible-placements board i)))
   board))

; Remark:
; The algorithm calculates all possible solutions and flattens them.
; If the puzzle is ambiguous, the result is not one of the solutions,
; but the concatenation of all solutions.
; See the example 'ambiguous' below.

; Second remark:
; The algorithm does not stop if a solution is found. As a consequence
; it is very, very slow

(def puzzle (vec ".24...38.6.72.91.481.7.3.96.48...97...........69...51.75.9.8.414.16.57.9.96...83."))
(solve puzzle)

(def ambiguous (vec "34..7659878934512656..89347413762859625891734897453612978534261256918473134627985"))
(count (solve ambiguous))

; --- from lwb
;; ## Pretty-printing puzzles and solutions

(defn pretty-print
  [puzzle]
  (let [rule "+-------+-------+-------+\n"]
    (doseq [[row col ch] (map-indexed #(vector (inc (quot %1 n2)) (inc (rem %1 n2)) %2) puzzle)]
      (if (and (= 1 col) (= 1 (mod row n1))) (print rule))
      (cond (= 1 (mod col n1)) (print (str "| " ch ))
            (= 2 (mod col n1)) (print (str " " ch ))
            (= 0 (mod col n1)) (print (str " " ch " "))
            )
      (if (= 9 col) (print "|\n"))
      )
    (print rule)))

;; ## Example for a puzzle

(pretty-print puzzle)

(pretty-print (solve puzzle))

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

(bench (take 4 easy50))
;=> 21 secs

;; top95.txt
(def top95 (parse "resources/sudoku/top95.txt"))

top95

(bench (take 1 top95))
;=> stopped after 2 min or so

;; hardest.txt
(def hardest (parse "resources/sudoku/hardest.txt"))

hardest

(bench hardest)


