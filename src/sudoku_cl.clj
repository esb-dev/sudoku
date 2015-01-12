; Sudoku solver using core.logic
; adapted from https://github.com/clojure/core.logic/wiki/Examples

; Copyright © 2014 Burkhardt Renz, THM. All rights reserved.
; The use and distribution terms for this software are covered by the
; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php).
; By using this software in any fashion, you are agreeing to be bound by
; the terms of this license.

(ns sudoku-cl
  (:refer-clojure :exclude [==])
  (:use clojure.core.logic)
  (:require [clojure.core.logic.fd :as fd])
  (:require [clojure.java.io :refer (reader)]))

(defn get-block
  "gets the block in the vector of rows beginning at index x and y."
  [rows x y]
  (for [x (range x (+ x 3))
        y (range y (+ y 3))]
    (get-in rows [x y])))

(defn init
  "Unifies variables with the given digits in the puzzle."
  [vars puzzle]
  (if (seq vars)
    (let [digit (first puzzle)]
      (all
        (if-not (zero? digit)
          (== (first vars) digit) s#)
        (init (next vars) (next puzzle))))
    s#))

(defn solve
  "Solve Sudoku puzzle, represented as a vector of integers in (range 1 10)"
  [puzzle]
  (let [vars (repeatedly 81 lvar)
        rows (mapv vec (partition 9 vars))
        cols (apply map vector rows)
        blocks (for [x (range 0 9 3)
                     y (range 0 9 3)]
                    (get-block rows x y))]
      (first (run 1 [q]
               (== q vars)
               (everyg #(fd/in % (fd/domain 1 2 3 4 5 6 7 8 9)) vars)
               (init vars puzzle)
               (everyg fd/distinct rows)
               (everyg fd/distinct cols)
               (everyg fd/distinct blocks)))))

(defn ctoi
  "Returns the int of given char, 0 if char is '.'."
  [char]
  (let [i0 (int \0)]
    (if (= \. char) 0 (- (int char) i0))))

(defn pretty-print
  "Prints puzzle of order 3 decoded as a vector of integers."
  [puzzle]
  (let [rule "+-------+-------+-------+\n"]
    (doseq [[row col dch] (map-indexed #(vector (inc (quot %1 9)) (inc (rem %1 9)) %2) puzzle)]
      (let [ch (if (zero? dch) \. dch)]
        (if (and (= 1 col) (= 1 (mod row 3))) (print rule))
        (cond (= 1 (mod col 3)) (print (str "| " ch ))
              (= 2 (mod col 3)) (print (str " " ch ))
              (= 0 (mod col 3)) (print (str " " ch " ")))
        (if (= 9 col) (print "|\n"))))
    (print rule)))

stop -- the following is the interactive part

(def puzzle (map ctoi ".24...38.6.72.91.481.7.3.96.48...97...........69...51.75.9.8.414.16.57.9.96...83."))

(pretty-print puzzle)

(pretty-print (solve puzzle))

(def hard (map ctoi "1.....7.9.4...72..8.........7..1..6.3.......5.6..4..2.........8..53...7.7.2....46"))

(pretty-print hard)

(pretty-print (solve hard))

;; ## Parser for files containing puzzles
;; The parsers looks for lines with 81 characters, the digits 1-9 and the character .
;; Other lines in the file are ignored

(defn parse
  "Parses file with filename and returns a list of puzzles."
  [filename]
  (with-open [rdr (reader filename)]
    (into () (map #(map ctoi %) (filter #(re-matches #"^([1-9]|\.){81}$" %) (line-seq rdr))))))

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

(dotimes [_ 10]
  (bench easy50))
;=>  1108 msecs

;; top95.txt
(def top95 (parse "resources/sudoku/top95.txt"))

top95

(dotimes [_ 10]
  (bench top95))
;=> 505145 msecs

;; hardest.txt
(def hardest (parse "resources/sudoku/hardest.txt"))

hardest

(dotimes [_ 10]
  (bench hardest))
;=>  2839.4 msecs

; average for 1 puzzle: 3263 msecs
