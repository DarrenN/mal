#lang racket

(define (READ s) s)
(define (EVAL s) s)
(define (PRINT s) s)

(define (rep s) ((compose PRINT EVAL READ) s))

(define (repl)
  (printf "user> ")
  (println (rep (read-line)))
  (repl))
