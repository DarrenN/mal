#lang racket

(require readline/readline
         "reader.rkt"
         "printer.rkt")

(define (READ s) (read-string s))
(define (EVAL s) s)
(define (PRINT s) (print-string s))

(define (rep s) ((compose PRINT EVAL READ) s))

(define (repl-loop)
  (let ([line (readline "user> ")])
    (add-history line)
    (printf "~a~n" (rep line)))
  (repl-loop))

(repl-loop)
