#lang racket
(require readline/readline
         readline/pread)

(define (READ s) s)
(define (EVAL s) s)
(define (PRINT s) s)

(define (rep s) ((compose PRINT EVAL READ) s))

(define (repl-loop)
  (let ([line (readline "user> ")])
    (add-history line)
    (printf "~a~n" (rep line)))
  (repl-loop))

(repl-loop)
