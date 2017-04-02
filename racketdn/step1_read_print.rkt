#lang racket

(require readline/readline
         "reader.rkt"
         "printer.rkt"
         "types.rkt")

(define (READ s) (read-string s))
(define (EVAL s) s)
(define (PRINT s) (print-string s))

(define (rep s) ((compose PRINT EVAL READ) s))

(define (repl-loop)
  (let ([line (readline "user> ")])
    (when (not (eq? null line))
      (with-handlers
        ([string? (lambda (exc) (printf "Error: ~a~n" exc))]
         [blank-exn? (lambda (exc) null)])
        (printf "~a~n" (rep line)))
      (repl-loop))))

(repl-loop)
