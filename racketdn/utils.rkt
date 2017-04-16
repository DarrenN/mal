#lang racket

(provide list->pairs)

(define (list->pairs lst)
  (define (_partition lst ps)
    (if (empty? lst)
      (reverse ps)
      (_partition (drop lst 2) (cons (take lst 2) ps))))
  (if (odd? (length lst))
      (raise "list must be an even number of pairs")
      (_partition lst '())))