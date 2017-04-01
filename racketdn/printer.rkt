#lang racket

(require racket/match
         "types.rkt")

(provide print-string)

(define (print-string val)
  (match val
    [(Number _) (number->string (Number-value val))]
    [(Symbol _) (symbol->string (Symbol-value val))]
    [(List _) (string-join (map print-string (List-value val)) " " #:before-first "(" #:after-last ")")]))