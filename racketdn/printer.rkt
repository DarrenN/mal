#lang racket

(require racket/match
         "types.rkt")

(provide print-string)

(define (print-string token [pretty #t])
  (cond
    [(number? token) (number->string token)]
    [(symbol? token) (handle-symbol token)]
    [(string? token)
     (if pretty
         (format "\"~a\""
                 (string-replace
                  (string-replace
                   (string-replace token "\\" "\\\\")
                   "\"" "\\\"")
                  "\n" "\\n"))
         token)]
    [(list? token)
     (string-join (map print-string token) " "
                  #:before-first LPAR #:after-last RPAR)]
    [(vector? token)
     (string-join (vector->list (vector-map print-string token)) " "
                  #:before-first LVEC #:after-last RVEC)]
    [(hash? token)
     (string-join (map print-string (flatten-hash token))
                  #:before-first LMAP #:after-last RMAP)]
    [(void? token) null]
    [else (~a token)]))

(define (handle-symbol sym)
  (let ([str (symbol->string sym)])
    (if (string-prefix? str KWSYM)
        (string-replace str KWSYM KW)
        str)))

;(define t (hasheq 'foo 1 'bar 2 'baz (hasheq 'quux 3 'quil 4)))
(define (flatten-hash h)
  (flatten (for/list ([p (hash->list h)])
             (flatten p))))
