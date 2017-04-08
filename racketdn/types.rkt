#lang racket

(provide blank-exn? make-blank-exn TOKENS LPAR RPAR QUOTE QUASI UNQUO SPLICE
         KW KWSYM LVEC RVEC LMAP RMAP COMM DEREF)

(define TOKENS (pregexp "[\\s,]*(~@|[\\[\\]{}()'`~^@]|\"(?:\\\\.|[^\\\\\"])*\"|;.*|[^\\s\\[\\]{}('\"`,;)]*)"))
(define LPAR "(")
(define RPAR ")")
(define LVEC "[")
(define RVEC "]")
(define LMAP "{")
(define RMAP "}")
(define QUOTE "'")
(define QUASI "`")
(define UNQUO "~")
(define SPLICE "~@")
(define DEREF "@")
(define KW ":")
(define KWSYM "\u29e")
(define COMM ";")

(define-struct (blank-exn exn:fail:user) ())