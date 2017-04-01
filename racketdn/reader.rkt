#lang racket
(require racket/match
         "types.rkt")

(provide read-string)

(define TOKENS (pregexp "[\\s,]*(~@|[\\[\\]{}()'`~^@]|\"(?:\\\\.|[^\\\\\"])*\"|;.*|[^\\s\\[\\]{}('\"`,;)]*)"))
(define LEFTPAREN "(")
(define RIGHTPAREN ")")

(struct reader-syntax (tokens [position #:mutable]) #:transparent)

(define (next stx)
  (let* ([position (reader-syntax-position stx)]
         [token (if (>= position (- (length (reader-syntax-tokens stx)) 1))
                    eof
                    (list-ref (reader-syntax-tokens stx) position))])
    (set-reader-syntax-position! stx (+ position 1))
    token))

(define (peek stx)
  (let ([position (reader-syntax-position stx)])
    (if (>= position (- (length (reader-syntax-tokens stx)) 1))
        eof
        (list-ref (reader-syntax-tokens stx) position))))

(define (tokenizer str)
  (regexp-match* TOKENS str #:match-select cadr))

(define (read-string str)
  (read-form (reader-syntax (tokenizer str) 0)))

(define (read-list stx)
  (define (loop out)
    (let ([v (read-form stx)])
      (cond
        [(match-leftparens? v) (loop out)]
        [(match-rightparens? v) (List (reverse out))]
        [(eof-object? v) (List (reverse out))]
        [else (loop (cons v out))])))
  (next stx)
  (loop '()))

(define (match-numbers? val)
  (if (string? val)
      (regexp-match-exact? (pregexp "\\d+") val)
      #f))

(define (match-leftparens? val)
  (if (string? val)
      (regexp-match-exact? #rx"\\[|\\(" val)
      #f))

(define (match-rightparens? val)
  (if (string? val)
      (regexp-match-exact? #rx"\\]|\\)" val)
      #f))

(define (read-atom stx)
  (let ([val (next stx)])
    (cond
      [(match-rightparens? val) val]
      [(eof-object? val) eof]
      [(match-numbers? val) (Number (string->number val))]
      [else (Symbol (string->symbol val))])))

(define (read-form stx)
  (let ([x (peek stx)])
    (if (match-leftparens? x)
        (read-list stx)
        (read-atom stx))))

#|
(define s (read-string "(123 4556 (abc))"))
(println s)
|#