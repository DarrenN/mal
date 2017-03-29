#lang racket
(require racket/match
         "types.rkt")

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
  (let ([v (next stx)])
    (define (loop stx last out)
      (cond
        [(and (eof-object? (peek stx)) (not (equal? RIGHTPAREN last)))
         (raise "EOF before list end")]
        [(eof-object? (peek stx)) (reverse out)]
        [else (loop stx (peek stx) (cons (read-form stx) out))]))
    (loop stx v (list v))))

(define (read-atom stx)
  (next stx))

(define (read-form stx)
  (let ([x (peek stx)])
    (if (equal? LEFTPAREN x)
        (read-list stx)
        (read-atom stx))))


(define s (read-string "true"))
s