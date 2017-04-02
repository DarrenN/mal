#lang racket
(require "types.rkt")

(provide read-string)

(struct reader-syntax (tokens [position #:mutable]) #:transparent)

(define (tokenizer str)
  (filter-not (λ (s) (equal? s ""))
              (regexp-match* TOKENS str #:match-select cadr)))

(define (next stx)
  (let* ([position (reader-syntax-position stx)]
         [token (if (>= position (length (reader-syntax-tokens stx)))
                    null
                    (list-ref (reader-syntax-tokens stx) position))])
    (set-reader-syntax-position! stx (+ position 1))
    token))

(define (peek stx)
  (let ([position (reader-syntax-position stx)])
    (if (>= position (length (reader-syntax-tokens stx)))
        null
        (list-ref (reader-syntax-tokens stx) position))))

(define (read-string str)
  (read-form (reader-syntax (tokenizer str) 0)))

(define (read-form reader)
  (let ([token (peek reader)])
    (if (or (null? token) (string-prefix? token COMM))
        (raise (make-blank-exn "blank line" (current-continuation-marks)))
        (cond
          [(equal? LPAR token) (read-list reader LPAR RPAR)]
          [(equal? LVEC token) (list->vector (read-list reader LVEC RVEC))]
          [(equal? LMAP token) (apply hasheq (read-list reader LMAP RMAP))]
          [(equal? QUOTE token)
           (next reader) (list 'quote (read-form reader))]
          [(equal? QUASI token)
           (next reader) (list 'quasiquote (read-form reader))]
          [(equal? UNQUO token)
           (next reader) (list 'unquote (read-form reader))]
          [(equal? SPLICE token)
           (next reader) (list 'splice-unquote (read-form reader))]
          [(equal? DEREF token)
           (next reader) (list 'deref (read-form reader))]
          [else (read-atom reader)]))))

(define (read-atom reader)
  (let ([token (next reader)])
    (cond
      [(regexp-match #px"^-?[0-9.]+$" token) (string->number token)]
      [(regexp-match #px"\"(?:\\\\.|[^\\\\\"])*" token)
       (regexp-match #px"^\".*\"$" token)
           (string-replace
             (string-replace
               (string-replace
                 (substring token 1 (- (string-length token) 1))
                 "\\\"" "\"")
               "\\n" "\n")
             "\\\\" "\\")]
      [(string-prefix? token KW)
       (string->symbol (string-replace token KW KWSYM))]
      [else (string->symbol token)])))


(define (read-list-entries reader end)
  (let ([token (peek reader)])
    (cond
        [(eq? token '()) (raise (string-append "expected '" end "'"))]
        [(equal? end token) '()]
        [else
          (cons (read-form reader) (read-list-entries reader end))])))

(define (read-list reader start end)
  (let ([token (next reader)])
    (if (equal? start token)
      (let ([lst (read-list-entries reader end)])
        (next reader)
        lst)
      (raise (string-append "expected '" start "'")))))
