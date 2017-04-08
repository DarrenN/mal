#lang racket

(require readline/readline
         "reader.rkt"
         "printer.rkt"
         "types.rkt")

(define repl-env (hasheq
                  '+ (λ (a . b) (apply + (cons a b)))
                  '- (λ (a . b) (apply - (cons a b)))
                  '* (λ (a . b) (* a b))
                  '/ (λ (a . b) (exact->inexact (/ a b)))))

(define (eval-ast ast env)
  (cond
    [(symbol? ast) (if (hash-has-key? env ast)
                       (hash-ref env ast)
                       (raise (format "symbol ~a not found" ast)))]
    [(list? ast) (map (λ (i) (EVAL i env)) ast)]
    [else ast]))

(define (READ s) (read-string s))

(define (EVAL ast env)
  (cond
    [(not (list? ast)) (eval-ast ast env)]
    [(and (list? ast) (empty? ast)) ast]
    [(list? ast) (let* ([evald (eval-ast ast env)]
                        [fn (car evald)])
                   (apply fn (cdr evald)))]))

(define (PRINT ast) (print-string ast))

(define (rep s) ((compose PRINT (curryr EVAL repl-env) READ) s))

(define (repl-loop)
  (let ([line (readline "user> ")])
    (when (not (eq? null line))
      (with-handlers
        ([string? (lambda (exc) (printf "Error: ~a~n" exc))]
         [blank-exn? (lambda (exc) null)])
        (printf "~a~n" (rep line)))
      (repl-loop))))

(repl-loop)
