#lang racket

(require readline/readline
         "env.rkt"
         "reader.rkt"
         "printer.rkt"
         "types.rkt"
         "utils.rkt")

(define env-init
  (hasheq
   '+ (λ (a . b) (apply + (cons a b)))
   '- (λ (a . b) (apply - (cons a b)))
   '* (λ (a . b) (apply * (cons a b)))
   '/ (λ (a . b) (apply / (cons a b)))))

(define repl-env (environment null))
(hash-for-each env-init (λ (k v) (set-env! repl-env k v)))

(define (eval-ast ast env)
  (cond
    [(symbol? ast) (get-env-val env ast)]
    [(list? ast) (map (λ (i) (EVAL i env)) ast)]
    [(vector? ast) (vector-map (λ (i) (EVAL i env)) ast)]
    [(hash? ast) (for/hash ([k (hash-keys ast)])
                   (values k (EVAL (hash-ref ast k) env)))]
    [else ast]))

(define (READ s) (read-string s))

(define (EVAL ast env)
  (cond
    [(not (list? ast)) (eval-ast ast env)]
    [(and (list? ast) (empty? ast)) ast]
    [(list? ast)
     (let ([sym (car ast)])
       (cond
         [(eq? sym 'def!)
          (set-env! env (second ast) (EVAL (third ast) env))]
         [(eq? sym 'let*) (handle-letrec EVAL ast env)]
         [else
          (let* ([evald (eval-ast ast env)]
                 [fn (car evald)])
            (apply fn (cdr evald)))]))]))

(define (PRINT ast) (print-string ast))

(define (rep s) ((compose PRINT (curryr EVAL repl-env) READ) s))

(define (repl-loop)
  (let ([line (readline "user> ")])
    (when (not (eq? null line))
      (with-handlers
        ([string? (lambda (exc) (printf "Error: ~a~n" exc))]
         [blank-exn? (lambda (exc) null)])
        (let ([out (rep line)])
          (when (not (null? out)) (printf "~a~n" out))))
      (repl-loop))))

(repl-loop)
