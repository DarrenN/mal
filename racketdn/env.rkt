#lang racket

(require "utils.rkt")

(provide (struct-out environment)
         set-env!
         find-env
         get-env-val
         handle-letrec)

(struct environment (outer [data #:auto #:mutable])
  #:auto-value (hasheq)
  #:transparent)

(define (set-env! env key val)
  (set-environment-data! env (hash-set (environment-data env) key val))
  val)

(define (find-env env key)
  (let ([outer (environment-outer env)]
        [data (environment-data env)])
    (cond
      [(hash-has-key? data key) env]
      [(null? outer) null]
      [else (find-env outer key)])))

(define (get-env-val env key)
  (let ([e (find-env env key)])
    (if (environment? e)
        (hash-ref (environment-data e) key)
        (raise (format "symbol ~a not found in env" key)))))

(define (process-bindings bds)
  (cond
    [(list? bds) (list->pairs bds)]
    [(vector? bds) (list->pairs (vector->list bds))]))

; Recursively set new values within an environment and then
; evaluate the body within that new environment
(define (handle-letrec EVAL ast env)
  (let* ([nenv (environment env)]
         [newbs (process-bindings (second ast))]
         [body (third ast)])
    ; load new syms/bindings into new env with set-env!
    (for ([b newbs])
      (let* ([nsym (first b)]
             [nevald (EVAL (second b) nenv)])
        (set-env! nenv nsym nevald)))
    ; eval body of let* with new env
    (EVAL body nenv)))
