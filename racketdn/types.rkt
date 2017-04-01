#lang racket

(provide (struct-out Number)
         (struct-out Symbol)
         (struct-out List))

(struct Number (value) #:transparent)

(struct Symbol (value) #:transparent)

(struct List (value) #:transparent)