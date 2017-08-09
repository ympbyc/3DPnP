#lang racket

(provide obj-methods)

(define (obj-methods obj)
  (interface->method-names (object-interface obj)))