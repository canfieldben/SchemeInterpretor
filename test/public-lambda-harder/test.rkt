#lang racket

(require "../../p3.rkt")

; The not-false values are treated as true.
(with-output-to-file "output"
                     (lambda ()
                       (printf "~a" 
                               (run '((lambda (x y z) (x (y (z (lambda (x y) (lambda (x) (lambda (x y) 10)))))))
 (lambda (x) (x x x))
 (lambda (x) (x x))
 (lambda (x) (x x x)))))))
