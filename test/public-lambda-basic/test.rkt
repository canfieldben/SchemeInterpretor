#lang racket

(require "../../p3.rkt")

; The not-false values are treated as true.
(with-output-to-file "output"
                     (lambda ()
                       (printf "~a" 
                               (run '(((lambda (x) x) (lambda (x) x)) 5)))))
