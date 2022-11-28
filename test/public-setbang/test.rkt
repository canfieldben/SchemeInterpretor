#lang racket

(require "../../p3.rkt")

(with-output-to-file "output"
                     (lambda ()
                       (printf "~a" (run '(let ([x 5]) (begin (set! x 12) x))))))