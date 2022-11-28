#lang racket

(require "../../p3.rkt")

(define testcase '(let ([x 1] [y 2]) (begin (set! x 3) (begin (set! x 4) (set! y x)) (begin (begin 1 (set! y 2)) y))))

(with-output-to-file "output"
                     (lambda ()
                       (printf "~a" (run testcase))))
