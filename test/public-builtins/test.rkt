#lang racket

(require "../../p3.rkt")

(with-output-to-file "output"
                     (lambda ()
                       (printf "~a" (run '(equal? (sub1 (add1 (- (/ 2 2) (+ 1 2)))) -2)))))
