#lang racket

(require "../../p3.rkt")


(define testcase
  '((lambda (x) (if (equal? x 0)
                    (begin (set! x 0) (begin (set! x (+ x 1)) (set! x (+ x 1)) x))
                    (begin (set! x (+ x x)) x)))
    10))

(with-output-to-file "output"
                     (lambda ()
                       (printf "~a" (run testcase))))
