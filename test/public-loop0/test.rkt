#lang racket

(require "../../p3.rkt")

(define testcase '(((lambda (f) (lambda (x) (if (equal? x 0) 1 (* x ((f f) (- x 1))))))
  (lambda (f) (lambda (x) (if (equal? x 0) 1 (* x ((f f) (- x 1)))))))
 20))

(with-output-to-file "output"
                     (lambda ()
                       (printf "~a" (run testcase))))
