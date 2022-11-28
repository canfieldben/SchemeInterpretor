#lang racket

(require "../../p3.rkt")

(with-output-to-file "output"
                     (lambda ()
                       (printf "~a ~a" (run #t) (run #f))))