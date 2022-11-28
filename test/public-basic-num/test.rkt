#lang racket

(require "../../p3.rkt")

(with-output-to-file "output"
                     (lambda () (printf "~a" (run 173539))))