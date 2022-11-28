#lang racket

(require "../../p3.rkt")

(with-output-to-file "output"
                     (lambda ()
                       (printf "~a" (run '(if #f 0 1)))))