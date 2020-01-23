#lang racket
;(require (lib "eopl.ss" "eopl"))
;(require "parse-scan.rkt")

(define (read-str s)
  (read (open-input-string  s)))

(define (read-program-from-file file)
  (file->string file)
  )

(provide (all-defined-out))




