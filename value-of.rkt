#lang racket
(require "parse-scan.rkt")
(require (lib "eopl.ss" "eopl"))


(sllgen:make-define-datatypes my-lexical-spec my-grammar)

(define scan&parse
  (sllgen:make-string-parser my-lexical-spec my-grammar))

(define (read-str s)
  (read (open-input-string  s)))

(define (read-program-from-file file)
  (file->string file)
  )


(define (value-of-program prog)
  (cases program prog
          [a-program (obj) (value-of-object obj 2 2)]))




(define (value-of-object obj env-var env-fun)
  (cases object obj
    [an-object (pair-list)
               (cases pair (car pair-list)
                 [a-pair (k v) (read-str k)])]))
    


(define (main program-file)
  (scan&parse (read-program-from-file program-file))
  ) 


(main "program.txt")
          

