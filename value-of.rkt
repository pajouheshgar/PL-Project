#lang racket
(require "parse-scan.rkt")
(require (lib "eopl.ss" "eopl"))
(require "env.rkt")
(require "utils.rkt")



(sllgen:make-define-datatypes my-lexical-spec my-grammar)

(define scan&parse
  (sllgen:make-string-parser my-lexical-spec my-grammar))

(define (value->num val)
  (cases value val
    [a-number-value (num) num]
    [else 0]))

(define (value->string val)
  (cases value val
    [a-string-value (str) (read-str str)]
    [else "sag"]))

(define (extract-pair-list-from-obj obj)
  (cases object obj
    [an-object (pair-list) pair-list]))

(define (extract-size pair-list)
  (if (null? pair-list)
      1000
      (cases pair (car pair-list)
        [a-pair (k v) (if (equal? (read-str k) "size")
                          (value->num v)
                          (extract-size (cdr pair-list)))])))

(define (extract-pair-list-from-a-object-value aov)
  (cases value aov
    [a-object-value (obj) (extract-pair-list-from-obj obj) ]
    [else (list)]))

(define (extract-key-value-pair-from-pair-list pair-list)
  (if (null? pair-list)
      (list)
      (cases pair (car pair-list)
        [a-pair (k v) (cons (var-item (read-str k) (value->string v)) (extract-key-value-pair-from-pair-list (cdr pair-list)))])))

(define (extract-assignment pair-list)
  (if (null? pair-list)
      (a-object-value (an-object (list)))
      (cases pair (car pair-list)
        [a-pair (k v) (if (equal? (read-str k) "assignment")
                          v
                          (extract-assignment (cdr pair-list)))])))


(define (extract-assignment-env-from-pair-list pair-list)
  (let ([aov (extract-assignment pair-list)])
    (let ([inner-pair-list (extract-pair-list-from-a-object-value (extract-assignment pair-list))])
      (extract-key-value-pair-from-pair-list inner-pair-list))))
      


(define (value-of-program prog)
  (cases program prog
          [a-program (obj) (value-of-object obj (empty-env))]))




(define (value-of-object obj prog-env)
  (cases object obj
    [an-object (pair-list)
               (begin (extract-assignment-env-from-pair-list pair-list))]))
    


(define (main program-file)
  (scan&parse (read-program-from-file program-file))
  ) 


;(main "program.txt")
(value-of-program (scan&parse (read-program-from-file "program.txt")))          

