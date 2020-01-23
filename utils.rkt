#lang racket

(require (lib "eopl.ss" "eopl"))

(define args-lexical-spec
  '([whitespace (whitespace) skip]
    [identifier (letter (arbno (or letter "_"))) string]
    [func-name (letter (arbno (or letter "_")) "(") string]
    )
  )

(define args-grammar
  '([args () void-args]
    [args (arg  (separated-list arg ",")) a-args]
    [arg (identifier) a-arg]
    [arg (func-name args ")") a-fun-arg]))

(sllgen:make-define-datatypes args-lexical-spec args-grammar)

(define argsparse
  (sllgen:make-string-parser args-lexical-spec args-grammar))

(define (read-str s)
  (read (open-input-string  s)))

(define (read-program-from-file file)
  (file->string file)
  )


(define (is-in-list list value)
 (cond
  [(empty? list) false]
  [(equal? (first list) value) true]
  [else (is-in-list (rest list) value)]))

(define (func-call-str->func-name fcs)
  (car (string-split fcs "(")))

(define (func-call-str->func-args fcs)
  (let ([func-name (func-call-str->func-name fcs)])
    (let ([name-len (string-length func-name)]
          [len (string-length fcs)])
      (extract-args
       (string-normalize-spaces
        (substring fcs (+ 1 name-len) (- len 1)) #px"\\s+" "") 0 0 0
       ))))
    

(define (extract-args args-str c k1 k2)
  (if (equal? k2 (string-length args-str))
      (list (substring args-str k1 k2))
      (if (equal? c 0)
          (cond [
                 (equal? (string-ref args-str k2) #\,)
                 (cons (substring args-str k1 k2) (extract-args args-str 0 (+ 1 k2) (+ 1 k2)))
                 ]
                [(equal? (string-ref args-str k2) #\()
                 (extract-args args-str (+ 1 c) k1 (+ 1 k2))
                 ]
                [(equal? (string-ref args-str k2) #\))
                 (extract-args args-str (- c 1) k1 (+ 1 k2))
                 ]
                [else (extract-args args-str c k1 (+ 1 k2))])
          (cond [
                 (equal? (string-ref args-str k2) #\,)
                 (extract-args args-str c k1 (+ 1 k2))
                 ]
                [(equal? (string-ref args-str k2) #\()
                 (extract-args args-str (+ 1 c) k1 (+ 1 k2))
                 ]
                [(equal? (string-ref args-str k2) #\))
                 (extract-args args-str (- c 1) k1 (+ 1 k2))
                 ]
                [else (extract-args args-str c k1 (+ 1 k2))]))))
  
             
          
          
  

(provide (all-defined-out))


(func-call-str->func-args "and-inputs(or_inputs(x, z),y)")

;(extract-args  (string-normalize-spaces "or_inputs(x, z(h(t, j))) ,y, w,t" #px"\\s+" "") 0 0 0)


