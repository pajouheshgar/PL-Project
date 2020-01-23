#lang racket
(require "parse-scan.rkt")
(require (lib "eopl.ss" "eopl"))
(require "env.rkt")
(require "utils.rkt")



(sllgen:make-define-datatypes my-lexical-spec my-grammar)

(define scan&parse
  (sllgen:make-string-parser my-lexical-spec my-grammar))


(define keywords (list "size" "docs" "assignment" "query"))

(define (is-keyword? k)
  (is-in-list keywords k)
  )
  

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

(define (extract-key-from-pair-list pair-list key default)
  (if (null? pair-list)
      default
      (cases pair (car pair-list)
        [a-pair (k v) (if (equal? (read-str k) key)
                          v
                          (extract-key-from-pair-list (cdr pair-list) key default))])))

(define (extract-size pair-list)
  (value->num (extract-key-from-pair-list pair-list "size" (a-number-value 1000))))

(define (extract-pair-list-from-a-object-value aov)
  (cases value aov
    [a-object-value (obj) (extract-pair-list-from-obj obj) ]
    [else (list)]))

(define (extract-key-value-pair-from-pair-list pair-list func-env)
  (if (null? pair-list)
      (list)
      (cases pair (car pair-list)
        [a-pair (k v) (cons (var-item (read-str k) (value->string v) func-env) (extract-key-value-pair-from-pair-list (cdr pair-list) func-env))])))

(define (extract-assignment pair-list)
  (extract-key-from-pair-list pair-list "assignment" (a-object-value (an-object (list)))))

(define (extract-query pair-list)
  (value->string (extract-key-from-pair-list pair-list "query" "sag")))





(define (extract-assignment-env-from-pair-list pair-list func-env)
  (let ([aov (extract-assignment pair-list)])
    (let ([inner-pair-list (extract-pair-list-from-a-object-value (extract-assignment pair-list))])
      (extract-key-value-pair-from-pair-list inner-pair-list func-env))))


(define (body->fun-body body)
  (cases value body
    [a-list-value (l)
                  (cases slist l
                    [a-list (first-operand operators rest-operands) (fun-body (read-str first-operand) operators (map read-str rest-operands))])]
    [else (fun-body "sag" (list) (list))]))


(define (is-func-call? val)
  (string-contains? val "("))


  
                   
    
(define (extract-func-env-from-pair-list pair-list current-env)
  (if (null? pair-list)
      (list)
      (cases pair (car pair-list)
        [a-pair (k v)
                (if (is-keyword? (read-str k))
                    (extract-func-env-from-pair-list (cdr pair-list) current-env)
                    (let ([pair-list-inner (extract-pair-list-from-a-object-value v)])
                      (let ([fun-body (body->fun-body (extract-key-from-pair-list pair-list-inner "body" 0))]
                            [args (string-split (value->string (extract-key-from-pair-list pair-list-inner "input" 0)) ",")])
                        (cons (fun-item (read-str k) fun-body args current-env) (extract-func-env-from-pair-list (cdr pair-list) current-env)))))])))

(define (value-of-program prog)
  (cases program prog
          [a-program (obj) (value-of-object obj (empty-env))]))




(define (value-of-object obj prog-env)
  (let ([pair-list (extract-pair-list-from-obj obj)])
    (let ([query (extract-query pair-list)]
          [size (extract-size pair-list)]
          [func-env (extract-func-env-from-pair-list pair-list prog-env)]
          )
      (let ([new-func-env (ext-env func-env prog-env)])
      (let (
            [assignment-env (extract-assignment-env-from-pair-list pair-list new-func-env)]
            )
        (let ([new-env (ext-env (append assignment-env func-env) prog-env)])
      (begin (display query)
             (display "\n")
             (display size)
             (display "\n")
             (display assignment-env)
             (display "\n")
             (display func-env)
             (display "\n"))
        (resolve-value query new-env)
      ))))))
             
             
               
(define (resolve-value val envi)
  (if (is-func-call? val)
      (let ([func-name (func-call-str->func-name val)])
      (let ([result (apply-env func-name envi)])
        result))
      (let ([result (apply-env val envi)])
        (if (equal? result "not-found")
            val
            (let ([found-val (var-item->value result)]
                  [found-env (var-item->env result)]
                  )
              (resolve-value found-val found-env))
            ))))
      


(define (main program-file)
  (scan&parse (read-program-from-file program-file))
  ) 


;(main "program.txt")
(value-of-program (scan&parse (read-program-from-file "program.txt")))          

