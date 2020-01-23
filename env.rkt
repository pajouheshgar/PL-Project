#lang racket
(require "parse-scan.rkt")
(require (lib "eopl.ss" "eopl"))
(require "utils.rkt")


(define-datatype func-body func-body?
  [fun-body [first-operand string?]
            [operators (list-of symbol?)]
            [rest-operands (list-of string?)]])

(define-datatype env-item env-item?
  [var-item [key-var string?]
            [value-var string?]
            [fun-env env?]]
  [fun-item [fun-name string?]
            [fun-body func-body?]
            [fun-args (list-of string?)]
            [fun-env env?]])
            
(define (env-item->key ei)
  (cases env-item ei
    [var-item (k v fe) k]
    [fun-item (fn fb fa fe) fn]))

(define (var-item->value vi)
  (cases env-item vi
    [var-item (k v fe) v]
    [else "error"]))

(define (var-item->env vi)
  (cases env-item vi
    [var-item (k v fe) fe]
    [else "error"]))

(define-datatype env env?
  [empty-env]
  [ext-env [new-env (list-of env-item?)]
           [save-env env?]])

(define (find-key-in-item-list item-list k)
  (if (null? item-list)
      #f
      (if (equal? (env-item->key (car item-list)) k)
          (car item-list)
          (find-key-in-item-list (cdr item-list) k))))

(define (apply-env k envi)
  (cases env envi
    [empty-env () "not-found"]
    [ext-env (item-list save-env)
             (if (is-in-list (map env-item->key item-list) k)
                 (find-key-in-item-list item-list k)
                 (apply-env k save-env))]))
             
  

(provide (all-defined-out))
