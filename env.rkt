#lang racket
(require "parse-scan.rkt")
(require (lib "eopl.ss" "eopl"))

(define-datatype func-body func-body?
  [fun-body [first-operand string?]
            [operators (list-of symbol?)]
            [rest-operands (list-of string?)]])

(define-datatype env-item env-item?
  [var-item [key-var string?]
            [value-var string?]]
  [fun-item [fun-name string?]
            [fun-body func-body?]
            [fun-args (list-of string?)]
            [fun-env env?]])
            
            

(define-datatype env env?
  [empty-env]
  [ext-env [new-env (list-of env-item?)]
           [save-env env?]])


(provide (all-defined-out))
