#lang racket
(require (lib "eopl.ss" "eopl"))



(define my-lexical-spec
  '([whitespace (whitespace) skip]
    [comment ("%" (arbno (not #\newline))) skip]
    [value-string ("\"" (arbno (not #\")) "\"") string]
    ;[key-string ("\"" letter (arbno (or letter digit "_")) "\"") symbol]
    [number (digit (arbno digit)) number]
    [operator ((or "*" "+")) symbol]
    
    )
  )

(define my-grammar
  '([program (object) a-program]
    [object ("{" (separated-list pair ",") "}") an-object]
    [pair (value-string ":" value) a-pair]
    [value (value-string) a-string-value]
    [value (number) a-number-value]
    [value (slist) a-list-value]
    [value (object) a-object-value]
    [slist ("[" value-string (arbno operator value-string) "]") a-list]))
    




(provide (all-defined-out))



