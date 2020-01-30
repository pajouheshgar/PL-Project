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


(define (a-obj-value->obj aov)
  (cases value aov
    [a-object-value (obj) obj]
    [else "error13"]))

(define (value->num val)
  (cases value val
    [a-number-value (num) num]
    [else 0]))

(define (value->string val)
  (cases value val
    [a-string-value (str) (read-str str)]
    [else "error16"]))

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

(define (extract-docs pair-list)
  (extract-key-from-pair-list pair-list "docs" "/files/"))

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
                      (begin ;(display pair-list-inner)
                      (let ([fun-body (body->fun-body (extract-key-from-pair-list pair-list-inner "body" 0))]
                            [args (string-split (value->string (extract-key-from-pair-list pair-list-inner "input" (a-string-value "\"shitarg\""))) ",")])
                        (begin ;(display args)
                        (cons (fun-item (read-str k) fun-body args current-env) (extract-func-env-from-pair-list (cdr pair-list) current-env)))))))])))

(define (value-of-program prog)
  (cases program prog
          [a-program (obj) (value-of-object obj (empty-env))]))

(define (doc-list->str-list doc-list)
  (map remove-punc (map file->string doc-list)))

(define (doc-contains? file query)
  (let ([doc-str (remove-punc (file->string file))])
    (string-contains? doc-str query)))

(define (filter-docs doc-list query)
    (filter (lambda (doc) (string-contains? (remove-punc (file->string doc)) query)) doc-list))

(define (value-of-object obj prog-env)
  (let ([pair-list (extract-pair-list-from-obj obj)])
    (let ([query (extract-query pair-list)]
          [size (extract-size pair-list)]
          [func-env (extract-func-env-from-pair-list pair-list prog-env)]
          [docs (extract-docs pair-list)]
          )
      (let ([new-func-env (ext-env func-env prog-env)])
      (let (
            [assignment-env (extract-assignment-env-from-pair-list pair-list new-func-env)]
            )
        (let ([new-env (ext-env (append assignment-env func-env) prog-env)])
      (begin
             (cases value docs
               [a-object-value (obj)
                               (let ([found-docs (value-of-object obj new-env)])
                                 (search-value query new-env found-docs))]
               [a-string-value (search-dir)
                               (search-value query new-env (dir->docs (read-str search-dir)))]
               [else "Error515"]))
      ))))))
             
(define (resolve-fun-val-from-args val arg-index arg-env arg-list doc-list)
  (let ([search-val (list-ref arg-list arg-index)])
    (search-value search-val arg-env doc-list)
  ))

(define (resolve-fun-val val fenv fargs arg-env arg-list doc-list)
  (if (is-in-list fargs val)
      (resolve-fun-val-from-args val (index-of fargs val) arg-env arg-list doc-list)
      (search-value val fenv doc-list)
      ))
      

(define (lazy-eval fitem fthunk starting-doc-list found-doc-list operand)
  (cases env-item fitem
    [fun-item (fname fbody fargs fenv)
              (cases thunk fthunk
                [args-thunk (arg-list arg-env)
                            (cases func-body fbody
                              [fun-body (fo op ro)
                                        (if (null? op)
                                            (cond
                                                [(equal? operand '!) (resolve-fun-val fo fenv fargs arg-env arg-list starting-doc-list)]
                                                [(equal? operand '+)
                                                 (remove-duplicates (append found-doc-list (resolve-fun-val fo fenv fargs arg-env arg-list starting-doc-list)))]
                                                [(equal? operand '*) (resolve-fun-val fo fenv fargs arg-env arg-list found-doc-list)])
                                            
                                            (let ([next-op (car op)]
                                                  [new-fitem (fun-item fname (fun-body (car ro) (cdr op) (cdr ro)) fargs fenv)])
                                              (cond
                                                [(equal? operand '!)
                                                 (let ([new-found-doc-list (resolve-fun-val fo fenv fargs arg-env arg-list starting-doc-list)])
                                                   (begin ;(display "")
                                                   (if (and (null? new-found-doc-list) (equal? next-op '*))
                                                       (list)
                                                       (lazy-eval new-fitem fthunk starting-doc-list new-found-doc-list next-op))))
                                                 ]
                                                [(equal? operand '+)
                                                 (let ([new-found-doc-list (remove-duplicates (append found-doc-list (resolve-fun-val fo fenv fargs arg-env arg-list starting-doc-list)))])
                                                   (if (and (null? new-found-doc-list) (equal? next-op '*))
                                                       (list)
                                                       (lazy-eval new-fitem fthunk starting-doc-list new-found-doc-list next-op)))                                                 
                                                 ]
                                                [(equal? operand '*)
                                                 (begin ;(display "")
                                                 (let ([new-found-doc-list (resolve-fun-val fo fenv fargs arg-env arg-list found-doc-list)])
                                                   (if (and (null? new-found-doc-list) (equal? next-op #\*))
                                                       (list)
                                                       (lazy-eval new-fitem fthunk starting-doc-list new-found-doc-list next-op))))
                                                 ])
                                                

                                              ))
                                        ])])
              
              ]
    [else 3]))

               
(define (search-value val envi doc-list)
  (if (is-func-call? val)
      (let ([func-name (func-call-str->func-name val)]
            [func-args (func-call-str->func-args val)]
            )
      (let ([func-item (apply-env func-name envi)]
            [func-thunk (args-thunk func-args envi)]
            )
        (begin (display (string-append "Calling function " func-name "\n"))
        (lazy-eval func-item func-thunk doc-list doc-list '!))
        ))
      (let ([result (apply-env val envi)])
        (if (equal? result "not-found")
            (begin ;(display val)
                   (filter-docs doc-list val))
            (let ([found-val (var-item->value result)]
                  [found-env (var-item->env result)]
                  )
              (search-value found-val found-env doc-list))
            ))))
      


(define (main program-file)
  (scan&parse (read-program-from-file program-file))
  ) 


;(main "program.txt")
(value-of-program (scan&parse (read-program-from-file "tests/input9.txt")))          

