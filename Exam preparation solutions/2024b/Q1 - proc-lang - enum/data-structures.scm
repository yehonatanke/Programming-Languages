(module data-structures (lib "eopl.ss" "eopl")

  ;; data structures for proc-lang/ds-rep

  (require "lang.scm")                  ; for expression?

  (provide (all-defined-out))               ; too many things to list

;;;;;;;;;;;;;;;; expressed values ;;;;;;;;;;;;;;;;

;;; an expressed value is either a number, a boolean or a procval.

  (define-datatype expval expval?
    (num-val
      (value number?))
    (bool-val
      (boolean boolean?))
    (proc-val 
      (proc proc?))

    ;; Solution
    (enum-val
     (enum enum?))
    
      (identifier-val
     (e-id symbol?))
    )

  ;; Solution

  ; Extractor for enum
  (define expval->enum
    (lambda (v)
      (cases expval v
        (enum-val (enum) enum)
        (else (expval-extractor-error 'enum v)))))

  ; Predicate for enum
  (define enum?-v1
    (lambda (lst)
      (if (pair? lst) ; Check if 'lst' is a pair (non-empty list)                
          (and (expval? (car lst)) ; Check if the first element of 'lst' is an "expval"                      
               (if (null? (cdr lst)) ; If the rest of the list is empty...
                   #t  ; ...then it's a valid tuple (base case)             
                   (enum? (cdr lst))))   ; Otherwise, recursively check if the rest of the list is a enum  
          #f))) ; If 'lst' is not a pair (i.e., it's empty or not a list), return false
 
  ; Extractor for identifier-val
  (define expval->identifier-val
    (lambda (v)
      (cases expval v
        (identifier-val (idval) idval)
        (else (expval-extractor-error identifier-val v)))))

  (define-datatype enum enum?
    (enumerate
     (ids (list-of expval?))))
  

  ;;; extractors:

    ;; expval->num : ExpVal -> Int
    (define expval->num
    (lambda (v)
      (cases expval v
	(num-val (num) num)
	(else (expval-extractor-error 'num v)))))

  ;; expval->bool : ExpVal -> Bool
  (define expval->bool
    (lambda (v)
      (cases expval v
	(bool-val (bool) bool)
	(else (expval-extractor-error 'bool v)))))

  ;; expval->proc : ExpVal -> Proc
  (define expval->proc
    (lambda (v)
      (cases expval v
	(proc-val (proc) proc)
	(else (expval-extractor-error 'proc v)))))

  (define expval-extractor-error
    (lambda (variant value)
      (eopl:error 'expval-extractors "Looking for a ~s, found ~s"
	variant value)))

;;;;;;;;;;;;;;;; procedures ;;;;;;;;;;;;;;;;

  ;; proc? : SchemeVal -> Bool
  ;; procedure : Var * Exp * Env -> Proc
  (define-datatype proc proc?
    (procedure
      (var symbol?)
      (body expression?)
      (env environment?)))

;;;;;;;;;;;;;;;; environment structures ;;;;;;;;;;;;;;;;

;; example of a data type built without define-datatype

  (define empty-env-record
    (lambda () 
      '()))

  (define extended-env-record
    (lambda (sym val old-env)
      (cons (list sym val) old-env)))
  
  (define empty-env-record? null?)
  
  (define environment?
    (lambda (x)
      (or (empty-env-record? x)
          (and (pair? x)
               (symbol? (car (car x)))
               (expval? (cadr (car x)))
               (environment? (cdr x))))))

  (define extended-env-record->sym
    (lambda (r)
      (car (car r))))

  (define extended-env-record->val
    (lambda (r)
      (cadr (car r))))

  (define extended-env-record->old-env
    (lambda (r)
      (cdr r)))

)
