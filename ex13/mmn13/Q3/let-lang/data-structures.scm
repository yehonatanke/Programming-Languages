(module data-structures (lib "eopl.ss" "eopl")


  (require "lang.scm")  
  ;; data structures for let-lang.

  (provide (all-defined-out))               ; too many things to list

  ;;;;;;;;;;;;;;;; expressed values ;;;;;;;;;;;;;;;;

  ;;; an expressed value is either a number, a boolean or a procval.

  (define-datatype expval expval?
    (num-val (value number?))
    (bool-val (boolean boolean?))
    (excp-val (Exception except?))
    )

  ;;; extractors:

  ;; expval->exception : ExpVal -> Exception
  (define expval->exception
    (lambda (v)
      (cases expval v
        (excp-val (Exception) Exception)
        (else (excp-val (Exception "general"))))))
  
  ;; expval->num : ExpVal -> Int
  (define expval->num
    (lambda (v)
      (cases expval v
        (num-val (num) num)
        (excp-val (excp) v)
        (else (expval-extractor-error 'num v)))))

  ;; expval->bool : ExpVal -> Bool
  (define expval->bool
    (lambda (v)
      (cases expval v
        (bool-val (bool) bool)
        (excp-val (excp) v)
        (else (expval-extractor-error 'bool v)))))

  (define expval-extractor-error
    (lambda (variant value)
      (excp-val (Exception
                 (case variant
                   ('num "not a number")
                   ('bool "not a boolean")
                   (else "general"))))))


  
  (define (exception-message excp)
    (cases except excp
      (Exception (msg) msg)
      (else "general")))

  (define (exception->message v)
    (cases except v
      (Exception (msg) msg)
      (else (Exception "general"))))

  (define (ensure-exception-type excp)
    (cases except excp
      (Exception (msg) excp)
      (else (Exception "general"))))

  (define (exception-message-correct excp)
    (display excp)
    (newline)
    (cases except excp
      (Exception (msg) msg)
      (else "general")))

  (define expval-is-except?
    (lambda (v)
      (cases expval v
        (excp-val (_) #t)
        (else #f))))

  (define (index-of-v2 pred target lst)
    (if (null? lst)
        #f
        (if (pred target (car lst))
            0
            (let ((next-index (index-of-v2 pred target (cdr lst))))
              (if next-index
                  (+ 1 next-index)
                  #f)))))

  (define (index-of pred target lst)
    (let loop ((lst lst) (index 0))
      (cond
        ((null? lst) #f)
        ((pred target (car lst)) index)
        (else (loop (cdr lst) (+ index 1))))))


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
