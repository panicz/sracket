#lang racket
(provide is isnt fold-right find for memoized define/memoized out)

(define fold-right foldr)

(define find findf)

(define-syntax infix/postfix
  (syntax-rules ()
    
    ((infix/postfix x somewhat?)
     (somewhat? x))

    ((infix/postfix left related-to? right)
     (related-to? left right))

    ((infix/postfix left related-to? right . likewise)
     (let ((right* right))
       (and (infix/postfix left related-to? right*)
	    (infix/postfix right* . likewise))))
    ))

(define-syntax extract-placeholders
  (syntax-rules (_)
    ((extract-placeholders final () () body)
     (final (infix/postfix . body)))

    ((extract-placeholders final () args body)
     (lambda args (final (infix/postfix . body))))

    ((extract-placeholders final (_ op . rest) (args ...) (body ...))
     (extract-placeholders final rest (args ... arg) (body ... arg op)))

    ((extract-placeholders final (arg op . rest) args (body ...))
     (extract-placeholders final rest args (body ... arg op)))

    ((extract-placeholders final (_) (args ...) (body ...))
     (extract-placeholders final () (args ... arg) (body ... arg)))

    ((extract-placeholders final (arg) args (body ...))
     (extract-placeholders final () args (body ... arg)))
    ))

(define-syntax-rule (identity-syntax form)
  form)

(define-syntax-rule (is . something)
  (extract-placeholders identity-syntax something () ()))

(define-syntax-rule (isnt . something)
  (extract-placeholders not something () ()))

(define-syntax for
  (syntax-rules (in =>)
    ((for (key => value) in hash-map actions . *)
     (hash-for-each (lambda (key value) actions . *) hash-map))
    
    ((for x in list actions . *)
     (for-each (lambda (x) actions . *) list))))

(define (memoized proc)
  (let ((cache (make-weak-hash)))
    (lambda args
      (if (hash-has-key? cache args)
	  (apply values (hash-ref cache args))
	  (call-with-values (lambda () (apply proc args))
	    (lambda result
	      (hash-set! cache args result)
	      (apply values result)))))))

(define-syntax-rule (define/memoized (name . args) . body)
  (define name (memoized (lambda args . body))))

(define (out . messages)
  (for-each display messages)
  (newline)
  (flush-output))
