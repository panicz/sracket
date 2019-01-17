#lang racket
(require racket/match)

(define-for-syntax (every pred stx)
  (andmap pred (syntax->list stx)))

(define-for-syntax (any pred stx)
  (ormap pred (syntax->list stx)))

(define-for-syntax (empty? stx)
  (null? (syntax->list stx)))

(define-for-syntax (racket-argument? x)
  "racket argument is either an identifier, a keyword or a [name value] pair"
  (or (identifier? x)
      (keyword? (syntax->datum x))
      (let ((list (syntax->list x)))
	(and (list? list)
	     (= (length list) 2)
	     (let ((name (syntax->datum (car list))))
	       (and (not (eq? name 'quote))
		    (not (eq? name 'quasiquote))))))))

(provide #%module-begin #%app #%datum syntax-rules
         (rename-out [cdefine define]
                     [mlambda lambda]
                     [named-match-let-values let]
                     [or/values or]
                     [match-let*-values let*]
                     [and-let*/match and-let*]
                     [letrec-syntax/rules letrec-syntax]
                     [let-syntax/rules let-syntax]
                     [define-syntax/rules define-syntax]))

(define-syntax match-lambda-rest
  (syntax-rules ()
    ((match-lambda-rest failure (first second . rest) (processed ...) body ...)
     (match-lambda-rest failure (second . rest) (processed ... first) body ...))

    ((match-lambda-rest failure (last) (processed ...) body ...)
     (match-lambda* ((list processed ... last) body ...)
                    (_ failure)))

    ((match-lambda-rest failure (last . tail) (processed ...) body ...)
     (match-lambda* ((list-rest processed ... last tail) body ...)
                    (_ failure)))))

(define-syntax define-syntax/rules
  (syntax-rules ()
    ((_ (name . pattern) template)
     (define-syntax name
       (syntax-rules ()
         ((name . pattern) template))))
    
    ((_ (name . pattern) . expansion)
     (define-syntax name
       (syntax-rules ()
         ((name . pattern) (begin . expansion)))))
    
    ((_ name transformer)
     (define-syntax name transformer))
    
    ((_ name keywords (pattern template) ...)
     (define-syntax name
       (syntax-rules keywords
         (pattern
          template)
         ...)))
    
    ((_ name keywords (pattern . expansion) ...)
     (define-syntax name
       (syntax-rules keywords
         (pattern
          (begin . expansion))
         ...)))
    ))

(define-syntax let*-syntax/rules
  (syntax-rules ()
    ((_ let*-syntax () processed-bindings body . *)
     (let*-syntax processed-bindings body . *))
    
    ((_ let*-syntax (((name pattern ...) template) bindings ...) 
        (processed ...) body . *)
     (let*-syntax/rules
      let*-syntax
      (bindings ...) 
      (processed ... (name (syntax-rules () 
                             ((_ pattern ...) 
                              template))))
      body . *))
    
    ((_ let*-syntax ((name value) bindings ...) (processed ...) body . *)
     (let*-syntax/rules
      let*-syntax
      (bindings ...) 
      (processed ... (name value))
      body . *))
    
    ((_ let*-syntax ((name keywords (pattern template) ...) bindings ...)
        (processed ...)
        body . *)
     (let*-syntax/rules
      let*-syntax
      (bindings ...)
      (processed ... (name (syntax-rules keywords 
                             (pattern 
                              template) 
                             ...)))
      body . *))
    ))

(define-syntax let-syntax/rules
  (syntax-rules ()
    ((_ (bindings ...) body . *)
     (let*-syntax/rules let-syntax (bindings ...) () body . *))))

(define-syntax letrec-syntax/rules
  (syntax-rules ()
    ((_ (bindings ...) body . *)
     (let*-syntax/rules letrec-syntax (bindings ...) () body . *))))

(define-syntax mlambda
  (lambda (stx)
    (syntax-case stx ()
      
      ((_ (first-arg ... last-arg . rest-args) . body)
       (and (every racket-argument? #'(first-arg ... last-arg))
            (or (identifier? #'rest-args)
                (empty? #'rest-args)))
       #'(lambda (first-arg ... last-arg . rest-args) . body))
      
      ((_ arg body ...)
       (or (identifier? #'arg) (empty? #'arg))
       #'(lambda arg body ...))
      
      ((_ args body ...)
       #'(match-lambda-rest (error '(lambda args body ...)) args () body ...))
      )))

(define-syntax cdefine
  (syntax-rules ()
    ((_ ((head . tail) . args) body ...)
     (cdefine (head . tail) (mlambda args body ...)))

    ((_ (function . args) body ...)
     (define function
       (procedure-rename
	(mlambda args body ...)
	'function)))
    ((_ . rest)
     (define . rest))
    ))

(define-syntax list<-values
  (syntax-rules ()
    ((_ call)
     (call-with-values (lambda () call) list))))

(define-syntax-rule (match-let/error ((structure expression) ...)
				     body + ...)
  ((match-lambda-rest
    (error "Invalid pattern:"
	   '(let ((structure expression) ...) body + ...)
	   `(expression ,expression) ...)
    
    (structure ...) () body + ...)
   expression ...))

(define-syntax named-match-let-values
  (lambda (stx)
    (syntax-case stx ()
      ((_ ((identifier expression) ...) ;; optimization: plain "let" form
          body + ...)
       (every identifier? #'(identifier ...))
       #'(let ((identifier expression) ...)
           body + ...))
      
      ((_ name ((identifier expression) ...) ;; optimization: regular named-let
          body + ...)
       (and (identifier? #'name) (every identifier? #'(identifier ...)))
       #'(let name ((identifier expression) ...)
           body + ...))
      
      ((_ name ((structure expression) ...)
          body + ...)
       (identifier? #'name)
       #'(letrec ((name (mlambda (structure ...) body + ...)))
           (name expression ...)))
      
      ((_ ((structure expression) ...)
          body + ...)
       #'(match-let/error ((structure expression) ...) 
                          body + ...))
      
      ((_ ((identifier identifiers ... expression)) body + ...)
       (every identifier? #'(identifier identifiers ...))
       #'(call-with-values (lambda () expression)
                           (lambda (identifier identifiers ... . _)
                             body + ...)))
      
      ((_ ((structure structures ... expression)) body + ...)
       #'(call-with-values (lambda () expression)
                           (match-lambda-rest
                            (error '(let ((structure structures ... expression))
				      body + ...))
                            (structure structures ... . _)
                            () body + ...)))
      
      ((_ name ((identifier identifiers ... expression)) body + ...)
       (and (identifier? #'name)
            (every identifier? #'(identifier identifiers ...)))
       #'(let ((name (lambda (identifier identifiers ...) body + ...)))
           (call-with-values (lambda () expression) name)))
      
      ((_ name ((structure structures ... expression)) body + ...)
       (identifier? #'name)
       #'(let ((name (match-lambda-rest
		      (error '(let name ((structure structures ... expression))
				body + ...))
                      (structure structures ...) () body + ...)))
           (call-with-values (lambda () expression) name)))
      
      ;; it should generally be discouraged to use the plain let
      ;; with multiple values, because there's no natural way to implement
      ;; that when there's more than one (multiple-value) binding,
      ;; but it could be added for completeness
      #|
      ((_ ((structure structures ... expression) ...)
	  body + ...)
       #'(match-let/error (((structure structures ... . _) 
			    (list<-values expression)) ...)
			  body + ...))
      
      ((_ name ((structure structures ... expression) ...)
	  body + ...)
       (identifier? #'name)
       #'(letrec ((loop 
		   (match-lambda* 
		       (((structure structures ... . _) ...)
			(let-syntax ((name (syntax-rules ()
					     ((_ args (... ...))
					      (loop (list<-values args)
						    (... ...))))))
			  body + ...))
		     (_ (error 'named-match-let-values 
			       (current-source-location)
			       'name)))))
		  (loop (list<-values expression) ...)))
|#
      )))

(define-syntax or/values
  (syntax-rules ()
    ((_)
     #false)
    
    ((or/values final)
     final)
    
    ((or/values first . rest)
     (call-with-values (lambda () first)
                       (lambda result
                         (if (and (pair? result) (car result))
                             (apply values result)
                             (or/values . rest)))))))

(define-syntax match-let*-values
  (lambda (stx)
    (syntax-case stx ()
      
      ((_ ((identifier expression) ...) ;; optimization: regular let*
          body + ...)
       (every identifier? #'(identifier ...))
       #'(let* ((identifier expression) ...)
           body + ...))
      
      ((_ ((identifier expression) remaining-bindings ...)
          body + ...)
       (identifier? #'identifier)
       #'(let ((identifier expression))
           (match-let*-values (remaining-bindings ...) body + ...)))
      
      ((_ ((structure expression) remaining-bindings ...)
          body + ...)
       #'(match-let/error ((structure expression))
                          (match-let*-values (remaining-bindings ...) 
                                             body + ...)))
      
      ((_ ((identifier identifiers ... expression) remaining-bindings ...)
          body + ...)
       (every identifier? #'(identifier identifiers ...))
       #'(call-with-values (lambda () expression) 
                           (lambda (identifier identifiers ... . _)
                             (match-let*-values (remaining-bindings ...) 
                                                body + ...))))
      
      ((_ ((structure structures ... expression) remaining-bindings ...)
          body + ...)
       #'(call-with-values (lambda () expression)
                           (match-lambda-rest
                            (error '(let* ((structure structures ... expression)
                                           remaining-bindings ...)
                                      body + ...))
                            (structure structures ... . _) ()
                            (match-let*-values (remaining-bindings ...) 
                                                              body + ...))))
      )))

(define-syntax and-let*/match
  (lambda (stx)
    (syntax-case stx ()
      
      ((_)
       #'#t)
      
      ((_ ())
       #'#t)
      
      ((_ () body ...)
       #'(let () body ...))
      
      ((_ ((value binding) rest ...) body ...)
       (identifier? #'value)
       #'(let ((value binding))
           (and value
                (and-let*/match (rest ...)
                                body ...))))
      
      ((_ ((value binding) rest ...) body ...)
       #'(match binding
           (value
            (and-let*/match (rest ...)
                            body ...))
           (_ #f)))
      
      ((_ ((condition) rest ...)
          body ...)
       #'(and condition
              (and-let*/match (rest ...)
                              body ...)))
      
      ((_ ((value values ... expression) rest ...) body ...)
       (identifier? #'value)
       #'(call-with-values (lambda () expression)
                           (match-lambda-rest #f (value values ... . _) ()
                              (and value
                                   (and-let*/match (rest ...)
                                                   body ...)))))
      
      ((_ ((values ... expression) rest ...) body ...)
       #'(call-with-values (lambda () expression)
                           (match-lambda-rest #f (values ... . _) ()
                              (and-let*/match (rest ...)
                                              body ...))))      
      )))

;;(named-match-let-values ((a b c (values 1 2 3))) (+ a b c))
;;(map (mlambda (`(,x ,y)) (+ x y)) '((1 2)(3 4)(5 6)))
