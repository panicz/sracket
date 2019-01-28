#!/usr/bin/env racket
#lang racket
(require racket/match)
(require "sracket.rkt")
(require "ground-scheme.rkt")
(require "grand-syntax.rkt")

(slayer-init #:title "GRASP 4: transferable and resizable boxes")

(keydn 'escape exit)

(define ((_ . message) _)
  (apply _ message))

(define (set-stage! stage)
  (set-display-procedure! (lambda () (draw-image! (stage 'as-image))))
  (mousemove (lambda (x y dx dy) (stage 'mouse-move x y dx dy)))
  (keydn 'mouse-left (lambda (x y) (stage 'mouse-down x y)))
  (keyup 'mouse-left (lambda (x y) (stage 'mouse-up x y))))

(define (bit #:left left #:top top
	     #:width width #:height height
	     #:background-color color)
  (let ((image (rectangle width height color)))
    (define (self . message)
      (match message
	(`(position) #;===> `(,left ,top))
	
	(`(size) #;===> `(,width ,height))

	(`(extents) #;===> `(,left ,top ,(+ left width) ,(+ top height)))

	(`(resize-by! ,dx ,dy)
	 (set! width (max 0 (+ width dx)))
	 (set! height (max 0 (+ height dy)))
	 (set! image (rectangle width height color)))
	
	(`(move-by! ,dx ,dy)
	 (set! left (+ left dx))
	 (set! top (+ top dy)))

	(`(embraces? ,x ,y)  (and (is left <= x <= (+ left width))
				  (is top <= y <= (+ top height))))
	
	(`(as-image)
	 (fill-image! image color)
	 image)

	(_ #false)))
    self))

(define (containing origin elements)
  (let ((hovered-element #false))
    (lambda message
      (let ((`(,left ,top) (origin 'position)))
	(match message	  
	  (`(mouse-down ,x ,y)
	   (if hovered-element
	       (hovered-element 'mouse-down (- x left) (- y top))
	       #false))

	  (`(mouse-up ,x ,y)
	   (when hovered-element
	     (hovered-element 'mouse-up (- x left) (- y top))))
	  
	  (`(mouse-move ,x ,y ,dx ,dy)
	   (let ((hovered (find (_ 'embraces? (- x left) (- y top))
				elements)))
	     (unless (eq? hovered hovered-element)
	       (when hovered-element
		 (hovered-element 'mouse-out))
	       (when hovered
		 (hovered 'mouse-over))
	       (set! hovered-element hovered))
	     (when hovered
	       (hovered 'mouse-move (- x left) (- y top) dx dy))
	     hovered-element))

	  (`(add! ,element)
	   (set! elements `(,element . ,elements)))
	  
	  (`(remove! ,element)
	   (set! elements (filter (lambda (_) (isnt _ eq? element)) elements)))
	  
	  (`(as-image)
	   (let ((image (origin 'as-image)))
	     (fold-right (lambda (element image)
			   (let ((`(,x ,y) (element 'position)))
			     (draw-image! (element 'as-image) x y image)
			     image))
			 image
			 elements)))
	  
	  (_
	   (apply origin message)))
	))))

(define (transferable origin)
  (let ((hovered-element #false)
	(dragged-element #false))
    (define (self . message)
      (match message
	(`(mouse-down ,x ,y)
	 (let ((acquired (self 'acquire-element!)))
	   (cond ((procedure? acquired)
		  (set! dragged-element acquired))
		 (else
		  (origin 'mouse-down x y)))
	   (self 'mouse-move x y 0 0 )))
	
	(`(mouse-up ,x ,y)
	 (when dragged-element
	   (self 'install-element! dragged-element)
	   (set! dragged-element #false))
	 (origin 'mouse-up x y)
	 (self 'mouse-move x y 0 0 ))

	(`(acquire-element!)
	 (and hovered-element
	      (or (let ((acquired (hovered-element 'acquire-element!)))
		    (and acquired
			 (begin 
			   (when (procedure? acquired)
			     (if (eq? acquired hovered-element)
				 (origin 'remove! acquired)
				 (let ((`(,x ,y) (hovered-element
						  'position)))
				   (acquired 'move-by! x y))))
			   acquired)))
		  (begin
		    (origin 'remove! hovered-element)
		    hovered-element))))
	
	(`(install-element! ,element)
	 (if hovered-element
	     (let ((`(,x ,y) (hovered-element 'position)))
	       (element 'move-by! (- x) (- y))
	       (hovered-element 'install-element! element))
	     (origin 'add! element)))

	(`(mouse-move ,x ,y ,dx ,dy)
	 (when dragged-element
	   (dragged-element 'move-by! dx dy))
	 (set! hovered-element (origin 'mouse-move x y dx dy))
	 hovered-element)

	(`(as-image)
	 (let ((image (origin 'as-image)))
	   (when dragged-element
	     (let ((`(,x ,y) (dragged-element 'position)))
	       (draw-image! (dragged-element 'as-image) x y image)))
	   image))
	
	(_
	 (apply origin message))))
    self))

(define-syntax-rule (fn (name . args) . body)
  (letrec ((name (lambda args . body)))
    (procedure-rename name 'name)))

(define (resizable origin)
  (let ((resizing #false)
	(resize! #false)
	(resizing-border-thickness 5))

    (define (resize-procedure x y)
      (let ((`(,left ,top ,right ,bottom) (origin 'extents)))
	(cond ((and (is left <= x <= (+ left resizing-border-thickness))
		    (is top <= y <= (+ top resizing-border-thickness)))
	       (fn (top-left-corner dx dy)
		   (origin 'resize-by! (- dx) (- dy))
		   (origin 'move-by! dx dy)))
	      
	      ((and (is (- right resizing-border-thickness) <= x <= right)
		    (is (- bottom resizing-border-thickness) <= y <= bottom))
	       (fn (bottom-right-corner dx dy)
		   (origin 'resize-by! dx dy)))
	      ((and (is left <= x <= (+ left resizing-border-thickness))
		    (is (- bottom resizing-border-thickness) <= y <= bottom))
	       (fn (bottom-left-corner dx dy)
		   (origin 'resize-by! (- dx) dy)
		   (origin 'move-by! dx 0)))

	      ((and (is (- right resizing-border-thickness) <= x <= right)
		    (is top <= y <= (+ top resizing-border-thickness)))
	       (fn (top-right-corner dx dy)
		   (origin 'resize-by! dx (- dy))
		   (origin 'move-by! 0 dy)))

	      ((is left <= x <= (+ left resizing-border-thickness))
	       (fn (left-side dx dy)
		   (origin 'resize-by! (- dx) 0)
		   (origin 'move-by! dx 0)))

	      ((is top <= y <= (+ top resizing-border-thickness))
	       (fn (upper-side dx dy)
		   (origin 'resize-by! 0 (- dy))
		   (origin 'move-by! 0 dy)))

	      ((is (- right resizing-border-thickness) <= x <= right)
	       (fn (right-side dx dy) (origin 'resize-by! dx 0)))

	      ((is (- bottom resizing-border-thickness) <= y <= bottom)
	       (fn (lower-side dx dy) (origin 'resize-by! 0 dy)))

	      (else
	       #false))))

    (define (self . message)
      (match message
	(`(mouse-move ,x ,y ,dx ,dy)
	 (cond (resizing
		(resize! dx dy))
	       (else
		(set! resize! (resize-procedure x y))
		(origin 'mouse-move x y dx dy))))
	
	(`(mouse-down ,x ,y)
	 (cond (resize!
		(set! resizing #true)
		#true)
	       (else
		(origin 'mouse-down x y))))

	(`(acquire-element!)
	 (if resize!
	     #true
	     (origin 'acquire-element!)))
	
	(`(mouse-up ,x ,y)
	 (if resizing
	     (set! resizing #false)
	     (origin 'mouse-up x y)))

	(`(mouse-out)
	 (set! resizing #false)
	 (origin 'mouse-out))
	
	(_
	 (apply origin message))))
    self))

(define (box #:left left #:top top
	     #:width width #:height height
	     #:background-color color . elements)
  (resizable
   (transferable
    (containing
     (bit #:left left #:top top
	  #:width width #:height height
	  #:background-color color)
     elements))))

(define stage
  (let ((`(,w ,h) (screen-size)))
    (transferable
     (containing
      (bit #:left 0 #:top 0
	   #:width w #:height h
	   #:background-color #x77000000)
      (list 
       (box #:left 10 #:top 10
	    #:width 200 #:height 200
	    #:background-color #x77cc00
	    (box #:left 10 #:top 10
		 #:width 50 #:height 50
		 #:background-color #x0077cc)
	    (box #:left 140 #:top 140
		 #:width 50 #:height 50
		 #:background-color #xcc0077))
       (box #:left (- w 210) #:top (- h 210)
	    #:width 200 #:height 200
	    #:background-color #x7700cc
	    (box #:left 140 #:top 140
		 #:width 50 #:height 50
		 #:background-color #xcc7700)))))))

(set-stage! stage)
