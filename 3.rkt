#lang racket
(require racket/match)
(require "sracket.rkt")
(require "ground-scheme.rkt")

(slayer-init #:title "GRASP 3: draggable rectangles with draggable rectangles\
 that can be taken outside")

(define (screen-size)
  `(640 480))

(keydn 'escape exit)

(define ((_ . message) _)
  (apply _ message))

(define (set-stage! stage)
  (set-display-procedure! (lambda () (draw-image! (stage 'as-image))))
  (mousemove (lambda (x y dx dy) (stage 'mouse-move x y dx dy)))
  (keydn 'mouse-left (lambda (x y) (stage 'mouse-down x y)))
  (keyup 'mouse-left (lambda (x y) (stage 'mouse-up x y))))

(define (box #:left left #:top top #:width width #:height height
	     #:background-color color 
	     #:name [name "box"] . elements)
  (let ((dragged-element #false)
	(hovered-element #false)
	(image (rectangle width height color)))
    (define (self . message)
      (match message
	(`(position) `(,left ,top))

	(`(move-by! ,dx ,dy)
	 (set! left (+ left dx))
	 (set! top (+ top dy)))
	
	(`(mouse-down ,x ,y)
	 (let ((acquired (self 'acquire-hovered-element!)))
	   (when acquired
	     (set! dragged-element acquired))))
	   
	(`(mouse-up ,x ,y)
	 (when dragged-element
	   (self 'install-element! dragged-element)
	   (set! dragged-element #false))
	 (when hovered-element
	   (hovered-element 'mouse-up (- x left) (- y top))))

	(`(acquire-hovered-element!)
	 (and hovered-element
	      (or (let ((acquired (hovered-element 'acquire-hovered-element!)))
		    (and acquired
			 (match-let ((`(,x ,y) (hovered-element 'position)))
			   (acquired 'move-by! x y)
			   acquired)))
		  (let ((acquired hovered-element))
		    (set! hovered-element #false)
		    (set! elements (filter (lambda (_) (isnt _ eq? acquired))
					   elements))
		    acquired))))

	(`(install-element! ,element)
	 (if hovered-element
	     (match-let ((`(,x ,y) (hovered-element 'position)))
	       (element 'move-by! (- x) (- y))
	       (hovered-element 'install-element! element))
	     (set! elements `(,element . ,elements))))
	
	(`(mouse-move ,x ,y ,dx ,dy)
	 (when dragged-element
	   (dragged-element 'move-by! dx dy))
	 (let ((hovered (find (_ 'embraces? (- x left) (- y top))
			      elements)))
	   (unless (eq? hovered hovered-element)
	     (when hovered-element
	       (hovered-element 'mouse-out))
	     (when hovered
	       (hovered 'mouse-over))
	     (set! hovered-element hovered))
	   (when hovered
	     (hovered 'mouse-move (- x left) (- y top) dx dy))))
	
	(`(embraces? ,x ,y) (and (is left <= x <= (+ left width))
				 (is top <= y <= (+ top height))))
	(`(as-image)
	 (fill-image! image color)
	 (fold-right (lambda (element image)
		       (match-let ((`(,x ,y) (element 'position)))
			 (draw-image! (element 'as-image) x y image)
			 image))
		     image
		     `(,@(if dragged-element
			     `(,dragged-element)
			     '())
		       ,@elements)))
	
	(`(mouse-over)
	 (out "mouse-over "self))
	(`(mouse-out)
	 (out "mouse-out "self)))
      )
    (set! self (procedure-rename self name))
    self))

(define stage
  (match-let ((`(,w ,h) (screen-size)))
    (box #:left 0 #:top 0 #:width w #:height h
	 #:name 'stage #:background-color #x77000000
	 (box #:left 10 #:top 10 #:width 200 #:height 200
	      #:name 'upper #:background-color #x77cc00
	      (box #:left 10 #:top 10 #:width 50 #:height 50
		   #:name 'upper-inner #:background-color #x0077cc)
	      (box #:left 140 #:top 140 #:width 50 #:height 50
		   #:name 'undraggable #:background-color #xcc0077))
	 (box #:left (- w 210) #:top (- h 210) #:width 200 #:height 200
	      #:name 'lower #:background-color #x7700cc
	      (box #:left 140 #:top 140 #:width 50 #:height 50
		   #:name 'lower-inner #:background-color #xcc7700)))))

(set-stage! stage)
