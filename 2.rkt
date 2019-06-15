#lang racket
(require racket/match)
(require "sracket.rkt")
(require "grand-syntax.rkt")
(require "ground-scheme.rkt")

(slayer-init
 #:title "GRASP 2: draggable rectangles with draggable rectangles inside")

(keydn 'escape exit)

(define ((_ . message) _)
  (apply _ message))

(define (set-stage! stage)
  (set-display-procedure! (lambda () (draw-image! (stage 'as-image))))
  (mousemove (lambda (x y dx dy) (stage 'mouse-move x y dx dy)))
  (keydn 'mouse-left (lambda (x y) (stage 'mouse-down x y)))
  (keyup 'mouse-left (lambda (x y) (stage 'mouse-up x y))))

(define (box #:left left #:top top #:width width #:height height
	     #:background-color color #:draggable? [draggable? #true]
	     . elements)
  (let ((dragged? #false)
	(hovered-element #false)
	(image (rectangle width height color)))
    (lambda message
      (match message
	(`(position) `(,left ,top))

	(`(mouse-down ,x ,y)
	 (if hovered-element
	     (hovered-element 'mouse-down (- x left) (- y top))
	     (when draggable?
	       (set! dragged? #true))))

	(`(mouse-up ,x ,y)
	 (when hovered-element
	   (hovered-element 'mouse-up (- x left) (- y top)))
	 (set! dragged? #false))

	(`(mouse-move ,x ,y ,dx ,dy)
	 (cond (dragged?
		(set! left (+ left dx))
		(set! top (+ top dy)))
	       (else
		(let ((hovered (find (_ 'embraces? (- x left) (- y top))
				     elements)))
		  (unless (eq? hovered hovered-element)
		    (when hovered-element
		      (hovered-element 'mouse-out))
		    (when hovered
		      (hovered 'mouse-over))
		    (set! hovered-element hovered))
		  (when hovered
		    (hovered 'mouse-move (- x left) (- y top) dx dy))))))

	(`(embraces? ,x ,y) (and (is left <= x <= (+ left width))
				 (is top <= y <= (+ top height))))
	(`(as-image)
	 (fill-image! image color)
	 (fold-right (lambda (element image)
		       (let ((`(,x ,y) (element 'position)))
			 (draw-image! (element 'as-image) x y image)
			 image))
		     image
		     elements))
	(`(mouse-over) #false)
	(`(mouse-out) (set! dragged? #false))))))

(define stage
  (let ((`(,w ,h) (screen-size)))
    (box #:left 0 #:top 0 #:width w #:height h
	 #:background-color #x77000000 #:draggable? #false
	 (box #:left 10 #:top 10 #:width 200 #:height 200
	      #:background-color #x77cc00
	      (box #:left 10 #:top 10 #:width 50 #:height 50
		   #:background-color #x0077cc
		   (box #:left 5 #:top 5 #:width 20 #:height 20
			#:background-color #xff0000))
	      (box #:left 140 #:top 140 #:width 50 #:height 50
		   #:background-color #xcc0077
		   #:draggable? #false))
	 (box #:left (- w 210) #:top (- h 210) #:width 200 #:height 200
	      #:background-color #x7700cc
	      (box #:left 140 #:top 140 #:width 50 #:height 50
		   #:background-color #xcc7700)))))

(set-stage! stage)
