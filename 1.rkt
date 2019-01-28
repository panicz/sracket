#lang racket
(require racket/match)
(require "sracket.rkt")
(require "grand-syntax.rkt")
(require "ground-scheme.rkt")

(slayer-init #:title "GRASP 1: draggable rectangles on the screen")

(keydn 'escape exit)

(define ((_ . message) _)
  (apply _ message))

(define (draggable-rectangle left top width height color)
  (let ((dragged? #false)
	(image (rectangle width height color)))
    (lambda message
      (match message
	(`(position)
	 `(,left ,top))
	(`(mouse-down ,x ,y)
	 (set! dragged? #true))
	(`(mouse-up ,x ,y)
	 (set! dragged? #false))
	(`(mouse-move ,x ,y ,dx ,dy)
	 (when dragged?
	   (set! left (+ left dx))
	   (set! top (+ top dy))))
	(`(embraces? ,x ,y)
	 (and (is left <= x <= (+ left width))
	      (is top <= y <= (+ top height))))
	(`(as-image)
	 image)
	(`(mouse-over)
	 #false)
	(`(mouse-out)
	 (set! dragged? #false))
	))))

(define stage
  (let* ((`(,width ,height) (screen-size))
         (elements (map (lambda (_)
	                  (draggable-rectangle
			   (random (- width 50))
			   (random (- height 50))
			   50 50 (random #x1000000)))
                        (range 10)))
	 (image (rectangle width height 0))
	 (hovered-element #false))
    
    (lambda message
      (match message
	(`(as-image)
	 (fill-image! image 0)
	 (fold-right (lambda (element image)
		       (let ((`(,x ,y) (element 'position)))
			 (draw-image! (element 'as-image) x y image)
			 image))
		     image
		     elements)
	 image)
	
	(`(mouse-down ,x ,y)
	 (when hovered-element
	   (hovered-element 'mouse-down x y)))

	(`(mouse-up ,x ,y)
	 (when hovered-element
	   (hovered-element 'mouse-up x y)))

	(`(mouse-move ,x ,y ,dx ,dy)
	 (let ((hovered (find (lambda (_) (_ 'embraces? x y)) elements)))
	   (unless (eq? hovered hovered-element)
	     (when hovered-element
	       (hovered-element 'mouse-out))
	     (when hovered
	       (hovered 'mouse-over))
	     (set! hovered-element hovered))
	   (when hovered
	     (hovered 'mouse-move x y dx dy))))
	))))


(set-display-procedure! (lambda () (draw-image! (stage 'as-image))))

(mousemove (lambda (x y dx dy) (stage 'mouse-move x y dx dy)))

(keydn 'mouse-left (lambda (x y)
		     (stage 'mouse-down x y)))
(keyup 'mouse-left (lambda (x y)
		     (stage 'mouse-up x y)))
