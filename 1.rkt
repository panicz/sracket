#lang racket
(require racket/match)
(require "sracket.rkt")
(require "ground-scheme.rkt")

(slayer-init #:title "GRASP 1: draggable rectangles on the screen")

(define (screen-size)
  `(640 480))

(keydn 'escape exit)

(define ((_ . message) _)
  (apply _ message))

(define stage
  (match-let* ((elements '())
	       (black 0)
	       (`(,width ,height) (screen-size))
	       (image (rectangle width height black))
	       (hovered-element #false))
    
    (lambda message
      (match message
	(`(as-image)
	 (fill-image! image black)
	 (fold-right (lambda (element image)
		       (match-let ((`(,x ,y) (element 'position)))
			 (draw-image! (element 'as-image) x y image)
			 image))
		     image
		     elements)
	 image)
	
	(`(resize! ,new-width ,new-height)
	 (set! width new-width)
	 (set! height new-height)
	 (set! image (rectangle width height 0)))
	
	(`(add! ,element)
	 (set! elements `(,element . ,elements)))

	(`(mouse-down ,x ,y)
	 (when hovered-element
	   (hovered-element 'mouse-down x y)))

	(`(mouse-up ,x ,y)
	 (when hovered-element
	   (hovered-element 'mouse-up x y)))

	(`(mouse-move ,x ,y ,dx ,dy)
	 (let ((hovered (find (_ 'embraces? x y) elements)))
	   (unless (eq? hovered hovered-element)
	     (when hovered-element
	       (hovered-element 'mouse-out))
	     (when hovered
	       (hovered 'mouse-over))
	     (set! hovered-element hovered))
	   (when hovered
	     (hovered 'mouse-move x y dx dy))))
	))))

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
	 'highlight?)
	(`(mouse-out)
	 (set! dragged? #false))
	))))

(set-display-procedure! (lambda () (draw-image! (stage 'as-image))))

(mousemove (lambda (x y dx dy) (stage 'mouse-move x y dx dy)))

(keydn 'mouse-left (lambda (x y)
		     (stage 'mouse-down x y)))
(keyup 'mouse-left (lambda (x y)
		     (stage 'mouse-up x y)))

(match-let ((`(,width ,height) (screen-size)))
  (for x in (range 10)
    (stage 'add! (draggable-rectangle (random (- width 50))
				      (random (- height 50))
				      50 50 (random #x1000000)))))
