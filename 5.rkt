#!/usr/bin/env racket
#lang racket
(require racket/match)
(require (only-in srfi/1 delete!))
(require "sracket.rkt")
(require "ground-scheme.rkt")
(require "grand-syntax.rkt")

(slayer-init #:title "GRASP LIMB")

(keydn 'escape exit)

(define (my self)
  (and self `(,(self 'class) ,(self 'as-expression))))

(define ((% . message) %)
  (apply % message))

(define (thing #:name [name 'thing])
  (define (self . message)
    (match message
      (`(name)
       name)
      (`(class)
       'thing)
      (_
       #false)))
  self)

(define (sprite #:image image
		#:name [name 'sprite]
		#:target [target (thing #:name name)])
  (define (self . message)
    (match message
      (`(class) 'sprite)
      (`(as-image) image)
      (`(size) (image-size image))
      (`(embraces? ,x ,y)
       (let ((`(,w ,h) (image-size image)))
	 (and (is 0 <= x <= w)
	      (is 0 <= y <= h))))
      (_
       (apply target message))))
  self)

(define (caption atomic-expression)
  (let* ((text (->string atomic-expression))
	 (name (string->symbol text))
	 (target (sprite #:image (render-text text)
			 #:name name)))
    (define (self . message)
      (match message
	(`(class) 'caption)
	(`(as-expression) atomic-expression)
	(_
	 (apply target message))))
  self))

(define (situated target
		  #:left [left 0]
		  #:top [top 0])
  (define (self . message)
    (match message
      (`(class) 'situated)
      (`(position) `(,left ,top))
      (`(move-by! ,x ,y)
       (set! left (+ left x))
       (set! top (+ top y)))
      (`(move-to! ,x ,y)
       (set! left x)
       (set! top y))
      (`(mouse-down)
       (and-let* ((`(,object ,action) (target 'mouse-down)))
	 (object 'move-by! (- left) (- top))
	 `(,object ,action)))
      
      (`(mouse-move ,x ,y ,dx ,dy)
       (target 'mouse-move (- x left) (- y top) dx dy))
      (`(embraces? ,x ,y)
       (target 'embraces? (- x left) (- y top)))
      (`(element-at ,x ,y)
       (target 'element-at (- x left) (- y top)))
      (`(situated?)
       #true)
      (_
       (apply target message))))
  self)

(define (overlay-images objects background)
  (fold-left (lambda (background object)
	       (let ((`(,x ,y) (object 'position))
		     (image (object 'as-image)))
		 (draw-image! image x y background)
		 background))
	     background
	     objects))

(define (total-area objects)
  (fold-left (lambda (`(,X ,Y) element)
	       (let ((`(,x ,y) (element 'position))
		     (`(,w ,h) (element 'size)))
		 `(,(max X (+ x w)) ,(max Y (+ y h)))))
	     '(0 0)
	     objects))

(define (before? lag1 lag2)
  (let ((`(,left1 ,top1) (lag1 'position))
	(`(,width1 ,height1) (lag1 'size))
	(`(,left2 ,top2) (lag2 'position))
	(`(,width2 ,height2) (lag2 'size)))
    (or (is (+ top1 height1) < top2)
	(and (is top1 <= top2 <= (+ top1 height1))
	     (is (+ left1 width1) < left2)))))

(define (transparent width height)
  (make-bitmap width height #true))

(define (collection elements #:name [name 'collection])
  (define (self . message)
    (match message
      (`(class) 'collection)
      (`(size) (total-area elements))

      (`(as-image)
       (let* ((`(,w ,h) (total-area elements))
	      (background (transparent w h)))
	 (overlay-images elements background)))

      (`(as-expression)
       (map (% 'as-expression) elements))
      
      (`(embraces? ,x ,y)
       (find (% 'embraces? x y) elements))

      (`(element-at ,x ,y)
       (find (% 'embraces? x y) elements))
      
      (`(add-elements! . ,new-elements)
       (set! elements (merge new-elements elements before?))
       (out "after addition: "(self 'as-expression)))
      
      (`(remove-element! ,element)
       (set! elements (delete! element elements))
       (out "after removal: "(self 'as-expression)))

      (`(collective ,modification!)
       (modification! elements))
      
      (`(collection?)
       #true)

      (`(name)
       name)
      
      (_
       #false)))
  self)

(define (hovering collection)
  (let ((hovered-element #false))
    (define (self . message)
      (match message
	(`(class) 'hovering)
	
	(`(mouse-move ,x ,y ,dx ,dy)
	 (let ((hovered (collection 'element-at x y)))
	   (unless (eq? hovered hovered-element)
	     (when hovered-element
	       (hovered-element 'mouse-out))
	     (when hovered
	       (hovered 'mouse-over))
	     (set! hovered-element hovered))
	   (when hovered-element
	     (hovered-element 'mouse-move x y dx dy))
	   hovered-element))
	
	(`(add-elements! . ,elements)
	 (if (and hovered-element (hovered-element 'collection?))
	     (let ((`(,x ,y) (hovered-element 'position)))
	       (for-each (% 'move-by! (- x) (- y)) elements)
	       (apply hovered-element 'add-elements! elements))
	     (apply collection 'add-elements! elements)))

	(`(remove-element! ,element)
	 (when (eq? element hovered-element)
	   (set! hovered-element #false))
	 (collection 'remove-element! element))

	(`(acquire-element!)
	 (and hovered-element
	      (or (let ((acquired (hovered-element 'acquire-element!)))
		    (and acquired
			 (begin 
			   (if (eq? acquired hovered-element)
			       (self 'remove-element! acquired)
			       (let ((`(,x ,y) (hovered-element 'position)))
				 (acquired 'move-by! x y)))
			   acquired)))
		  (let ((element hovered-element))
		    (self 'remove-element! hovered-element)
		    element))))

	(_
	 (apply collection message))))
    self))

(define (obscurable acquirable #:width width #:height height)
  (let ((obscuring #false)
	(on-drag #false)
	(background (rectangle width height)))
    (define (self . message)
      (match message
	(`(class) 'obscurable)
	(`(as-image)
	 (clear-image! background)
	 (draw-image! (acquirable 'as-image) 0 0 background)
	 (when obscuring
	   (let ((`(,x ,y) (obscuring 'position)))
	     (draw-image! (obscuring 'as-image) x y background)))
	 background)

	(`(mouse-move ,x ,y ,dx ,dy)
	 (when (and obscuring on-drag)
	   (on-drag obscuring x y dx dy))
	 (acquirable 'mouse-move x y dx dy))

	(`(mouse-down)
	 (let ((element (acquirable 'acquire-element!)))
	   (when element
	     (set! obscuring element)
	     (let ((action (element 'obscuring-action)))
	       (set! on-drag action)))))

	(`(mouse-up)
	 (and-let* ((formerly obscuring))
	   (acquirable 'add-elements! obscuring)
	   (set! obscuring #false)
	   (set! on-drag #false)
	   formerly))
	(_
	 (apply acquirable message))))
    self))

(define (draw-parentheses! image)
  (let ((`(,width ,height) (image-size image))
	(X 3))
    (line-between! 0 0 X 0 image)
    (line-between! 0 0 0 height image)
    (line-between! 0 (- height 1) X (- height 1) image)
    
    (line-between! (- width 1) 0 (- width X) 0 image)
    (line-between! (- width 1) 0 (- width 1) (- height 1) image)
    (line-between! (- width 1) (- height 1) (- width X) (- height 1) image)))

(define (parenthesized target
		       #:width [width #false]
		       #:height [height #false]
		       #:name [name 'parenthesized])
  (let* ((`(,target-width ,target-height) (target 'size))
	 (width (or width target-width))
	 (height (or height target-height))
	 (background (rectangle width height)))
    (define (self . message)
      (match message
	(`(class) 'parenthesized)
	(`(as-image)
	 (clear-image! background)
	 (draw-parentheses! background)
	 (draw-image! (target 'as-image) 0 0 background)
	 background)

	(`(size)
	 `(,width ,height))

	(`(embraces? ,x ,y)
	 (and (is 0 <= x <= width)
	      (is 0 <= y <= height)))

	(`(resize-to! ,w ,h)
	 (set! width w)
	 (set! height h)
	 (set! background (rectangle width height)))

	(`(resize-by! ,dx ,dy)
	 (set! width (+ width dx))
	 (set! height (+ height dy))
	 (set! background (rectangle width height)))

	(_
	 (apply target message))))
    self))

(define (highlighting target)
  (let ((highlight? #false))
    (define (self . message)
      (match message
	(`(class) 'highlighting)
	(`(mouse-over)
	 (set! highlight? #true)
	 (target 'mouse-over))
	(`(mouse-out)
	 (set! highlight? #false)
	 (target 'mouse-out))
	(`(as-image)
	 (let* ((image (target 'as-image))
		(`(,w ,h) (image-size image)))
	   (when highlight?
	     (line-between! 0 0 w 0 image)
	     (line-between! 0 (- h 1) w (- h 1) image))
	   image))
	(_
	 (apply target message))))
    self))
  
(define (mouse-tracking target)
  (let ((mouse-left 0)
	(mouse-top 0))
    (define (self . message)
      (match message
	(`(class) 'mouse-tracking)
	(`(mouse-move ,x ,y ,dx ,dy)
	 (set! mouse-left x)
	 (set! mouse-top y)
	 (target 'mouse-move x y dx dy))

	(`(mouse-position)
	 `(,mouse-left ,mouse-top))

	(`(mouse-down)
	 (target 'mouse-down))
	(_
	 (apply target message))))
    self))

(define (drag object x y dx dy)
  (object 'move-by! dx dy))

(define (draggable mouse-tracking-collection)
  (define (self . message)
    (match message
      (`(obscuring-action)
       drag)
      (_
       (apply mouse-tracking-collection message))))
  self)

(define (stretchable target #:margin (margin 5))
  (define (self . message)
    (match message
      (`(class) 'stretchable)
      (`(obscuring-action)
       (let ((`(,left ,top) (target 'position))
	     (`(,width ,height) (target 'size))
	     (`(,x ,y) (target 'mouse-position)))
	 
	 (cond ((is x <= margin)
		(if (is y < (/ height 2))
		    (lambda (self x y dx dy)
		      (self 'move-by! dx dy)
		      (self 'resize-by! (- dx) (- dy)))
		    (lambda (self x y dx dy)
		      (self 'move-by! dx 0)
		      (self 'resize-by! (- dx) dy))))
	       ((is (- width margin) <= x)
		(if (is y < (/ height 2))
		    (lambda (self x y dx dy)
		      (self 'move-by! 0 dy)
		      (self 'resize-by! dx (- dy)))
		    (lambda (self x y dx dy)
		      (self 'resize-by! dx dy))))
	       (else
		(target 'obscuring-action)))))
      (_
       (apply target message))))
  self)


(define (set-stage! stage)
  (set-display-procedure! (lambda () (draw-image! (stage 'as-image))))
  (mousemove (lambda (x y dx dy) (stage 'mouse-move x y dx dy)))
  (keydn 'mouse-left (lambda _ (stage 'mouse-down)))
  (keyup 'mouse-left (lambda _ (stage 'mouse-up))))

(define vertical-space (make-parameter 5))

(define horizontal-space (make-parameter 10))

(define (horizontal-layout! elements)
  (fold-left (lambda (`(,x ,y) element)
	       (let ((`(,w ,h) (element 'size)))
		 (element 'move-to! x y)
		 `(,(+ x w (horizontal-space)) ,y)))
	     `(,(horizontal-space) ,(vertical-space))
	     elements))

(define (vertical-layout! elements)
  (fold-left (lambda (`(,x ,y) element)
	       (let ((`(,w ,h) (element 'size)))
		 (element 'move-to! x y)
		 `(,x ,(+ y h (vertical-space)))))
	     `(,(horizontal-space) ,(vertical-space))
	     elements))

(define (box bitboxes)
  (let ((collection (collection bitboxes)))
    (collection 'collective horizontal-layout!)
    (let ((`(,w ,h) (collection 'size)))
      (stretchable
       (draggable
	(situated
	 (mouse-tracking
	  (hovering
	   (highlighting
	    (parenthesized
	     collection
	     #:width (+ w (horizontal-space))
	     #:height (+ h (vertical-space))))))))))))

(define (bit atom)
  (draggable (mouse-tracking (situated (caption atom)))))

(define (bitbox document)
  (if (list? document)
      (box (map bitbox document))
      (bit document)))

(define (workdesk initial-document #:width width #:height height)
  (let* ((desk (obscurable
		(hovering
		 (collection (map bitbox initial-document)))
		#:width width #:height height)))
    (desk 'collective vertical-layout!)
    desk))

(define desk
  (let ((`(,w ,h) (screen-size)))
    (workdesk '(x
		(f x)
		(f (f x))
		(define (! n)
		  (if (= n 0)
		      1
		      (* n (! (- n 1))))))
	      #:width w
	      #:height h)))

(set-stage! desk)
