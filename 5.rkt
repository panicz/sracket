#!/usr/bin/env racket
#lang racket
(require racket/match)
(require (only-in srfi/1 delete!))
(require racket/draw/arrow)
(require "sracket.rkt")
(require "ground-scheme.rkt")
(require "grand-syntax.rkt")

(slayer-init #:title "GRASP LIMB")

(keydn 'escape exit)

(define (my self)
  (and self `(,(self 'class) ,(self 'as-expression))))

(define ((% . message) %)
  (apply % message))

(define (??? . _) ???)

#;(define# (instance? x)
  #false)

(define (instance? x)
  (and (procedure? x)
       (isnt x eq? ???)))

(define (Sprite #:image image)
   (lambda message
     (match message
       (`(class) 'Sprite)
       (`(as-image) image)
       (`(size) (image-size image))
       (`(embraces? ,x ,y)
	(let ((`(,w ,h) (image-size image)))
	  (and (is 0 <= x <= w)
	       (is 0 <= y <= h))))
       (_
	#false))))

(define (Caption atomic-expression)
  (let* ((text (->string atomic-expression))
	 (name (string->symbol text))
	 (target (Sprite #:image (render-text text))))
    (lambda message
      (match message
	(`(class) 'Caption)
	(`(as-expression) atomic-expression)
	(_
	 (apply target message))))))

(define (Situated target #:left [left 0] #:top [top 0])
  (lambda message
    (match message
      (`(class) 'Situated)
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
       (apply target message)))))

(define (overlay-images objects background)
  (fold-left (lambda (background object)
	       (let ((`(,x ,y) (object 'position))
		     (image (object 'as-image)))
		 (draw-image! image x y background)
		 background))
	     background
	     objects))

(define (exact number)
  (if (inexact? number)
      (inexact->exact number)
      number))

(define (total-area objects)
  (map (compose exact ceiling)
       (fold-left (lambda (`(,X ,Y) element)
		    (let ((`(,x ,y) (element 'position))
			  (`(,w ,h) (element 'size)))
		      `(,(max X (+ x w)) ,(max Y (+ y h)))))
		  '(0 0)
		  objects)))

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

(define (Collection elements #:name [name 'collection])
  (define (self . message)
    (match message
      (`(class) 'Collection)
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

(define (Hovering collection)
  (let ((hovered-element #false))
    (define (self . message)
      (match message
	(`(class) 'Hovering)
	
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
			       (when (instance? acquired)
				 (let ((`(,x ,y) (hovered-element 'position)))
				   (acquired 'move-by! x y))))
			   acquired)))
		  (let ((element hovered-element))
		    (self 'remove-element! hovered-element)
		    element))))

	(_
	 (apply collection message))))
    self))

(define (Obscurable acquirable #:width width #:height height)
  (let ((obscuring #false)
	(on-drag #false)
	(background (rectangle width height)))
    (lambda message
      (match message
	(`(class) 'Obscurable)
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
	     (if (instance? element)
		 (let ((action (element 'obscuring-action)))
		   (set! on-drag action))
		 (acquirable 'mouse-down)))))

	(`(mouse-up)
	 (and-let* ((formerly obscuring))
	   (when (instance? obscuring)
	     (acquirable 'add-elements! obscuring))
	   (set! obscuring #false)
	   (set! on-drag #false)
	   formerly))
	(_
	 (apply acquirable message))))))

(define (draw-parentheses! image width height)
  (let ((X 3))
    (line-between! 0 0 X 0 image)
    (line-between! 0 0 0 height image)
    (line-between! 0 (- height 1) X (- height 1) image)
    
    (line-between! (- width 1) 0 (- width X) 0 image)
    (line-between! (- width 1) 0 (- width 1) (- height 1) image)
    (line-between! (- width 1) (- height 1) (- width X) (- height 1) image)))

(define (draw-softbox! image w h)
  (draw-ellipsis! 0 0 w h image))

(define (Decorated target
		   #:width [width #false]
		   #:height [height #false]
		   #:left [left 0]
		   #:top [top 0]
		   #:decoration [decorate! (lambda _ #false)])
  (let* ((`(,target-width ,target-height) (target 'size))
	 (width (or width (+ left target-width)))
	 (height (or height (+ top target-height)))
	 (background (rectangle width height)))
    (lambda message
      (match message
	(`(class) 'Decorated)
	(`(as-image)
	 (clear-image! background)
	 (decorate! background width height)
	 (draw-image! (target 'as-image) left top background)
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
	 (apply target message))))))

(define (parallel-lines! image w h)
  (line-between! 0 0 w 0 image)
  (line-between! 0 (- h 1) w (- h 1) image))

(define (Highlighting target #:decoration [highlight! parallel-lines!])
  (let ((highlight? #false))
    (lambda message
      (match message
	(`(class) 'Highlighting)
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
	     (highlight! image w h))
	   image))
	(_
	 (apply target message))))))
  
(define (MouseTracking target)
  (let ((mouse-left 0)
	(mouse-top 0))
    (lambda message
      (match message
	(`(class) 'MouseTracking)
	(`(mouse-move ,x ,y ,dx ,dy)
	 (set! mouse-left x)
	 (set! mouse-top y)
	 (target 'mouse-move x y dx dy))

	(`(mouse-position)
	 `(,mouse-left ,mouse-top))

	(`(mouse-down)
	 (target 'mouse-down))
	(_
	 (apply target message))))))

(define (drag object x y dx dy)
  (object 'move-by! dx dy))

(define (Draggable mouse-tracking-collection)
  (lambda message
    (match message
      (`(obscuring-action)
       drag)
      ('(class)
       'Draggable)
      (_
       (apply mouse-tracking-collection message)))))

(define (Stretchable target #:margin (margin 5))
  (lambda message
    (match message
      (`(class) 'Stretchable)
      (`(obscuring-action)
       (let ((`(,left ,top) (target 'position))
	     (`(,width ,height) (target 'size))
	     (`(,x ,y) (target 'mouse-position)))	 
	 (cond ((is x <= margin)
		(if (is y < (/ height 2))
		    (lambda (self x y dx dy)
		      (self 'move-by! dx dy)
		      (target 'collective
			      (lambda (items)
				(for-each (% 'move-by! (- dx) (- dy)) items)))
		      (self 'resize-by! (- dx) (- dy)))
		    (lambda (self x y dx dy)
		      (self 'move-by! dx 0)
		      (target 'collective
			      (lambda (items)
				(for-each (% 'move-by! (- dx) 0) items)))
		      (self 'resize-by! (- dx) dy))))
	       ((is (- width margin) <= x)
		(if (is y < (/ height 2))
		    (lambda (self x y dx dy)
		      (self 'move-by! 0 dy)
		      (target 'collective
			      (lambda (items)
				(for-each (% 'move-by! 0 (- dy)) items)))
		      (self 'resize-by! dx (- dy)))
		    (lambda (self x y dx dy)
		      (self 'resize-by! dx dy))))
	       (else
		(target 'obscuring-action)))))
      (_
       (apply target message)))))


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

(define (Box bitboxes)
  (let ((collection (Collection bitboxes)))
    (collection 'collective horizontal-layout!)
    (let ((`(,w ,h) (collection 'size)))
      (Stretchable
       (Draggable
	(Situated
	 (MouseTracking
	  (Hovering
	   (Highlighting
	    (Decorated
	     collection
	     #:width (+ w (horizontal-space))
	     #:height (+ h (vertical-space))
	     #:decoration draw-parentheses!))))))))))

(define (Bit atom)
  (let* ((caption (Caption atom))
	 (`(,w ,h) (caption 'size)))
    (Draggable
     (Situated
      (MouseTracking
       (Highlighting
	(Decorated caption
		   #:left 3 #:top 3
		   #:width (+ w 6) #:height (+ h 6))
	#:decoration draw-softbox!))))))

(define (BitBox document)
  (if (list? document)
      (Box (map BitBox document))
      (Bit document)))

(define# (interactions symbol)
  #false)

(define-syntax (define-interaction (name . args) interaction)
  (set! (interactions 'name) (lambda args interaction)))

(define (BitBox+ document)
  (cond ((and-let* ((`(,keyword . ,args) document)
		    (interaction (interactions keyword)))
	   (apply interaction args)))
	((list? document)
	 (Box (map BitBox+ document)))
	(else
	 (Bit document))))

(define (points-on-circle number)
  (let* ((2pi (* 8 (atan 1)))
	 (slice (/ 2pi (exact->inexact number))))
    (map (lambda (k)
	   (let ((fraction (* slice k)))
	     `(,(cos fraction) ,(sin fraction))))
	 (range 0 number))))

(define (lay-out-in-circle! objects #:radius [radius #false])
  (let* ((n (length objects))
	 (r (or radius
		(* 2 (apply max (map (lambda (object)
				       (apply max (object 'size)))
				     objects)))))
	 (points (points-on-circle n)))
    (for-each (lambda (object `(,x ,y))
		(object 'move-by! (+ (* x r) r) (+ (* y r) r)))
	      objects points)))

(define (Vertex name #:surplus [surplus 3] #:radius [radius #false])
  (let* ((caption (Caption name))
	 (`(,w ,h) (caption 'size))
	 (radius (or radius (/ (+ (sqrt (+ (* w w) (* h h))) surplus) 2)))
	 (diameter (inexact->exact (ceiling (* 2 radius))))
	 (left (inexact->exact (ceiling (- radius (/ w 2)))))
	 (top (inexact->exact (ceiling (- radius (/ h 2))))))
    (Draggable
     (Situated
      (MouseTracking
       (Decorated
	(lambda message
	  (match message
	    (`(radius)
	     radius)
	    (_
	     (apply caption message))))
	#:left left #:top top
	#:width diameter #:height diameter
	#:decoration draw-softbox!))))))

(define (draw-edge! v1 v2 image)
  (let* ((r1 (v1 'radius))
	 (p1 (v1 'position))
	 (r2 (v2 'radius))
	 (p2 (v2 'position))
	 (d (map - p2 p1))
	 (/d (/ (sqrt (apply + (map (lambda (x) (* x x)) d)))))
	 (`(,p1x ,p1y) (map (lambda (a b)
			      (inexact->exact (ceiling (+ a (* r1 b /d)))))
			    p1 d))
	 (`(,p2x ,p2y) (map (lambda (a b)
			      (inexact->exact (ceiling (- a (* r2 b /d)))))
			    p2 d)))
    (draw-arrow (drawing-context image)
		p1x p1y p2x p2y 15 15)))

(define (Graph neighbour-list)
  (let ((vertices (map (lambda (`(,node . ,neigbours))
			 (Vertex node))
		       neighbour-list)))
    (lay-out-in-circle! vertices)
    (let* ((collection (Collection vertices))
	   (`(,w ,h) (collection 'size))
	   (contents (Draggable
		      (Situated
		       (MouseTracking
			(Hovering
			 (Decorated collection
				    #:left 3 #:top 3
				    #:width (+ w 6) #:height (+ h 6)
				    #:decoration draw-parentheses!)))))))
      (lambda message
	(match message
	  (`(as-image)
	   (let ((image (contents 'as-image)))
	     (for `(,source (,node . ,neighbours)) in (zip vertices
							   neighbour-list)
	       (for neighbour in neighbours
		 (let ((target (find (lambda (v)
				       (eq? neighbour (v 'as-expression)))
				     vertices)))
		   (draw-edge! source target image))))
	     image))
	  (`(as-expression)
	   `(digraph . ,neighbour-list))
	  (`(acquire-element!)
	   #false)
	  (_
	   (apply contents message)))))))
	
(define-interaction (digraph . neighbour-list)
  (Graph neighbour-list))

(define (Workdesk initial-document #:width width #:height height)
  (let* ((desk (Obscurable
		(Hovering
		 (Collection (map BitBox+ initial-document)))
		#:width width #:height height)))
    (desk 'collective vertical-layout!)
    desk))

(define desk
  (let ((`(,w ,h) (screen-size)))
    (Workdesk '(x
		(f x)
		(f (f x))
		(define (! n)
		  (if (= n 0)
		      1
		      (* n (! (- n 1)))))
		(e.g. (! 5) ===> 120)
		(digraph
		 (A B C D)
		 (B A C)
		 (C B)
		 (D A)
		 (E)))
	      #:width w
	      #:height h)))

(set-stage! desk)
