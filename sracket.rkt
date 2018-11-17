#lang racket/gui
(require racket/set)
(require "ground-scheme.rkt")

(provide slayer-init screen-size)
(provide set-display-procedure! draw-image! fill-image! rectangle load-image)

(provide width height)
(provide render-text current-font)
(provide keyup keydn mousemove)

(define (nothing . _)
  nothing)

(define current-drawing-context
  (make-parameter #f))

(define display-procedure nothing)

(define (set-display-procedure! proc)
  (set! display-procedure proc))

(define keydown-bindings (make-hasheq))
(define keyup-bindings (make-hasheq))

(define (keydn key proc)
  (hash-set! keydown-bindings key proc))

(define (keyup key proc)
  (hash-set! keyup-bindings key proc))

(define (key-press key . args)
  (apply (hash-ref keydown-bindings key
                   (lambda _
                     (out "keydn for "key" not defined")
                     nothing))
         args))

(define (key-release key . args)
  (apply (hash-ref keyup-bindings key
                   (lambda _
                     (out "keyup for "key" not defined")
                     nothing))
         args))

(define mouse-move nothing)

(define (mousemove proc)
  (set! mouse-move proc))

(define slayer-canvas%
  (class canvas%
    (inherit refresh)

    (define previous-x 0)
    (define previous-y 0)
    (define pressed-keys (mutable-set))
    
    (define/override (on-char keyboard-event)
      (let ((code (send keyboard-event get-key-code)))
        (if (eq? code 'release)
            (let ((code (send keyboard-event get-key-release-code)))
	      (set-remove! pressed-keys code)
              (key-release code))
	    (unless (set-member? pressed-keys code)
	      (key-press code)))
        (send this refresh)))
    
    (define/override (on-event mouse-event)
      (let ((x (send mouse-event get-x))
            (y (send mouse-event get-y))
            (type (send mouse-event get-event-type))
            (mouse-key-name (lambda (event-type)
                              (case event-type
                                ((left-up left-down) 'mouse-left)
                                ((middle-up middle-down) 'mouse-middle)
                                ((right-up right-down) 'mouse-right)))))
        (case type
          ((left-up middle-up right-up)
           (key-release (mouse-key-name type) x y))
          ((left-down middle-down right-down)
           (key-press (mouse-key-name type) x y))
          ((motion)
           (let ((dx (- x previous-x))
                 (dy (- y previous-y)))
             (mouse-move x y dx dy)
             (set! previous-x x)
             (set! previous-y y))))
        (send this refresh)))
    
    (super-new (paint-callback (lambda (canvas context)
                                 (parameterize ((current-drawing-context context))
                                   (display-procedure)))))))

(define slayer #false)

(define (slayer-init #:title [title ""] #:width [w 640] #:height [h 480])
  (unless slayer
    (let* ((frayer (new frame% [label title] [width w] [height h]))
	   (canvas (new slayer-canvas% [parent frayer])))
      (send frayer show #true)
      (set! slayer frayer))))

(define (screen-size)
  `(,(send slayer get-width) ,(send slayer get-height)))

(define/memoized (drawing-context object)
  (cond ((is-a? object bitmap%)
	 (new bitmap-dc% [bitmap object]))
	(else
	 (error "Unable to create drawing context for "object))))

(define (rgb color)
  (let ((blue (bitwise-and color #xff))
	(green (bitwise-and (arithmetic-shift color -8) #xff))
	(red (bitwise-and (arithmetic-shift color -16) #xff)))
    `(,red ,green ,blue)))

(define (rectangle width height [color #f])
  (let ((image (make-bitmap width height #;alpha #true)))
    (when color
      (match-let* ((`(,red ,green ,blue) (rgb color))
		   (color (make-object color% red green blue))
		   (dc (new bitmap-dc% [bitmap image])))
	(send dc set-background color)
	(send dc clear)))
    image))


(define (fill-image! image color)
  (let* ((blue (bitwise-and color #xff))
         (green (bitwise-and (arithmetic-shift color -8) #xff))
         (red (bitwise-and (arithmetic-shift color -16) #xff))
         (color (make-object color% red green blue))
         (dc (new bitmap-dc% [bitmap image])))
    (send dc set-background color)
    (send dc clear)
    image))

(define (draw-image! image [x 0] [y 0] [target (current-drawing-context)])
  (cond ((is-a? target dc<%>)
	 (send target draw-bitmap image x y))
	((is-a? target bitmap%)
	 (send (drawing-context target) draw-bitmap image x y))))

(define (load-image path)
  (read-bitmap path))

(define current-font (make-parameter (make-font)))

(define (render-text string
		     [font (current-font)]
		     [color #false]
		     [background-color #false])
  (let ((measure-context (new record-dc%)))
    (send measure-context set-font font)
    (let-values (((w h baseline-to-bottom extra-vertical-space)
		  (send measure-context get-text-extent string)))
      (match-let* ((`(,w ,h) (map (lambda (v)
				    (if (inexact? v)
					(inexact->exact
					 (ceiling v))
					v))
				  `(,w ,h)))
		   (background (rectangle w h #;background-color))
		   (dc (drawing-context background)))
	(cond (background-color
	       (match-let* ((`(,r ,g ,b) (rgb background-color))
			    (bg-color (make-object color% r g b)))
		 (send dc set-text-mode 'solid)
		 (send dc set-text-background bg-color)))
	      (else
	       (send dc set-text-mode 'transparent)))
	(send dc set-font font)
	(when color
	  (match-let* ((`(,r ,g ,b)  (rgb color))
		       (color (make-object color% r g b)))
	    (send dc set-text-foreground color)))
	(send dc draw-text string 0 0)
	background))))

(define (width image)
  (send image get-width))

(define (height image)
  (send image get-height))
