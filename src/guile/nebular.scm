(define-class <nebular> ()
  (pos    #:init-value '(0 . 800)
	  #:accessor   pos
	  #:init-keyword #:pos))

(define nebular:sprite (sprite:create "nebular"))

(define (nebular:create)
  (make <nebular>))

(define (nebular:update obj delta)
  (let ((data (gameobj:get-data obj)))
    (set! (pos data) (cons (+ (car (pos data)) (* 100 delta))
			   (cdr (pos data))))))

(define (nebular:draw obj)
  #f)
;;  (let ((data (gameobj:get-data obj)))))

(gameobj-factory:register "nebular" (vector nebular:create
					    nebular:update
					    nebular:draw))

;; EOF ;;