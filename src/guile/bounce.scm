(define-class <bounce> ()
  (pos    #:init-value '(0 . 0)
	  #:accessor   pos
	  #:init-keyword #:pos)
  (add    #:init-value (cons 100.0 100.0)
	  #:accessor add
	  #:init-keyword #:add))

(define bounce:sprite (sprite:create "shoot/bounce"))

(define (bounce:create)
  (make <bounce> #:pos (cons (random 1000) (random 1000))))

(define (bounce:update obj delta)
  (let* ((data (gameobj:get-data obj))
	 (tmppos (pos data)))
    (set! (pos data) (cons (+ (car (pos data)) (* (car (add data)) delta))
			   (+ (cdr (pos data)) (* (cdr (add data)) delta))))

    (cond ((bounce:on-ground obj)
	   (set! (pos data) tmppos)
	   (set! (add data) (bounce:reverse (add data)))))))

(define (bounce:reverse data)
  (cons (- (car data))
	(- (cdr data))))

(define (bounce:on-ground obj)
  (let ((position (pos (gameobj:get-data obj))))
    (tilemap:is-ground (gameworld:get-tilemap (gameobj:get-world obj))
		       (car position) (cdr position))))

(define (bounce:draw obj)
  (let ((data (gameobj:get-data obj)))
    ;;(println "Position: " (pos data))
    (sprite:draw bounce:sprite 
		 (car (pos data))
		 (cdr (pos data)))
    ))

(gameobj-factory:register "bounce" (vector bounce:create
					   bounce:update
					   bounce:draw))

;; EOF ;;