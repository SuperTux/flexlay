;;(display "sprite: ") (display sprite) (newline)

(define-class <scmdog> ()
  (pos    #:init-value '(0 . 0)
	  #:accessor   pos
	  #:init-keyword #:pos)
  (center #:init-value '(100 . 800)
	  #:accessor center
	  #:init-keyword #:center)
  (offset #:init-value #f
	  #:accessor offset
	  #:init-keyword #:offset)
  (sprite #:init-value #f
	  #:accessor sprite
	  #:init-keyword #:sprite))

(define (scmdog:create)
  (let ((data (make <scmdog>
		#:offset (random 3.141)
		#:center (cons (random 300) (+ 700 (random 300)))
		#:sprite (sprite:create (vector-ref #("lights/light1"
						      "lights/light2"
						      "lights/light3") (random 3)))
					)))
    (println "Center: " (pos data))
    data))

(define (scmdog:update obj delta)
  (let ((time (* 5 (gameworld:get-time (gameobj:get-world obj))))
	(data (gameobj:get-data obj)))

    (set! (pos data)
	  (cons (+ (car (center data)) (* 100 (sin (+ time (offset data)))))
		(+ (cdr (center data)) (* 100 (cos (+ time (offset data)))))))))

(define (scmdog:draw obj)  
  (let ((data (gameobj:get-data obj)))
    ;;(println "Data: " data)
    ;;(println "Position: " (pos data))
    (sprite:draw (sprite data) (car (pos data)) (cdr (pos data)))))

(gameobj-factory:register "scmdog" (vector scmdog:create
					   scmdog:update
					   scmdog:draw))

;; EOF ;;