;; Dragon ;;

(define turr-rec (make-record-type "turrican" '(pos update draw)))

(define-class <vector2d>
  ...)

(define-class <dragon>
  (pos <vector2d>))

(define (create <dragon> x-pos y-pos)
  (make <dragon> #:pos (vector2d x-pos y-pos)))

(define (update (dragon <dragon>) (delta <float>))
  (println "Updating dragon: " delta))

(define (draw (dragon <dragon>))
  ???
  (draw view (sprite dragon) pos))

(define my-sprite (sprite:create bla #:set))



(register-gameobj "dragon" <dragon>
		  #:update dragon:update
		  #:draw   dragon:draw)

;; EOF ;;