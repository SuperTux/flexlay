
(load "helper.scm")

(define (*key-down-handler* key)
  ;;(display "Keydown: ")
  ;;(display key)
  ;;(newline)
  (cond ((string=? key "i")
         (game-add-igel (+ (player-get-x) 0)
                        (- (player-get-y) 300)))
        ((string=? key "s")
         (windstille:repl))
        ((string=? key "escape")
         (game-quit))
        ((string=? key "p")
         (toggle-pause))
        
        ((string=? key "d")
         (dialog-show))

        ((string=? key "1")
         (set-game-speed 1.0))

        ((string=? key "2")
         (set-game-speed 2.0))

        ((string=? key "3")
         (set-game-speed .5))

        ((string=? key "4")
         (set-game-speed .25))

        ((string=? key "5")
         (set-game-speed .1))

        ((string=? key "h")
         (dialog-hide))
        ))

(define (*mouse-up-handler* x y)
  #f)

(define (*mouse-down-handler* x y)
  (display "(game-add-igel ")
  (display (inexact->exact x))
  (display " ")
  (display (inexact->exact y))
  (display ")")
  ;;(player-set-pos x y)
  (game-add-igel (inexact->exact x)
                 (inexact->exact y))
  (newline))

(define (toggle-pause)
  (game-set-pause (not (game-get-pause))))

(define (game-get-time-str)
  (let* ((t (inexact->exact (game-get-time)))
         (minutes (quotient  t 60))
         (seconds (remainder t 60)))
   (string-append (number->string minutes) 
                  ":"
                  (if (< seconds 10)
                      "0" "")
                  (number->string seconds))))

;; EOF ;;
