
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
  (player-set-pos x y)
  ;;(game-add-igel (inexact->exact x)
   ;;              (inexact->exact y))
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
