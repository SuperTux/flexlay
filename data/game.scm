
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
  (display "ClickPos: ")
  (display (inexact->exact x))
  (display " ")
  (display (inexact->exact y))
  (player-set-pos x y)
  (newline))

(define (toggle-pause)
  (game-set-pause (not (game-get-pause))))


;; EOF ;;
