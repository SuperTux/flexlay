;;(start-game (make-game-world 10 10))
(use-modules (ice-9 readline))
(activate-readline)

(load "helper.scm")

(game-add-water 10 100 180 50)
(game-set-player 1000 1300)

(game-add-igel 1000 1300)
(game-add-igel 800 1300)
(game-add-igel 500 1300)

(add-region-trigger 10 10 10000 1200
                    (lambda ()
                      (display "Triggered the trigger\n")
                      (game-add-igel (+ (player-get-x) 0)
                                     (- (player-get-y) 300))))

(add-region-trigger 192 157 317 348
                    (lambda ()
                      (display "Triggered trigger\n")
                      (dialog-clear)
                      (dialog-add "hero/portrait" 
                                  "You have reached the deepest point in this level, congratulations")
                      (dialog-show)
                      (remove-trigger)
                      ))

(add-region-trigger 4995 2056 5124 2211
                    (lambda ()
                      (display "Triggered trigger\n")
                      (dialog-clear)
                      (dialog-add "hero/portrait" 
                                  "End of world!")
                      (dialog-show)
                      (remove-trigger)                     
                      ))

(add-region-trigger 738 2612 931 2822
                    (lambda ()
                      (dialog-clear)
                      (dialog-add "hero/portrait" 
                                  "Deep cave here...")
                      (dialog-show)
                      (remove-trigger)
                      ))

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
  (newline))

(player-set-pos 100 400)
(player-set-direction "west")

(define (toggle-pause)
  (game-set-pause (not (game-get-pause))))

(dialog-add "hero/portrait"
            (string-append 
             "Welcome to Windstille, I'll explain you a bunch of basic controlls now.\n"
             "<control> acts as the fire button, "
             "cursors keys allow you to control the suit, "
             " <down>+<control> drops a bomb, "
             "You can hide this dialog with <control>"))
(dialog-show)
            

;; EOF ;;
