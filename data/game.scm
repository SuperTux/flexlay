(use-modules (ice-9 format))
(load "helper.scm")

(define (*key-down-handler* key)
  (display "Keydown: ")
  (display key)
  (newline)
  (cond ((string=? key "i")
         (game-add-igel (+ (player-get-x) 0)
                        (- (player-get-y) 300)))
        ((string=? key "s")
         (windstille:repl))

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
  (display (inexact->exact x))
  (display " ")
  (display (inexact->exact y))
  (newline)

  (cond (#t
         (player-set-pos x y))
        (else
         (display "(game-add-igel ")
         (display (inexact->exact x))
         (display " ")
         (display (inexact->exact y))
         (display ")")
         
         (game-add-igel (inexact->exact x)
                        (inexact->exact y))
         (newline))))

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


(define save-player-position #f)
(let ((x 10)
      (y 45)
      (buttons '())
      (window (gui-create-window 20 20 300 200 "Position Save GUI")))

  (gui-push-component (gui-window-get-client-area window))
  (gui-create-button-func 10 10 100 25 "Save Position"
                          (lambda () (save-player-position (inexact->exact (player-get-pos-x))
                                                           (inexact->exact (player-get-pos-y)))))
  (gui-create-button-func 115 10 100 25 "Clear"
                          (lambda () 
                            (for-each gui-remove-component buttons)
                            (set! buttons '())
                            (set! x 10)
                            (set! y 45)))
  (gui-pop-component)

  (set! save-player-position
        (lambda (player-x player-y)
          (gui-push-component (gui-window-get-client-area window))
          (set! buttons
                (cons (gui-create-button-func x y 70 25 (format #f "~d, ~d" player-x player-y)
                                              (lambda ()
                                                (player-set-pos player-x player-y)))
                      buttons))
          (set! x (+ x 75))
          (cond ((> x 220)
                 (set! y (+ y 30))
                 (set! x 10)))
          (gui-pop-component))))

(save-player-position 258 607)
(save-player-position 3989 703)
(save-player-position 7635 3391)
(save-player-position 969 3263)

(let ((window (gui-create-window 20 320 310 100 "Eval Scheme")))
  (gui-push-component (gui-window-get-client-area window))
  (let ((input  (gui-create-inputbox 10 10 240 30 ""))
        (button (gui-create-button 255 10 50 25 "eval")))
    (gui-component-on-click button
                            (lambda ()
                              (catch #t
                                     (lambda () 
                                       (eval-string (gui-inputbox-get-text input)))
                                     (lambda err
                                       (display "Error: ")(display err)(newline))))))
  (gui-pop-component))

(gui-create-button-func 0 0 100 25 "Hide Debug GUI"
                        (lambda () (gui-hide)))

;; Default to non-debug mode
(gui-hide)

;; (define array
;;   (let ((data '(1 2 3 4 5)))
;;     (make-procedure-with-setter 
;;      (lambda (i)
;;        (list-ref data i))
;;      (lambda (i val)
;;        (list-set! data i val))
;;      )))

;; EOF ;;
