(display "EDITOR HELLOWORLD\n")

(define screen-width  800)
(define screen-height 600)
(define empty (lambda () #f))

(define (create-a-button)
  (editor-add-button (random 640) (random 480) 100 50 "Create a button" 
                     create-a-button))

(editor-add-button 0
                   (- screen-height 25)
                   100 25 "Brush" 
                   (lambda ()
                     (display "Tile: ")(display (editor-get-brush-tile))(newline)
                     (editor-set-brush-tile (1+ (editor-get-brush-tile)))))

(editor-add-button (+ 100)
                   (- screen-height 25)
                   100 25 "Tile" 
                   empty)

(editor-add-button (+ 200)
                   (- screen-height 25)
                   100 25 "Erase" 
                   (lambda ()
                     (editor-set-brush-tile 0)))
                 
(editor-add-button 0 0 50 25 "Quit" 
                   (lambda ()
                     (editor-quit)))

#!
(editor-add-window 400 200 125 300 "Hello Window")
(editor-add-button 10  10 100 25 "Hello World1"  empty)
(editor-add-button 10  40 100 25 "Hello World2"  empty)
(editor-add-button 10  70 100 25 "Hello World3"  empty)
(editor-add-button 10 100 100 25 "Hello World3"  empty)
(editor-add-label  10 130   "Foobar:")
(editor-add-inputbox  60 130 50 25  "50")

(editor-add-window 10 10 125 300 "Hello Window")
(editor-add-button 10  10 100 25 "Hello World1"  empty)
(editor-add-button 10  40 100 25 "Hello World2"  empty)
(editor-add-button 10  70 100 25 "Hello World3"  empty)
(editor-add-button 10 100 100 25 "Hello World3"  empty)
(editor-add-label  10 130   "Foobar:")
(editor-add-inputbox  60 130 50 25  "50")
!#

;; EOF ;;