(display "EDITOR HELLOWORLD\n")

(editor-add-button 200 200 100 50 "Hello World" 
                   (lambda ()
                     (display "You clicked me...\n")))

(define (create-a-button)
  (editor-add-button (random 640) (random 480) 100 50 "Create a button" 
                     create-a-button))

(editor-add-button 200 260 100 50 "Create a button" 
                   create-a-button)

(editor-add-button 200 320 100 50 "Hello World" 
                   (lambda ()
                     (display "You clicked me...\n")))

(editor-add-label 190 320 "Hello World")
                 
(editor-add-button 0 0 50 25 "Quit" 
                   (lambda ()
                     (editor-quit)))

(define empty (lambda () #f))

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


;; EOF ;;