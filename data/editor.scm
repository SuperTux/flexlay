(use-modules (ice-9 pretty-print))

(load "helper.scm")

(define screen-width  (screen-get-width))
(define screen-height (screen-get-height))
(define empty (lambda () #f))
(define *tileeditor* #f)
(define *tileeditor-window* #f)
(define last-files (list *windstille-levelfile*))
(define datadir  *windstille-datadir*)

(define (push-last-file filename)
  (cond ((not (string=? filename (get-last-file)))
         (set! last-files (cons filename last-files)))))

(define (get-last-file)
  (car last-files))

(define (serialize-level)
  `(windstille-level
    (properties
     (name "Hello World"))
    (tilemap
     (width ,(map-get-width)) (height ,(map-get-height))
     (data ,@(map-get-data 1)))
    (background-tilemap
     (width ,(map-get-width)) (height ,(map-get-height))
     (data ,@(map-get-data 0)))))

(define (save-map filename)
  (let ((level (serialize-level)))
    (with-output-to-file filename
      (lambda ()
        (write level)
        (newline)))))

(editor-add-button-func 0 0
                   50 25 "New" 
                   (lambda () 
                     (show-new-level-dialog)))
(editor-add-button-func 0 25
                   50 25 "Load" 
                   (lambda ()
                     (simple-file-dialog "Load a level..." (get-last-file)
                                         (lambda (filename)
                                           (editor-load filename)
                                           (push-last-file filename)))))
(editor-add-button-func 0 50
                   50 25 "Save" 
                   (lambda () 
                     (simple-file-dialog "Save a level..." (get-last-file)
                                         (lambda (filename) 
                                           (save-map filename)
                                           (push-last-file filename)))))

(editor-add-button-func 0 75 50 25 "Play" 
                   (lambda ()
                     (let ((file (tmpnam)))
                       (save-map file)
                       (game-play file)
                       (delete-file file))))

(editor-add-button-func 0 100 50 25 "Quit" 
                   (lambda ()
                     (editor-quit)))

(editor-add-button-func 100 0
                   100 25 "Background" 
                   (lambda () (tilemap-set-active-layer 0)))

(editor-add-button-func 200 0
                   100 25 "Foreground" 
                   (lambda () (tilemap-set-active-layer 1)))

(editor-add-button-func (- screen-width 80)
                   (- screen-height 25)
                   80 25 "Shell" 
                   windstille:repl)

(editor-add-button-func 0
                   (- screen-height 25)
                   100 25 "Tile" 
                   (lambda ()
                     (display "Tile: ")(display (editor-get-brush-tile))(newline)
                     (editor-set-brush-tile (1+ (editor-get-brush-tile)))))

(editor-add-button-func (+ 100)
                   (- screen-height 25)
                   100 25 "Tile Erase" 
                   (lambda ()
                     (editor-set-brush-tile 0)))

(editor-add-button-func (+ 200)
                   (- screen-height 25)
                   100 25 "Objects" 
                   (lambda ()
                     (editor-set-brush-tile 0)))

(editor-add-button (+ 300)
                   (- screen-height 25)
                   100 25 "Select")

(editor-add-button-func (+ 400)
                        (- screen-height 25)
                        100 25 "Tile Editor"
                        (lambda ()
                          (component-show *tileeditor-window*)))

(tile-selector-create (- screen-width (* 3 64)) 0 3 8)

(define (show-new-level-dialog)
  (let ((window (editor-add-window 200 200 200 160 "Property Window")))
    (push-component (window-get-client-area window))

    (editor-add-label 10 10 "Width: ")
    (editor-add-label 10 30 "Height: ")

    (let ((width  (editor-add-inputbox 100 10 50 25 "50"))
          (height (editor-add-inputbox 100 30 50 25 "50"))

          (ok     (editor-add-button 90 100 50 25 "Ok"))
          (cancel (editor-add-button 140 100 50 25 "Cancel")))
      
      (component-on-click ok 
                          (lambda ()   
                            (editor-new (string->number (inputbox-get-text width))
                                        (string->number (inputbox-get-text height)))
                            (component-hide window)))

      (component-on-click cancel
                          (lambda () 
                            (component-hide window)))
      (pop-component))))

(define (simple-file-dialog title filename func)
  (let ((window (editor-add-window 200 200 200 160 title)))
    (push-component (window-get-client-area window))
    (editor-add-label 10 10 "Filename: ")
    (let ((ok       (editor-add-button 90 100 50 25 "Ok"))
          (cancel   (editor-add-button 140 100 50 25 "Cancel"))
          (filename (editor-add-inputbox 10 30 180 25 filename)))

      (component-on-click ok 
                          (lambda ()   
                            (func (inputbox-get-text filename))
                            (component-hide window)))

      (component-on-click cancel
                          (lambda () 
                            (component-hide window)))
      (pop-component)
      )))

(define (dump-tile-definitions filename)
  (with-output-to-file filename
    (lambda ()
     (pretty-print (get-tile-defs))
     (newline)
     (display ";; EOF ;;\n"))))

(let ((window (editor-add-window 200 200 250 180 "Tile Editor")))
  (push-component (window-get-client-area window))
  (set! *tileeditor* (editor-add-tileeditor 10 10))
  (let ((gettile (editor-add-button 148 10 75 25 "Get Tile"))
        (close   (editor-add-button 148 45 75 25 "Close"))
        (dump    (editor-add-button 148 75 75 25 "Dump")))
    
    (component-on-click gettile
                        (lambda ()
                          (tileeditor-set-tile *tileeditor* (editor-get-brush-tile))))
    (component-on-click close
                        (lambda () 
                          (component-hide window)))
    (component-on-click dump
                        (lambda () 
                          (dump-tile-definitions (string-append datadir "tiles.scm"))))

    (set! *tileeditor-window* window))
  (pop-component))

;; EOF ;;