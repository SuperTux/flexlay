(use-modules (ice-9 pretty-print))

(load "helper.scm")

(define screen-width  (screen-get-width))
(define screen-height (screen-get-height))
(define empty (lambda () #f))
(define *tileeditor* #f)
(define *tileeditor-window* #f)
(define *tileselector-window* #f)
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
     (name "Hello World")
     (width  ,(map-get-width))
     (height ,(map-get-height)))
    (scripts ,@(map-get-scripts))
    (tilemap
     (data ,@(map-get-data 1)))
    (background-tilemap
     (data ,@(map-get-data 0)))
    (diamond-map
     ,@(diamond-map-get-data)
     )))

(define (new-map width height)
  (display "Creating new level...\n")
  (editor-new width height))

(define (load-map filename)
  (catch #t
         (lambda ()
           (editor-load filename))
         (lambda args
           (display "Error: ")
           (display args)
           (newline)))
  (push-last-file filename))

(define (save-map filename)
  (let ((level (serialize-level)))
    (with-output-to-file filename
      (lambda ()
        ;;(pretty-print level)
        (display level)
        (newline)))))

(define (resize-map)
  (let ((window (gui-create-window 200 200 150 160 "Resize Map")))
    (gui-push-component (gui-window-get-client-area window))

    (gui-create-label 10 10 "X: ")
    (gui-create-label 10 30 "Y: ")

    (gui-create-label 10 50 "Width: ")
    (gui-create-label 10 70 "Height: ")

    (let ((x (gui-create-inputbox 80 10 50 25 "0"))
          (y (gui-create-inputbox 80 30 50 25 "0"))
          (w (gui-create-inputbox 80 50 50 25 (number->string (map-get-width))))
          (h (gui-create-inputbox 80 70 50 25 (number->string (map-get-height)))))
      
      (gui-create-button-func 60 100 75 25 "Ok"
                              (lambda ()
                                (gui-hide-component window)
                                (catch #t
                                       (lambda ()
                                         (editor-resize-map
                                          (string->number (gui-inputbox-get-text w))
                                          (string->number (gui-inputbox-get-text h))
                                          (string->number (gui-inputbox-get-text x))
                                          (string->number (gui-inputbox-get-text y))))
                                       (lambda args
                                         (display "Error: ")
                                         (display args)
                                         (newline))))))

    (gui-component-on-close window (lambda ()
                                     (gui-hide-component window)))

    (gui-pop-component)))
    
(define (show-new-level-dialog)
  (let ((window (gui-create-window 200 200 200 160 "Create a New Level...")))
    (gui-push-component (gui-window-get-client-area window))

    (gui-create-label 10 10 "Width: ")
    (gui-create-label 10 30 "Height: ")

    (let ((width  (gui-create-inputbox 100 10 50 25 "50"))
          (height (gui-create-inputbox 100 30 50 25 "50"))

          (ok     (gui-create-button 90 100 50 25 "Ok"))
          (cancel (gui-create-button 140 100 50 25 "Cancel")))
      
      (gui-component-on-click ok 
                              (lambda ()   
                                (new-map (string->number (gui-inputbox-get-text width))
                                         (string->number (gui-inputbox-get-text height)))
                                (gui-hide-component window)))

      (gui-component-on-click cancel
                              (lambda () 
                                (gui-hide-component window)))
      (gui-pop-component))))

(define menu (gui-create-menu))
;; File Menu
(gui-add-menu-item menu "File/New.."  show-new-level-dialog)
(gui-add-menu-item menu "File/Open.." 
                   (lambda ()
                     (simple-file-dialog "Load a level..." (get-last-file)
                                         (lambda (filename)
                                           (load-map filename)))))

(define *open-history* (list "../data/levels/tuxlevel3.scm"
                             "../data/levels/tuxlevel2.scm"
                             "../data/levels/tuxlevel4.scm"))

(for-each (lambda (level)
            (gui-add-menu-item menu (string-append "File/Open Recent >/" (basename level))
                               (lambda ()
                                 (load-map level))))
          *open-history*)

(gui-add-menu-item menu "File/Save..." 
                   (lambda ()
                     (simple-file-dialog "Save a level..." (get-last-file)
                                         (lambda (filename) 
                                           (save-map filename)
                                           (push-last-file filename)))))

(gui-add-menu-item menu "File/Play" 
                   (lambda ()
                     (let ((file (tmpnam)))
                       (save-map file)
                       (game-play file)
                       (delete-file file))))

(gui-add-menu-item menu "File/Quit" 
                   (lambda ()
                     (gui-quit)))

;; Dialog Menu
(gui-add-menu-item menu "Dialogs/Resize.."  resize-map)

(gui-add-menu-item menu "Dialogs/TileSelector" 
                   (lambda ()
                     (gui-component-toggle-visibility *tileselector-window*)))

(gui-add-menu-item menu "Dialogs/Tile Editor"
                   (lambda ()
                     (gui-component-toggle-visibility *tileeditor-window*)))

(define *clipboard* #f)
(gui-create-button-func 720 475
                        80 25 "selection2brush" 
                        (lambda () 
                          (set! *clipboard* (editor-get-tile-selection))
                          (tilemap-paint-tool-set-brush *clipboard*)
                          (editor-set-tool 0)
                          ))

(gui-create-button-func 720 500
                        80 25 "Background" 
                        (lambda () (tilemap-set-active-layer 0)))

(gui-create-button-func 720 525
                        80 25 "Foreground" 
                        (lambda () (tilemap-set-active-layer 1)))

(gui-create-button-func 260 0
                        80 25 "Diamonds" 
                        (lambda ()
                          (tilemap-set-active-layer 1)))


(gui-create-button-func (- screen-width 80)
                        (- screen-height 25)
                        80 25 "Shell" 
                        windstille:repl)

(gui-create-button-func 0
                        (- screen-height 25)
                        100 25 "Tile" 
                        (lambda ()
                          (editor-set-tool 0)))

(gui-create-button-func (+ 100)
                        (- screen-height 25)
                        100 25 "Diamond" 
                        (lambda ()
                          (editor-set-tool 2)))

(gui-create-button-func (+ 200)
                        (- screen-height 25)
                        100 25 "Objects" 
                        (lambda ()
                          (editor-set-tool 3)))

(gui-create-button-func (+ 300)
                        (- screen-height 25)
                        100 25 "Select"
                        (lambda ()
                          (editor-set-tool 1)))

(define (simple-file-dialog title filename func)
  (let ((window (gui-create-window 200 200 250 160 title)))
    (gui-push-component (gui-window-get-client-area window))
    (gui-create-label 10 10 "Filename: ")
    (let ((ok       (gui-create-button 90 100 50 25 "Ok"))
          (cancel   (gui-create-button 140 100 50 25 "Cancel"))
          (filename (gui-create-inputbox 10 30 180 30 filename))
          (browse   (gui-create-button 190 30 50 20 "Browse...")))

      (gui-component-on-click ok 
                              (lambda ()   
                                (func (gui-inputbox-get-text filename))
                                (gui-hide-component window)))

      (gui-component-on-click cancel
                              (lambda () 
                                (gui-hide-component window)))

      (gui-component-on-click browse
                              (lambda ()
                                (gui-file-dialog (gui-inputbox-get-text filename)
                                                 (lambda (filename)
                                                   (gui-inputbox-set-text filename)))))

      (gui-pop-component)
      window)))

(define (dump-tile-definitions filename)
  (with-output-to-file filename
    (lambda ()
      (pretty-print (get-tile-defs))
      (newline)
      (display ";; EOF ;;\n"))))


(let ((window (gui-create-window 200 200 250 180 "Tile Editor")))
  (gui-push-component (gui-window-get-client-area window))
  (set! *tileeditor* (editor-add-tileeditor 10 10))
  (let ((gettile (gui-create-button 148 10 75 25 "Get Tile"))
        (dump    (gui-create-button 148 95 75 25 "Dump")))
    
    (gui-component-on-click gettile
                            (lambda ()
                              (tileeditor-set-tile *tileeditor* (editor-get-brush-tile))))
    (gui-component-on-click dump
                            (lambda () 
                              (dump-tile-definitions (string-append datadir "tiles.scm"))))

    (gui-component-on-close window (lambda ()
                                     (gui-hide-component window)))

    (set! *tileeditor-window* window))
  (gui-pop-component))

(define (create-disclaimer)
  (let ((window (gui-create-window 200 200 300 130 "Disclaimer")))
    (gui-push-component (gui-window-get-client-area window))
    (gui-create-label 10 10
                      (string-append "This editor is buggy and might crash quite a bit,\n"
                                     "it isn't really end user ready at the moment. \n"
                                     "It is included here for those who might find it usefull\n"
                                     "anyway, but don't complain when it locks your system"))
    (gui-create-button-func 210 70 75 25 "Ok"
                            (lambda ()
                              (gui-hide-component window)))
    (gui-pop-component)))

(let ((window (gui-create-window 600 25 200 400 "TileSelector")))
  (gui-push-component (gui-window-get-client-area window))
  
  (case *game*
    ((windstille)
     (tile-selector-create (- screen-width (* 3 64)) 0 3 8 .5))
    ((supertux)
     (tile-selector-create (- screen-width (* 3 64)) 0 6 8 1.0))
    (else
     (tile-selector-create (- screen-width (* 3 64)) 0 3 8 .5)))

  (gui-component-on-close window (lambda ()
                                   (gui-hide-component window)))
  (set! *tileselector-window* window)
  (gui-pop-component))

(gui-hide-component *tileeditor-window*)

(let ((window (gui-create-window 460 490 230 110 "Minimap")))
  (gui-push-component (gui-window-get-client-area window))
  (minimap-create 0 0 225 85)
  (gui-pop-component))

;; EOF ;;
