(use-modules (ice-9 pretty-print))

(load "helper.scm")

(define screen-width  (screen-get-width))
(define screen-height (screen-get-height))
(define empty (lambda () #f))
(define *editor-map* #f)
(define *editor-variables* '())
(define *tileeditor* #f)
(define *tileeditor-window* #f)
(define *tileselector-window* #f)
(define *object-inserter-window* #f)
(define *object-selector* #f)
(define *minimap* #f)
(define *menu*    #f)
(define *statusbar* #f)
(define *clipboard* #f)
(define *recent-files* '())
(define *recent-files-size* 25)
(define datadir  *windstille-datadir*)
(define *buffers* '())

(define (add-buffer m)
  ;; FIXME: Doesn't work?!
  (gui-add-menu-item *menu*
                     (format #f "Buffers/~a. ~a"
                             (gensym "")
                             (basename (editor-map-get-filename m))
                             )
                     (lambda ()
                       (editor-map-component-set-map *editor-map* m)))

  (set! *buffers* (cons m *buffers*)))

(define (push-last-file filename)
  (cond ((not (string=? filename (get-last-file)))
         (set! *recent-files* (cons filename *recent-files*)))))


(define (get-last-file)
  (car *recent-files*))

(define (new-map width height)
  (display "Creating new level...\n")
  (let ((levelmap (create-level-map width height)))
    (editor-map-component-set-map *editor-map* levelmap)
    (add-buffer levelmap)))

(define (editor-error . args)
  (display "EditorError: ")
  (for-each display args)
  (newline))

(define (create-level-map width height)
  (let* ((m       (editor-map-create))
         (tilemap (editor-tilemap-create width height 32))
         (objmap  (editor-objmap-create)))
    (editor-map-add-layer m tilemap)
    (editor-map-add-layer m objmap)
    m))

(define (get-value-from-tree pos lst default)
  (cond ((null? pos)
         lst)
        ((null? lst)
         default)
        ((equal? pos '(_))
         (car lst))
        (else
         (let ((el (assoc-ref lst (car pos))))
           (cond (el
                  (get-value-from-tree (cdr pos) el default))
                 (else
                  default
                  ))))))

(define (create-level-map-from-file filename)
  (let ((data (with-input-from-file filename
                (lambda () (cdr (read))))))

    (let ((width      (get-value-from-tree '(properties width _)  data 20))
          (height     (get-value-from-tree '(properties height _) data 15))
          (foreground (get-value-from-tree '(tilemap data) data '()))
          (background (get-value-from-tree '(background-tilemap data) data '()))
          (diamonds   (get-value-from-tree '(diamond-map) data '())))
      
      ;; load level file and extract tiledata and w/h
      (let* ((m       (editor-map-create))
             (tilemap (editor-tilemap-create width height 32))
             (objmap  (editor-objmap-create)))
        
        (editor-map-add-layer m tilemap)
        (editor-map-add-layer m objmap)
    
        (editor-map-set-filename m filename)

        ;; set data to the tilemap
        (editor-tilemap-set-data tilemap 1 foreground)
        (editor-tilemap-set-data tilemap 0 background)

        m))))

(define (load-map filename)
  (catch #t
         (lambda ()
           (let ((levelmap (create-level-map-from-file filename)))
             (editor-map-component-set-map *editor-map* levelmap)
             (add-buffer levelmap)
             ))
         (lambda args
           (editor-error args)))
  (push-last-file filename))

(define (write-field indent width field)
  (display indent)
  (let ((x 0))
    (for-each (lambda (el)
                (cond ((or (>= el 10) (< el 0))
                       (display el)
                       (display " "))
                      (else
                       (display el)
                       (display "  ")))

                (set! x (1+ x))
                (cond ((>= x width)
                       (newline)
                       (display indent)
                       (set! x 0)
                       )))
              field)))

(define (save-map filename)
  ;; FIXME: This is old style singleton code
  (if (access? filename F_OK)
      (rename-file filename (string-append filename "~")))

  (with-output-to-file filename
    (lambda ()
      (display   "(windstille-level\n\n")

      (display   "  (properties\n")
      (display   "    (name \"Hello World\")\n")
      (format #t "    (width  ~a)~%" (map-get-width))
      (format #t "    (height ~a)~%" (map-get-height))
      (display   "   )\n\n")

      (display   "  (scripts ")
      (for-each (lambda (file)
                  (write file)
                  (display " "))
                (map-get-scripts))
      (display   "   )\n\n")

      (display     "  (tilemap (data\n")
      (write-field "   " (map-get-width) (map-get-data 1))
      (display     "   ))\n\n")

      (display   "  (background-tilemap (data\n")
      (write-field "   " (map-get-width) (map-get-data 0))
      (display   "   ))\n\n")

      (format #t "  (diamond-map\n")
      (write-field "   " 
                   (map-get-width)
                   (diamond-map-get-data))
      (format #t "   )\n\n")

      (display   " )\n")
      (newline)
      (display ";; EOF ;;\n")

      (editor-map-set-unmodified (editor-map-component-get-map *editor-map*))
      )))

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
                                         (editor-error args))))))

    (gui-component-on-close window (lambda ()
                                     (gui-hide-component window)))

    (gui-pop-component)))

(define (yes-no-dialog title-text text yes-text no-text yes-func no-func)
  (let ((window (gui-create-window 250 200 300 90 title-text)))
    (gui-push-component (gui-window-get-client-area window))
    (gui-create-label 10 10 text)

    (let ((yes (gui-create-button 240 30 50 25 yes-text))
          (no  (gui-create-button 180 30 50 25 no-text)))
      (gui-component-on-click yes
                              (lambda () 
                                (gui-hide-component window)
                                (if yes-func (yes-func))))
      (gui-component-on-click no
                              (lambda ()
                                (gui-hide-component window)
                                (if no-func (no-func)))))
    (gui-pop-component)))

(define (show-new-level-dialog)
  (let ((window (gui-create-window 200 200 200 160 "Create a New Level...")))
    (gui-push-component (gui-window-get-client-area window))

    (gui-create-label 10 10 "Width: ")
    (gui-create-label 10 30 "Height: ")

    (let ((width  (gui-create-inputbox 100 10 50 25 "500"))
          (height (gui-create-inputbox 100 30 50 25 "15"))

          (ok     (gui-create-button 140 100 50 25 "Ok"))
          (cancel (gui-create-button 90 100 50 25 "Cancel")))
      
      (gui-component-on-click ok 
                              (lambda ()   
                                (new-map (string->number (gui-inputbox-get-text width))
                                         (string->number (gui-inputbox-get-text height)))
                                (gui-hide-component window)))

      (gui-component-on-click cancel
                              (lambda () 
                                (gui-hide-component window)))
      (gui-pop-component))))

(define (create-menu)
  (set! *menu* (gui-create-menu))
  (let ((menu *menu*))
    ;; File Menu
    (gui-add-menu-item menu "File/New.."  show-new-level-dialog)
    (gui-add-menu-item menu "File/Open.." 
                       (lambda ()
                         (simple-file-dialog "Load a level..." (get-last-file)
                                             (lambda (filename)
                                               (load-map filename)))))


    (for-each (lambda (level)
                (gui-add-menu-item menu (string-append "File/Open Recent >/" (basename level))
                                   (lambda ()
                                     (load-map level))))
              *recent-files*)

    (gui-add-menu-item menu "File/Save" 
                       (lambda ()
                         (simple-file-dialog "Save a level..." (get-last-file)
                                             (lambda (filename) 
                                               (save-map filename)
                                               (push-last-file filename)))))

    (gui-add-menu-item menu "File/Save As..." 
                       (lambda ()
                         (simple-file-dialog "Save As" (get-last-file)
                                             (lambda (filename) 
                                               (cond ((access? filename F_OK)
                                                      (yes-no-dialog 
                                                       "File already exist!"
                                                       (string-append
                                                        "Replace '" filename "'?")
                                                       "Replace" "Cancel" 
                                                       (lambda ()
                                                         (save-map filename)
                                                         (push-last-file filename))
                                                       #f))
                                                     (else
                                                      (save-map filename)
                                                      (push-last-file filename)))))))

    (gui-add-menu-item menu "File/Play" 
                       (lambda ()
                         (let ((file (tmpnam)))
                           (save-map file)
                           (game-play file)
                           (delete-file file))))

    (gui-add-menu-item menu "File/Quit" 
                       (lambda ()
                         (let ((modified #f))
                           (for-each (lambda (el)
                                       (set! modified (or modified
                                                          (editor-map-is-modified el))))
                                     *buffers*)

                           (cond (modified
                                  (yes-no-dialog 
                                   "Quit"
                                   "Some files have been modified, do you want to quit?"
                                   "Quit" "Cancel" 
                                   (lambda ()
                                     (on-gui-quit)
                                     (gui-quit))
                                   #f))
                                 (else
                                  (on-gui-quit)
                                  (gui-quit)))
                           )))

    ;; Dialog Menu
    (gui-add-menu-item menu "Dialogs/Draw Grid" editor-toggle-grid)
    (gui-add-menu-item menu "Dialogs/Resize.."  resize-map)
    (gui-add-menu-item menu "Dialogs/Minimap"  (lambda ()
                                                 (gui-component-toggle-visibility *minimap*)))

    (gui-add-menu-item menu "Dialogs/TileSelector" 
                       (lambda ()
                         (gui-component-toggle-visibility *tileselector-window*)))

    (gui-add-menu-item menu "Dialogs/Tile Editor"
                       (lambda ()
                         (gui-component-toggle-visibility *tileeditor-window*)))))

(define (set-tool sym)
  (case sym 
    ((tile)
     (editor-set-tool 0)
     (gui-show-component *tileselector-window*)
     (gui-hide-component *object-inserter-window*))

    ((select)
     (editor-set-tool 1))
    ((diamond)
     (editor-set-tool 2))
    ((object)
     (editor-set-tool 3)
     (gui-show-component *object-inserter-window*)
     (gui-hide-component *tileselector-window*))
    (else
     (editor-error "Tool unknown"))))

(define (create-toolbar)
  (let ((window (gui-create-window 0 25 50 360 "Toolbar")))
    (gui-push-component (gui-window-get-client-area window))
    

    (gui-create-button-func 0 0
                            40 25 "Tile" 
                            (lambda ()
                              (set-tool 'tile)))

    (gui-create-button-func 0  25
                            40 25 "Select"
                            (lambda ()
                              (set-tool 'select)))

    (gui-create-button-func 0 50
                            40 25 "Diam." 
                            (lambda ()
                              (set-tool 'diamond)))

    (gui-create-button-func 0 75                              
                            40 25 "Objs" 
                            (lambda ()
                              (set-tool 'object)))

    (gui-create-button-func 0 100
                            40 25 "Brush" 
                            (lambda () 
                              (set! *clipboard* (editor-get-tile-selection))
                              (cond (*clipboard*
                                     (tilemap-paint-tool-set-brush *clipboard*)
                                     (set-tool 'tile)))))

    (gui-create-button-func 0 150
                            40 25 "BG" 
                            (lambda () (tilemap-set-active-layer 0)))

    (gui-create-button-func 0 175
                            40 25 "FG" 
                            (lambda () (tilemap-set-active-layer 1)))

    (gui-create-button-func 0 225
                            40 25 "Undo" 
                            (lambda () (editor-undo)))

    (gui-create-button-func 0 250
                            40 25 "Redo" 
                            (lambda () (editor-redo)))

    (gui-create-button-func 0 300
                            40 25 "Shell" 
                            windstille:repl)
    )
  (gui-pop-component))

(define (simple-file-dialog title filename func)
  (let ((window (gui-create-window 200 200 250 160 title)))
    (gui-push-component (gui-window-get-client-area window))
    (gui-create-label 10 10 "Filename: ")
    (let ((ok       (gui-create-button 190 100 50 25 "Ok"))
          (cancel   (gui-create-button 130 100 50 25 "Cancel"))
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

(define (create-tile-editor)
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
    (gui-pop-component)
    (gui-hide-component *tileeditor-window*)))

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

(define (create-object-inserter)
  (let ((window (gui-create-window 600 25 200 400 "ObjectInserter")))
    (gui-push-component (gui-window-get-client-area window))
    
    (gui-create-label 5 5 "[Drag objects from here to the map]")
    (set! *object-selector* (object-selector-create 0 24 3 4 64 64))

    (gui-component-on-close window (lambda ()
                                     (gui-hide-component window)))
    (gui-pop-component)
    (set! *object-inserter-window* window)
    (gui-hide-component *object-inserter-window*)
    ))

(define (create-tile-selector)
  (let ((window (gui-create-window 600 25 200 400 "TileSelector")))
    (gui-push-component (gui-window-get-client-area window))
    
    (case *game*
      ((windstille)
       (tile-selector-create (- screen-width (* 3 64)) 0 3 8 .5))
      ((supertux)
       (tile-selector-create (- screen-width (* 3 64)) 0 6 12 1.0))
      (else
       (tile-selector-create (- screen-width (* 3 64)) 0 3 8 .5)))

    (gui-component-on-close window (lambda ()
                                     (gui-hide-component window)))
    (set! *tileselector-window* window)
    (gui-hide-component *tileselector-window*)
    (gui-pop-component)))

(define (create-minimap)
  ;;  (let ((window (gui-create-window (- screen-width 230) 
  ;;                                   (- screen-height 110)
  ;;                                   230 110 "Minimap")))
  ;;    (gui-push-component (gui-window-get-client-area window))
  (let ((width screen-width)
        (height 50))
    (set! *minimap* (minimap-create *editor-map*
                                    (- screen-width width) 
                                    (- screen-height height)
                                    width height))))
;;    (gui-pop-component)
;;  (gui-component-on-close window (lambda ()
;;                                  (gui-hide-component window)))
;;     window)))

(define (on-gui-quit)
  (with-output-to-file (string-append *windstille-homedir* "editor-variables.scm")
    (lambda ()
      (display ";; Automatically Written file, don't edit by hand!\n\n")
      (write (list
              (cons '*recent-files* 
                    *recent-files*)))
      (newline)
      (display "\n;; EOF ;;\n")
      )))

(define (truncate-list n lst)
  (cond ((or (null? lst)
             (= n 0))
         '())
        (else
         (cons (car lst)
               (truncate-list (- n 1) (cdr lst))))))

(define (remove-doubles lst)
  (define (remove-doubles-helper lst ret)
    (cond ((null? lst)
           (reverse ret))
          (else
           (if (not (member (car lst) ret))
               (remove-doubles-helper (cdr lst) (cons (car lst) ret))
               (remove-doubles-helper (cdr lst) ret)
               ))))

  (remove-doubles-helper lst '()))

(define (load-variables)
  (let ((editor-variables '()))
    (catch #t
           (lambda ()
             (call-with-input-file (string-append *windstille-homedir* "editor-variables.scm")
               (lambda (port)
                 (let ((vars (read port)))
                   (cond ((list? vars)
                          (set! *editor-variables* vars))
                         (else
                          (editor-error "Couldn't read vars from config file\n"))
                         )))))
           (lambda args
             (editor-error args)))))

(define (init-recent-files)
  (if (not (string=? "" *windstille-levelfile*))
      (set! *recent-files* (cons  *windstille-levelfile* *recent-files*)))


  (let ((ent (assoc-ref *editor-variables* '*recent-files*)))
    (cond (ent
           (set! *recent-files* 
                 (truncate-list *recent-files-size*
                                (remove-doubles (append *recent-files* ent)))))
          (else
           (editor-error *editor-variables*))))

  (set! *recent-files* (filter (lambda (file)
                                 (access? file F_OK))
                               *recent-files*)))

(load-variables)
(init-recent-files)

(set! *editor-map* (editor-map-component-create 0 25 screen-width (- screen-height 25)))
(create-menu)

(create-toolbar)
(create-tile-editor)
(create-tile-selector)
(create-object-inserter)
(create-minimap)

(set-tool 'tile)

(object-selector-add-brush *object-selector* "igel" '(Igel))
(object-selector-add-brush *object-selector* "hero/run" '(Hero))

(new-map 60 15)

;; EOF ;;
