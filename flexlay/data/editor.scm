(use-modules (srfi srfi-13)
             (srfi srfi-1)
             (ice-9 pretty-print))

(use-modules (ice-9 readline))
(activate-readline)

(load "helper.scm")
(debug-enable 'backtrace)
(define screen-width  (screen-get-width))
(define screen-height (screen-get-height))
(define empty (lambda () #f))
(define *editor:file-plugins '())
(define *editor-map* #f)
(define *editor-variables* '())
(define *tileeditor* #f)
(define *brushes* '())
(define *tileeditor-window* #f)
(define *brush-selector* #f)
(define *tileselector-window* #f)
(define *tileselector* #f)
(define *object-inserter-window* #f)
(define *object-selector* #f)
(define *minimap* #f)
(define *menu*    #f)
(define *statusbar* #f)
(define *clipboard* #f)
(define *recent-files* '())
(define *recent-files-size* 25)
(define datadir  *flexlay-datadir*)
(define *tilemap* #f)
(define *objmap*  #f)
(define *buffers* '())
(define *workspace* #f)
(define *variable-file* (string-append *flexlay-homedir*
                                      (symbol->string *game*)
                                      "-variables.scm"))
(define *editor:file-plugins* '())

(define (editor-map-activate m)
  (workspace-set-current-map *workspace* m))

(define (editor-map-get-current)
  (workspace-get-current-map *workspace*))

(define (*error-handler* . args)
  (display "Error: Something went wrong on the C++ side\n")
  (backtrace)
  (display args)(newline))

(define (editor:add-file-plugin pred func)
  "Add a handler for a new filetype"
  (set! *editor:file-plugins*
        (cons (cons pred func) *editor:file-plugins*)))

(define (editor:get-plugin filename)
  (cdr (find (lambda (el) ((car el) filename))
             *editor:file-plugins*)))

;; Default file plug-in, in case others fail
(editor:add-file-plugin (lambda (filename) #t)
                        (lambda (filename)
                          (error "Couldn't load " filename)))

(define (add-buffer m)
  ;; FIXME: Doesn't work?!
  (gui-add-menu-item *menu*
                     (format #f "Buffers/~a. ~a"
                             (gensym "")
                             (basename (editor-map-get-filename m)))
                     (lambda ()
                       (editor-map-activate m)))

  (set! *buffers* (cons m *buffers*)))

(define (push-last-file filename)
  (cond ((not (string=? filename (get-last-file)))
         (set! *recent-files* (cons filename *recent-files*)))))


(define (get-last-file)
  (cond ((null? *recent-files*)
         "../data/levels/newlevel.scm")
        (else
         (car *recent-files*))))

(define (editor-error . args)
  (display "EditorError: ")
  (for-each display args)
  (newline))

(define y 0);; FIXME:
(define (load-map filename)
  (catch #t
         (lambda ()
           (let* ((plugin   (editor:get-plugin filename))
                  (levelmap (plugin filename)))
             (editor-map-activate levelmap)
             ;;(workspace-add-map *workspace* levelmap 0 y)
             (set! y (+ y 600))
             (add-buffer levelmap)
             ))
         (lambda args
           (backtrace)
           (editor-error args)
           ))
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
      (format #t "    (width  ~a)~%" (editor-tilemap-get-width  *tilemap*))
      (format #t "    (height ~a)~%" (editor-tilemap-get-height *tilemap*))
      (display   "   )\n\n")

      (display     "  (tilemap (data\n")
      (write-field "   " (map-get-width) (editor-tilemap-get-data *tilemap*))
      (display     "   ))\n\n")

      (format #t "  (objects\n")
      (for-each (lambda (el)
                  (let* ((obj (editor-objectmap-get-object el)))
                    (format #t "    (~a  (pos ~a ~a))~%" 
                            (caaddr obj)
                            (car obj)
                            (cadr obj)
                            )))
                (editor-objectmap-get-objects))
      (format #t "  )\n")

      (format #t "   )\n\n")

      (newline)
      (display ";; EOF ;;\n")

      (editor-map-set-unmodified (editor-map-get-current))
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
          (w (gui-create-inputbox 80 50 50 25 (number->string                      
                                               (editor-tilemap-get-width *tilemap*))))
          (h (gui-create-inputbox 80 70 50 25 (number->string 
                                               (editor-tilemap-get-height *tilemap*)))))
      
      (gui-create-button-func 60 100 75 25 "Ok"
                              (lambda ()
                                (gui-hide-component window)
                                (catch #t
                                       (lambda ()
                                         (let ((w (string->number (gui-inputbox-get-text w)))
                                               (h (string->number (gui-inputbox-get-text h)))
                                               (x (string->number (gui-inputbox-get-text x)))
                                               (y (string->number (gui-inputbox-get-text y))))
                                           (case *game*
                                             ((supertux)
                                              (supertux:resize 
                                               (editor-map-get-metadata (editor-map-get-current))
                                               w h x y))
                                             (else
                                              (editor-tilemap-resize *tilemap* w h x y)))))
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
                                (let ((width  (string->number (gui-inputbox-get-text width)))
                                      (height (string->number (gui-inputbox-get-text height))))
                                  (case *game*
                                    ((supertux)
                                     (supertux:new-map width height))
                                    ((netpanzer)
                                     (netpanzer:new-map width height))
                                    ((windstille)
                                     (windstille:new-map width height))))
                                (gui-hide-component window)))

      (gui-component-on-click cancel
                              (lambda () 
                                (gui-hide-component window)))
      (gui-pop-component))))

(define (create-menu)
  (set! *menu* (gui-create-menu))
  (let ((menu *menu*))
    ;; File Menu
    (case *game*
      ((supertux)
       (gui-add-menu-item menu "File/New/Level..."  show-new-level-dialog)
       (gui-add-menu-item menu "File/New/Worldmap..."  show-new-level-dialog))
      (else
       (gui-add-menu-item menu "File/New.."  show-new-level-dialog)))

    (for-each (lambda (level)
                (gui-add-menu-item menu (string-append "File/Open Recent >/" (basename level))
                                   (lambda ()
                                     (load-map level))))
              *recent-files*)

    (case *game*
      ((supertux)
       (gui-add-menu-item menu "File/Open..." 
                          (lambda ()
                            (simple-file-dialog "Load level..." (get-last-file)
                                                (lambda (filename) 
                                                  (supertux:load-map filename)))))

       (gui-add-menu-item menu "File/Save..." 
                          (lambda ()
                            (simple-file-dialog "Save level..." (get-last-file)
                                                (lambda (filename)   
                                                  (supertux:save-map (get-current-map-data) filename))))))
      
      ((netpanzer)
       (gui-add-menu-item menu "File/Import NetPanzer.." 
                          (lambda ()
                            (simple-file-dialog "Import netPanzer level..." (get-last-file)
                                                (lambda (filename)
                                                  (netpanzer:load-map filename)))))


       (gui-add-menu-item menu "File/Export Netpanzer" 
                          (lambda ()
                            (simple-file-dialog "Export netPanzer level..." (get-last-file)
                                                (lambda (filename) 
                                                  (netpanzer:save-map filename)
                                                  (push-last-file filename))))))
      
      (else
       (gui-add-menu-item menu "File/Open.." 
                          (lambda ()
                            (simple-file-dialog "Load a level..." (get-last-file)
                                                (lambda (filename)
                                                  (load-map filename)))))

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
                                                         (push-last-file filename))))))))
      )

    ;; Move this to game specifc code
    ;;    (gui-add-menu-item menu "File/Play" 
    ;;                       (lambda ()
    ;;                         (let ((file (tmpnam)))
    ;;                           (save-map file)
    ;;                           (game-play file)
    ;;                           (delete-file file))))

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
    (case *game*
      ((netpanzer)
       (gui-add-menu-item menu "Dialogs/Edit Metadata" 
                          (lambda ()
                            (netpanzer:metadata-editor (get-current-map-data))))))

    (gui-add-menu-item menu "Dialogs/Draw Grid" 
                       (lambda ()
                         (editor-toggle-grid *tilemap*)))
    (gui-add-menu-item menu "Dialogs/Draw Attributes" 
                       (lambda ()
                         (editor-toggle-attributes *tilemap*)))
    (gui-add-menu-item menu "Dialogs/Resize.."  resize-map)
    (gui-add-menu-item menu "Dialogs/Minimap"  (lambda ()
                                                 (gui-component-toggle-visibility *minimap*)))

    (gui-add-menu-item menu "Dialogs/TileSelector" 
                       (lambda ()
                         (gui-component-toggle-visibility *tileselector-window*)))

    (gui-add-menu-item menu "Dialogs/Tile Editor"
                       (lambda ()
                         (gui-component-toggle-visibility *tileeditor-window*)))
    
    (case *game*
      ((supertux)
       (gui-add-menu-item menu "Layers/Background"
                          (lambda () (supertux:set-background-layer-active)))
       (gui-add-menu-item menu "Layers/Interactive"
                          (lambda () (supertux:set-interactive-layer-active)))
       (gui-add-menu-item menu "Layers/Foreground"
                          (lambda () (supertux:set-foreground-layer-active)))

       (gui-add-menu-item menu "Layers/Background only"
                          (lambda () (supertux:set-background-layer-only)))
       (gui-add-menu-item menu "Layers/Interactive only"
                          (lambda () (supertux:set-interactive-layer-only)))
       (gui-add-menu-item menu "Layers/Foreground only"
                          (lambda () (supertux:set-foreground-layer-only)))

       (gui-add-menu-item menu "Layers/Show All"
                          (lambda () (supertux:show-all-layers)))))

    (gui-add-menu-item menu "Zoom/1:4 (25%) " (lambda ()
                                                (editor-map-component-set-zoom *editor-map* .25)))
    (gui-add-menu-item menu "Zoom/1:2 (50%) " (lambda ()
                                                (editor-map-component-set-zoom *editor-map* .5)))
    (gui-add-menu-item menu "Zoom/1:1 (100%) " (lambda ()
                                                 (editor-map-component-set-zoom *editor-map* 1.0)))
    (gui-add-menu-item menu "Zoom/2:1 (200%) " (lambda ()
                                                 (editor-map-component-set-zoom *editor-map* 2.0)))
    (gui-add-menu-item menu "Zoom/4:1 (400%) " (lambda ()
                                                 (editor-map-component-set-zoom *editor-map* 4.0)))
    ))

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
    ((zoom)
     (editor-set-tool 4))
    (else
     (editor-error "Tool unknown"))))


(define (get-current-map-data)
  (editor-map-get-metadata (editor-map-get-current)))

(define (create-metadata-editor)
  (let ((window (gui-create-window 100 100 400 200 "Metadata Editor")))
    (gui-push-component (gui-window-get-client-area window))
    
    (let* ((y 10)
           (boxes '()))
      (for-each (lambda (el)
                  (gui-create-label    10  y         (format #f "~a" (car el)))
                  (set! boxes 
                        (cons (cons (car el)
                                    (gui-create-inputbox 100 y  290 25 (format #f "~a" (cdr el))))
                              boxes))
                  (set! y (+ y 25)))
                (or (editor-map-get-metadata (editor-map-get-current))
                    '()))
      
      (gui-create-button-func 300 140 75 25 "Ok"
                              (lambda ()
                                (editor-map-set-metadata (editor-map-get-current)
                                                         (map (lambda (el)
                                                                (cons (car el)
                                                                      (gui-inputbox-get-text (cdr el))))
                                                              (reverse boxes)))
                                (gui-hide-component window))))

    (gui-pop-component)))

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

    ;;    (gui-create-button-func 0 50
    ;;                            40 25 "Diam." 
    ;;                            (lambda ()
    ;;                              (set-tool 'diamond)))

    (gui-create-button-func 0 50
                            40 25 "Zoom" 
                            (lambda ()
                              (set-tool 'zoom)))

    (gui-create-button-func 0 75                              
                            40 25 "Objs" 
                            (lambda ()
                              (set-tool 'object)))

    (gui-create-button-func 0 100
                            40 25 "Brush" 
                            (lambda () 
                              (set! *clipboard* (editor-get-tile-selection))
                              (cond (*clipboard*
                                     (set! *brushes*
                                           (assoc-set! *brushes* "Fooname" *clipboard*))))
                              (display *brushes*)(newline)))

    ;;(tilemap-paint-tool-set-brush *clipboard*)
    ;;(set-tool 'tile)

    (gui-create-button-func 0 150
                            40 25 "SavePNG" 
                            (lambda () 
                              (editor-tilemap-save-png *tilemap* "/tmp/foobar.pnm")))
    ;;(tilemap-set-active-layer 0)))

;;    (gui-create-button-func 0 175
;;                            40 25 "FG" 
;;                            (lambda () #f))

    (gui-create-button-func 0 225
                            40 25 "Undo" 
                            (lambda () (editor-undo)))

    (gui-create-button-func 0 250
                            40 25 "Redo" 
                            (lambda () (editor-redo)))

    (gui-create-button-func 0 300
                            40 25 "Shell" 
                            windstille:repl))
  (gui-pop-component))

(define (simple-file-dialog title filename func)
  (let ((window (gui-create-window 200 100 460 125 title)))
    (gui-push-component (gui-window-get-client-area window))
    (gui-create-label 10 10 "Filename: ")
    (let ((ok       (gui-create-button 390 60 50 25 "Ok"))
          (cancel   (gui-create-button 330 60 50 25 "Cancel"))
          (filename (gui-create-inputbox 10 30 435 30 filename))
          ;;(browse   (gui-create-button 190 30 50 20 "Browse..."))
          )

      (gui-component-on-click ok 
                              (lambda ()   
                                (func (gui-inputbox-get-text filename))
                                (gui-hide-component window)))

      (gui-component-on-click cancel
                              (lambda () 
                                (gui-hide-component window)))

      ;;      (gui-component-on-click browse
      ;;                              (lambda ()
      ;;                                (gui-file-dialog (gui-inputbox-get-text filename)
      ;;                                                (lambda (filename)
      ;;                                                   (gui-inputbox-set-text filename)))))

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
    (set! *tileeditor* (editor-add-tileeditor 10 10 *tile-size* *tile-size*))
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
  (let ((window (gui-create-window 600 25 200 500 "ObjectInserter")))
    (gui-push-component (gui-window-get-client-area window))
    
    (gui-create-label 5 5 "[Drag objects from here to the map]")
    (set! *object-selector* (object-selector-create 0 24 4 4 48 48))

    (gui-component-on-close window (lambda ()
                                     (gui-hide-component window)))
    (gui-pop-component)
    (set! *object-inserter-window* window)
    (gui-hide-component *object-inserter-window*)
    ))

(define (create-tile-selector)
  (let ((window (gui-create-window 600 25 200 400 "TileSelector")))
    (gui-push-component (gui-window-get-client-area window))
    
    (set! *tileselector*
          (case *game*
            ((windstille)
             (tile-selector-create (- screen-width (* 3 64)) 0 3 8 .5))
            ((supertux)
             (tile-selector-create (- screen-width (* 3 64)) 0 6 15 1.0))
            ((netpanzer)
             (display "Netpanzer\n")
             (tile-selector-create (- screen-width (* 20 32)) 0 20 15 1.0))
            (else
             (tile-selector-create (- screen-width (* 3 64)) 0 3 8 .5))))

    (gui-component-on-close window (lambda ()
                                     (gui-hide-component window)))
    (set! *tileselector-window* window)
    (gui-hide-component *tileselector-window*)
    (gui-pop-component)))

(define (create-minimap width height)
  ;;  (let ((window (gui-create-window (- screen-width 230) 
  ;;                                   (- screen-height 110)
  ;;                                   230 110 "Minimap")))
  ;;    (gui-push-component (gui-window-get-client-area window))
  (set! *minimap* (minimap-create *editor-map*
                                  (- screen-width width) 
                                  (- screen-height height)
                                  width height)))
;;    (gui-pop-component)
;;  (gui-component-on-close window (lambda ()
;;                                  (gui-hide-component window)))
;;     window)))

(define (create-brush-selector)
  (let ((window (gui-create-window (- screen-width 230) 
                                   30
                                   230 400 "Brush Selector"))
        (y 30))
    (gui-push-component (gui-window-get-client-area window))
    (for-each (lambda (brush) 
                (gui-create-button-func 10 y
                                        150 25
                                        (lambda ()
                                          (tilemap-paint-tool-set-brush brush))))
              *brushes*)
    (gui-pop-component)))

(define (on-gui-quit)
  (variables:save *variable-file*))

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

(define (variables:save filename)
  (with-output-to-file filename
    (lambda ()
      (display ";; Automatically Written file, don't edit by hand!\n\n")
      (write (list
              (cons '*recent-files* 
                    *recent-files*)))
      (newline)
      (display "\n;; EOF ;;\n")
      )))

(define (variables:load filename)
  (let ((editor-variables '()))
    (catch #t
           (lambda ()
             (call-with-input-file (string-append filename)
               (lambda (port)
                 (let ((vars (read port)))
                   (cond ((list? vars)
                          (set! *editor-variables* vars))
                         (else
                          (editor-error "Couldn't read vars from config file\n"))
                         )))))
           (lambda args
             #f ;; Do nothing if file isn't there
             ))))

(define (init-recent-files)
  (set! *recent-files* (append  *flexlay-levelfiles* *recent-files*))


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

(objectmap-tool-set-popupmenu-callback 
 (lambda (menu)
   (gui-add-menu-item menu "Print Objects" 
                      (lambda () 
                        (for-each (lambda (el)
                                    (display (editor-objectmap-get-object el))
                                    (newline))
                                  (tilemap-object-tool-get-objects))))

   (gui-add-menu-item menu "Print Selection"
                      (lambda ()
                        (display (tilemap-object-tool-get-objects))
                        (newline)))

   (gui-add-menu-item menu "Delete Selection"
                      (lambda ()
                        (editor-objectmap-delete-objects *objmap* (tilemap-object-tool-get-objects))
                        (tilemap-object-tool-clear-selection)))
   
   (gui-add-menu-item menu "Duplicate Selection"
                      (lambda ()
                        (let ((lst (map (lambda (el)
                                          (editor-objectmap-duplicate-object *objmap* el))
                                        (tilemap-object-tool-get-objects))))
                          (tilemap-object-tool-set-objects *objmap* lst)
                          (display lst)(newline)
                          )))

   (gui-add-menu-item menu "Flip Screen"
                      (lambda ()
                        (for-each objmap-sprite-object-flip
                                  ;;(editor-objectmap-get-objects)
                                  (tilemap-object-tool-get-objects)
                                  )))
   ))

(variables:load *variable-file*)
(init-recent-files)

(set! *editor-map* (editor-map-component-create 0 22 screen-width (- screen-height 22)))
(set! *workspace*  (workspace-create))
(editor-map-component-set-workspace *editor-map* *workspace*)

(gui-add-on-resize-callback
 (lambda (w h)
   (cond (*editor-map*
          (set! screen-width  w)
          (set! screen-height h)
          
          (gui-component-set-rect *editor-map*
                                  0 22 screen-width (- screen-height 22))

          (cond (*minimap*
                 (let ((width  (gui-component-get-width  *minimap*))
                       (height (gui-component-get-height *minimap*)))
                   (gui-component-set-rect *minimap*
                                           (- screen-width  width) 
                                           (- screen-height height)
                                           width height))))))))

(create-menu)

(create-toolbar)
(create-tile-selector)
(create-object-inserter)

(case *game*
  ((netpanzer)
   (create-minimap 150 150)
   (object-selector-add-brush *object-selector* "sprites/spawnpoint"  '(spawnpoint))
   (object-selector-add-brush *object-selector* "sprites/outpost"     '(outpost "Unnamed"))
   (editor:add-file-plugin
    (lambda (filename) (string=? (filename:ext filename) ".npm"))
    (lambda (filename) (netpanzer:create-level-map-from-file filename)))
   (editor:add-file-plugin
    (lambda (filename) (string=? (filename:ext filename) ".opt"))
    (lambda (filename) (netpanzer:create-level-map-from-file 
                        (string-append (filename:wo/ext filename) ".npm"))))
   (editor:add-file-plugin
    (lambda (filename) (string=? (filename:ext filename) ".spn"))
    (lambda (filename) (netpanzer:create-level-map-from-file 
                        (string-append (filename:wo/ext filename) ".npm")))))
  
  ((supertux)
   (object-selector-add-brush *object-selector* 
                              (string-append *supertux:datadir* "images/shared/jumpy-left-middle-0.png")
                              '(money))
   (object-selector-add-brush *object-selector*
                              (string-append *supertux:datadir* "images/shared/snowball-left-0.png")
                              '(snowball))
   (object-selector-add-brush *object-selector*
                              (string-append *supertux:datadir* "images/shared/mriceblock-left-0.png")
                              '(mriceblock))
   (object-selector-add-brush *object-selector*
                              (string-append *supertux:datadir* "images/shared/mrbomb-left-0.png")
                              '(mrbomb))
   (object-selector-add-brush *object-selector* 
                              (string-append *supertux:datadir* "images/shared/flame-0.png")
                              '(flame))
   (object-selector-add-brush *object-selector* 
                              (string-append *supertux:datadir* "images/shared/stalactite.png")
                              '(stalactite))
   (object-selector-add-brush *object-selector*
                              (string-append *supertux:datadir* "images/shared/fish-left-0.png")
                              '(fish))

   (object-selector-add-brush *object-selector*   
                              (string-append *supertux:datadir* "images/shared/flyingsnowball-left-0.png")
                              '(flyingsnowball))

   (object-selector-add-brush *object-selector*
                              (string-append *supertux:datadir* "images/shared/bouncingsnowball-left-0.png")
                              '(bouncingsnowball))

   (object-selector-add-brush *object-selector*
                              (string-append *supertux:datadir* "images/shared/spiky-left-0.png")
                              '(spiky))

   (object-selector-add-brush *object-selector*
                         (string-append *supertux:datadir* "images/shared/resetpoint.png")
                         '(resetpoint))

   (create-minimap screen-width 50)
   (editor:add-file-plugin
    (lambda (filename) (or (string=? (filename:ext filename) ".stl")
                           (string=? (filename:ext filename) ".stlv")))
    (lambda (filename) (supertux:create-level-map-from-file filename)))
   (editor:add-file-plugin
    (lambda (filename) (string=? (filename:ext filename) ".stwm"))
    (lambda (filename) (supertux:create-worldmap-from-file filename))))
  
  ((windstille)
   (editor:add-file-plugin
    (lambda (filename) (string=? (filename:ext filename) ".scm"))
    (lambda (filename) (windstille:create-levelmap-from-file filename)))
   (create-tile-editor)
   (create-minimap 128 128))

  ((pingus)
   (editor:add-file-plugin
    (lambda (filename) (or (string=? (filename:ext filename) ".pingus")
                           (string=? (filename:ext filename) ".plf")
                           (string=? (filename:ext filename) ".xml")))
    (lambda (filename) (pingus:create-levelmap-from-file filename)))))

;;(create-brush-selector)
;;(create-netpanzer-tiler)
(cond ((equal? *game* 'netpanzer)
       (create-netpanzer-brushbox)
       (gui-hide-component *tileselector-window*)))

(set-tool 'tile)

(case *game*
  ((supertux)
   (tile-selector-set-tileset *tileselector* *level-tileset*)
   (tile-selector-set-tiles   *tileselector* (seq 1 150))
   (supertux:new-map 20 15))

  ((netpanzer)
   (tile-selector-set-tileset *tileselector* *tileset*)
   (netpanzer:new-map 20 15))

  ((pingus)
   (load-brushes "images/groundpieces/ground/crystal/")
   (load-brushes "images/groundpieces/ground/snow/")
   (load-brushes "images/groundpieces/ground/desert/")
   (pingus:new-map 800 600))

  ((windstille)
   (tile-selector-set-tileset *tileselector* *tileset*)
   (tile-selector-set-tiles   *tileselector* (seq 1 150))
   (windstille:new-map 20 15)))

;; EOF ;;
