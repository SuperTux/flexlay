(use-modules (ice-9 pretty-print))

(load "helper.scm")

(define screen-width  (screen-get-width))
(define screen-height (screen-get-height))
(define empty (lambda () #f))
(define *editor-map* #f)
(define *editor-variables* '())
(define *tileeditor* #f)
(define *brushes* '())
(define *tileeditor-window* #f)
(define *brush-selector* #f)
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
(define *tilemap* #f)
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
  (cond ((null? *recent-files*)
         "../data/levels/newlevel.scm")
        (else
         (car *recent-files*))))

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
         (tilemap (editor-tilemap-create width height *tile-size*))
         (objmap  (editor-objmap-create)))

    (set! *tilemap* tilemap)

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

(define (netpanzer:create-level-map-from-file filename)
  (let* ((file (load-netpanzer-map filename))
         (m       (editor-map-create))
         (objmap  (editor-objmap-create)))

    (editor-map-set-metadata m (list
                                (cons 'id-header   (NetPanzerFileStruct-id-header-get file))
                                (cons 'name        (NetPanzerFileStruct-name-get file))
                                (cons 'description (NetPanzerFileStruct-description-get file))))

    (editor-map-add-layer m (NetPanzerFileStruct-tilemap-get file))
    (editor-map-add-layer m objmap)

    (cond ((equal? *game* 'netpanzer)
           (let* ((rawname (substring filename 0 (- (string-length filename) 4)))
                  (optname (string-append rawname ".opt"))
                  (spnname (string-append rawname ".spn")))
             
             ;; Generate outposts
             (for-each 
              (lambda (el)
                (objectmap-add-object objmap "sprites/outpost"
                                      (+ (* (string->number (cadr el))  32) 16)
                                      (+ (* (string->number (caddr el)) 32) 16)
                                      (list 'outpost (car el))))
              (parse-netpanzer-opt-file optname))
             
             ;; Generate spawnpoints
             (for-each
              (lambda (el)
                (objectmap-add-object objmap "sprites/spawnpoint"
                                      (+ (* (string->number (car el)) 32) 16)
                                      (+ (* (string->number (cadr el)) 32) 16)
                                      '(spawnpoint)))
              (parse-netpanzer-spn-file spnname)))))
    
    (editor-map-set-filename m filename)
    m))

(define (create-level-map-from-file filename)
  (let ((data (with-input-from-file filename
                (lambda () (cdr (read))))))

    (let ((width      (get-value-from-tree '(properties width _)      data 20))
          (height     (get-value-from-tree '(properties height _)     data 15))
          (foreground (get-value-from-tree '(tilemap data)            data '()))
          (background (get-value-from-tree '(background-tilemap data) data '()))
          ;;          (diamonds   (get-value-from-tree '(diamond-map)             data '()))
          (objects    (get-value-from-tree '(objects)                 data '())))
      
      ;; load level file and extract tiledata and w/h
      (let* ((m       (editor-map-create))
             (tilemap (editor-tilemap-create width height *tile-size*))
             (objmap  (editor-objmap-create)))

        (set! *tilemap* tilemap)

        (for-each (lambda (el)
                    (let ((x (car  (get-value-from-tree '(pos) (cdr el) 0)))
                          (y (cadr (get-value-from-tree '(pos) (cdr el) 0))))
                      (case (car el)
                        ((mriceblock)
                         (objectmap-add-object objmap "sprites/mriceblock" x y '(mriceblock)))
                        ((mrbomb)
                         (objectmap-add-object objmap "sprites/mrbomb" x y '(mrbomb)))
                        ((spawnpoint)
                         (objectmap-add-object objmap "sprites/spawnpoint" x y '(outpost)))
                        ((outpost)
                         (objectmap-add-object objmap "sprites/outpost"  x y '(spawnpoint)))
                        )))
                  objects)
        
        (editor-map-add-layer m tilemap)
        (editor-map-add-layer m objmap)
        
        (editor-map-set-filename m filename)

        ;; set data to the tilemap
        (if (not (null? foreground))
            (editor-tilemap-set-data tilemap foreground))
        ;;        (if (not (null? background))
        ;;            (editor-tilemap-set-data tilemap 0 background))
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

(define (netpanzer:load-map filename)
  (catch #t
         (lambda ()
           (let ((levelmap (netpanzer:create-level-map-from-file filename)))
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

(define (netpanzer:save-map filename)
  ;; Save .npm
  (let* ((levelmap (editor-map-component-get-map *editor-map*))
         (metadata (editor-map-get-metadata levelmap)))
    (save-netpanzer-map filename
                        levelmap
                        (or (assoc-ref metadata 'id-header) "Created with Windstille Editor")
                        (or (assoc-ref metadata 'name) "<no name>")
                        (or (assoc-ref metadata 'description) "<no description>")))
  
  ;; Save .opt/.spn
  (let* ((rawname (substring filename 0 (- (string-length filename) 4)))
         (optname (string-append rawname ".opt"))
         (spnname (string-append rawname ".spn"))
         (spawnpoints '())
         (outposts    '()))
    
    (for-each (lambda (ref)
                (let ((el (editor-objectmap-get-object ref)))
                  (case (caaddr el)
                    ((outpost)
                     (set! outposts    (cons el outposts)))
                    ((spawnpoint)
                     (set! spawnpoints (cons el spawnpoints)))
                    (else
                     (display "Unknown: ")
                     (display el)
                     (newline)))))
              (editor-objectmap-get-objects))

    (set! spawnpoints (reverse spawnpoints))
    (set! outposts    (reverse outposts))

    (with-output-to-file optname
      (lambda ()
        (format #t "ObjectiveCount: ~a~%" (length outposts))
        (newline)

        (for-each (lambda (el)
                    (format #t "Name: ~a~%" "Foobar");;(car el))
                    (format #t "Location: ~a ~a~%" 
                            (quotient (car el)  32)
                            (quotient (cadr el) 32))
                    (newline))
                  outposts)))

    (with-output-to-file spnname
      (lambda ()
        (format #t "SpawnCount: ~a~%" (length spawnpoints))

        (for-each (lambda (el)
                    (format #t "Location: ~a ~a~%" 
                            (quotient (car el)  32)
                            (quotient (cadr el) 32)))
                  spawnpoints)))))

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
      (write-field "   " (map-get-width) (editor-tilemap-get-data *tilemap*))
      (display     "   ))\n\n")

      ;; FIXME: Currently not supported
      ;;      (display   "  (background-tilemap (data\n")
      ;;      (write-field "   " (map-get-width) (map-get-data 0))
      ;;     (display   "   ))\n\n")

      ;;      (format #t "  (diamond-map\n")
      ;;      (write-field "   " 
      ;;                   (map-get-width)
      ;;                   (diamond-map-get-data))
      ;;      (display   " )\n")

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
    (gui-add-menu-item menu "File/Open NetPanzer.." 
                       (lambda ()
                         (simple-file-dialog "Load a level..." (get-last-file)
                                             (lambda (filename)
                                               (netpanzer:load-map filename)))))


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

    (gui-add-menu-item menu "File/Save Netpanzer" 
                       (lambda ()
                         (simple-file-dialog "Save a level..." (get-last-file)
                                             (lambda (filename) 
                                               (netpanzer:save-map filename)
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
    (gui-add-menu-item menu "Dialogs/Edit Metadata" (lambda () (create-metadata-editor)))

    (gui-add-menu-item menu "Dialogs/Draw Grid" editor-toggle-grid)
    (gui-add-menu-item menu "Dialogs/Draw Attributes" editor-toggle-attributes)
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
                (or (editor-map-get-metadata (editor-map-component-get-map *editor-map*))
                    '()))
      
      (gui-create-button-func 300 140 75 25 "Ok"
                              (lambda ()
                                (editor-map-set-metadata (editor-map-component-get-map *editor-map*)
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
                            40 25 "BG" 
                            (lambda () #f));;(tilemap-set-active-layer 0)))

    (gui-create-button-func 0 175
                            40 25 "FG" 
                            (lambda () #f))

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
      ((netpanzer)
       (display "Netpanzer\n")
       (tile-selector-create (- screen-width (* 20 32)) 0 20 15 1.0))
      (else
       (tile-selector-create (- screen-width (* 3 64)) 0 3 8 .5)))

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

(define (seq start end)
  (let loop ((ret '())
             (start start)
             (end   end))
    (cond ((< start end)
           (set! ret (cons start ret))
           (loop ret (+ start 1) end))
          (else
           (reverse ret)))))

(define (create-netpanzer-brushbox)
  (let ((window (gui-create-window (- screen-width 200) 25 200 400 "netPanzer Brushbox")))
    (gui-push-component (gui-window-get-client-area window))

    (let* ((listbox (gui-listbox-create 10 5 175 360))
           (objects (with-input-from-file "netpanzer-tile-objects.txt"
                      (lambda ()
                        (read)))))
      (for-each (lambda (el)
                  (gui-listbox-add listbox (format #f "~a - ~ax~a"
                                                   (cadddr el)
                                                   (cadr el)
                                                   (caddr el))))
                objects)
      (gui-listbox-on-click listbox
                            (lambda (index)
                              
                              (let* ((obj (list-ref objects index))
                                     (start  (car obj))
                                     (width  (cadr obj))
                                     (height (caddr obj))
                                     (opaque #t))
                                (tilemap-paint-tool-set-brush
                                 (list width height opaque
                                       (list->vector (seq start (+ start (* width height)))))))))
      )
    (gui-component-on-close window (lambda ()
                                     (gui-hide-component window)))
    (gui-pop-component)))

(define (create-netpanzer-tiler)
  (let ((window (gui-create-window 100 100 800 600 "netPanzer Tiler")))
    (gui-push-component (gui-window-get-client-area window))

    (gui-create-label    10 10 "Width: ")
    (gui-create-label    10 50 "Height: ")
    (gui-create-label    10 90 "Start: ")

    (let* ((width         8)
           (height        6)
           (start         0)
           (index         0)
           (objects       (with-input-from-file "netpanzer-tile-objects.txt"
                            (lambda ()
                              (read))))
           
           (width+-button   (gui-create-button   10 270 20 25 "W+"))
           (width--button   (gui-create-button   40 270 20 25 "W-"))

           (height+-button   (gui-create-button   10 300 20 25 "H+"))
           (height--button   (gui-create-button   40 300 20 25 "H-"))

           (update-button (gui-create-button   10 150 50 25 "Update"))
           (update2-button (gui-create-button   10 420 50 25 "Update2"))
           (bug-button (gui-create-button   10 360 50 25 "Bug"))
           (dump-button   (gui-create-button   10 240 50 25 "Dump"))
           
           (save-button   (gui-create-button   10 390 50 25 "Save"))

           (next-button   (gui-create-button       10 210 50 25 "Next"))
           (previous-button   (gui-create-button   10 180 50 25 "Previous"))
           (start-box     (gui-create-inputbox 10 110 50 25 (number->string start)))
           (height-box    (gui-create-inputbox 10 70 50 25 (number->string height)))
           (width-box     (gui-create-inputbox 10 30 50 25 (number->string width)))
           (index-box     (gui-create-inputbox 10 330 50 25 (number->string index)))
           (map-component (editor-map-component-create 75 10 730 550))
           (levelmap      (editor-map-create))
           (tilemap       (editor-tilemap-create 10 10 32)))
      
      (set! *tilemap* tilemap)

      (define (update-from-boxes)
        (catch #t
               (lambda ()
                 (set! index  (string->number (gui-inputbox-get-text index-box)))
                 (set! width  (string->number (gui-inputbox-get-text width-box)))
                 (set! height (string->number (gui-inputbox-get-text height-box)))
                 (set! start  (string->number (gui-inputbox-get-text start-box))))
               (lambda args
                 (display "Error: ")
                 (display args)
                 (newline))))

      (define (goto-bug)
        (let ((next  0)
              (count 0)
              (muindex #f)
              (lst objects)
              (done #f))

          (while (and (pair? lst) (not done))
                 (set! count (+ count 1))
                 (let* ((el (car lst))
                        (start  (car el))
                        (width  (cadr el))
                        (height (caddr el)))

                   (cond ((not (= next start))
                          (if (not muindex)
                              (set! muindex count))
                          (format #t "Error at: ~a: got: ~a wanted: ~a - ~a~%" count next start el)
                          (set! done #t)
                          ))

                   (set! next (+ start (* width height))))
                 (set! lst (cdr lst)))

          (if muindex
              (set! index muindex))
          (display "Goto index: ")(display index)(newline)
          (update-index)
          (update)))

      (define (update-to-boxes)
        (gui-inputbox-set-text index-box  (number->string index))
        (gui-inputbox-set-text width-box  (number->string width))
        (gui-inputbox-set-text height-box (number->string height))
        (gui-inputbox-set-text start-box  (number->string start)))

      (define (do-update)
        (update-from-boxes)
        (update))

      (define (do-update2)
        (update-from-boxes)
        (update-index)
        (update))

      (define (update)
        (update-to-boxes)
        (tilemap-resize tilemap 0 0 width height)
        (editor-tilemap-set-data tilemap 1 (seq start (+ start (* width height)))))

      (define (update-index)
        (set! width  (cadr  (list-ref objects index)))
        (set! height (caddr (list-ref objects index)))
        (set! start  (car   (list-ref objects index))))

      (define (do-next)
        (set! index (+ index 1))
        ;;(set! start (+ start (* width height)))
        ;;(editor-tilemap-set-data tilemap 1 (seq start (+ start (* width height))))
        (update-index)
        (update))

      (define (do-previous)
        (set! index (- index 1))
        ;;(set! start (- start (* width height)))
        ;;(editor-tilemap-set-data tilemap 1 (seq start (+ start (* width height))))
        (update-index)
        (update))

      (define (do-save)
        (list-set! objects index (list start width height)))

      (define (do-dump)
        ;;(set! objects (cons (list start width height) 
        ;;                    objects))
        (format #t "Object: ~a ~ax~a~%" start width height)
        (with-output-to-file "netpanzer-tile-objects.txt"
          (lambda ()
            (display ";; netPanzer Objects in tiles\n")
            (display ";; Start Width Height\n")
            (pretty-print objects))))

      (editor-map-add-layer levelmap tilemap)
      (editor-map-component-set-map map-component levelmap)

      (gui-component-on-click update-button   do-update)
      (gui-component-on-click update2-button   do-update2)
      (gui-component-on-click next-button     do-next)
      (gui-component-on-click previous-button do-previous)
      (gui-component-on-click dump-button     do-dump)
      (gui-component-on-click save-button     do-save)
      (gui-component-on-click bug-button     goto-bug)

      (gui-component-on-click width+-button  (lambda ()
                                               (set! width (+ width 1))
                                               (update)))
      (gui-component-on-click width--button  (lambda ()
                                               (set! width (- width 1))
                                               (update)))

      (gui-component-on-click height+-button  (lambda ()
                                                (set! height (+ height 1))
                                                (update)))
      (gui-component-on-click height--button  (lambda ()
                                                (set! height (- height 1))
                                                (update)))
      )
    
    (gui-component-on-close window (lambda ()
                                     (gui-hide-component window)))
    (gui-pop-component)))

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
             #f ;; Do nothing if file isn't there
             ))))

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

(objectmap-tool-set-popupmenu-callback 
 (lambda (menu)
   (gui-add-menu-item menu "Print Objects" 
                      (lambda () 
                        (for-each (lambda (el)
                                    (display (editor-objectmap-get-object el))
                                    (newline))
                                  (tilemap-object-tool-get-objects))))

   (gui-add-menu-item menu "Add Object"
                      (lambda ()
                        (editor-objectmap-add-object "sprites/mrbomb" 100 100 '())))

   (gui-add-menu-item menu "Print Selection"
                      (lambda ()
                        (display (tilemap-object-tool-get-objects))
                        (newline)))

   (gui-add-menu-item menu "Delete Selection"
                      (lambda ()
                        (editor-objectmap-delete-objects (tilemap-object-tool-get-objects))
                        (tilemap-object-tool-clear-selection)))
   
   (gui-add-menu-item menu "Duplicate Selection"
                      (lambda ()
                        (let ((lst (map editor-objectmap-duplicate-object
                                        (tilemap-object-tool-get-objects))))
                          (tilemap-object-tool-set-objects lst)
                          (display lst)(newline)
                          )))

   (gui-add-menu-item menu "Flip Screen"
                      (lambda ()
                        (for-each objmap-sprite-object-flip
                                  ;;(editor-objectmap-get-objects)
                                  (tilemap-object-tool-get-objects)
                                  )))
   ))

(load-variables)
(init-recent-files)

(set! *editor-map* (editor-map-component-create 0 22 screen-width (- screen-height 25)))
(create-menu)

(create-toolbar)
(create-tile-editor)
(create-tile-selector)
(create-object-inserter)

(case *game*
  ((netpanzer)
   (create-minimap 150 150)
   (object-selector-add-brush *object-selector* "sprites/spawnpoint"  '(spawnpoint))
   (object-selector-add-brush *object-selector* "sprites/outpost"     '(outpost "Unnamed"))
   )
  (else
   (create-minimap screen-width 50)))

;;(create-brush-selector)
;;(create-netpanzer-tiler)
(cond ((equal? *game* 'netpanzer)
       (create-netpanzer-brushbox)
       (gui-hide-component *tileselector-window*)))

(set-tool 'tile)

;;(object-selector-add-brush *object-selector* "sprites/mriceblock" '(mriceblock))
;;(object-selector-add-brush *object-selector* "sprites/mrbomb"     '(mrbomb))

(new-map 60 15)

;; EOF ;;
