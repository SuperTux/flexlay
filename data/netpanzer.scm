(use-modules (srfi srfi-13)
             (ice-9 pretty-print))

(display "netPanzer Startup Script: ...\n")

(define *game* 'netpanzer)
(define *tile-size* 32)
(game-set-tilesize 32 16)
(game-load-resources "netpanzertiles.xml")
(game-load-resources "netpanzersprites.xml")
(game-load-tiles     "netpanzertiles.scm")

(set-window-title "Windstille Editor - netPanzer Mode")

(display "netPanzer Startup Script: done\n")

(define (tokenize-input)
  (let ((line   (read-line)))
    (cond ((eof-object? line)
           '())
          (else
           (let ((tokens (string-tokenize line)))
             (cond ((null? tokens) ;; Ignore empty lines
                    (tokenize-input))
                   (else
                     (cons tokens
                           (tokenize-input)))))))))

;; filename -> ((x y) ...)
(define (parse-netpanzer-spn-file filename)
  (with-input-from-file filename
    (lambda ()
      (let* ((tokens (tokenize-input))
             (res '()))
        (for-each 
         (lambda (el)
           (cond ((string=? "SpawnCount:" (car el))
                  #f)
                 ((string=? "Location:" (car el))
                  (set! res (cons (list (cadr el) (caddr el))
                                  res)))
                 (else
                  (display "Unknown token: ")
                  (display el)
                  (newline))))
         tokens)
        (reverse res)))))

;; filename -> ((name x y) ...)
(define (parse-netpanzer-opt-file filename)
  (with-input-from-file filename
    (lambda ()
      (let* ((tokens (tokenize-input))
             (res '())
             (name ""))
        (for-each 
         (lambda (el)
           (cond ((string=? "ObjectiveCount:" (car el))
                  #f)
                 ((string=? "Name:" (car el))
                  (set! name (cadr el)))
                 ((string=? "Location:" (car el))
                  (set! res (cons (list name (cadr el) (caddr el)) res)))
                 (else
                  (display "Unknown token: ")
                  (display el)
                  (newline))))
         tokens)
        (reverse res)))))

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
                                       (list->vector (seq start (+ start (* width height))))))))))
    (gui-component-on-close window (lambda ()
                                     (gui-hide-component window)))
    (gui-pop-component)))

;; Editor Windows that allows to comfortable browse a raw stream of
;; tiles and split it into brushes
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

;; EOF ;;
