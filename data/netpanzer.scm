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

;; EOF ;;
