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

;; EOF ;;
