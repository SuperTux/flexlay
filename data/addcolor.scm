(use-modules (ice-9 pretty-print))
(define tiles (cdr (with-input-from-file "netpanzertiles.scm"
                     (lambda ()
                       (read)))))
(define attrs  (with-input-from-file "/tmp/colors"
                 (lambda ()
                   (read))))

(define (attr2color at)
  (cond
   ((= at 0) ;; base
    (list 255   0   0 255))
   ((= at 1) ;; grass
    (list   0   0 255 255))
   ((= at 4) ;; nomove
    (list   0 255   0 255))
   ((= at 5) ;; nomove2?
    (list 255 255   0 255))
   (else
    (error "kaputt" at))))

(for-each 
 (lambda (el)
   (let* ((id (cadadr el))
          (at (append (assoc-ref attrs id) '(255) )))
     (pretty-print (append el (list (cons 'color at))))
     ))
 tiles)

;; EOF ;;