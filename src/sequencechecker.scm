

(define objects 
                 (with-input-from-file "netpanzer-tile-objects.txt"
                   (lambda ()
                     (read))))


(let ((next 0))
  (for-each
   (lambda (el)
     (let ((start (car el))
           (width (cadr el))
           (height (caddr el)))
       (cond ((not (= next start))
              (display "Error at: ")
              (display el)
              (display " ")
              (display next)
              (display " ")
              (display start)
              (newline)))
       (set! next (+ start (* width height)))))
     objects))