(define (seq start end)
  (let loop ((ret '())
             (start start)
             (end   end))
    (cond ((< start end)
           (set! ret (cons start ret))
           (loop ret (+ start 1) end))
          (else
           (reverse ret)))))

(cond (#f
       (catch #t
              (lambda () 
                (eval
                 '(begin (use-modules (ice-9 readline))
                         (activate-readline))
                 (current-module)))
              (lambda args 
                (display "Error: ")
                (display args)
                (newline)))))

(define (windstille:repl)
  (display "### Windstille repl, exit with (quit)\n")
  (let ((old-prompt scm-repl-prompt))
    (set-repl-prompt! "windstille> ")
    (catch #t
           (lambda ()
             (top-repl)
             (display "Windstille Readline exited nicly.\n"))
           (lambda args
             (display "Error: ")
             (display args)(newline)))
    (set-repl-prompt! old-prompt)))

(define (filter pred? objects)
  (let loop ((objs objects)
	     (result '()))
    (cond ((null? objs) (reverse! result))
	  ((pred? (car objs)) (loop (cdr objs) (cons (car objs) result)))
	  (else (loop (cdr objs) result)))))

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

(load "gui.scm")

;; EOF ;;
