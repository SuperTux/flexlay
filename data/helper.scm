(cond (#t
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

(load "gui.scm")

;; EOF ;;
