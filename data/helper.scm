(catch #t
       (lambda () 
         (eval
          '(begin (use-modules (ice-9 readline))
                  (activate-readline))
          (current-module)))
       (lambda args 
         (display "Error: ")
         (display args)
         (newline)))

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

;; EOF ;;