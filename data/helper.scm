(use-modules (srfi srfi-13))

(define (directory->list:files-only path)
  (filter (lambda (el)
            (equal? (stat:type (stat (string-append path el))) 'regular))
          (directory->list path)))

(define (directory->list path)
  (let* ((dir (opendir path))
	 (lst '()))
    (let loop ((fobj (readdir dir)))
      (cond ((not (eof-object? fobj))
	     (set! lst (cons fobj lst))
	     (loop (readdir dir)))))

    (closedir dir)
    (reverse lst)))

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

(define (filename:ext filename)
  (let ((i (string-index-right filename #\.)))
    (if (and i (> i 0))
        (substring filename i)
        #f))) ;; doesn't have an extension

(define (filename:wo/ext filename)
  (let ((i (string-index-right filename #\.)))
    (if (and i (> i 0))
        (substring filename 0 i) 
        filename))) ;; filename doesn't have extension

(load "gui.scm")

;; EOF ;;
