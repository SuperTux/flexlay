(define (get-keyword-value args keyword default)
  (let ((kv (memq keyword args)))
    (if (and kv (>= (length kv) 2))
	(cadr kv)
	default)))

;; Return a list containing all the objects which match 'pred?'
(define (filter pred? objects)
  (let loop ((objs objects)
	     (result '()))
    (cond ((null? objs) (reverse! result))
	  ((pred? (car objs)) (loop (cdr objs) (cons (car objs) result)))
	  (else (loop (cdr objs) result)))))

;; Return a list of the strings which match the regex 'rx'
(define (grep rx strings)
  (let ((r (make-regexp rx)))
    (filter (lambda (x) (regexp-exec r x)) strings)))

(define (print . str)
  (for-each (lambda (x) (display x)) str)
  (force-output))

(define (printerr . str)
  (for-each (lambda (x) (display x (current-error-port))) str))

(define (println-sep . str)
  (for-each (lambda (x) 
	      (display "\"")
	      (display x)
	      (display "\"")) str)
  (newline))

;; Print the given strings and a newline
(define (println . str)
  (apply print str) (newline))

(define (printerrln . str)
  (apply printerr str) (newline (current-error-port)))


(define (split str char)
  (let ((index (string-index str char)))
    (cond ((not index)
	   (cons str '()))
	  (else
	   (cons (substring str 0 index)
		 (split (substring str (1+ index)) char))))))



(define (file->list filename)
  (let ((port (open-input-file filename))
	(lst  '()))
    (let loop ((line (read-line port)))
      (cond ((not (eof-object? line))
	     (set! lst (cons line lst))
	     (loop (read-line port)))))
    (close port)
    (reverse lst)))

(define (assoc->hash-table lst)
  (let* ((lst-length (length lst))
	 (table  (make-vector (* 2 lst-length))))
    (for-each (lambda (el)
		(hash-set! table (car el) (cdr el)))
	      lst)
    table))

(define (passwd-file->assoc filename)
  (let* ((passwd-lst (sort (map (lambda (line) 
				  (split line #\:))
				(file->list filename))
			   (lambda (a b) ;; sort after the third element
			     (string<=? (list-ref a 3)
					(list-ref b 3)))))
	 (user/crypt-lst (map (lambda (el)
			    (cons (car el) (cadr el)))
			      passwd-lst)))
    user/crypt-lst))

(define (command->list command)
  (let ((pipe (open-input-pipe command))
	(lst  '()))
    (let loop ((line (read-line pipe)))
      (cond ((not (eof-object? line))
	     (set! lst (cons line lst))
	     (loop (read-line pipe)))))
    (reverse lst)))

(define (passwd-file->hash-table filename)
  (assoc->hash-table (passwd-file->assoc filename)))

(define (crypt salt passwd)
  (let* ((port   (open-input-pipe (string-append "mkpasswd --salt=" salt " " passwd)))
	 (result (read-line port)))
    (close port)
    result))

;; EOF ;;