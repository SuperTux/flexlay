;  For the impatient (ie me):
;
;  This implements a simple looping structure.
;
;  [[syntax]] (for (<counter> <start> <finish>) <body> ...)
;             (for (<counter> <start> <finish> <step>) <body> ...)
;
;      For is an iteration construct.  It specifies the variable <counter>
;      to take values between <start> and <finish>.  If <step> is provided
;      then <counter> is incremented by that value between each iteration,
;      otherwise, it changes by 1.  If <start> is less than <finish> then
;      <counter> will increase with each iteration.  If <start> is greater
;      than <finish> then <counter> will decrease, and if <start> is equal
;      to <finish> the <body> will be executed once for the value of
;      <start>. 
; 
;  [[example]]
;     (for (i 1 3) 
;        (display i) 
;        (display " "))
;            will print "1 2 3".
;
;     (for (i 3 1) 
;        (display i) 
;        (display " "))
;            will print "3 2 1".
;
;     (for (i 1.0 4.0 1.5)
;        (display i)
;        (display " "))
;            will print "1.0 2.5 4.0".
;     

;  For the curious:
;
;  This is an example for the implementation of a simple syntactic
;  extension for guile.  If instead you only want to use the extension
;  presented here, you can simply include it in its optimized form, as it
;  comes with the guile distribution.  Just type "(use-modules (ice-9
;  convenience))".
;
;  If you're new to scheme, you've probably noticed (and missed) the simple
;  iterative loop:
;
;  perl:
;    for $i (1..10) {
;       print $i;
;    }
;
;  C:
;    for (i=1; i<=10; ++i) {
;       printf("%d",i);
;    }
;
;  fortran:  
;    do i = 1, 10
;       print *, i
;    enddo
;
;  Scheme has the "do" special form and loops really should be implemented
;  using tail recursion, but the common case where a counter increments
;  between a low and a high bound is a little tricky.  Compare:
;
;    (do ((i 1 (+ i 1)))
;        ((> 5) #t)
;        (display i))
;
;  It's terse, but there's very little eye candy.  Fortunately, it's easy
;  to add a macro to scheme so this can become:
;
;    (simple-for (i 1 10)
;                (display i))
;
;  This can be implemented using hygenic macros defined in R5RS:

(use-modules (ice-9 syncase))

;; Add a new simple syntax (aka a "special form") to scheme to make loops a
;; little easier to use.  There is a more complete version with error
;; checking included in (ice-9 convenience)
(define-syntax simple-for 
  (syntax-rules ()
    ;; A template for a simple-for loop.  When scheme reads the code
    ;; "(simple-for (<count> <start> <finish>) <body>)"  it will be
    ;; translated into a DO loop.  The <count> will run between <start> and
    ;; <finish> (including the boundaries).
    ((simple-for (<counter> <start> <finish>) <body> ...)

     ;; How the simple-for template is translated into a DO loop.
     (do ((<counter> <start> (+ <counter> 1)))
	 ((< <finish> <counter>) #t)
       <body> ...))))
;
;  Using this macro:
;      (simple-for (i 1 10) 
;         (display i) 
;         (newline))
;  becomes:
;      (do ((i 1 (+ i 1)))
;          ((< 10 i) #t)
;        (display i)
;        (newline))
;
;  The "simple-for" macro is nice for simple situations, but it's a little
;  too simple for the general case.  For instance it only handles loops
;  where the <counter> variable is increasing.  It's nice if
;
;     (for (i 1 3) (display i))
;
;  prints "123" and 
;
;     (for (i 3 1) (display i)) 
;
;  prints "321".  It's also good if the step size can be set so that
;
;     (for (i 1.0 4.0 1.5) (display i) (display " "))
;
;  prints "1.0 2.5 4.0".
;
;  Here's a full implementation.  This handles both forms of syntax (with
;  or without a step size) for both incrementing and decrementing loops.
;  This is the version that is document in the executive summary at the
;  top. 
;
(define-syntax for
  (syntax-rules ()
    ;; Handle the "normal" case.  This is used when the evaluator stumbles
    ;; across "(for (i 1 5) body)" or "(for (i 5 1) body)".
    ((for (<counter> <start> <finish>) <body> ...)
     (do
	 ;; Set the initial conditions for the loop.
	 ((<counter>			; The name of the counter
	   <start>			; The initial value of the counter
	   (if (> <finish> <start>)		; The increment. This checks the
	       (+ <counter> 1)		; sign so the loop goes the right 
	       (- <counter> 1))))		; direction.
	 ;; Decide when the loop should terminate.
	 ((if (< <start> <finish>)
	      (< <finish> <counter>)	; An incrementing loop.
	      (< <counter> <finish>))	; A decrementing loop.
	  #t)
       ;; The code that gets executed at each iteration will go here.  
       <body> ...))

    ;; Handle a for loop with a user defined step.  This is used when the 
    ;; evalulator stumbles across "(for (i 1 5 2) body)"
    ((for (<counter> <start> <finish> <step>) <body> ...)
     (do 
	 ;; Set the initial conditions for the loop.
	 ((<counter>			; The name of the counter.
	   <start>			; The initial value of the counter.
	   (+ <counter> <step>)))	; The increment for each iteration.
	 ;; Decide when the loop should terminate.
	 ((if (< <start> <finish>)
	      (or (< <counter> <start>)	; An incrementing loop
		  (< <finish> <counter>))
	      (or (> <counter> <start>)	; A decrementing loop.
		  (> <finish> <counter>))) 
	  #t)
       ;; The code that gets executed at each iteration will go here.  
       <body> ...))))

