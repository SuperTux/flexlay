(define (gui-component-toggle-visibility comp)
  (if (gui-component-is-visible comp)
      (gui-hide-component comp)
      (gui-show-component comp)))

(define-macro (gui-with-component comp . body)
  `(begin (gui-push-component ,comp)
          ,@body
          (gui-pop-component)))

(define (create-property-editor title lst func)
  (let ((window (gui-create-window 200 200 300 (+ 75 (* (length lst) 25)) title)))
    (gui-push-component (gui-window-get-client-area window))
    (let* ((y -20)
           (comps (map (lambda (el)
                         (set! y (+ y 25))
                         (gui-create-label     10 y (list-ref el 0))
                         (gui-create-inputbox 100 y 180 25 (format #f "~a" (list-ref el 2))))
                       lst)))
      (set! y (+ y 35))

      (gui-create-button-func 150 y 55 25 "Cancel"
                              (lambda () (gui-hide-component window)))
      (gui-create-button-func 
       220 y 55 25 "Ok"
       (lambda ()
         (apply func
                (map (lambda (comp el)
                       (case (list-ref el 1)
                         ((int)
                          (string->number (gui-inputbox-get-text comp)))
                         ((string)
                          (gui-inputbox-get-text comp))))
                     comps lst)))))
    (gui-pop-component)))

;; EOF ;;
