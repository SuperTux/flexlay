(define (gui-component-toggle-visibility comp)
  (if (gui-component-is-visible comp)
      (gui-hide-component comp)
      (gui-show-component comp)))

(define-macro (gui-with-component comp . body)
  `(begin (gui-push-component ,comp)
          ,@body
          (gui-pop-component)))

;; EOF ;;
