(define (gui-component-toggle-visibility comp)
  (if (gui-component-is-visible comp)
      (gui-hide-component comp)
      (gui-show-component comp)))

;; EOF ;;