(use-modules (srfi srfi-1))

(define (node:get-name node)
  (cadr node))

(define (node:get-attributes node)
  (caddr node))

(define (node:get-attr node name)
  (assoc-ref (node:get-attributes node) name))

(define (node:get-childs node)
  (cdddr node))

(define (nodeset:get nodes name)
  (find (lambda (node)
          (string=? (node:get-name node) name))
        nodes))

(define (nodeset:get-text nodes name)
  (let* ((node    (nodeset:get nodes name)))
    (cond (node
           (let ((content (car (node:get-childs node))))
             (if (equal? (car content) 'text-data)
                 (cadr content)
                 #f)))
          (else 
           #f))))
    
;; EOF ;;
