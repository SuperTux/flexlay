(use-modules (oop goops))
(load "helper.scm")
(display "Pingus Startup Script\n")

(define *game* 'pingus)
(define *pingus:datadir* "/home/ingo/projects/pingus/svn/branches/pingus_0_6/data/")

(define-class <pingus-level> ()
  (author #:init-value #f
          #:init-keyword #:author
          #:accessor plf:author)
  (objects #:init-value #f
           #:init-keyword #:objects
           #:accessor plf:objects))

(define-method (pingus:create-levelmap width height)
  (let* ((m      (editor-map-create))
         (objmap (editor-objmap-create)))
    
    (editor-map-add-layer m objmap)
    (editor-map-set-metadata m (make <pingus-level>
                                 #:author "Some Author"
                                 #:objects objmap))

    m))

(define-method (pingus:create-levelmap-from-file filename)
  (let* ((m      (editor-map-create))
         (objmap (editor-objmap-create))

         (plf (make <pingus-level>
                #:author "Some Author"
                #:objects objmap))
         
         (data (load-xml filename)))

    (editor-map-add-layer m objmap)    
    (editor-map-set-metadata m (make <pingus-level>
                                 #:author "Some Author"
                                 #:objects objmap))

    (pingus:load-level objmap (cdddr data))
    
    m))

(define (pingus:load-level objmap lst)
  (for-each (lambda (el)
              (let* ((element    (cadr  el))
                     (attributes (caddr el))
                     (childs     (cdddr el)))
                (format #t "~a: ~a ~a~%" element attributes childs)
                ))
            lst))

;; EOF ;;
