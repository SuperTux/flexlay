(use-modules (oop goops))
(load "helper.scm")
(load "xml.scm")
(display "Pingus Startup Script\n")

(define *game* 'pingus)
(define *pingus:datadir* "/home/ingo/projects/pingus/svn/branches/pingus_0_6/data/")

(define-class <pingus-level> ()
  (author #:init-value #f
          #:init-keyword #:author
          #:accessor plf:author)
  (mapsize #:init-value #f
           #:init-keyword #:mapsize
           #:accessor plf:mapsize)
  (objects #:init-value #f
           #:init-keyword #:objects
           #:accessor plf:objects))

(define (pingus:new-map width height)
  (let ((levelmap (pingus:create-levelmap width height)))
    (editor-map-activate levelmap)
    (add-buffer levelmap)))

(define-method (pingus:create-levelmap width height)
  (let* ((m      (editor-map-create))
         (objmap (editor-objmap-create)))
    
    (editor-map-add-layer m objmap)
    (editor-map-set-metadata m (make <pingus-level>
                                 #:author "Some Author"
                                 #:objects objmap))
    (editor-objectmap-set-current   objmap)

    m))

(define-method (pingus:create-levelmap-from-file filename)
  (let* ((m       (editor-map-create))
         (objmap  (editor-objmap-create))
         (mapsize (editor-mapsize-layer-create 1270 600))

         (plf (make <pingus-level>
                #:author "Some Author"
                #:mapsize mapsize
                #:objects objmap))
         
         (data (load-xml filename)))

    (editor-map-add-layer m mapsize)
    (editor-map-add-layer m objmap)    
    (editor-map-set-metadata m (make <pingus-level>
                                 #:author "Some Author"
                                 #:objects objmap))

    (editor-objectmap-set-current   objmap)
    (pingus:load-level objmap (cdddr data))
    
    m))

(define (pingus:load-level objmap lst)
  (define (parse-surface lst)
    (let ((resfile (nodeset:get-text (node:get-childs (nodeset:get (node:get-childs lst) "resource")) "resource-datafile"))
          (ident   (nodeset:get-text (node:get-childs (nodeset:get (node:get-childs lst) "resource")) "resource-ident")))
      (string-append *pingus:datadir* "/images/" 
                     (string-map (lambda (char) (if (char=? char #\-) #\/ char))
                                 resfile)
                     "/" ident ".png")))

  (define (parse-position lst)
    (let ((childs (node:get-childs lst)))
      (list (string->number (nodeset:get-text childs "x-pos"))
            (string->number (nodeset:get-text childs "y-pos"))
            (string->number (nodeset:get-text childs "z-pos")))))

  (define (parse-background lst)
    (let ((childs (node:get-childs lst)))
      (nodeset:get-text childs  "type")
      (display "Background: \n")
      (parse-position (nodeset:get childs  "position"))
      ))

  (define (parse-hotspot lst)
    #f)

  (define (parse-groundpiece lst)
    (let* ((childs (node:get-childs lst))
           (pos (parse-position (nodeset:get childs  "position"))))
        (objectmap-add-object objmap
                              (parse-surface  (nodeset:get childs  "surface"))
                              (car pos) (cadr pos) '(groundpiece))))

  (define (parse-worldobj lst)
    (let ((type (node:get-attr lst "type")))
      (cond ((string=? type "groundpiece")
             (parse-groundpiece lst))
            ((string=? type "hotspot")
             (parse-hotspot lst)))))
  
  (for-each (lambda (node)
              (let ((name (node:get-name node)))
                (cond ((string=?  name "worldobj")
                       (parse-worldobj node))
                      ((string=?  name "background")
                       (parse-background node))
                      )))
            lst))

(define (load-brushes path)
  (let ((lst (directory->list:files-only (string-append *pingus:datadir* path))))
    (for-each (lambda (filename)
                (object-selector-add-brush *object-selector*
                                           (string-append *pingus:datadir* path filename)
                                           '(groundpiece)))
              lst)))

;; EOF ;;
