(use-modules (oop goops))

(load "helper.scm")
(display "Windstille Startup Script\n")

(define *game* 'windstille)
(define *tile-size* 128)
(define *windstille-datadir* "../../data/")
(define *tileset* (tileset-create *tile-size*))

(game-load-resources (string-append *windstille-datadir* "tiles.xml"))

(define-class <windstille-level> ()
  (background #:init-value #f 
              #:init-keyword #:background
              #:accessor wslv:background)
  (foreground #:init-value #f 
              #:init-keyword #:foreground
              #:accessor wslv:foreground)
  (objects    #:init-value #f
              #:init-keyword #:objects
              #:accessor wslv:objects))

(define (windstille:load-tiles filename)
  (with-input-from-file filename
    (lambda ()
      (let* ((data (read))
             (ident (car data)))
        (cond ((equal? ident 'windstille-tiles)
               (for-each 
                (lambda (el)
                  (cond ((equal? (car el) 'tile)
                         ;;(display (cdr el))(newline)
                         (tileset-add-tile 
                          *tileset*
                          (list (list 'id   
                                      (get-value-from-tree '(id _) (cdr el) -1))
                                (list 'image 
                                      (or (get-value-from-tree '(image _) (cdr el) #f)
                                          (get-value-from-tree '(images _) (cdr el) "notile.png"))))
                          ))))
                (cdr data)))
              (else
               (error "Not a windstille tileset")))))))

(windstille:load-tiles (string-append *windstille-datadir* "tiles.scm"))

(define (windstille:new-map width height)
  (display "Creating new level...\n")
  (let ((levelmap (windstille:create-levelmap width height)))
    (editor-map-activate levelmap)
    (add-buffer levelmap)))

(define (windstille:create-levelmap width height)
  (let* ((m       (editor-map-create))
         (foreground (editor-tilemap-create *tileset* width height *tile-size*))
         (background (editor-tilemap-create *tileset* width height *tile-size*))
         (objmap  (editor-objmap-create))
         (level   (make <windstille-level>
                    #:objects    objmap
                    #:foreground foreground
                    #:background background)))

    (editor-map-add-layer m background)
    (editor-map-add-layer m foreground)
    (editor-map-add-layer m objmap)

    (editor-map-set-metadata m level)
    (windstille:activate level)

    m))

(define (windstille:create-levelmap-from-file filename)
  (let ((data (with-input-from-file filename
                (lambda () (cdr (read))))))

    (let ((width      (get-value-from-tree '(properties width _)      data 
                                           (get-value-from-tree '(tilemap width _) data 20)))
          (height     (get-value-from-tree '(properties height _)     data
                                           (get-value-from-tree '(tilemap height _) data 15)))
          (foreground (get-value-from-tree '(tilemap data)            data '()))
          (background (get-value-from-tree '(background-tilemap data) data '()))
          (objects    (get-value-from-tree '(objects)                 data '())))
      
      ;; load level file and extract tiledata and w/h
      (let* ((m       (editor-map-create))
             (tilemap    (editor-tilemap-create *tileset* width height *tile-size*))
             (bg-tilemap (editor-tilemap-create *tileset* width height *tile-size*))
             (objmap  (editor-objmap-create))
             (level   (make <windstille-level>
                        #:objects    objmap
                        #:foreground tilemap
                        #:background bg-tilemap)))

        ;; FIXME: Replace this with windstille objects handling
        (for-each (lambda (el)
                    (let ((x (car  (get-value-from-tree '(pos) (cdr el) 0)))
                          (y (cadr (get-value-from-tree '(pos) (cdr el) 0))))
                      (case (car el)
                        ((mriceblock)
                         (objectmap-add-object objmap "sprites/mriceblock" x y '(mriceblock)))
                        ((mrbomb)
                         (objectmap-add-object objmap "sprites/mrbomb" x y '(mrbomb)))
                        ((spawnpoint)
                         (objectmap-add-object objmap "sprites/spawnpoint" x y '(outpost)))
                        ((outpost)
                         (objectmap-add-object objmap "sprites/outpost"  x y '(spawnpoint)))
                        )))
                  objects)

        ;; set data to the tilemap
        (editor-tilemap-set-data tilemap foreground)
        (editor-tilemap-set-data bg-tilemap background)
        
        (editor-map-add-layer m bg-tilemap)
        (editor-map-add-layer m tilemap)
        (editor-map-add-layer m objmap)
        
        (editor-map-set-filename m filename)
        (editor-map-set-metadata m level)
        (windstille:activate level)

        m))))

(define-method (windstille:activate (level <windstille-level>))
  (tilemap-paint-tool-set-tilemap (wslv:foreground level))
  (set! *tilemap*                 (wslv:foreground level))
  (editor-tilemap-set-current     (wslv:foreground level))
  (tileset-set-current *tileset*)
  (tile-selector-set-tileset *tileselector* *tileset*))

;; EOF ;;
