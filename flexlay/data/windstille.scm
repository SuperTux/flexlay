(load "helper.scm")
(display "Windstille Startup Script\n")

(define *game* 'windstille)
(define *tile-size* 128)
;;(game-set-tilesize 128 16)
;;(game-load-tiles     "tiles.scm")

(define *tileset* (tileset-create *tile-size*))

(define *windstille-datadir* "../../data/")

(game-load-resources (string-append *windstille-datadir* "tiles.xml"))

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
    (editor-map-component-set-map *editor-map* levelmap)
    (add-buffer levelmap)))

(define (windstille:create-levelmap width height)
  (let* ((m       (editor-map-create))
         (tilemap (editor-tilemap-create *tileset* width height *tile-size*))
         (objmap  (editor-objmap-create)))

    (set! *tilemap* tilemap)

    (editor-map-add-layer m tilemap)
    (editor-map-add-layer m objmap)

    (tilemap-paint-tool-set-tilemap tilemap)
    (set! *tilemap* tilemap)
    (editor-tilemap-set-current tilemap)
    (tileset-set-current *tileset*)
    (tile-selector-set-tileset *tileselector* *tileset*)

    m))

(define (windstille:create-levelmap-from-file filename)
  (let ((data (with-input-from-file filename
                (lambda () (cdr (read))))))

    (let ((width      (get-value-from-tree '(properties width _)      data 20))
          (height     (get-value-from-tree '(properties height _)     data 15))
          (foreground (get-value-from-tree '(tilemap data)            data '()))
          (background (get-value-from-tree '(background-tilemap data) data '()))
          (objects    (get-value-from-tree '(objects)                 data '())))
      
      ;; load level file and extract tiledata and w/h
      (let* ((m       (editor-map-create))
             (tilemap (editor-tilemap-create *tileset* width height *tile-size*))
             (bg-tilemap (editor-tilemap-create *tileset* width height *tile-size*))
             (objmap  (editor-objmap-create)))

        (set! *tilemap* tilemap)

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
        
        (editor-map-add-layer m bg-tilemap)
        (editor-map-add-layer m tilemap)
        (editor-map-add-layer m objmap)
        
        (editor-map-set-filename m filename)

        (tilemap-paint-tool-set-tilemap tilemap)
        (set! *tilemap* tilemap)
        (editor-tilemap-set-current tilemap)
        (tileset-set-current *tileset*)
        (tile-selector-set-tileset *tileselector* *tileset*)

        ;; set data to the tilemap
        (if (not (null? foreground))
            (editor-tilemap-set-data tilemap foreground))
        (if (not (null? background))
            (editor-tilemap-set-data bg-tilemap background))
        m))))

;; EOF ;;
