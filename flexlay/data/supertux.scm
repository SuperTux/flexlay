(use-modules (oop goops))
(load "helper.scm")
(display "SuperTux Startup Script\n")

(define *game* 'supertux)
(define *tile-size* 32)
(define *supertux:datadir* "/home/ingo/cvs/supertux/supertux/data/")
(game-load-resources "tuxtiles.xml")
(game-load-resources "tuxsprites.xml")

(define *worldmap-tileset* (tileset-create *tile-size*))
(define *level-tileset*    (tileset-create *tile-size*))

(define (supertux:load-tiles filename)
  (with-input-from-file filename
    (lambda ()
      (let* ((data (read))
             (ident (car data)))
        (cond ((equal? ident 'supertux-tiles)
               (for-each (lambda (el)
                           (cond ((equal? (car el) 'tile)
                                  ;;(display (cdr el))(newline)
                                  (tileset-add-tile *level-tileset*
                                   (list (list 'id   
                                               (get-value-from-tree '(id _) (cdr el) -1))
                                         (list 'image 
                                               (string-append *supertux:datadir* "images/tilesets/" 
                                                              (or (get-value-from-tree '(editor-images _) (cdr el) #f)
                                                                  (get-value-from-tree '(images _) (cdr el) "notile.png"))))
                                         )))))
                         (cdr data)))
              (else
               (error "Not a supertux tileset")))))))

(define (supertux:load-worldmap-tiles filename)
  (with-input-from-file filename
    (lambda ()
      (let* ((data (read))
             (ident (car data)))
        (cond ((equal? ident 'supertux-worldmap-tiles)
               (for-each (lambda (el)
                           (cond ((equal? (car el) 'tile)
                                  (tileset-add-tile *worldmap-tileset*
                                   (list (list 'id   
                                               (get-value-from-tree '(id _) (cdr el) -1))
                                         (list 'image 
                                               (string-append *supertux:datadir* "images/worldmap/"
                                                              (get-value-from-tree '(image _) (cdr el) "notile.png"))))
                                   ))))
                         (cdr data)))
              (else
               (error "Not a supertux worldmap tileset")))))))

(supertux:load-worldmap-tiles (string-append *supertux:datadir* "images/worldmap/antarctica.stwt"))
(supertux:load-tiles (string-append *supertux:datadir* "images/tilesets/supertux.stgt"))

;;(tileset-add-tile '((id 6)
;;                    (image "/home/ingo/projects/windstille/trunk/data/images/tuxsprites/mrbomb.png")
;;                    (colmap   0   0   0   0   0   0   0   0)))

(set-window-title "Flexlay - SuperTux Mode")

;; Metadata for a SuperTux level
(define-class <supertux-level> ()
  (name   #:init-value "Hello World"
          #:accessor supertux:name)
  (author #:init-value ""
          #:accessor supertux:author)
  ;;  (width  #:init-value 20
  ;;          #:accessor supertux:width)
  ;;  (height #:init-value 15
  ;;          #:accessor supertux:height)
  (background #:init-value "" 
              #:accessor supertux:background)
  (music      #:init-value "Mortimers_chipdisko.mod"
              #:accessor supertux:music)

  (start-pos-x #:accessor supertux:start-pos-x
               #:init-value 100)
  (start-pos-y #:accessor supertux:start-pos-y
               #:init-value 100)

  (bkgd_red_bottom   #:init-value 150
                  #:accessor supertux:bkgd_red_bottom)
  (bkgd_green_bottom #:init-value 200
              #:accessor supertux:bkgd_green_bottom)
  (bkgd_blue_bottom  #:init-value 255
              #:accessor supertux:bkgd_blue_bottom)

  (bkgd_red_top   #:init-value 150
                  #:accessor supertux:bkgd_red_top)
  (bkgd_green_top #:init-value 200
              #:accessor supertux:bkgd_green_top)
  (bkgd_blue_top  #:init-value 255
              #:accessor supertux:bkgd_blue_top)

  (time       #:init-value 500
              #:accessor supertux:time)
  (gravity    #:init-value 10
              #:accessor supertux:gravity)
  (particle_system #:init-value ""
                   #:accessor supertux:particle-system)
  (theme      #:init-value "antarctica"
              #:accessor supertux:theme)
  (interactive-tm #:init-value #f
                  #:accessor supertux:interactive-tm)
  (foreground-tm  #:init-value #f
                  #:accessor supertux:foreground-tm)
  (background-tm  #:init-value #f 
                  #:accessor supertux:background-tm)
  (objmap #:init-value #f 
          #:accessor supertux:objmap))

(define-class <supertux-worldmap> ()
  (tilemap #:init-value #f
           #:init-keyword #:tilemap
           #:accessor stwm:tilemap))

(define-generic supertux:activate)

(define-method (supertux:activate (stwm <supertux-worldmap>))
  (let ((tilemap (stwm:tilemap stwm)))
    (tilemap-paint-tool-set-tilemap tilemap)
    (set! *tilemap* tilemap)
    (editor-tilemap-set-current tilemap)
    (tileset-set-current *worldmap-tileset*)
    (tile-selector-set-tileset *tileselector* *worldmap-tileset*)
    ))

(define-method (supertux:activate (stlv <supertux-level>))
  (tilemap-paint-tool-set-tilemap (supertux:interactive-tm stlv))
  (editor-tilemap-set-current     (supertux:interactive-tm stlv))
  (editor-objectmap-set-current   (supertux:objmap stlv))
  (set! *tilemap* (supertux:interactive-tm stlv))
  (set! *objmap* (supertux:objmap stlv))
  (tileset-set-current *level-tileset*)
  (tile-selector-set-tileset *tileselector* *level-tileset*))

(define (supertux:new-map width height)
  (let ((levelmap (supertux:create-levelmap width height)))
    (editor-map-activate levelmap)
    (add-buffer levelmap)))

(define (supertux:create-levelmap width height)
  (let ((level    (make <supertux-level>))
        (levelmap (editor-map-create)))
    
    (set! (supertux:foreground-tm  level) (editor-tilemap-create *level-tileset*
                                                                 width height *tile-size*))
    (set! (supertux:interactive-tm level) (editor-tilemap-create *level-tileset*
                                                                 width height *tile-size*))
    (set! (supertux:background-tm  level) (editor-tilemap-create *level-tileset*
                                                                 width height *tile-size*))
    (set! (supertux:objmap         level) (editor-objmap-create))
    
    (editor-map-add-layer levelmap (supertux:background-tm  level))
    (editor-map-add-layer levelmap (supertux:interactive-tm level))
    (editor-map-add-layer levelmap (supertux:foreground-tm  level))
    (editor-map-add-layer levelmap (supertux:objmap         level))

    (supertux:activate level)
    (editor-toggle-grid *tilemap*)
    
    (editor-map-set-filename levelmap "/tmp/foobar.stlv")
    (editor-map-set-metadata levelmap level)
    levelmap))

(define-method (supertux:resize (stwm <supertux-worldmap>) w h x y)
  (editor-tilemap-resize (stwm:tilemap stwm) w h x y))

(define-method (supertux:resize (stlv <supertux-level>) w h x y)
  (editor-tilemap-resize (supertux:background-tm  stlv) w h x y)
  (editor-tilemap-resize (supertux:interactive-tm stlv) w h x y)
  (editor-tilemap-resize (supertux:foreground-tm  stlv) w h x y))

(define-method (supertux:save-map (stlv <supertux-level>) filename)
  ;; FIXME: This is old style singleton code
  (if (access? filename F_OK)
      (rename-file filename (string-append filename "~")))

  (let* ((levelmap (editor-map-get-current))
         (level    (editor-map-get-metadata levelmap)))
    (format #t "Level: ~a~%" level)
    (with-output-to-file filename
      (lambda ()
               ;; New style save code
               (display   ";; Generated by Flexlay Editor\n")
               (display   "(supertux-level\n")

               (display   "  (version 1)\n")
               (format #t "  (author ~s)\n" (supertux:author level))
               (format #t "  (name ~s)~%"   (supertux:name level))
               (format #t "  (width  ~a)~%" (editor-tilemap-get-width  (supertux:interactive-tm level)))
               (format #t "  (height ~a)~%" (editor-tilemap-get-height (supertux:interactive-tm level)))

               (format #t "  (start_pos_x    ~a)~%" (supertux:start-pos-x level))
               (format #t "  (start_pos_y    ~a)~%" (supertux:start-pos-y level))

               (format #t "  (background ~s)~%" (supertux:background level))
               (format #t "  (music ~s)~%" (supertux:music level))

               (format #t "  (bkgd_red_top    ~a)~%" (supertux:bkgd_red_top level))
               (format #t "  (bkgd_green_top  ~a)~%" (supertux:bkgd_green_top level))
               (format #t "  (bkgd_blue_top   ~a)~%" (supertux:bkgd_blue_top level))

               (format #t "  (bkgd_red_bottom    ~a)~%" (supertux:bkgd_red_bottom level))
               (format #t "  (bkgd_green_bottom  ~a)~%" (supertux:bkgd_green_bottom level))
               (format #t "  (bkgd_blue_bottom   ~a)~%" (supertux:bkgd_blue_bottom level))

               (format #t "  (time  ~a)~%" (supertux:time level))
               (format #t "  (gravity  ~a)~%" (supertux:gravity level))
               (format #t "  (particle_system ~s)~%" (supertux:particle-system level))
               (format #t "  (theme ~s)~%" (supertux:theme level))

               (let ((width (editor-tilemap-get-width (supertux:interactive-tm level))))
                 (display     "  (interactive-tm\n")
                 (write-field "   " width
                              (editor-tilemap-get-data (supertux:interactive-tm level)))
                 (display     "   )\n\n")

                 (display     "  (background-tm\n")
                 (write-field "   " width
                              (editor-tilemap-get-data (supertux:background-tm level)))
                 (display     "   )\n\n")

                 (display     "  (foreground-tm\n")
                 (write-field "   " width
                              (editor-tilemap-get-data (supertux:foreground-tm level)))
                 (display     "   )\n\n"))
               
               (format #t "  (reset-points\n")
               (for-each (lambda (el)
                           (let* ((obj (editor-objectmap-get-object (supertux:objmap level) el)))
                             (cond ((equal? (caaddr obj) 'resetpoint)
                                    (format #t "    (point (x ~a) (y ~a))~%" 
                                            (car obj)
                                            (cadr obj)
                                            )))))
                         (editor-objectmap-get-objects (supertux:objmap level)))
               (format #t "   )\n")

               (format #t "  (objects\n")
               (for-each (lambda (el)
                           (let* ((obj (editor-objectmap-get-object (supertux:objmap level) el)))
                             (cond ((not (equal? (caaddr obj) 'resetpoint))
                                    (format #t "    (~a  (x ~a) (y ~a))~%" 
                                            (caaddr obj)
                                            (car obj)
                                            (cadr obj)
                                            )))))
                         (editor-objectmap-get-objects (supertux:objmap level)))
               (format #t "  )\n")

               (format #t "   )\n\n")

               (newline)
               (display ";; EOF ;;\n")))
    (editor-map-set-unmodified levelmap)))

(define (supertux:load-map filename)
  (catch #t
         (lambda ()
           (let ((levelmap (supertux:create-level-map-from-file filename)))
             (editor-map-activate levelmap)
             (add-buffer levelmap)
             ))
         (lambda args
           (editor-error args)))
  (push-last-file filename))

(define (supertux:translate-v0-tiles num)
  (let ((transtbl '(
                    (#\! 103)
                    (#\# 11)
                    (#\$ 82)
                    (#\& 75)
                    (#\* 80)
                    (#\. 0)
                    (#\0 0)
                    (#\1 0)
                    (#\2 0)
                    (#\= 14)
                    (#\A 83)
                    (#\B 102)
                    (#\C 85)
                    (#\D 86)
                    (#\E 87)
                    (#\F 88)
                    (#\G 93)
                    (#\H 94)
                    (#\I 95)
                    (#\J 96)
                    (#\X 77)
                    (#\Y 105)
                    (#\[ 13 )
                    (#\\ 81)
                    (#\] 15)
                    (#\^ 76)
                    (#\a 84)
                    (#\c 89)
                    (#\d 90)
                    (#\e 91)
                    (#\f 92)
                    (#\g 97)
                    (#\h 98)
                    (#\i 99)
                    (#\j 100)
                    (#\x 104)
                    (#\y 78)
                    (#\| 79)
                    )))
    (let ((ret (assoc-ref transtbl (integer->char num))))
      (cond (ret
             (car ret))
            (else
             (format #t "couldn't translate ~a~%" num)
             0)))))


(define (supertux:create-worldmap-from-file filename)
  (let* ((m       (editor-map-create))
         (data    (with-input-from-file filename
                    (lambda () (cdr (read)))))
         (width   (get-value-from-tree '(tilemap width  _) data 19))
         (height  (get-value-from-tree '(tilemap height _) data 14))
         (tilemap (editor-tilemap-create *worldmap-tileset*
                                         width height 32))
         (stwm    (make <supertux-worldmap> #:tilemap tilemap)))
    (format #t "Size: ~ax~a~%" width height)
    (editor-tilemap-set-data tilemap (get-value-from-tree '(tilemap data) data '()))
    (editor-map-add-layer m tilemap)
    (editor-map-set-metadata m stwm)
    (editor-toggle-grid *tilemap*)
    (supertux:activate stwm)
    m))

(define-method (supertux:save-map (stwm <supertux-worldmap>) filename)
  (if (access? filename F_OK)
      (rename-file filename (string-append filename "~")))

  (let* ((m       (editor-map-get-current))
         (stwm    (editor-map-get-metadata m))
         (tilemap (stwm:tilemap stwm)))
    (with-output-to-file filename
      (lambda ()
        (display ";; Generated with Flexlay Editor\n")
        (display "(supertux-worldmap\n")
        (display "  (tilemap \n")
        (format #t "    (width  ~a)~%" (editor-tilemap-get-width  tilemap))
        (format #t "    (height ~a)~%" (editor-tilemap-get-height tilemap))
        (display "    (data\n")
        (write-field "       " 
                     (editor-tilemap-get-width tilemap)
                     (editor-tilemap-get-data  tilemap))
        (display "    ))\n")
        (display ")\n")
        (newline)
        ))))

(define (supertux:create-level-map-from-file filename)
  (display "Loading SuperTux level: ")
  (display filename)
  (newline)
  (let* ((data (with-input-from-file filename
                 (lambda () (cdr (read)))))
         (level   (make <supertux-level>))
         (version (get-value-from-tree '(version _) data 0)))
    ;; read in the level metadata
    (set! (supertux:name  level)      (get-value-from-tree '(name _) data 20))
    (set! (supertux:author level)     (get-value-from-tree '(author _) data ""))
    (set! (supertux:theme level)      (get-value-from-tree '(theme _) data "antarctica"))
    (set! (supertux:music level)      (get-value-from-tree '(music _) data "Mortimers_chipdisko.mod"))
    (set! (supertux:background level) (get-value-from-tree '(background _)   data "arctis.png"))

    (set! (supertux:start-pos-x level) (get-value-from-tree '(start_pos_x  _) data 100))
    (set! (supertux:start-pos-y level) (get-value-from-tree '(start_pos_y  _) data 170))

    (set! (supertux:bkgd_red_bottom   level) (get-value-from-tree '(bkgd_red_bottom   _) data 150))
    (set! (supertux:bkgd_green_bottom level) (get-value-from-tree '(bkgd_green_bottom _) data 200))
    (set! (supertux:bkgd_blue_bottom  level) (get-value-from-tree '(bkgd_blue_bottom  _) data 255))

    (set! (supertux:bkgd_red_top   level) (get-value-from-tree '(bkgd_red_top   _) data 150))
    (set! (supertux:bkgd_green_top level) (get-value-from-tree '(bkgd_green_top _) data 200))
    (set! (supertux:bkgd_blue_top  level) (get-value-from-tree '(bkgd_blue_top  _) data 255))

    (set! (supertux:time  level)      (get-value-from-tree '(time _)         data 250))
    (set! (supertux:particle-system level)     (get-value-from-tree '(particle_system _) data ""))
    (set! (supertux:gravity         level)     (get-value-from-tree '(gravity _)      data 10))

    (let ((width   (get-value-from-tree '(width _)    data 20))
          (height  (get-value-from-tree '(height _)   data 15))
          (objects (get-value-from-tree '(objects)   data '()))
          (reset-points (get-value-from-tree '(reset-points)  data '()))
          (interactive-tm (get-value-from-tree '(interactive-tm) data '()))
          (background-tm  (get-value-from-tree '(background-tm)  data '()))
          (foreground-tm  (get-value-from-tree '(foreground-tm)  data '())))

      ;; load level file and extract tiledata and w/h
      (let* ((m       (editor-map-create))
             (objmap  (editor-objmap-create)))

        (set! (supertux:objmap          level) objmap)
        (set! (supertux:interactive-tm  level) (editor-tilemap-create *level-tileset*
                                                                      width height *tile-size*))
        (set! (supertux:background-tm   level) (editor-tilemap-create *level-tileset*
                                                                      width height *tile-size*))
        (set! (supertux:foreground-tm   level) (editor-tilemap-create *level-tileset*
                                                                      width height *tile-size*))
        (set! *tilemap* (supertux:interactive-tm  level))
        (set! *objmap*  (supertux:objmap level))

        ;; set data to the tilemap
        (if (not (null? interactive-tm))
            (editor-tilemap-set-data (supertux:interactive-tm level) interactive-tm)
            (display "Error: interactive-tm missing\n"))
        (if (not (null? foreground-tm))
            (editor-tilemap-set-data (supertux:foreground-tm level) foreground-tm)
            (display "Error: foreground-tm missing\n"))
        (if (not (null? background-tm))
            (editor-tilemap-set-data (supertux:background-tm level) background-tm)
            (display "Error: background-tm missing\n"))

        (cond ((= version 0)
               (let ((tilemap (get-value-from-tree '(interactive-tm) data 
                                                   (get-value-from-tree '(tilemap) data '()))))

                 (let ((i 0))
                   (for-each (lambda (el)
                               (let ((x (* 32 (remainder i width)))
                                     (y (* 32 (quotient  i width))))
                                 (cond ((= el 48) ;; 0
                                        (objectmap-add-object objmap 
                                                              (string-append *supertux:datadir* "images/shared/snowball-left-0.png")
                                                              x y '(snowball)))
                                       ((= el 49) ;; 1
                                        (objectmap-add-object objmap 
                                                              (string-append *supertux:datadir* "images/shared/mriceblock-left-0.png")
                                                              x y '(mriceblock)))
                                       ((= el 50) ;; 2
                                        (objectmap-add-object objmap
                                                              (string-append *supertux:datadir* "images/shared/jumpy-left-middle-0.png")
                                                              x y '(money)))))
                               (set! i (+ i 1)))
                             tilemap))
                 (editor-tilemap-set-data (supertux:interactive-tm level)
                                          (map supertux:translate-v0-tiles tilemap)))))

        (display (supertux:interactive-tm level))(newline)

        (for-each (lambda (el)
                    (let ((x (get-value-from-tree '(x _) (cdr el) 0))
                          (y (get-value-from-tree '(y _) (cdr el) 0)))
                      (objectmap-add-object objmap
                                            (string-append *supertux:datadir* "images/shared/resetpoint.png")
                                            x y '(resetpoint))))
                  reset-points)

        (for-each (lambda (el)
                    (let ((x (get-value-from-tree '(x _) (cdr el) 0))
                          (y (get-value-from-tree '(y _) (cdr el) 0)))
                      (case (car el)
                        ((money jumpy)
                         (objectmap-add-object objmap
                                               (string-append *supertux:datadir* "images/shared/jumpy-left-middle-0.png")
                                               x y '(money)))
                        ((mriceblock laptop)
                         (objectmap-add-object objmap
                                               (string-append *supertux:datadir* "images/shared/mriceblock-left-0.png")
                                               x y '(mriceblock)))
                        ((snowball bsod)
                         (objectmap-add-object objmap
                                               (string-append *supertux:datadir* "images/shared/snowball-left-0.png")
                                               x y '(snowball)))
                        ((mrbomb)
                         (objectmap-add-object objmap
                                               (string-append *supertux:datadir* "images/shared/mrbomb-left-0.png")
                                               x y '(mrbomb)))
                        ((stalactite)
                         (objectmap-add-object objmap
                                               (string-append *supertux:datadir* "images/shared/stalactite.png")
                                               x y '(stalactite)))
                        ((flame)
                         (objectmap-add-object objmap
                                               (string-append *supertux:datadir* "images/shared/flame-0.png")
                                               x y '(flame)))
                        ((fish)
                         (objectmap-add-object objmap
                                               (string-append *supertux:datadir* "images/shared/fish-left-0.png")
                                               x y '(fish)))
                        ((flyingsnowball)
                         (objectmap-add-object objmap
                                               (string-append *supertux:datadir* "images/shared/flyingsnowball-left-0.png")
                                               x y '(flyingsnowball)))
                        ((bouncingsnowball)
                         (objectmap-add-object objmap
                                               (string-append *supertux:datadir* "images/shared/bouncingsnowball-left-0.png")
                                               x y '(bouncingsnowball)))
                        ((spiky)
                         (objectmap-add-object objmap
                                               (string-append *supertux:datadir* "images/shared/spiky-left-0.png")
                                               x y '(spiky)))
                        )))
                  objects)
        
        (editor-map-add-layer m (supertux:background-tm  level))
        (editor-map-add-layer m (supertux:interactive-tm level))
        (editor-map-add-layer m (supertux:foreground-tm  level))

        (editor-tilemap-set-bgcolor (supertux:background-tm  level) 150 200 255 255)

        (editor-map-add-layer m (supertux:objmap         level))

        (supertux:activate level)

        (editor-map-set-filename m filename)
        (editor-map-set-metadata m level)
        m))))

(define (supertux:set-background-layer-only)
  (let ((level (editor-map-get-metadata (editor-map-get-current))))
    (editor-tilemap-set-fgcolor (supertux:background-tm  level) 255 255 255 255)
    (editor-tilemap-set-fgcolor (supertux:interactive-tm level)   0   0   0  10)
    (editor-tilemap-set-fgcolor (supertux:foreground-tm  level)   0   0   0  10)
    (tilemap-paint-tool-set-tilemap (supertux:background-tm level))))

(define (supertux:set-interactive-layer-only)
  (let ((level (editor-map-get-metadata (editor-map-get-current))))
    (editor-tilemap-set-fgcolor (supertux:background-tm  level)   0   0   0  10)
    (editor-tilemap-set-fgcolor (supertux:interactive-tm level) 255 255 255 255)
    (editor-tilemap-set-fgcolor (supertux:foreground-tm  level)   0   0   0  10)
    (tilemap-paint-tool-set-tilemap (supertux:interactive-tm level))))

(define (supertux:set-foreground-layer-only)
  (let ((level (editor-map-get-metadata (editor-map-get-current))))
    (editor-tilemap-set-fgcolor (supertux:background-tm  level)   0   0   0  10)
    (editor-tilemap-set-fgcolor (supertux:interactive-tm level)   0   0   0  10)
    (editor-tilemap-set-fgcolor (supertux:foreground-tm  level) 255 255 255 255)
    (tilemap-paint-tool-set-tilemap (supertux:foreground-tm level))))


(define (supertux:set-background-layer-active)
  (let ((level (editor-map-get-metadata (editor-map-get-current))))
    (editor-tilemap-set-fgcolor (supertux:background-tm  level) 255 255 255 255)
    (editor-tilemap-set-fgcolor (supertux:interactive-tm level) 150 250 150 150)
    (editor-tilemap-set-fgcolor (supertux:foreground-tm  level) 255 150 150 150)
    (tilemap-paint-tool-set-tilemap (supertux:background-tm level))))

(define (supertux:set-interactive-layer-active)
  (let ((level (editor-map-get-metadata (editor-map-get-current))))
    (editor-tilemap-set-fgcolor (supertux:background-tm  level) 150 150 250 150)
    (editor-tilemap-set-fgcolor (supertux:interactive-tm level) 255 255 255 255)
    (editor-tilemap-set-fgcolor (supertux:foreground-tm  level) 255 150 150 150)
    (tilemap-paint-tool-set-tilemap (supertux:interactive-tm level))))

(define (supertux:set-foreground-layer-active)
  (let ((level (editor-map-get-metadata (editor-map-get-current))))
    (editor-tilemap-set-fgcolor (supertux:background-tm  level) 150 150 250 150)
    (editor-tilemap-set-fgcolor (supertux:interactive-tm level) 255 150 150 150)
    (editor-tilemap-set-fgcolor (supertux:foreground-tm  level) 255 255 255 255)
    (tilemap-paint-tool-set-tilemap (supertux:foreground-tm level))))

(define (supertux:show-all-layers)
  (let ((level (editor-map-get-metadata (editor-map-get-current))))
    (editor-tilemap-set-fgcolor (supertux:background-tm  level) 255 255 255 255)
    (editor-tilemap-set-fgcolor (supertux:interactive-tm level) 255 255 255 255)
    (editor-tilemap-set-fgcolor (supertux:foreground-tm  level) 255 255 255 255)))

;; EOF ;;
