(display "Windstille Startup Script\n")

(define *game* 'windstille)
(define *tile-size* 128)
(game-set-tilesize 128 16)
(game-load-resources "tiles.xml")
(game-load-tiles     "tiles.scm")

;; EOF ;;
