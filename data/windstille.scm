(display "Windstille Startup Script\n")

(define *game* 'windstille)
(game-set-tilesize 128 16)
(game-load-resources "tiles.xml")
(game-load-tiles     "tiles.scm")


;; EOF ;;
