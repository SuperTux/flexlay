(display "SuperTux Startup Script\n")

(define *game* 'supertux)
(game-set-tilesize 32 16)
(game-load-resources "tuxtiles.xml")
(game-load-resources "tuxsprites.xml")
(game-load-tiles     "tuxtiles.scm")

;; EOF ;;
