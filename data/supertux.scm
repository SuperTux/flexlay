(display "SuperTux Startup Script\n")

(define *game* 'supertux)
(define *tile-size* 32)
(game-set-tilesize 32 16)
(game-load-resources "tuxtiles.xml")
(game-load-resources "tuxsprites.xml")
(game-load-tiles     "tuxtiles.scm")

(set-window-title "Windstille Editor - SuperTux Mode")

;; EOF ;;
