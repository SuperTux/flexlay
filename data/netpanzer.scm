(display "netPanzer Startup Script: ...\n")

(define *game* 'netpanzer)
(define *tile-size* 32)
(game-set-tilesize 32 16)
(game-load-resources "netpanzertiles.xml")
(game-load-tiles     "netpanzertiles.scm")

(set-window-title "Windstille Editor - netPanzer Mode")

(display "netPanzer Startup Script: done\n")

;; EOF ;;
