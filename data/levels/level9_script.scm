(player-set-pos  258 485)
(player-set-direction "east")

(game-add-water 1724 1650 3014 1756)

(game-add-water 1454 2160 2262 2255)
(game-add-water 2606 2254 2896 2393)
(game-add-water 4403 3523 6217 3664)

(game-add-water 438 3329 1100 3542)

(game-add-water 4792 876 5581 980)
(game-add-water 8890 2130 9031 2261)

(game-add-water 8378 2130 8520 2261)

(game-add-water 9007 3549 9934 3674)

(game-add-igel 668 460)
(game-add-igel 1611 748)
(game-add-igel 1993 637)
(game-add-igel 2568 788)
(game-add-igel 3038 733)
(game-add-igel 2503 2128)
(game-add-igel 3000 2043)
(game-add-igel 3136 2357)
(game-add-igel 3431 2360)
(game-add-igel 3748 2411)
(game-add-igel 4060 2325)
(game-add-igel 4358 2319)
(game-add-igel 7091 1182)
(game-add-igel 7523 684)
(game-add-igel 7797 685)
(game-add-igel 5851 481)
(game-add-igel 5883 451)
(game-add-igel 6097 461)
(game-add-igel 1829 190)
(game-add-igel 579 71)
(game-add-igel 8570 842)
(game-add-igel 9193 912)
(game-add-igel 8986 891)
(game-add-igel 8590 1954)
(game-add-igel 7550 1888)
(game-add-igel 7214 2124)
(game-add-igel 7572 2330)
(game-add-igel 8374 2454)
(game-add-igel 7679 3355)


(define (end-dialog)
  (dialog-clear)
  (dialog-add "hero/portrait" 
              (string-append
               "Mission complete!\n\n"
               "Time needed: " (game-get-time-str) "\n"
               "Diamonds collected: " 
               (number->string (- (game-get-max-diamonds) (game-get-diamonds)))
               "/"
               (number->string (game-get-max-diamonds))
               ))
  (dialog-show)
  (remove-trigger))

(add-region-trigger 7221 3744  7493 3837
                    end-dialog)

;; EOF ;;
