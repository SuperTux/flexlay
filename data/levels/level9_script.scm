(player-set-pos 717 660)

(game-add-water 1724 1650 3014 1736)

(game-add-water 1454 2160 2262 2255)
(game-add-water 2606 2254 2896 2383)
(game-add-water 4403 3523 6217 3654)

(game-add-water 438 3329 1100 3532)

(game-add-water 4792 876 5581 970)
(game-add-water 8890 2130 9031 2251)

(game-add-water 8378 2130 8520 2251)

(game-add-water 9007 3549 9934 3664)

(add-region-trigger 7221 3744  7493 3837
                    (lambda ()
                      (dialog-clear)
                      (dialog-add "hero/portrait" 
                                  "Well, done, you completed your first mission with NUM diamonds.")
                      (dialog-show)
                      (remove-trigger)
                      ))

;; EOF ;;
