(require "evaluator.rkt")


; uses picture as background for character sheet
(define sheetpicture (read-bitmap "./DND/ddcharsheet.png"))
(define pic-w (send sheetpicture get-width))
(define pic-h (send sheetpicture get-height))

; main frame for the character sheet
(define charsheet (new frame% [label "Character Sheet"]
                  [width pic-w]
                  [height pic-h]))


  
; canvas on which to draw data
(define cansheet
  (new canvas% [parent charsheet]
       [style '(vscroll)]
               [paint-callback
              (λ (canvas dc)
                (send dc draw-bitmap sheetpicture 0 0)
                (send dc set-text-foreground "red")

                ; prints stats in hash tables onto character sheet
               (let* ((statsx 65) (statss 210) (statsd (+ statss 100)) (statsc (+ statsd 100)) (statsi (+ statsc 90))
                                  (statsw (+ statsi 100)) (statsch (+ statsw 90)) (char_x 90) (char_y 90) (line1_y 65)
                                  (line2_y 100) (class_x 350) (player_x 650) (align_x 500) (race_x 370)(level_x (+ class_x 120)))
                 (unless (hash-empty? hash-stats)
                  (for ([(key value) (in-hash hash-stats)])
                    (cond ((equal? key 'stat-strength) (send dc draw-text (number->string (car value)) statsx statss))
                          ((equal? key 'stat-dexterity) (send dc draw-text (number->string (car value)) statsx statsd))
                          ((equal? key 'stat-constitution) (send dc draw-text (number->string (car value)) statsx statsc))
                          ((equal? key 'stat-intelligence) (send dc draw-text (number->string (car value)) statsx statsi))
                          ((equal? key 'stat-wisdom) (send dc draw-text (number->string (car value)) statsx statsw))
                          ((equal? key 'stat-charisma) (send dc draw-text (number->string (car value)) statsx statsch)))))
                 (unless (hash-empty? hash-base)
                   (for ([(key value) (in-hash hash-base)])
                     (cond ((equal? key 'character-name) (send dc draw-text value char_x char_y))
                          ((equal? key 'character-class) (send dc draw-text value class_x line1_y))
                          ((equal? key 'character-level) (send dc draw-text (number->string value) level_x line1_y))
                          ((equal? key 'player-name) (send dc draw-text value player_x line1_y))
                          ((equal? key 'character-race) (send dc draw-text value race_x line2_y))
                          ((equal? key 'character-alignment) (send dc draw-text value align_x line2_y))
                          ((equal? key 'character-exp) (send dc draw-text (number->string value)  player_x line2_y))
                          ((equal? key 'character-hp) (send dc draw-text (number->string (get-hash-base "hp")) player_x line2_y))
                          )))
                ))]))

; boolean parameter determines whether to draw character sheet or not
(define (genCS showsheet)
  (cond ((eq? showsheet #t) (begin (send cansheet on-paint)
                                   (send cansheet init-auto-scrollbars pic-w pic-h 0 0)
                                   (send charsheet show #t)))
        (else (send charsheet on-exit))))
       
  