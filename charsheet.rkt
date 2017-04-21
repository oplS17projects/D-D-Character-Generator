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
               (let* ((statsx 65) (statsstr 210) (statsdex (+ statsstr 100)) (statscon (+ statsdex 100)) (statsint (+ statscon 90))
                                  (statswis (+ statsint 100)) (statschar (+ statswis 90)) (char_x 90) (char_y 90) (line1_y 65)
                                  (line2_y 100) (class_x 350) (player_x 650) (align_x 500) (race_x 370) (level_x (+ class_x 120)))
                 (unless (hash-empty? hash-stats)
                  (for ([(key value) (in-hash hash-stats)])
                    (cond ((equal? key 'stat-strength) (send dc draw-text (number->string (car value)) statsx statsstr))
                          ((equal? key 'stat-dexterity) (send dc draw-text (number->string (car value)) statsx statsdex))
                          ((equal? key 'stat-constitution) (send dc draw-text (number->string (car value)) statsx statscon))
                          ((equal? key 'stat-intelligence) (send dc draw-text (number->string (car value)) statsx statsint))
                          ((equal? key 'stat-wisdom) (send dc draw-text (number->string (car value)) statsx statswis))
                          ((equal? key 'stat-charisma) (send dc draw-text (number->string (car value)) statsx statschar)))))
                 (unless (hash-empty? hash-base)
                   (for ([(key value) (in-hash hash-base)])
                     (cond ((equal? key 'character-name) (send dc draw-text value char_x char_y))
                          ((equal? key 'character-class) (send dc draw-text value class_x line1_y))
                          ((equal? key 'character-level) (send dc draw-text (number->string value) level_x line1_y))
                          ((equal? key 'player-name) (send dc draw-text value player_x line1_y))
                          ((equal? key 'character-race) (send dc draw-text value race_x line2_y))
                          ((equal? key 'character-alignment) (send dc draw-text value align_x line2_y))
                          ((equal? key 'character-exp) (send dc draw-text (number->string value)  player_x line2_y))
                          ((equal? key 'character-hp) (send dc draw-text (number->string value) (+ class_x 50) 257))
                          ((equal? key 'character-armor-class) (send dc draw-text (number->string value) 325 200))
                          ((equal? key 'character-hit-dice) (send dc draw-text value 325 450))
                          ((equal? key 'character-proficiency-bonus) (send dc draw-text (number->string value) 140 225))
                          )))
                 (unless (hash-empty? hash-skills)
                   (for ([(key value) (in-hash hash-skills)])
                     (cond ((equal? key 'acrobatics) (send dc draw-text (number->string (get-skill-mod key)) 150 419))
                           ((equal? key (string->symbol "animal handling")) (send dc draw-text (number->string (get-skill-mod key)) 150 437))
                           ((equal? key 'arcana) (send dc draw-text (number->string (get-skill-mod key)) 150 455))
                           ((equal? key 'athletics) (send dc draw-text (number->string (get-skill-mod key)) 150 473))
                           ((equal? key 'dception) (send dc draw-text (number->string (get-skill-mod key)) 150 491))
                           ((equal? key 'history) (send dc draw-text (number->string (get-skill-mod key)) 150 509))
                           ((equal? key 'insight) (send dc draw-text (number->string (get-skill-mod key)) 150 527))
                           ((equal? key 'intimidation) (send dc draw-text (number->string (get-skill-mod key)) 150 545))
                           ((equal? key 'investigation) (send dc draw-text (number->string (get-skill-mod key)) 150 563))
                           ((equal? key 'medicine) (send dc draw-text (number->string (get-skill-mod key)) 150 581))
                           ((equal? key 'nature) (send dc draw-text (number->string (get-skill-mod key)) 150 599))
                           ((equal? key 'perception) (send dc draw-text (number->string (get-skill-mod key)) 150 617))
                           ((equal? key 'performance) (send dc draw-text (number->string (get-skill-mod key)) 150 635))
                           ((equal? key 'persuasion) (send dc draw-text (number->string (get-skill-mod key)) 150 653))
                           ((equal? key 'religion) (send dc draw-text (number->string (get-skill-mod key)) 150 671))
                           ((equal? key (string->symbol "slight of hand")) (send dc draw-text (number->string (get-skill-mod key)) 150 689))
                           ((equal? key 'stealth) (send dc draw-text (number->string (get-skill-mod key)) 150 706))
                           ((equal? key 'survival) (send dc draw-text (number->string (get-skill-mod key)) 150 724))
                           )))
                ))]))

; boolean parameter determines whether to draw character sheet or not
(define (genCS showsheet)
  (cond ((eq? showsheet #t) (begin (send cansheet on-paint)
                                   (send cansheet init-auto-scrollbars pic-w pic-h 0 0)
                                   (send charsheet show #t)))
        (else (send charsheet on-exit))))
       
  