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
       [style '(hscroll vscroll no-focus)]
               [paint-callback
              (λ (canvas dc)
                (send dc draw-bitmap sheetpicture 0 0)
                (send dc set-text-foreground "red")

                ; adds some delta to a value
                (define (add-delta val delta)
                  (+ val delta))
                
                ; prints stats in hash tables onto character sheet
               (let* ((statsx 65) (statsstr 210) (stats_delta_y 95)
                                  (statsdex (add-delta statsstr stats_delta_y))
                                  (statscon (add-delta statsdex stats_delta_y))
                                  (statsint (add-delta statscon stats_delta_y))
                                  (statswis (add-delta statsint stats_delta_y))
                                  (statschar (add-delta statswis stats_delta_y)))
                 (unless (hash-empty? hash-stats)
                  (for ([(key value) (in-hash hash-stats)])
                    (let ((stats-val (let ((z ((λ (x) (car x)) value))) (number->string z))))
                      (cond ((equal? key 'stat-strength) (send dc draw-text stats-val statsx statsstr))
                            ((equal? key 'stat-dexterity) (send dc draw-text stats-val statsx statsdex))
                            ((equal? key 'stat-constitution) (send dc draw-text stats-val statsx statscon))
                            ((equal? key 'stat-intelligence) (send dc draw-text stats-val statsx statsint))
                            ((equal? key 'stat-wisdom) (send dc draw-text stats-val statsx statswis))
                            ((equal? key 'stat-charisma) (send dc draw-text stats-val statsx statschar)))))))

                   ; prints player info
                  (unless (hash-empty? hash-base)
                    (let* ((char_x 90) (char_y 90)
                                  (line1_y 65)
                                  (line2_y 100)
                                  (class_x 350)
                                  (player_x 630)
                                  (align_x 500)
                                  (race_x 350)
                                  (level_x (add-delta class_x 120))) 
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
                              ))))

                ; prints skills
                 (unless (hash-empty? hash-skills)
                   (let* ((skills_x 150) (skills_delta_y 18)
                                  (acrobatics_y 419)
                                  (animal-handling_y (add-delta acrobatics_y skills_delta_y))
                                  (arcana_y (add-delta animal-handling_y skills_delta_y))
                                  (athletics_y (add-delta arcana_y skills_delta_y))
                                  (dception_y (add-delta athletics_y skills_delta_y))
                                  (history_y (add-delta dception_y skills_delta_y))
                                  (insight_y (add-delta history_y skills_delta_y))
                                  (intimidation_y (add-delta insight_y skills_delta_y))
                                  (investigation_y (add-delta intimidation_y skills_delta_y))
                                  (medicine_y (add-delta investigation_y skills_delta_y))
                                  (nature_y (add-delta medicine_y skills_delta_y))
                                  (perception_y (add-delta nature_y skills_delta_y))
                                  (performance_y (add-delta perception_y skills_delta_y))
                                  (persuasion_y (add-delta performance_y skills_delta_y))
                                  (religion_y (add-delta persuasion_y skills_delta_y))
                                  (slight-of-hand_y (add-delta religion_y skills_delta_y))
                                  (stealth_y (add-delta slight-of-hand_y skills_delta_y))
                                  (survival_y (add-delta stealth_y skills_delta_y)))
                   (for ([(key value) (in-hash hash-skills)])
                     (let ((val-num (let ((z ((λ (x) (get-skill-mod x)) key))) (number->string z))))
                       (cond ((equal? key 'acrobatics) (send dc draw-text val-num skills_x acrobatics_y))
                             ((equal? key (string->symbol "animal handling")) (send dc draw-text val-num skills_x animal-handling_y))
                             ((equal? key 'arcana) (send dc draw-text val-num skills_x arcana_y))
                             ((equal? key 'athletics) (send dc draw-text val-num skills_x athletics_y))
                             ((equal? key 'dception) (send dc draw-text val-num skills_x dception_y))
                             ((equal? key 'history) (send dc draw-text val-num skills_x history_y))
                             ((equal? key 'insight) (send dc draw-text val-num skills_x insight_y))
                             ((equal? key 'intimidation) (send dc draw-text val-num skills_x intimidation_y))
                             ((equal? key 'investigation) (send dc draw-text val-num skills_x investigation_y))
                             ((equal? key 'medicine) (send dc draw-text val-num skills_x medicine_y))
                             ((equal? key 'nature) (send dc draw-text val-num skills_x nature_y))
                             ((equal? key 'perception) (send dc draw-text val-num skills_x perception_y))
                             ((equal? key 'performance) (send dc draw-text val-num skills_x performance_y))
                             ((equal? key 'persuasion) (send dc draw-text val-num skills_x persuasion_y))
                             ((equal? key 'religion) (send dc draw-text val-num skills_x religion_y))
                             ((equal? key (string->symbol "slight of hand")) (send dc draw-text val-num skills_x slight-of-hand_y))
                             ((equal? key 'stealth) (send dc draw-text val-num skills_x stealth_y))
                             ((equal? key 'survival) (send dc draw-text val-num skills_x survival_y))
                             )))))

                ; prints out list that contains elements of cons items
                (define (print-list lst x y delta)
                             (unless (equal? lst '())
                               (let ((item (cadar lst)) (quantity (cddar lst)))
                                 ;(if (equal? "any*" (substring item 0 4))
                                  ;   (print-list (cdr lst) x y delta)
                                     (begin (send dc draw-text
                                                  (if (number? quantity) (string-append (number->string quantity) " " item)
                                                      (string-append quantity " " item)) x y)
                                            (print-list (cdr lst) x (add-delta y delta) delta)))))
                
                ; prints weapons list
                (unless (hash-empty? hash-weapons)
                  (let ((weapons-list (hash->list hash-weapons)))
                    (print-list weapons-list 300 590 15)))

                             
                ; prints inventory of equipment
               (unless (hash-empty? hash-inventory)
                 (let ((inventory-list (hash->list hash-inventory)))
                     (print-list inventory-list 355 780 15)))

                ; prints proficiencies
                (unless (hash-empty? hash-proficiencies-list)
                  (define proficiencies (hash->list hash-proficiencies-list))
                  (define (print-proficiencies lst x-coord y-coord delta)
                    (unless (equal? lst '())
                      (let ((prof (cadar lst)))
                        (begin (send dc draw-text prof x-coord y-coord)
                               (print-proficiencies (cdr lst) x-coord (add-delta y-coord delta) delta)))))
                  (print-proficiencies proficiencies 45 823 15))

                ; prints notes and miscellaneous info
                (define line-length 20)

               #| (unless (hash-empty? hash-notes)
                  (define notes (hash->list hash-notes))
                  (define (print-notes lst x-coord y-coord delta)
                    (unless (equal? lst '())
                      (let ((prof (cdar lst)))
                        (begin (send dc draw-text prof x-coord y-coord)
                               (print-notes (cdr lst) x-coord (add-delta y-coord delta) delta)))))
                  (print-notes notes 545 503 15))|#
                
                #|(unless (hash-empty? hash-notes)
                  (let ((notes (map string-split (map cdr (hash->list hash-notes)))) (lz 21) (y-coord 503))
                        (begin (define (ps lst str x y delta)
                                 (cond ((equal? lst '()) (send dc draw-text str x y))
                                       (else (let ((z (string-append str " " (car lst))))
                                               (if (> (string-length z) lz) (send dc draw-text str x y (ps lst "" x (+ y delta) delta))
                                                   (ps (cdr lst) z x y delta))))))
                               (ps (car notes) "" 545 y-coord 15))))|#
                
                (unless (hash-empty? hash-notes)
                  (begin (define notes (map string-split (map cdr (hash->list hash-notes))))
                         (define lz 27)
                         (define y-coord 502)
                         (define (ps lst str x y delta)
                           (cond ((equal? lst '()) (begin (send dc draw-text str x y) (send dc draw-text "" x (add-delta y delta) (set! y-coord (add-delta y delta)))))
                                 (else (let ((z (string-append str " " (car lst))))
                                         (if (> (string-length z) lz) (begin (send dc draw-text str x y) (ps lst "" x (+ y delta) delta))
                                             (ps (cdr lst) z x y delta))))))
                         (define (pn lst str x y delta)
                           (unless (equal? lst '())
                             (begin (ps (car lst) str x y delta) (pn (cdr lst) str x (+ y-coord delta) delta))))
                         (pn notes "" 545 y-coord 15)))

               
               
                )]))

 

; boolean parameter determines whether to draw character sheet or not
(define (genCS showsheet)
  (cond ((eq? showsheet #t) (begin (send cansheet on-paint)
                                   (send cansheet init-auto-scrollbars pic-w pic-h 0 0)
                                   (send charsheet show #t)))
        (else (send charsheet on-exit))))
       
  