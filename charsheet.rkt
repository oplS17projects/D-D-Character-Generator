(require "evaluator.rkt")

; uses picture as background for character sheet
(define sheetpicture (read-bitmap "./DND/ddcharsheet.png"))

; main frame for the character sheet
(define charsheet (new frame% [label "Character Sheet"]
                  [width 850]
                  [height 1200]))

; canvas on which to draw data
(define cansheet
  (new canvas% [parent charsheet]
               [paint-callback
              (λ (canvas dc)
                (send dc draw-bitmap sheetpicture 0 0)
                (send dc set-text-foreground "red")
                (send dc draw-text (getStatVal "Character Name") 100 90) ; character name
                (send dc draw-text (string-append "17" " " (getStatVal "Class")) 370 65) ; class level
                (send dc draw-text (getStatVal "Player Name") 650 65) ; player name
                (send dc draw-text (getStatVal "Race") 370 100) ; race
                (send dc draw-text "Lawful Good" 500 100) ;alignment
                (send dc draw-text "0 exp" 650 100) ; experience points
                (send dc draw-text "17" 65 210) ; strength
                (send dc draw-text "16" 65 310)  ; dexterity
                (send dc draw-text "15" 65 410) ; constitution
                (send dc draw-text "14" 65 500) ; intelligence
                (send dc draw-text "13" 65 600) ; wisdom
                (send dc draw-text "12" 65 690) ; charisma
)]))

; boolean parameter determines whether to draw character sheet or not
(define (genCS showsheet)
  (cond ((eq? showsheet #t) (begin (send cansheet on-paint) (send charsheet show #t)))
        (else (send charsheet show #f) exit)))
       
  