#lang racket
(require racket/gui)
(require rsound)
(include "charsheet.rkt")

(define logo (read-bitmap "./DND/ddlogo.png"))

; Johnny Douglas' Dungeons & Dragons cartoon theme
; converted from youtube video
; https://www.youtube.com/watch?v=v2u7-M7Ouok&t=37s
(define theme (rs-read "./DND/DDTheme.wav"))
(play theme)


; hash table to contain stats
(define stats (make-hash))

; gets value in hash table by 
(define (getStatVal statKey)
  (hash-ref stats statKey))

; sets key/value pair in hash table
(define (setStat key val)
  (hash-set! stats key val))

; variable to show main screen
(define mainOn #t)

; sets race in stats hash table based on radiobox choice
(define (setRace x)
  (cond ((eqv? x 0) (setStat "Race" "Human"))
        ((eqv? x 1) (setStat "Race" "Elf"))
        ((eqv? x 2) (setStat "Race" "Dwarf"))))

; sets class
(define (setClass x)
  (cond ((eqv? x 0) (setStat "Class" "Barbarian"))
        ((eqv? x 1) (setStat "Class" "Cleric"))
        ((eqv? x 2) (setStat "Class" "Mage"))))


; main window
(define main (new frame% [label "D & D Character Generator"]
                  [width 560]
                  [height 300]
                  ))

; panel for logo
(define logoPanel (new horizontal-panel% [parent main]
                       [min-height 72]))

; panels for attributes and character generation
(define choicePanel (new horizontal-panel% [parent main]
                         [min-height 200]))
(define genPanel (new horizontal-panel% [parent main]
                      [alignment '(center center)]))

;draws logo
(new canvas% [parent logoPanel]
    [style '(control-border)]
    [paint-callback
     (λ (canvas dc)
     (send dc draw-bitmap logo 0 0))])

; tabs for various attribute choices
(define tab (new tab-panel%
                 [parent choicePanel]
                 [choices (list "Names"
                                "Race"
                                "Class"
                                "Base Stats")]
                 [callback (λ (b e)
                             (case (send b get-selection)
                               ((0) (send b change-children (λ (children) (list name-panel))))
                               ((1) (send b change-children (λ (children) (list race-panel))))
                               ((2) (send b change-children (λ (children) (list class-panel))))
                               ((3) (send b change-children (λ (children) (list stats-panel))))
                               ))]))

; panels for each attribute tab 
(define name-panel (new panel% [parent tab]))
(define race-panel (new panel% [parent tab]
                        [style '(deleted)]))
(define class-panel (new panel% [parent tab]
                         [style '(deleted)]))
(define stats-panel (new panel% [parent tab]
                         [style '(deleted)]))

; text boxes for character and player names tab
(define align-name-panel (new vertical-panel% [parent name-panel]))
(define playerName (new text-field%
                        [label "Player Name"]
                        [parent align-name-panel]
                        [vert-margin 4]
                        [callback (λ (b e)
                                    (setStat "Player Name" (send playerName get-value)))]))

(define charName (new text-field%
                        [label "Character Name"]
                        [parent align-name-panel]
                        [vert-margin 4]
                        [callback (λ (b e)
                                  (setStat "Character Name" (send charName get-value)))]))

; choices for race tab
(define raceBox (new radio-box%
                     [label "Race"]
                     [choices (list "Human" "Elf" "Dwarf")]
                     [parent race-panel]
                     [vert-margin 10]
                     [horiz-margin 5]
                     [style (list 'vertical 'vertical-label)]
                     [callback (λ (b e)
                               (setRace (send raceBox get-selection)))]
                     ))

; choices for class tab
(define classBox (new radio-box%
                     [label "Class"]
                     [choices (list "Barbarian" "Cleric" "Mage")]
                     [parent class-panel]
                     [style (list 'vertical 'vertical-label)]
                     [callback (λ (b e)
                               (setClass (send classBox get-selection)))]
                     ))

; character sheet generation button
(define genSheet (new button%
                      [label "Generate Character Sheet"]
                      [parent genPanel]
                      [callback (λ  (b e)
                                  (genCS #t))]))

; closes character sheet button
(define closeSheet (new button%
                        [label "Close Character Sheet"]
                        [parent genPanel]
                        [callback (λ (b e)
                                    (genCS #f))]))
; Exits application button
(define Exit (new button%
                  [label "Exit"]
                  [parent genPanel]
                  [callback (λ (b e)
                              (begin (stop)
                                      (set! mainOn #f)
                                      (send main show mainOn)
                                      exit))]))

; displays main window
(send main show mainOn)