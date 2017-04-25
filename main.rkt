#lang racket

(require racket/gui rsound "HashTableDefinitions.rkt")
(include "charsheet.rkt")

; pictures and races and race descriptions from https://dnd.wizards.com/dungeons-and-dragons/ and D&D Player's Handbook 5e
; picture of cleric from http://forgottenrealms.wikia.com/wiki/Cleric and description from D&D Player's Handbook 5e
; picture of wizard from http://forgottenrealms.wikia.com/wiki/Wizard and description from D&D Player's handbook 5e
; picture of barbarian from http://www.sidharthchaturvedi.com/2014/08/d-next-barbarian.html and description from D&D Player's Handbook 5e
(define logo (read-bitmap "./DND/ddlogo.png"))
(define logo-w (send logo get-width))
(define logo-h (send logo get-height))
(define frame-w 820)
(define frame-h 820)
(define choice-panel-h 630)

; sets picture shown on race and class selection screens when an option is selected
(define pic 'nil)
(define (set-pic choice)
  (set! pic (read-bitmap (string-append (path->string (cdr choice)) "/pic.png"))))

; Johnny Douglas' Dungeons & Dragons cartoon theme
; converted from youtube video
; https://www.youtube.com/watch?v=v2u7-M7Ouok&t=37s
(define main-theme (rs-read "./DND/DDTheme.wav"))

; Human theme is "Caramel" by Suzanne Vega from the album "Nine Objects of Desire"
; High Elf theme is "Lothlorien" by Enya from the album "Shepherd Moons"
; Mountain Dwarf theme is "Hall of the Mountain King" by Savatage from the album "Hall of the Mountain King"

(define current-theme main-theme)
(define play-music-message "Toggle Music")
(define music-status #t)
; plays background theme if different from theme currently playing
(define (play-theme choice)
  (let ((song (rs-read (string-append (path->string (cdr choice)) "/song.wav"))))
        (unless (equal? song current-theme)
          (when music-status (begin (stop) (set! current-theme song) (play current-theme))))))

(play current-theme)

; adjust points to allocate to stats
(define points-to-allocate 0)
(define (inc-points)
  (set! points-to-allocate (add1 points-to-allocate)))
(define (dec-points)
  (unless (<= points-to-allocate 0) (set! points-to-allocate (sub1 points-to-allocate))))

; updates points to allocate in points tab panel
(define (inc-stat-points x)
  (unless (<= points-to-allocate 0) (begin (inc-stat x) (dec-points)  (send points-tally on-paint))))
(define (dec-stat-points x)
  (unless (<= (get-stat-num x) 0) (begin (dec-stat x) (inc-points)  (send points-tally on-paint))))

; update hp based on change to constitution modifier
(define base-hp 0)
(define (update-hp op)
  (let* ((old-con-mod (getmod "constitution")) (new-con-mod (calc-mod "constitution" op 1))
                                               (diff (- new-con-mod old-con-mod))
                                               (new-base-hp (+ base-hp new-con-mod)))
    (unless (or (eqv? diff 0) (<= new-base-hp 0) (<= points-to-allocate 0) (<= (get-stat-num "constitution") 0)) (set-hash-base "hp" new-base-hp)
                                )))

; calculates ability modifier based on changes in the modifier
(define (calc-mod str op num)
  (floor (/ (- (op (car (getstat str)) num) 10) 2))
  )

; sets race in stats hash table based on radiobox choice
(define race 'nil)
(define (set-race x)
  (let ((choice (list-ref (get-race-list) x)))
        (begin (set-pic choice) (set! race choice) (re-init-stats) (generatestats)
               (set-race-init choice) (setspeed)
               (unless (equal? class 'nil) (set-class-init class)) (play-theme choice))))

; sets speed of character
(define (setspeed)
  (cond ((equal? (car race) "Mountain Dwarf") (set-hash-base "speed" 20))
        ((or (equal? (car race) "Human") (equal? (car race) "High Elf")) (set-hash-base "speed" 30))))

; sets class and class stats
(define class 'nil)
(define (set-class x)
  (let ((choice (list-ref (get-class-list) x)))
    (begin (set-pic choice) (re-init-stats) (generatestats) (set! class choice)
           (unless (equal? race 'nil) (set-race-init race)) (set-class-init choice)
             (set! base-hp (- (getHP) (getmod "constitution"))))))



; makes a roman font of a certain size
(define (my-roman-font size)
  (make-object font% size 'roman))

; variable to show main screen
(define mainOn #t)

; main window
(define main (new frame% [label "D & D Character Generator"]
                  [width frame-w]
                  [height frame-h]
                  ))

(define main2 (new vertical-panel% [parent main]
                   [style '(vscroll)]))
; panel for logo
(define logo-panel (new horizontal-panel% [parent main2]
                       [min-height logo-h]))

; panels for attributes and character generation
(define choice-panel (new horizontal-panel% [parent main2]
                         [min-height choice-panel-h]))
(define gen-panel (new horizontal-panel% [parent main2]
                      [alignment '(center center)]))

;draws logo
(new canvas% [parent logo-panel]
    [style '(control-border no-focus)]
    [paint-callback
     (λ (canvas dc)
       (send dc set-scale (/ (send logo-panel get-width) logo-w) (/ (send logo-panel get-height) logo-h))
       (send dc set-background "black")
       (send dc clear)
     (send dc draw-bitmap logo 0 0))])

; tabs for various attribute choices
(define tab (new tab-panel%
                 [parent choice-panel]
                 [choices (list "Names"
                                "Race"
                                "Class"
                                "Alignment"
                                "Base Stats")]
                 [callback (λ (b e)
                             (case (send b get-selection)
                               ((0) (send b change-children (λ (children) (list name-panel))))
                               ((1) (begin (send b change-children (λ (children) (list race-panel)))
                                           (if (equal? race 'nil) (begin (set-pic (list-ref (get-race-list) 0)))
                                               (begin (set-pic race) (send r-can on-paint)))))
                               ((2) (begin (send b change-children (λ (children) (list class-panel)))
                                           (if (equal? class 'nil) (begin (set-pic (list-ref (get-class-list) 0))
                                                                          (send c-can on-paint))
                                               (begin (set-pic class) (send c-can on-paint)))))
                               ((3) (send b change-children (λ (children) (list alignment-panel))))
                               ((4) (send b change-children (λ (children) (list stats-panel))))
                               ))]))

; panels for each attribute tab 
(define name-panel (new panel% [parent tab]))
(define race-panel (new panel% [parent tab]
                        [style '(deleted)]))
(define class-panel (new panel% [parent tab]
                         [style '(deleted)]))
(define alignment-panel (new horizontal-panel% [parent tab]
                             [style '(deleted)]))
(define stats-panel (new horizontal-panel% [parent tab]
                         [style '(deleted)]))

; text boxes for character and player names tab
(define align-name-panel (new vertical-panel% [parent name-panel]))
(define player-name (new text-field%
                        [label "Player Name"]
                        [parent align-name-panel]
                        [vert-margin 4]
                        [callback (λ (b e)
                                    (set-hash-base "player-name" (send player-name get-value)))]))

(define char-name (new text-field%
                        [label "Character Name"]
                        [parent align-name-panel]
                        [vert-margin 4]
                        [callback (λ (b e)
                                  (set-hash-base "character-name" (send char-name get-value)))]))

; race panel setup
(define splitrace (new horizontal-panel% [parent race-panel]))
(define l-panel (new panel% [parent splitrace]
                     [style '(border)]))

(define r-panel (new panel% [parent splitrace]
                     [style '(border)]))

(define r-can (new canvas%
                         [parent r-panel]
                         [min-height 630]
                          [min-width 650]
                                      [paint-callback
                                      (λ (c dc)
                                        (send dc clear)
                                        (send dc draw-bitmap pic 0 0)
                                       )]))
; choices for race tab
(define race-box (new radio-box%
                     [label "Race"]
                     [choices (map car (get-race-list))]
                     [parent l-panel]
                     [vert-margin 10]
                     [horiz-margin 5]
                     [style (list 'vertical 'vertical-label)]
                     [font (my-roman-font 20)]
                     [callback (λ (b e)
                               (begin (set-race (send race-box get-selection))
                                 (send r-can on-paint)
                                 ))]))
                          
; class panel setup
(define splitclass (new horizontal-panel% [parent class-panel]))
(define l-c-panel (new panel% [parent splitclass]
                       [style '(border)]))
(define r-c-panel (new panel% [parent splitclass]
                       [style '(border)]))
(define c-can (new canvas%
                         [parent r-c-panel]
                         [min-height 630]
                          [min-width 650]
                                      [paint-callback
                                      (λ (c dc)
                                        (send dc clear)
                                        (send dc draw-bitmap pic 0 0)
                                       )]))

; alignment tabs and functions
(define (get-alignment-list)
  (list "Lawful Good" "Lawful Neutral" "Lawful Evil"
                                          "Neutral Good" "Neutral" "Neutral Evil"
                                          "Chaotic Good" "Chaotic Neutral" "Chaotic Evil"))

(define (set-alignment align)
  (set-hash-base "alignment" (list-ref (get-alignment-list) align)))

(define (get-alignment align)
  (file->string (string-append "./DND/" align ".txt")))

(define l-alignment-panel (new panel%
                           [parent alignment-panel]
                           [style '(border)]))

(define r-alignment-panel (new panel%
                           [parent alignment-panel]
                           [style '(border)]))

(define a-can (new canvas%
                         [parent r-alignment-panel]
                         [min-height 630]
                          [min-width 650]
                                      [paint-callback
                                      (λ (c dc)
                                        (begin (send dc clear)
                                               (send dc set-scale 1.5 1.5)
                                        (unless (equal? "" (get-hash-base "alignment")) (unless (equal? "" (get-alignment (get-hash-base "alignment")))
                                                         (begin (define (print-string str x y delta length)
                                                                  (unless (equal? str "")
                                                                    (begin (define align-list (string-split str))
                                                                           (define len length)
                                                                           (define (ps lst str x y delta)
                                                                             (cond ((equal? lst '()) (send dc draw-text str x y))
                                                                                   (else (let ((z (string-append str " " (car lst))))
                                                                                           (if (> (string-length z) len) (begin (send dc draw-text str x y) (ps lst "" x (+ y delta) delta))
                                                                                               (ps (cdr lst) z  x y delta)))))))
                                                                    (ps align-list "" x y delta)))
                                                                (print-string (get-alignment (get-hash-base "alignment")) 0 5 20 50))))))
                                      ]))
                                      
                                       

(define alignment-box (new radio-box%
                           [label "Alignment"]
                           [choices (get-alignment-list)]
                           [parent l-alignment-panel]
                           [style  '(vertical)]
                           [font (my-roman-font 20)]
                           [callback (λ (b e)
                                       (begin (set-alignment (send alignment-box get-selection))
                                              (send a-can on-paint)))]))

; choices for class tab
(define class-box (new radio-box%
                     [label "Class"]
                     [choices (map car (get-class-list))]
                     [parent l-c-panel]
                     [style (list 'vertical 'vertical-label)]
                     [font (my-roman-font 20)]
                     [selection 0]
                     [callback (λ (b e)
                               (begin (set-class (send class-box get-selection)))
                                (send c-can on-paint))]))

; tab panel to adjust base stats
(define l-stats (new vertical-panel% [parent stats-panel]
                     [style '(border)]
                     [min-width 300]))
(define m-stats (new vertical-panel% [parent stats-panel]
                     [style '(border)]))
(define r-stats (new vertical-panel% [parent stats-panel]
                     [style '(border)]
                     [min-width 300]))



(define points-panel (new vertical-panel% [parent m-stats]))
(define str-panel (new horizontal-panel% [parent m-stats]))
(define str-box (new horizontal-panel% [parent m-stats]))
(define l-str (new panel% [parent str-box]
                   [min-width 25]))
(define str-canvas (new canvas% [parent str-box]
                        [min-height 20]
                        [style '(no-focus)]
                        [paint-callback (λ (c dc)
                                    (begin (send dc clear)
                                           (send dc set-scale 2 2)
                                           (send dc set-text-foreground "blue")
                                           (send dc draw-text (number->string (get-stat-num "strength")) 2 0)))]))
(define r-str (new panel% [parent str-box]
                   [min-width 25]))

(define dex-panel (new horizontal-panel% [parent m-stats]))
(define dex-box (new horizontal-panel% [parent m-stats]))
(define l-dex (new panel% [parent dex-box]
                   [min-width 25]))
(define dex-canvas (new canvas% [parent dex-box]
                        [min-height 20]
                        [style '(no-focus)]
                        [paint-callback (λ (c dc)
                                    (begin (send dc clear)
                                           (send dc set-scale 2 2)
                                           (send dc set-text-foreground "blue")
                                           (send dc draw-text (number->string (get-stat-num "dexterity")) 2 0)))]))
(define r-dex (new panel% [parent dex-box]
                   [min-width 25]))

(define con-panel (new horizontal-panel% [parent m-stats]))
(define con-box (new horizontal-panel% [parent m-stats]))
(define l-con (new panel% [parent con-box]
                   [min-width 25]))
(define con-canvas (new canvas% [parent con-box]
                        [min-height 20]
                        [style '(no-focus)]
                        [paint-callback (λ (c dc)
                                    (begin (send dc clear)
                                           (send dc set-scale 2 2)
                                           (send dc set-text-foreground "blue")
                                           (send dc draw-text (number->string (get-stat-num "constitution")) 2 0)))]))
(define r-con (new panel% [parent con-box]
                   [min-width 25]))

(define wis-panel (new horizontal-panel% [parent m-stats]))
(define wis-box (new horizontal-panel% [parent m-stats]))
(define l-wis (new panel% [parent wis-box]
                   [min-width 25]))
(define wis-canvas (new canvas% [parent wis-box]
                        [min-height 20]
                        [style '(no-focus)]
                        [paint-callback (λ (c dc)
                                    (begin (send dc clear)
                                           (send dc set-scale 2 2)
                                           (send dc set-text-foreground "blue")
                                           (send dc draw-text (number->string (get-stat-num "wisdom")) 2 0)))]))
(define r-wis (new panel% [parent wis-box]
                   [min-width 25]))

(define int-panel (new horizontal-panel% [parent m-stats]))
(define int-box (new horizontal-panel% [parent m-stats]))
(define l-int (new panel% [parent int-box]
                   [min-width 25]))
(define int-canvas (new canvas% [parent int-box]
                        [min-height 20]
                        [style '(no-focus)]
                        [paint-callback (λ (c dc)
                                    (begin (send dc clear)
                                           (send dc set-scale 2 2)
                                           (send dc set-text-foreground "blue")
                                           (send dc draw-text (number->string (get-stat-num "intelligence")) 2 0)))]))
(define r-int (new panel% [parent int-box]
                   [min-width 25]))

(define cha-panel (new horizontal-panel% [parent m-stats]))
(define cha-box (new horizontal-panel% [parent m-stats]))
(define l-cha (new panel% [parent cha-box]
                   [min-width 25]))
(define cha-canvas (new canvas% [parent cha-box]
                        [min-height 20]
                        [style '(no-focus)]
                        [paint-callback (λ (c dc)
                                    (begin (send dc clear)
                                           (send dc set-scale 2 2)
                                           (send dc set-text-foreground "blue")
                                           (send dc draw-text (number->string (get-stat-num "charisma")) 2 0)))]))
(define r-cha (new panel% [parent cha-box]
                   [min-width 25]))

(define blank-panel (new panel% [parent m-stats]
                         [min-height 5]))

(define points-header (new message% [parent l-stats]
                           [label "Points to Allocate"]
                           [font (my-roman-font 20)]))
(define points-pan (new horizontal-panel% [parent l-stats]
                        [style '(border)]))
(define points-l-buffer (new panel% [parent points-pan]
                             [min-width 15]))
(define points-tally (new canvas% [parent points-pan]
                          [min-height 50]
                          [style '(no-focus)]
                          [paint-callback (λ (canvas dc)
                                            (send dc clear)
                                            (send dc set-text-foreground "blue")
                                            (send dc set-scale 4 4)
                                            (send dc draw-text (number->string points-to-allocate) 2 0))]))
(define points-r-buffer (new panel% [parent points-pan]
                             [min-width 15]))
(define blank-points-panel (new panel% [parent l-stats]
                                [min-height 400]
                                [style '(border)]))

; panels showing hit points

(define hp-header (new message% [parent r-stats]
                       [label "Hit Points"]
                       [font (my-roman-font 20)]))
(define hp-pan (new horizontal-panel% [parent r-stats]
                        [style '(border)]))
(define hp-l-buffer (new panel% [parent hp-pan]
                             [min-width 15]))
(define hp-tally (new canvas% [parent hp-pan]
                          [min-height 50]
                          [style '(no-focus)]
                          [paint-callback (λ (canvas dc)
                                            (send dc clear)
                                            (send dc set-text-foreground "blue")
                                            (send dc set-scale 4 4)
                                            (when (number? (getHP)) (send dc draw-text (number->string (getHP)) 2 0)))]))
(define hp-r-buffer (new panel% [parent hp-pan]
                             [min-width 15]))
(define blank-hp-panel (new panel% [parent r-stats]
                                [min-height 400]
                                [style '(border)]))

; adjust strength
(define dec-str (new button% [parent str-panel]
                     [label "-"]
                     [font (my-roman-font 15)]
                     (callback (λ ( b e)
                                 (begin  (dec-stat-points "strength") (send str-canvas on-paint)
                                        )))))
(define str-message (new message% [parent str-panel]
                         [label "Strength"]
                         [font (my-roman-font 15)]))
(define inc-str (new button% [parent str-panel]
                     [label "+"]
                     [font (my-roman-font 15)]
                     (callback (λ (b e)
                                 (begin  (inc-stat-points "strength") (send str-canvas on-paint))))))

; adjust dexterity
(define dec-dex (new button% [parent dex-panel]
                     [label "-"]
                     [font (my-roman-font 15)]
                     (callback (λ ( b e)
                                 (begin (dec-stat-points "dexterity") (re-eval) (send dex-canvas on-paint)
                                        )))))
(define dex-message (new message% [parent dex-panel]
                         [label "Dexterity"]
                         [font (my-roman-font 15)]))
(define inc-dex (new button% [parent dex-panel]
                     [label "+"]
                     [font (my-roman-font 15)]
                     (callback (λ (b e)
                                 (begin (inc-stat-points "dexterity") (re-eval) (send dex-canvas on-paint))))))

; adjust constitution
(define dec-con (new button% [parent con-panel]
                     [label "-"]
                     [font (my-roman-font 15)]
                     (callback (λ ( b e)
                                 (begin (update-hp -) (dec-stat-points "constitution")
                                        (send hp-tally on-paint) (send con-canvas on-paint))))))
                    
(define con-message (new message% [parent con-panel]
                         [label "Constitution"]
                         [font (my-roman-font 15)]))
(define inc-con (new button% [parent con-panel]
                     [label "+"]
                     [font (my-roman-font 15)]
                     (callback (λ (b e)
                                 (begin (update-hp +) (inc-stat-points "constitution")
                                        (send hp-tally on-paint) (send con-canvas on-paint))))))

; adjust wisdom
(define dec-wis (new button% [parent wis-panel]
                     [label "-"]
                     [font (my-roman-font 15)]
                     (callback (λ ( b e)
                                 (begin (dec-stat-points "wisdom") (send wis-canvas on-paint))))))
(define wis-message (new message% [parent wis-panel]
                         [label "Wisdom"]
                         [font (my-roman-font 15)]))
(define inc-wis (new button% [parent wis-panel]
                     [label "+"]
                     [font (my-roman-font 15)]
                     (callback (λ (b e)
                                 (begin (inc-stat-points "wisdom") (send wis-canvas on-paint))))))

; adjust intelligence
(define dec-int (new button% [parent int-panel]
                     [label "-"]
                     [font (my-roman-font 15)]
                     (callback (λ ( b e)
                                 (begin (dec-stat-points "intelligence") (send int-canvas on-paint))))))
(define int-message (new message% [parent int-panel]
                         [label "Intelligence"]
                         [font (my-roman-font 15)]
                         ))
(define inc-int (new button% [parent int-panel]
                     [label "+"]
                     [font (my-roman-font 15)]
                     (callback (λ (b e)
                                 (begin (inc-stat-points "intelligence") (send int-canvas on-paint))))))

; adjust charisma
(define dec-cha (new button% [parent cha-panel]
                     [label "-"]
                     [font (my-roman-font 15)]
                     (callback (λ ( b e)
                                 (begin (dec-stat-points "charisma") (send cha-canvas on-paint))))))
(define cha-message (new message% [parent cha-panel]
                         [label "Charisma"]
                         [font (my-roman-font 15)]
                         ))
(define inc-cha (new button% [parent cha-panel]
                     [label "+"]
                     [font (my-roman-font 15)]
                     (callback (λ (b e)
                                 (begin (inc-stat-points "charisma") (send cha-canvas on-paint))))))

; reroll stats button
(define reroll (new button%
                    [label "Reroll Stats"]
                    [parent gen-panel]
                    [callback (λ (b e)
                                (begin (generatestats)
                                       (set-class-init class)
                                       (set! points-to-allocate 0)
                                       (send str-canvas on-paint)
                                       (send dex-canvas on-paint)
                                       (send con-canvas on-paint)
                                       (send wis-canvas on-paint)
                                       (send int-canvas on-paint)
                                       (send cha-canvas on-paint)
                                       (send points-tally on-paint)
                                       (send hp-tally on-paint)))]))

; character sheet generation button
(define genSheet (new button%
                      [label "Generate Character Sheet"]
                      [parent gen-panel]
                      [callback (λ  (b e)
                                  (genCS #t))]))

; closes character sheet button
(define closeSheet (new button%
                        [label "Close Character Sheet"]
                        [parent gen-panel]
                        [callback (λ (b e)
                                    (genCS #f))]))

(define play-song-button (new button%
                              [label play-music-message]
                              [parent gen-panel]
                              [callback (λ (b e)
                                          (if music-status (begin (stop) (set! music-status #f))
                                              (begin (play main-theme) (set! music-status #t))))]))

; Exits application button
(define Exit (new button%
                  [label "Exit"]
                  [parent gen-panel]
                  [callback (λ (b e)
                              (begin (stop)
                                     (genCS #f)
                                      (set! mainOn #f)
                                      (send main show mainOn)
                                      exit))]))

; displays main window
(send main show mainOn)