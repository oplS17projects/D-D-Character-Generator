#lang racket
(define hash-base (make-hash)) ; hash-hash is the hash used for the table

 ; fn to easily init variables
(define (hash-base-init sym) (hash-set! hash-base sym ""))

; defining base character variables
(hash-base-init 'character-name)
(hash-base-init 'player-name)
(hash-base-init 'character-class)
(hash-base-init 'character-race)
(hash-base-init 'character-background)
(hash-set! hash-base 'character-proficiency-bonus 0)
(hash-base-init 'character-hp)
(hash-base-init 'character-hit-dice)
(hash-base-init 'character-armor-class)
(hash-base-init 'character-armor-class-eval)
(hash-base-init 'character-level)
(hash-base-init 'character-speed)
(hash-base-init 'character-alignment)

; define base stats
(define hash-stats (make-hash))
(define (hash-stats-init sym) (hash-set! hash-stats sym (cons 3 #f)))
(hash-stats-init 'stat-strength)
(hash-stats-init 'stat-dexterity)
(hash-stats-init 'stat-constitution)
(hash-stats-init 'stat-wisdom)
(hash-stats-init 'stat-intelligence)
(hash-stats-init 'stat-charisma)
(define (set-stat stat num saving-throw) ; Given stat, set the stat the the number and if it is that character's saving throw
  (define stata stat)
  (begin
    (cond ((symbol? stata) (set! stata (symbol->string stata))))
    (cond
      [(string=? stata "strength") (hash-set! hash-stats 'stat-strength (cons num saving-throw))]
      [(string=? stata "dexterity") (hash-set! hash-stats 'stat-dexterity (cons num saving-throw))]
      [(string=? stata "constitution") (hash-set! hash-stats 'stat-constitution (cons num saving-throw))]
      [(string=? stata "wisdom") (hash-set! hash-stats 'stat-wisdom (cons num saving-throw))]
      [(string=? stata "intelligence") (hash-set! hash-stats 'stat-intelligence (cons num saving-throw))]
      [(string=? stata "charisma") (hash-set! hash-stats 'stat-charisma (cons num saving-throw))]
      [else #f])))
(define (get-stat stat) ; Given stat, returns the cons(num, is saving throw)
  (define stata stat)
  (begin
    (cond ((symbol? stata) (set! stata (symbol->string stata))))
    (cond
      [(string=? stata "strength") (hash-ref hash-stats 'stat-strength)]
      [(string=? stata "dexterity") (hash-ref hash-stats 'stat-dexterity)]
      [(string=? stata "constitution") (hash-ref hash-stats 'stat-constitution)]
      [(string=? stata "wisdom") (hash-ref hash-stats 'stat-wisdom)]
      [(string=? stata "intelligence") (hash-ref hash-stats 'stat-intelligence)]
      [(string=? stata "charisma") (hash-ref hash-stats 'stat-charisma)]
      [else #f])))
(define (inc-stat stat) ; Given stat, increase it by 1
  (define got-stat (get-stat stat))
  (if (eq? got-stat #f)
      #f
      (set-stat stat (+ (car got-stat) 1) (cdr got-stat))))
(define (dec-stat stat) ; Given stat, decrease it by 1
  (define got-stat (get-stat stat))
  (if (eq? got-stat #f)
      #f
      (set-stat stat (- (car got-stat) 1) (cdr got-stat))))
(define (add-stat stat num) ; Given stat will be increased by num
  (define got-stat (get-stat stat))
  (if (eq? got-stat #f)
      #f
      (set-stat stat (+ (car got-stat) num) (cdr got-stat))))
(define (get-stat-num stat)
  (car (get-stat stat)))
(define (get-stat-mod stat)
  (floor (/ (- (get-stat-num stat) 10) 2)))

; define skills
(define hash-skills (make-hash))
(define (hash-skills-init sym parent isprof) (hash-set! hash-skills sym (cons parent isprof)))
(hash-skills-init 'acrobatics 'dexterity #f)
(hash-skills-init '|animal handling| 'wisdom #f)
(hash-skills-init 'arcana 'intelligence #f)
(hash-skills-init 'athletics 'strength #f)
(hash-skills-init 'dception 'charisma #f)
(hash-skills-init 'history 'intelligence #f)
(hash-skills-init 'insight 'wisdom #f)
(hash-skills-init 'intimidation 'charisma #f)
(hash-skills-init 'investigation 'intelligence #f)
(hash-skills-init 'medicine 'wisdom #f)
(hash-skills-init 'nature 'intelligence #f)
(hash-skills-init 'perception 'wisdom #f)
(hash-skills-init 'performance 'charisma #f)
(hash-skills-init 'persuasion 'charisma #f)
(hash-skills-init 'religion 'intelligence #f)
(hash-skills-init '|slight of hand| 'dexterity #f)
(hash-skills-init 'stealth 'dexterity #f)
(hash-skills-init 'survival 'wisdom #f)
(define (set-skill-prof sym prof)
  (define sym2 sym)
  (cond [(string? sym2) (set! sym2 (string->symbol sym2))])
  (define sk (get-skill sym2))
  (if sk
      (hash-skills-init sym2 (car sk) prof)
      #f
      ))
(define (get-skill sym)
  (define sym2 sym)
  (cond [(string? sym2) (set! sym2 (string->symbol sym2))])
  (hash-ref hash-skills sym2 (lambda () #f)))
(define (get-skill-mod sym)
  (define sym2 sym)
  (cond [(string? sym2) (set! sym2 (string->symbol sym2))])
  (define sk (get-skill sym2))
  (if sk
      (if (cdr sk)
          (+ (hash-ref hash-base 'character-proficiency-bonus) (get-stat-mod (car sk)))
          (get-stat-mod (car sk)))
      #f))
(define (get-list-of-prof-full) ; ex: '(("slight of hand" dexterity . #t))
  (filter (lambda (n) (cddr n)) (hash->list hash-skills)))
(define (get-list-of-prof-names) ; ex: '("slight of hand")
  (map car (get-list-of-prof-full)))

; inventory definitions
(define hash-inventory (make-hash))
(define (IsInInventory? item)
  (if (hash-ref hash-inventory item (lambda () #f))
      #t
      #f))
(define (add-item-to-hash item qty)
  (if (IsInInventory? item)
      (hash-set! hash-inventory item (cons item (+ (if (string? qty)
                                                       (string->number qty)
                                                       qty)
                                                   (if (string? (cdr (hash-ref hash-inventory item)))
                                                                (string->number (cdr (hash-ref hash-inventory item)))
                                                                (cdr (hash-ref hash-inventory item))))))
      (hash-set! hash-inventory item (cons item qty))))

; Lists for making random choices; ex: Choose any 2 skills: Stealth Survival Religion etc
(define hash-choice-lists (make-hash))
(define (add-skill-choice exp)
  (hash-set! hash-choice-lists (+ 1 (length (hash->list hash-choice-lists))) exp))
(define (get-skill-choice-list)
  (map cdr (hash->list hash-choice-lists)))

; General Notes for the character; ex: Can speak any 1 language of your choice
(define hash-notes (make-hash))
(define (add-note-to-hash note)
  (hash-set! hash-notes (+ 1 (length (hash->list hash-notes))) note)
  )

(define hash-abilities (make-hash))
(define (add-ability-to-hash ability num note)
  (hash-set! hash-abilities ability (cons num note)))

(define hash-proficiencies-list (make-hash))

; Weapon Definitions
(define hash-weapons (make-hash))
(define (IsInWeapons? item)
  (if (hash-ref hash-weapons item (lambda () #f))
      #t
      #f))
(define (add-weapon-to-hash item qty)
  (if (IsInWeapons? item)
      (hash-set! hash-weapons item (cons item (+ (if (string? qty)
                                                       (string->number qty)
                                                       qty)
                                                   (if (string? (cdr (hash-ref hash-weapons item)))
                                                                (string->number (cdr (hash-ref hash-weapons item)))
                                                                (cdr (hash-ref hash-weapons item))))))
      (hash-set! hash-weapons item (cons item qty))))


(define (set-hash-base ability value)
  (cond ((equal? ability "class") (hash-set! hash-base 'character-class value))
        ((equal? ability "race") (hash-set! hash-base 'character-race value))
        ((equal? ability "background") (hash-set! hash-base 'character-background value))
        ((equal? ability "character-proficieny-bonus") (hash-set! hash-base 'character-proficiency-bonus value))
        ((equal? ability "hp") (hash-set! hash-base 'character-hp value))
        ((equal? ability "hit-dice") (hash-set! hash-base 'character-hit-dice value))
        ((equal? ability "armor-class") (hash-set! hash-base 'character-armor-class value))
        ((equal? ability "armor-class-eval") (hash-set! hash-base 'character-armor-class-eval value))
        ((equal? ability "character-name") (hash-set! hash-base 'character-name value))
        ((equal? ability "player-name") (hash-set! hash-base 'player-name value))
        ((equal? ability "speed") (hash-set! hash-base 'character-speed value))
        ((equal? ability "alignment") (hash-set! hash-base 'character-alignment value))
        ))

(define (get-hash-base ability)
  (cond ((equal? ability "class") (hash-ref hash-base 'character-class))
        ((equal? ability "race") (hash-ref hash-base 'character-race))
        ((equal? ability "background") (hash-ref hash-base 'character-background))
        ((equal? ability "character-proficiency-bonus") (hash-ref hash-base 'character-proficiency-bonus))
        ((equal? ability "hp") (hash-ref hash-base 'character-hp))
        ((equal? ability "hit-dice") (hash-ref hash-base 'character-hit-dice))
        ((equal? ability "armor-class") (hash-ref hash-base 'character-armor-class))
        ((equal? ability "armor-class-eval") (hash-ref hash-base 'character-armor-class-eval))
        ((equal? ability "character-name") (hash-ref hash-base 'character-name))
        ((equal? ability "player-name") (hash-ref hash-base 'player-name))
        ((equal? ability "speed") (hash-ref hash-base 'character-speed))
        ((equal? ability "alignment") (hash-ref hash-base 'character-alignment))
        ))

(define (re-init-stats)
  (begin (hash-clear! hash-inventory)
         (hash-clear! hash-choice-lists)
         (hash-clear! hash-notes)
         (hash-clear! hash-abilities)
         (hash-clear! hash-weapons)
         (hash-clear! hash-stats)
         (hash-clear! hash-skills)
         (hash-clear! hash-proficiencies-list)
         (hash-stats-init 'stat-strength)
         (hash-stats-init 'stat-dexterity)
         (hash-stats-init 'stat-constitution)
         (hash-stats-init 'stat-wisdom)
         (hash-stats-init 'stat-intelligence)
         (hash-stats-init 'stat-charisma)
         (for-each (λ (x) (set-hash-base x ""))
              (list "class" "race" "background" "hp" "hit-dice" "armor-class" "armor-class-eval" "level"))
         (hash-set! hash-base 'character-proficiency-bonus 0)
         (hash-skills-init 'acrobatics 'dexterity #f)
         (hash-skills-init '|animal handling| 'wisdom #f)
         (hash-skills-init 'arcana 'intelligence #f)
         (hash-skills-init 'athletics 'strength #f)
         (hash-skills-init 'dception 'charisma #f)
         (hash-skills-init 'history 'intelligence #f)
         (hash-skills-init 'insight 'wisdom #f)
         (hash-skills-init 'intimidation 'charisma #f)
         (hash-skills-init 'investigation 'intelligence #f)
         (hash-skills-init 'medicine 'wisdom #f)
         (hash-skills-init 'nature 'intelligence #f)
         (hash-skills-init 'perception 'wisdom #f)
         (hash-skills-init 'performance 'charisma #f)
         (hash-skills-init 'persuasion 'charisma #f)
         (hash-skills-init 'religion 'intelligence #f)
         (hash-skills-init '|slight of hand| 'dexterity #f)
         (hash-skills-init 'stealth 'dexterity #f)
         (hash-skills-init 'survival 'wisdom #f)
         ))

(provide (all-defined-out))