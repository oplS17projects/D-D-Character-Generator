#lang racket

(define hash-base (make-hash)) ; hash-hash is the hash used for the table

 ; fn to easily init variables
(define (hash-base-init sym) (hash-set! hash-base sym ""))

; defining base character variables
(hash-base-init 'character-class)
(hash-base-init 'character-race)
(hash-base-init 'character-background)
(hash-set! hash-base 'character-proficiency-bonus 0)
(hash-base-init 'character-hp)
(hash-base-init 'character-hit-dice)
(hash-base-init 'character-armor-class)
(hash-base-init 'character-armor-class-eval)
(hash-base-init 'character-level)

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
(hash-skills-init 'intimidaiton 'charisma #f)
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
  (define sk (get-skill sym))
  (if sk
      (hash-skills-init sym (car sk) prof)
      #f
      ))
(define (get-skill sym)
  (hash-ref hash-skills sym (lambda () #f)))
(define (get-skill-mod sym)
  (define sk (get-skill sym))
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
(define (add-ability-to-hash ability num)
  (hash-set! hash-abilities ability num))

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



(provide (all-defined-out))