#lang racket

(define hash-base (make-hash)) ; hash-hash is the hash used for the table

 ; fn to easily init variables
(define (hash-base-init sym) (hash-set! hash-base sym ""))

; defining base character variables
(hash-base-init 'character-class)
(hash-base-init 'character-race)
(hash-base-init 'character-background)

; define base stats
(define hash-stats (make-hash))
(define (hash-stats-init sym) (hash-set! hash-stats sym ""))
(hash-stats-init 'stat-strength)
(hash-stats-init 'stat-dexterity)
(hash-stats-init 'stat-constitution)
(hash-stats-init 'stat-wisdom)
(hash-stats-init 'stat-intelligence)
(hash-stats-init 'stat-charisma)
(define (set-stat stat num saving-throw) ; Given stat, set the stat the the number and if it is that character's saving throw
  (cond
    [(string=? stat "strength") (hash-set! hash-stats 'stat-strength (cons num saving-throw))]
    [(string=? stat "dexterity") (hash-set! hash-stats 'stat-dexterity (cons num saving-throw))]
    [(string=? stat "constitution") (hash-set! hash-stats 'stat-constitution (cons num saving-throw))]
    [(string=? stat "wisdom") (hash-set! hash-stats 'stat-wisdom (cons num saving-throw))]
    [(string=? stat "intelligence") (hash-set! hash-stats 'stat-intelligence (cons num saving-throw))]
    [(string=? stat "charisma") (hash-set! hash-stats 'stat-charisma (cons num saving-throw))]
    [else #f]))
(define (get-stat stat) ; Given stat, returns the cons(num, is saving throw)
  (cond
    [(string=? stat "strength") (hash-ref hash-stats 'stat-strength)]
    [(string=? stat "dexterity") (hash-ref hash-stats 'stat-dexterity)]
    [(string=? stat "constitution") (hash-ref hash-stats 'stat-constitution)]
    [(string=? stat "wisdom") (hash-ref hash-stats 'stat-wisdom)]
    [(string=? stat "intelligence") (hash-ref hash-stats 'stat-intelligence)]
    [(string=? stat "charisma") (hash-ref hash-stats 'stat-charisma)]
    [else #f]))
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


; other defs
(define hash-other (make-hash))
(define (hash-other-init sym) (hash-set! hash-other sym ""))
(hash-other-init 'inventory-list)
(hash-other-init 'weapons-lists)



(provide (all-defined-out))