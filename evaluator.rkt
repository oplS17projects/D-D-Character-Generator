#lang racket
(require csv-reading)
; Csv viewing functions
; Copied from csv article
; docs.racket-lang.org/csv-reading/index.html
;(define make-food-csv-reader
;  (make-csv-reader-maker
;   '((separator-chars            #\|)
;     (strip-leading-whitespace?  . #t)
;     (strip-trailing-whitespace? . #t))))
;     
;(define (next-row path)
;  (make-food-csv-reader (open-input-file path)))
(require "HashTableDefinitions.rkt")

; Static Vars
(define nil '())
(define folder-base "DND")

; Relevant Structs - will be deleted when this is cleaned up
(define-struct Stat-Struct (Num IsSavingThrow) #:transparent)
(define-struct Skill-Struct (IsProficient ParentSkillName) #:transparent)

; Generate Stats List
(define (generatestats)
  (begin
    (set-stat 'strength (diceroll"3d6") #f)
    (set-stat 'dexterity (diceroll"3d6") #f)
    (set-stat 'constitution (diceroll"3d6") #f)
    (set-stat 'wisdom (diceroll"3d6") #f)
    (set-stat 'intelligence (diceroll"3d6") #f)
    (set-stat 'charisma (diceroll"3d6") #f)
  )
  "Ok\n"
  )

; Generate Skill List
(define (generateskills)
  (define skills-file (string-append folder-base "/skills.csv"))
  (csv-map
   (lambda (n)
     (cons (car n)
           (make-Skill-Struct 0 (car (cdr n)))))
   (open-input-file skills-file))
  )


; Assumes exp is a list
(define (evaluator exp)
  (begin
;    (display "Trying: ")
;    (display exp)
;    (display "\n")
    (cond
      [(number? exp) exp]
      [(diceroll? exp) (diceroll exp)]
      [(add:? exp) (add:+ exp)]
      [(sub:? exp) (sub:- exp)]
      [(choose? exp) (choose exp)]
      [(add-item? exp) (add-item exp)]
      [(add-weapon? exp) (add-weapon exp)]
      [(roll-gold? exp) (roll-gold exp)]
      [(isMod? exp) (string->mod exp)]
      [(isSetHp? exp) (setHP exp)]
      [(add-profiencies? exp) (add-profiencies exp)]
      [(self-eval? exp) (self-eval exp)]
      [(set-hit-dice? exp) (set-hit-dice exp)]
      [(set-skill-proficient? exp) (set-skill-proficient exp)]
      [(set-saving-throw? exp) (set-saving-throw exp)]
      [(add-skill-choice? exp) (add-to-choice-skill-list exp)]
      [(inc-skill-choice? exp) (inc-choice-counter exp)]
      [(add-ability? exp) (add-ability exp)]
      [(add-note? exp) (add-note exp)]
      [(set-ac? exp) (set-ac exp)]
      [else (begin
              (display "Failed on: " )
              (display exp)
              (display "\n\n")
              #f)])))

; Returns true if exp1 is a list and matches exp2
; Taken from evaluator hw
(define (if-exp-match? exp1 exp2)
  (if (list? exp1)
      (if (string? (car exp1))
          (string=? (remove-white-space (car exp1)) exp2)
          (string=? (remove-white-space (symbol->string (car exp1))) exp2))
      #f))

; Add functions
(define (add:? exp)
  (if-exp-match? exp "add:+"))

(define (add:+ exp)
  (if (or (null? exp) (null? (cdr exp)))
      0
        (proclst (map (lambda (n) (evaluator n)) (cdr exp)) +)))

(define (proclst lst proc)
  (if (null? lst)
      0
      (apply proc lst)))

; Sub functions
(define (sub:? exp)
  (if-exp-match? exp "sub:-"))

(define (sub:- exp)
(if (or (null? exp) (null? (cdr exp)))
      0
        (proclst (map (lambda (n) (evaluator n)) (cdr exp)) -)))

; Choose function
(define (choose? exp)
  (if-exp-match? exp "choose"))

(define (choose exp)
  (evaluator (string->csvlist (car (shuffle (cdr exp))))))

(define (string->csvlist str)
  (string-split str " "))

; Inventory functions

(define (add-item? exp)
  (if-exp-match? exp "add-item"))

(define (add-item exp) ; Adds item and quantitty to inventory
  (define inputitem (cons (remove-beginning-white-space (cadr exp)) (remove-beginning-white-space (caddr exp))))
  (add-item-to-hash (car inputitem) (cdr inputitem)))

(define (add-weapon? exp)
  (if-exp-match? exp "add-weapon"))

(define (add-weapon exp) ; Adds item and quantitty to inventory
  (define inputitem (cons (remove-beginning-white-space (cadr exp)) (remove-beginning-white-space (caddr exp))))
  (add-weapon-to-hash (car inputitem) (cdr inputitem)))

(define (roll-gold? exp)
  (if-exp-match? exp "roll-gold"))

(define (roll-gold exp) ; Evaluates the amount of gold and adds it to the inventory
  (define goldnum (proclst (map (lambda (n) (evaluator n)) (map (lambda (n) (remove-white-space n)) (cdr exp))) *))
  (evaluator (list "add-item" "Gold" goldnum)))

(define (getstat str)
  (get-stat str)
  )

(define (getmod str)
  (floor (/ (- (car (getstat str)) 10) 2))
  )

(define (isSetHp? exp)
  (or (if-exp-match? exp "setHP") (if-exp-match? exp "1st-lvl-hp")))

(define (setHP exp) ; Would change stat in hash table to the following value value
  (define hp (evaluator (just-string-to-csv (car (cdr exp)))))
  (hash-set! hash-base 'character-hp hp)
)

(define (getHP) ; Would lookup hp in hash table and return value, for now returning 10
  (hash-ref hash-base 'character-hp)
  )

(define (add-profiencies? exp)
  (if-exp-match? exp "add-profiencies"))

(define (add-profiencies exp)
  ; Would add profiency to list of profiencies
  ; instead print it out
  (hash-set! hash-proficiencies-list (cdr exp) (cdr exp))
  )

(define (set-hit-dice? exp)
  (if-exp-match? exp "set-hit-dice"))

(define (set-hit-dice exp) ; Would set the relephant hash in the table to this value
  (hash-set! hash-base 'character-hit-dice (cadr exp))
  "set-hit-dice Ok\n"
  )

(define (set-skill-proficient? exp)
  (or (if-exp-match? exp "add-skill") (if-exp-match? exp "set-skill-prof")))

(define (set-skill-proficient exp) ; Would refrence hash table for skills and mark them proficient
  (set-skill-prof (cadr exp) #t)
  "set-skill-prof Ok\n")

(define (add-note? exp)
  (if-exp-match? exp "add-note"))

(define (add-note exp) ; Needs to add note to table, for now just prints it out
  (add-note-to-hash (cadr exp))
  "Ok\n")

(define (is-this-in-list-string this lst) ; Used for filtering through the choice list against proficient skills
  (define itm this)
  (define fst "")
  (if (null? lst)
      #t
      (begin
        (cond [(symbol? itm) (set! itm (symbol->string itm))])
        (if (symbol? (car lst))
            (set! fst (symbol->string (car lst)))
            (set! fst (car lst)))
        (if (string=? itm fst)
            #f
            (is-this-in-list-string this (cdr lst))))))

(define (add-skill-choice? exp)
  (if-exp-match? exp "add-skill-choice"))

(define (add-to-choice-skill-list exp)
  (add-skill-choice (cdr exp)))

(define (inc-skill-choice? exp)
  (if-exp-match? exp "inc-skill-choice"))

(define (inc-choice-counter exp)
  (define num (car (cdr exp)))
  (define lst (filter (lambda (n) (is-this-in-list-string n (get-list-of-prof-names)))
                      (map (lambda (n)
                             (string->symbol (string-downcase (remove-beginning-white-space (car (cdr n))))))
                             (hash->list hash-choice-lists))))
  (begin
    (cond [(string? num) (set! num (string->number (remove-beginning-white-space num)))])
    (eval-choices-skills num lst)
    (hash-clear! hash-choice-lists)
    "inc choices Ok\n"))

(define (eval-choices-skills num lst) ; Using list and counter above instead of hash table
  (define lists "")
  (if (or (= num 0) (null? lst))
      "Eval choices Ok\n"
     (begin
       (set! lists (shuffle lst))
       (display (car lists))
       (display " ")
       (display num)
       (display "\n\n")
       (set-skill-prof (car lists) #t)
       (eval-choices-skills (- num 1) (cdr lists))
       )))

(define (isMod? exp)
  (if (list? exp)
      #f
      (if (string? exp)
          (regexp-match? #px"[a-z]+-mod"  exp)
          (regexp-match? #px"[a-z]+-mod"  (symbol->string exp))
          )))

(define (string->mod exp)
  (getmod (car (regexp-match #px"[a-z]+" exp))))

(define (self-eval? exp)
  (not (list? exp)))

(define (self-eval exp)
  (cond
    [(string->number exp) (string->number exp)]
    [(string=? "getHP" exp) (getHP)]
    [else exp]
    ))

(define (set-saving-throw? exp)
  (if-exp-match? exp "set-saving-throw"))

(define (set-saving-throw exp)
  (set-stat (remove-beginning-white-space (car (cdr exp))) (get-stat-num (remove-beginning-white-space (car (cdr exp)))) #t))

(define (add-ability? exp)
  (if-exp-match? exp "add-ability"))

(define (add-ability exp)
  (hash-set! hash-abilities (cadr exp) (string->number (remove-beginning-white-space (car (cddr exp))))))

(define (set-ac? exp)
  (if-exp-match? exp "set-ac"))

(define (set-ac exp)
  (hash-set! hash-base 'character-armor-class-eval (just-string-to-csv (car (cdr exp))))
  (hash-set! hash-base 'character-armor-class (evaluator (hash-ref hash-base 'character-armor-class-eval)))
  "set-ac Ok\n"
  )

; Diceroll?
(define (diceroll? dr)
  (begin
    (define dir 0)
    (if (list? dr)
        (set! dir (remove-white-space (car dr)))
        (set! dir (remove-white-space dr)))
    (if (string? dir )
        (regexp-match? #px"[0-9]+d[0-9]+"  dir)
        #f)))

(define (diceroll roll-string)
  (define numdice (car (regexp-match #px"[0-9]+" roll-string)))
  (define dicesize (car (regexp-match #px"[0-9]+" (car (regexp-match #px"d[0-9]+"  roll-string)))))
  (roll (string->number numdice) (string->number dicesize))
  )

(define (roll numdice dicesize)
  (if (eq? numdice 0)
      0
      (+ (+ 1 (random  dicesize)) (roll (- numdice 1) dicesize))))

(define (just-string-to-csv exp)
  (car (csv->list (string-join (map (lambda (n) (remove-white-space n)) (string-split exp " ")) ","))))

; File Functions
; Get List of Files

; Get list of backgrounds in format cons("Name", file)
(define (get-background-list)
  (define location (string-append folder-base "/background"))
  (map (lambda (f) (cons (remove-csv (path-name-to-file-name (path->string f))) f)) (directory-list location #:build? #t))
  )

; Get list of classes
(define (get-class-list)
  (define location (string-append folder-base "/class"))
  (map (lambda (f) (cons (string-titlecase (path-name-to-file-name (path->string f))) f)) (directory-list location #:build? #t))
  )

; Get list of races

(define (get-race-list)
  (define location (string-append folder-base "/race"))
  (map (lambda (f) (cons (remove-csv (path-name-to-file-name (path->string f))) f)) (directory-list location #:build? #t))
  )

; Random Helper Functions
; Remove Csv, given filename, return Name without csv
;            Ex: "Outlander.csv" -> "Outlander"
(define (remove-csv filenamecsv)
  (string-titlecase (car (string-split filenamecsv ".")))
  )

; Given pathname, get file name
;            Ex: "DND/background/outlander.csv" -> "outlander.csv"
(define (path-name-to-file-name filenamepath)
  (define str (string-split filenamepath "/"))
  (list-ref str (- (length str) 1))
  )

; Removes white space
(define (remove-white-space str)
  (if (string? str)
      (list->string (filter (lambda (c) (not (char-whitespace? c))) (string->list str)))
      str))

(define (remove-beginning-white-space str)
  (if (string? str)
      (string-trim str #:right? #f)
      str))

(define (if-str-then-num str)
  (if (string? str)
      (string->number str)
      str))

(define (re-eval)
  (begin
    (hash-set! hash-base 'character-armor-class (evaluator (hash-ref hash-base 'character-armor-class-eval)))
    "Reeval Ok\n"))

(define (reset-hash-table)
  (for-each ;reset base stats
   (lambda (n)
     (hash-set! hash-base n ""))
   (hash->list hash-base))
  (hash-set! hash-base 'character-proficiency-bonus 0)
  
  (for-each ;reset skills
   (lambda (n)
     (set-skill-prof n #f))
   (hash->list hash-skills))

  (hash-clear! hash-inventory)
  (hash-clear! hash-choice-lists)
  (hash-clear! hash-notes)
  (hash-clear! hash-abilities)
  (hash-clear! hash-weapons)

  
  "Reset OK\n"
  )

; Setting and evaluating race, class, armor
(define (set-class-init pr)
  (begin
    (hash-set! hash-base 'character-class (string-titlecase (car pr)))
    (hash-set! hash-base 'character-level 1)
    (csv-map evaluator (open-input-file (string-append (path->string (cdr pr)) "/level1.csv")))
    "Set-class Ok\n"
    ))

(define (set-class-level pr lvl)
  (begin
    (hash-set! hash-base 'character-class (string-titlecase (car pr)))
    (csv-map evaluator (open-input-file (string-append* (path->string (cdr pr)) "/level" (number->string lvl) ".csv" (list))))
    "Set-class Ok\n"
    ))

(define (set-background-init pr)
  (begin
    (hash-set! hash-base 'character-background (string-titlecase (car pr)))
    (csv-map evaluator (open-input-file (string-append (path->string (cdr pr)) "/data.csv")))
    "Set-background Ok\n"
    ))

(define (set-race-init pr)
  (begin
    (hash-set! hash-base 'character-race (string-titlecase (car pr)))
    (csv-map evaluator (open-input-file (string-append (path->string (cdr pr)) "/data.csv")))
    "Set-race Ok\n"
    ))

(define (get-text-file pr)
  (file->string (string-append (path->string (cdr pr)) "/info.txt")))

(provide (all-defined-out))

(define (print-info) ;Example print out function
  (begin
  (display "The character has the following information:\n\n\n Base stats:\n")
  (display "The character's class is: ") (display (hash-ref hash-base 'character-class))
  (display "\nThe character's race is: ") (display (hash-ref hash-base 'character-race))
  (display "\nThe character's background is: ") (display (hash-ref hash-base 'character-background))
  (display "\nThe character's overall proficiency-bonus is: ") (display (hash-ref hash-base 'character-proficiency-bonus))
  (display "\nThe character's hp is: ") (display (hash-ref hash-base 'character-hp))
  (display "\nThe character's hit-dice is: ") (display (hash-ref hash-base 'character-hit-dice))
  (display "\nThe character's armor class is: ") (display (hash-ref hash-base 'character-armor-class))
  (display "\nThe character's level is: ") (display (hash-ref hash-base 'character-level))

  (display "\n\nThe characters stats are the following: \n")
  (for-each
   (lambda (n)
         (display "\nStat name: ") (display (substring (symbol->string (car n)) 5))
         (display "\nStat number: ") (display (get-stat-num (substring (symbol->string (car n)) 5)))
         (display "\nStat mod: ") (display (get-stat-mod (substring (symbol->string (car n)) 5)))
         (display "\nIs saving throw: ") (display (cddr n))
         (display "\n"))
   (hash->list hash-stats))

  (display "\nThe full list of character skills are: \n")
  (for-each
   (lambda (n)
     (display "Skill name: ") (display (car n))
     (display "\nSkill parent: ") (display (cadr n))
     (display "\nIs proficient: ") (display (cddr n))
     (display "\nSkill modifier: ") (display (get-skill-mod (car n)))
     (display "\n"))
   (hash->list hash-skills))

  (display "\nThe following is a condensed list of character's proficient skills: \n")
  (for-each
   (lambda (n)
     (display n)
     (display "\n"))
   (get-list-of-prof-names))
  (display "\n\n")

  (display "\nThe full list of the character's inventory: \n")
  (for-each
   (lambda (n)
           (display "Item name: ") (display (car n))
           (display "\nItem quantity: ") (display (cddr n))
           (display "\n"))
   (hash->list hash-inventory))

    (display "\n\nThe full list of the character's weapons: \n")
  (for-each
   (lambda (n)
           (display "Weapon name: ") (display (car n))
           (display "\nWeapon quantity: ") (display (cddr n))
           (display "\n"))
   (hash->list hash-weapons))

  (display "Character abilities and uses: \n")
  (for-each
   (lambda (n)
     (display "Ability name: ") (display (car n))
     (display " with ") (display (cdr n))
     (display " uses.\n"))
   (hash->list hash-abilities))
  
  (display "\nNotes and proficiencies: \n")
  (for-each
   (lambda (n)
     (begin
       (display (car (cdr n)))
       (display "\n")))
   (hash->list hash-notes))
  (for-each
   (lambda (n)
     (begin
       (display (car (cdr n)))
       (display "\n")))
   (hash->list hash-proficiencies-list))
  
  ))