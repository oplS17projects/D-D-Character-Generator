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


; Static Vars
(define nil '())
(define folder-base "DND")

; Relevant Structs
(define-struct Stat-Struct (Num IsSavingThrow) #:transparent)
(define-struct Skill-Struct (IsProficient ParentSkillName) #:transparent)

; Generate Stats List
(define (generatestats)
  (define stats-file (string-append folder-base "/stats.csv"))
  (define stats-roll (car (csv->list (open-input-file (string-append folder-base "/statsroll.csv")))))
  (csv-map (lambda (n)
              (cons
               (car n)
               (make-Stat-Struct (evaluator stats-roll) #f))) (open-input-file stats-file))
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
      [(roll-gold? exp) (roll-gold exp)]
      [(isMod? exp) (string->mod exp)]
      [(isSetHp? exp) (setHP exp)]
      [(add-profiencies? exp) (add-profiencies exp)]
      [(self-eval? exp) (self-eval exp)]
      [(set-hit-dice? exp) (set-hit-dice exp)]
      [(set-skill-proficient? exp) (set-skill-proficient exp)]
      [(add-note? exp) (add-note exp)]
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


; Begin TODO
; Inventory functions, this is a mockup and not the final product!!!!

(define (add-item? exp)
  (if-exp-match? exp "add-item"))

(define inventorylist (list))

(define (add-item exp) ;Need to actually add item to inventory, adds item to list ^
  ;(display (string-append "Adding item  " (cadr exp) ", of quantity " (caddr exp) ".\n"))
  (define itrnum (IsInInventory? (cadr exp) inventorylist))
  (define currentitem 0)
  (if itrnum
      (begin
        (set! currentitem (list-ref inventorylist itrnum))
        (RemoveFromInv currentitem)
        (set! currentitem (cons (car currentitem) (+ (caddr exp) (cdr currentitem))))
        (set! inventorylist (cons currentitem inventorylist)))
      (set! inventorylist (cons (cons (cadr exp) (caddr exp)) inventorylist))))

(define (IsInInventory? str lst)
  (define itrnum 0)
  (if (null? lst)
      #f
      (if (string=? (caar lst) "Gold")
          itrnum
          (begin
            (set! itrnum (+ itrnum 1))
            (IsInInventory? str (cdr lst))))))

(define (RemoveFromInv itm)
  (remove itm inventorylist))

(define (roll-gold? exp)
  (if-exp-match? exp "roll-gold"))

(define (roll-gold exp) ;Need to actually add gold to inventory
  (define goldnum (proclst (map (lambda (n) (evaluator n)) (map (lambda (n) (remove-white-space n)) (cdr exp))) *))
;  (define currentgold 0)
;  (define itrnum (IsInInventory? "Gold" inventorylist))      
;  (if itrnum
;      (begin
;        (set! currentgold (list-ref inventorylist itrnum))
;        (RemoveFromInv currentgold)
;        (set! currentgold (cdr currentgold))
;        (evaluator (list "add-item" "Gold" (+ currentgold goldnum))))
      (evaluator (list "add-item" "Gold" goldnum)))

(define (getstat str)
  ; Would refrence hash to get relevant stat
  ; Ie, (hash-get stat str)
  ; for now, using str as a number to test
  (if (number? str)
      str
      (cond
        [(string=? str "strength") 20]
        [(string=? str "dexterity") 16]
        [(string=? str "constitution") 12]
        [(string=? str "intelligence") 8]
        [(string=? str "wisdom") 4]
        [(string=? str "charisma") 10]
        [else 36]))
  )

(define (getmod str)
  (floor (/ (- (getstat str) 10) 2))
  )

(define (isSetHp? exp)
  (or (if-exp-match? exp "setHP") (if-exp-match? exp "1st-lvl-hp")))

(define (setHP exp) ; Would change stat in hash table to the following value value
  (display "Setting HP to: ")
  (display (evaluator (just-string-to-csv (car (cdr exp)))))
  (display "\n")
)

(define (getHP) ; Would lookup hp in hash table and return value, for now returning 10
  10
  )

(define (add-profiencies? exp)
  (if-exp-match? exp "add-profiencies"))

(define (add-profiencies exp)
  ; Would add profiency to list of profiencies
  ; instead print it out
  (display "Add profiency to list of profiencies")
  (display (car (cdr exp)))
  (display "\n")
  )

(define (set-hit-dice? exp)
  (if-exp-match? exp "set-hit-dice"))

(define (set-hit-dice exp) ; Would set the relephant hash in the table to this value
  (display "Setting hit dice to: ")
  (display (car (cdr exp)))
  (display "\n")
  )

(define (set-skill-proficient? exp)
  (or (if-exp-match? exp "add-skill") (if-exp-match? exp "set-skill-prof")))

(define (set-skill-proficient exp) ; Would refrence hash table for skills and mark them proficient
  (display "Setting skill to proficient: ")
  (display (car (cdr exp)))
  (display "\n"))

(define (add-note? exp)
  (if-exp-match? exp "add-note"))

(define (add-note exp) ; Needs to add note to table, for now just prints it out
  (display "Adding the following note: ")
  (display (car (cdr exp)))
  (display "\n"))


(define choice-list (list))
(define choice-count 0)

(define (add-to-choice-list exp)
  1)

(define (inc-choice-counter exp)
  (set! choice-count (car (cdr exp)))
  (eval-choices))

(define (eval-choices) ; Using list and counter above instead of hash table
  (if (or (null? choice-list) (= choice-count 0))
      (begin
        (set! choice-list (list))
        (set! choice-count 0))
      (map (lambda (n) (evaluator n)) (take (shuffle choice-list) choice-count))
  ))

; END TODO

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

(provide (all-defined-out))