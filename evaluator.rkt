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
    (display "Trying: ")
    (display exp)
    (display "\n")
    (cond
      [(number? exp) exp]
      [(diceroll? exp) (diceroll exp)]
      [(add:? exp) (add:+ exp)]
      [(sub:? exp) (sub:- exp)]
      [(choose? exp) (choose exp)]
      [(add-item? exp) (add-item exp)]
      [(roll-gold? exp) (roll-gold exp)]
      ;[(string->number exp) (string->number exp)]
      [(self-eval? exp) (self-eval exp)]
      [else (display "Failed\n\n" )])))

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
  (apply proc lst))

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

; Inventory functions, this is a mockup and not the final product!!!!
;TODO

(define (add-item? exp)
  (if-exp-match? exp "add-item"))

(define (add-item exp) ;Need to actually add item to inventory
  (display (string-append "Adding item  " (cadr exp) ", of quantity " (caddr exp) ".\n")))

(define (roll-gold? exp)
  (if-exp-match? exp "roll-gold"))

(define (roll-gold exp) ;Need to actually add gold to inventory
  (proclst (map (lambda (n) (evaluator n)) (map (lambda (n) (remove-white-space n)) (cdr exp))) *))


(define (self-eval? exp)
  (not (list? exp)))

(define (self-eval exp)
  (cond
    [(string->number exp) (string->number exp)]
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