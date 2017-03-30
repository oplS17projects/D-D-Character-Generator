#lang racket
(require csv-reading)
; Csv viewing functions
; Copied from csv article
; docs.racket-lang.org/csv-reading/index.html
(define make-food-csv-reader
  (make-csv-reader-maker
   '((separator-chars            #\|)
     (strip-leading-whitespace?  . #t)
     (strip-trailing-whitespace? . #t))))
     
(define (next-row path)
  (make-food-csv-reader (open-input-file path)))


; Static Vars
(define nil '())
(define folder-base "DND")

; Character Vars
(define CharClassName nil)
(define CharSubClassName nil)
(define CharStatList nil)
(define CharSkillList nil)
(define Hitdice nil)
(define CharInventory nil)
(define CharNotes nil)

; Relevant Structs
(define-struct Stat-Struct (Num IsSavingThrow) #:transparent)
(define-struct Skill-Struct (IsProficient ParentSkillName) #:transparent)

; Generate Stats List
(define (generatestats)
  (define stats-file (string-append folder-base "/stats.csv"))
  (define lines (next-row stats-file))
  (define roll-string (car (lines)))
  (define list-stats (car (csv->list (car (lines)))))
  (build-list (length list-stats)
              (lambda (n)
                (cons (list-ref list-stats n)
                      (make-Stat-Struct (evaluator (list roll-string)) #f))))
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
  (cond
    [(diceroll? (car exp)) (diceroll (car exp))]
    [else #f]))

; Diceroll?
(define (diceroll? dr)
  (if (string? dr)
      (regexp-match? #px"[0-9]+d[0-9]+" dr)
      #f))

(define (diceroll roll-string)
  (define numdice (car (regexp-match #px"[0-9]+" roll-string)))
  (define dicesize (car (regexp-match #px"[0-9]+" (car (regexp-match #px"d[0-9]+" roll-string)))))
  (roll (string->number numdice) (string->number dicesize))
  )

(define (roll numdice dicesize)
  (if (eq? numdice 0)
      0
      (+ (+ 1 (random  dicesize)) (roll (- numdice 1) dicesize))))

; File Functions
; Get List of Files