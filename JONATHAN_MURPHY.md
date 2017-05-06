# Dungeon and Dragons Character Creator

## Jonathan Murphy
### April 30, 2017


# Overview
I worked together with Van Ha to create a mostly functioning Dungeons and Dragons 5e character creator.
My portion of the project consisted of creating the backend which was a metacircular evaluator and the hash tables.

We used hash tables to store all of the character information. The metacircular evaluator was used in conjunction with a domain specific language that I came up with. The reason being is that I wanted to have something file based so that it is easily expandable for other races and classes. With this method, anyone can create a new folder with a relevant CSV file that can be parsed through the evaluator. I documented part of it [here](Documentation.md).

The folders are generated into a race list and a class list which is then presented to the user on the GUI.

**Authorship note:** All of the code described here was written by myself.

# Libraries Used
External Libraries Used:
```
(require csv-reading)
(require racket/gui rsound)
```
* csv-reading was used to parse the relevant csv files.
* racket/gui was used to draw the gui.
* rsound was used to play music alongside the gui.

Internal Libraries Used:
```
(require "HashTableDefinitions.rkt")
(require "evaluator.rkt")
(include "charsheet.rkt")
```
* HashTableDefinitions.rkt has all of the relevant hash table functions, required by evaluator.rkt to modify hash table data
* evaluator.rkt has the entire evaluator, required by charsheet.rkt
* charsheet.rkt has the functions to generate the character sheet, required by main.rkt


# Key Code Excerpts
The following are code excerpts that are import to the project itself and show the ideas of the spring 2017 Organizational of Programming Languages course.

## 1. Metacircular Evaluator
```
(define (evaluator exp)
  (begin
    (cond
      [(number? exp) exp]
      [(diceroll? exp) (diceroll exp)]
      ...
      [(set-speed? exp) (set-speed exp)]
      [(and (list? exp) (eq? 1 (length exp))) (evaluator (car exp))]
      [else (begin
              (display "Failed on: " )
              (display exp)
              (display "\n\n")
              #f)])))
```
To save space, the function was cut down, the entire function can be found in evaluator.rkt. This evaluator makes up the internal language and evaluates it do whatever function I need. An example is adding an item of a specific name and quantity or marking a skill as proficient.

## 2. Using recursion for dice rolls
```
(define (diceroll roll-string)
  (define numdice (car (regexp-match #px"[0-9]+" roll-string)))
  (define dicesize (car (regexp-match #px"[0-9]+" (car (regexp-match #px"d[0-9]+"  roll-string)))))
  (roll (string->number numdice) (string->number dicesize))
  )
(define (roll numdice dicesize)
  (if (eq? numdice 0)
      0
      (+ (+ 1 (random  dicesize)) (roll (- numdice 1) dicesize))))
```
To do a diceroll, the string is first passed to diceroll which separates what the dice are and how many. That gets passed to the roll function. If the number of dice is 0, return 0. Otherwise, choose a random number between 1 and the dice size and add that to the roll function with 1 less dice.
Example: 2d8 = 1d8 + 1d8 + 0

## 3. Use of filter/map
```
(define (get-list-of-prof-full)
  (filter (lambda (n) (cddr n)) (hash->list hash-skills)))
(define (get-list-of-prof-names)
  (map car (get-list-of-prof-full)))
```
These functions demonstrate the use of map and filter for creating lists.
These functions are used to get the list of all skills a character is proficient in. get-list-of-prof-full takes the hash table hash-skills, converts it to a list, and filters it by the last element in each item which will either be #t or #f. get-list-of-prof-names takes the list provided by get-list-of-prof-full and uses map to only return the name of each skill.

I also use map in another instance.
```
(define (get-class-list)
  (define location (string-append folder-base "/class"))
  (map (lambda (f) (cons (string-titlecase (path-name-to-file-name (path->string f))) f)) (directory-list location #:build? #t))
  )
(define (get-race-list)
  (define location (string-append folder-base "/race"))
  (map (lambda (f) (cons (remove-csv (path-name-to-file-name (path->string f))) f)) (directory-list location #:build? #t))
  )
```
get-class-list and get-race-list both search through their respective folders (DND/race and DND/class) to generate a list of classes and races with each item being a cons. The first element is a string which is the name of the class/race. The second item is the path folder to the relevant data.

## 4. Domain Specific Language
Last but not least is about the domain specific language that I implemented. This part I'm not quite sure what to talk about, but as I mentioned before I wanted to use CSV files. So the structure of the language is as follows:
```
function, arg1, arg2, .... , argn
```
Which would be translated as a list and passed to the evaluator which would evaluate the relevant function.

A few examples:
```
inc-stat, dexterity, 2
inc-stat, intelligence, 1
```
The evaluator would take that, find the relevant stat and increase it by the last number. This function has 3 arguments, including itself.

```
add-language, common
add-language, elvish
```
The evalautor would take that and note that the character can speak, read and write common and elvish. This function has 2 arguments, including itself.
