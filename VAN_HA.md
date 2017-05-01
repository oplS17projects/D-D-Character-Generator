# GUI to D & D 5e Character Generator

## Van Ha

### April 30, 2017

# Overview

I worked with Jonathan Murphy on this. He wrote the backend which consisted of his evaluator and the hash table. I wrote the frontend GUI and some additional functions in the hash table.

This set of code creates a GUI to interface with the backend of the Dungeons and Dragons 5e
character generator. The GUI allows one to select race, class, alignment. input a character
name and player name, as well as adjust the base statistics and reroll the character if one
wanted to. It also plays a different theme song for each race when it is selected and allows
for the music to be toggled on and off. The GUI allows access to the information which are stored in
various hash tables.

I found it interesting to create something with a GUI. Everything I've written before was all based on
around interfacing through the command line. In addition, it allowed me to bridge an unfamiliar library, ```racket/gui``` into something I am more familiar with, hash tables.

**Authorship note:** All of the code described herein was written by myself except where mentioned otherwise.


# Libraries Used
### External Libraries:
```
(racket/gui)
(rsound)
```
* racket/gui provides the interface through which to interact with the character generator
* rsound provides the ability to play music

### Internal Libraries:
```
(require "evaluator.rkt")
(require "charsheet.rkt")
(require "HashTableDefinitions.rkt")
```
* evaluator.rkt provided the metacircular evaluator
* charsheet.rkt provided the frame to draw the character sheet by main.rkt
* HashTableDefinitions.rkt provided the hash table definitions and procedures

# Key Code Concepts

## 1. Closure to Create Local State Variable

```
(define (play-theme choice)
  (let ((song (rs-read (string-append (path->string (cdr choice)) "/song.wav"))) (current-theme main-theme))
        (unless (equal? song current-theme)
          (when music-status (begin (stop) (set! current-theme song) (play current-theme))))))
```
The procedure ```play-theme```, when first called, returns a procedure that is a closure created by the ```let```. Since the call to ```play-theme``` is not assigned to a variable, each procedural call is using the same closure. This procedure sets the local state variable ```current-theme``` to ```main-theme```.
At each successive call to this procedure, the ```song``` which is stored on the local storage drive and whose
path is derived through taking the information stored in the ```cdr``` of the ```choice``` object passed as an argument
and appending it to a ```path->string``` conversion is compared to the object in ```current-theme```. Through closure, the variable ```current-theme``` is lexically bound to the procedure returned by ```play-theme```, can only be seen within that procedure, and ```current-theme``` is able to remember what value it currently holds each time ```play-theme``` is called. If different, ```play-theme``` assigns ```song``` to the ```current-theme``` through modification by using ```set!```.


## 2. Recursion, Map, and Let to Print Lists of Strings

```
(unless (hash-empty? hash-notes)
                  (letrec ((notes (map string-split (map cdr (hash->list hash-notes))))
                           (line-length 25)
                           (x-coord 545)
                           (y-coord 502)
                           (dy 15)
                           (print-string (λ (lst str x y delta)
                                 (cond ((equal? lst '()) (begin (send dc draw-text str x y) (send dc draw-text "" x (add-delta y delta)) (add-delta y (* 2 delta))))
                                       (else (let ((z (string-append str " " (car lst))))
                                               (if (> (string-length z) line-length) (begin (send dc draw-text str x y) (print-string lst "" x (+ y delta) delta))
                                                   (print-string (cdr lst) z x y delta)))))))
                           (print-notes (λ (lst str x y delta)
                                 (unless (equal? lst '())
                                   (print-notes (cdr lst) str x (print-string (car lst) str x y delta) delta)))))
                    (print-notes notes "" x-coord y-coord dy)))
```
This particular piece of code was used to print a list of strings in the hash table ```hash-notes```. It used two recursive
functions, ```print-notes``` and ```print-strings```. The issue was that the ```canvas``` object of ```racket/gui``` draws strings on one line and does not recognize newlines or any type of carriage returns. Thus the ```draw-text``` function of ```canvas``` would draw a string off the canvas if it was too long, which in this case, it did. Therefore the strings needed
to be split up which is what the ```(letrec ((notes (map string-split (map cdr (hash->list hash-notes))))``` code does. The hash table is turned into a list, which is sent as an argument to ```map cdr``` which creates the list of strings which in turn is sent to ```map string-split``` to finally create a list of lists of strings which is assigned to ```notes``` by ```letrec```. ```letrec``` also assigns the various local variables which are needed to do the recursion such as ```line-length```. The recursive procedure ```print-strings``` prints the inner list of strings by recursively concatenating the ```str``` string object with the ```car``` of the list and only printing out those strings under the value of ```line-length``` and then recursively doing the same to the rest of the list. ```print-notes``` recursively prints all the list of strings in the list ```notes```. It accepts the ```print-strings``` as one of its arguments, the one representing the y-coordinate, since at the end of ```print-strings``` returns the y-coordinate for ```print-notes``` to start printing the new string. Thus, this creates a nested recursive procedure.

## 3. Procedural Abstraction

```
(define (update-hp op)
  (let* ((old-con-mod (getmod "constitution")) (new-con-mod (calc-mod "constitution" op 1))
                                               (diff (- new-con-mod old-con-mod))
                                               (new-base-hp (+ base-hp new-con-mod)))
    (unless (or (eqv? diff 0) (<= new-base-hp 0) (<= points-to-allocate 0) (<= (get-stat-num "constitution") 0)) (set-hash-base "hp" new-base-hp)
                                )))
                                
(define (calc-mod str op num)
  (floor (/ (- (op (car (getstat str)) num) 10) 2))
  )
  ```
  
These procedures are an example of procedural abstraction. ```calc-mod``` has three paramters: ```str``` is the statistic whose modifier should be calculated, ```op``` is the operator, and ```num``` is a number on which the operator will be used. This procedure returns the value of the modifier as calculated by this implementation. If future editions of Dungeons & Dragons changes how modifiers should be calculated, this procedure can be changed without the user's knowing about it.

```update-hp``` has one parameter: ```op``` which is an operator. It uses ```calc-mod``` to calculate one of its ```let*``` variables. As with ```calc-mod```, if the implementation of ```update-hp``` needs to be changed, it could be done so without the user's knowing about the internal operations. Just by changing ```calc-mod```, both procedures would be changed.
