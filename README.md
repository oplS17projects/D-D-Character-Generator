# D&D Character Creator

### Statement
The idea of this project is to be able to generate a random Dungeon and Dragons 5th edition character. We have done this with various csv files that are parsed through an evaluator to generate the character.

### Analysis
Map will be used extensively throughout this project to iterate through csv files (csv->map) and to iterate through lists.

Recursion is used in the roll function.
The roll "2d6" means rolling 2 6 sided dice. We also used a regex to parse a string like "2d6" to pass it to the roll function. Example (diceroll "2d6") evaluates to (roll 2 6). The following is the code for the function.
~~~
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
~~~

Each expression will be evaluated through a metacircular evaluator similiar to what we did in class.
Each function is detailed in [this documentation file.]("documentation.md")

### Data Sets or other Source Materials
Any and all csv files to generate the race or class will be written by us from either the [Dungeon and Dragon's 5e Player's Handbook](http://a.co/hDUb8oH) or other homebrew material.
--Note someone scanned the book online [here][online-pdf]

An example of a Barbarian class is something like the following, commented after with ; is an explication
~~~
1st-lvl-hp, "add:+, 12, cons-mod"          ; Evaluate the following at level 1
set-hit-dice, 1d12                         ; Setting the hit dice character variable
add-profiencies, "light-armor"             ; Barbarians are proficient in light armor, make a note
add-profiencies, "medium armor"            ; Barbarians are proficient in medium armor, make a note
add-profiencies, "shields"                 ; Barbarians are proficient in shields, make a note
add-profiencies, "simple-weapons"          ; Barbarians are proficient in simplec weapons, make a note
add-profiencies, "maritial weapons"        ; Barbarians are proficient in maritial wepaons, make a note
set-saving-throw, strength                 ; Strength should have a is-saving-throw element, set to true
set-saving-throw, constitution             ; Constitution should have a is-saving-throw element, set to true
add-skill-choice, "Animal Handling"        ; Player is given a choice of skills they could be proficient in
add-skill-choice, "Athletics"              ; For now, note the list and print it out later, if we have time
add-skill-choice, "Intimidation"           ;	implement a skill chooser.
add-skill-choice, "Nature"
add-skill-choice, "Perception"
add-skill-choice, "Survival"
inc-skill-choice, 2                        ; Out of those, choose 2
add-ability, Rage, 2                       ; Note the ability Rage, at level 1 you have 2 uses
set-ac, "add:+, 10, dex-mod, cons-mod"     ; Armor class is usually defined by armor, Barbarian defines it otherwise
roll-gold, "mul:*, 2d4, 10"                ; Each class has its own amount of gold the player starts with
choose, "add-item, greataxe, 1", "add-item, any-martial-weapon, 1"    ; Need to choose one or the other, random selection
choose, "add-item, handaxe, 2", "add-item, any-simple-weapon, 1"
add-item, explorer's pack, 1               ; Add the following items with coresponding quantity
add-item, javelin, 4
~~~

### Deliverable and Demonstration
The end product is a gui that allows the person to choose a race, a class, an alignment, the ability to reroll stats and a function that generates the character sheet with details printed on it.

### Evaluation of Results
We can fully generate a character, except for background.

## Architecture Diagram
![Diagram image](diagramafter.png?raw=true "Architecture Diagram")

The project starts with a variety of csv files. This files are a list of races and classes. Each of these has a function (get-class-list, get-race-list) that returns a list of cons, where the first argument is a readable name and the second element is a path to the correct file or folder.

This list gets passed to the evaluator. Each race and class is chosen or chosen randomly. After choosing one of each, each race csv file gets evaluated through the evaluator. Then the class correct level#.csv file gets evaluated. Then the background.csv file gets evaluated.

Throughout the evaluation, all values are stored in a hash table. The data is accessible through the Gui.

## Schedule

### First Milestone (Sun Apr 9)
For the first milestone, we have a fully functional GUI that allows selection of different races and classes. The evaluator has most functions working.

### Second Milestone (Sun Apr 16)
The backend has been mostly completed (including hash table). There are a few evaluator functions that need to be implemented. More features of the gui are done, including background music. There is some overall polishing that needs to be done.

### Public Presentation (Wed Apr 26)
We have a functional character generator. We left out backgrounds because that is more of a roll playing thing that the player can choose. Also there are some place holder names such as "any*-simple-weapon" because we would have to implement a dictionary of sorts that would take time.

## Group Responsibilities

### Van Ha @VanHaUML
Van Ha worked on the GUI to show and generate all of the information.

### Jonathan Murphy @MurphyWants
Jonathan Murphy worked on the back end and the evaluator.


[online-pdf]: https://dnd.rem.uz/5e%20D%26D%20Books/D%26D%205e%20-%20Players%20Handbook%20(Small).pdf
