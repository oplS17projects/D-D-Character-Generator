# D&D Character Creator

### Statement
The idea of this project is to be able to generate a random Dungeon and Dragons 5th edition character. We're doing this with various csv files that are parsed through an evaluator to generate the character.

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
The following functions need to be checked and implimented:
~~~
add:+                        ; Standard add the list
mul:*                        ; Multiply the list
1st-lvl-hp                   ; Set the hp at first level
set-hit-dice                 ; Sets the hit dice used for level up, ex: 1d8
level-up                     ; Applies level up stuff
inc-health                   ; Increase health
roll                         ; Dice roll, already done, see above
add-profiencies              ; Proficiency list notes?
set-saving-throw             ; Stat should have a is-saving-throw element, set to true
add-skill                    ; Sets skill to proficient
add-skill-choice             ; (?) Save choices in list for future use?
inc-skill-choices            ; Var starts at 0, increases by number
choose-choices               ; Eval choices or print out choices?
add-ability                  ; Set note of what the skill is and total uses
set-ac                       ; Store the string to eval armor class
roll-gold                    ; Evaluate the string and store the gold in inventory
choose                       ; Given a list of options, choose a random one
add-item                     ; Add item and quantity to inventory
add-language-choice          ; See add-skill-choice
inc-language-count           ; See inc-skill-choices
add-note                     ; Adds generic note to character
inc-stat                     ; Increases given stat by number, example (inc-stat "strength" 1)
set-speed                    ; Sets walking speed
set-sc                       ; Set subclass
if-sc                        ; If character has subclass, do something
~~~

### Data Sets or other Source Materials
Any and all csv files will be written by us from either the [Dungeon and Dragon's 5e Player's Handbook](http://a.co/hDUb8oH) or other homebrew material.
--Note someone scanned the book online [here][online-pdf]

An example of a Barbarian class would be something like the following, commented after with ; is an explication
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
The end product will be some sort of GUI to display the generated character.

### Evaluation of Results
We'll know it works if a complete character is shown.

## Architecture Diagram
Upload the architecture diagram you made for your slide presentation to your repository, and include it in-line here.

Create several paragraphs of narrative to explain the pieces and how they interoperate.

## Schedule
Explain how you will go from proposal to finished product.

There are three deliverable milestones to explicitly define, below.

The nature of deliverables depend on your project, but may include things like processed data ready for import, core algorithms implemented, interface design prototyped, etc.

You will be expected to turn in code, documentation, and data (as appropriate) at each of these stages.

Write concrete steps for your schedule to move from concept to working system.

### First Milestone (Sun Apr 9)
We will have some functions of the evaluator working, be able to read from the necessary files and some basic functions of a GUI.

### Second Milestone (Sun Apr 16)
We'll have the evaluator working and a fully functional GUI.

### Public Presentation (Mon Apr 24, Wed Apr 26, or Fri Apr 28 [your date to be determined later])
We'll have multiple races, background and classes.

## Group Responsibilities

### Van Ha @VanHaUML
I will be working on the GUI and the functions to extract information from the hash table that stores the data and display them in the GUI.

### Jonathan Murphy @MurphyWants
The evaluator, reading from files and storing data in the hash table.


[online-pdf]: https://dnd.rem.uz/5e%20D%26D%20Books/D%26D%205e%20-%20Players%20Handbook%20(Small).pdf