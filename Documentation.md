#D&D Character Creator Evaluator Documentation
##File Formatting
###Races
#####Folder: DND/race
race-name

    ├── data.csv
    └── info.txt

race-name: Folder is the name of the race ex: Human, Elf, Dwarf etc.
data.csv: Csv formatted file that applies to the character, formatting in "Evaluator Functions"
info.txt: Text file containing basic description shown in the GUI


###Backgrounds
#####Folder: DND/background
background-name

    ├── data.csv
    └── info.txt

background-name: Folder is the name of the background ex: Outlander, Sage etc.
data.csv: Csv formatted file that applies to the character, formatting in "Evaluator Functions"
info.txt: Text file containing basic description shown in the GUI


###Classes
####Folder: DND/class
class-name

    ├── info.txt
    ├── level1.csv
    ├── level2.csv
    ├── level3.csv
    ├── level4.csv
    ├── level5.csv
    ├── level6.csv
    ├── level7.csv
    ├── level8.csv
    ├── level9.csv
    ├── level10.csv
    ├── level11.csv
    ├── level12.csv
    ├── level13.csv
    ├── level14.csv
    ├── level15.csv
    ├── level16.csv
    ├── level17.csv
    ├── level18.csv
    ├── level19.csv
    └── level20.csv

class-name: Folder is the name of the class ex: Barbarian, Fighter, Wizard etc.
level[1-20].csv: Csv formatted file that applies to the character, formatting in "Evaluator Functions"
info.txt: Text file containing basic description shown in the GUI

##Evaluator Functions
function name, other function name (arguments if any) : Description
###HP Evaluator Functions
* setHP, set-HP, 1st-lvl-hp (list): Evaluates the rest of the string and sets the hp to that value
	* Ex: 1st-lvl-hp, add:+ 12 constitution-mod
		* Sets hp to 12 + the constitution mod
* inc-health (list): Increases hp by adding all parts together
	* Ex: inc-health, hit-dice constitution-mod
		* Sets hp to the current hp + rolling the hit-dice + the constitution mod
* set-hit-dice (string): Sets the value of the hit dice to the given string
	* Ex: set-hit-dice, 1d12
* set-ac (string): Stores the string as an evaluating string, evaluates the value and stores the armor class
    * Ex: set-ac, add:+ 10 dexterity-mod constitution-mod


###Skill Evaluator Functions
* set-proficiency-bonus (number): Sets the value of the proficiency bonus to the given number
	* Ex: set-proficiency-bonus, 2
		* Sets the proficiency bonus to 2
* skill-choice (number, list): First value is a number, the rest are a list of skills, it will choose that many random skills that aren't already proficient to be marked proficient
	* Ex: skill-choices, 2, Animal Handling, Athletics, intimidation, Nature, Perception, Survival
		* Will choose 2 skills that aren't already proficient and mark them proficient


###Inventory Evaluator Functions
* add-item (string, number): Given an item name and a number, will add that item to the inventory with the correct quantity. If the item is already there, the qualtity will be added
	* Ex: add-item, Hunting Trap, 1
		* Adds 1 Hunting Trap to the inventory
* add-weapon (string, number): Similar to add-item, given an weapon name and a number, will add that weapon to the weapon list with the correct quantity. If the weapon is already there, the qualtity will be added
	* Ex: add-weapon, greataxe, 1
		* Adds 1 greataxe to weapon list
* roll-gold (list): Evaluates the rest of the list, multiplies it together and adds that much gold to the inventory
	* Ex: roll-gold, 2d4, 10
		* Adds 2d4 * 10 gold to the inventory


###Note, Proficiencies, Skills Functions
* add-profiencies (string): adds the argument to the list of proficiencies
	* Ex: add-profiencies, light-armor
		* Notes that the character is proficient in light armor
* set-skill-proficient, add-skill (string): takes in a skill name as an argument and marks it as proficient
    *  Ex: add-skill, athletics
*  set-saving-throw (string): takes in a stat name as an argument and marks it as a saving throw
    *  Ex: set-saving-throw, strength
*  add-ability (String, Number, String): Takes in a skill name as the first argument, a number (to be evaluated) as a second argument and a string that states the number. Either uses between rests, spells known etc.
    *  add-ability, Rage, 2, uses
        *  The ability Rage can be used twice between long rests
    *  add-ability, Spells, add:+ wisdom-mod level, total prepared spells
*  add-note (String): Takes the string and stores it as a relevent note
    *  add-note, Has Darkvision



###Other Evaluator Functions
* add:+ (list): Will add evaluate everything in the list and add them together
	* Ex: add:+, 12, constitution-mod
		* Will add 12 and constitution mod
* sub:- (list): Will subtract everything from left to right
	* Simliar to above
* choose (list): Given a list of functions, choose one to evaluate, useful for "choose x or y item"
	* Ex: choose, add-weapon greataxe 1, add-weapon any*-martial-weapon 1
		* Will either add 1 greataxe or 'any martial weapon'


###Self-Evaluating Evaluator Functions
* Numbers: return number
* Dicerolls (string): evaluates the random dice roll as xdy
	* Random number between 1 and y gets evaluated x times and added together
    * Ex: 1d6, random number [1-6]
    * Ex: 2d8, random number [1-8] + random number [1-8]
* "getHP" returns the current hp
* "hit-dice" returns the evaluated hit-dice
	* Ex: if the hit-dice is set to 1d12, returns diceroll of 1d12
* *-mod
	* Where * is a stat (strength, dexterity, constitution, wisdom, intelligence, charisma) it will return the stat modifier
* "level" return the character level
