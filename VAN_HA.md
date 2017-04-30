# GUI to D & D 5e Character Generator

## Van Ha

### April 30, 2017

# Overview

This set of code creates a GUI to interface with the backend of the Dungeons and Dragons 5e
character generator. The GUI allows one to select race, class, alignment. input a character
name and player name, as well as adjust the base statistics and reroll the character if one
wanted to. It also plays a different theme song for each race when it is selected and allows
for the music to be toggled on and off. The GUI allows access to the information which are stored in
various hash tables.

** Authorship note ** All of the code described herein was written by myself except where mentioned.


# Libraries Used

(racket/gui)
(rsound)

* racket/gui provides the interface
* rsound provides the ability to play music

# Key Code Concepts
