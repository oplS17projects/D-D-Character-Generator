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

I found it interesting to create something with a GUI. Everything I've written before was all based on
around interfacing through the command line. 

### Authorship note ### All of the code described herein was written by myself except where mentioned.


# Libraries Used
```
(racket/gui)
(rsound)
```

* racket/gui provides the interface through which to interact with the character generator
* rsound provides the ability to play music

# Key Code Concepts

## 1. Closure to Create Local State Variable

```
(define (play-theme choice)
  (let ((song (rs-read (string-append (path->string (cdr choice)) "/song.wav"))) (current-theme main-theme))
        (unless (equal? song current-theme)
          (when music-status (begin (stop) (set! current-theme song) (play current-theme))))))
```
The procedure ```play-theme```, when first called, sets the local state variable ```current-theme``` to ```main-theme```.
At each successive call to this procedure, the ```song``` which is stored on the local storage drive and whose
path is derived through taking the information stored in the ```cdr``` of the ```choice``` object passed as an argument
and appending it to a ```path->string``` conversion is compared to the object in ```current-theme```. If different, ```play-theme``` assigns ```song``` to the ```current-theme```. Through closure, the variable ```current-theme``` is lexically bound to the ```play-theme``` procedure and ```current-theme``` is able to remember what value it currently holds.
