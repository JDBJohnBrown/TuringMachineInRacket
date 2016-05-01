#lang racket
;-----------------------------------------------------------------------
; -- Graphical Deterministic Turing Machine simulator in Racket --
; Author: John@JdbJohnBrown.com
; Copyright John Brown ~ May 1, 2016
; This is the control code for my dTM simulator.
; Everything for the user is executed here.
; To run, use the commands (edit-states) or (watch-states)
(require racket/file)
(require "StateCode.rkt")
(require "DrawCode.rkt")

;---------------------------------------------------------------------------------------------------------------------------------------
;(watch-states)                                     ;watch the state machine step by step
;(edit-states)                                      ;edit the state machine
;(set-tape "tape-string" tape-position start-state) ;change the original input/tape posisition/the starting state
;(save)                                             ;save the state machine to the file at "path" from change-path
;(load)                                             ;load the state machine from the file at "path" from change-path
;(change-path "path")                               ;changes where you want to save/load files from.
;(set-screen-size width height)                     ;changes how big the edit&watch windows are. (Default 1200*1000)
;(set-transition-font-size size)                    ;changes the size of the text for transitions (Default 10)
;(set-interface-font-size size)                     ;changes the size of the text for the interface (Default 12)
;---------------------------------------------------------------------------------------------------------------------------------------

;--------SAMPLE STATEMENTS-----------------------------------

;sets the save/load path to the desktop
(change-path (string-append (path->string(find-system-path 'desk-dir)) "\\code.states"))
;loads the file names above as the state machine
(load)
;calls the graphic state-machine editor
(edit-states)
;changes the input for the single-tape machine, makes the machine start at position , at state 0
(set-tape "01100110" 1 0)
;opens the state-machine simulator
(watch-states)
;--------SAMPLE STATEMENTS-----------------------------------



;---------------------------------------------------------------------------------------------------------------------------------------
;watch-states controls
     ;Green-Arrow: Do one step in the state machine.
     ;Blue-Arrow: Do all steps until you reach the next pause, or until the machine halts
     ;Red-X: Reset the machine. Sets the tape to the input/state/tape-pos that existed when you ran (watch-states)

     ;Click a state to make it the pause state.
     ;     When the blue-arrow reaches this state, it will pause.
     ;Re-click it, or select a new state to remove the pause.

     ;Current state is shown as yellow.
     ;Current tape and position is shown under controls.
;---------------------------------------------------------------------------------------------------------------------------------------
;edit-states controls
     ;Mouse:
     ;     Click any existing state to make it the "edit-state". The "edit-state" appears yellow.
     ;     Click and drag any state to move it.
     ;Keyboard:
     ;     B - toggles whether a state is accpeting or not. If the TM halts at this location, it will print Done: #t
     ;     A - add state. Adds a new state, move the mouse to choose its location, click to place.
     ;     T - add transition. Adds a new transtion from the "edit-state" to whatever state you click next.
     ;         New Transition: prompts the user to select the input/output/tape direction for the new transition
     ;               Use the keyboard to choice any character as the input and output.
     ;               Use the arrows for Left/Right/Stay or l,r,s for the tape direction
     ;               Enter to confirm selection. Escape to abandon adding a new transition
     ;     R - move self-state text. Move the mouse to choose a location for self-transitions.
     ;          Self-transitions are in ex. q1->q1 transitions. You may move the location of the image for self transitions relative the the state
     ;     right/left keys- Change which transition is selected.
     ;          Selected transitions appear red. Can only select transitions that start at the "edit-state"
     ;     Backspace - If there is a transition selected, it will be deleted.
     ;                 If no transition is selected, the "edit-state" will be deleted.



;Additional notes!
;     The # character should not be used in transitions.
;     It will work, and will behave appropriately, however, upon saving it will be changed to a tilde (~),
;                    which may cause conflicts if you use both characters.
;     The # negatively impacts the loading process, and this change was added to prevent breaks.