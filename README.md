# TuringMachineInRacket
Graphical Deterministic Turing Machine simulator in Racket

The program is a graphical simulator and editor of Deterministic Turing Machines. It is written entirely in Racket by myself, John Brown.

## Forward
The purpose of writting this program was entirely to appease the professor of my Foundations of Computer Science class in hopes of extra credit.
The reason I chose to write a Turing Machine simulator, starts with the fact that I believe I am a strong programmer, however when it
comes to the theoretical topics discussed in my Foundations class, my comprehension is less than satisfactory. In an effort to teach myself more about
Turing Machines, and machines in general, I wrote this application to both understand the back end of the machines, and to test new ones
I design quickly for correctness. Why Racket? I like it, my professor seems to adore it, win/win. I get to learn, he'll be able to understand it easily.

I am certain that something like this may already exist, however, my intentions are not to create something entirely original, just to prove some level of comprehension.

## Running
Unzip the entirety of the release to a directory.
Move code.states to the desktop.
Open Master.rkt in DrRacket.
Run. (All controls are described in Master.rkt)
     By default, Master.rkt includes loading "code.states" from the user desktop
     It then opens up the screen for editing the state machine.
     Once this is closed, it will open the simulator.
     
## Notes
* The Master file contains extensive notes on the use and operation of this application.
* This program is not capable of comprehending Multi-tape Turing Machines.
* This program is not capable of comprehending Non-deterministic Turing Machines
* The tapes are infinite on both ends. The underscore character "_" is added to either end as needed.
* Anytime there is a no appropriate transition from the current state to any state with the current input, the machine will Halt.
* The code within all the files in this release was written in entirety by John Brown.

## Used Libraries
[2htdp/image](https://docs.racket-lang.org/teachpack/2htdpimage.html "2htdp Image") \ [2htdp/universe](https://docs.racket-lang.org/teachpack/2htdpuniverse.html "2htdp Universe") - Racket libraries used to draw the GUI.
