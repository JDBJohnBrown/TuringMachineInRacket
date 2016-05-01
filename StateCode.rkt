#lang racket
;-----------------------------------------------------------------------
; -- Graphical Deterministic Turing Machine simulator in Racket --
; Author: John@JdbJohnBrown.com
; Copyright John Brown ~ May 1, 2016
; All the code wihin this file deals with accessing/editing the machine states
;-----------------------------------------------------------------------
(require racket/file)
(require "Tools.rkt")
(provide tr-in-val)
(provide tr-write-val)
(provide tr-dir-val)
(provide tr-state-val)
(provide st-num)
(provide st-accept)
(provide st-trans)
(provide st-xpos)
(provide st-ypos)
(provide st-selfx)
(provide st-selfy)
(provide get-state)
(provide states)
(provide edit-state)
(provide edit-x-y)
(provide edit-self-x-y)
(provide delete-state)
(provide add-state)
(provide delete-trans)
(provide add-trans)
(provide save)
(provide load)
(provide file-path)
(provide change-path)
(provide set-tape)
(provide one-step)
(provide all-steps)
(provide track)
(provide trackpos)
;functions to pull information out of a transition list
(define (tr-in-val x) (car x))
(define (tr-write-val x) (cadr x))
(define (tr-dir-val x) (caddr x))
(define (tr-state-val x) (cadddr x))

;functions to pull information out of a state list
(define (st-num x)(car x))
(define (st-accept x)(cadddr x))
(define (st-trans x)(car(cddddr x)))
(define (st-xpos x)(caadr x))
(define (st-ypos x)(cadadr x))
(define (st-selfx x)(car(caddr x)))
(define (st-selfy x)(car(cdaddr x)))

;default state program. Loads when no file is found.
; (state #, (x y coords) (coords of self transtions) accepting? (transitions))
(define states
 (list       
       (list 0 '(45 45) '(0 20)  #f (list '("0" "0" "R" 0) '("1" "1"  "R" 0) '("_" "#" "L" 1)))
       (list 1 '(45 120) '(0 20) #f (list '("1" "1" "L" 1) '("0" "0" "L" 1) '("_" "_" "R" 2))) 
       (list 2 '(120 45) '(0 20) #f (list
                              '("0" "X" "R" 3) '("1" "Y" "R" 5) '("#" "#" "L" 6)
                              ))
       (list 3 '(120 120) '(0 20) #f (list
                               '("0" "0" "R" 3) '("1" "1" "R" 3) '("#" "#" "R" 3) '("_" "0" "L" 4)
                               ))
       (list 4 '(160 120) '(0 20) #f (list
                               '("0" "0" "L" 4) '("1" "1" "L" 4) '("#" "#" "L" 4) '("X" "X" "R" 2) '("Y" "Y" "R" 2)
                               ))
       (list 5 '(160 160) '(0 20) #f (list
                               '("0" "0" "R" 5) '("1" "1" "R" 5) '("#" "#" "R" 5) '("_" "1" "L" 4)
                              ))
       (list 6 '(160 160) '(0 20) #f (list
                               '("X" "0" "L" 6) '("Y" "1" "L" 6) '("_" "_" "R" 7)
                              ))
       (list 7 '(180 160) '(0 20) #f (list
                              '("X" "B" "R" 7) '("0" "B" "R" 8) '("1" "B" "R" 10) '("B" "B" "R" 11) '("#" "#" "S" 11)
                              ))
        (list 8 '(160 120) '(0 20) #f (list
                               '("0" "0" "R" 8) '("X" "X" "R" 8) '("1" "X" "L" 9) 
                               ))
        (list 9 '(160 120) '(0 20) #f (list
                               '("0" "0" "L" 9) '("X" "X" "L" 9) '("1" "1" "L" 9) '("B" "B" "R" 7) 
                               ))
        (list 10 '(160 120) '(0 20) #f (list
                               '("1" "1" "R" 10) '("X" "X" "R" 10) '("0" "X" "L" 9) 
                               ))
        (list 11 '(10 10) '(0 20) #f (list
                              '("B" "B" "R" 11) '("#" "#" "R" 12)
                              ))
        (list 12 '(10 10) '(0 20) #t (list
                              '("B" "B" "R" 11) '("#" "#" "R" 12)
                              ))
    )
  )

;iteratively goes through each state until the state # matches x
;returns state object whose state # = x
(define (get-state x)
  (define (num n lst)
    (if (= n 0) (car lst) (num (- n 1) (cdr lst))))
  (if (number? x) (num x states)  "failed")
  )

;does one step in the state machine
(define (do-step params)
  (define str (car params))
  (define pos (cadr params))
  (define state (caddr params))
  (define (pathloop in lst)
    (if (null? lst) 1
        (if (equal? in (tr-in-val (car lst)))
            (car lst)
            (pathloop in (cdr lst)))))                    
  
  (define next-step (pathloop (substring str (- pos 1) pos) (st-trans (get-state state))))
  
  (if (number? next-step) (list str -1 state)
      (cond
        [(equal? "R" (tr-dir-val next-step))         
          (list (string-append (substring str 0 (- pos 1)) (tr-write-val next-step) (substring str pos (string-length str)))
                (+ pos 1)
                (tr-state-val next-step))]
        [(equal? "L" (tr-dir-val next-step))
          (list (string-append (substring str 0 (- pos 1)) (tr-write-val next-step) (substring str pos (string-length str)))
                (- pos 1)
                (tr-state-val next-step))]
        [(equal? "S" (tr-dir-val next-step))
          (list (string-append (substring str 0 (- pos 1)) (tr-write-val next-step) (substring str pos (string-length str)))
                pos
                (tr-state-val next-step))]))          
  )

;adjusts the output of do-step
;kept outside so I could do adjustments as needed.
(define (adj-do-step params)   
  (define str (car params))
  (define pos (cadr params))
  (define state (caddr params))
  (cond
    [(eqv? 0 pos)
     (list (string-append "_" str) 1 state)]
    [(> pos (string-length str))
     (list (string-append str "_") pos state)]
    [else params])
  )

;replaces state # x with "new"
(define (edit-state x new)
  (set! states (map (lambda (st) (if (= (st-num st) x) new st)) states)))

;calls edit-state, and creates the "new" input using existing values of n, and coords x,y
(define (edit-x-y n x y)
  (define s (get-state n))
  (list (st-num s) (list x y) (list (st-selfx s) (st-selfy s))(st-accept s) (st-trans s)))

;same as above but for the self transtions relative location
(define (edit-self-x-y n x y)
  (define s (get-state n))
  (list (st-num s)(list (st-xpos s) (st-ypos s)) (list x y) (st-accept s) (st-trans s)))

;default input for running the state machine.
;tape, tape-position (minimum 1), state
(define track (list "01100110" 1 0))

;does one step in the state machine.
;displays output with each run
(define (one-step)
  (set! track (adj-do-step (do-step track)))
  (displayln track)
  (if (= (cadr track) -1) (begin (displayln (string-append "Done: " (format "~a" (st-accept (get-state (caddr track)))))) #f)
  (if (null? (st-trans (get-state (caddr track))))
       (begin (displayln (string-append "Done: " (format "~a" (st-accept (get-state (caddr track)))))) #f)
       (begin (displayln (st-accept (get-state (caddr track)))) #t) ))
  )

;fires one-step until it says it cannot complete another step
(define (all-steps)  
    (if (one-step) (all-steps) '()))






;deletes state with the number sn
;deletes all transitions from all states to sn
;decrements the state numbers of all states with a # higher than sn
(define (delete-state sn)  
  (set! states (remove (get-state sn) states))
  (set! states (map (lambda (x)
                      (define num (st-num x))
                      (if (> (st-num x) sn) (set! num (- num 1)) "") 
                      (list num (list (st-xpos x) (st-ypos x)) (list (st-selfx x) (st-selfy x)) (st-accept x)
                                      (map (lambda (x)
                                             (define num (tr-state-val x))
                                             (if (> (tr-state-val x) sn) (set! num (- num 1) )"")
                                             (list (tr-in-val x) (tr-write-val x) (tr-dir-val x) num) )
                                             (filter  (lambda (x) (not (= sn (tr-state-val x)))) (st-trans x))))
                      )
                       states))
  )
;adds a new state with the next highest state-number 
(define (add-state)
  (set! states (append states (list (list (+ (st-num (list-last states)) 1) '(0 0) '(0 20) #f '()))))
  )

;deletes a transition from state w/ # sn. tr is passed in as a transtion object 
(define (delete-trans sn tr)
  (define s (get-state sn))  
  (edit-state sn (list sn (list (st-xpos s)(st-ypos s)) (list (st-selfx s)(st-selfy s)) (st-accept s) (remove tr (st-trans s))))
  )

;adds a new transtion to the state with # "sn". The transition looks for input character "in", writes "write"
; goes in direction "dir" and transitions to state with the # "st"
(define (add-trans sn in write dir st)
  (define s (get-state sn))  
  (edit-state sn (list sn (list (st-xpos s)(st-ypos s)) (list (st-selfx s)(st-selfy s)) (st-accept s)
                       (append (st-trans s) (list (list in write dir st)))))
  )

;default file-path for saving and loading the state machine.
;currently saves to the desktop with the name code.states
(define file-path (string->path (string-append (path->string(find-system-path 'desk-dir)) "\\code.states")))

;writes a list to a file
(define (list->file lst file)
  (display-lines-to-file lst
                         file
                         #:exists 'replace
                         #:mode 'binary))
;converts a number or a symbol to a string
; if it is neither, returns the input
(define (sym-or-num->str x)
  (cond [(number? x) (number->string x)]
        [(symbol? x) (symbol->string x)]
        [else x]))


;saves the state machine to a file at location "file-path"
(define (save)
  (define (p2)
    (define instr (file->string file-path))
    (define a (string-replace (string-replace (string-replace instr "#t" "t") "#f" "f") "#" "~"))
    (display-to-file a file-path #:exists 'replace)
   )
    (list->file states file-path)
  (p2)
  )

;loads the state-machine from a file at location "file-path"
(define (input)
  (define val #t)
  (if (file-exists? file-path)
  (let ([in (file->list file-path)])  
  (map (lambda (i)         
         (if (equal? (st-accept i) 't) (set! val #t) (set! val #f))
         (list (st-num i) (list (st-xpos i) (st-ypos i)) (list (st-selfx i) (st-selfy i)) val
               (map (lambda (j)
                      (list (sym-or-num->str (car j)) (sym-or-num->str (cadr j)) (sym-or-num->str (caddr j)) (cadddr j))) (st-trans i))))
       in)) states)
  )

;calls the input function, and changes states to it's return value
(define (load)
(set! states (input)))


;allows the user to change the save/load path to string str
(define (change-path str)
  (set! file-path (string->path str)))

;edits the tape to the values given
(define (set-tape str pos state)
  (set! track (list str pos state)))

;creates a string that is the same length as the tape.
;every character will be X except the current position of the tape with a _
(define (trackpos)
  (define (loop str n x out)
    (if (> n (string-length str)) out
        (if (= n x) (loop str (+ n 1) x (string-append out "_"))
            (loop str (+ n 1) x (string-append out "X")))))
  (loop (car track) 1 (cadr track) "")
  )