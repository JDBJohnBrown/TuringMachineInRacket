#lang racket
;-----------------------------------------------------------------------
; -- Graphical Deterministic Turing Machine simulator in Racket --
; Author: John@JdbJohnBrown.com
; Copyright John Brown ~ May 1, 2016
; All the code wihin this file deals with the GUI of the edit & watch screens.
;-----------------------------------------------------------------------
(require 2htdp/image)
(require 2htdp/universe)
 (require lang/posn)
(require "StateCode.rkt")
(require "Tools.rkt")
(provide edit-states)
(provide watch-states)
(provide set-screen-size)
(provide set-transition-font-size)
(provide set-interface-font-size)

;----------------------Start variables------------------
(define sc-sizex 1200) ;size of the edit & view window. (sc-size x sc-size)
(define sc-sizey 1000) ;size of the edit & view window. (sc-size x sc-size)
(define (set-screen-size width height)(set! sc-sizex width)(set! sc-sizey height))
(define my_scene (empty-scene sc-sizex sc-sizey)) ;the main draw image. Everything is overlayed on this
(define st-r 15) ;State Radius
(define text-size 10) ;size of the transition texts
(define interface-font-size 12)
(define (set-transition-font-size size)(set! text-size size))
(define (set-interface-font-size size)(set! interface-font-size size))
(define input? #f)
(define input-q? "Title")
(define input-q2? "Subtext")
(define input-ans? "Answer")
(define input1 "")
(define input2 "")
(define input3 "")
;when addind a new transition, a line is drawn from the source to the mouse
;this tells (update-draw) whether this is occuring or not, and where the mouse currently is.
(define line-to-mouse? #f)
(define mouse-x 0)
(define mouse-y 0)

;-------------Used to track editing--------------
;currently selected object
(define edit_obj 0)
;when adding a transition, this is the destination
(define add_trans_obj 1)
;currently selected transition in the currently selected object
(define edit_trans '())
;whether or not we are trying to move the self-transition text. Originially pivoted around the state, hence rotating.
;was replaced, but the name stayed.
(define rotating #f)
;MASTER editing function. Change this to tell the program what we are currently trying to achieve
(define curr_mode 'edit-state)
;------------------------------------------------
;If an object is the pause object, then all-steps will pause at it.
(define pause_obj -1)
;-----------------------End variables-------------------


;basic standrdized circle for drawing states
(define state_circle
   (circle st-r 'outline "black"))

;draws the state circle as yellow if needed.
(define yellow_state_circle
  (overlay
   (circle st-r 'outline "black")
   (circle st-r 'solid "yellow")
   ))
;draws the state circle as red if needed.
(define red_state_circle
  (overlay
   (circle st-r 'outline "black")
   (circle st-r 'solid "salmon")
   ))
;creates an input box.
;Draws ques as a title
;Draws ques2 as a subtitle
;Draws "CAPSLOCK" if the program is going to capitalize input
;Draws a white "input box" with the text input in it.
(define (input_box ques ques2 input)
  (overlay
   (above
    (text ques interface-font-size "black")
    (rectangle 550 5 'solid "transparent")
    (text ques2 interface-font-size "black")
    (rectangle 550 55 'solid "transparent")
    (overlay/align "left" "middle"
                   (text (string-append " " input) interface-font-size "black")
                   (overlay
                   (rectangle 500 20 'solid "white")
                   (rectangle 502 22 'solid "black"))
                   )  
    
    )

   (overlay/align "middle" "bottom"
     (text "Enter to Accept                                  Esc to Exit" interface-font-size "black")
     (rectangle 550 200 'solid "gray"))
  (rectangle 554 204 'solid "black")
  )
  )




;simple text image q & state number
(define (state_text x)
  (beside/align "bottom"
    (text "q" 18 "black")
    (text x 8 "black")))

;draws a line from state 1 to state 2, in color c, and overlays it on img
(define (draw-line s1 s2 c img)  
  (define d (sqrt (+ (abs (expt (- (st-xpos s2) (st-xpos s1)) 2)) (abs (expt (- (st-ypos s2) (st-ypos s1)) 2)))))  
  (define r (/ st-r (+ 0.1 d)))
  (define x3 (+ (* r (st-xpos s2)) (* (- 1 r) (st-xpos s1))))
  (define y3 (+ (* r (st-ypos s2)) (* (- 1 r) (st-ypos s1))))
  (define x4 (+ (* r (st-xpos s1)) (* (- 1 r) (st-xpos s2))))
  (define y4 (+ (* r (st-ypos s1)) (* (- 1 r) (st-ypos s2))))
  (if (> (st-num s1) (st-num s2))
      (begin (set! x3 (+ x3 0.2)) (set! x4 (+ x4 0.2)) (set! y3 (+ y3 0.2)) (set! y4 (+ y4 0.2)))
      (begin (set! x3 (- x3 0.2)) (set! x4 (- x4 0.2)) (set! y3 (- y3 0.2)) (set! y4 (- y4 0.2))))
  (add-line img x3 y3 x4 y4 c)
  )

;draws a line from s to cords x y
(define (line-to-mouse x y sn img)
  (define s (get-state sn))
  (define d (sqrt (+ (abs (expt (- (st-xpos s) x) 2)) (abs (expt (- (st-ypos s) y) 2)))))  
  (define r (/ st-r (+ 0.1 d)))
  ;(define x3 (+ (* r (st-xpos s)) (* (- 1 r) x)))
  ;(define y3 (+ (* r (st-ypos s)) (* (- 1 r) y)))
  (define x3 (+ (* r x) (* (- 1 r) (st-xpos s))))
  (define y3 (+ (* r y) (* (- 1 r) (st-ypos s)))) 
  (add-line img x3 y3 x y "black")
  )
;adds the texts for one state transition.
;if it is the first transion from s1 to s2, then it will draw the line, and text saying s1->s2
(define (add-txt s1 trans txts c img c2)
  (if (null? txts) (set! txts txts)
  (if (pair? (car txts)) (set! txts txts) (set! txts (list txts))))
  (if (null? (exist-list (tr-state-val trans) txts) )     
      (cons (append  txts (list  (list (tr-state-val trans) (above 
                                (text (string-append (tr-in-val trans) " -> "
                                               (tr-write-val trans) ", " (tr-dir-val trans))
                                       text-size c)
                                (text (string-append "q" (number->string(st-num s1)) " -> q" (number->string(tr-state-val trans))) text-size c)
                                )))) (draw-line s1 (get-state (tr-state-val trans)) c2 img)) 
      (cons (edit-list (tr-state-val trans) txts (list  (tr-state-val trans) (above (text (string-append (tr-in-val trans) " -> "
                                                                                                   (tr-write-val trans) ", " (tr-dir-val trans))
                                                                                    text-size c)
                                                                        (cadr (exist-list (tr-state-val trans) txts)))))
            img)
            )
      )
;loop which draws all transitions from the state s.
;Overlays the text and lines onto img
(define (draw-trans s img)
  (define x1 (st-xpos s))
  (define y1 (st-ypos s))
  (define x2 0)
  (define y2 0)
  (define x3 0)
  (define y3 0)
  (define d 0)
  (define r 0)
  (define hpos "right")
  (define vpos "bottom")
  (define sts (st-trans s))
  (define sn (st-num s))
  (define (loop sts txts lines)
    (define c1 "black")
    (define c2 "black")
    (if (null? sts) (list txts lines)
        (begin
        (if (and (equal? (car sts) edit_trans)(= (st-num s) edit_obj))(begin (set! c1 "red")(set! c2 "red"))"")
        (if (null? edit_trans) 1
            (begin
            (if (and (equal? sn (tr-state-val edit_trans))(= (st-num s) edit_obj)) (set! c2 "red") "")
            (if (= (tr-state-val edit_trans) edit_obj)(set! c2 "black")"")))        
        (let ([a (add-txt s (car sts) txts c1 lines c2)])
        (loop (cdr sts) (car a) (cdr a)))))
    )
  (define b (loop sts '() img))
  (define (s-loop lst img)    
    (if (null? lst) img
        (begin
        (if (not (= (caar lst) sn))
        (begin
          (set! x2 (st-xpos (get-state (caar lst))))
          (set! y2 (st-ypos (get-state (caar lst))))
          (if (and (equal? x2 x1)(equal? y2 y1))
              (begin (edit-state (caar lst) (edit-x-y (caar lst) (+ x2 1) (+ y2 1)))
                     (set! x2 (st-xpos (get-state (caar lst))))
                     (set! y2 (st-ypos (get-state (caar lst)))))
                     "")          
          (set! d (sqrt (+ (expt (- x2 x1) 2) (expt (- y2 y1) 2))))
          (set! r (/ (/ d 2) d))
          (set! x3 (+ (* r x2) (* (- 1 r) x1)))
          (set! y3 (+ (* r y2) (* (- 1 r) y1)))
          (if (> x1 x2) (set! vpos "bottom") (set! vpos "top"))
          (if (> y1 y2) (set! hpos "left")(set! hpos "right")))
        (begin
          (set! x3 (+ x1 (st-selfx s)))
          (set! y3 (+ y1 (st-selfy s)))
          (if (> y1 y3) (set! vpos "bottom")(set! vpos "top"))
          (if (> x1 x3) (set! hpos "left") (set! hpos "right"))
          ))
        (s-loop (cdr lst) (place-image/align  (cadar lst) x3 y3 hpos vpos img)))))


  ;(set! b (fix-list b))

  (s-loop (car b) (cadr b))

  )

;draws a circle with the state's name in it
;invokes draw-trans for the state s
;makes the state yellow if it is currently being edited
;draws a larger circle if the state is accpeting
(define (draw_state s)
  (define img (overlay
           (state_text (number->string (st-num s)))
           state_circle))
  (if (equal? pause_obj (st-num s)) (set! img (overlay (state_text (number->string (st-num s))) red_state_circle))
      (set! img img))
  (if (equal? edit_obj (st-num s)) (set! img (overlay (state_text (number->string (st-num s))) yellow_state_circle))
      (set! img img))
  (if (equal? (st-accept s) #t)
      (set! img (overlay img (circle (+ st-r 5)  'outline "black")))
      #f)
  
  (if (null? (st-trans s)) (set! my_scene my_scene) (set! my_scene (draw-trans s my_scene) ))
  (set! my_scene (place-image img (st-xpos s) (st-ypos s) my_scene))
 )

;checks if cords x,y are within the radius st-r of the state with the number st.
(define (in-circle? x y st)
  (if (and (>= x (- (st-xpos st) st-r)) (<= x (+ (st-xpos st) st-r)))
  (if (and (>= y (- (st-ypos st) st-r)) (<= y (+ (st-ypos st) st-r)))
   #t #f) #f)
 )

;iterates through all states. if one is clicked, then it will be changed to be the pause state
(define (set-pause-value x y)
  (define (loop lst)
    (if (null? lst) #f
    (if (in-circle? x y (car lst))
        (begin
          (if (= pause_obj (st-num (car lst))) (set! pause_obj -1)
               (set! pause_obj (st-num (car lst))))
               #t)
        (loop (cdr lst)))))
  (loop states))
;loops through all of the states.
;if x & y are within any of the states radius; it selects it, and sets the curr_mode to moving
(define (change-from-editing x y)
  (define (loop lst)
    (if (null? lst) #f
    (if (in-circle? x y (car lst))
        (begin 
               (set! curr_mode 'moving-state)
               (set! edit_obj (st-num (car lst)))
               #t)
        (loop (cdr lst)))))
  (loop states))

;add transition, checks if the mouse clicked a state
(define (add-transition-line x y)
  (define (loop lst)
    (if (null? lst) #f
    (if (in-circle? x y (car lst))
        (begin
               (set! line-to-mouse? #f)
               (set! curr_mode 'trans-input-1)
               (set! add_trans_obj (st-num (car lst)))
               (set! input-q? (string-append "Transition from q" (number->string edit_obj) " to q" (number->string add_trans_obj)))
               (set! input-q2? "What input should the transition read?")
               (set! input-ans? "|")
               (set! input? #t)
               #t)
        (loop (cdr lst)))))
  (loop states))
;attempt to rotate the text from the self transitions of a state around the state circle
;currently a WIP as it does not effeciently work yet
(define (rotate-state sn x2 y2)
  (define s (get-state sn))
  (define x1 (st-xpos s))
  (define y1 (st-ypos s))  
  (define x3 (- x2 x1))
  (define y3 (- y2 y1))
  (edit-state sn (edit-self-x-y sn x3 y3))
  )

;toggles if state sn is accepting or not
(define (swap-accepting sn)
  (define s (get-state sn))
  (edit-state sn (list (st-num s) (list (st-xpos s) (st-ypos s)) (list (st-selfx s) (st-selfy s)) (not (st-accept s)) (st-trans s)))
  )

;Does the input screen for new transitions
;Prompts the user for the input, output, and direction, then waits for them to confirm it with the return key
(define (do-trans-input a)
  (define char "")  
  (if (key=? a "escape")
      (begin
        (set! input? #f)
        (set! curr_mode 'edit-state))
  (cond [(equal? curr_mode 'trans-input-1)
         (set! char a)
         (if (or (equal? char "\b")(equal? char "\t")(equal? char "\r")(equal? char " "))""
         (if (= (string-length char) 1)
             (begin
               (set! curr_mode 'trans-input-2)
               (set! input1 char)
               (set! input-q2? "What should the transition write to tape?")
               (set! input-ans? (string-append " " input1 " -> |"))
               ) ""
          ))
         ]
        [(equal? curr_mode 'trans-input-2)
         (set! char a)
         (if (or (equal? char "\t")(equal? char "\r")(equal? char " "))""
         (if (or (equal? char "\b")(equal? char "left")(equal? char "LEFT"))
             (begin
             (set! curr_mode 'trans-input-1)
               (set! input-q2? "What input should the transition read?")
               (set! input-ans? "|"))
         (if (= (string-length char) 1)
             (begin
               (set! curr_mode 'trans-input-3)
               (set! input2 char)
               (set! input-q2? "What direction should the tape go?")
               (set! input-ans? (string-append " " input1 " -> " input2 ",  |"))
               ) ""
          )))
         ]
        [(equal? curr_mode 'trans-input-3)
         (set! char a)
         (if (or (equal? char "\t")(equal? char "\r")(equal? char " "))""
         (if (equal? char "\b")
             (begin
               (set! curr_mode 'trans-input-2)               
               (set! input-q2? "What should the transition write to tape?")
               (set! input-ans? (string-append " " input1 " -> |"))
               )
          (cond [(or(equal? char "left")(equal? char "l")(equal? char "4"))
                 (set! curr_mode 'trans-input-4)
                 (set! input-q2? "Press Enter/Return to confirm.")                 
                 (set! input3 "L")
                 (set! input-ans? (string-append " " input1 " -> " input2 ",  " input3))
                 ]
                [(or(equal? char "right")(equal? char "r")(equal? char "6"))
                 (set! curr_mode 'trans-input-4)
                 (set! input-q2? "Press Enter/Return to confirm.")                 
                 (set! input3 "R")
                 (set! input-ans? (string-append " " input1 " -> " input2 ",  " input3))]
                [(or(equal? char "up")(equal? char "down")(equal? char "s")(equal? char "8")(equal? char "2"))
                 (set! curr_mode 'trans-input-4)
                 (set! input-q2? "Press Enter/Return to confirm.")                 
                 (set! input3 "S")
                 (set! input-ans? (string-append " " input1 " -> " input2 ",  " input3))]
                [else ""])
         ))
         ]
        [(equal? curr_mode 'trans-input-4)
         (set! char a)
         (if (equal? char "\b")
             (begin
               (set! curr_mode 'trans-input-3)               
               (set! input-q2? "What direction should the tape go?")
               (set! input-ans? (string-append " " input1 " -> " input2 ",  |"))
               )
          (if (equal? char "\r")
              (begin
                (add-trans edit_obj input1 input2 input3 add_trans_obj)
                (set! input? #f)
                (set! curr_mode 'edit-state))
              "")
        )])
))







;handles all mouse interaction  with the world
(define (mouse-stuff w x y a)
  (if input? ""
  (cond [(mouse=? a "button-down")
         (set! edit_trans '())(set! rotating #f)
         (cond [(equal? 'edit-state curr_mode)
                (change-from-editing x y)]
               [(equal? 'adding-state curr_mode)
                (set! curr_mode 'edit-state)]
               [(equal? 'adding-trans curr_mode)
                (add-transition-line x y)]
               )      
         ]
        [(mouse=? a "drag")         
      (cond [(equal? 'moving-state curr_mode)            
             (edit-state edit_obj (edit-x-y edit_obj x y))])]
        [(mouse=? a "button-up")
      (cond [(equal? 'moving-state curr_mode)
             (set! curr_mode 'edit-state)]            
            )]
        [(mouse=? a "move")
         (cond
           [(equal? curr_mode 'adding-state)
            (edit-state edit_obj (edit-x-y edit_obj x y))]
           [rotating (rotate-state edit_obj x y)]
           [(equal? curr_mode 'adding-trans)
            (set! mouse-x x) (set! mouse-y y)
            ]
           )]
        )))

;handles all keyboard interaction with the world
(define (edit-keys w a)
  (if input?
      (do-trans-input a)
  (cond [(key=? a "r") (set! rotating #t)]
        [(key=? a "b") (swap-accepting edit_obj)]        
        [(key=? a "\b") (if (equal? edit_trans '())(delete-state edit_obj)(delete-trans edit_obj edit_trans))]        
        [(key=? a "a") (add-state)(set! curr_mode 'adding-state)(set! edit_obj (st-num (list-last states)))]
        [(key=? a "left") (set! edit_trans (prev-in-list (st-trans (get-state edit_obj)) edit_trans))]
        [(key=? a "right") (set! edit_trans (next-in-list (st-trans (get-state edit_obj)) edit_trans))]

        [(key=? a "t")
         (set! edit_trans '())
         (set! curr_mode 'adding-trans)
         (set! line-to-mouse? #t)
         ]
        )))

;updates the state drawings every tick.
(define (update-draw)
  (define (loop lst)
    (if (null? lst) my_scene
        (begin
        (draw_state (car lst))
        (loop (cdr lst)))))
  
     (loop states)
     (if line-to-mouse? (set! my_scene (line-to-mouse mouse-x mouse-y edit_obj my_scene)) "")
     (if input? (set! my_scene (overlay (input_box  input-q? input-q2? input-ans?) my_scene)) ""
     )
  my_scene
  )

;draws the edit screen. 
(define (draw-edit-scene x)
  (set! my_scene (empty-scene sc-sizex sc-sizey))  
  (update-draw)
  )

;draws the run screen. Same as the edit screen but with controls on the right, along with a view of the tape
(define (draw-view-scene x)
  (set! edit_obj (caddr track))
  (set! my_scene (empty-scene sc-sizex sc-sizey))  
  (update-draw)
  (define new_scene  (empty-scene (+ sc-sizex 200) sc-sizey))
  (set! new_scene (overlay
                   (beside/align "top"
                                 my_scene
                                 (above
                                 (beside
                                 (overlay
                                  (polygon (list (make-posn 0 0)(make-posn 0 20)(make-posn 50 0)(make-posn 0 -20)) "solid" "green")
                                  (rectangle 65 60 'outline "black"))                                 
                                 (overlay
                                  (beside
                                  (polygon (list (make-posn 0 0)
                                                 (make-posn 0 20)
                                                 (make-posn 50 0)
                                                 (make-posn 0 -20))
                                           "solid"
                                           "blue")
                                  (rectangle 10 40 "solid" "blue")) 
                                  (rectangle 65 60 'outline "black"))
                                 (overlay
                                  (text "X" 50 "red") 
                                  (rectangle 65 60 'outline "black")))
                                 (rectangle 0 20 "solid" "transparent")
                                 (overlay/align "left" "top"
                                  (text (car track) interface-font-size "black")
                                  (above
                                      (rectangle 0 2 "solid" "transparent")
                                      (overlay
                                       (rectangle (image-width (text (trackpos) interface-font-size "black")) (* interface-font-size .8) "solid" "white")
                                      (text (trackpos) interface-font-size "black")))))
                                 )
                   new_scene))
  new_scene
  )

(define (all-steps-w-pause)
  (define a (one-step))
    (if (and a (not(= (caddr track) pause_obj))) (all-steps-w-pause) '()))

(define (view-mouse w x y a)
  (cond [(mouse=? a "button-down")
  (cond [(and (> x sc-sizex)(< y 70))
         (cond [(<= x (+ sc-sizex 60))
                (one-step)]
               [(<= x (+ sc-sizex 120))
                (all-steps-w-pause)]
               [(<= x (+ sc-sizex 180))
                (set-tape (car orig)(cadr orig)(caddr orig))])
         ]
        [else (set-pause-value x y)]
        )]))



(define (set-edit x)
  (set! edit_obj x)
  )

(define (edit-states)
(big-bang '(75 .75) (on-mouse mouse-stuff) (to-draw draw-edit-scene) (on-key edit-keys) )
)

(define orig track)
(define (watch-states)
  (set! orig track)
  (big-bang '(75 .75) (on-mouse view-mouse) (to-draw draw-view-scene))
  )

