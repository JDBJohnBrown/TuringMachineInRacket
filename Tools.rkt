#lang racket
;-----------------------------------------------------------------------
; -- Graphical Deterministic Turing Machine simulator in Racket --
; Author: John@JdbJohnBrown.com
; Copyright John Brown ~ May 1, 2016
; Code within this file was commonly used by both StateCode and DrawCode
;-----------------------------------------------------------------------
(provide exist-list)
(provide edit-list)
(provide list-last)
(provide next-in-list)
(provide prev-in-list)

; checks if the list contains an element with x at (caar)
(define (exist-list x lst)
    (if (null? lst) '()
        (if (= (caar lst) x) (car lst)
            (exist-list x (cdr lst))))
  )

;replaces every instance having the value x at (caar) of the list with "edit"
(define (edit-list x lst edit)
  (define (loop x lst new edit)
          (if (null? lst) new              
              (if (equal? (caar lst) x)                  
                  (loop x (cdr lst) (append new (list edit)) edit)
                  (loop x (cdr lst) (append new (list(car lst))) edit))))
  (loop x lst '() edit))

;returns the last element of a list
(define (list-last lst)
  (car (reverse lst)))

;gets the "next" element in a list.
;if the "curr" doesnt exist in lst or the last element is curr'
;will return first element
(define (next-in-list lst curr)
  (define (loop lst curr orig)
    (if (null? lst) (car orig)
        (if (equal? (car lst) curr)
            (if (null? (cdr lst)) (car orig) (cadr lst))
            (loop (cdr lst) curr orig))))
  (if (equal? curr '())
      (car lst)
      (loop lst curr lst))
)
;does the same as above but in reverse
(define (prev-in-list lst curr)
  (next-in-list (reverse lst) curr)
  )

