#lang scheme
; run_funcs.scm
; This file is provided as part of project1.
(provide mydisplay)
(provide select)
(provide flatten)
(provide crossproduct)


; Students are to implement all function here and
; display the results in a clear, labeled way so
; it is obvious which output belongs to which expression
(define (mydisplay value)
  (display value)
  (newline)
)

; #1- 'select' function:
; The select function returns a new list holding only elements
; of the givel list that are in the range of given pair
; - Andrew Albert
(define (select range list)
  ; Check if the first number in range is greater than the second
  ; number, if so it shows an Error.
  (if (> (car range) (cdr range)) "Error"
  (cond
    ; This is our null case, if the list is null we want to return
    ; the empty list '()
    ((null? list) '())

    ; If the current first result in list is greater than or equal to
    ; the first number in range and less than or equal to the second
    ; number in range we want to convert that number to a list and call
    ; the select function with the first result in list removed to iterate
    ((and (>= (car list) (car range)) (<= (car list) (cdr range)))
     (append (cons (car list) '()) (select range (cdr list))))

    ; This is our default case, in this case no numbers are appended to a list
    ; and the select function is called with the first result in list removed
    ; to iterate
    (else (select range (cdr list)))
  )
  )
)

; You can change these displays to make them look nicer
(mydisplay (select '(1 . 2) '(-1 1 2 3 4 -4 5))) ; -> (1 2)
(mydisplay (select '(-1 . 3) '(-1 1 1 2 3 4 -4 5))) ; -> (-1 1 1 2 3)
(mydisplay (select '(8 . 9) '(-1 1 1 2 3 4 -4 5))) ; -> ()
(mydisplay (select '(3 . 1) '(-1 1 1 2 3 4 -4 5))) ; -> Error

; #2- 'flatten' function:
; Returns a list identical to the first list, while having all elements
; that are inside nested loops taken out. So we want to flatten all elements and have
; them all in a single list. For example ’(a (a a) a))) should become (a a a a)
; - Andrew Albert
(define (flatten lst)
  (cond
    ; This is our null case, if the list is null we want to return
    ; the empty list '()
    ((null? lst) '())

    ; If the current first result in lst is a list we want to enter that list with the
    ; flatten function and append the rest of the current list (lst) behind it to keep
    ; everything in order.
    ((list? (car lst)) (append (flatten (car lst)) (flatten (cdr lst))))

    ; If the current first result in lst is not a list we just want to turn it into a list
    ; and append the rest of the list with the first result removed with the flatten function.
    (else (append (cons (car lst) '()) (flatten (cdr lst))))
  )
)

(mydisplay (flatten '("a" "b" "c")))  ; -> (a b c)
(mydisplay (flatten '("a" ("a" "a") "a")))  ; -> (a a a a)
(mydisplay (flatten '(("a" "b") ("c" ("d") "e") "f")))  ; -> (a b c d e f)

; #3- 'crossproduct' function:
; The parameters are two lists. The result should contain the cross product
; between the two lists:
; The inputs ’(1 2) and ’(a b c) should return a single list:
; ((1 a) (1 b) (1 c) (2 a) (2 b) (2 c))
; lst1 & lst2 – two flat lists.
; - Andrew Albert
(define (crossproduct lst1 lst2)
  ; This is a helper function inside of crossproduct that is used to pair a pre-set
  ; item with each element of a list
  (define (pairelement item lst)
    (if (null? lst)
        ; If the lst is null just return the empty list '()
        '()
        ; Otherwise it constructs a list of (item, car lst) and iterates through the
        ; list lst with the first element removed each time.
        (cons (list item (car lst))
              (pairelement item (cdr lst)))))

  (if (or (null? lst1) (null? lst2))
      ; If either list is null return the empty list '()
      '()
      ; Otherwise it calls the helper function with the first element of lst1 as item,
      ; and calls crossproduct with the first element of lst1 removed to iterate through
      ; the list.
      (append (pairelement (car lst1) lst2) (crossproduct (cdr lst1) lst2)))
)

(mydisplay (crossproduct '(1 2) '("a" "b" "c"))) ; -> ((1 a) (1 b) (1 c) (2 a) (2 b) (2 c))
(mydisplay (crossproduct '(1 2 "j") '(5 -1))) ; -> ((1 5) (1 -1) (2 5) (2 -1) (j 5) (j -1))