#lang scheme

(require "zipcodes.scm")
(require "run_funcs.scm")

(define (ShowResults)
  (display "Select List: '(1 . 2) '(-1 1 2 3 4 -4 5): ")
  (mydisplay (select '(1 . 2) '(-1 1 2 3 4 -4 5)))
  (display "Select List: '(-1 . 3) '(-1 1 1 2 3 4 -4 5)): ")
  (mydisplay (select '(-1 . 3) '(-1 1 1 2 3 4 -4 5)))
  (display "Select List: '(8 . 9) '(-1 1 1 2 3 4 -4 5): ")
  (mydisplay (select '(8 . 9) '(-1 1 1 2 3 4 -4 5)))
  (display "Select List: '(3 . 1) '(-1 1 1 2 3 4 -4 5): ")
  (mydisplay (select '(3 . 1) '(-1 1 1 2 3 4 -4 5)))

  (display "Flatten List: '('a' 'b' 'c'): ")
  (mydisplay (flatten '("a" "b" "c")))
  (display "Flatten List: '('a' ('a' 'a') 'a'): ")
  (mydisplay (flatten '("a" ("a" "a") "a")))
  (display "Flatten List: '(('a' 'b') ('c' ('d') 'e') 'f'): ")
  (mydisplay (flatten '(("a" "b") ("c" ("d") "e") "f")))

  (display "Crossproduct: '(1 2) & '('a' 'b' 'c'): ")
  (mydisplay (crossproduct '(1 2) '("a" "b" "c")))
  (display "Crossproduct: '(1 2 'j') & '(5 -1): ")
  (mydisplay (crossproduct '(1 2 "j") '(5 -1)))

  (MenuLoop)
)

(define (ZipSearchHelper zip lst)
  (cond
    ((null? lst) (displayln "Zipcode not found."))

    ((= (caar lst) zip)
      (let ((ans (car lst)))
        (display "\nZipcode: ")
        (display (car ans))
        (display "\nPlace: ")
        (display (cadr ans))
        (display "\nState: ")
        (display (caddr ans))
        (display "\nCounty: ")
        (display (cadddr ans))
        (display "\nLat and Long: ")
        (display (cadddr (cdr ans)))
        (display ", ")
        (display (cadddr (cddr ans)))
      ))
    
    (else (ZipSearchHelper zip (cdr lst)))
  )
)

(define (PlaceSearchHelper plc lst)
  (cond
    ((null? lst) (displayln "Place not found."))

    ((string=? (cadar lst) plc)
      (let ((ans (car lst)))
        (display "\nZipcode: ")
        (display (car ans))
        (display "\nPlace: ")
        (display (cadr ans))
        (display "\nState: ")
        (display (caddr ans))
        (display "\nCounty: ")
        (display (cadddr ans))
        (display "\nLat and Long: ")
        (display (cadddr (cdr ans)))
        (display ", ")
        (display (cadddr (cddr ans)))
      ))
    
    (else (PlaceSearchHelper plc (cdr lst)))
  )
)

(define (FindByZipcode)
  (display "Input Zipcode >")
  (flush-output)
  (let ((zipcode (read)))
    (ZipSearchHelper zipcode zipcodes)
  )

  (MenuLoop)
)

(define (FindByPlace)
  (display "Input Place >")
  (flush-output)
  (read-line)
  (let ((place (read-line)))
    (PlaceSearchHelper place zipcodes)
  )

  (MenuLoop)
)

; This is the helper function for StatesWithPlace. 
(define (StatesWithPlaceHelper plc lst states)
  (cond
    ; This is the base case of the recursion. If the list
    ; is null then the function navigated through each zipcode.
    ; This will display the appropriate result.
    ((null? lst)
     ; If states is null then there was no state that contained
     ; the place, so the place was not found.
     (if (null? states)
         (displayln "Place not found")
     ; This begins the else statement. If the function got to the
     ; end of the zipcodes list and found states, then the function
     ; will output the list of states with the mydisplay function
     ; from the run_funcs.scm. 
     (begin
       (display "\nStates with Place '")
       (display plc)
       (display "': ")
       (mydisplay states)))
     )
    ; If the list of zipcodes is not null, then the function needs to
    ; continue navigating through the zipcodes list. If the second
    ; element in the first record in lst is the same as the given place
    ; then state becomes the third element of the first record in lst
    ; (which is the state of that record).
    ((string=? (cadar lst) plc)
     (let ((state (caddar lst)))
       ; If state is a member of the states list then the helper
       ; function is called without adding the state to the states list.
       (if (member state states)
           (StatesWithPlaceHelper plc (cdr lst) states)
           ; If the state is not a member then  the helper function
           ; is called and the state is added to the states list via cons.
           (StatesWithPlaceHelper plc (cdr lst) (cons state states)))))
    ; If the given place does not match the current record's place,
    ; then skip the current record and call the helper function with
    ; the rest of the zipcodes.
    (else (StatesWithPlaceHelper plc (cdr lst) states))))

; Function that gets a place name and displays all non-duplicate
; states that have the given place name. 
(define (StatesWithPlace)
  ; Asks the user to input a place
  (display "Input Place >")
  (flush-output)
  ; The first (read-line) consumes the leftover newline
  ; from the menu's read call.
  (read-line)
  ; Binds the input to a local variable place
  (let ((place (read-line)))
    ; Calls the helper function with the given place,
    ; zipcodes (which is the list of zipcodes from zipcodes.scm),
    ; and an empty list that the states will go in.
    (StatesWithPlaceHelper place zipcodes '()))
  ; The function calls menu loop again in order to display all the
  ; options to the user.
  (MenuLoop)
 )

;(define (CommonPlacesBetween)
;)

;(define (CountZipCodesForState)
;)


(define (MenuLoop)
  (display "\n-----------------------------------------------\n
Pick your option\n
1. Show Results\n
2. Find by Zipcode\n
3. Find by Place\n
4. Find states that have the given place\n
5. Find common places between states\n
6. Count zip codes for a given state\n
-----------------------------------------------\n
# > ")
  (flush-output)
  (let ((select-val (read)))
  (cond
    ((= select-val 1) (displayln "Show Results") (ShowResults))
    ((= select-val 2) (displayln "Find by Zipcode" (FindByZipcode)))
    ((= select-val 3) (displayln "Find by Place") (FindByPlace))
    ((= select-val 4) (displayln "Find states that have the given place") (StatesWithPlace))
    ;((= select-val 5) (displayln "Find common places between states") (CommonPlacesBetween))
    ;((= select-val 6) (displayln "Count zip codes for a given state") (CountZipCodesForState))
    (else (displayln "End Function"))
  ))
)

(MenuLoop)