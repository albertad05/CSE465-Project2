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

; Option 4 helper: walks the list and collects each state with this place name - Ranold Antwi
(define (StatesWithPlaceHelper plc lst found)
  (cond
    ((null? lst) found)

    ((and (string=? (cadar lst) plc)
          (not (member (caddar lst) found)))
      (StatesWithPlaceHelper plc (cdr lst) (cons (caddar lst) found)))

    (else (StatesWithPlaceHelper plc (cdr lst) found))
  )
)

; Option 4: asks for a place name and prints every state that has it - Ranold Antwi
(define (StatesWithPlace)
  (display "Input Place >")
  (flush-output)
  (read-line)
  (let ((states (StatesWithPlaceHelper (read-line) zipcodes '())))
    (if (null? states)
      (displayln "No states have that place.")
      (begin
        (display "\nStates with that place: ")
        (displayln states)
      )
    )
  )

  (MenuLoop)
)
;(define (CommonPlacesBetween)
;)

; Option 6 helper: counts entries whose state matches - Ranold Antwi 
(define (CountZipsHelper st lst count)
  (cond
    ((null? lst) count)
    ((string=? (caddar lst) st) (CountZipsHelper st (cdr lst) (+ count 1)))
    (else (CountZipsHelper st (cdr lst) count))
  )
)

; Option 6: asks for a state abbreviation and prints how many zipcodes it has - Ranold Antwi 
 (define (CountZipCodesForState)
  (display "Input State (ex: OH) >")
  (flush-output)
  (read-line)
  (let ((state (string-upcase (read-line))))
    (let ((total (CountZipsHelper state zipcodes 0)))
      (if (= total 0)
        (displayln "No zipcodes found. Check that the state abbreviation is valid.")
        (begin
          (display "\nZipcodes in ")
          (display state)
          (display ": ")
          (displayln total)
        )
      )
    )
  )

  (MenuLoop)
)

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
    ;((= select-val 4) (displayln "Find states that have the given place") (StatesWithPlace))
    ;((= select-val 5) (displayln "Find common places between states") (CommonPlacesBetween))
    ;((= select-val 6) (displayln "Count zip codes for a given state") (CountZipCodesForState))
    (else (displayln "End Function"))
  ))
)

(MenuLoop)
