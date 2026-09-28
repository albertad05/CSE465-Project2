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
  (if (= (caar lst) zip)
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
      )
      (ZipSearchHelper zip (cdr lst))
  )
)

(define (FindByZipcode)
  (display "Input Zipcode >")
  (let ((zipcode (read)))
    (ZipSearchHelper zipcode zipcodes)
  )

  (MenuLoop)
)

;(define (FindByPlace)
;)

;(define (StatesWithPlace)
;)

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
  (let ((select-val (read)))
  (cond
    ((= select-val 1) (displayln "Show Results") (ShowResults))
    ((= select-val 2) (displayln "Find by Zipcode" (FindByZipcode)))
    ;((= select-val 3) (displayln "Find by Place") (FindByPlace))
    ;((= select-val 4) (displayln "Find states that have the given place") (StatesWithPlace))
    ;((= select-val 5) (displayln "Find common places between states") (CommonPlacesBetween))
    ;((= select-val 6) (displayln "Count zip codes for a given state") (CountZipCodesForState))
    (else (displayln "End Function"))
  ))
)

(MenuLoop)