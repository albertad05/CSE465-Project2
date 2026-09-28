#lang scheme

(require "zipcodes.scm")
(require "run_funcs.scm")

(define (MenuLoop)
  (display "Pick your option\n
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
    ((= select-val 1) (displayln "Show Results"))
    ((= select-val 2) (displayln "Find by Zipcode"))
    ((= select-val 3) (displayln "Find by Place"))
    ((= select-val 4) (displayln "Find states that have the given place"))
    ((= select-val 5) (displayln "Find common places between states"))
    ((= select-val 6) (displayln "Count zip codes for a given state"))
    (else (displayln "End Function"))
  ))

  (MenuLoop)
)

(MenuLoop)