                                                                     
                                                                     
                                                                     
                                             
;;--------------------------------
;;Michael Goldman
;;Lab 7
;;--------------------------------

(load "asmt-helper.txt")
(load "lab7-helper.txt")
(problem "1) IS-FULL-HOUSE?")

;;Input: Listy, takes a list of 5 elements in a list, 1-13, to represent cards
;;Output: Returns a boolean if it has a full-house or not

(define is-full-house?
  (lambda (listy)
    (define temp (sort listy <=))
    ;Define a temp variable of the sorted list
    (cond
      [(and (equal? (first temp) (second temp))
            (equal? (first temp) (third temp)))
       ;If first is equal to the second and third
       (equal? (fourth temp) (fifth temp))]
      ;return the boolean of if fourth is equal to the fifth
      [(and (equal? (third temp) (fourth temp))
            (equal? (third temp) (fifth temp)))
       ;if third is equal to the fourth and the fifth
       (equal? (first temp) (second temp))]
      ;return the boolean of if first is equal to the second
      [else #f]
      )
    )
  )


(tester '(is-full-house? '(3 10 3 10 3)))
(tester '(is-full-house? '(10 10 10 2 2)))
(tester '(is-full-house? '(3 3 10 10 10)))
(tester '(is-full-house? '(1 1 1 1 5)))
(tester '(is-full-house? '(1 2 3 4 5)))
(tester '(is-full-house? '(13 13 12 1 12)))

(define *deck* '(1 1 1 1 2 2 2 2 3 3 3 3 4 4 4 4 5 5 5 5 6 6 6 6
                   7 7 7 7 8 8 8 8 9 9 9 9 10 10 10 10 11 11 11 11
                   12 12 12 12 13 13 13 13))
(define 5-rand-cards 
  (lambda () 
    (rand-choose-k 5 *deck*)))

(5-rand-cards)


(problem "2) TEST-FULL-HOUSE-ACC/-WR")

;Input: num, a non-zero natural integer
;       acc, 0
;Output: returns the number of full houses in num hands dealt
(define test-full-house-acc
  (lambda (num acc)
    (cond
      [(zero? num) acc]
      ;If num is zero, return the accumulator
      [(is-full-house? (5-rand-cards))
       ;test to see if the hand if a full house
       (test-full-house-acc (sub1 num) (add1 acc))]
      ;if it is, recall the function subtracting 1 from num and add 1 to acc
      [else (test-full-house-acc (sub1 num) acc)]
      ;else, recall the function subracting 1 from num
      )
    )
  )

;Input: num, a non-zero natural integer
;Output: returns the percentage of full houses in a num hands dealt
(define test-full-house-wr
  (lambda (num)
    (/ (* 1.0 (test-full-house-acc num 0)) num)
    )
  )
(* 1.0 (/ 3744 2598960))
(tester '(test-full-house-wr 100000))
(tester '(test-full-house-wr 100000))

;Input: listy, a list of 5 elements, 1-13 that represents a card hand
;Output: a boolean value if the hand has four of a kind
(problem "3) FOUR-OF-A-KIND")
(define is-four-of-a-kind?
  (lambda (listy)
    (define temp (sort listy <=))
    ;define temp that sorts listy
    (cond
      [(and (equal? (first temp) (second temp))
            ;if listy's first is equal to the second, third and fourth
            (equal? (first temp) (third temp))
            (equal? (first temp) (fourth temp)))
       ;then return true
       #t]
      [(and (equal? (second temp) (third temp))
            ;if listy's second is equal to the third, fourth and fifth
            (equal? (second temp) (fourth temp))
            (equal? (second temp) (fifth temp)))
       ;then return true
       #t]
      [else #f]
      )
    )
  )
(tester '(is-four-of-a-kind? '(1 4 1 1 1)))
(tester '(is-four-of-a-kind? '(1 2 3 4 5)))
(tester '(is-four-of-a-kind? '(1 1 1 1 4)))
(tester '(is-four-of-a-kind? '(10 10 10 10 2)))
(tester '(is-four-of-a-kind? '(10 12 12 10 10)))
(tester '(is-four-of-a-kind? '(1 11 1 11 1)))

;Input: num, a non-zero natural integer
;       acc, 0
;Output: returns the number of four of a kind in num hands dealt
(define test-four-of-a-kind-acc
  (lambda (num acc)
    (cond
      [(zero? num) acc]
      ;If num is zero, return acc
      [(is-four-of-a-kind? (5-rand-cards))
       ;test to see if your hand is a four of a kind
       (test-four-of-a-kind-acc (sub1 num) (add1 acc))]
      ;if true, recall the function sub1 to num and add1 to acc
      [else (test-four-of-a-kind-acc (sub1 num) acc)]
      ;else: recall the  function, sub1 to num
      )
    )
  )

;Input: num, a non-zero natural integer
;Output: returns the probability of four of a kind in num hands dealt
(define test-four-of-a-wr
  (lambda (num)
    (/ (* 1.0 (test-four-of-a-kind-acc num 0)) num)
    )
  )

(tester '(test-four-of-a-wr 100000))
(tester '(test-four-of-a-wr 100000))
