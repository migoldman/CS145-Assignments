;;======================================
;; CMPU-145, Lab 4
;; Michael Goldman
;;======================================

(load "asmt-helper.txt")

;;A) Global Variables

(define nn0 ())
(define nn1 '(1))
(define nn2 '(1 1))
(define nn3 '(1 1 1))
(define nn4 '(1 1 1 1))
(define nn5 '(1 1 1 1 1))



(problem "B) NN-SUCC")
;; INPUT: N, a natural number displayed as a list of 1's
;; OUTPUT: The successor to N where it is a list of 1's

(define nn-succ
  (lambda (n)
    (cons 1 n)
    )
  )

(tester '(nn-succ nn0))
(tester '(nn-succ nn1))
(tester '(nn-succ nn4))

(problem "C) MAKE-NN")
;; INPUT: N, an ordinary natural number
;; OUTPUT: makes list of N size of 1's

(define make-nn
  (lambda (n)
    (cond
      ;If n is zero, return nn0
      [(zero? n) nn0]
      ;else, take the nn-succ of the list of n-1 1's
      [else (nn-succ (make-nn (- n 1)))]
      )
    )
  )
(tester '(make-nn 4))
(tester '(make-nn 0))
(tester '(make-nn 6))

(problem "D) NN-ZERO?")
;; INPUT: n, a number displayed as a list of 1's
;; OUTPUT: a boolean value whether or not the list is null
(define nn-zero?
  (lambda (n)
    (null? n)
    )
  )
(tester '(nn-zero? nn2))
(tester '(nn-zero? nn0))
(tester '(nn-zero? '(1 1 1 1 1 1)))
(tester '(nn-zero? ()))


(problem "E) NN-PRED")
;; INPUT: N, a number displayed as a list of 1's
;; OUTPUT: The predecessor of N, also represented as a list of 1's

(define nn-pred
  (lambda (n)
    ;If n is equal to nn0, return nn0
    (if (equal? n nn0)
        nn0
        ;Else: return the rest of n, giving the predecessor 
        (rest n))
    )
  )

(tester '(nn-pred nn0))
(tester '(nn-pred nn3))
(tester '(nn-pred '(1 1 1 1 1 1 1 1 1)))

(problem "F) NN-ADDN")
;; INPUT: X, Y, nautral numbers represented by a list of 1's
;; OUTPUT: X+Y represented by a list of 1's

(define nn-addn
  (lambda (x y)
    (cond
      [(nn-zero? y) x]
      ;if y is equal to zero, return x
      [else (nn-succ (nn-addn x (nn-pred y)))]
      ;Else: return the successor of the recursive call nn-addn x and y-1
      )
    )
  )
(tester '(nn-addn nn0 nn0))
(tester '(nn-addn nn3 nn2))
(tester '(nn-addn nn4 nn0))
(tester '(nn-addn nn0 nn4))
  
  
  