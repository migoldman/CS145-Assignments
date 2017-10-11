;;===========================
;; Lab 145, HW5
;; Michael Goldman
;;===========================

(load "hw5-helper.txt")

;;; CRACKIE
;;;------------------------------------------
;;; INPUTS: N, a natural number
;;; OUTPUT: N number of expressions of two random numbers added together
;;; that then provides a boolean value to see if the theorm CRACKIE holds
;;; true for them, or if it is false
(problem "1) CRACKIE")
(define test-crackie
  (lambda (n)
    ; In the following expression, the variable I takes on the values
    ; 0, 1, 2, ..., n-1. For each value of I, the "body" of the DOTIMES
    ; (i.e. the LET* expression is evaluated
    (dotimes (i n)
             ;RNDX, RNDY = A random number from 0 to 100
             ;NNX, NNY = the corresponding list of ones
             (let* ((rndx (random 100)) 
               (nnx (make-nn rndx)))
             (let* ((rndy (random 100))
               (nny (make-nn rndy)))
               ;Print out RNDX and RNDY and either #t or #f (#t indicates
               ;success, #f failure)
             (printf "[~A + ~A: ~A] "
                     rndx
                     rndy
                     (equal? (nn-addn nnx (nn-succ nny))
                             (nn-addn (nn-succ nnx) nny))))))))
             
(tester '(test-crackie 20))
  
;;; NN-MULT
;;; -------------------------------------------
;;; INPUTS: NNX, NNY, two natural numbers represented
;;; as lists of ones
;;; OUTPUT: The product of NNX and NNY represented
;;; as a list of ones
;;; Uses the following rules:
;;; x * 0 = 0
;;; x * S(y) = (x * y) + x

(problem "2)NN-MULT")
(define nn-mult
  (lambda (nnx nny)
    (cond
      [(or (equal? nnx null) (equal? nny null)) nn0]
      ;If nnx or nny is null, then return nn0 (since any number * 0 = 0)
      [else (nn-addn nnx (nn-mult nnx (nn-pred nny)))]
      ;Else add nnx to the recursive call of nn-mult subtracting one of nny
      )
    )
  )
(tester '(nn-mult nn2 nn3))
(tester '(nn-mult nn3 nn4))
(tester '(nn-mult nn0 nn5))
(tester '(nn-mult '(1) '(1 1 1 1)))

