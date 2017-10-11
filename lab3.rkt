;;=================================
;;CMPU-145, Spring 2014
;;Lab 3
;;Michael Goldman
;;=================================
(load "asmt-helper.txt")

(header "Lab3" "Michael Goldman and Jake Brawer")


;;;  FIND-IFFY
;;; ----------------------------------------
;;;  INPUTS:  PRED, a predicate that takes one input
;;;           LISTY, a list of potential inputs for PRED
;;;  OUTPUT:  The first element of LISTY that satisfies PRED;
;;;           or #f if no element of LISTY satisfies PRED.

(problem "Problem 1: FIND-IFFY")
(define find-iffy
  (lambda (pred listy)
    (cond
      ;;Base case: if listy is empty, return false
      ((null? listy) #f)
      ;;Case 1: If the pred function is true for the first of listy, return
      ;;the first element of listy
      ((equal? (pred (first listy)) #t)
       (first listy))
      ;;Else: recursively call the funciton with the rest of listy
      (else (find-iffy pred (rest listy)))
      )
    )
  )

(tester '(find-iffy even? '(1 23 5 40 17 16 2)))
(tester '(find-iffy even? '(1 3 5 7)))
(tester '(find-iffy even? '()))
(tester '(find-iffy even? '(2 4 6 8)))

;;;  FETCH-M2O-MATCH
;;; --------------------------------
;;;  INPUTS:  X, some Scheme item
;;;           Y, some Scheme item
;;;           PAIRS, a list of pairs
;;;  OUTPUT:  #f, if none of the pairs in PAIRS has the form (A Y)
;;;           for some item A that is different from X;
;;;           #t (or something that counts as #t) otherwise.

(problem "Problem 2: FETCH-M2O-MATCH")
(define fetch-m2o-match
  (lambda (x y pairs)
    (cond
      ;;Base case: If pairs is null, return false
      ((null? pairs) #f)
      ;;Case1: If the list of x and y is a member of  the first of pairs, recall
      ;;the list with same x and y with the rest of pairs
      ((member (list x y) (first pairs))
       (fetch-m2o-match x y (rest pairs)))
      ;;Case2: If x is not equal to the domain of the first of pairs, and y is
      ;;an element of the range of 
      ((and (not (equal? x (first (first pairs)))) 
            (equal? y (second (first pairs))))
       (first pairs))
      (else (fetch-m2o-match x y (rest pairs)))
      )
    )
  )

(tester '(fetch-m2o-match 1 2 '((3 5) (5 4) (8 2) (6 1))))
(tester '(fetch-m2o-match 1 2 '((1 2) (3 3))))
(tester '(fetch-m2o-match 1 2 '((1 2) (2 3) (1 3) (3 2))))

(problem "Problem 3: MANY-TO-ONE?")

;;;  MANY-TO-ONE?
;;; -------------------------------------
;;;  INPUT:  PAIRS, a list of pairs representing a relation
;;;  OUTPUT:  #t (or something that counts as TRUE) if the relation
;;;   is "many to one" (i.e., if it contains at least two pairs
;;;   that have different first items, but the same second item)

(define many-to-one?
  (lambda (pairs)
    (cond
      ((null? pairs) #f)
      ;;
      ((fetch-m2o-match (first (first pairs)) 
                        (second (first pairs)) pairs)
       ;;
           (fetch-m2o-match (first (first pairs)) 
                            (second (first pairs)) pairs))
      ;;
       (else (many-to-one? (rest pairs)))
       )
    )
  )

(tester '(many-to-one? '((1 2) (3 4) (5 8) (6 7) (2 12) (9 1) (10 1))))
(tester '(many-to-one? '((1 2) (3 4) (5 8) (6 7) (2 4) (9 1))))
(tester '(many-to-one? '((1 2) (3 4) (5 8) (6 7) (2 12) (9 1))))
      
       

  
  