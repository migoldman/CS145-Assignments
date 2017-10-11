;;==================================
;;Michael Goldman
;;Assignment 3
;;==================================

(load "asmt-helper.txt")
(header "Assignment 3" "Michael Goldman")

(problem "Problem 1: Partitions and Equivalence Relations")

;;Input: listy ==> a list of elements
;;Output: Go through the first element of the list and pair it to all other
;;elements of the listy

(define helper
  (lambda (listy)
    (local    
      [(define listyfirst (first listy))
       (define helpy
         (lambda (listy)
           (cond
             ;Base case: if empty, return null
             [(empty? listy) null]
             ;Else: append the list of the list to the first of the original
             ;list and recall recursively
             [else (append (list (list listyfirst (first listy))) 
                           (helpy (rest listy)))]
             )
           )
         )]
      (helpy listy)
      )
    )
  )


;;Input: listy ==> a list of elements
;;Output: A list that cycles through all elements of helper and applies
;;the function to the rest of the elements.

;;To use cycle the funciton helper through the entire list 
(define helperrest
  (lambda (listy)
    (cond
      ;Base Case; if empty, return null
      [(empty? listy) null]
      ;Else; Append helper list to the recursive call
      [else (append (helper listy) (helperrest (rest listy)))]  
      )
    )
  )

;;Input: listy ==> a list of elements
;;Outputs: a list that contains the reverse combinations of elements in 
;;helperrest

(define reversehelp
  (lambda (listy)
    (helperrest (reverse listy))
    )
  )


;;Input: listy ==> a list of elements
;;Outputs: A list that contains all elements in reversehelp that
;;are not in helperrest. All unique elements of reversehelp

(define removehelp
  (lambda (listy)
    (local
      [(define removehelpacc
         (lambda (listy reverselisty acc)
           (cond
             ;If reverselisty is empty, return the accumulator
             [(empty? reverselisty) acc]
             ;If the first of reverselisty is in the orginal listy, then 
             ;recall the funciton recursively
             [(member (first reverselisty) listy)
              (removehelpacc listy (rest reverselisty) acc)]
             ;Else: append the accumulator to the list of the first of
             ;reverselisty and recall the funciton
             [else (append acc (list (first reverselisty))
                           (removehelpacc listy (rest reverselisty) acc))]
             )
           )
         )]
      (removehelpacc (helperrest listy) (reversehelp listy) ())
      )
    )
  )

;; Input: listy ==> a list of lists which is a partition
;; Output: a list that takes listy and returns a list of pairs representing
;;the corresponding equivalence relation

(define partition-to-equiv-reln
  (lambda (listy)
    (cond
      ;Base case: if listy is empty, return empty
      [(empty? listy) ()]
      [else (append (append (helperrest (first listy)) 
                            (removehelp (first listy)))
                    (partition-to-equiv-reln (rest listy)))]
      )
    )
  )
(tester '(partition-to-equiv-reln '((1 2) (3))))
(tester '(partition-to-equiv-reln '(())))
(tester '(partition-to-equiv-reln '((1 2) (3) (4))))
(tester '(partition-to-equiv-reln '((4 5 6))))
(tester '(partition-to-equiv-reln '((1 2) (3 4) (5 6))))




