;;; ==========================
;;; Michael Goldman, Spring 2014
;;; Assign 4
;;; ==========================

(load "asmt-helper.txt")

(header "Assignment 4" "Michael Goldman")

(problem "Problem 1: GET-FUNC-VALUE")

(define get-func-value
  (lambda (domain-value func-pairs)
    (cond
      [(null? func-pairs) ()]
      [(null? domain-value) ()]
      [(equal? domain-value (first (first func-pairs)))
       (second (first func-pairs))]
      [else (get-func-value domain-value (rest func-pairs))]
      )
    )
  )

(tester '(get-func-value 2 '((1 2) (3 2) (2 4) (5 6))))
(tester '(get-func-value 3 '((1 2) (3 2) (2 4) (5 6))))
(tester '(get-func-value 7 '((1 2) (3 2) (5 2) (1 3))))
(tester '(get-func-value () '(1 2)))
(tester '(get-func-value 4 '()))
(newline)
(problem "Problem 2: IMAGE")

(define helper
  (lambda (listy)
    (cond
      [(empty? listy) ()]
      [(member (first listy) (rest listy))
       (helper (rest listy))]
      [else (cons (first listy) (helper (rest listy)))]
      )
    )
  )
(tester '(helper '(1 1 2 2 2 3 4 4)))
(tester '(helper ()))
(tester '(helper '((1 1) (1 2) (1 1) (1 3))))
(newline)

(define image
  (lambda (subset-of-domain func-pairs)
    (cond
      [(null? subset-of-domain) ()]
      [(null? func-pairs) ()]
      [else (helper (map (lambda (subbie)
                           (get-func-value subbie func-pairs))
                         subset-of-domain))]
      )
    )
  )
(tester '(image '(1 2 3) '((5 4) (3 4) (1 4) (2 6) (8 9))))
(tester '(image '(1 2 3) '()))
(tester '(image () '((1 2) (3 2))))
(tester '(image '(1 2 3) '((1 1) (2 2) (3 1))))
(tester '(image '(1 4 2) '((4 1) (1 1) (2 1))))