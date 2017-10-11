;;-------------------------------
;;Michael Goldman
;;HW5
;;-------------------------------

(load "asmt-helper.txt")

(problem "1) GEN-ALL-PERMS")

(define two-helper
  (lambda (listy)
    (cond
      [(empty? listy) ()]
      [else (cons  listy (list (reverse listy)))]
      )
    )
  )

(tester '(two-helper '(1 2)))
(tester '(two-helper '(4 2)))    


(define gen-all-perms
  (lambda (listy)
    (cond
      [(equal? (length listy) 2) (two-helper listy)]
      [else (apply append (map (lambda (subbie) 
                   (map (lambda (permele) 
                          (cons subbie permele))
                         (gen-all-perms (remove subbie listy)))) listy))]
      )
    )
  )

(tester '(gen-all-perms '(a b)))
(tester '(gen-all-perms '(a b c)))
(tester '(length (gen-all-perms '(1 2 3 4))))
(tester '(length (gen-all-perms '(1 2 3 4 5))))


(problem "2) GEN-ALL-WAYS-OF-CHOOSING-K-ITEMS")
;1234 2
;all the combies with the first ele is 12 13 14, make the funciton, then do that

  
  
(define gen-all-ways-of-choosing-k-items
  (lambda (listy k)
    (cond
      ;Base case: if you have 0 k or 0 elements in a list, return ()
      [(or (equal? 0 k) (empty? listy)) ()]
      ;If k is 1, return the list of all the elements in the list
      [(equal? k 1) (map list listy)]
      ;If k is bigger than listy, return ()
      [else (append
             (map (lambda (subbie)
                    (cons (first listy) subbie))
                  (gen-all-ways-of-choosing-k-items (rest listy) (sub1 k)))
             (gen-all-ways-of-choosing-k-items (rest listy) k))]            
      )
    )
  )
     
(tester '(gen-all-ways-of-choosing-k-items '(1 2 3) 1))
(tester '(gen-all-ways-of-choosing-k-items '(1 2 3 4) 2))
(tester '(gen-all-ways-of-choosing-k-items '(1 2 3 4 5) 3))
(tester '(length (gen-all-ways-of-choosing-k-items '(1 2 3 4 5 6 7) 3)))
(tester '(gen-all-ways-of-choosing-k-items '(1 2 3) 20))