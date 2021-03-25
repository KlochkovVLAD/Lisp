
;20
(defun f (x)
(cond ((ATOM (car x)) (car x))
(t (f (car x)))))

(print (f '(((5) 6 7)4 (1 2) 2 3)))

;4
(defun f1 (x n k)
(cond ((< k (+ n 1)) (cons k (f1 x n (+ k 1))))))

(print (f1 () 10 1)) 


;31
(defun f (x y)
(cond ((null x) nil)
((mem (car x) y) (car x))
(t (f (cdr x) y))))


(defun mem (x y)
(cond
((null y) nil)
((equal x (car y)) x)
(t (mem x (cdr y)))))


(print (f '(1 4 6 10) '(0 2 3 5 10))) 


;26


(defun f (x)
(cond
((null (cdr x)) x)
(t (cons (list (car x) (cadr x)) (f (cddr x))))))
(print (f '(a b c d e f)))


;11
(defun separ (lst n head)
(cond
((equal n 0) (list (rev head '()) lst))
(t (separ (cdr lst) (- n 1) (cons (car lst) head)))
)
)
(defun rev (x y)
(cond
((null x) y)
(t (rev (cdr x) (cons (car x) y)))))



(print (separ '(1 2 3 4 5 6 7 8 9) 3 '()))
