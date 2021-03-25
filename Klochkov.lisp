
;20 |Найти первый атом списка
(defun f (x)
(cond ((ATOM (car x)) (car x))
(t (f (car x)))))

(print (f '(((5) 6 7)4 (1 2) 2 3)))

;4 Дано число N создать список числен от 1 до N
(defun f1 (x n k)
(cond ((< k (+ n 1)) (cons k (f1 x n (+ k 1))))))

(print (f1 () 10 1)) 


;31 Даны 2 списка, найти первый элемент первого списка, принадлижащий обоим спискам
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


;26 Определить функцию разбивающую список (a b c d e f)  на пары ((a b) (c d) (e f))


(defun f (x)
(cond
((null (cdr x)) x)
(t (cons (list (car x) (cadr x)) (f (cddr x))))))
(print (f '(a b c d e f)))


;11 Определить функцию осуществляющую разделение исходного списка на два подсписка, в первом первый N элементов, во втором оставшиеся
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

;36Проверить пересекаются  ли списки
(defun f (x y)
(cond ((null x) t)
((mem (car x) y) nil)
(t (f (cdr x) y))))


(defun mem (x y)
(cond
((null y) nil)
((equal x (car y)) x)
(t (mem x (cdr y)))))


(print (f '(1 4 6 10) '(0 2 3 5 10)))


(print (separ '(1 2 3 4 5 6 7 8 9) 3 '()))
