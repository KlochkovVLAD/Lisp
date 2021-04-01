
(print "Задание 4. Список 1-n")
(defun start (lst1 n k)
    (cond ((< k (+ n 1)) (cons k (start lst1 n (+ k 1))))))

(defun lst (n)
    (start () n 1)
    )

(print (lst 10))
;1 2 3 4 5 6 7 8 9 10
(print (lst 5))
; 1 2 3 4 5
(print (lst 0))
;NIL

(print "Задание 11. Разбиение списка на 2 подсписка")

(defun separ (lst n head)
    (cond
        ((NULL lst) nil)
        ((equal n 0) (list (rev head '()) lst))
        (t (separ (cdr lst) (- n 1) (cons (car lst) head)))))

(defun rev (lst1 lst2)
    (cond
        ((null lst1) lst2)
        (t (rev (cdr lst1) (cons (car lst1) lst2)))))

(defun start (lst n)
    (separ lst n ()))

(print (start '(1 2 3 4 5 6 7 8 9) 3))
;((1 2 3) (4 5 6 7 8 9))
(print (start '(1 2 3 4 5) 10))
;NIL
(print (start '(1 2 3 4 5) 0))
;(NIL (1 2 3 4 5))

(print "Задание 20. Первый атом")

(defun ATM (x)
    ((lambda (f1)
        (cond 
            ((ATOM f1) f1)
            (t (ATM f1))
        ))
    (car x)))

(print (ATM '(((5) 6 7)4 (1 2) 2 3)))
;5
(print (ATM '(1 2 3 4 5)))
;1
(print (ATM '(() (1 2 3) 1 1)))
;NIL

(print "Задание 26. Разбиение списка по парам")

(defun sep (lst)
    (cond
        ((NULL lst) lst)
        (t (cons (list (car lst) (cadr lst)) (sep (cddr lst))))))

(print (sep '(a b c d e f)))
;((A B) (C D) (E F))
(print (sep '()))
;NIL
(print (sep '(1 2 3)))
;((1 2) (3 NIL)) (изначально было ((1 2) 3) не знаю, какой правильный вариант)


(print "Задание 31. Первый совпадающий")

(defun Первый-Совпадающий (lst1 lst2)
    ((lambda (f1)
        (cond ((null lst1) nil)
        ((mem f1 lst2) f1)
        (t (Первый-Совпадающий (cdr lst1) lst2))))
     (car lst1)
     ))


(defun mem (atm lst2)
    (cond
        ((null lst2) nil)
        ((equal atm (car lst2)) atm)
        (t (mem atm (cdr lst2)))))


(print (Первый-Совпадающий '(1 4 6 10) '(0 2 3 5 10)))
;10
(print (Первый-Совпадающий '(1 4 6 10) '(2 3 5)))
;NIL
(print (Первый-Совпадающий '(1 4 6 10) '()))
;NIL

(print "Задание 35. Подмножество")

(defun Подмнож (lst1 lst2)
    (cond ((null lst2) (check lst1 lst2))
        ((equal (car lst1) (car lst2)) (ПроверкаВсехСледующих lst1 lst2))
        (t (Подмнож lst1 (cdr lst2)))))

(defun check (lst1 lst2)
    (cond
        ((equal lst1 lst2) t)))


(defun ПроверкаВсехСледующих (lst1 lst2)
    (cond
        ((NULL lst1) t)
        ((equal (mem (car lst1) lst2) nil) nil)
        (t (ПроверкаВсехСледующих (cdr lst1) (cdr lst2)))))

(defun mem (atm lst2)
    (cond ((null lst2) nil)
        ((equal atm (car lst2)) t)
        (t (mem atm (cdr lst2)))))

(print (Подмнож '(1 2 4) '(1 2 3 4)))
;T
(print (Подмнож '() '(1 2 3 4)))
;T
(print (Подмнож '(1 2 4) '()))
;NIL

(print "Задание 36. Не Пересечение")

(defun Пересечение (lst1 lst2)
    (cond ((null lst1) t)
        ((mem (car lst1) lst2) nil)
        (t (Пересечение (cdr lst1) lst2))))


(defun mem (x y)
    (cond
        ((null y) nil)
        ((equal x (car y)) t)
        (t (mem x (cdr y)))))


(print (Пересечение '(1 4 6 10) '(0 2 3 5 10)))
;NIL
(print (Пересечение '(1 4 6) '(2 3 5 10)))
;T
(print (Пересечение '() '()))
;T

(print "Задание 43. Множество вершин дерева")

(defun cntr (tree cnt)
    ((lambda (f1 f2)
    (cond
        ((NULL tree) cnt)
        ((ATOM f1) (cntr f2 (+ cnt 1)))
        (t (cntr f2 (cntr f1 cnt)))
        ))
     (car tree)
     (cdr tree)
))
(defun strt (lst)
    (cntr lst 0))

(print (strt '(1 (2 (3 4) 5 (6 7))) ) )
;7
(print (strt '(1 (2 3 4 5 6) ) ))
;6
(print (strt '() ) )
;0
