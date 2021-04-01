
(print "Задание 4. Определите функцию,порождающую по заданному натуральному числу N список,состоящий из натураланых чисел от 1 до N.")
(defun start ( n &optional(k 1) (lst '()))
    (cond 
        ((< k (+ n 1)) (cons k (start n (+ k 1) lst)))))


(print (start 10))
;1 2 3 4 5 6 7 8 9 10
(print (start 1))
; 1 2 3 4 5
(print (start 0))
;NIL

(print "Задание 11. Определите функцию,осуществляющую разделение исходного списка на два подсписка.
       Впервый из них должно попасть указанное количество элементов сначала списка,во второй оставшиес элементы.")

(defun separ (lst n &optional (head '()))
    (cond
        ((NULL lst) nil)
        ((equal n 0) (list (rev head '()) lst))
        (t (separ (cdr lst) (- n 1) (cons (car lst) head)))))

(defun rev (lst1 lst2)
    (cond
        ((null lst1) lst2)
        (t (rev (cdr lst1) (cons (car lst1) lst2)))))

(print (separ '(1 2 3 4 5 6 7 8 9) 3))
;((1 2 3) (4 5 6 7 8 9))
(print (separ '(1 2 3 4 5) 10))
;NIL
(print (separ '(1 2 3 4 5) 0))
;(NIL (1 2 3 4 5))

(print "Задание 20. Определите функцию ПЕРВЫЙ-АТОМ,результатом которой будет первый атом списка.")

(defun ПЕРВЫЙ-АТОМ(x)
    ((lambda (f1)
        (cond 
            ((ATOM f1) f1)
            (t (ПЕРВЫЙ-АТОМ f1))
        ))
    (car x)))

(print (ПЕРВЫЙ-АТОМ '(((5) 6 7)4 (1 2) 2 3)))
;5
(print (ПЕРВЫЙ-АТОМ '(1 2 3 4 5)))
;1
(print (ПЕРВЫЙ-АТОМ '(() (1 2 3) 1 1)))
;NIL

(print "Задание 26. Разбиение списка по парам");ПРИНЯТО

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


(print "Задание 31. Первый совпадающий");ПРИНЯТО

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

(print "Задание 35. Определите функцию ПОДМНОЖЕСТВО,которая проверяет,является ли одно множество подмножеством другого.")

(defun Подмнож (lst1 lst2)
    (cond
        ((null lst1) t)
        ((null lst2) (check lst1 lst2))
        ((mem (car lst1) lst2) (Подмнож (cdr lst1) lst2))
        (t nil)))

(defun check (lst1 lst2)
    (cond
        ((equal lst1 lst2) t)))

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

(print "Задание 36. Не Пересечение");ПРИНЯТО

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
