;Задания по парам и спискам
#lang racket


;Выражение возвращающее список ((1 2) (3 4) (5 6))
(define expr-by-cons 
  (cons (cons 1 (cons 2 '())) (cons (cons 3 (cons 4 '())) (cons (cons 5 (cons 6 '() )) '() ))))

(define expr-by-list 
  (list (list 1 2) (list 3 4) (list 5 6)))


;Выражение, возвращ 4, при примении к выражению
(define expr-for-task
  (list (list 7) (list 6 5 4) (list 3 2) 1))

(define extract-4
  (cadr(cdadr expr-for-task)))


;Написать функцию, возвращающию нов. список с добавленным нов. элементом
(define (snoc l a)
  (if (null? l)
      (cons a '())
      (cons (car l) (snoc (cdr l) a))))


;Написать функцию возвращающию переврнутый список
;Оценить по времени в кол-ве cons в зависиости от длины списка

;Нехвостовая рекурсия,
;Время:О(n)=n+(n-1)+...+1~ n^2
;Память: O(n)=n (максимальная длина продолжения)
(define (reverse-rec list)
  (if (null? list)
      list
      (snoc (reverse-rec (cdr list)) (car list) )))

;Хвостовая,
;Время: O(n)=n
;Память: O(n)= 1(нет продолжений)
(define (reverse-tail list)
  (define (iter list-1 list-2)
    (if (null? list-1)
        list-2
        (iter (cdr list-1) (cons (car list-1) list-2))))
  (iter list '() ))

;№2.27
;Реаизация глубогого первеворота списка
(define (deep-reverse lst)
  (if (null? lst)
      '()
      (if (pair? (car lst))
          (snoc (deep-reverse (cdr lst)) (deep-reverse (car lst)) )
           (snoc (deep-reverse (cdr lst)) (car lst) ) )))

;No2.28
;Реализовать функцию получения листьев дерева
(define (leafs tree)
  (if (null? tree)
      '()
      (if (pair? tree)
          (append (leafs (car tree)) (leafs (cdr tree)))
          (list tree))))

;№2.31
(define (tree-map f tree)
  (if (null? tree)
      '()
      (if (pair? tree)
          (cons (tree-map f (car tree))  (tree-map f (cdr tree)))
          (f tree))))