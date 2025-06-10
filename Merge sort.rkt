#|
Отсортировать числа в массиве согласно сумме их цифр.
Для реализации используеттся Сортировка слиянием (Merge sort)
|#
#lang racket


;Подсчет суммы цифр:
;Удобно реализовать через итеративно-рекурсивный процесс
;   In: целое число (отрицательное будет считаться без знака)
;   Out: натуральное число (включая 0)
(define (sum-of-digits num)
  (define (iter num sum)
    (if (<  num 10)
        (+ sum num)
        (iter (quotient num 10) (+ sum (remainder num 10)))
   ))
  
  (iter (abs num) 0)
)

;Сливание 2-х упорядоченных списков:
;Вспомогательна для сортировки слиянием.
;Обобщена для использования целевой функции metric-function, по которой оценивается, а после сортируются элементы
;   In: 2 отсортированных списка чисел (т.к. списки из 1-ого элемента отсортированы и последующие будут так же) и целевая функция metric-function
;   Out: соедененный остортированный список
(define (sorted-merge list-1 list-2 metric-function)
  (define(rec-step l1 l2 merged-list)
    (cond
      [(null? l1) l2]
      [(null? l2) l1]
      [(> (metric-function (car l1)) (metric-function (car l2))) (cons (car l2) (rec-step l1 (cdr l2) merged-list))]
      [else (cons (car l1) (rec-step (cdr l1) l2  merged-list))]
   ))
  
  (rec-step list-1 list-2 '())
 )

;Располовинивание списка:
;   In: список
;   Out: список, в 1-ом элементе которого первая половина списка, во 2-ом элементе остальное
(define (halve lst)
  (cons (take lst (quotient (length lst) 2))
        (drop lst (quotient (length lst) 2))
))

;Сортировка слиянием:
;Обобщена для использования целевой функции metric-function, по которой оценивается, а после сортируются элементы
;   In: список и целевая функция metric-function
;   Out: отсортированный по метрике список 
(define (merge-sort lst metric-function)
  (if (< (length lst) 2)
      lst
      (let ([divided-lst (halve lst)]) ;Подстановка половинок, вместо вычисления на каждую половину отдельно
        (sorted-merge (merge-sort (car divided-lst) metric-function) (merge-sort (cdr divided-lst) metric-function) metric-function)
)))


;Итог:
;Создаём сортировку по цифрам через сортировку слиянием.
;Подсчет суммы чисел передается как целевая функция, по которой сортирует.
(define (sorted-by-digits lst) (merge-sort lst sum-of-digits))



;Сложность по времени:
;T(n) = 2*T(n/2) + Theta(n)

;По основной теореме о рекурентных соотношениях(Master theorem):
;Theta(n^1) = Theta(n^(log2(2))) => T(n) = Theta(n*logn) или упрощенно O(n*logn)