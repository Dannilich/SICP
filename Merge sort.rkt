;Merge sort
#lang racket


;Сливание 2-х упорядоченных списков линейно-рекурсивным процессом (итеративным надо каждый раз пробегатся в конец, когда тут можно просто на ходу заполнять)
(define (sorted-merge list-1 list-2)
  (define(rec-step l1 l2 merged-list)
    (cond [(null? l1) l2]
          [(null? l2) l1]
          [(> (car l1) (car l2)) (cons (car l2) (rec-step l1 (cdr l2) merged-list))]
          [else (cons (car l1) (rec-step (cdr l1) l2  merged-list))]
          ))
  
  (rec-step list-1 list-2 '()))

;Располовинивание списка
(define (halve lst)
  (cons (take lst (quotient (length lst) 2))
        (drop lst (quotient (length lst) 2))
        ))


;Итог
(define (merge-sort lst)
  (if (< (length lst) 2)
      lst
      (let ([divided-lst (halve lst)]) ;Подстановка половинок, вместо вычисления на каждую половину отдельно
        (sorted-merge (merge-sort (car divided-lst)) (merge-sort (cdr divided-lst)))
        )))

;Сложность по времени:
;T(n) = 2*T(n/2) + Theta(n)

;По основной теореме о рекурентных соотношениях(Master theorem):
;Theta(n^1) = Theta(n^(log2(2))) => T(n) = Theta(n*logn)