;Задания ведущие к методу умножения из древнего Египта
#lang racket

(define (even? x) (= (remainder x 2) 0))


;№1.16
;Итеративная версия быстрого возведения в степень
(define (fast-pow x n)
  (define (iter accum count)
    (if (= count n)
        accum
        (if (and (< (* count count) n ) (not(zero? count)))
            (iter (* accum accum) (* count 2)) 
            (iter (* accum x) (+ count 1)))))
  (iter 1 0))


;№1.17
;Реализация умножения через сложение
(define (double x) (+ x x))
(define (halve x)
  (define (iter cur)
    (if (or (= (double cur) x) (= (double cur) (- x 1)))
        cur
        (iter (- cur 1))))
  (iter x))

(define (additional-mul x y)
  (cond [(= y 0) 0]
        [(even? y) (double (additional-mul x (halve y)))]
        [else (+ x (additional-mul  x (- y 1)))]))


;№1.18
;Метод уножения из древнего Египта (в SICP метод русского крестьянина)
(define (egyptian-mul x y)
  (define (iter x-halved y-doubled sum)
    (if  (or (= x-halved 0) (= y-doubled 0))
        sum
        (if (even? x-halved)
            (iter (halve x-halved) (double y-doubled) sum)
            (iter (halve x-halved) (double y-doubled) (+ sum y-doubled)))))

  (iter x y 0))

;Cложность по времени:
;T(n) <= 1*T(n/2) + 2 
;Тогда Theta(2)=Theta(n^0), n^0 = n^(log2(1) = 0)

;Согласно основной теоремы о рекурсивных отношениях(Master theorem):
;T(n) = Theta(logn)  