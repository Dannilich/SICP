;Задания по функции Fold 
#lang racket

 
(define (fold f a n)
  (if (zero? n)
      a
      (f (- n 1) (fold f a (- n 1)))))


;Функция многократного применения (или как кратность применения)
(define (apply-n f x n)
  (if (zero? n)
      x
      (apply-n f (f x) (- n 1))))

;Реализуем через неё fold
(define (fold-by-apply-n f a n)
  (define (accum pair)
    (cons (+ 1 (car pair)) (f (car pair) (cdr pair))))
  (cdr (apply-n accum (cons 0 a) n)))


;Реализация функций через fold

;Факториал
(define (factorial n)
  (fold-by-apply-n  (lambda (x y) (if (zero? x) 1 (* (+ 1 x) y)))
        1
        n))

;Возведение в степень
;Выражено с помощью лямбда исчисления: \n.\m.\x.\y.n(m(x y))
(define (pow m n)
  ((fold-by-apply-n (lambda (a b) (lambda (x) (* x (b x))))
         (lambda (x) 1)
          n)
    m))