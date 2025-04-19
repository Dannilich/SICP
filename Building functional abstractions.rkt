;Построение абстракций, через более и более общие процедуры
#lang racket

(define (id x) x)
(define (add1 x) (+ x 1))
(define (add2 x) (+ x 2))

;Вычисление квадратного корня, через достаточную погрешность
;Как метод Герона Александрийского

;По заданию
(define epsilon 0.001)
(define (square x) (* x x))
(define (close-enough? x y)(< (abs (- x y)) epsilon))
(define (average x y) (/ (+ x y) 2))
(define (improve y x) (average y (/ x y)))

(define (sqrt-loop y x)
        (if (close-enough? (square y) x)
            y
            (sqrt-loop (improve y x) x)))

(define (sqrt x) (sqrt-loop 1.0 x))

;По SICP
(define (average-damp f) (lambda (x)(average x (f x)))) ;Торможние среднем
(define (fixed-point f first-guess)
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          guess
          (try next))))
  (try first-guess))

(define (sqrt-fixed-point x)
  (fixed-point (average-damp (lambda (y) (/ x y))) x))



;SCIP №1.7,
;вычисление кв. корня через фундаментальную последовательность (достаточно малая разность 2-х членов подряд)
(define (sqrt-fundamental-sequence x)
  (define (iter first-member second-member)
    (if (< (abs (- first-member second-member)) epsilon)
        second-member
        (iter second-member (improve second-member x))))
  (iter 1.0 (improve 1.0 x)))
;По итогу, при eps = 0.001, выигрывает этот подход в точности везде (и на малых 0.3 и больших 99999)


;Одна из задач дз:
;Представить Пи с помощью функции из примера и через ряд: пи/4 = 1 - 1/3 + 1/5...
(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a) (sum term (next a) next b))))

(define (pi-sum n)
  (* (sum
       (lambda (x) (* (/ 1 (+ (* (- x 1) 2) 1))  ((if (= (remainder x 2) 0) - +) 1)))
       1
       add1
       n)
     4))

;SCIP N1.31,
;Реализовать product итеративно и рекусривно
;Реализовать через неё функции для вычислений
(define (product-rec term a next b)
  (if (> a b)
      1
      (* (term a) (product-rec  term (next a) next b))))

;(define (product-iter term a next b)
;  (define (iter a accum)
;    (if (> a b)
;        accum
;        (iter (next a) (* (term a) accum ))))
;  (iter a 1))


(define (factorial n)
  (product-iter id 1 add1 n))

(define (neared-odd x) (if (= (remainder x 2) 0) (+ 1 x) x))

(define (pi-product n)
  (* 4 
   (/ (* 2 (product-iter square 4.0 add2 n))
      ((if (= (neared-odd n) n) / *)  (product-iter square 3.0 add2 n) (neared-odd n) ))))


;SCIP N1.32,
;Реализовать accumulate итеративно и рекусривно
;Реализовать через неё функции для вычислений
(define (accumulate-rec combine null-val term a next b)
  (if (> a b)
      null-val
      (combine (term a) (accumulate-rec combine null-val term (next a) next b))))

(define (accumulate-iter combine null-val term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (combine (term a) result))))
  (iter a null-val))


(define (product-iter term a next b)
  (accumulate-rec (lambda (x y) (* x y)) 1 term a next b))
;(define (sum term a next b)
;  (accumulate-rec (lambda (x y) (+ x y)) 0 term a next b))


;SCIP N1.33,
;Реализовать filter
(define (filter predicate combine null-val term a next b)
  (define (iter a result)
     (if (and (> a b) (predicate a))
         result
         (iter (next a) (combine (term a) result))))
   (iter a null-val))