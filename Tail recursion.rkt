;Построить функцию суммы арифметической прогресси на произваольным целом диапозоне
#lang racket


;Рекусивный процесс (не хвостовая рекурсия)
(define (Sum-of-sequence-recursive begin step end)
  (define (next-member begin)
    (if (> (+ begin step) end)
        end
        (+ begin (next-member (+ begin step)))))
  
  (next-member begin))


;Итеративный процесс (хвостовая рекурсия)
(define (Sum-of-sequence-iterative begin step end)
  (define (iter current-member sum )
    (if (> (+ current-member step) end)
        (+ sum end)
        (iter (+ current-member step) (+ sum current-member))))

  (iter begin 0))