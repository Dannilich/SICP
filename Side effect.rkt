;Задачки по теме "Побочный эффект и присваивание"
#lang racket

;№3.1
(define (make-accumulator value)
  (lambda (income)
    (begin (set! value (+ value income)) value)))

;№3.2
(define (make-monitored f)
  (define calls-count 0)
  (lambda (args)
    (if (equal? args "how-many-calls?")
        calls-count
        (begin (set! calls-count (+ 1 calls-count))
               (f args)))))