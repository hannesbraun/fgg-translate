;; AUTOMATICALLY GENERATED 2021-08-04 09:11:36.732936 UTC
#lang racket

(require racket/match)

(define (app f x)
  (cond
   ((procedure? f) (f x))
   ((symbol? f) (list f x))
   ((list? f) (append f (list x)))))

(define (not-equal? x y) (not (equal? x y)))
(define -impl (lambda (x-0) (match x-0)))
(app (app 'tuple-2 (app 'Kt-A 'tuple-0)) 'tuple-0)
