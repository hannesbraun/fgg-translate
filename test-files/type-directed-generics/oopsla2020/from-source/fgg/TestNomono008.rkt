;; AUTOMATICALLY GENERATED 2023-12-01 11:12:28.319545 UTC
#lang racket

(require racket/match)

(define (app f x)
  (cond
   ((procedure? f) (f x))
   ((symbol? f) (list f x))
   ((list? f) (append f (list x)))))

(define (not-equal? x y) (not (equal? x y)))

(define (-fst x)
  (match x
   ((list 'tuple-2 a _) a)
   ((list 'tuple-3 a _ _) a)))

(define (-snd x)
  (match x
   ((list 'tuple-2 _ b) b)
   ((list 'tuple-3 _ b _) b)))

(define (-thd x)
  (match x
   ((list 'tuple-3 _ _ c) c)))

(define (-id x) x)

(define (-to-string fmt . args) (apply format (cons fmt (map -render args))))
(define (-print-string fmt . args)
  (display (apply format (cons fmt (map -render args)))))

; Called when converting to string. Extracts the struct part from an interface value
(define (-render x)
  (match x
   ((list 'tuple-3 _ s _) (-render s))
   (_ (if (list? x) (map -render x) x))))

(define (-add x y)
  (if (string? x) (string-append x y) (+ x y)))

(define (iface-sigs? x)
  (and (list? x) (not (null? x)) (equal? (car x) '-sig)))

(define (-tyrep x)
  (cond
   [(number? x) 'Kt-int]
   [(char? x) 'Kt-rune]
   [(boolean? x) 'Kt-bool]
   [(string? x) 'Kt-string]
   [else
    (match
     x
     ((list 'tuple-2 tyrep _) tyrep)
     (_ (error 'ERROR "malformed value ~a" x)))]))

(define (-unpack x)
  (match
   x
   ((list 'tuple-3 _ val _) val)
   (val val)))

(define (-dyn-assert x)
  (match
   x
   ((list 'tuple-2 val sigs)
    (if (iface-sigs? sigs)
        ;; sigs is a pair (improper list), so we need cadr
        (list 'tuple-3 sigs val (-impls (-tyrep val) (cadr sigs)))
        (error 'ERROR "cannot cast value ~a to runtime type ~a" val sigs)))))

(define (-impls tyrep sigs)
  (if (null? sigs)
      '()
      (cons (-impl (list 'tuple-2 tyrep (car sigs))) (-impls tyrep (cdr sigs)))))

(define
 -impl
 (match-lambda**
  ((x-8)
   (match
    x-8
    ((list 'tuple-2 'Kt-A (list 'Km-ma1 x-1))
     (if
      (equal? (list 'tuple-2 (list 'tuple-1 'Kt-Any) (list 'tuple-1 'Kt-Any)) x-1)
      (app m-ma1-A 'tuple-0)
      (error
       'ERROR
       "found implementation of method ~a for struct ~a with signature ~a  but this does not match required signature ~a"
       'Km-ma1
       'Kt-A
       (list 'tuple-2 (list 'tuple-1 'Kt-Any) (list 'tuple-1 'Kt-Any))
       x-1)))
    ((list 'tuple-2 'Kt-B (list 'Km-mb1 x-5))
     (if
      (equal? (list 'tuple-2 (list 'tuple-1 'Kt-Any) (list 'tuple-1 'Kt-Any)) x-5)
      (app m-mb1-B 'tuple-0)
      (error
       'ERROR
       "found implementation of method ~a for struct ~a with signature ~a  but this does not match required signature ~a"
       'Km-mb1
       'Kt-B
       (list 'tuple-2 (list 'tuple-1 'Kt-Any) (list 'tuple-1 'Kt-Any))
       x-5)))
    (_ (error 'ERROR "no method implementation for ~a" x-8))))))
(define -star-ty (match-lambda** ((x-8) (match x-8 ('Kt-Any (app '-sig (list))) ('Kt-A 'Kt-A) ('Kt-B 'Kt-B) (_ x-8)))))
(define
 m-ma1-A
 (match-lambda**
  (('tuple-0)
   (match-lambda**
    ((x0)
     (match-lambda**
      (((list 'tuple-1 tv-a))
       (match-lambda**
        (('tuple-0)
         (app
          (app
           (app (app m-mb1-B 'tuple-0) (list 'tuple-2 'Kt-B 'tuple-0))
           (list
            'tuple-1
            (list 'tuple-3 (app -fst tv-a) (app -fst tv-a) (match-lambda** ((x-0) (app (app -thd tv-a) x-0))))))
          'tuple-0))))))))))
(define
 m-mb1-B
 (match-lambda**
  (('tuple-0)
   (match-lambda**
    ((x0)
     (match-lambda**
      (((list 'tuple-1 tv-b))
       (match-lambda**
        (('tuple-0)
         (app
          (app
           (app (app m-ma1-A 'tuple-0) (list 'tuple-2 'Kt-A 'tuple-0))
           (list
            'tuple-1
            (list 'tuple-3 'Kt-A 'Kt-A (match-lambda** ((x-4) (list 'tuple-3 (app '-sig (list)) x-4 (list)))))))
          'tuple-0))))))))))
(match
 (app
  (app
   (app (app m-ma1-A 'tuple-0) (list 'tuple-2 'Kt-A 'tuple-0))
   (list 'tuple-1 (list 'tuple-3 'Kt-Any (app '-sig (list)) -id)))
  'tuple-0)
 (_ (void)))
