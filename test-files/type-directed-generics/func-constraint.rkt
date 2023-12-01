;; AUTOMATICALLY GENERATED 2023-12-01 10:36:22.991816 UTC
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
  ((x-14)
   (match
    x-14
    ((list 'tuple-2 'Kt-int (list 'Km-foo x-1))
     (if
      (equal? (list 'tuple-2 'tuple-0 (list 'tuple-1 'Kt-int)) x-1)
      (app m-foo-int 'tuple-0)
      (error
       'ERROR
       "found implementation of method ~a for struct ~a with signature ~a  but this does not match required signature ~a"
       'Km-foo
       'Kt-int
       (list 'tuple-2 'tuple-0 (list 'tuple-1 'Kt-int))
       x-1)))
    ((list 'tuple-2 (list 'Kt-Box (list 'tuple-1 x-8)) (list 'Km-bar x-6))
     (if
      (equal? (list 'tuple-2 'tuple-0 (list 'tuple-1 'Kt-int)) x-6)
      (app
       m-bar-Box
       (list
        'tuple-1
        (list
         'tuple-3
         x-8
         (app -star-ty x-8)
         (match-lambda**
          ((x-7)
           (app
            -dyn-assert
            (list
             'tuple-2
             (app -unpack x-7)
             (app '-sig (list (app 'Km-foo (list 'tuple-2 'tuple-0 (list 'tuple-1 'Kt-int))))))))))))
      (error
       'ERROR
       "found implementation of method ~a for struct ~a with signature ~a  but this does not match required signature ~a"
       'Km-bar
       'Kt-Box
       (list 'tuple-2 'tuple-0 (list 'tuple-1 'Kt-int))
       x-6)))
    (_ (error 'ERROR "no method implementation for ~a" x-14))))))
(define
 -star-ty
 (match-lambda**
  ((x-14)
   (match
    x-14
    ('Kt-Any (app '-sig (list)))
    ((list 'Kt-Box (list 'tuple-1 x-0)) (app 'Kt-Box (list 'tuple-1 x-0)))
    ('Kt-I (app '-sig (list (app 'Km-foo (list 'tuple-2 'tuple-0 (list 'tuple-1 'Kt-int))))))
    ('Kt-J (app '-sig (list (app 'Km-bar (list 'tuple-2 'tuple-0 (list 'tuple-1 'Kt-int))))))
    ('Kt-Result 'Kt-Result)
    (_ x-14)))))
(define
 m-foo-int
 (match-lambda**
  (('tuple-0) (match-lambda** ((this) (match-lambda** (('tuple-0) (match-lambda** (('tuple-0) this)))))))))
(define
 m-bar-Box
 (match-lambda**
  (((list 'tuple-1 tv-a))
   (match-lambda**
    ((this)
     (match-lambda**
      (('tuple-0)
       (match-lambda**
        (('tuple-0)
         (match
          (app (app -thd tv-a) (match this ((list 'tuple-2 _ (list 'tuple-1 x-3)) x-3)))
          ((list 'tuple-3 _ x-5 (list x-4)) (app (app (app x-4 x-5) 'tuple-0) 'tuple-0))))))))))))
(define
 f-doWork
 (match-lambda**
  (('tuple-0)
   (match-lambda**
    (((list 'tuple-1 x))
     (app
      (app
       (app
        (app
         m-bar-Box
         (list
          'tuple-1
          (list
           'tuple-3
           'Kt-int
           'Kt-int
           (match-lambda**
            ((x-13)
             (list
              'tuple-3
              (app '-sig (list (app 'Km-foo (list 'tuple-2 'tuple-0 (list 'tuple-1 'Kt-int)))))
              x-13
              (list (app m-foo-int 'tuple-0))))))))
        (app
         (match-lambda**
          ((x-11)
           (match
            x-11
            ((list 'tuple-3 _ x-12 _)
             (if
              (equal? (app 'Kt-Box (list 'tuple-1 'Kt-int)) (app -fst x-12))
              x-12
              (error
               'ERROR
               "Cannot cast value ~a to struct type ~a with runtime type ~a"
               x-11
               "Box(int)"
               (app 'Kt-Box (list 'tuple-1 'Kt-int))))))))
         x))
       'tuple-0)
      'tuple-0))))))
(match
 (app
  (app
   (app
    (app
     m-bar-Box
     (list
      'tuple-1
      (list
       'tuple-3
       'Kt-int
       'Kt-int
       (match-lambda**
        ((x-16)
         (list
          'tuple-3
          (app '-sig (list (app 'Km-foo (list 'tuple-2 'tuple-0 (list 'tuple-1 'Kt-int)))))
          x-16
          (list (app m-foo-int 'tuple-0))))))))
    (list 'tuple-2 (app 'Kt-Box (list 'tuple-1 'Kt-int)) (list 'tuple-1 1)))
   'tuple-0)
  'tuple-0)
 (x1
  (match
   (app
    (app f-doWork 'tuple-0)
    (list
     'tuple-1
     (app
      (match-lambda** ((x-18) (list 'tuple-3 (app '-sig (list)) x-18 (list))))
      (list 'tuple-2 (app 'Kt-Box (list 'tuple-1 'Kt-int)) (list 'tuple-1 2)))))
   (x2 (match (-print-string "~a" (list 'tuple-2 'Kt-Result (list 'tuple-2 x1 x2))) (_ (void)))))))
