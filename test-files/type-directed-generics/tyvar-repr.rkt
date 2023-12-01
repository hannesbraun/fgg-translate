;; AUTOMATICALLY GENERATED 2023-12-01 11:14:22.604363 UTC
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

(define -impl (match-lambda** ((x-7) (match x-7 (_ (error 'ERROR "no method implementation for ~a" x-7))))))
(define
 -star-ty
 (match-lambda**
  ((x-7)
   (match
    x-7
    ('Kt-Any (app '-sig (list)))
    ((list 'Kt-Box (list 'tuple-1 x-0)) (app 'Kt-Box (list 'tuple-1 x-0)))
    (_ x-7)))))
(define
 f-toAny
 (match-lambda**
  (((list 'tuple-1 tv-a))
   (match-lambda** (((list 'tuple-1 x)) (app (match-lambda** ((x-1) (app (app -thd tv-a) x-1))) x))))))
(define
 f-foo
 (match-lambda**
  (((list 'tuple-2 tv-a tv-b))
   (match-lambda**
    (((list 'tuple-1 x))
     (app
      (match-lambda**
       ((x-5)
        (match
         x-5
         ((list 'tuple-3 _ x-6 _)
          (if
           (equal? (app 'Kt-Box (list 'tuple-1 (app -fst tv-b))) (app -fst x-6))
           x-6
           (error
            'ERROR
            "Cannot cast value ~a to struct type ~a with runtime type ~a"
            x-5
            "Box(b)"
            (app 'Kt-Box (list 'tuple-1 (app -fst tv-b)))))))))
      (app
       (app
        f-toAny
        (list
         'tuple-1
         (list 'tuple-3 (app -fst tv-a) (app -fst tv-a) (match-lambda** ((x-4) (app (app -thd tv-a) x-4))))))
       (list 'tuple-1 x))))))))
(match
 (app
  (app
   f-foo
   (list
    'tuple-2
    (list
     'tuple-3
     (app 'Kt-Box (list 'tuple-1 'Kt-int))
     (app 'Kt-Box (list 'tuple-1 'Kt-int))
     (match-lambda** ((x-8) (list 'tuple-3 (app '-sig (list)) x-8 (list)))))
    (list 'tuple-3 'Kt-int 'Kt-int (match-lambda** ((x-9) (list 'tuple-3 (app '-sig (list)) x-9 (list)))))))
  (list 'tuple-1 (list 'tuple-2 (app 'Kt-Box (list 'tuple-1 'Kt-int)) (list 'tuple-1 0))))
 (x (match (-print-string "~a" x) (_ (void)))))
