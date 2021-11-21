#lang racket
(current-namespace (make-base-namespace))


(define (compare-value a b)
    (cond
        [(equal? a b) 
            a]
        [(and (boolean? a) (boolean? b)) 
            (if a '% '(not %))]
        [(and (list? a) (list? b))
            (compare_list a b)]
        [else 
            (default_diff a b)]))

(define (compare_list a b)
  (


(define (default_diff a b)
    (cons (list 'if '% a b)))

