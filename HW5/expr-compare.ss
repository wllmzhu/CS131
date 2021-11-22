#lang racket
(current-namespace (make-base-namespace))

(define (default x y) 
    (list 'if '% x y))

(define (boolean_default x)
    (if x '% '(not %)))

(define (args_len x)
    (length (car (cdr x))))

(define (list_same_len? x y)
    (equal? (length x) (length y)))

(define (fst_arg? x f)
    (equal? (car x) f))

(define (fst_arg_lambda? x)
    (or (fst_arg? x 'lambda) (fst_arg? x 'λ)))

(define (concat_symbols x y)
    (string->symbol
        (string-append
            (symbol->string x)
            "!"
            (symbol->string y))))

(define (make_dict x y x?)
    (cond 
        [(and (empty? x) (empty? y))
            (hash)]
        [(equal? (car x) (car y))
            (hash-set (make_dict (cdr x) (cdr y) x?) (car x) (car y))]
        [else
            (if x?
                (hash-set (make_dict (cdr x) (cdr y) x?)
                    (car x)
                    (concat_symbols (car x) (car y)))
                (hash-set (make_dict (cdr x) (cdr y) x?)
                    (car y)
                    (concat_symbols(car x) (car y))))]))


(define (expr-compare x y)
    (cond
        [(equal? x y) 
            x]
        [(and (boolean? x) (boolean? y)) 
            (boolean_default x)]
        [(and (list? x) (list? y))
            (list_compare x y)]
        ;else if one is not list
        [else 
            (default x y)]))

(define (list_compare x y)
    (cond
        [(list_same_len? x y)
            (list_compare_same_length x y)]
        [else 
            (default x y)]))

(define (list_compare_same_length x y)
    (cond
        ;special form if
        [(and (fst_arg? x 'if) (fst_arg? y 'if))
            (list_compare_helper x y)]
        ;mismatch with if
        [(or (fst_arg? x 'if) (fst_arg? y 'if))
            (default x y)]
        ;quote
        [(or (fst_arg? x 'quote) (fst_arg? y 'quote))
            (default x y)]
        ;lambda or λ
        [(and (fst_arg_lambda? x) (fst_arg_lambda? y))
            (lambda_cases x y)]
        [else
            (list_compare_helper x y)]))

(define (list_compare_helper x y)
    (cond
        [(and (empty? x) (empty? y))
            '()]
        [(equal? (car x) (car y))
            (cons (car x) (list_compare_helper (cdr x) (cdr y)))]
        [(and (boolean? (car x)) (boolean? (car y)))
            (cons
                (boolean_default (car x)) 
                (list_compare_helper (cdr x) (cdr y)))]
        [(and (list? (car x)) (list? (car y)))
            (list_compare_cases x y)]
        [else
            (cons
                (list 'if '% (car x) (car y))
                (list_compare_helper (cdr x) (cdr y)))]))


(define (list_compare_cases x y)
    (if (list_same_len? (car x) (car y))
        (cons 
            (list_compare_same_length (car x) (car y))
            (list_compare_helper (cdr x) (cdr y)))
        (cons 
            (list 'if '% (car x) (car y)) 
            (list_compare_helper (cdr x) (cdr y)))))


(define (lambda_cases x y)
    (cond
        ;only when both lambda, we use lambda
        [(and (fst_arg? x 'lambda) (fst_arg? y 'lambda))
            (if (equal? (args_len x) (args_len y))
                (lambda_compare 'lambda (cdr x) (cdr y) '() '())
                (default x y))]
        ;else we use λ
        [else
            (if (equal? (args_len x) (args_len y))
                (lambda_compare 'λ (cdr x) (cdr y) '() '())
                (default x y))]))    

(define (lambda_compare lambda_sym x y x_dict y_dict)
    (list 
        lambda_sym
        (lambda_compare_args (car x) (car y))
        (lambda_compare_expr
            (cadr x)
            (cadr y)
            (cons (make_dict (car x) (car y) #t) x_dict)
            (cons (make_dict (car x) (car y) #f) y_dict))))


(define (lambda_compare_args x y)
    ;iterate symbol by symbol
    (cond
        [(and (empty? x) (empty? y))
            '()]
        ;if current symbol the same, keep it
        [(equal? (car x) (car y))
            (cons 
                (car x) 
                (lambda_compare_args (cdr x) (cdr y)))]
        ;else bind them
        [else
            (cons 
                (concat_symbols (car x) (car y))
                (lambda_compare_args (cdr x) (cdr y)))]))

(define (lambda_compare_expr x y x_dict y_dict)
    (let (
        [newx (if (equal? (lookup x_dict x) "failed")
                    x
                    (lookup x_dict x))]
        [newy (if (equal? (lookup y_dict y) "failed")
                    y
                    (lookup y_dict y))])
        (cond ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
            [(equal? newx newy) newx]
            [(and (boolean? x) (boolean? y))
                (default x)]
            [(and (list? x) (list? y))
                (lambda_list_cases x y x_dict y_dict)]
            ;else if at least one is not list
            [else
                (list 'if '%
                    (if (list? x)
                        (replace x x_dict #t) 
                        newx)
                    (if (list? y)
                        (replace y y_dict #t) 
                        newy))])))

                        
(define (lambda_list_cases x y x_dict y_dict)
    (if (list_same_len? x y)
        (lambda_compare_expr_list x y x_dict y_dict)
        (list 'if '% (replace x x_dict #t) (replace y y_dict #t))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (lambda_compare_expr_list x y x_dict y_dict)
    (cond
        ;special form if
        [(and (fst_arg? x 'if) (fst_arg? y 'if))
            (cons 
                'if 
                (lambda_compare_expr_list_helper (cdr x) (cdr y) x_dict y_dict))]
        ;mismatch with if
        [(or (fst_arg? x 'if) (fst_arg? y 'if))
            (list 
                'if '% 
                (replace x x_dict #t) 
                (replace y y_dict #t))]
        ;quote
        [(or (fst_arg? x 'quote) (fst_arg? y 'quote))
            (if (equal? x y) 
                x 
                (list 
                    'if '% 
                    (replace x x_dict #t) 
                    (replace y y_dict #t)))]
        ;lambda
        [(and (fst_arg_lambda? x) (fst_arg_lambda? y))
            (lambda_compare_expr_list_lambda_cases x y x_dict y_dict)]
        [else
            (lambda_compare_expr_list_helper x y x_dict y_dict)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;    
(define (lambda_compare_expr_list_helper x y x_dict y_dict)
    (if (and (empty? x) (empty? y))
        '()
        (let (
            [newx (if (equal? (lookup x_dict (car x)) "failed") 
                    (car x) 
                    (lookup x_dict (car x)))]
            [newy (if (equal? (lookup y_dict (car y)) "failed") 
                    (car y) 
                    (lookup y_dict (car y)))])
            
            (cond 
                [(and (list? newx) (list? newy))
                    (cond
                        [(list_same_len? x y)
                            (cons 
                                (lambda_compare_expr_list (car x) (car y) x_dict y_dict)
                                (lambda_compare_expr_list_helper (cdr x) (cdr y) x_dict y_dict))]
                        [else
                            (cons
                                (list 'if '% 
                                    (replace (car x) x_dict #t)
                                    (replace (car y) y_dict #t))
                                (lambda_compare_expr_list_helper (cdr x) (cdr y) x_dict y_dict))])]
                [(equal? newx newy)
                    (cons
                        newx
                        (lambda_compare_expr_list_helper (cdr x) (cdr y) x_dict y_dict))]
                [(and (boolean? (car x)) (boolean? (car y)))
                    (cons 
                        (boolean_default x)
                        (lambda_compare_expr_list_helper (cdr x) (cdr y) x_dict y_dict))]
                [(or (list? newx) (list? newy))
                    (list 'if '% 
                        (if (list? x) 
                            (replace x x_dict #t) newx) 
                        (if (list? y) 
                            (replace y y_dict #t) newy))]
                [else 
                    (cons 
                        (list 'if '% newx newy) 
                        (lambda_compare_expr_list_helper (cdr x) (cdr y) x_dict y_dict))]))))


(define (lambda_compare_expr_list_lambda_cases x y x_dict y_dict)
    (cond 
        ;only when both lambda, we use lambda
        [(and (fst_arg? x 'lambda) (fst_arg? y 'lambda))
            (if (equal? (args_len x) (args_len y))
                (lambda_compare 'lambda (cdr x) (cdr y)  x_dict y_dict)
                (list 
                    'if '% 
                    (replace x x_dict #t) 
                    (replace y y_dict #t)))]
        ;else we use λ
        [else
            (if (equal? (args_len x) (args_len y))
                (lambda_compare 'λ (cdr x) (cdr y)  x_dict y_dict)
                (list 
                    'if '% 
                    (replace x x_dict #t) 
                    (replace y y_dict #t)))]))   

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (replace x dictx-list head)
  (cond
    [(empty? x) '()]
    ; if x is a quotes list, we don't want to rename variables
    [(equal? (car x) 'quote) x]
    ; check if begin with lambda or lambda symbol
    [(and head (or (equal? (car x) 'lambda) (equal? (car x) 'λ)))
     (cons (car x) (cons (car (cdr x)) (replace (cdr (cdr x)) (cons (make_dict (car (cdr x)) (car (cdr x)) #t) dictx-list) #f)))]
    ; check if begin with 'if' 
    [(and head (equal? (car x) 'if)) (cons (car x) (replace (cdr x) dictx-list #f))]
    ; check if (car x) is a list
    [(list? (car x)) (cons (replace (car x) dictx-list #t) (replace (cdr x) dictx-list #f))]
    ; case for boolean, don't want to replace
    [(boolean? (car x)) (cons (car x) (replace (cdr x) dictx-list #f))]
    ; all other cases, we look for latest name in dictionary if there exists one
    [else (cons
           (if (equal? (lookup dictx-list (car x)) "Not Found1") (car x) (lookup dictx-list (car x)))
           (replace (cdr x) dictx-list #f))]
    ))

(define (lookup dict x)
    (cond
        [(empty? dict) 
            "failed"]
        [(equal? (hash-ref (car dict) x "failed") "failed")
            (lookup (cdr dict) x)]
        [else
            (hash-ref (car dict) x)]))


    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#|
  QUESTION 2: test-expr-compare
|#

(define (test-expr-compare x y)
  (and
   (equal? (eval x) (eval (list 'let '([% #t]) (expr-compare x y))))
   (equal? (eval y) (eval (list 'let '([% #f]) (expr-compare x y))))
   )
  )

#|
  QUESTION 3: test-expr-x and test-expr-y
|#

(define test-expr-x
  '(lambda (a b) (lambda (b) (if a (quote (a b)) (lambda (d e f) (+ d a f))))))

(define test-expr-y
  '(lambda (c d) (lambda (if) (if if '(a b) (lambda (d h) (+ d if c))))))

    
   
; (equal? (expr-compare 12 12) '12)
; (equal? (expr-compare 12 20) '(if % 12 20))
; (equal? (expr-compare #t #t) #t)
; (equal? (expr-compare #f #f) #f)
; (equal? (expr-compare #t #f) '%)
; (equal? (expr-compare #f #t) '(not %))
; (equal? (expr-compare 'a '(cons a b)) '(if % a (cons a b)))
; (equal? (expr-compare '(cons a b) '(cons a b)) '(cons a b))
; (equal? (expr-compare '(cons a lambda) '(cons a λ)) '(cons a (if % lambda λ)))
; (equal? (expr-compare '(cons (cons a b) (cons b c))
;               '(cons (cons a c) (cons a c))) '(cons (cons a (if % b c)) (cons (if % b a) c)))
; (equal? (expr-compare '(cons a b) '(list a b)) '((if % cons list) a b))
; (equal? (expr-compare '(list) '(list a)) '(if % (list) (list a)))
; (equal? (expr-compare ''(a b) ''(a c)) '(if % '(a b) '(a c)))
; (equal? (expr-compare '(quote (a b)) '(quote (a c))) '(if % '(a b) '(a c)))
; (equal? (expr-compare '(quoth (a b)) '(quoth (a c))) '(quoth (a (if % b c))))
; (equal? (expr-compare '(if x y z) '(if x z z)) '(if x (if % y z) z))
; (equal? (expr-compare '(if x y z) '(g x y z)) '(if % (if x y z) (g x y z)))
; (equal? (expr-compare '((lambda (a) (f a)) 1) '((lambda (a) (g a)) 2)) '((lambda (a) ((if % f g) a)) (if % 1 2)))
; (equal? (expr-compare '((lambda (a) (f a)) 1) '((λ (a) (g a)) 2)) '((λ (a) ((if % f g) a)) (if % 1 2)))
; (equal? (expr-compare '((lambda (a) a) c) '((lambda (b) b) d)) '((lambda (a!b) a!b) (if % c d)))
; (equal? (expr-compare ''((λ (a) a) c) ''((lambda (b) b) d)) '(if % '((λ (a) a) c) '((lambda (b) b) d)))
; (equal? (expr-compare '(+ #f ((λ (a b) (f a b)) 1 2))
;               '(+ #t ((lambda (a c) (f a c)) 1 2))) '(+
;      (not %)
;      ((λ (a b!c) (f a b!c)) 1 2)))
; (equal? (expr-compare '((λ (a b) (f a b)) 1 2)
;               '((λ (a b) (f b a)) 1 2)) '((λ (a b) (f (if % a b) (if % b a))) 1 2))
; (equal? (expr-compare '((λ (a b) (f a b)) 1 2)
;               '((λ (a c) (f c a)) 1 2)) '((λ (a b!c) (f (if % a b!c) (if % b!c a))) 1 2))

; (equal? (expr-compare '((lambda (lambda) (+ lambda if (f lambda))) 3)
;               '((lambda (if) (+ if if (f λ))) 3)) '((lambda (lambda!if) (+ lambda!if (if % if lambda!if) (f (if % lambda!if λ)))) 3))
; (equal? (expr-compare '((lambda (a) (eq? a ((λ (a b) ((λ (a b) (a b)) b a))
;                                     a (lambda (a) a))))
;                 (lambda (b a) (b a)))
;               '((λ (a) (eqv? a ((lambda (b a) ((lambda (a b) (a b)) b a))
;                                 a (λ (b) a))))
;                 (lambda (a b) (a b)))) '((λ (a)
;       ((if % eq? eqv?)
;        a
;        ((λ (a!b b!a) ((λ (a b) (a b)) (if % b!a a!b) (if % a!b b!a)))
;         a (λ (a!b) (if % a!b a)))))
;      (lambda (b!a a!b) (b!a a!b))))

; ; TA test cases posted on Piazza
; (equal? (expr-compare '(cons a lambda) '(cons a λ)) '(cons a (if % lambda λ)))
; (equal? (expr-compare '(lambda (a) a) '(lambda (b) b)) '(lambda (a!b) a!b))
; (equal? (expr-compare '(lambda (a) b) '(cons (c) b)) '(if % (lambda (a) b) (cons (c) b)))
; (equal? (expr-compare '((λ (if) (+ if 1)) 3) '((lambda (fi) (+ fi 1)) 3)) '((λ (if!fi) (+ if!fi 1)) 3))
; (equal? (expr-compare '(lambda (lambda) lambda) '(λ (λ) λ)) '(λ (lambda!λ) lambda!λ))
; (equal? (expr-compare ''lambda '(quote λ)) '(if % 'lambda 'λ))
; (equal? (expr-compare '(lambda (a b) a) '(λ (b) b)) '(if % (lambda (a b) a) (λ (b) b)))
; (equal? (expr-compare '(λ (a b) (lambda (b) b)) '(lambda (b) (λ (b) b))) '(if % (λ (a b) (lambda (b) b)) (lambda (b) (λ (b) b))))
; (equal? (expr-compare '(λ (let) (let ((x 1)) x)) '(lambda (let) (let ((y 1)) y))) '(λ (let) (let (((if % x y) 1)) (if % x y))))
; (equal? (expr-compare '(λ (x) ((λ (x) x) x))
;               '(λ (y) ((λ (x) y) x))) '(λ (x!y) ((λ (x) (if % x x!y)) (if % x!y x))))
; (equal? (expr-compare '(((λ (g)
;                    ((λ (x) (g (λ () (x x))))     ; This is the way we define a recursive function
;                     (λ (x) (g (λ () (x x))))))   ; when we don't have 'letrec'
;                  (λ (r)                               ; Here (r) will be the function itself
;                    (λ (n) (if (= n 0)
;                               1
;                               (* n ((r) (- n 1))))))) ; Therefore this thing calculates factorial of n
;                 10)
;               '(((λ (x)
;                    ((λ (n) (x (λ () (n n))))
;                     (λ (r) (x (λ () (r r))))))
;                  (λ (g)
;                    (λ (x) (if (= x 0)
;                               1
;                               (* x ((g) (- x 1)))))))
;                 9)) '(((λ (g!x)
;                     ((λ (x!n) (g!x (λ () (x!n x!n))))
;                      (λ (x!r) (g!x (λ () (x!r x!r))))))
;                   (λ (r!g)
;                     (λ (n!x) (if (= n!x 0)
;                                  1
;                                  (* n!x ((r!g) (- n!x 1)))))))
;                  (if % 10 9)))
                 
; ; my own test case, I decided to not override if/lambdas at beginning of list, both ways work
; (equal? (expr-compare '(lambda (a) (a)) '(lambda (b) (a))) '(lambda (a!b) ((if % a!b a)))) ;1
; (equal? (expr-compare '(lambda (a) a) '(lambda (b) a)) '(lambda (a!b) (if % a!b a))) ;2
; (equal? (expr-compare '(lambda (a b) a) '(lambda (c d) (+ c d))) '(lambda (a!c b!d) (if % a!c (+ a!c b!d)))) ;3
; (equal? (expr-compare '(lambda (a b c) (if a b c)) '(lambda (d e f) (lambda (c) (c d e f))))
;         '(lambda (a!d b!e c!f) (if % (if a!d b!e c!f) (lambda (c) (c a!d b!e c!f))))) ;4
; (equal? (expr-compare '(λ (a b c) (if a b c)) '(lambda (d e f) (lambda (c) (c d e f))))
;         '(λ (a!d b!e c!f) (if % (if a!d b!e c!f) (lambda (c) (c a!d b!e c!f))))) ;5
; (equal? (expr-compare '(λ (a b c) (if a b c)) '(lambda (d e f) (lambda (c d) (c d e f))))
;         '(λ (a!d b!e c!f) (if % (if a!d b!e c!f) (lambda (c d) (c d b!e c!f))))) ;6
; (equal? (expr-compare '(λ (a if c) (if a if c)) '(lambda (d e f) (lambda (c d) (c d e f))))
;         '(λ (a!d if!e c!f) (if % (if a!d if!e c!f) (lambda (c d) (c d if!e c!f))))) ;7
; (equal? (expr-compare '(lambda (a) (lambda (a) (+ a 2))) '(lambda (b) (lambda (if) (if if b c))))
;         '(lambda (a!b) (lambda (a!if) (if % (+ a!if 2) (if a!if a!b c))))) ;8
; (equal? (expr-compare '(lambda (a) (lambda (a) (+ a 2))) '(lambda (b) (lambda (if b) (if if b c))))
;         '(lambda (a!b) (if % (lambda (a) (+ a 2)) (lambda (if b) (if if b c))))) ;9
; (equal? (expr-compare '(lambda (a) (lambda (a) (+ a 2))) '(lambda (if) (lambda (b c) (if if b c))))
;         '(lambda (a!if) (if % (lambda (a) (+ a 2)) (lambda (b c) (if a!if b c))))) ;10
; (equal? (expr-compare '(lambda (a) (lambda (b c) (+ a b))) '(lambda (b) (lambda (e c d) (+ b e c))))
;         '(lambda (a!b) (if % (lambda (b c) (+ a!b b)) (lambda (e c d) (+ a!b e c))))) ;11
; (equal? (expr-compare '(lambda (a) (quote (a b))) '(lambda (b) (quote (a b))))
;         '(lambda (a!b) '(a b))) ;12
; (equal? (expr-compare '(lambda (a) (a '(a d) c d)) '(lambda (b) (b '(a b) c d)))
;         '(lambda (a!b) (a!b (if % '(a d) '(a b)) c d))) ;13
; (equal? (expr-compare '(lambda (a) (a (quote (a d)) c d)) '(lambda (b) (b '(a d) c d)))
;         '(lambda (a!b) (a!b '(a d) c d))) ;14
; (equal? (expr-compare '(lambda (a) (quote (a d))) '(lambda (b) (b '(a d) c d)))
;         '(lambda (a!b) (if % '(a d) (a!b '(a d) c d)))) ;15
; (equal? (expr-compare '(lambda (quote) (quote (quote 1))) '(lambda (a) (a (a 1))))
;         '(lambda (quote!a) (if % ''1 (quote!a (quote!a 1))))) ;16
    


