#lang racket
(current-namespace (make-base-namespace))
(provide (all-defined-out))

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
        ;if one isn't any form of lambda
        [(or (fst_arg_lambda? x) (fst_arg_lambda? y))
            (list 'if '% x y)]
        ;if both not any form of lambda
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
            [(and (list? x) (list? y) (list_same_len? x y))
                (lambda_compare_expr_list x y x_dict y_dict)]
            [(equal? newx newy) newx]
            [(and (boolean? x) (boolean? y))
                (default x)]
            [(and (list? x) (list? y) (list_same_len? x y))
                (list 'if '% (replace x x_dict #t) (replace y y_dict #t))]
            ;else if at least one is not list
            [else
                (list 'if '%
                    (if (list? x)
                        (replace x x_dict #t) 
                        newx)
                    (if (list? y)
                        (replace y y_dict #t) 
                        newy))])))

                        
;(define (lambda_list_cases x y x_dict y_dict)
;    (if (list_same_len? x y)
;        (lambda_compare_expr_list x y x_dict y_dict)
;        (list 'if '% (replace x x_dict #t) (replace y y_dict #t))))

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
           (if (equal? (lookup dictx-list (car x)) "failed") (car x) (lookup dictx-list (car x)))
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

    


