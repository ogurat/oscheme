;
; mlib.scm : micro Scheme 用ライブラリ
;
;            Copyright (C) 2009 Makoto Hiroi
;
; http://www.geocities.jp/m_hiroi/func/scheme.html

; 述語
#;(define null? (lambda (x) (eq? x '())))
#;(define not (lambda (x) (if (eq? x #f) #t #f)))

; cxxr
#;(define cadr (lambda (x) (car (cdr x))))
#;(define cdar (lambda (x) (cdr (car x))))
#;(define caar (lambda (x) (car (car x))))
#;(define cddr (lambda (x) (cdr (cdr x))))

;;; リスト操作関数

#;(define list (lambda args args))

#;(define append
  (lambda (xs ys)
    (if (null? xs)
        ys
      (cons (car xs) (append (cdr xs) ys)))))

#;(define reverse
  (lambda (ls)
    (letrec ((iter (lambda (ls a)
                     (if (null? ls)
                         a
                       (iter (cdr ls) (cons (car ls) a))))))
      (iter ls '()))))

; リストの探索
#;(define memq
  (lambda (x ls)
    (if (null? ls)
        #f
        (if (eq? x (car ls))
            ls
          (memq x (cdr ls))))))

#;(define memv
  (lambda (x ls)
    (if (null? ls)
        #f
        (if (eqv? x (car ls))
            ls
          (memv x (cdr ls))))))

; 連想リストの探索
#;(define assq
  (lambda (x ls)
    (if (null? ls)
        #f
      (if (eq? x (car (car ls)))
          (car ls)
        (assq x (cdr ls))))))

;
#;(define assv
  (lambda (x ls)
    (if (null? ls)
        #f
      (if (eqv? x (car (car ls)))
          (car ls)
        (assv x (cdr ls))))))

;;; 高階関数

; マッピング
#;(define map
  (lambda (fn ls)
    (if (null? ls)
        '()
      (cons (fn (car ls)) (map fn (cdr ls))))))

#;(define map-2
  (lambda (fn xs ys)
    (if (null? xs)
        '()
        (cons (fn (car xs) (car ys)) (map-2 fn (cdr xs) (cdr ys))))))


; フィルター
(define filter
  (lambda (fn ls)
    (if (null? ls)
        '()
      (if (fn (car ls))
          (cons (car ls) (filter fn (cdr ls)))
        (filter fn (cdr ls))))))

; 畳み込み
#;(define fold-right
  (lambda (fn a ls)
    (if (null? ls)
        a
      (fn (car ls) (fold-right fn a (cdr ls))))))

#;(define fold-left
  (lambda (fn a ls)
    (if (null? ls)
        a
      (fold-left fn (fn a (car ls)) (cdr ls)))))


;;; マクロ

(define-macro let
  (lambda (args . body)
    (if (pair? args)
        `((lambda ,(map car args) ,@body) ,@(map cadr args))
      ; named-let
      `(letrec ((,args (lambda ,(map car (car body)) ,@(cdr body))))
        (,args ,@(map cadr (car body)))))))

(define-macro aand
  (lambda args
    (if (null? args)
        #t
      (if (null? (cdr args))
          (car args)
          `(if ,(car args)
               (aand ,@(cdr args))
               #f)))))

(define-macro oor
  (lambda args
    (if (null? args)
        #f
      (if (null? (cdr args))
          (car args)
          `(let ((value+ ,(car args)))
             (if value+
                 value+
                 (oor ,@(cdr args))))))))

(define-macro let*
  (lambda (args . body) 
    (if (null? (cdr args))
        `(let (,(car args)) ,@body)
        `(let (,(car args)) (let* ,(cdr args) ,@body)))))

#;(define-macro letrec
  (lambda (args . body)
    (let ((vars (map car args))
          (vals (map cadr args)))
      `(let ,(map (lambda (x) `(,x '*undef*)) vars)
            ,@(map-2 (lambda (x y) `(set! ,x ,y)) vars vals)
            ,@body))))



(define-macro bbegin
  (lambda args
    (if (null? args)
        `((lambda () '*undef*))
        `((lambda () ,@args)))))


(define-macro ccond
  (lambda args
    (if (null? args)
        '*undef*
      (if (eq? (caar args) 'else)
          `(bbegin ,@(cdar args))
          (if (null? (cdar args))
              `(let ((value+ ,(caar args)))
                 (if value+ value+ (ccond ,@(cdr args))))
              `(if ,(caar args)
                   (bbegin ,@(cdar args))
                   (ccond ,@(cdr args))))))))

(define-macro case
  (lambda (key . args)
    (if (null? args)
        '*undef*
      (if (eq? (caar args) 'else)
          `(begin ,@(cdar args))
          `(if (memv ,key ',(caar args))
               (begin ,@(cdar args))
               (case ,key ,@(cdr args)))))))

(define-macro ddo
  (lambda (var-form test-form . args)
    (define map-2
      (lambda (fn xs ys)
        (if (null? xs)
            '()
            (cons (fn (car xs) (car ys)) (map-2 fn (cdr xs) (cdr ys))))))
    (let ((vars (map car var-form))
          (vals (map cadr var-form))
          (step (map cddr var-form)))
      `(letrec ((loop 
                 (lambda ,vars
                   (if ,(car test-form)
                       (bbegin ,@(cdr test-form))
                       (bbegin
                         ,@args
                         (loop ,@(map-2 (lambda (x y)
                                          (if (null? x) y (car x)))
                                        step
                                        vars)))))))
         (loop ,@vals)))))



(define-macro (aaa x y z)
  (define (sq x) (* x x))
  `(and x y z))

(define-macro bbb
  (lambda (x y z)
    (define (sq x) (* x x))
    `(a b c)))


(define (aa x y)
  (aand 'a 'b (+ x y) (* x y)))

(define (cond1 x s)
  (ccond ((eqv? x 1) 'a 'first)
         ((eqv? x 2) 'b 'second)
         ((assoc s '((a 1) (b 2))))
         (else 'else)))


(define (dotest x1 x2)
  (define (f1)
    (ddo ((x x1 (cdr x))
          (sum 0 (+ sum (car x))))
      ((null? x) sum)))
  (define (f2)
    (ddo ((x 0 (+ x 1))
          (y x2))
      ((= x 5) 'a 'b y)))
  (list (f1) (f2)))

(define (condtest x s)
 (list
  (ccond ((eqv? x 1) 'a 'first)
         ((eqv? x 2) 'b 'second)
         (else 'else))
  #;(ccond ('(abc edf ghi) => cdr)
           (else 'else2))
  #;(ccond ((assoc s '((a 1) (b 2))) => cadr)
           (else 'else2))
  (ccond ((assoc s '((a 1) (b 2))))
         (else 'else3))
  (ccond (#f 'first)
        (else 'else4))
  #;(let ((temp 'xyz))
    (ccond ('(abc edf ghi) => (lambda (y) temp))))
  ))
