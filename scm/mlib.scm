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

(define-macro llet
  (lambda (args . body)
    (if (list? args)
    ;(if (pair? args)
        `((lambda ,(map car args) ,@body) ,@(map cadr args))
      ; named-let
      `(lletrec ((,args (lambda ,(map car (car body)) ,@(cdr body))))
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
          `(llet ((+value+ ,(car args)))
             (if +value+
                 +value+
                 (oor ,@(cdr args))))))))

(define-macro llet*
  (lambda (args . body) 
    (if (null? (cdr args))
        `(llet (,(car args)) ,@body)
        `(llet (,(car args)) (llet* ,(cdr args) ,@body)))))

(define-macro lletrec
  (lambda (args . body)
    (llet ((vars (map car args))
           (vals (map cadr args)))
      `(llet ,(map (lambda (x) `(,x '*undef*)) vars)
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
              `(llet ((+value+ ,(caar args)))
                 (if +value+ +value+ (ccond ,@(cdr args))))
              `(if ,(caar args)
                   (bbegin ,@(cdar args))
                   (ccond ,@(cdr args))))))))

(define-macro ccase
  (lambda (key . args)
    (if (null? args)
        '*undef*
      (if (eq? (caar args) 'else)
          `(bbegin ,@(cdar args))
          `(if (memv ,key ',(caar args))
               (bbegin ,@(cdar args))
               (ccase ,key ,@(cdr args)))))))

(define-macro ddo
  (lambda (var-form test-form . args)
    (define map-2
      (lambda (fn xs ys)
        (if (null? xs)
            '()
            (cons (fn (car xs) (car ys)) (map-2 fn (cdr xs) (cdr ys))))))
    (llet ((vars (map car var-form))
           (vals (map cadr var-form))
           (step (map cddr var-form)))
      `(lletrec ((loop 
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


(define (let1)
  (llet ((x 1) (y 2)) (+ x y)))
(define (let2)
  (llet () 'asd))
(define (let3)
  (llet () 'a 'b 'asd))

(define (aa x y)
  (aand 'a 'b (+ x y) (* x y)))

(define (cond0 x s)
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

(define (cond1 x s)
 (list
  (ccond ((eqv? x 1) 'a 'first)
         ((eqv? x 2) 'b 'second)
         (else 'else))
  #;(ccond ('(abc edf ghi) => cdr)
           (else 'else))
  #;(ccond ((assoc s '((a 1) (b 2))) => cadr)
           (else 'else))
  (ccond ((assoc s '((a 1) (b 2))))
         (else 'else))
  (ccond (#f 'first)
        (else 'else))
  #;(let ((temp 'xyz))
    (ccond ('(abc edf ghi) => (lambda (y) temp))))
  ))

(define (case1 x)
  (define (f x)
    (case (car x)
      ((a s d) 'first)
      ((f g h) 'second)
      (else 'else)))
  (list
   (ccase x
     ((2 3 5 7) 'prime)
     ((1 4 6 8 9) 'composit)
     (else 'else))
   (ccase (* 2 3)
     ((2 3 5 7) 'prime)
     ((1 4 6 8 9) 'composit))
   (f '(s d)) (f '(h i)) (f '(i a))))

