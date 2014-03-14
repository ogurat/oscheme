
#;(define (eq? x y)
  (= x y))

#;(define (eqv? x y)
  (= x y))

#;(define (equal? x y)
  (= x y))

(define (dotest x x2)
  (define (f1)
    (do ((x x (cdr x))
         (sum 0 (+ sum (car x))))
      ((null? x) sum)
      (display x)))
  (define (f2)
    (do ((x 0 (+ x 1)) (y x2))
      ((= x 5) 'a 'b y)))
  (list (f1) (f2)))

(define varf
  (lambda (a b . c)
    c))

(define (varf2 . x)
  (apply + x))

(define (varf3 a b c)
  (define (f . x)
    x)
  (define g 
    (lambda x
      x))
  (cons (f a b c) (g a b c)))



(define c
  '(
    (b (c . d))
    
    (b (c . (d . ())))
    (b (c . (d)))
    (b (c d) . ())
    (b (c d))
    
    (b . (c . (d . ())))
    (b . (c . (d)))
    (b . (c d . ()))
    (b . (c d))
    (b c . (d . ()))
    (b c . (d))
    (b c d . ())
    (b c d)

    (b . (c . d))
    (b c . d)
    
    ))

(define (dotp a b c)
  (list
   (and a b . (c))
   (and a . (b c))
   (and . (a b c))
   (or a b . (c))
   (or a . (b c))
   (cons . (a b))
   (cons . (a . (b . ())))
   (cons a . (b))))

(define (dot)
  (cons . ('a . ('b . ()))))

(define closure
  (lambda (a b)
    (let ((f1
      (lambda (c d)
        (let ((f2
          (lambda (e f)
            (list a b c d e f))))
          f2))))
      f1)))


(define (closure2 a b)
  (define (f1 c d)
    (define (f2 e f)
      (list a b c d e f))
    f2)
  f1)

  


(define (fact nn)
  (if (= nn 1) 1 (* nn (fact (- nn 1))  )))

(define (fact2 m)
  (define (iter i result)
    (if (= i 1)
        result
        (iter (- i 1) (* i result))))
  (iter m 1))

(define (fact3 m)
  (define (iter i result)
    (if (> i m)
        result
        (iter (+ i 1) (* i result))))
  (iter 1 1))

(define (fact4 m)
  (let iter ((i m) (result 1))
    (if (= i 1)
        result
        (iter (- i 1) (* i result)))))

(define (fact5 m)
  (letrec ((iter (lambda (i result)
    (if (= i 1)
        result
        (iter (- i 1) (* i result))))))
    (iter m 1)))


(define (fib n)
  (if (= n 0)
      0
      (if (= n 1)
          1
          (+ (fib (- n 1)) (fib (- n 2))))))


(define (zeroo? n) (= n 0))

(define (plus x y) (+ x y))
(define (times x y) (* x y))
(define (square x) (* x x))
(define (less x y) (< x y))
(define (greater x y) (> x y))
(define (eq x y) (= x y))



(define (yonjo x)
  (let ((y (* x x)))
    (* y y)))

(define (ev n)
  (letrec
      ((e? (lambda (k)
                (if (zeroo? k)
                    (= 1 1)
                    (o? (- k 1)))))
       (o? (lambda (j)
                (if (zeroo? j)
                    (= 1 0)
                    (e? (- j 1))))))
    (even? n)))

(define (ev2 x)
  (define e? (lambda (n)
                  (if (zeroo? n)
                      (= 1 1)
                      (o? (- n 1)))))
  (define o? (lambda (n)
                  (if (zeroo? n)
                      (= 1 0)
                      (e? (- n 1)))))
  (even? x))

(define (aaa x1 x2)
  (define (a) (+ x1 x2))
  (define (b) (* x1 x2))
  (if (< x1 x2) a b))

(define aaa2
  (lambda (x1 x2)
    (define (a) (+ x1 x2))
    (define (b) (* x1 x2))
    (if (< x1 x2) a b)))


(define d square)

(define even? (lambda (n)
                (if (zeroo? n)
                    (= 1 1)
                    (odd? (- n 1)))))
(define odd?  (lambda (n)
                (if (zeroo? n)
                    (= 1 0)
                    (even? (- n 1)))))

#;(define (ttt)
  (display 10)
  (display 20))

(define (testequal)
  (let ((a (list 1 2 3)) (b (list 1 2 3)) (c (list 1 2 3 4)))
    (list (equal? a b) (equal? a c) (eqv? 2 2) (eq? 4 5))))



(define (xtest a b)
  ((lambda (f g)
     (list  (f a b) (g a) (g b)))
   (lambda (x y) (* x y)) (lambda (x) (* x x))))

(define (xtestt a b)
  (let ((f (lambda (x y) (* x y)))
        (g (lambda (x) (* x x)))) 
    (list (f a b) (g a) (g b))))


(define (testand)
  (list (list (and 1) (and 1 2) (and 5 6 (eqv? 5 5)) (and 8 (eqv? 5 6) 10))
  ; (display (list (an> 1) (an> 1 2) (an> 5 6 (eqv? 5 5)) (an> 8 (eqv? 5 6) 10)))
  (list (or 1) (or 1 2) (or 5 6 (eqv? 5 5))  (or (eqv? 5 6) (+ 2 5)) (or (eqv? 5 5) 10)))
  )

(define (testdata)
  (list 'true 'bb '(a b c) '(+ 1 2) (car '(a b c)) '() '(quote a) ))

(define (condtest x s)
 (list
  (cond ((eqv? x 1) 'first)
        ((eqv? x 2) 'second)
        (else 'else))
  (cond ('(abc edf ghi) => cdr)
        (else 'else2))
  (cond ((assoc s '((a 1) (b 2))) => cadr)
        (else 'else2))
  (cond ((assoc s '((a 1) (b 2))))
        (else 'else3))
  (cond (#f 'first)
        ('(x y z) => cdr)
        (else 'else4))
  (let ((temp 'xyz))
    (cond ('(abc edf ghi) => (lambda (x) temp))))

 ; (cond (#f  'first) 
 ;       (else 'else)
 ;       (#f 'last))
  ))


(define (a1 x)
  (list 
   (list (ev x) (ev2 x) (even? x) (odd? x))
   (list (fact x) (fact2 x) (fact3 x) (fact4 x))))

(define (a2 x y)
  (list (+ x) (+ x y) (+ x x y) (+ x y 5 8) (+ (+ x y) (* x y))
        (* x) (* x y) (* x x y) (* x x y y)))


(define (a5 x y)
  (define (f1 i j) (let ((i (+ i j)) (j (* i j))) (plus i j)))
  (define (f2 i j) (let ((a (+ i j)) (b (* i j))) (times a b)))
  (define (f3 i j) (let ((a (+ i j)) (b (* i j))) (* a b)))
  (define (f4 i j) (let ((a (plus i j)) (b (times i j))) (* a b)))
  (map (lambda (f) (f x y)) (list f1 f2 f3 f4)))

(define (a6)
  (list
   (apply plus '(4 9))
   (apply plus 5 9 '())
   (apply cons 'a 'b '())
   (apply cons '(c d))
   (apply pair? 8 '())))


(begin
  (display (dotest '(1 2 3 4) 'c))
  (display (list (a1 5) (a1 4) (a2 4 5) (a5 10 5)))
  (display (let ((a (+ 10 5)) (b (* 7 8))) (+ a b))  )
  (display ((lambda (a b) (+ a b)) (+ 10 5) (* 7 8))  ) ;この2つは同じ
  (yonjo 3)
  (display (list (varf 'a 'b 'c 'd 'e) (varf2 1 2 3 4) (varf3 'a 'd 'g)))
  (display c)

  (display (((closure 'a 'b) 'c 'd) 'e ' f))
  ;(display (aaa 7 6)) (display (aaa 6 7))
  
  (testequal)
  (display (xtest 3 8))
  (display (xtestt 3 8))
  (display ((aaa 7 6)))
  (display (testand))
  (testdata)
  )

