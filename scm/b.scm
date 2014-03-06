
#;(define (eq? x y)
  (= x y))

#;(define (eqv? x y)
  (= x y))

#;(define (equal? x y)
  (= x y))

#;(define (f a b . c)
  (+ a b))


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


(define (a5 x y)
  (define (f1 i j) (let ((i (+ i j))    (j (* i j))) (plus i j)))
  (define (f2 i j) (let ((a (+ i j))    (b (* i j))) (plus a b)))
  (define (f3 i j) (let ((a (+ i j))    (b (* i j))) (* a b)))
  (define (f4 i j) (let ((a (plus i j)) (b (times i j))) (* a b)))
  (map (lambda (f) (f x y)) (list f1 f2 f3 f4)))


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
  (display (list 'true 'bb '(a b c) '(+ 1 2) (car '(a b c)) '() '(quote a) )))

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
   (list (fact x) (fact2 x) (fact3 x))))

(define (a2 x y)
  (list (+ x) (+ x y) (+ x x y) (+ x y 5 8) (+ (+ x y) (* x y))
        (* x) (* x y) (* x x y) (* x x y y)))



(begin 
  (display (a1 5))
  
  (display (let ((a (+ 10 5)) (b (* 7 8))) (+ a b))  )
  (display ((lambda (a b) (+ a b)) (+ 10 5) (* 7 8))  ) ;この2つは同じ
  (display (a5 5 6))
  (yonjo 3)
  (display (list 1 2 3 4))
  ;(display (aaa 7 6)) (display (aaa 6 7))
  
  (testequal)
  (display (xtest 3 8))
  (display (xtestt 3 8))
  (display ((aaa 7 6)))
  (display (testand))
  (testdata)
  )

(display (apply plus '(4 9)))
(display (apply plus 5 9 '()))
(display (apply cons 'a 'b '()))
(display (apply cons '(c d)))
(display (apply pair? 8 '()))

