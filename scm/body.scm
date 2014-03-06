

(define (a1 x y)
  (+ x y)
  (* x y))

(define a2
  (lambda (x y)
    (+ x y)
    (* x y)))

(define (a3 x y)
  (if (> x y)
      (begin (+ x y))
      (begin (* y y) (* x x) 'q)))

(define (a4 x y)
  (cond
    ((= x 1) 'a 'b 'c)
    ((= y 1) 'x 'y)
    ((= x 0) (
    (else 'f 'g 'h)))

(define (a5 x y)
  (let ((a x) (b y))
    'a 'b (+ a b)))

(define (fib n)
  (let iter ((a 1) (b 0) (count n))
    'a 
    (if (= count 0)
        b
        (iter (+ a b) a (- count 1)))))


(define (a7 x y)
  (letrec
      ((f (lambda (x y) 'a 'b (* x y)))
       (g (lambda (x y) 'x 'y (+ x y))))
    'a 'b 'c
    ((if (< x y) f g) x y)))

(define (test)
  (a4 1 2)
  (a3 1 2)
  (fib 10)
  (a7 10 6)
  (a7 5 9)
  (begin (a1 1 2) (a1 6 5) (a1 5 6))
  )

;(test)
