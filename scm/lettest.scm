
;(define (map f l)
;  (if (null? l)
;      '()
;      (cons (f (car l)) (map f (cdr l)))))


(define (fact2 m)
  (letrec ((iter 
            (lambda (i result)
              (if (> i m)
                  result
                  (iter (+ i 1) (* i result))))))
    (iter 1 1)))


(define (fib n)
  (let iter ((a 1) (b 0) (count n))
    (if (= count 0)
        b
        (iter (+ a b) a (- count 1)))))

(define (fib2 n)
  (letrec 
      ((iter (lambda (a b count)
               (if (= count 0)
                   b
                   (iter (+ a b) a (- count 1))))))
    (iter 1 0 n)))

(define (fib3 n)
  (define (iter a b count)
    (if (= count 0)
        b
        (iter (+ a b) a (- count 1))))
  (iter 1 0 n))

(define (fib4 n)
  (if (= n 0)
      0
      (if (= n 1)
          1
          (+ (fib4 (- n 1)) (fib4 (- n 2))))))

(define def
  (lambda (x y)
    (define (f a b) (* a b))
    (f x y)))


(define (mapsquare l)
  (let f ((x l))
    (define (sq x) (* x x))
    (if (null? x)
        '()
        (cons (sq (car x)) (f (cdr x))))))

(define (letlist x y)
  (let ((a (* x x)) (b (+ y y)) (c 5))
    (list a b c)))



(define (plus x y) (+ x y))
(define (times x y) (* x y))



(define (fourtimes x) (let ((x (+ x x))) (+ x x)))
(define (setxy x y) (begin (set! x 10) (set! y 5) (+ x y)))

(define (mapf f)
  (map f '(4 5 18 19 20)))


(define (fibs)
 (map mapf 
      (list fib fib2 fib3 fib4 (lambda (x) (* x x)))))


(begin
  (display (mapf fourtimes))
  (display (let ((a (times 2 5)) (b (plus 2 5))) (plus a b)))
  (display (let ((a 10) (b 20)) (plus a b)))
  (display (mapsquare '(1 2 3)))
  (display (letlist 3 4))
  (display (setxy 5 8) )
  (display (fact2 5)))

(display (fibs))
