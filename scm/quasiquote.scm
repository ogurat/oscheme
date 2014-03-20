;#lang racket

(define (qq x y)
  (list
   `(a b ,(+ x y))
   `(a b ((q ,(+ x y))))
   `,x
   `x))

(define (qq2 name)
   `(list ,name ',name))



(define (qq5)
  `(a ,(+ 1 2) ,@(map abs '(4 5 6)) b))
(define (qq51)
  `(,@(map abs '(4 5 6))))


(define (qq55)
  `((foo ,(- 10 3)) ,@(cdr '(c)) . ,(car '(cons)))) ; ((foo 7) . cons)



  

(define (qq6)
  `(a `(b ,(+ 1 2) ,(foo ,(+ 1 3) d) e) f))

(define (qq7 name1 name2)
  `(a `(b ,,name1 ,',name2 d) e))

(define (qq71)
  `(1 ```,,@,,@(list (+ 1 2)) 4))

; error 
(define (qq8 x y)
  `(a ,(+ 1 2) ,@(+ x y) b))

(define (qq10 x)
  `(7 ,@(list x)))

(define (qq11 a)
  `(,`(,a b))) ; '((a b))

#;(define (qq12)
  `(,,(a b))
  )

#;(define (quasi7)
  `,@(list 1 2)
  )


(define (qq12 x)
  (quasiquote (a b ((c unquote x)))))

(define (qq12_2)
  (quasiquote (a b ((c unquote (+ 1 2))))))
#;(define (qq12_3)
  (quasiquote (a b ((c unquote x y))))
  )



(define (qq13 x)
  (quasiquote (foo (unquote-splicing x))))

(define (qq13_ x)
  `(foo ,@x))

(define (qq13__ x)
  `(foo . ,x)) ; (foo . x)

#;(define (qq14 x)
  (quasiquote (foo unquote-splicing x))
  )

#;(define (qq15 x y)
  (quasiquote (foo (unquote-splicing x y)))
  )
