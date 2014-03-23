;#lang racket

(define (qq x y)
  (list
   `(a b ,(+ x y))
   `(a b ((q ,(+ x y))))
   `,x
   `x))

(define (qq2 name)
   `(list ,name ',name))

(define (qq4 x)
  `(a (b (c (d ,(cdr x))))))
(define (qq4-2 x)
  `(a . (b (c (d ,(cdr x))))))

; racket: (a quasiquote (b (c (d ,(cdr x)))))
; gauche: (qq4-3 '(a s d))  => (a quasiquote (b (c (d (s d)))))
(define (qq4-3 x)
  `(a . `(b (c (d ,(cdr x))))))


(define (qq5)
  `(a ,(+ 1 2) ,@(map abs '(4 5 6)) b))
(define (qq5-1)
  `(,@(map abs '(4 5 6))))


(define (qq5-5)
  `((foo ,(- 10 3)) ,@(cdr '(c)) . ,(car '(cons)))) ; ((foo 7) . cons)
(define (qq5-6)
  `((foo ,(- 10 3)) ,@(cdr '(c)) ,@(car '(cons)))) ; ((foo 7) . cons)

(define (qq5-7)
  `(foo ,(- 10 3) . ,(car '(cons)))) ; (foo 7 . cons)
(define (qq5-8)
  `(foo ,(- 10 3) ,@(car '(cons)))) ; (foo 7 . cons)



  
; nest
(define (qq6)
  `(a `(b ,(+ 1 2) ,(foo ,(+ 1 3) d) e) f))

(define (qq7 name1 name2 name3)
  `(a `(b ,,name1 ,',name2 d) ,name3 e))

(define (qq7-1 name1  name3)
  `(a `(b . ,,name1 ) ,name3 e))


(define (qq7-2 name1 name2)
  `(a `(b (c . ,,name1) ,',name2 d) e))
(define (qq7-3 name1 name2)
  `(a `(b (c . ,(d . ,name1)) ,',name2 e) f))

(define (qq7-5 name1 name2)
  `(a `(b `(c . ,,,name1) ,',name2 d) e))

(define (qq7-5-2 name1 name2)
  `(a `(b `(c ,,,name1) ,',name2 d) e))

; racket: (a quasiquote (,name1))
; gauche: (qq7-6 'x) => (a quasiquote (x))
(define (qq7-6 name1)
  `(a . `(,name1)))

; racket: (a quasiquote (,x))
; gauche: unquote appeared outside quasiquote: ,'x
#;(define (qq7-7)
  `(a . `(,,'x))
  )

; racket: (a `(b quasiquote (c . ,,name1)) e)
; gauche: (qq7-8 'x) => (a `(b quasiquote (c unquote x)) e)
(define (qq7-8 name1)
  `(a `(b . `(c . ,,name1)) e))

; racket: (a `(b quasiquote (c . ,,x)) e)
; gauche: unquote appeared outside quasiquote: ,'x
#;(define (qq7-9)
  `(a `(b . `(c . ,,,'x)) e)
  )


(define (qq71)
  `(1 ```,,@,,@(list (+ 1 2)) 4))

; error 
(define (qq8 x y)
  `(a ,(+ 1 2) ,@(+ x y) b))

(define (qq10 x)
  `(7 ,@(list x)))

(define (qq11 a)
  `(,`(,a b))) ; '((a b))

; racket: unquote: not in quasiquote in: (unquote (a b))
; gauche: unquote appeared outside quasiquote: ,(a b)
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

(define (qq14 x)
  `(,@x))

; racket: unquote-splicing: invalid context within quasiquote in: unquote-splicing
; gauche: invalid unquote-splicing form in this context: ,@'x
#;(define (qq15-0)
  `(foo . ,@'x)
  )
#;(define (qq15-1)
  `(foo . (unquote-splicing 'x))
  )
#;(define (qq15-2)
  `(foo unquote-splicing 'x)
  )
#;(define (qq15-3)
  (quasiquote (foo unquote-splicing 'x))
  )

#;(define (qq15)
  (quasiquote (foo (unquote-splicing 'x 'y)))
  )

; racket: unquote-splicing: invalid context within quasiquote in: unquote-splicing
; gauche: invalid unquote-splicing form in this context: ,@'(a b)
#;(define (qq16)
  `(a . ,@'(a b))
  )

; syntax-error: malformed quasiquote: (quasiquote 'x 'y)
; gauche: : invalid unquote-splicing form in this context: ,@'(a b c)
#;(define (qq17)
  `,(quasiquote 'x 'y)
  )

(define (qq18 x y)
  `,`(,x ,y))

(define (qq19 x y)
  `(a . `(,x ,y)))

; http://togetter.com/li/134984
(define (qq20)
  `(`(,,@(list 1 2))))

; r6rs
(define (qq21 g)
  ``(foo ,,@g))

(define (qq22 x)
  ``(a b ,,x))
