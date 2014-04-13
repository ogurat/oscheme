(define (square x)
  (* x x))

(define (a1 s n)
  (list 
   (list-tail s n) (list-ref s n))) 

(define (a2 s x)
  (list
   (memq x s)
   (memv x s)
   (member x s)))

(define (a3 s x)
  (list
   (assq x s)
   (assv x s)
   (assoc x s)))



(define (maptest)
  (list
   (map square '(3 4 5))
   (map + '(1 5 6) '(4 5 10))
   (map + '(2 3 4 ) '(4 5 6) '(7 8 9) '(10 11 12))
   (map list '(a s d) '(x y z) '(1 2 3 4 5) '(asd fgh jkl))))

(define (a4 s)
  (list
   (fold-right + 0 s)
   (fold + 0 s)
   (fold cons '() s)))

(define (a5 s n)
  (list
   (string? s)
   (string-length s)
   (string-ref s n)))

(define (a6 s)
  (let ((vec (list->vector s)))
    (list
     vec (vector-ref vec 1) (vector-length vec) (vector->list vec))))
