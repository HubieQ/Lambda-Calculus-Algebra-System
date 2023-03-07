#lang lazy


;Adds, Subtracts, and Multiplies positive integers through the lambda calculus
;(TAN x) -> converts x from positive racket number to lambda calculus number
;(NAT x) -> converts x from positive lambda calculus number to racket number
;(ADD a b) -> adds a and b, which are positive lambda calculus numbers
;(SUB a b) -> subtracts b from a, provided a >= b, a,b lambda calculus numbers
;(MUL a b) -> multiplies 2 lambda calculus numbers using karatsuba's algorithm

(define True (lambda (a) (lambda (b) a)))

(define False (lambda (a) (lambda (b) b)))

(define (If a b c) ((a b) c))

(define (Or a b) (If a True b))
(define (And a b) (If a b False))
(define (Not a) (If a False True))

(define (Cons a b) (lambda (x) ((x a) b)))

(define Z (lambda (x) True))
(define (Z? x) (x (lambda (a) (lambda (b) False))))

(define (cdR x)
  (If (Z? x) Z (x False)))

(define (caR x)
  (If (Z? x) Z (x True)))

(define (ADD1 x)
   (If (Z? x)
       (Cons True Z)
       (If (caR x) (Cons False (ADD1 (cdR x)))
           (Cons True (cdR x)))))

(define (ADD a b)
  (If (Z? a) b
   (If (Z? b) a
    (If (Not (caR a)) (Cons (caR b) (ADD (cdR a) (cdR b)))
     (If (Not (caR b)) (Cons (caR a) (ADD (cdR a) (cdR b)))
       (Cons False (ADD1 (ADD (cdR a) (cdR b)))))))))

(define (TAN x)
  (If (Z? x) 0
      (If (caR x)
          (add1 (* 2 (TAN (cdR x))))
          (* 2 (TAN (cdR x))))))

(define (NAT n)
  (if (zero? n) Z (ADD1 (NAT (sub1 n)))))

(define (MULt a b)
  (If (Z? b) Z
      (If (Z? a) Z
          (If (caR b) (ADD a (MULt (Cons False a) (cdR b)) ) (MULt (Cons False a) (cdR b) )))))

(define (Xor a b) ((a ((b False) True)) ((b True) False)))

(define (SuB1 x)
  (If (caR x) (Cons False (cdR x)) (Cons True (SuB1 (cdR x)))))

(define (prune a)
  (If (Z? a) Z (If (Not (caR a)) (prune (cdR a)) a)))

(define (REVERSE a b)  (If (Z? a) b (REVERSE (cdR a) (Cons (caR a) b))))

(define (PRUNE a)  (REVERSE (prune (REVERSE a Z)) Z)) 

(define (SUB1 x) (PRUNE (SuB1 x)))

(define (Equal a b)
  (If (And (Z? a) (Z? b)) True
      (If (Or (And (Not (Z? a)) (Z? b)) (And (Z? a) (Not (Z? b)))) False
          (If (caR a) (If (caR b) (Equal (cdR a) (cdR b)) False)
              (If (caR b) False (Equal (cdR a) (cdR b)))))))

(define (SUB a b carry)
  (If (Z? a) a
      (If (And (Z? b) (Not carry)) a
          (If (And (Z? b) carry) (SUB1 a)
              (If (Not (Xor (caR a) (caR b))) (Cons carry (SUB (cdR a) (cdR b) carry)) ; 1 1 
                  (If (And carry (And (caR a) (Not (caR b)))) (Cons False (SUB (cdR a) (cdR b) False)) ; carry 1 0
                      (If (And (And (Not carry) (caR a)) (Not (caR b))) (Cons True (SUB (cdR a) (cdR b) False)) ; dont carry 1 0
                          (If (And (And (Not carry) (Not (caR a))) (caR b)) (Cons True (SUB (cdR a) (cdR b) True)) ;dont carry  0 1
                              (If (And (And carry (Not (caR a))) (caR b)) (Cons False (SUB (cdR a) (cdR b) True)) ; carry 0 1
                          `(Cons True (SUB (cdR a) (cdR b) True)))))))))))

(define (Sub a b) (PRUNE (SUB a b False)))

(define (Shift x y a) (If (Equal x y) a (Cons False (Shift (ADD1 x) y  a))))

(define (Length x) (If (Z? x) Z (ADD1 (Length (cdR x)))))

(define (COMBINE a b c d) (MUL (ADD a b) (ADD c d)))

(define (KaratsubaStep x y z bits)  (ADD (Shift Z bits x) (ADD (Shift Z (cdR bits) (Sub z (ADD x y) )) y))) ;x = ac, y = bd; z = (a+d)(b+c)

(define (Smaller? x) (If (Z?   (cdR (cdR (cdR (cdR (cdR (cdR (cdR x)))))))) True False))

(define (getRight x y n m) (If (Equal m n) y (getRight (cdR x) (Cons (caR x) y) (ADD1 n) m)))

(define (getLeft x n m) (If (Equal m n) x (getLeft (cdR x) (ADD1 n) m)))

(define (Decompose a b n) (DecomposeAgain (getLeft a Z (cdR n))  (PRUNE (REVERSE (getRight a Z Z (cdR n)) Z)) (getLeft b Z (cdR n)) (PRUNE (REVERSE (getRight b Z Z (cdR n)) Z)) n))

(define (DecomposeAgain a b c d n) (KaratsubaStep (MUL a c) (MUL b d) (COMBINE a b c d) n)) 

(define (MUL a b)
  (If  (Or (Smaller? (Length a)) (Smaller? (Length b)) ) (MULt a b)
      (PRUNE (Decompose a b  (Cons False (ADD1 (cdR (Length a)))))) ))
;remember to update smaller to add more cdrs



