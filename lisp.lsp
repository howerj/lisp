; Richard James Howe, Public Domain, 
; https://github.com/howerj/lisp
; howe.r.j.89@gmail.com
; Lisp Test Code

; Library Code
(pgn
 (def caar (fn (x) car (car x)))
 (def cadr (fn (x) car (cdr x)))
 (def cdar (fn (x) cdr (car x)))
 (def cddr (fn (x) cdr (cdr x)))
 (def atom (fn (x) eq (type x) (type (cons nil nil))))
 (def len (fn (l) if l (add 1 (len (cdr l))) 0))
 (def not (fn (x) if x nil t))
 (def null (fn (x) if x nil t))
 (def id (fn (x) car (cons x ())))
 (def odd? (fn (x) eq 1 (and x 1)))
 (def even? (fn (x) eq 0 (and x 1)))
 (def #bits 0)
 (set #bits (add 1 ((fn (x) do (more x 0) pgn (set x (lls x 1)) (set #bits (add #bits 1))) 1)))
 (def #bytes (div #bits 8))
 (def #min (lls 1 (sub #bits 1)))
 (def #max (xor -1 #min))
 'ok)

; Test Functions

(def fac (fn (n) if (eq n 1) 1 (mul n (fac (sub n 1)))))
(def facl ; loop factorial
  (fn (n) 
      if (leq n 1) 
      '1 
      ((fn (n r) pgn
	   (set r 1) 
	   (do (more n 1) 
		 pgn
		 (set r (mul r n)) 
		 (set n (sub n 1)) 
		 (id r))) 
       n 0)))

(len '(1 2 3 4 5))
(fac 6)
(odd? 1)
(odd? 2)
(odd? 3)
(cons '(a b c (d e f)) 'x)

(def + (fn (x y) add x y))
+
(+ 1 2)


(def a 10)
(do (more a 0) out (set a (sub a 1)))

((fn (x) add x x) 16)
((fn (x) mul x x) 16)

; Extension only functions / tests
; (eval '(add 1 2) (env))
; (evlis '(add 1 2) (env))




