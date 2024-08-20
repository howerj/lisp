; Richard James Howe, Public Domain, 
; https://github.com/howerj/lisp
; howe.r.j.89@gmail.com
; Lisp Test Code

; Library Code
(pgn
  (def version '(1 0 0))
  (def license '0BSD)
  (def repo 'https://github.com/howerj/lisp)
  (def email 'howe.r.j.89@gmail.com)
  (def caar (fn (x) car (car x)))
  (def cadr (fn (x) car (cdr x)))
  (def cdar (fn (x) cdr (car x)))
  (def cddr (fn (x) cdr (cdr x)))
  (def not (fn (x) if x nil t))
  (def atom (fn (x) neq (type x) (type (cons nil nil))))
  (def cons? (fn (x) not (atom x)))
  (def error? (fn (x) eq '! x))
  (def null (fn (x) if x nil t))
  (def id (fn (x) car (cons x nil)))
  (def list (fn _ id _))
  (def odd? (fn (x) eq 1 (and x 1)))
  (def even? (fn (x) eq 0 (and x 1)))
  (def #bits 0)
  (set #bits (add 1 ((fn (x) do (more x 0) pgn (set x (lls x 1)) (set #bits (add #bits 1))) 1)))
  (def #bytes (div #bits 8))
  (def #min (lls 1 (sub #bits 1)))
  (def #max (xor -1 #min))
  (def extension (cons? (set env))) ; Test for function that only exists in the extension code
  (def reverse 
     (fn (x) 
	 pgn
	 (set _ nil)
	 (do
	   (cons? x)
	   pgn
	   (set _ (cons (car x) _))
	   (set x (cdr x))
	   (id _))))
  (def length
     (fn (x) 
	 pgn
	 (set _ 0)
	 (do
	   (cons? x)
	   pgn
	   (set x (cdr x))
	   (set _ (add _ 1)))
	 (id _)))
  'ok)

; Test Functions
(pgn
  (def ok t)
  (def test (fn (n v) if (neq n v) (set ok '!) ok))
  (def fac (fn (n) if (eq n 1) 1 (mul n (fac (sub n 1)))))
  (def facl ; loop factorial
    (fn (n) 
        if (leq n 1) 
        1 
        ((fn (n r) pgn
             (set r 1) 
             (do (more n 1) 
                   pgn
                   (set r (mul r n)) 
                   (set n (sub n 1)) 
                   (id r))) 
         n 0)))
  (test (length '(1 2 3 4 5)) 5)
  (test (fac 6) 720)
  (test ((fn (x) add x x) 16) 32)
  (test ((fn (x) mul x x) 16) 256)
  (test (odd? 1) t)
  (test (odd? 2) nil)
  (test (odd? 3) t)
  (test (odd? 4) nil)
  (if (neq ok '!) 'ok ok))


; Extension only functions / tests
; (eval '(add 1 2) (env))
; (evlis '(add 1 2) (env))
; (def one 1)
; (def inc (eval @(fn (_) add _ ,one) (env)))
; @(1 2 ,(add 3 4))
; @(a b ,ok)
; @(1 2 ,(list 3) 4)
; @(1 2 ,@(list 3) 4)
; @(1 2 ,@(list 3 4) 5)
; @(1 2 ,@(in) 5)
;
(if extension
  (pgn 
    (def apply (fn (func args) eval (cons func args)))
    (def + (fn _ apply add _))
    (def * (fn _ apply mul _))
    (def / (fn _ apply div _))
    (def - (fn _ apply sub _))
    (def = (fn _ apply eq _))
    (def nl (fn () put 10))
    (def space (fn () put 32))
    (def prompt (fn () pgn (nl) (put '>) (space) nil))
    (def read (fn () in))
    (def write (fn (_) out _))
    (def bool (fn (_) if (eq _ 0) t nil))
    (def print (fn (_) pgn (set _ (write _)) (nl) (bool _)))
    (def repl (fn () do t pgn (prompt) (print (eval (read)))))
    'ok)
  'ok)

