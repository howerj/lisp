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
  (def atom (fn (x) eq (type x) (type (cons nil nil))))
  (def len (fn (l) if l (add 1 (len (cdr l))) 0))
  (def not (fn (x) if x nil t))
  (def null (fn (x) if x nil t))
  (def id (fn (x) car (cons x ())))
  (def list (fn _ id _))
  (def odd? (fn (x) eq 1 (and x 1)))
  (def even? (fn (x) eq 0 (and x 1)))
  (def #bits 0)
  (set #bits (add 1 ((fn (x) do (more x 0) pgn (set x (lls x 1)) (set #bits (add #bits 1))) 1)))
  (def #bytes (div #bits 8))
  (def #min (lls 1 (sub #bits 1)))
  (def #max (xor -1 #min))
  'ok)

; Test Functions
;

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
  (test (len '(1 2 3 4 5)) 5)
  (test (fac 6) 720)
  (def + (fn (x y) add x y))
  (test (+ 1 2) 3)
  (test ((fn (x) add x x) 16) 32)
  (test ((fn (x) mul x x) 16) 256)
  (test (odd? 1) t)
  (test (odd? 2) nil)
  (test (odd? 3) t)
  (test (odd? 4) nil)
  (test (cons '(a b c (d e f)) 'x) '((a b c (d e f)) . x))
  (if ok 'ok ok))

; Extension only functions / tests
; (eval '(add 1 2) (env))
; (evlis '(add 1 2) (env))


