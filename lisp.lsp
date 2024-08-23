; Richard James Howe, Public Domain, 
; https://github.com/howerj/lisp
; howe.r.j.89@gmail.com
; Lisp Test Code

; Library Code
'(Loading Lush Lisp Library...)
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
  (def inc (fn (_) add _ 1))
  (def dec (fn (_) add _ -1))
  (def bool (fn (_) if (eq _ 0) nil t))
  (def atom (fn (x) neq (type x) (type (cons nil nil))))
  (def invert (fn (_) bxor _ -1))
  (def negate (fn (_) mul _ -1))
  (def cons? (fn (x) not (atom x)))
  (def signum (fn (_) if (more _ 0) 1 (if (less _ 0) -1 0)))
  (def error? (fn (x) eq '! x))
  (def null (fn (x) if x nil t))
  (def id (fn (x) car (cons x nil)))
  (def list (fn _ id _))
  (def odd? (fn (x) eq 1 (band x 1)))
  (def even? (fn (x) eq 0 (band x 1)))
  (def abs (fn (_) if (less _ 0) (negate _) (id _)))
  (def #bits 0)
  (set #bits (add 1 ((fn (x) do (more x 0) pgn (set x (lls x 1)) (set #bits (add #bits 1))) 1)))
  (def #bytes (div #bits 8))
  (def #min (lls 1 (sub #bits 1)))
  (def #max (bxor -1 #min))
  (def extension (cons? (set env))) ; Test for function that only exists in the extension code
  (def copy (fn (l) if (atom l) l (cons (car l) (copy (cdr l))))) ; create a copy of list
  (def log (fn (n b) pgn (set r 0) (do (bool n) pgn (set r (inc r)) (set n (div n b)) (id r))))
  (def popcnt (fn (n) pgn (set r 0) (if (eq 0 n) 0 (do (bool n) pgn (set r (if (neq 0 (band n 1)) (inc r) (id r))) (set n (lrs n 1)) (id r)))))
  (def reverse 
     (fn (x) 
         pgn
         (def _ nil)
         (do
           (cons? x)
           pgn
           (set _ (cons (car x) _))
           (set x (cdr x))
           (id _))))
  (def length
     (fn (x) 
         pgn
         (def _ 0)
         (do
           (cons? x)
           pgn
           (set x (cdr x))
           (set _ (add _ 1)))
         (id _)))
  (def memb 
     (fn (v l)
         do 
         (if (cons? l) (neq v (car l)) nil)
         pgn
         (set l (cdr l))
         (id l)))
  (def equal
     (fn (a b)
         if (atom a) (if (atom b) (eq a b) nil)
         (if (atom b) nil
         (if (equal (car a) (car b)) (equal (cdr a) (cdr b)) nil))))
  (def nthcar
     (fn (n l)
         if (leq n 0) l 
         (do
           (more n 0)
           pgn
           (set n (add n -1))
           (set l (if (cons? l) (cdr l) nil)))))

  (def nth (fn (n l) pgn (set l (nthcar n l)) (if (cons? l) (car l) nil)))
  (def append 
     (fn (x y)
	 if (null x) y
	 (cons (car x) (append (cdr x) y))))
  (def flatten
     (fn (l)
	 if (null l) l
	 (if (atom l) (list l)
	   (append (flatten (car l)) (flatten (cdr l))))))

  (def history ())
  (def history? (fn (n) nth n history))
  (def history-push (fn (e) set history (cons e history)))
  (def history-pop (fn () if history (set history (cdr history)) nil))
  (def history-clear (fn () set history ()))
  'ok)

'BIST ; A set of Built In Self Tests
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
  (test (equal '(1 2 3) '(1 2 3)) t)
  (test (equal '(1 2 3) '(1 2 2)) nil)
  (test (equal 'a 'a) t)
  (test (equal 'a 'b) nil)
  (test (equal '(a (b) c) '(a (b) c)) t)
  (test (equal '(a (b) c) '(a (x) c)) nil)
  (test (equal '(a (b) c) '1) nil)
  (test (equal (memb 'x '(a b c x y z)) '(x y z)) t)
  (test (equal (memb 'x '(a b c y z)) '()) t)
  (test (nth 0 '(1 2 3)) 1)
  (test (nth 1 '(1 2 3)) 2)
  (test (nth 2 '(1 2 3)) 3)
  (test (nth 3 '(1 2 3)) nil)
;  (test (equal (mapcar (fn (_) mul _ _) '(1 -2 3 4)) '(1 4 9 16)) t)
  (test (equal (flatten '()) '()) t)
  (test (equal (flatten '(a)) '(a)) t)
  (test (equal (flatten '(1 2 3)) '(1 2 3)) t)
  (test (equal (flatten '(1 (2) 3)) '(1 2 3)) t)
  (test (equal (flatten '((1) ((2) 3))) '(1 2 3)) t)
  (test (log 1 10) 1)
  (test (log 10 10) 2)
  (test (log 100 10) 3)
  (test (log 101 10) 3)
  (test (log 999 10) 3)
  (test (log 1000 10) 4)
  (test (popcnt #min) 1)
  (test (popcnt #max) (dec #bits))
  (test (popcnt 0) 0)
  (test (popcnt 1) 1)
  (test (popcnt -1) #bits)
  (test (popcnt 3) 2)
  (if (neq ok '!) 'ok ok))

; TODO: COND, AND, OR, MAP, META, test EVLIS, more tests
; TODO: exp

(def fold ; TODO: Work for `list` ?
     (pgn
       (set f (gensym))
       (set l (gensym))
       (set r (gensym))
       (eval (expand @(fn (,f ,l)
       if (atom ,l) ,l
       (if (atom (cdr ,l)) (car ,l)
        (pgn
         (set ,r (car ,l))
         (set ,l (cdr ,l))
         (do
          (cons? ,l)
          pgn
          (set ,r (eval (list ,f ,r (car ,l))))
          (set ,l (cdr ,l))
          (id ,r)))))))))

(if extension
  (pgn 
    (def apply (pgn
         (set func (gensym))
         (set args (gensym))
         (eval (expand @(fn (,func ,args) eval (cons ,func ,args))))))
    (def mapcar
     (pgn
       (set f (gensym))
       (set l (gensym))
       (set r (gensym))
       (eval (expand @(fn (,f ,l)
         pgn
         (set ,r nil)
         (do
           (cons? ,l)
           pgn
           (set ,r (cons (eval (list ,f (car ,l))) ,r))
           (set ,l (cdr ,l))
           (id ,r)))))))
    (def + (fn _ apply add _))
    (def * (fn _ apply mul _))
    (def / (fn _ apply div _))
    (def - (fn _ apply sub _))
    (def = (fn _ apply eq _))
    (def & (fn _ apply band _))
    (def | (fn _ apply bor _))
    (def ^ (fn _ apply bxor _))
    (def nl (fn () put 10))
    (def tab (fn () put 9))
    (def space (fn () put 32))
    (def prompt (fn () pgn (nl) (put '>) (space) nil))
    (def read (fn () in))
    (def write (fn (_) out _))
    (def print (fn (_) pgn (set _ (write _)) (nl) (bool _)))
    (def writes (fn _ do (cons? _) pgn (out (car _)) (set _ (cdr _)) t))
    (def ansi t) ; (def ansi (eq (getenv 'COLOR) 'ON))
    (def csi (fn () pgn (put 27) (put 91)))
    (def reset (fn () if ansi (pgn (csi) (put 48) (put 109) t) t))
    (def colors 
         '(
           (black . 0) (red . 1) (green . 2) 
           (yellow . 3) (blue . 4) (magenta . 5)
           (cyan . 6) (white . 7)))
    (def color 
         (fn (c back bright) 
             if ansi
             (pgn 
               (csi) 
               (if bright (put 49) (put 48)) (put 59) 
               (if back (put 52) (put 51)) (put (add 48 (cdr (assoc c colors))))
               (put 109) t) t))
    (def black   (fn () color 'black nil t))
    (def red     (fn () color 'red nil t))
    (def green   (fn () color 'green nil t))
    (def yellow  (fn () color 'yellow nil t))
    (def blue    (fn () color 'blue nil t))
    (def magenta (fn () color 'magenta nil t))
    (def cyan    (fn () color 'cyan nil t))
    (def white   (fn () color 'white nil t))
    
    (def colorize
         (fn (_)
             pgn
             (if (null _) (red) nil)
             (if (eq (type _) (type 1)) (blue) nil)
             (if (eq (type _) (type 'a)) (yellow) nil)
             (if (eq _ t) (green) nil)
             (if (eq (type _) (type add)) (magenta) nil)
             (if (eq (type _) (type id)) (cyan) nil)
             (write _)
             (reset)
             t))
    
    (def spaces (fn (_) do (more _ 0) pgn (space) (set _ (dec _))))
    (def lpar (fn () put 40))
    (def rpar (fn () put 41))
    (def double (fn (_) add _ _))
    
    (def _pretty 
         (fn (n d)
             if (atom n) (colorize n)
             (pgn
               (nl)
               (spaces (double d))
               (lpar)
               (nl)
               (spaces (double (inc d)))
               (do
                 (cons? n)
                 pgn
                 (_pretty (car n) (inc d))
                 (set n (cdr n)))
	       (if (null n) t (pgn (put 46) (space) (_pretty n (inc d))))
               (nl)
               (spaces (double d))
               (rpar)
               (nl)
               (spaces (double d))
               t)))
    
    (def pp (fn (n) _pretty n 0))
    (def pretty (fn (n) _pretty n 0))
    (def history-save (fn () save '.history history))
    (def history-load (fn () if (eq '! (set history (load '.history))) (set history ()) history))
    (writes 'Progam 'Lisp) 
    (eval @(writes 'Version ,@version)) (nl)
    (writes 'Email email) (nl)
    (writes 'Repo repo) (nl)
    (writes 'License license) (nl)
    (writes 'REPL)
    (def repl 
     (pgn
       (set line (gensym))
       (set running (gensym))
       (eval (expand @(fn ()
	 pgn
	 (set ,line t)
	 (history-load)
	 (do
	 (neq ,line '%)
	 pgn
	 (gc)
	 (prompt)
	 (set ,line (read))
	 (if (neq ,line (eof)) (history-push ,line) nil)
	 (pretty (eval ,line)))
	 (history-save))))))
    (repl)
    t)
  'ok)
