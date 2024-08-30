; * Richard James Howe
; * License Public Domain,
; * Repo: <https://github.com/howerj/lisp>
; * Email: <mailto:howe.r.j.89@gmail.com>
; * Lisp Library and Test Code

; Library Code
;
; Many of these words could be (and should be) rewritten to use loops instead
; of recursion due to interpreter limitations. Another aspect is that functions
; should have all variables that can be bound, bound, when creating the lambda,
; this mainly goes for calling other functions and primitives.
'(Loading Lush Lisp Library...)
(pgn
  (def version '(1 0 0))
  (def license '0BSD)
  (def repo 'https://github.com/howerj/lisp)
  (def email 'howe.r.j.89@gmail.com)
  (def defun
     (fexpr (name args . code)
       eval (expand (cons 'def (cons name (cons (cons 'fn (cons args code)) nil)))) (env)))
  (defun caar (x) car (car x))
  (defun cadr (x) car (cdr x))
  (defun cdar (x) cdr (car x))
  (defun cddr (x) cdr (cdr x))
  (defun not (x) if x nil t)
  (defun inc (_) add _ 1)
  (defun dec (_) add _ -1)
  (defun bool (_) if (eq _ 0) nil t)
  (defun atom (x) neq (type x) (type (cons nil nil)))
  (defun invert (_) bxor _ -1)
  (defun negate (_) mul _ -1)
  (defun cons? (x) not (atom x))
  (defun signum (_) if (more _ 0) 1 (if (less _ 0) -1 0))
  (defun null (x) if x nil t)
  (defun id (x) pgn x)
  (defun list _ pgn _)
  (defun odd? (x) eq 1 (band x 1))
  (defun even? (x) eq 0 (band x 1))
  (defun abs (_) if (less _ 0) (negate _) (id _))
  (def #bits 0)
  (set #bits (add 1 ((fn (x) do (more x 0) (set x (lls x 1)) (set #bits (add #bits 1))) 1)))
  (def #bytes (div #bits 8))
  (def #min (lls 1 (sub #bits 1)))
  (def #max (bxor -1 #min))
  (def extension (cons? (set exit))) ; Test for function that only exists in the extension code
  (defun copy (l) if (atom l) l (cons (car l) (copy (cdr l)))) ; create a copy of list
  (defun log (n b) pgn (def r 0) (do (bool n) (set r (inc r)) (set n (div n b)) (id r)))
  (defun popcnt (n) 
           pgn 
           (def r 0) 
           (if (eq 0 n) 
           0 
           (do 
             (bool n) 
             (set r (if (neq 0 (band n 1)) (inc r) (id r))) 
             (set n (lrs n 1)) 
             (id r))))
  (defun gcd (n m) if (eq m 0) n (gcd m (mod n m)))
  (defun lcm (n m) div (mul n m) (gcd n m))
  (defun square (_) mul _ _)
  (defun double (_) add _ _)
  (defun reverse (x) ; reverse a list
         pgn
         (def _ nil)
         (do
           (cons? x)
           (set _ (cons (car x) _))
           (set x (cdr x))
           (id _)))
  (defun length (x) ; length of a list
         pgn
         (def _ 0)
         (do
           (cons? x)
           (set x (cdr x))
           (set _ (add _ 1)))
         (id _))
  (defun memb (v l) ; is atom `v` a list of `l`, returning rest of list
         do 
         (if (cons? l) (neq v (car l)) nil)
         (set l (cdr l))
         (id l))
  (defun equal (a b) ; are two trees equal?
         if (atom a) (if (atom b) (eq a b) nil)
         (if (atom b) nil
         (if (equal (car a) (car b)) (equal (cdr a) (cdr b)) nil)))
  (defun nthcar (n l) ; get n-th element of list
         if (leq n 0) l 
         (do
           (more n 0)
           (set n (add n -1))
           (set l (if (cons? l) (cdr l) nil))))
  (defun nth (n l) pgn (set l (nthcar n l)) (if (cons? l) (car l) nil))
  (defun append (x y) ; append to a list
         if (null x) y
         (cons (car x) (append (cdr x) y)))
  (defun flatten (l) ; flatten a list `(a (b) c)` -> `(a b c)`
         if (null l) l
         (if (atom l) (list l)
           (append (flatten (car l)) (flatten (cdr l)))))
  (defun subst (to from tree) ; substitute all `from` with `to` in tree
      if (atom tree)
      (if (eq tree from) to tree)
      (cons (subst to from (car tree))
            (subst to from (cdr tree))))
  (defun last (l) ; last element of a list
         pgn
         (def r (car l))
         (do
           (cons? l)
           (set r (car l))
           (set l (cdr l))
           (id r)))
  (def random ; XORSHIFT32, although not capped if 64-bit
       ((fn () pgn ; Form closure hiding global state `seed`
            (def seed (add nil)) ; Get pointer value of `nil`, hopefully will change each run (Address space layout randomization?)
            (set seed (if (eq seed 0) 1 seed)) ; Must never be zero, never will be
            (fn ()
                pgn
                  (set seed (bxor seed (lls seed 13)))
                  (set seed (bxor seed (lrs seed 7)))
                  (set seed (bxor seed (lls seed 17)))))))
  (defun lat? (l) ; Is List Of Atoms?
     if (null l) t
     (if (atom (car l)) (lat? (cdr l)) nil))
  (def sort ; Sort a list of numbers
     ((fn ()
         pgn
         (def sort-insert
         (fn (x l)
         if 
           (null l)
           (list x)
           (if 
             (leq x (car l))
             (cons x l)
             (cons 
               (car l)
               (sort-insert x (cdr l))))))
     (fn (l) ; Super inefficient numeric only sort
       if (null l)
         nil
         (sort-insert 
           (car l)
           (sort (cdr l)))))))
  (defun member (a lat) ; Find `a` in list of atoms
         if (atom lat) nil
         (if (eq (car lat) a)
           t (member a (cdr lat))))
  (defun remove-member (a lat)
     if (null lat) nil
     (if (eq (car lat) a) (remove-member a (cdr lat))
       (cons (car lat) (remove-member a (cdr lat)))))
  (defun list-tail (l k) ; Exclude all the elements from 0 to k in a list
       if (eq k 0) l
        (list-tail (cdr l) (sub k 1)))
  (defun list-head (l k) ; Get all the elements from 0 to k in a list
        if (eq k 0) (cons (car l) nil)
        (cons (car l) (list-head (cdr l) (sub k 1))))
  (defun sublist (l start end)
       list-tail (list-head l end) start)
  (defun random-element (l) ; pick a random element from a list
        pgn
        (def item (mod (abs (random)) (length l)))
        (car (sublist l item item)))
  (def *months* ; months of the year association list
    '((0 . January)  (1 . February)  (2 . March) 
      (3 . April)    (4 . May)       (5 . June) 
      (6 . July)     (7 . August)    (8 . September)
      (9 . October)  (10 . November) (11 . December)))
  (def *week-days* ; days of the week association list
    '((0 . Sunday)  (1 . Monday)  (2 . Tuesday) (3 . Wednesday)
      (4 . Thurday) (5 . Friday)  (6 . Saturday)))
  ; Programmers in their arrogance see how dates and times are handled and want
  ; to simplify the calendar we use, which given this bullshit seems
  ; reasonable. Perhaps we should just use Unix time in day to day conversation
  ; just so I do not have to look at this code, alternatively we could all go
  ; live under rocks.
  (defun date (z) ; Convert Unix Time to a Date List, See <https://stackoverflow.com/questions/7960318>
         pgn
         (def o z)
         (set z (div z 86400)) ; z -> Days
         (set z (add z 719468))
         (def era (div (if (meq z 0) z (sub z 146096)) 146097))
         (def doe (abs (sub z (mul era 146097))))
         (def yoe (div (add doe (negate (div doe 1460)) (div doe 36524) (negate (div doe 146096))) 365))
         (def y (add yoe (mul era 400)))
         (def doy (sub doe (add (mul 365 yoe) (div yoe 4) (negate (div yoe 100)))))
         (def mp (div (add 2 (mul 5 doy)) 153))
         (def d (sub (add 1 doy) (div (add 2 (mul mp 153)) 5)))
         (def m (add mp (if (less mp 10) 3 -9)))
         (list (add y (if (leq m 2) 1 0)) m d (div (mod o 86400) 3600) (div (mod o 3600) 60) (mod o 60)))
  (defun unix (z) ; Convert Date List `(year months days hours minutes seconds)` to Unix Time
         pgn
         (def y (car z)) (set z (cdr z))
         (def m (car z)) (set z (cdr z))
         (def d (car z)) (set z (cdr z))
         (def H (car z)) (set z (cdr z))
         (def M (car z)) (set z (cdr z))
         (def S (car z))
         (def r (add S (mul M 60) (mul H 3600)))
         (set y (sub y (if (leq m 2) 1 0)))
         (def era (div (if (meq y 0) y (sub y 399)) 400))
         (def yoe (abs (sub y (mul era 400))))
         (def doy (add (div (add (mul 153 (add m (if (more m 2) -3 9))) 2) 5) d -1))
         (def doe (add (mul yoe 365) (div yoe 4) (negate (div yoe 100)) doy))
         (add r (mul 86400 (add (mul era 146097) doe -719468))))
  ; https://en.wikipedia.org/wiki/Zeller%27s_congruence
  ; https://news.ycombinator.com/item?id=11358999
  (defun day-of-week (z)
    pgn
      (set year (car z)) (set z (cdr z))
      (set month (car z)) (set z (cdr z))
      (set day (car z)) ; Don't care about rest
      (set day 
        (add day
              (if
                (less month 3)
                (add 1 (set year (sub year 1))) ; increment year but return original year
                (sub year 2))))
      (mod (add
        (div (mul 23 month) 9)
        day
        4
        (div year 4)
        (negate (div year 100))
        (div year 400)) 7))
  (defun day? (s)
           pgn 
           (def d (date s)) 
           (cdr (assoc (day-of-week (list (car d) (cadr d) (car (cddr d)))) *week-days*)))
  (defun unique? (lat) ; is a list a set (no repeated symbols)
	 if (null lat) t
	 (if (member (car lat) (cdr lat)) nil
	   (unique? (cdr lat))))
  (defun subset (a b) ; A ⊆ B
      if (null a) t
      (if (member (car a) b)
	(subset (cdr a) b)
	nil))
  (defun eqset (a b) ; A = B
    if (subset a b) (subset b a) nil)
  (defun intersects (a b) ; (A ∩ B)?
      if (null a) nil
        (if (member (car a) b) t
        (intersects (cdr a) b)))
  (defun intersection (a b) ; A ∩ B 
      if (null a) nil
        (if (member (car a) b)
	  (cons (car a)
	      (intersection (cdr a) b))
	  (intersection (cdr a) b)))
  (defun union (a b) ; A ∪ B
      if (null a) b
      (if (member (car a) b) (union (cdr a) b)
        (cons (car a) (union (cdr a) b))))
  (defun a\b (a b) ; a \ b
      if (null a) nil
      (if (member (car a) b) (a\b (cdr a) b)
      (cons (car a) (a\b (cdr a) b))))
  (defun relative-difference (a b) a\b b a) ; b \ a
  (defun symmetric-difference (a b) union (a\b a b) (a\b b a)) ; A △ B
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
         (reverse (do
           (cons? ,l)
           (set ,r (cons (eval (list ,f (car ,l))) ,r))
           (set ,l (cdr ,l))
           (id ,r))))))))
  (def fold ; TODO: Work for `list` ? Also foldl/foldr
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
          (set ,r (eval (list ,f ,r (car ,l))))
          (set ,l (cdr ,l))
          (id ,r)))))))))
  (defun sum-of-squares l fold add (mapcar square l))
  (defun + _ apply add _)
  (defun * _ apply mul _)
  (defun / _ apply div _)
  (defun - _ apply sub _)
  (defun = _ apply eq _)
  (defun /= _ apply neq _)
  (defun & _ apply band _)
  (defun | _ apply bor _)
  (defun ^ _ apply bxor _)
  (defun ~ (_) invert _)
  (def history ())
  (def history? (fn (n) nth n history))
  (def history-push (fn (e) if (eq ! e) history (if (eq % e) history (set history (cons e history)))))
  (def history-pop (fn () if history (set history (cdr history)) nil))
  (def history-clear (fn () set history ()))
  (def counter (fn (cnt inc) pgn (fn () set cnt (add cnt inc))))
  'ok)

'BIST 
(pgn ; A set of Built In Self Tests
  (def ok t) ; Any test can set this to `!`
  (def test (fn (n v) if (neq n v) (set ok '!) ok))
  (def fac (fn (n) if (eq n 1) 1 (mul n (fac (sub n 1)))))
  (def facl ; loop factorial
    (fn (n) 
        if (leq n 1) 
        1 
        ((fn (n r) pgn
             (set r 1) 
             (do (more n 1) 
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
  (test (lcm 10 5) 10)
  (test (gcd 99 3) 3)
  (test (gcd 1 1) 1)
  (test (lcm 0 0) '!)
  (test (last '(1 2)) 2)
  (test (last '(1)) 1)
  (test (last '(1 2 3)) 3)
  (test (equal (subst 'x 'y '(x y 1 2))) '(x x 1 2))
  (test (equal (subst 'x 'y '((x) z (y))) '((x) z (x))))
  (test (lat? '(1 2 3)) t)
  (test (lat? '(1)) t)
  (test (lat? '(1 2 (3))) nil)
  (test (lat? '(1 (2) 3)) nil)
  (test (lat? '((1) 2 3)) nil)
  (test (equal (sort '(2 1 3 4)) '(1 2 3 4)) t)
  (test (equal (sort '(1)) '(1)) t)
  (test (equal (sort '(2 1)) '(1 2)) t)
  (test (unix '(1970 1 1 0 0 0)) 0)
  (test (equal (remove-member 'x '(1 2 x y x)) '(1 2 y)) t)
  (test (equal (remove-member 'q '(1 2 x y x)) '(1 2 x y x)) t)
; (test (sum-of-squares 3 4) 25)
  (if (neq ok '!) 'ok ok))

; TODO: COND, AND, OR, MAP, META, more tests, performance program, prolog?

(if extension
  (pgn 
    (defun nl () put 10)
    (defun tab () put 9)
    (defun space () put 32)
    (def prompt (fn () pgn (nl) (put '>) (space) nil))
    (defun read () in)
    (defun write (_) out _)
    (defun print (_) pgn (set _ (write _)) (nl) (bool _))
    (defun writes _ do (cons? _) (out (car _)) (set _ (cdr _)) t)
    (def ansi (eq (getenv 'COLOR) 'on)) ; (def ansi t)
    (def csi (fn () pgn (put 27) (put '[)))
    (def reset (fn () if ansi (pgn (csi) (put 48) (put 'm) t) t))
    (def colors 
         '((black . 0) (red . 1) (green . 2) 
           (yellow . 3) (blue . 4) (magenta . 5)
           (cyan . 6) (white . 7)))
    (def color 
         (fn (c back bright) 
             if ansi
             (pgn 
               (csi) 
               (if bright (put 49) (put 48)) (put 59) 
               (if back (put 52) (put 51)) (put (add 48 (cdr (assoc c colors))))
               (put 'm) t) t))
    (def black   (fn () color 'black nil t))
    (def red     (fn () color 'red nil t))
    (def green   (fn () color 'green nil t))
    (def yellow  (fn () color 'yellow nil t))
    (def blue    (fn () color 'blue nil t))
    (def magenta (fn () color 'magenta nil t))
    (def cyan    (fn () color 'cyan nil t))
    (def white   (fn () color 'white nil t))
    (defun colorize (_)
             pgn
             (if (eq (type _) (type 1)) (blue) nil)
             (if (eq (type _) (type 'a)) (yellow) nil)
             (if (eq _ t) (green) nil)
             (if (null _) (red) nil)
             (if (eq _ !) (red) nil)
             (if (eq _ %) (red) nil)
             (if (eq (type _) (type add)) (magenta) nil)
             (if (eq (type _) (type id)) (cyan) nil)
             (write _)
             (reset)
             t)
    (defun spaces (_) do (more _ 0) (space) (set _ (dec _)))
    (defun lpar () put 40)
    (defun rpar () put 41)
    (defun _pretty (n d) ; A really poor pretty printer
             if (atom n) (colorize n)
             (pgn
               (nl)
               (spaces (double d))
               (lpar)
               (nl)
               (spaces (double (inc d)))
               (do
                 (cons? n)
                 (_pretty (car n) (inc d))
                 (set n (cdr n)))
               (if (null n) t (pgn (put 46) (space) (_pretty n (inc d))))
               (nl)
               (spaces (double d))
               (rpar)
               (nl)
               (spaces (double d))
               t))
    (defun pp (n) _pretty n 0)
    (defun pretty (n) _pretty n 0)
    (def history-save (fn () save '.history history))
    (def history-load (fn () if (eq ! (set history (load '.history))) (set history ()) history))
    (writes 'Progam 'Lisp) 
    (eval @(writes 'Version ,@version)) (nl)
    (writes 'Email email) (nl)
    (writes 'Repo repo) (nl)
    (writes 'License license) (nl)
    (writes 'Nil 'is (add nil)) (nl)
    (writes 'REPL)
    (def repl 
     (pgn
       (def line (gensym))
       (def running (gensym))
       (eval (expand @(fn ()
         pgn
         (set ,line t)
         (history-load)
         (do
         (neq ,line '%)
         (gc)
         (prompt)
         (set ,line (read))
         (if (neq ,line %) (history-push ,line) nil)
         (pretty (eval ,line)))
         (history-save))))))
    (repl)
    t)
  'ok)

(def and ; TODO: Macro expansion
     (fexpr args
	    pgn
	    (set args (reverse args))
	    (def r nil)
	    (def f t)
	    (list 'pgn (do
	      (cons? args)
	      pgn
	      (set l (null (cdr args)))
	      (set n (list (if f 'ifnot 'if) (car args)))
	      (set r (if f n (append n (list r nil))))
	      (set f nil)
	      (set args (cdr args))
	      (id r)))))

(def or
     (fexpr args
	    pgn
	    (set args (reverse args))
	    (def r nil)
	    (do
	      (cons? args)
	      pgn
	      (set l (cons? (cdr args)))
	      (set r (list 'ifnot (car args) r))
	      (set args (cdr args))
	      (id r))))

; (define make-set 
;   (let
;    (_make-set ; Does the work of making a set from a list
;      (lambda (lat)
;        (cond
;          ((is-nil lat) nil)
;          ((member car.lat cdr.lat) (_make-set cdr.lat))
;          (t (cons car.lat (_make-set cdr.lat))))))
;    (lambda "make a set from a list of strings *or* numbers" (lat)
;     (_make-set sort.lat))))
; 
; (define defun
;   (flambda "define a new function" (x)
;            (let 
;              (name (car x))
;              (doc  (cadr x))   ; documentation string
;              (args (caddr x))  ; function arguments
;              (code (cadddr x))
;              (eval (list define name (list lambda doc args code)) (environment)))))
;

; (setq cond
;   (macro
;     (lambda cs
;       (if (null cs)
;           nil
;           (if (null (cdar cs))
;               (list 'ifnot (caar cs)
;                            (cons 'cond (cdr cs)))
;               (if (eq 'else (caar cs))
;                   (cons 'prog (cdar cs))
;                   (list 'if (caar cs)
;                             (cons 'prog (cdar cs))
;                             (cons 'cond (cdr cs)))))))))
; 
; (setq and
;   (macro
;     (lambda xs
;       (cond ((null xs))
;             ((null (cdr xs)) (car xs))
;             (else (list 'if (car xs)
;                             (cons 'and (cdr xs))
;                             nil))))))
; 
; (setq qquote
;   (macro
;     (lambda (x)
;       (cond ((atom x)
;               (list 'quote x))
;             ((eq 'unquote (car x))
;               (cadr x))
;             ((and (not (atom (car x)))
;                   (eq 'unquote (caar x)))
;               (list 'cons (cadar x)
;                           (list 'qquote (cdr x))))
;             ((and (not (atom (car x)))
;                   (eq 'splice (caar x)))
;               (list 'conc (cadar x)
;                           (list 'qquote (cdr x))))
;             (else
;               (list 'cons (list 'qquote (car x))
;                           (list 'qquote (cdr x))))))))
; (setq or
;   (macro
;     (lambda xs
;       (cond ((null xs) nil)
;             ((null (cdr xs)) (car xs))
;             (else @(ifnot ,(car xs)
;                           (or . ,(cdr xs))))))))
; 

 
