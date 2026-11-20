;;; core.lisp — tests for the M1 evaluator, reader, printer and prelude.

;;; reader and printer
(check (prin1-to-string '(a b . c)) "(A B . C)")
(check (prin1-to-string '(quote x)) "'X")
(check (prin1-to-string "a\"b") "\"a\\\"b\"")
(check (princ-to-string "a\"b") "a\"b")
(check (prin1-to-string #\a) "#\\a")
(check (prin1-to-string #\Space) "#\\Space")
(check (prin1-to-string #(1 2 (3))) "#(1 2 (3))")
(check (prin1-to-string -42) "-42")
(check (prin1-to-string 1152921504606846975) "1152921504606846975")
(check (prin1-to-string -1152921504606846976) "-1152921504606846976")
(check '#|block #| nested |# comment|# x 'x)
(check (symbol-name 'foo-bar) "FOO-BAR")
(check (eq 'abc 'ABC) t)
(check :key :key)
(check (car '()) nil)
(check (cdr nil) nil)

;;; arithmetic
(check (+ 1 2 3) 6)
(check (- 10) -10)
(check (- 10 3 2) 5)
(check (* 6 7) 42)
(check (/ 7 2) 7/2)                    ; a ratio (M18); it used to truncate
(check (/ -7 2) -7/2)
(check (truncate 7 2) 3)
(check (mod -7 2) 1)
(check (rem -7 2) -1)
(check (< 1 2 3) t)
(check (< 1 3 2) nil)
(check (max 3 9 2) 9)
(check (min 3 9 2) 2)
(check (expt 2 10) 1024)

;;; functions
(defun fact (n) (if (< n 2) 1 (* n (fact (1- n)))))
(check (fact 10) 3628800)
(check (fact 19) 121645100408832000)

(defun fib (n) (if (< n 2) n (+ (fib (- n 1)) (fib (- n 2)))))
(check (fib 20) 6765)

(defun opt (a &optional (b 10) c) (list a b c))
(check (opt 1) '(1 10 nil))
(check (opt 1 2 3) '(1 2 3))
(defun rst (a &rest r) (list a r))
(check (rst 1 2 3) '(1 (2 3)))
(check (rst 1) '(1 nil))

(check (funcall #'+ 1 2) 3)
(check (apply #'+ 1 2 '(3 4)) 10)
(check (funcall (lambda (x) (* x x)) 9) 81)
(check ((lambda (x y) (list y x)) 1 2) '(2 1))
(check (eval '(+ 1 2)) 3)

;;; closures
(defun make-counter ()
  (let ((n 0))
    (lambda () (setq n (1+ n)))))
(defvar c1 (make-counter))
(defvar c2 (make-counter))
(funcall c1) (funcall c1)
(check (funcall c1) 3)
(check (funcall c2) 1)

(defun adder (n) (lambda (x) (+ x n)))
(check (mapcar (adder 10) '(1 2 3)) '(11 12 13))

;;; tail calls run in constant space
(defun count-down (n) (if (= n 0) 'done (count-down (1- n))))
(check (count-down 1000000) 'done)
(defun even-p (n) (if (= n 0) t (odd-p (1- n))))
(defun odd-p (n) (if (= n 0) nil (even-p (1- n))))
(check (even-p 100001) nil)

;;; deep non-tail recursion uses the Lisp stack, not the C stack
(defun build (n) (if (= n 0) nil (cons n (build (1- n)))))
(check (length (build 10000)) 10000)

;;; special forms
(check (if nil 1 2 3) 3)                ; else is an implicit PROGN
(check (if t 1 2) 1)
(check (cond ((= 1 2) 'a) ((= 1 1) 'b) (t 'c)) 'b)
(check (cond ((+ 1 2))) 3)
(check (cond (nil 1)) nil)
(check (and 1 2 3) 3)
(check (and 1 nil 3) nil)
(check (and) t)
(check (or nil 2 3) 2)
(check (or) nil)
(check (let ((a 1) (b 2)) (+ a b)) 3)
(check (let ((a 1)) (let ((a 2) (b a)) b)) 1)
(check (let* ((a 1) (b (+ a 1))) (list a b)) '(1 2))
(check (let (x) x) nil)
(check (progn 1 2 3) 3)
(check (let ((i 0) (s 0)) (while (< i 10) (setq s (+ s i) i (1+ i))) s) 45)

;;; macros and backquote
(defmacro swap (a b) (let ((tmp (gensym))) `(let ((,tmp ,a)) (setq ,a ,b) (setq ,b ,tmp))))
(check (let ((x 1) (y 2)) (swap x y) (list x y)) '(2 1))
(check (let ((x 5) (l '(a b))) `(x ,x ,@l end)) '(x 5 a b end))
(check (let ((x 5)) `(a . ,x)) '(a . 5))
(check `(1 ,@nil 2) '(1 2))
(check (when t 1 2) 2)
(check (unless t 1) nil)
(check (case 'b (a 1) ((b c) 2) (t 3)) 2)
(check (case 'z (a 1) (otherwise 3)) 3)
(defun repeat-call () (let ((n 0)) (dotimes (i 3) (when (evenp i) (incf n))) n))
(check (repeat-call) 2)
(check (repeat-call) 2)                 ; displaced macros still work the second time

;;; lists
(check (append '(1 2) '(3) nil '(4 5)) '(1 2 3 4 5))
(check (reverse '(1 2 3)) '(3 2 1))
(check (nth 2 '(a b c d)) 'c)
(check (last '(1 2 3)) '(3))
(check (member 2 '(1 2 3)) '(2 3))
(check (assoc "b" '(("a" . 1) ("b" . 2))) '("b" . 2))
(check (remove-if #'evenp '(1 2 3 4 5)) '(1 3 5))
(check (reduce #'+ '(1 2 3 4)) 10)
(check (list* 1 2 '(3 4)) '(1 2 3 4))
(check (let ((l nil)) (push 1 l) (push 2 l) (list (pop l) l)) '(2 (1)))
(check (let ((l (list 1 2 3))) (setf (cadr l) 'x) l) '(1 x 3))
(check (equal '(1 (2 #(3 "x"))) '(1 (2 #(3 "x")))) t)
(check (equal "abc" "abd") nil)

;;; symbols and properties
(putprop 'apple 'red 'color)
(check (get 'apple 'color) 'red)
(setf (get 'apple 'color) 'green)
(check (get 'apple 'color) 'green)
(check (boundp (gensym)) nil)
(check (eq (intern "HELLO") 'hello) t)
(check (eq (make-symbol "HELLO") 'hello) nil)

;;; strings, characters, vectors
(check (string-append "foo" "-" "bar") "foo-bar")
(check (substring "hello world" 6) "world")
(check (aref "abc" 1) #\b)
(check (char-code #\A) 65)
(check (code-char 97) #\a)
(check (string 'abc) "ABC")
(let ((v (make-vector 3 0)))
  (setf (aref v 1) 'x)
  (check v #(0 x 0)))
(check (length #(1 2 3)) 3)
(check (length "four") 4)

(report "core")
