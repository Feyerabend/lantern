;;; control.lisp — tests for M2: the compiler, closures, special variables,
;;; non-local exits and conditions.

;;; everything is compiled now
(defun sq (x) (* x x))
(check (compiled-function-p #'sq) t)
(check (compiled-function-p #'mapcar) t)        ; boot.lisp was compiled at boot
(check (compiled-function-p #'compile-function) t) ; and so was the compiler
(check (compiled-function-p (macro-function 'dolist)) t)

;;; closures: shared, mutable, nested, per-iteration
(defun make-account (balance)
  (list (lambda (n) (setq balance (+ balance n)))
        (lambda () balance)))
(let ((acc (make-account 100)))
  (funcall (car acc) 50)
  (funcall (car acc) -30)
  (check (funcall (cadr acc)) 120))

(defun nest (a) (lambda (b) (lambda (c) (list a b c))))
(check (funcall (funcall (nest 1) 2) 3) '(1 2 3))

(defun closures-per-iteration ()
  (let ((fs nil) (i 0))
    (while (< i 3)
      (let ((j i)) (push (lambda () j) fs))
      (setq i (1+ i)))
    (mapcar #'funcall fs)))
(check (closures-per-iteration) '(2 1 0))

(defun counter-pair ()
  (let ((n 0))
    (cons (lambda () (setq n (1+ n))) (lambda () n))))
(let ((p (counter-pair)))
  (funcall (car p)) (funcall (car p))
  (check (funcall (cdr p)) 2))

;;; &optional defaults may now be any expression, using earlier parameters
(defun opt2 (a &optional (b (* a 10)) (c (list a b))) (list a b c))
(check (opt2 1) '(1 10 (1 10)))
(check (opt2 1 2) '(1 2 (1 2)))
(defun rest2 (&rest xs) xs)
(check (apply #'rest2 1 2 '(3)) '(1 2 3))
(check (rest2) nil)

;;; tail calls in compiled code
(defun loop-to (n acc) (if (= n 0) acc (loop-to (1- n) (+ acc 1))))
(check (loop-to 1000000 0) 1000000)

;;; special variables
(defvar *depth* 0)
(defun get-depth () *depth*)
(defun with-depth (n) (let ((*depth* n)) (get-depth)))
(check (with-depth 5) 5)
(check *depth* 0)
(defun bump-depth (*depth*) (get-depth))       ; special parameter
(check (bump-depth 7) 7)
(check *depth* 0)
(defun set-inside () (let ((*depth* 1)) (setq *depth* 2) (get-depth)))
(check (set-inside) 2)
(check *depth* 0)

;;; catch / throw
(check (catch 'done (dotimes (i 10) (when (= i 3) (throw 'done i))) 'never) 3)
(check (catch 'outer (catch 'inner (throw 'outer 1)) 2) 1)
(defun thrower (tag) (throw tag 'thrown))
(check (catch 'x (thrower 'x)) 'thrown)
(check (catch 'x (funcall #'throw 'x 5)) 5)

;;; block / return
(check (block foo (return-from foo 1) 2) 1)
(check (dolist (x '(1 2 3 4)) (when (= x 3) (return (* x 10)))) 30)
(check (dotimes (i 100) (when (= i 7) (return i))) 7)
(check (let ((n 0)) (loop (setq n (1+ n)) (when (> n 4) (return n)))) 5)
(defun find-first (pred list) (dolist (x list) (when (funcall pred x) (return-from find-first x))))
(check (find-first #'evenp '(1 3 4 5)) 4)
(defun via-closure () (block b (mapc (lambda (x) (when (= x 2) (return-from b 'got-2))) '(1 2 3)) 'none))
(check (via-closure) 'got-2)

;;; unwind-protect
(defvar *log* nil)
(check (unwind-protect 1 (push 'cleanup *log*)) 1)
(check *log* '(cleanup))
(setq *log* nil)
(check (catch 'x (unwind-protect (throw 'x 'v) (push 'a *log*))) 'v)
(check *log* '(a))
(setq *log* nil)
(check (catch 'x
         (unwind-protect
              (unwind-protect (throw 'x 'deep) (push 'inner *log*))
           (push 'outer *log*)))
       'deep)
(check *log* '(outer inner))
(setq *log* nil)
(check (catch 'x (let ((*depth* 99)) (unwind-protect (throw 'x *depth*) (push *depth* *log*)))) 99)
(check *log* '(99))
(check *depth* 0)

;;; conditions
(check (handler-case (car 5) (error (c) 'caught)) 'caught)
(check (handler-case (+ 1 2) (error (c) 'caught)) 3)
(check (condition-message (handler-case (error "Boom" 1 2) (error (c) c))) "Boom")
(check (condition-irritants (handler-case (error "Boom" 1 2) (error (c) c))) '(1 2))
(check (handler-case (error 'my-error "custom") (other-error (c) 'wrong) (my-error (c) 'right)) 'right)
(check (handler-case (handler-case (error 'outer-kind "x") (inner-kind (c) 'inner)) (outer-kind (c) 'outer)) 'outer)
(check (ignore-errors (undefined-function-abc)) nil)
(check (ignore-errors 42) 42)
(check (handler-case (let ((*depth* 3)) (error "x")) (error (c) *depth*)) 0)
(setq *log* nil)
(check (handler-case (unwind-protect (error "x") (push 'cleaned *log*)) (error (c) 'handled)) 'handled)
(check *log* '(cleaned))
(defun deep (n) (if (= n 0) (error "bottom") (+ 1 (deep (1- n)))))
(check (handler-case (deep 1000) (error (c) (condition-message c))) "bottom")
(defun runaway2 (n) (+ 1 (runaway2 n)))
(check (handler-case (runaway2 0) (error (c) 'overflow-caught)) 'overflow-caught)

;;; compiled and interpreted code calling each other
(%interpret '(defun interp-add1 (x) (1+ x)))
(check (%interpreted-function-p #'interp-add1) t)
(check (mapcar #'interp-add1 '(1 2 3)) '(2 3 4))
(%interpret '(defun interp-calls-compiled (x) (sq x)))
(check (interp-calls-compiled 9) 81)
(check (catch 'x (%interpret '(throw 'x 'from-interpreter))) 'from-interpreter)
(compile 'interp-add1)
(check (compiled-function-p #'interp-add1) t)
(check (interp-add1 41) 42)

;;; eval goes through the compiler too
(check (eval '(let ((a 2)) (* a 21))) 42)
(check (funcall (eval '(lambda (x) (list x x))) 1) '(1 1))

;;; macros defined after boot are compiled
(defmacro my-unless (test &body body) `(if ,test nil (progn ,@body)))
(check (compiled-function-p (macro-function 'my-unless)) t)
(check (my-unless nil 1 2) 2)

(report "control")
