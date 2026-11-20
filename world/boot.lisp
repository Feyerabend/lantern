;;; -*- Mode: Lisp -*-
;;; boot.lisp — the first Lisp code the machine runs.
;;;
;;; The kernel provides the special forms (QUOTE FUNCTION IF PROGN SETQ LAMBDA
;;; DEFUN DEFMACRO LET LET* COND AND OR WHILE) and the primitives in
;;; kernel/prims.c. Everything else starts here.
;;;
;;; This file is run by the interpreter, which has some limits: LET binds
;;; lexically even for DEFVAR'd variables, &optional defaults must be
;;; constants, and there is no BLOCK, CATCH or UNWIND-PROTECT. compiler.lisp,
;;; loaded next, compiles all of this and lifts those limits.

;;; ---- basic macros -----------------------------------------------------

(defmacro when (test &body body)
  `(if ,test (progn ,@body)))

(defmacro unless (test &body body)
  `(if ,test nil ,@body))                ; IF's else part is an implicit PROGN

(defmacro defvar (name &optional value)
  `(progn (%proclaim-special ',name)
          (if (boundp ',name) nil (set ',name ,value))
          ',name))

(defmacro defparameter (name value)
  `(progn (%proclaim-special ',name) (set ',name ,value) ',name))

(defmacro defconstant (name value)
  `(progn (set ',name ,value) ',name))

;;; ---- list accessors ------------------------------------------------------

(defun caar (x) (car (car x)))
(defun cadr (x) (car (cdr x)))
(defun cdar (x) (cdr (car x)))
(defun cddr (x) (cdr (cdr x)))
(defun caddr (x) (car (cddr x)))
(defun cdddr (x) (cdr (cddr x)))
(defun cadddr (x) (car (cdddr x)))
(defun first (x) (car x))
(defun second (x) (cadr x))
(defun third (x) (caddr x))
(defun fourth (x) (cadddr x))
(defun rest (x) (cdr x))

(defun nthcdr (n list)
  (while (and list (> n 0))
    (setq list (cdr list) n (1- n)))
  list)

(defun nth (n list) (car (nthcdr n list)))

(defun last (list)
  (while (consp (cdr list)) (setq list (cdr list)))
  list)

(defun identity (x) x)

(defun list* (&rest args)
  (if (null (cdr args))
      (car args)
      (cons (car args) (apply #'list* (cdr args)))))

;;; ---- SETF and friends ----------------------------------------------------

(defun %set-car (cell value) (rplaca cell value) value)
(defun %set-cdr (cell value) (rplacd cell value) value)

(defmacro setf (place value)
  (cond ((symbolp place) `(setq ,place ,value))
        ((eq (car place) 'car)    `(%set-car ,(cadr place) ,value))
        ((eq (car place) 'cdr)    `(%set-cdr ,(cadr place) ,value))
        ((eq (car place) 'first)  `(%set-car ,(cadr place) ,value))
        ((eq (car place) 'rest)   `(%set-cdr ,(cadr place) ,value))
        ((eq (car place) 'cadr)   `(%set-car (cdr ,(cadr place)) ,value))
        ((eq (car place) 'second) `(%set-car (cdr ,(cadr place)) ,value))
        ((eq (car place) 'cddr)   `(%set-cdr (cdr ,(cadr place)) ,value))
        ((eq (car place) 'nth)    `(%set-car (nthcdr ,(cadr place) ,(caddr place)) ,value))
        ((eq (car place) 'aref)   `(aset ,value ,(cadr place) ,(caddr place)))
        ((eq (car place) 'get)    `(putprop ,(cadr place) ,value ,(caddr place)))
        ((eq (car place) 'symbol-value)    `(set ,(cadr place) ,value))
        ((eq (car place) 'symbol-function) `(fset ,(cadr place) ,value))
        (t (error "SETF does not know how to set" place))))

(defmacro push (item place)
  `(setf ,place (cons ,item ,place)))

(defmacro prog1 (first &body body)
  (let ((g (gensym)))
    `(let ((,g ,first)) ,@body ,g)))

(defmacro pop (place)
  `(prog1 (car ,place) (setf ,place (cdr ,place))))

(defmacro incf (place &optional (delta 1))
  `(setf ,place (+ ,place ,delta)))

(defmacro decf (place &optional (delta 1))
  `(setf ,place (- ,place ,delta)))

;;; ---- iteration ------------------------------------------------------------

(defmacro dolist (spec &body body)
  (let ((tail (gensym)))
    `(let ((,tail ,(cadr spec))
           (,(car spec) nil))
       (while ,tail
         (setq ,(car spec) (car ,tail))
         ,@body
         (setq ,tail (cdr ,tail)))
       ,(caddr spec))))

(defmacro dotimes (spec &body body)
  (let ((limit (gensym)))
    `(let ((,limit ,(cadr spec))
           (,(car spec) 0))
       (while (< ,(car spec) ,limit)
         ,@body
         (setq ,(car spec) (1+ ,(car spec))))
       ,(caddr spec))))

;;; ---- mapping ---------------------------------------------------------------

(defun mapcar (f list)
  (let ((result nil))
    (dolist (x list) (push (funcall f x) result))
    (nreverse result)))

(defun mapc (f list)
  (dolist (x list) (funcall f x))
  list)

(defun mapcan (f list)
  (apply #'append (mapcar f list)))

(defun remove-if (pred list)
  (let ((result nil))
    (dolist (x list) (unless (funcall pred x) (push x result)))
    (nreverse result)))

(defun remove-if-not (pred list)
  (let ((result nil))
    (dolist (x list) (when (funcall pred x) (push x result)))
    (nreverse result)))

(defun remove (item list)
  (remove-if (lambda (x) (equal x item)) list))

(defun reduce (f list)
  (let ((acc (car list)))
    (dolist (x (cdr list)) (setq acc (funcall f acc x)))
    acc))

(defun some (pred list)
  (let ((found nil))
    (while (and list (not found))
      (setq found (funcall pred (car list)) list (cdr list)))
    found))

(defun every (pred list)
  (let ((ok t))
    (while (and list ok)
      (setq ok (funcall pred (car list)) list (cdr list)))
    ok))

;;; ---- searching ----------------------------------------------------------------

(defun memq (item list)
  (while (and list (not (eq item (car list)))) (setq list (cdr list)))
  list)

(defun member (item list)                ; EQUAL, as in Lisp Machine Lisp
  (while (and list (not (equal item (car list)))) (setq list (cdr list)))
  list)

(defun assq (key alist)
  (while (and alist (not (eq key (caar alist)))) (setq alist (cdr alist)))
  (car alist))

(defun assoc (key alist)
  (while (and alist (not (equal key (caar alist)))) (setq alist (cdr alist)))
  (car alist))

(defun copy-list (list) (append list nil))

;;; ---- CASE -------------------------------------------------------------------------

(defmacro case (key &body clauses)
  (let ((g (gensym)))
    `(let ((,g ,key))
       (cond ,@(mapcar (lambda (c)
                         (cond ((memq (car c) '(t otherwise)) `(t ,@(cdr c)))
                               ((consp (car c)) `((memq ,g ',(car c)) ,@(cdr c)))
                               (t `((eq ,g ',(car c)) ,@(cdr c)))))
                       clauses)))))

;;; ---- numbers -------------------------------------------------------------------------

(defun zerop (n) (= n 0))
(defun plusp (n) (> n 0))
(defun minusp (n) (< n 0))
(defun evenp (n) (= (rem n 2) 0))
(defun oddp (n) (not (evenp n)))
(defun abs (n) (if (< n 0) (- n) n))

(defun max (n &rest more)
  (dolist (m more) (when (> m n) (setq n m)))
  n)

(defun min (n &rest more)
  (dolist (m more) (when (< m n) (setq n m)))
  n)

(defun expt (base power)
  (let ((result 1))
    (dotimes (i power) (setq result (* result base)))
    result))
