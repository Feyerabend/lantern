;;; cl7.lisp -- tests for M17: Common Lisp, part 6 (multiple dispatch,
;;; EQL specializers, unbound slots, CHANGE-CLASS and redefined classes,
;;; WITH-SLOTS, the remaining forms, documentation, TRACE and TIME, the
;;; clock, keyword names).
;;;
;;; The first part is the done-test: a small program in ordinary Common Lisp
;;; style. It keeps its records in classes, redefines one while instances of
;;; it exist, changes the class of a record, dispatches on two arguments,
;;; traces and times its own functions, and reads the documentation of what
;;; it defines.

;;; ---- the program -----------------------------------------------------------------

(defpackage :library (:use :common-lisp)
  (:export #:item #:book #:film #:lend #:due #:title #:catalogue #:describe-loans))
(in-package :library)

(defclass item ()
  ((title :initarg :title :reader title :documentation "What the item is called.")
   (borrower :initform nil :accessor borrower))
  (:documentation "Anything the library lends."))

(defclass book (item) ((pages :initarg :pages :reader pages)))
(defclass film (item) ((minutes :initarg :minutes :reader minutes)))

(defclass member () ((name :initarg :name :reader name)))
(defclass student (member) ())

(defgeneric loan-days (who what)
  (:documentation "How long WHO may keep WHAT.")
  (:method ((who member) (what item)) 21)
  (:method ((who student) (what book)) 42)
  (:method ((who member) (what film)) 7))

(defun lend (item who)
  "Lend ITEM to WHO; => the number of days it may be kept."
  (setf (borrower item) who)
  (loan-days who item))

(defun catalogue (items)
  (mapcar (lambda (i) (with-slots (title borrower) i
                        (list title (if borrower (name borrower) :in))))
          items))

(defparameter *items* (list (make-instance 'book :title "Dune" :pages 412)
                            (make-instance 'film :title "Metropolis" :minutes 153)
                            (make-instance 'book :title "Emma" :pages 474)))

(in-package :cl-user)

(defvar *ann* (make-instance 'library::student :name "Ann"))
(defvar *bo* (make-instance 'library::member :name "Bo"))
(check (list (library:lend (first library::*items*) *ann*)
             (library:lend (second library::*items*) *ann*)
             (library:lend (third library::*items*) *bo*))
       '(42 7 21))
(check (library:catalogue library::*items*) '(("Dune" "Ann") ("Metropolis" "Ann") ("Emma" "Bo")))

;; the class gets a new slot while the books exist: they take it on, with its initform
(defclass library:book (library:item)
  ((library::pages :initarg :pages :reader library::pages)
   (library::shelf :initform :fiction :accessor library::shelf)))
(check (mapcar #'library::shelf (remove-if-not (lambda (i) (typep i 'library:book)) library::*items*))
       '(:fiction :fiction))
(check (library::pages (first library::*items*)) 412)

;; a book becomes a film: the slots both have keep their values
(let ((emma (third library::*items*)))
  (change-class emma 'library:film :minutes 129)
  (check (list (library:title emma) (library::minutes emma) (slot-exists-p emma 'library::pages)
               (library:lend emma *ann*))
         '("Emma" 129 nil 7)))

(check (documentation 'library:lend 'function) "Lend ITEM to WHO; => the number of days it may be kept.")
(check (documentation 'library::loan-days 'function) "How long WHO may keep WHAT.")
(check (documentation 'library:item 'type) "Anything the library lends.")

(check (with-output-to-string (*standard-output*)
         (trace library:lend)
         (library:lend (first library::*items*) *bo*)
         (untrace library:lend))
       "0: (LIBRARY:LEND #<LIBRARY:BOOK> #<MEMBER>)
0: LIBRARY:LEND returned 21
")
(check (let ((out (with-output-to-string (*trace-output*) (time (library:catalogue library::*items*)))))
         (and (search " ms, " out) (search "collection" out) t))
       t)

;;; ---- multiple dispatch and EQL specializers ---------------------------------------

(defgeneric meet (a b))
(defmethod meet ((a integer) (b integer)) :numbers)
(defmethod meet ((a integer) (b t)) :number-and-thing)
(defmethod meet ((a t) (b string)) :thing-and-string)
(defmethod meet ((a (eql :hello)) b) (list :greeting b))
(defmethod meet ((a symbol) (b (eql 0))) :symbol-and-zero)
(check (meet 1 2) :numbers)
(check (meet 1 "s") :number-and-thing)            ; the first argument decides first
(check (meet 'x "s") :thing-and-string)
(check (meet :hello 5) '(:greeting 5))
(check (meet 'x 0) :symbol-and-zero)
(check (meet :hello 0) '(:greeting 0))            ; EQL beats a class
(check (handler-case (meet 'x 'y) (error () :none)) :none)
(defgeneric cascade (a b))
(defmethod cascade ((a integer) (b integer)) (list :int-int (call-next-method)))
(defmethod cascade ((a number) (b integer)) (list :num-int (call-next-method)))
(defmethod cascade ((a t) (b t)) :end)
(defvar *cascade-log* nil)
(defmethod cascade :before ((a integer) (b t)) (push :before *cascade-log*))
(check (cascade 1 2) '(:int-int (:num-int :end)))
(check *cascade-log* '(:before))
(check (cascade 1.5 2) '(:num-int :end))
(check (cascade "a" 2) :end)
(let ((m (find-method #'meet nil (list 'integer 'integer))))
  (check (method-specializers m) (list (find-class 'integer) (find-class 'integer)))
  (remove-method #'meet m)
  (check (meet 1 2) :number-and-thing))

;;; ---- slots ---------------------------------------------------------------------------

(defclass counter ()
  ((count :initarg :count :accessor counter-count)
   (total :allocation :class :initform 0 :accessor counter-total)
   (label :initarg :label :accessor counter-label))
  (:default-initargs :label "unnamed"))
(defvar *c1* (make-instance 'counter))
(defvar *c2* (make-instance 'counter :count 5 :label "five"))
(check (slot-boundp *c1* 'count) nil)
(check (handler-case (counter-count *c1*) (unbound-slot (c) (list :unbound (cell-error-name c)))) '(:unbound count))
(check (counter-label *c1*) "unnamed")
(check (counter-label *c2*) "five")
(setf (counter-total *c1*) 7)
(check (counter-total *c2*) 7)                     ; one slot for the whole class
(check (progn (slot-makunbound *c2* 'count) (slot-boundp *c2* 'count)) nil)
(check (slot-exists-p *c1* 'total) t)
(check (slot-exists-p *c1* 'nope) nil)
(check (handler-case (slot-value *c1* 'nope) (error () :missing)) :missing)
(defclass watched () ((log :initform nil :accessor watch-log) (v :initarg :v :accessor watch-v)))
(defmethod initialize-instance :after ((w watched) &key v)
  (push (list :init v) (watch-log w)))
(defvar *shared-calls* 0)
(defmethod shared-initialize :before ((w watched) slots &rest initargs)
  (setq *shared-calls* (1+ *shared-calls*)))
(check (watch-log (make-instance 'watched :v 3)) '((:init 3)))
(let ((w (make-instance 'watched :v 1)))
  (reinitialize-instance w :v 9)
  (check (watch-v w) 9))
(defclass readable () ((a :initarg :a :reader get-a :writer put-a)))
(let ((r (make-instance 'readable :a 1)))
  (put-a 2 r)
  (check (get-a r) 2))
(check (let ((x (make-instance 'counter :count 1)))
         (with-slots ((n count) label) x
           (incf n 10)
           (setf label "eleven")
           (list n label (counter-count x))))
       '(11 "eleven" 11))
(check (let ((x (make-instance 'counter :count 2)))
         (with-accessors ((n counter-count)) x (setq n (* n 3)) (counter-count x)))
       6)
(defstruct spot x y)
(check (let ((s (make-spot :x 1 :y 2))) (with-slots (x y) s (list x y))) '(1 2))
(check (let* ((s (make-spot :x 1)) (c (copy-spot s))) (setf (spot-x c) 9) (list (spot-x s) (spot-x c))) '(1 9))
(check (with-output-to-string (s) (describe *c1* s)) "#<COUNTER> is an instance of COUNTER
  COUNT: unbound
  TOTAL: 7
  LABEL: \"unnamed\"
")
(defmethod describe-object ((x counter) stream) (format stream "A counter at ~A" (slot-value x 'label)))
(check (with-output-to-string (s) (describe *c1* s)) "A counter at unnamed")

;;; ---- forms ---------------------------------------------------------------------------

(check (symbol-macrolet ((top (car stack))) (let ((stack (list 1 2))) (setf top 10) (incf top) stack)) '(11 2))
(check (symbol-macrolet ((x 'outer)) (list x (let ((x 'inner)) x))) '(outer inner))
(define-symbol-macro *first-item* (first *items-list*))
(defvar *items-list* (list 'a 'b))
(check *first-item* 'a)
(check (progn (setq *first-item* 'z) *items-list*) '(z b))
(check (eval-when (:compile-toplevel :load-toplevel :execute) :ran) :ran)
(check (eval-when (:compile-toplevel) :ran) nil)
(defvar *pv* :global)
(check (progv '(*pv*) '(:bound) (symbol-value '*pv*)) :bound)
(check *pv* :global)
(check (progv (list (intern "PV-FRESH")) (list 5) (symbol-value (intern "PV-FRESH"))) 5)
(check (catch :out (progv '(*pv*) '(:inner) (throw :out *pv*))) :inner)
(check *pv* :global)
(defun ltv () (load-time-value (list :made)))
(check (eq (ltv) (ltv)) t)
(define-modify-macro appendf (&rest more) append)
(define-modify-macro multf (factor) *)
(check (let ((l (list 1))) (appendf l '(2) '(3)) l) '(1 2 3))
(check (let ((n 4)) (multf n 5) n) 20)
(check (let ((pl (list :a 1 :b 2 :c 3))) (list (remf pl :b) pl (remf pl :z))) '(t (:a 1 :c 3) nil))
(check (multiple-value-list (get-properties '(:a 1 :b 2) '(:b :c))) '(:b 2 (:b 2)))
(check (sublis '((a . 1) (b . 2)) '(a (b c) . a)) '(1 (2 c) . 1))
(check (map-into (list 0 0 0) #'+ '(1 2 3) '(10 20 30)) '(11 22 33))
(check (let ((v (vector 1 2))) (map-into v #'- v) v) #(-1 -2))
(check (ccase 2 (1 :one) (2 :two)) :two)
(define-compiler-macro sq3 (x) (list '* x x x))
(check (functionp (compiler-macro-function 'sq3)) t)
(defvar *doc-var* 1 "A documented variable.")
(defparameter *doc-par* 2 "Another.")
(check (list (documentation '*doc-var* 'variable) (documentation '*doc-par* 'variable)) '("A documented variable." "Another."))
(defun undocumented (x) x)
(check (documentation 'undocumented 'function) nil)
(setf (documentation 'undocumented 'function) "Now it is.")
(check (documentation #'undocumented 'function) "Now it is.")
(check (member 'meet (apropos-list "MEE")) '(meet))
(check (apropos-list "NO-SUCH-NAME-ANYWHERE-XYZZY") nil)

;;; ---- lists with :TEST and :KEY ---------------------------------------------------------

(check (member "b" '("a" "b" "c")) '("b" "c"))                       ; EQUAL without a test, as before
(check (member 2.0 '(1 2 3) :test #'=) '(2 3))
(check (member 'b '((a 1) (b 2)) :key #'car) '((b 2)))
(check (member 3 '(1 2 3) :test-not #'>) '(3))
(check (member-if #'evenp '(1 3 4 5)) '(4 5))
(check (assoc "B" '(("a" . 1) ("b" . 2)) :test #'string-equal) '("b" . 2))
(check (assoc 2 '((1 . a) (2 . b)) :test #'=) '(2 . b))
(check (rassoc 'b '((1 . a) (2 . b))) '(2 . b))
(check (let ((l (list 1 2))) (pushnew 2.0 l :test #'=) (pushnew 3 l) l) '(3 1 2))
(check (adjoin '(a) '((a) (b)) :test #'equal) '((a) (b)))
(check (sort (union '(1 2 3) '(2.0 4) :test #'=) #'<) '(1 2.0 3 4))
(check (intersection '((a 1) (b 2)) '((b 9)) :key #'car) '((b 2)))
(check (set-difference '(1 2 3 4) '(2 4)) '(1 3))
(check (subsetp '(1 2) '(3 2 1)) t)
(check (every #'digit-char-p "123") t)
(check (some #'alpha-char-p "12a") t)
(check (every #'< '(1 2 3) '(2 3 4)) t)
(check (some #'> '(1 5) #(2 3)) t)
(check (notany #'zerop #(1 2)) t)
(check (notevery #'plusp '(1 -1)) t)

;;; ---- keywords, the clock ---------------------------------------------------------------

(check (symbol-name :foo) "FOO")
(check (string :bar) "BAR")
(check (princ-to-string :baz) "BAZ")
(check (prin1-to-string :baz) ":BAZ")
(check (format nil "~A ~S" :a :b) "A :B")
(check (keywordp :x) t)
(check (eq (intern "QUX" :keyword) :qux) t)
(check (multiple-value-list (decode-universal-time 0 0)) '(0 0 0 1 1 1900 0 nil 0))
(check (multiple-value-list (decode-universal-time (encode-universal-time 5 4 3 2 1 2000 0) 0))
       '(5 4 3 2 1 2000 6 nil 0))
(check (encode-universal-time 0 0 0 1 1 1970 0) 2208988800)
(check (nth-value 5 (decode-universal-time (encode-universal-time 0 0 12 29 2 2024 -2) -2)) 2024)
(check (nth-value 3 (decode-universal-time (encode-universal-time 0 0 12 29 2 2024 5) 5)) 29)
(check (> (get-universal-time) 3900000000) t)     ; after 2023
(check (length (multiple-value-list (get-decoded-time))) 9)

(report "cl7")
