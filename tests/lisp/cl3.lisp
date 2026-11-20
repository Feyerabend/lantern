;;; cl3.lisp -- tests for M13: Common Lisp, part 3 (DO and DESTRUCTURING-BIND,
;;; arrays, condition classes, input streams and READ, #+ and #-).
;;;
;;; The first part is the done-test: a small program in ordinary Common Lisp
;;; style. It reads records with READ from a string stream, takes them apart
;;; with DESTRUCTURING-BIND, keeps a table in a two-dimensional array, signals
;;; its own condition for a bad record, and recovers through a restart chosen
;;; by a HANDLER-BIND, which also logs. The rest checks each piece.

;;; ---- the program -----------------------------------------------------------------

(defpackage :grades (:use :common-lisp) (:export #:grade-report #:*log*))
(in-package :grades)

(defvar *log* nil)

(define-condition bad-record (error)
  ((record :initarg :record :reader bad-record-record)
   (why :initarg :why :reader bad-record-why))
  (:report (lambda (c stream)
             (format stream "Bad record ~S: ~A" (bad-record-record c) (bad-record-why c)))))

(defun check-record (record)
  (destructuring-bind (name &rest scores) record
    (unless (symbolp name)
      (error 'bad-record :record record :why "the name is not a symbol"))
    (unless (= (length scores) 3)
      (error 'bad-record :record record :why "there should be three scores"))
    (unless (every #'realp scores)
      (error 'bad-record :record record :why "a score is not a number"))
    record))

;; every record that can be read, skipping (and logging) the bad ones
(defun read-records (text)
  (let ((records (make-array 0 :adjustable t :fill-pointer 0))
        (eof (list :eof)))
    (with-input-from-string (in text)
      (do ((record (read in nil eof) (read in nil eof)))
          ((eq record eof))
        (restart-case (vector-push-extend (check-record record) records)
          (skip-record () :report "Leave this record out" nil))))
    records))

(defun grade-table (records)
  (let ((table (make-array (list (length records) 3) :initial-element 0)))
    (dotimes (i (length records))
      (destructuring-bind (name a b c) (aref records i)
        (declare (ignore name))
        (setf (aref table i 0) a (aref table i 1) b (aref table i 2) c)))
    table))

(defun average-row (table row)
  (/ (+ (aref table row 0) (aref table row 1) (aref table row 2)) 3.0))

(defun grade-report (text)
  (handler-bind ((bad-record (lambda (c)
                               (push (princ-to-string c) *log*)
                               (invoke-restart 'skip-record))))
    (let* ((records (read-records text))
           (table (grade-table records)))
      (with-output-to-string (out)
        (dotimes (i (array-dimension table 0))
          (format out "~A~10T~,1F~%" (string-downcase (first (aref records i))) (average-row table i)))
        #+lisp-m (format out "on a Lisp machine~%")
        #-lisp-m (format out "elsewhere~%")))))

(in-package :cl-user)

(check (grades:grade-report "(ada 90 95 100) (bob 70 80) (cy 60 70 80) (\"dee\" 1 2 3) (eve 88 92 x)")
       "ada       95.0
cy        70.0
on a Lisp machine
")
(check (reverse grades:*log*)
       '("Bad record (BOB 70 80): there should be three scores"
         "Bad record (\"dee\" 1 2 3): the name is not a symbol"
         "Bad record (EVE 88 92 X): a score is not a number"))

;;; ---- DO, PSETQ, ROTATEF ----------------------------------------------------------------

(check (do ((i 0 (1+ i)) (acc nil (cons i acc))) ((= i 4) acc)) '(3 2 1 0))
(check (do* ((i 0 (1+ i)) (j i i)) ((> i 3) j)) 4)
(check (do ((i 0 (1+ i))) ((= i 10) :never) (when (= i 3) (return :early))) :early)
(check (let ((n 0)) (do () ((> n 5) n) (setq n (1+ n)))) 6)
(check (let ((a 1) (b 2)) (psetq a b b a) (list a b)) '(2 1))
(check (let ((a 1) (b 2) (c 3)) (rotatef a b c) (list a b c)) '(2 3 1))
(check (let ((l (list 1 2 3))) (list (shiftf (car l) (cadr l) 9) l)) '(1 (2 9 3)))
(check (let ((v (vector 1 2))) (rotatef (aref v 0) (aref v 1)) v) #(2 1))

;;; ---- DESTRUCTURING-BIND and DEFMACRO lambda lists --------------------------------------------

(check (destructuring-bind (a (b c) &optional (d 4 d-p) &rest more) '(1 (2 3)) (list a b c d d-p more))
       '(1 2 3 4 nil nil))
(check (destructuring-bind (a (b c) &optional (d 4 d-p) &rest more) '(1 (2 3) 5 6 7) (list a b c d d-p more))
       '(1 2 3 5 t (6 7)))
(check (destructuring-bind (a &key (x 10) y ((:zed z) :none z-p)) '(1 :y 2) (list a x y z z-p))
       '(1 10 2 :none nil))
(check (destructuring-bind (a . b) '(1 2 3) (list a b)) '(1 (2 3)))
(check (destructuring-bind (&whole w a b) '(1 2) (list w a b)) '((1 2) 1 2))
(check (destructuring-bind ((a b) (c (d))) '((1 2) (3 (4))) (list a b c d)) '(1 2 3 4))
(check (destructuring-bind (a &aux (b (* a 2))) '(5) b) 10)
(check (handler-case (destructuring-bind (a b) '(1) (list a b)) (error () :too-few)) :too-few)
(check (handler-case (destructuring-bind (a) '(1 2) a) (error () :too-many)) :too-many)
(defmacro with-pair ((x y) pair &body body) `(destructuring-bind (,x ,y) ,pair ,@body))
(check (with-pair (p q) (list 7 8) (+ p q)) 15)
(defmacro my-let1 ((var value) &body body) `(let ((,var ,value)) ,@body))
(check (my-let1 (x 3) (* x x)) 9)
(defmacro dotted (a . rest) `(list ',a ',rest))
(check (dotted 1 2 3) '(1 (2 3)))

;;; ---- lambda lists: supplied-p, &aux, (keyword var) ----------------------------------------

(defun ll-test (a &optional (b 2 b-p) (c (* b 10)) &key (k 5 k-p) ((:other o) :none) &aux (sum (+ a b)))
  (list a b b-p c k k-p o sum))
(check (ll-test 1) '(1 2 nil 20 5 nil :none 3))
(check (ll-test 1 3) '(1 3 t 30 5 nil :none 4))
(check (ll-test 1 3 4 :k 9 :other 8) '(1 3 t 4 9 t 8 4))

;;; ---- SETF: several pairs, (setf name) functions, DEFSETF -------------------------------------

(check (let ((a (list 1 2)) (b (vector 1 2))) (setf (car a) 10 (aref b 1) 20) (list a b)) '((10 2) #(1 20)))
(defun (setf middle) (new list) (setf (second list) new))
(check (let ((l (list 1 2 3))) (setf (middle l) :m) l) '(1 :m 3))
(check (funcall #'(setf middle) 5 (list 1 2 3)) 5)
(defun my-first (l) (car l))
(defsetf my-first (l) (v) `(setf (car ,l) ,v))
(check (let ((l (list 1 2))) (setf (my-first l) 9) l) '(9 2))
(defun set-last-element (l v) (setf (car (last l)) v))
(defun last-element (l) (car (last l)))
(defsetf last-element set-last-element)
(check (let ((l (list 1 2 3))) (setf (last-element l) :end) l) '(1 2 :end))

;;; ---- arrays -------------------------------------------------------------------------------

(defvar *m* (make-array '(2 3) :initial-contents '((1 2 3) (4 5 6))))
(check (aref *m* 1 2) 6)
(check (progn (setf (aref *m* 1 2) 60) (aref *m* 1 2)) 60)
(check (array-dimensions *m*) '(2 3))
(check (array-rank *m*) 2)
(check (array-dimension *m* 1) 3)
(check (array-total-size *m*) 6)
(check (array-row-major-index *m* 1 1) 4)
(check (row-major-aref *m* 4) 5)
(check (array-in-bounds-p *m* 1 2) t)
(check (array-in-bounds-p *m* 2 0) nil)
(check (prin1-to-string *m*) "#2A((1 2 3) (4 5 60))")
(check (prin1-to-string (make-array '(2 2) :initial-element 0)) "#2A((0 0) (0 0))")
(check (aref #2A((1 2) (3 4)) 1 0) 3)
(check (prin1-to-string (read-from-string "#2A((a b) (c d))")) "#2A((A B) (C D))")
(check (aref (make-array nil :initial-element 7)) 7)
(check (handler-case (aref *m* 2 0) (error () :out)) :out)
(check (handler-case (aref *m* 0) (error () :rank)) :rank)
(check (arrayp *m*) t)
(check (vectorp *m*) nil)
(check (arrayp "abc") t)
(check (make-array 3 :initial-element 1) #(1 1 1))
(check (make-array 3 :element-type 'character :initial-element #\x) "xxx")
;; fill pointers and growing
(defvar *v* (make-array 0 :adjustable t :fill-pointer 0))
(dotimes (i 40) (vector-push-extend (* i i) *v*))
(check (length *v*) 40)
(check (aref *v* 39) 1521)
(check (vectorp *v*) t)
(check (vector-pop *v*) 1521)
(check (fill-pointer *v*) 39)
(check (find 100 *v*) 100)
(check (position 49 *v*) 7)
(check (reduce #'+ *v*) 19019)
(check (count-if #'evenp *v*) 20)
(check (subseq (coerce *v* 'list) 0 5) '(0 1 4 9 16))
(check (progn (setf (fill-pointer *v*) 3) (prin1-to-string *v*)) "#(0 1 4)")
(check (adjustable-array-p *v*) t)
(check (array-has-fill-pointer-p *v*) t)
(check (array-has-fill-pointer-p #(1 2)) nil)
(check (let ((v (make-array 2 :fill-pointer 0)))
         (list (vector-push :a v) (vector-push :b v) (vector-push :c v) (prin1-to-string v)))
       '(0 1 nil "#(:A :B)"))
(check (let ((s (make-array 0 :element-type 'character :adjustable t :fill-pointer 0)))
         (dolist (c '(#\h #\i)) (vector-push-extend c s))
         (list (prin1-to-string s) (coerce s 'string) (length s)))
       '("\"hi\"" "hi" 2))
(check (prin1-to-string (adjust-array (make-array '(2 2) :initial-contents '((1 2) (3 4))) '(3 3) :initial-element 0))
       "#2A((1 2 0) (3 4 0) (0 0 0))")
(check (adjust-array (vector 1 2 3) 5 :initial-element 0) #(1 2 3 0 0))
(check (prin1-to-string (make-array '(2 3) :displaced-to (make-array 6 :initial-contents '(1 2 3 4 5 6))))
       "#2A((1 2 3) (4 5 6))")
;; COERCE
(check (coerce '(1 2) 'vector) #(1 2))
(check (coerce "ab" 'list) '(#\a #\b))
(check (coerce '(#\a #\b) 'string) "ab")
(check (coerce 3 'float) 3.0)
(check (coerce #\a 'character) #\a)
(check (coerce "x" 'character) #\x)
(check (coerce #(1 2) 'list) '(1 2))
(check (functionp (coerce 'car 'function)) t)

;;; ---- conditions ------------------------------------------------------------------------

(define-condition bad-input (error)
  ((value :initarg :value :reader bad-input-value))
  (:report (lambda (c s) (format s "Bad input: ~S" (bad-input-value c)))))
(define-condition worse-input (bad-input) () (:report "It got worse"))
(define-condition note (condition) ((text :initarg :text :reader note-text)))

(check (handler-case (error 'bad-input :value 42) (bad-input (c) (list (bad-input-value c) (princ-to-string c))))
       '(42 "Bad input: 42"))
(check (handler-case (error 'worse-input :value 1) (bad-input (c) (princ-to-string c))) "It got worse")
(check (handler-case (error 'worse-input :value 1) (error (c) (class-name (class-of c)))) 'worse-input)
(check (typep (make-condition 'worse-input :value 1) 'bad-input) t)
(check (typep (make-condition 'worse-input :value 1) 'error) t)
(check (typep (make-condition 'note) 'error) nil)
(check (handler-case (error "Value ~A is wrong" 7) (simple-error (c) (format nil "~A" c))) "Value 7 is wrong")
(check (handler-case (error "Not found:" 'x) (error (c) (princ-to-string c))) "Not found: X")
(check (handler-case (error (make-condition 'bad-input :value :obj)) (bad-input (c) (bad-input-value c))) :obj)
;; the kernel's own errors have classes
(check (handler-case (/ 1 0) (division-by-zero () :div0)) :div0)
(check (handler-case (/ 1 0) (arithmetic-error () :arith)) :arith)
(check (handler-case (car 5) (type-error (c) (list (type-error-datum c) (type-error-expected-type c))))
       '(5 list))
(check (handler-case undefined-variable-xyz (unbound-variable (c) (cell-error-name c))) 'undefined-variable-xyz)
(check (handler-case (no-such-function-xyz 1) (undefined-function (c) (cell-error-name c))) 'no-such-function-xyz)
(check (handler-case (car 5) ((or division-by-zero type-error) () :either)) :either)
(check (handler-case (car 5) (division-by-zero () :wrong) (error () :general)) :general)
;; HANDLER-BIND runs before unwinding, innermost first, and may decline
(check (let ((log nil))
         (list (handler-case
                   (handler-bind ((error (lambda (c) (declare (ignore c)) (push :outer log))))
                     (handler-bind ((error (lambda (c) (declare (ignore c)) (push :inner log))))
                       (car 5)))
                 (error () :handled))
               log))
       '(:handled (:outer :inner)))
;; an inner HANDLER-CASE takes it first: an outer HANDLER-BIND never sees it
(check (let ((log nil))
         (list (handler-bind ((error (lambda (c) (declare (ignore c)) (push :outer log))))
                 (handler-case (car 5) (error () :inner)))
               log))
       '(:inner nil))
;; a handler runs with only the outer handlers in effect
(check (handler-case
           (handler-bind ((bad-input (lambda (c) (declare (ignore c)) (error "from the handler"))))
             (error 'bad-input :value 1))
         (simple-error (c) (princ-to-string c)))
       "from the handler")
;; restarts from handlers
(check (restart-case (handler-bind ((bad-input (lambda (c) (use-value (* 2 (bad-input-value c))))))
                       (error 'bad-input :value 21))
         (use-value (v) v))
       42)
(check (handler-bind ((error (lambda (c) (declare (ignore c)) (invoke-restart 'use-value 7))))
         (restart-case (car 5) (use-value (v) v)))
       7)
;; SIGNAL returns NIL when nobody takes it; HANDLER-CASE catches any condition
(check (signal 'note :text "hello") nil)
(check (handler-case (signal 'note :text "hello") (note (c) (note-text c))) "hello")
(check (let ((seen nil)) (handler-bind ((note (lambda (c) (push (note-text c) seen)))) (signal 'note :text "x")) seen)
       '("x"))
;; WARN, MUFFLE-WARNING
(check (let ((seen nil))
         (handler-bind ((warning (lambda (c) (push (princ-to-string c) seen) (muffle-warning c))))
           (warn "careful ~D" 3))
         seen)
       '("careful 3"))
(check (with-output-to-string (*standard-output*) (warn "printed")) ";; Warning: printed
")
;; :NO-ERROR, IGNORE-ERRORS, ASSERT
(check (handler-case (values 1 2) (:no-error (a b) (list a b))) '(1 2))
(check (handler-case (values 1 2) (error () :no)) 1)
(check (multiple-value-list (handler-case (values 1 2) (error () :no))) '(1 2))
(check (ignore-errors (car 5)) nil)
(check (class-name (class-of (second (multiple-value-list (ignore-errors (error 'bad-input :value 1)))))) 'bad-input)
(check (handler-case (assert (= 1 2)) (error (c) (princ-to-string c))) "Assertion failed: (= 1 2)")
(check (handler-case (assert (= 1 2) () 'bad-input :value :a) (bad-input (c) (bad-input-value c))) :a)

;;; ---- input streams and READ ----------------------------------------------------------------

(check (with-input-from-string (s "(+ 1 2) foo \"bar\" 42")
         (list (read s) (read s) (read s) (read s) (read s nil :done)))
       '((+ 1 2) foo "bar" 42 :done))
(check (with-input-from-string (s (format nil "line one~%line two~%three"))
         (list (read-line s) (read-char s) (peek-char nil s) (multiple-value-list (read-line s))
               (multiple-value-list (read-line s)) (read-line s nil :eof)))
       '("line one" #\l #\i ("ine two" nil) ("three" t) :eof))
(check (let ((s (make-string-input-stream "   x y"))) (list (peek-char t s) (read-char s) (read-char s) (listen s)))
       '(#\x #\x #\Space t))
(check (let ((s (make-string-input-stream "ab"))) (read-char s) (unread-char #\a s) (read-char s)) #\a)
(check (handler-case (read (make-string-input-stream "")) (end-of-file () :eof)) :eof)
(check (handler-case (read (make-string-input-stream "(a b")) (error () :unfinished)) :unfinished)
(check (multiple-value-list (read-from-string "foo bar")) '(foo 4))
(check (read-from-string "" nil :empty) :empty)
(check (input-stream-p (make-string-input-stream "x")) t)
(check (output-stream-p (make-string-output-stream)) t)
(check (with-output-to-string (s) (write-line "a" s) (write 'b :stream s) (write "c" :stream s :escape nil))
       "a
Bc")
;; LOAD from a stream: IN-PACKAGE inside applies to the rest of it, not after
(check (load (make-string-input-stream
              "(defpackage :loaded (:use :cl)) (in-package :loaded) (defun f () 'from-loaded)"))
       t)
(check (list (prin1-to-string (loaded::f)) (package-name *package*)) '("LOADED::FROM-LOADED" "COMMON-LISP-USER"))

;;; ---- #+ and #- -------------------------------------------------------------------------------

(check (list #+common-lisp 1 #-common-lisp 2 #+(or foo lisp-m) 3 #+(and lisp-m (not foo)) 4 #-(and) 5) '(1 3 4))
(check (list #+sbcl (sb-ext:quit) :portable) '(:portable))       ; left out: SB-EXT need not exist
(check '(a #+nope b) '(a))
(check (not (null (member :common-lisp *features*))) t)

(report "cl3")
