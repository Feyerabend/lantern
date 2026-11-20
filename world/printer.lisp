;;; -*- Mode: Lisp -*-
;;; printer.lisp -- the printer in Lisp (M14): PRINT-OBJECT, the *PRINT-...*
;;; variables, WRITE, structures as #S(...), PRINT-UNREADABLE-OBJECT; and
;;; *PRINT-CIRCLE* (M16). The pretty printer is in pretty.lisp.
;;;
;;; The kernel's printer (kernel/print.c) is fast and never allocates; it
;;; still prints everything that is plain data. This one takes over when an
;;; object holds an instance (a structure, a condition, anything DEFCLASS
;;; made), so that PRINT-OBJECT methods are used however deep it is, or when
;;; a *PRINT-...* variable asks for something the kernel does not do.

(defvar *print-escape* t)       ; PRIN1 (T) or PRINC (NIL)
(defvar *print-base* 10)
(defvar *print-radix* nil)
(defvar *print-length* nil)     ; at most this many elements of a list or vector
(defvar *print-level* nil)      ; at most this deep
(defvar *print-case* :upcase)   ; :upcase or :downcase or :capitalize, for symbols
(defvar *print-pretty* nil)     ; T: through the pretty printer (pretty.lisp)
(defvar *print-readably* nil)
(defvar *print-circle* nil)
(defvar *print-array* t)
(defvar *print-gensym* t)
(defvar *print-right-margin* nil) ; NIL: the width of the stream, or 80

(defgeneric print-object (object stream))

;; what the kernel's printer writes
(defun %kernel-print (x stream)
  (write-string (if *print-escape* (%prin1-to-string x) (%princ-to-string x)) stream))

(defmethod print-object ((object t) stream)
  (%kernel-print object stream)
  object)

;; a structure: #S(POINT :X 1 :Y 2)
(defun structurep (x) (and (instancep x) (get (class-name (class-of x)) 'struct-slots) t))

(defun %print-structure (x stream)
  (write-string "#S(" stream)
  (%write (class-name (class-of x)) stream 1)
  (let ((n 0))
    (dolist (s (get (class-name (class-of x)) 'struct-slots))
      (write-string " " stream)
      (if (and *print-length* (>= n *print-length*))
          (progn (write-string "..." stream) (return nil))
          (progn (write-string (string-append ":" (symbol-name (car s))) stream)
                 (write-string " " stream)
                 (%write (slot-value x (car s)) stream 1)))
      (setq n (1+ n))))
  (write-string ")" stream))

;; structures as #S, conditions by their report when not escaping,
;; anything else as #<CLASS>
(defmethod print-object ((object structure-object) stream)
  (%print-structure object stream)
  object)

(defmethod print-object ((object condition) stream)
  (if *print-escape*
      (%kernel-print object stream)
      (write-string (condition-report-string object) stream))
  object)

(defmacro print-unreadable-object (spec &body body)
  (destructuring-bind (object stream &key type identity) spec
    (let ((o (gensym)) (s (gensym)))
      `(let ((,o ,object) (,s ,stream))
         (write-string "#<" ,s)
         ,@(when type `((let ((*print-escape* nil)) (%write (class-name (class-of ,o)) ,s 0))
                        ,@(when body `((write-string " " ,s)))))
         ,@body
         ,@(when identity `((write-string " {object}" ,s)))
         (write-string ">" ,s)
         nil))))

;;; ---- the walk -------------------------------------------------------------------------

(defun %plain-vector-p (x)
  (and (vectorp x) (not (stringp x)) (not (%array-header-p x))
       (not (and (> (length x) 0) (symbolp (aref x 0)) (keywordp (aref x 0))))))   ; system objects

(defun %write-integer (n stream)
  (let ((digits (%fmt-radix n *print-base*)))
    (when *print-radix*
      (write-string (case *print-base* (2 "#b") (8 "#o") (16 "#x") (10 "") (t (format nil "#~Dr" *print-base*)))
                    stream))
    (write-string (if (> *print-base* 10) (string-upcase digits) digits) stream)
    (when (and *print-radix* (= *print-base* 10)) (write-string "." stream))))

(defun %write-symbol (x stream)
  (let ((s (if *print-escape* (%prin1-to-string x) (%princ-to-string x))))
    (write-string (cond ((%char-index #\| s 0 (length s)) s)    ; |Name| keeps its case
                        ((eq *print-case* :downcase) (string-downcase s))
                        ((eq *print-case* :capitalize) (string-capitalize s))
                        (t s))
                  stream)))

(defun %write-elements (items stream level open close)
  (write-string open stream)
  (let ((n 0))
    (loop
      (when (null items) (return nil))
      (when (> n 0) (write-string " " stream))
      (when (and *print-length* (>= n *print-length*))
        (write-string "..." stream)
        (return nil))
      (when (and (> n 0) (consp items) (%circle-shared-p items))   ; a shared tail: . #1#
        (write-string ". " stream)
        (%write items stream (1+ level))
        (return nil))
      (if (consp items)
          (progn (%write (car items) stream (1+ level)) (setq items (cdr items)))
          (progn (write-string ". " stream) (%write items stream (1+ level)) (setq items nil)))
      (setq n (1+ n))))
  (write-string close stream))

;;; ---- *PRINT-CIRCLE* (M16) --------------------------------------------------------------
;;;
;;; Before printing, a walk finds the conses, vectors, arrays and instances
;;; met more than once. The first time one of them is printed it gets a
;;; label, #1=, and after that it prints as #1#. (The reader reads them back
;;; the same: world/reader.lisp.)

(defvar *%circle* nil)          ; while printing: object => :once, :shared or its label
(defvar *%circle-count* 0)

(defun %circle-candidate-p (x)
  (or (consp x) (%plain-vector-p x) (instancep x)
      (and (%array-header-p x) (not (stringp (%array-field x ar-data))))))

(defun %circle-scan (x table)
  (loop
    (unless (%circle-candidate-p x) (return nil))
    (when (gethash x table)
      (setf (gethash x table) :shared)
      (return nil))
    (setf (gethash x table) :once)
    (cond ((consp x) (%circle-scan (car x) table) (setq x (cdr x)))
          ((instancep x) (let ((v (instance-slots x)) (i 0))
                           (while (< i (length v)) (%circle-scan (aref v i) table) (setq i (1+ i))))
                         (return nil))
          (t (dolist (e (coerce x 'list)) (%circle-scan e table)) (return nil)))))

(defun %circle-shared-p (x)
  (and *%circle* (let ((e (gethash x *%circle*))) (and e (not (eq e :once))))))

;; => T when X was printed as #n#; else writes #n= if X needs a label
(defun %circle-mark (x stream)
  (let ((e (gethash x *%circle*)))
    (cond ((integerp e) (write-string (format nil "#~D#" e) stream) t)
          ((eq e :shared)
           (setq *%circle-count* (1+ *%circle-count*))
           (setf (gethash x *%circle*) *%circle-count*)
           (write-string (format nil "#~D=" *%circle-count*) stream)
           nil)
          (t nil))))

;; call F with *%CIRCLE* ready for printing X: a new table, unless printing has started
(defun %with-circle (x f)
  (if (or (not *print-circle*) *%circle* (not (%circle-candidate-p x)))
      (funcall f)
      (let ((table (make-hash-table :test #'eq)))
        (%circle-scan x table)
        (let ((*%circle* table) (*%circle-count* 0)) (funcall f)))))

(defun %write (x stream level)
  (cond ((and *%circle* (%circle-candidate-p x) (%circle-mark x stream)) nil)
        ((and *print-level* (>= level *print-level*) (or (consp x) (%plain-vector-p x) (instancep x)))
         (write-string "#" stream))
        ((and *print-pretty* (or (consp x) (%plain-vector-p x))) (%pprint-object x stream))   ; pretty.lisp
        ((consp x)
         (if (and (memq (car x) '(quote function)) (consp (cdr x)) (null (cddr x)))
             (progn (write-string (if (eq (car x) 'quote) "'" "#'") stream)
                    (%write (cadr x) stream level))
             (%write-elements x stream level "(" ")")))
        ((instancep x) (print-object x stream))
        ((%plain-vector-p x) (%write-elements (coerce x 'list) stream level "#(" ")"))
        ((and (%array-header-p x) (= (array-rank x) 1) (not (stringp (%array-field x ar-data))))
         (%write-elements (coerce x 'list) stream level "#(" ")"))
        ((and (integerp x) (or (/= *print-base* 10) *print-radix*)) (%write-integer x stream))
        ((and (rationalp x) (or (/= *print-base* 10) *print-radix*))      ; a ratio, 7/2 (M18)
         (let ((prefix (cond ((not *print-radix*) "") ((= *print-base* 10) "#10r") ((= *print-base* 2) "#b")
                             ((= *print-base* 8) "#o") ((= *print-base* 16) "#x") (t (format nil "#~Dr" *print-base*))))
               (*print-radix* nil))
           (write-string prefix stream)
           (%write-integer (numerator x) stream)
           (write-string "/" stream)
           (%write-integer (denominator x) stream)))
        ((and (symbolp x) (not (eq *print-case* :upcase))) (%write-symbol x stream))
        ((and (conditionp x) (not *print-escape*)) (write-string (condition-report-string x) stream))
        (t (%kernel-print x stream))))

;; can the kernel print X by itself?
(defun %kernel-can-print-p (x)
  (and (eql *print-base* 10) (not *print-radix*) (null *print-length*) (null *print-level*)
       (eq *print-case* :upcase) (not (%holds-instance-p x))
       (or *print-escape* (not (conditionp x)))
       (not (and (or *print-circle* *print-pretty*)
                 (or (consp x) (vectorp x) (%array-header-p x))))))

;;; ---- the printing functions ------------------------------------------------------------

(defun %to-string (x escape)
  (let ((*print-escape* escape))
    (if (%kernel-can-print-p x)
        (if escape (%prin1-to-string x) (%princ-to-string x))
        (with-output-to-string (s) (%with-circle x (lambda () (%write x s 0)))))))

(defun prin1-to-string (x) (%to-string x t))
(defun princ-to-string (x) (%to-string x nil))

(defun %write-to (x stream)
  (if (%kernel-can-print-p x)
      (%kernel-print x stream)
      (%with-circle x (lambda () (%write x stream 0)))))

(defun write (object &key (stream *standard-output*) (escape *print-escape*) (base *print-base*)
                          (radix *print-radix*) (length *print-length*) (level *print-level*)
                          (case *print-case*) (pretty *print-pretty*) (circle *print-circle*)
                          (right-margin *print-right-margin*) (readably *print-readably*)
                          &allow-other-keys)
  (let ((*print-escape* (or escape readably)) (*print-base* base) (*print-radix* radix) (*print-length* length)
        (*print-level* level) (*print-case* case) (*print-pretty* pretty) (*print-circle* circle)
        (*print-right-margin* right-margin) (*print-readably* readably))
    (%write-to object (resolve-stream stream)))
  object)

(defun write-to-string (object &rest keys)
  (with-output-to-string (s) (apply #'write object :stream s keys)))

(defun prin1 (object &optional stream)
  (let ((*print-escape* t)) (%write-to object (resolve-stream stream)))
  object)

(defun princ (object &optional stream)
  (let ((*print-escape* nil)) (%write-to object (resolve-stream stream)))
  object)


(defun print (object &optional stream)
  (terpri stream)
  (prin1 object stream)
  (write-string " " stream)
  object)
