;;; -*- Mode: Lisp -*-
;;; signals.lisp -- the condition system's classes (M13): DEFINE-CONDITION,
;;; MAKE-CONDITION, ERROR, SIGNAL and WARN as Common Lisp has them, and the
;;; standard condition types.
;;;
;;; What is signalled is always a kernel condition (#<ERROR ...>), which the
;;; kernel, the debugger and the error reports know: its type, its message
;;; (the report, already written) and its irritants. A condition a program
;;; makes with MAKE-CONDITION, an instance of a class from DEFINE-CONDITION,
;;; rides inside it, and that instance is what handlers receive. The kernel's
;;; own errors ("Not a list:", "Division by zero") are kernel conditions
;;; alone; their class is worked out from their message, so a handler for
;;; TYPE-ERROR or DIVISION-BY-ZERO catches them too.
;;;
;;; HANDLER-BIND, HANDLER-CASE and the handler stack are in compiler.lisp;
;;; restarts are in conditions.lisp.

;;; ---- the standard condition classes ----------------------------------------------

(defclass serious-condition (condition) ())
(defclass error (serious-condition) ())
(defclass warning (condition) ())
(defclass simple-condition (condition)
  ((format-control :initarg :format-control :initform nil)
   (format-arguments :initarg :format-arguments :initform nil)))
(defclass simple-error (simple-condition error) ())
(defclass simple-warning (simple-condition warning) ())
(defclass style-warning (warning) ())
(defclass type-error (error)
  ((datum :initarg :datum :initform nil)
   (expected-type :initarg :expected-type :initform nil)))
(defclass simple-type-error (simple-condition type-error) ())
(defclass program-error (error) ())
(defclass control-error (error) ())
(defclass arithmetic-error (error)
  ((operation :initarg :operation :initform nil)
   (operands :initarg :operands :initform nil)))
(defclass division-by-zero (arithmetic-error) ())
(defclass floating-point-overflow (arithmetic-error) ())
(defclass cell-error (error) ((name :initarg :name :initform nil)))
(defclass unbound-variable (cell-error) ())
(defclass undefined-function (cell-error) ())
(defclass unbound-slot (cell-error) ((instance :initarg :instance :initform nil)))
(defclass package-error (error) ((package :initarg :package :initform nil)))
(defclass stream-error (error) ((stream :initarg :stream :initform nil)))
(defclass end-of-file (stream-error) ())
(defclass file-error (error) ((pathname :initarg :pathname :initform nil)))
(defclass parse-error (error) ())
(defclass reader-error (parse-error stream-error) ())
(defclass storage-condition (serious-condition) ())

(defun condition-class-p (class)
  (and class (memq (find-class 'condition) (class-precedence-list class)) t))

;; The class of a kernel condition: its type, if that names a condition
;; class; else the kind of error its message says; else SIMPLE-ERROR.
(defvar *kernel-error-classes*
  '(("Division by zero" . division-by-zero)
    ("Floating-point overflow" . floating-point-overflow)
    ("Unbound variable" . unbound-variable)
    ("Undefined function" . undefined-function)
    ("End of file" . end-of-file)
    ("No package named" . package-error)
    ("Not a" . type-error)
    ("Index out of range" . type-error)
    ("Too few arguments" . program-error)
    ("Too many arguments" . program-error)
    ("There is no restart" . control-error)
    ("Stack overflow" . storage-condition)))

(defun %string-prefix-p (prefix s)
  (and (stringp s) (>= (length s) (length prefix)) (string= prefix (substring s 0 (length prefix)))))

(defun %kernel-condition-class (c)
  (let ((type (condition-type c)))
    (or (and (not (eq type 'error)) (let ((k (find-class-or-nil type))) (and (condition-class-p k) k)))
        (let ((m (%condition-message c)) (r nil))
          (dolist (e *kernel-error-classes*)
            (when (and (null r) (%string-prefix-p (car e) m)) (setq r (find-class (cdr e)))))
          r)
        (find-class 'simple-error))))

(defun find-class-or-nil (name) (and (symbolp name) (find-class name nil)))

(defun condition-object-p (x)
  (or (conditionp x) (and (instancep x) (condition-class-p (class-of x)))))

;; the kernel's condition accessors, kept for kernel conditions
(fset '%condition-message (symbol-function 'condition-message))
(fset '%condition-irritants (symbol-function 'condition-irritants))
(fset '%condition-type (symbol-function 'condition-type))

;; ... and the same names for both kinds
(defun condition-type (c) (if (conditionp c) (%condition-type c) (class-name (class-of c))))
(defun condition-message (c) (if (conditionp c) (%condition-message c) (condition-report-string c)))
(defun condition-irritants (c) (if (conditionp c) (%condition-irritants c) nil))

;; a type a handler names: a class name, or (or ...), (and ...), (not ...)
(defun %condition-matches-p (object type)
  (cond ((eq type t) t)
        ((consp type)
         (case (car type)
           (or (some (lambda (ty) (%condition-matches-p object ty)) (cdr type)))
           (and (every (lambda (ty) (%condition-matches-p object ty)) (cdr type)))
           (not (not (%condition-matches-p object (cadr type))))
           (t nil)))
        ((and (conditionp object) (eq (%condition-type object) type)) t)
        (t (let ((k (find-class-or-nil type)))
             (and k (memq k (class-precedence-list (class-of object))) t)))))

;;; ---- reports ------------------------------------------------------------------------

;; how a condition reads: a kernel condition's message and irritants; for
;; an instance, the :REPORT of its class or of a superclass
(defun condition-report-string (c)
  (if (conditionp c)
      (let ((s (%condition-message c)))
        (dolist (i (%condition-irritants c)) (setq s (string-append s " " (prin1-to-string i))))
        s)
      (let ((report nil))
        (dolist (k (class-precedence-list (class-of c)))
          (when (and (null report) (get (class-name k) 'condition-report))
            (setq report (get (class-name k) 'condition-report))))
        (cond ((stringp report) report)
              (report (with-output-to-string (s) (funcall report c s)))
              ((typep c 'simple-condition)
               (apply #'format nil (or (slot-value c 'format-control) "~A")
                      (slot-value c 'format-arguments)))
              (t (format nil "Condition ~S was signalled." (class-name (class-of c))))))))

(putprop 'simple-condition nil 'condition-report)
(putprop 'type-error
         (lambda (c s) (format s "~S is not of type ~S." (type-error-datum c) (type-error-expected-type c)))
         'condition-report)
(putprop 'unbound-variable (lambda (c s) (format s "The variable ~S is unbound." (cell-error-name c)))
         'condition-report)
(putprop 'undefined-function (lambda (c s) (format s "The function ~S is undefined." (cell-error-name c)))
         'condition-report)
(putprop 'unbound-slot
         (lambda (c s) (format s "The slot ~S is unbound in ~S." (cell-error-name c) (%slot-or c 'instance nil)))
         'condition-report)
(putprop 'division-by-zero "Division by zero" 'condition-report)
(putprop 'end-of-file "End of file" 'condition-report)

;; PRINC (and ~A) of a condition writes its report, as in Common Lisp:
;; see printer.lisp

;;; ---- accessors, for kernel conditions and instances alike ----------------------------

(defun %slot-or (c slot default)
  (if (and (instancep c) (slot-boundp c slot)) (slot-value c slot) default))

(defun simple-condition-format-control (c)
  (if (conditionp c) (%condition-message c) (%slot-or c 'format-control nil)))
(defun simple-condition-format-arguments (c)
  (if (conditionp c) (%condition-irritants c) (%slot-or c 'format-arguments nil)))
(defun type-error-datum (c)
  (if (conditionp c) (car (%condition-irritants c)) (%slot-or c 'datum nil)))
(defun type-error-expected-type (c)
  (if (conditionp c)
      (let ((m (%condition-message c)))       ; "Not a list:" => LIST
        (if (and (%string-prefix-p "Not a" m) (> (length m) 6))
            (let* ((words (string-trim ":" (substring m (if (%string-prefix-p "Not an " m) 7 6))))
                   (end (or (position #\Space words) (length words))))
              (intern (string-upcase (substring words 0 end)) (find-package "COMMON-LISP")))
            t))
      (%slot-or c 'expected-type nil)))
(defun cell-error-name (c)
  (if (conditionp c) (car (%condition-irritants c)) (%slot-or c 'name nil)))
(defun arithmetic-error-operation (c) (%slot-or c 'operation nil))
(defun arithmetic-error-operands (c) (%slot-or c 'operands nil))
(defun package-error-package (c)
  (if (conditionp c) (car (%condition-irritants c)) (%slot-or c 'package nil)))
(defun stream-error-stream (c)
  (if (conditionp c) (car (%condition-irritants c)) (%slot-or c 'stream nil)))
(defun file-error-pathname (c) (%slot-or c 'pathname nil))

;;; ---- DEFINE-CONDITION and MAKE-CONDITION -----------------------------------------------

;; (define-condition name (parents...) (slots...) (:report r) (:documentation d))
;; R is a string, or a function of the condition and a stream
(defmacro define-condition (name parents slots &rest options)
  (let ((report (cadr (assq :report options))))
    `(progn
       (defclass ,name ,(or parents '(condition)) ,slots)
       ,(cond ((null report) nil)
              ((stringp report) `(putprop ',name ,report 'condition-report))
              (t `(putprop ',name (function ,report) 'condition-report)))
       ',name)))

(defun make-condition (type &rest initargs)
  (let ((k (find-class-or-nil type)))
    (unless (condition-class-p k) (error "MAKE-CONDITION: not a condition type:" type))
    (apply #'make-instance type initargs)))

;; DATUM and ARGUMENTS as ERROR, SIGNAL and WARN take them: a condition, a
;; condition type and initargs, or a format control and arguments (DEFAULT
;; is the type then)
(defun %coerce-to-condition (datum arguments default)
  (cond ((condition-object-p datum) datum)
        ((and (symbolp datum) (condition-class-p (find-class-or-nil datum)))
         (apply #'make-condition datum arguments))
        ((stringp datum)
         (make-condition default :format-control datum :format-arguments arguments))
        (t (error "Not a condition designator:" datum))))

;; the kernel condition that carries condition object C
(defun %carrier (c)
  (if (conditionp c)
      c
      (%make-condition (class-name (class-of c)) (condition-report-string c) nil c)))

;; Common Lisp's ERROR. A string with no ~ directives keeps the kernel's way,
;; (error "Not found:" x) with irritants; one with directives is a format
;; control; a symbol that is not a condition type is a typed kernel error.
(defun error (datum &rest arguments)
  (cond ((and (stringp datum) (not (%char-index #\~ datum 0 (length datum))))
         (apply #'%error datum arguments))
        ((and (symbolp datum) (not (condition-class-p (find-class-or-nil datum))))
         (apply #'%error datum arguments))
        (t (%resignal (%carrier (%coerce-to-condition datum arguments 'simple-error))))))

;; the handlers have it; if none takes it, SIGNAL returns NIL
(defun signal (datum &rest arguments)
  (let ((c (%coerce-to-condition datum arguments 'simple-condition)))
    (%run-handlers c)
    nil))

(defun warn (datum &rest arguments)
  (let ((c (%coerce-to-condition datum arguments 'simple-warning)))
    (restart-case
        (progn (%run-handlers c)
               (format *error-output* "~&;; Warning: ~A~%" (condition-report-string c)))
      (muffle-warning () :report "Do not print the warning" nil))
    nil))

(defun muffle-warning (&optional c) (declare-ignored c) (invoke-restart 'muffle-warning))
(defun use-value (value &optional c) (declare-ignored c)
  (let ((r (find-restart 'use-value))) (when r (invoke-restart r value))))
(defun store-value (value &optional c) (declare-ignored c)
  (let ((r (find-restart 'store-value))) (when r (invoke-restart r value))))

(defun invoke-debugger (condition)
  (%resignal (%carrier condition)))

(defmacro assert (test &optional places datum &rest arguments)
  (declare-ignored places)
  `(unless ,test
     ,(if datum
          `(error ,datum ,@arguments)
          `(error "Assertion failed:" ',test))))
