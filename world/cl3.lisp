;;; -*- Mode: Lisp -*-
;;; cl3.lisp -- Common Lisp, part 6 (M17): what ordinary programs still
;;; missed. Slots as variables (WITH-SLOTS, WITH-ACCESSORS), the remaining
;;; forms (EVAL-WHEN, PROGV, LOAD-TIME-VALUE, DEFINE-MODIFY-MACRO,
;;; DEFINE-SYMBOL-MACRO ...), documentation strings, the developer's tools
;;; (TRACE, TIME, APROPOS) and the clock.
;;;
;;; The object system's part of M17 (multiple dispatch, EQL specializers,
;;; unbound slots, CHANGE-CLASS, redefining classes) is in objects.lisp, and
;;; SYMBOL-MACROLET in the compiler.

;;; ---- slots as variables ------------------------------------------------------

;; (with-slots (x (my-y y)) point body): X and MY-Y stand for the slots
(defmacro with-slots (slots instance &body body)
  (let ((g (gensym)))
    `(let ((,g ,instance))
       (symbol-macrolet ,(mapcar (lambda (s)
                                   (if (consp s)
                                       (list (car s) (list 'slot-value g (list 'quote (cadr s))))
                                       (list s (list 'slot-value g (list 'quote s)))))
                                 slots)
         ,@body))))

;; (with-accessors ((x point-x) (y point-y)) point body)
(defmacro with-accessors (entries instance &body body)
  (let ((g (gensym)))
    `(let ((,g ,instance))
       (symbol-macrolet ,(mapcar (lambda (e) (list (car e) (list (cadr e) g))) entries)
         ,@body))))

(defmacro define-symbol-macro (name expansion)
  `(progn (putprop ',name ',expansion 'symbol-macro) ',name))

;;; ---- forms ----------------------------------------------------------------------

;; Everything here is loaded by evaluating it, so a body runs when it would
;; at load or execution time, and not otherwise
(defmacro eval-when (situations &body body)
  (if (intersection situations '(:execute :load-toplevel eval load))
      `(progn ,@body)
      nil))

;; bind SYMBOLS (a list computed at run time) to VALUES, dynamically
(defmacro progv (symbols values &body body)
  (let ((h (gensym)))
    `(let ((,h (%progv-bind ,symbols ,values)))
       (unwind-protect (progn ,@body) (%progv-unbind ,h)))))

;; FORM's value, worked out the first time the code runs, then kept
(defmacro load-time-value (form &optional read-only-p)
  (declare (ignore read-only-p))
  `(%load-time-value ',(list '%unset) (lambda () ,form)))

(defun %load-time-value (cell f)
  (when (eq (car cell) '%unset) (setf (car cell) (funcall f)))
  (car cell))

;; (define-modify-macro appendf (&rest lists) append): (appendf x '(1)) sets X
;; to (append x '(1)). The place's subforms are evaluated twice.
(defmacro define-modify-macro (name lambda-list function &optional doc)
  (let ((place (gensym)) (args nil) (rest nil) (mode nil))
    (dolist (p lambda-list)
      (cond ((eq p '&optional) (setq mode :optional))
            ((eq p '&rest) (setq mode :rest))
            ((eq mode :rest) (setq rest (if (consp p) (car p) p)))
            (t (push (if (consp p) (car p) p) args))))
    `(progn
       (defmacro ,name (,place ,@lambda-list)
         ,@(and doc (list doc))
         (list 'setf ,place (list* ',function ,place ,@(reverse args) ,rest)))
       ',name)))

(defmacro ccase (key &body clauses) `(ecase ,key ,@clauses))
(defmacro ctypecase (key &body clauses) `(etypecase ,key ,@clauses))

;; accepted; the compiler does not use them
(defmacro define-compiler-macro (name lambda-list &body body)
  `(progn (putprop ',name (lambda ,lambda-list ,@body) 'compiler-macro) ',name))
(defun compiler-macro-function (name) (get name 'compiler-macro))

;; DEFVAR, DEFPARAMETER and DEFCONSTANT with a documentation string
(defmacro defvar (name &optional value doc)
  `(progn (%proclaim-special ',name)
          (if (boundp ',name) nil (set ',name ,value))
          ,@(and doc `((%set-documentation ',name 'variable ,doc)))
          ',name))

(defmacro defparameter (name value &optional doc)
  `(progn (%proclaim-special ',name) (set ',name ,value)
          ,@(and doc `((%set-documentation ',name 'variable ,doc)))
          ',name))

(defmacro defconstant (name value &optional doc)
  `(progn (set ',name ,value)
          ,@(and doc `((%set-documentation ',name 'variable ,doc)))
          ',name))

;;; ---- lists and sequences ----------------------------------------------------------

(defun %remf (plist indicator)              ; => (new-plist . found)
  (cond ((null plist) (cons nil nil))
        ((eq (car plist) indicator) (cons (cddr plist) t))
        (t (let ((r (%remf (cddr plist) indicator)))
             (cons (cons (car plist) (cons (cadr plist) (car r))) (cdr r))))))

(defmacro remf (place indicator)
  (let ((r (gensym)))
    `(let ((,r (%remf ,place ,indicator)))
       (setf ,place (car ,r))
       (cdr ,r))))

;; => the first indicator of INDICATORS on PLIST, its value, and the tail there
(defun get-properties (plist indicators)
  (let ((tail plist))
    (while (and tail (not (memq (car tail) indicators))) (setq tail (cddr tail)))
    (if tail (values (car tail) (cadr tail) tail) (values nil nil nil))))

(defun sublis (alist tree &key (test #'eql) key)
  (let ((found (assoc-if (lambda (k) (funcall test k (if key (funcall key tree) tree))) alist)))
    (cond (found (cdr found))
          ((consp tree) (cons (sublis alist (car tree) :test test :key key)
                              (sublis alist (cdr tree) :test test :key key)))
          (t tree))))

(defun nsublis (alist tree &rest keys) (apply #'sublis alist tree keys))
(defun nsubst (new old tree) (subst new old tree))

;; RESULT's elements from F applied to the elements of SEQUENCES in turn
(defun map-into (result f &rest sequences)
  (let* ((lists (mapcar #'sequence-list sequences))
         (n (apply #'min (length result) (mapcar #'length lists))))
    (dotimes (i n)
      (%set-elt result i (apply f (mapcar #'car lists)))
      (setq lists (mapcar #'cdr lists)))
    result))

;;; ---- lists with :TEST and :KEY ---------------------------------------------------
;;;
;;; Without a test these compare with EQUAL, as they always have here (Lisp
;;; Machine Lisp); :TEST, :TEST-NOT and :KEY work as in Common Lisp.

(defun %list-test (test test-not key)
  (%test (or test (and (not test-not) #'equal)) test-not key))

(defun member (item list &rest keys)
  (if keys
      (apply #'%member-keyed item list keys)
      (progn (while (and list (not (equal item (car list)))) (setq list (cdr list)))
             list)))

(defun %member-keyed (item list &key test test-not key)
  (let ((f (%list-test test test-not key)))
    (while (and list (not (funcall f item (car list)))) (setq list (cdr list)))
    list))

(defun member-if (pred list &key key)
  (while (and list (not (funcall pred (if key (funcall key (car list)) (car list))))) (setq list (cdr list)))
  list)

(defun member-if-not (pred list &key key)
  (member-if (lambda (x) (not (funcall pred x))) list :key key))

(defun assoc (item alist &rest keys)
  (if keys
      (apply #'%assoc-keyed item alist keys)
      (progn (while (and alist (not (and (car alist) (equal item (caar alist))))) (setq alist (cdr alist)))
             (car alist))))

(defun %assoc-keyed (item alist &key test test-not key)
  (let ((f (%list-test test test-not key)))
    (dolist (pair alist nil) (when (and pair (funcall f item (car pair))) (return pair)))))

(defun rassoc (item alist &key test test-not key)
  (let ((f (%list-test test test-not key)))
    (dolist (pair alist nil) (when (and pair (funcall f item (cdr pair))) (return pair)))))

(defun rassoc-if (pred alist &key key)
  (dolist (pair alist nil)
    (when (and pair (funcall pred (if key (funcall key (cdr pair)) (cdr pair)))) (return pair))))

;; ITEM, with KEY applied as the set functions apply it to their elements
(defun %keyed (x keys) (let ((k (getf keys :key))) (if k (funcall k x) x)))

(defun adjoin (item list &rest keys)
  (if (apply #'member (%keyed item keys) list keys) list (cons item list)))

(defmacro pushnew (item place &rest keys)
  `(setf ,place (adjoin ,item ,place ,@keys)))

(defun union (a b &rest keys)
  (let ((r b)) (dolist (x a) (unless (apply #'member (%keyed x keys) b keys) (push x r))) r))
(defun intersection (a b &rest keys)
  (remove-if-not (lambda (x) (apply #'member (%keyed x keys) b keys)) a))
(defun set-difference (a b &rest keys)
  (remove-if (lambda (x) (apply #'member (%keyed x keys) b keys)) a))
(defun set-exclusive-or (a b &rest keys)
  (append (apply #'set-difference a b keys) (apply #'set-difference b a keys)))
(defun subsetp (a b &rest keys)
  (every (lambda (x) (apply #'member (%keyed x keys) b keys)) a))
(defun nunion (a b &rest keys) (apply #'union a b keys))
(defun nintersection (a b &rest keys) (apply #'intersection a b keys))
(defun nset-difference (a b &rest keys) (apply #'set-difference a b keys))

;; EVERY, SOME, NOTANY and NOTEVERY on any sequences, one or several
(defun %map-while (pred sequences stop-when result-if-stopped result-at-end)
  (let ((lists (mapcar #'sequence-list sequences)))
    (loop
      (when (some #'null lists) (return result-at-end))
      (let ((v (apply pred (mapcar #'car lists))))
        (when (funcall stop-when v) (return (funcall result-if-stopped v))))
      (setq lists (mapcar #'cdr lists)))))

(defun every (pred seq &rest more)
  (if (and (null more) (listp seq))
      (let ((ok t)) (while (and seq ok) (setq ok (funcall pred (car seq)) seq (cdr seq))) (and ok t))
      (%map-while pred (cons seq more) #'not (lambda (v) v nil) t)))

(defun some (pred seq &rest more)
  (if (and (null more) (listp seq))
      (let ((found nil)) (while (and seq (not found)) (setq found (funcall pred (car seq)) seq (cdr seq))) found)
      (%map-while pred (cons seq more) #'identity #'identity nil)))

(defun notany (pred seq &rest more) (not (apply #'some pred seq more)))
(defun notevery (pred seq &rest more) (not (apply #'every pred seq more)))

;;; ---- documentation ----------------------------------------------------------------

;; (documentation 'name 'function), 'variable, 'type, 'structure; or of a function
(defun documentation (x doc-type)
  (let ((name (if (functionp x) (%function-name x) x)))
    (and (symbolp name) name (get name (%doc-key doc-type)))))

(defun %set-documentation-value (x doc-type doc)
  (%set-documentation (if (functionp x) (%function-name x) x) doc-type doc))
(defsetf documentation %set-documentation-value)

;;; ---- the developer's tools --------------------------------------------------------

(defvar *trace-output* nil)                 ; NIL: *STANDARD-OUTPUT*
(defvar *traced* nil)                       ; the names being traced
(defvar *trace-depth* 0)

(defun %trace-stream () (or *trace-output* *standard-output*))

(defun %trace-1 (name)
  (unless (fboundp name) (error "TRACE: no function named" name))
  (unless (memq name *traced*)
    (let ((f (symbol-function name)))
      (putprop name f 'traced-function)
      (fset name (lambda (&rest args)
                   (let ((s (%trace-stream)) (depth *trace-depth*))
                     (fresh-line s)
                     (write-string (make-string (* 2 (min depth 20)) :initial-element #\Space) s)
                     (format s "~D: ~S~%" depth (cons name args))
                     (let ((values (let ((*trace-depth* (1+ depth))) (multiple-value-list (apply f args)))))
                       (fresh-line s)
                       (write-string (make-string (* 2 (min depth 20)) :initial-element #\Space) s)
                       (format s "~D: ~S returned~{ ~S~}~%" depth name values)
                       (values-list values)))))
      (push name *traced*)))
  name)

(defun %untrace-1 (name)
  (when (memq name *traced*)
    (fset name (get name 'traced-function))
    (putprop name nil 'traced-function)
    (setq *traced* (remove name *traced*)))
  name)

;; (trace f g) traces them; (trace) => what is traced
(defmacro trace (&rest names)
  (if names `(mapcar #'%trace-1 ',names) '(reverse *traced*)))

;; (untrace f) stops; (untrace) stops them all
(defmacro untrace (&rest names)
  (if names `(mapcar #'%untrace-1 ',names) '(mapcar #'%untrace-1 (reverse *traced*))))

;; (time form): FORM's values, after saying on *TRACE-OUTPUT* how long it took
(defmacro time (form)
  `(%time (lambda () ,form)))

(defun %time (thunk)
  (let* ((stats (%heap-stats)) (start (get-internal-real-time))
         (values (multiple-value-list (funcall thunk)))
         (end (get-internal-real-time)) (after (%heap-stats)) (s (%trace-stream)))
    (fresh-line s)
    (format s "; ~:D ms, ~D collection~:P~%" (- end start) (- (third after) (third stats)))
    (values-list values)))

(defvar internal-time-units-per-second 1000)

;; the symbols whose names contain STRING (in PACKAGE, or in every package)
(defun apropos-list (string &optional package)
  (let ((s (string-upcase (string string))) (r nil))
    (if package
        (do-symbols (x (find-package package)) (when (search s (symbol-name x)) (pushnew x r)))
        (do-all-symbols (x) (when (search s (symbol-name x)) (pushnew x r))))
    (sort r #'string< :key #'symbol-name)))

(defun apropos (string &optional package)
  (dolist (x (apropos-list string package))
    (fresh-line)
    (prin1 x)
    (when (fboundp x) (princ (if (macro-function x) "  (macro)" "  (function)")))
    (when (boundp x) (princ "  (variable)"))
    (terpri))
  (values))

;;; ---- the clock ---------------------------------------------------------------------
;;;
;;; Universal time counts seconds from 1900-01-01 00:00 UTC; the host gives
;;; seconds from 1970 (%WALL-CLOCK) and the local time zone (%UTC-OFFSET).
;;; A time zone here is as in Common Lisp: hours WEST of Greenwich.

(defconstant %unix-epoch 2208988800)        ; 1970-01-01 in universal time

;; minutes east of UTC => hours west: a ratio for a zone off the hour (-11/2 for +5:30)
(defun %zone (minutes-east) (/ (- minutes-east) 60))

(defun get-universal-time () (+ (%wall-clock) %unix-epoch))

;; days since 1970-01-01 => (year month day), the proleptic Gregorian calendar
(defun %civil-from-days (z)
  (let* ((z (+ z 719468))
         (era (floor z 146097))
         (doe (- z (* era 146097)))
         (yoe (floor (- doe (floor doe 1460) (- (floor doe 36524)) (floor doe 146096)) 365))
         (doy (- doe (- (+ (* 365 yoe) (floor yoe 4)) (floor yoe 100))))
         (mp (floor (+ (* 5 doy) 2) 153))
         (d (+ (- doy (floor (+ (* 153 mp) 2) 5)) 1))
         (m (if (< mp 10) (+ mp 3) (- mp 9)))
         (y (+ yoe (* era 400) (if (<= m 2) 1 0))))
    (list y m d)))

(defun %days-from-civil (y m d)
  (let* ((y (if (<= m 2) (1- y) y))
         (era (floor y 400))
         (yoe (- y (* era 400)))
         (doy (+ (floor (+ (* 153 (if (> m 2) (- m 3) (+ m 9))) 2) 5) (1- d)))
         (doe (+ (* yoe 365) (floor yoe 4) (- (floor yoe 100)) doy)))
    (+ (* era 146097) doe -719468)))

;; => second minute hour date month year day-of-week daylight-p zone
(defun decode-universal-time (universal-time &optional time-zone)
  (let* ((unix (- universal-time %unix-epoch))
         (zone (or time-zone (%zone (%utc-offset unix))))   ; local: hours west
         (local (- unix (round (* zone 3600))))
         (days (floor local 86400))
         (secs (- local (* days 86400)))
         (ymd (%civil-from-days days)))
    (values (rem secs 60) (rem (floor secs 60) 60) (floor secs 3600)
            (third ymd) (second ymd) (first ymd)
            (rem (+ days 3) 7)                 ; 1970-01-01 was a Thursday; Monday is 0
            nil zone)))

(defun encode-universal-time (second minute hour date month year &optional time-zone)
  (let* ((days (%days-from-civil year month date))
         (local (+ (* days 86400) (* hour 3600) (* minute 60) second))
         (zone (or time-zone (%zone (%utc-offset (- local (* 60 (%utc-offset local))))))))
    (+ local (round (* zone 3600)) %unix-epoch)))

(defun get-decoded-time () (decode-universal-time (get-universal-time)))
