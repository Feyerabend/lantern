;;; -*- Mode: Lisp -*-
;;; pretty.lisp -- the pretty printer (M16): logical blocks, conditional
;;; newlines and indentation as Common Lisp has them, code laid out the way
;;; it is written, and the pprint dispatch table.
;;;
;;; Printing to a pretty stream does not write anything yet. It builds a
;;; tree of logical blocks. A block holds:
;;;   - text (strings, never with a newline in them)
;;;   - conditional newlines: (:newline kind), kind :linear, :fill, :miser,
;;;     :mandatory, :literal (a newline in the text) or :fresh (FRESH-LINE)
;;;   - indentation changes: (:indent :block n) or (:indent :current n)
;;;   - the blocks nested in it
;;; When the outermost block ends, the tree is laid out against the right
;;; margin and written to the real stream in one piece:
;;;   - a block that fits on the rest of the line is printed flat
;;;   - otherwise its :linear newlines are all taken
;;;   - its :fill newlines are taken when the next section does not fit, or
;;;     when the section before them took more than one line
;;;   - :miser newlines are never taken (there is no miser style)

(defvar *print-miser-width* 40)       ; accepted
(defvar *print-lines* nil)            ; accepted
(defvar *%pp-depth* 0)                ; logical blocks we are inside, for *PRINT-LEVEL*

(defstruct pp-block prefix per-line suffix items width)
(defstruct pp-state stack)            ; the open blocks, innermost first

(defun pretty-stream-p (s) (and (streamp s) (pp-state-p (stream-data s))))

(defun %pp-stream (state)
  (make-stream (lambda (s string) (%pp-text (stream-data s) string))
               (lambda (s) (%pp-item (stream-data s) '(:newline :fresh)))
               state))

(defun %pp-item (state item)
  (push item (pp-block-items (car (pp-state-stack state)))))

;; text: a newline in it is a newline that must be taken
(defun %pp-text (state string)
  (let ((start 0) (n (length string)) (i 0))
    (while (< i n)
      (when (eq (aref string i) #\Newline)
        (when (> i start) (%pp-item state (substring string start i)))
        (%pp-item state '(:newline :literal))
        (setq start (1+ i)))
      (setq i (1+ i)))
    (when (< start n) (%pp-item state (if (= start 0) string (substring string start n))))))

(defun %pp-open (state prefix per-line suffix)
  (push (make-pp-block :prefix (or per-line prefix "") :per-line per-line :suffix (or suffix ""))
        (pp-state-stack state)))

;; => the block, which is put in the one around it if there is one
(defun %pp-close (state)
  (let ((b (pop (pp-state-stack state))))
    (setf (pp-block-items b) (nreverse (pp-block-items b)))
    (when (pp-state-stack state) (%pp-item state b))
    b))

;;; ---- the stream's column and width -------------------------------------------------

(defun %stream-column (s)
  (let ((d (stream-data s)))
    (cond ((and (consp d) (listp (car d)) (every #'stringp (car d)))   ; a string output stream
           (let ((col 0) (done nil))
             (dolist (chunk (car d))
               (unless done
                 (let ((nl (position #\Newline chunk :from-end t)))
                   (if nl
                       (setq col (+ col (- (length chunk) nl 1)) done t)
                       (setq col (+ col (length chunk)))))))
             col))
          ((and (instancep d) (fboundp 'listener-term) (slot-boundp d 'term))
           (or (ignore-errors (funcall 'listener-column (listener-term d))) 0))
          (t 0))))

(defun %stream-margin (s)
  (let ((d (stream-data s)))
    (and (instancep d) (fboundp 'listener-columns) (slot-boundp d 'term)    ; listener.lisp, loaded later
         (ignore-errors (funcall 'listener-columns d)))))

(defun %pp-margin (s) (or *print-right-margin* (%stream-margin s) 80))

;;; ---- PPRINT-LOGICAL-BLOCK and friends ------------------------------------------------

(defun %pprint-logical-block (stream object prefix per-line suffix fn)
  (let ((stream (if (or (null stream) (eq stream t)) *standard-output* stream)))
    (cond ((not (listp object)) (write object :stream stream))
          ((and *print-level* (>= *%pp-depth* *print-level*)) (write-string "#" stream))
          ((pretty-stream-p stream)
           (let ((state (stream-data stream)) (*%pp-depth* (1+ *%pp-depth*)))
             (%pp-open state prefix per-line suffix)
             (unwind-protect (funcall fn stream object)
               (%pp-close state))))
          (t (let* ((state (make-pp-state)) (ps (%pp-stream state)) (b nil))
               (let ((*%pp-depth* (1+ *%pp-depth*)))
                 (%pp-open state prefix per-line suffix)
                 (unwind-protect (funcall fn ps object)
                   (setq b (%pp-close state))))
               (write-string (%pp-layout b (%stream-column stream) (%pp-margin stream)) stream))))
    nil))

;; the tail is not a list, or *PRINT-LENGTH* is reached, or a shared tail
;; under *PRINT-CIRCLE*: => T after printing what stands for the rest
(defun %pp-pop-stop (list count stream)
  (cond ((and list (atom list)) (write-string ". " stream) (write list :stream stream) t)
        ((and *print-length* (>= count *print-length*)) (when list (write-string "..." stream)) t)
        ((and (> count 0) (consp list) (%circle-shared-p list))
         (write-string ". " stream) (write list :stream stream) t)
        (t nil)))

(defmacro pprint-logical-block (spec &body body)
  (destructuring-bind (stream-symbol object &key (prefix "") per-line-prefix (suffix "")) spec
    (let* ((var (if (or (null stream-symbol) (eq stream-symbol t)) '*standard-output* stream-symbol))
           (list (gensym)) (count (gensym)) (blk (gensym))
           (pop-form `(if (%pp-pop-stop ,list ,count ,var)
                          (return-from ,blk nil)
                          (progn (setq ,count (1+ ,count)) (pop ,list))))
           (exit-form `(when (null ,list) (return-from ,blk nil))))
      `(%pprint-logical-block ,var ,object ,prefix ,per-line-prefix ,suffix
                              (lambda (,var ,list)
                                (let ((,count 0))
                                  (declare (ignorable ,count))
                                  (block ,blk
                                    (macrolet ((pprint-pop () ',pop-form)
                                               (pprint-exit-if-list-exhausted () ',exit-form))
                                      ,@body))))))))

(defun pprint-newline (kind &optional stream)
  (let ((s (resolve-stream stream)))
    (when (and *print-pretty* (pretty-stream-p s))
      (%pp-item (stream-data s) (list :newline kind))))
  nil)

(defun pprint-indent (relative-to n &optional stream)
  (let ((s (resolve-stream stream)))
    (when (and *print-pretty* (pretty-stream-p s))
      (%pp-item (stream-data s) (list :indent relative-to n))))
  nil)

(defun pprint-tab (kind colnum colinc &optional stream)     ; accepted
  (declare (ignore kind colnum colinc stream))
  nil)

;;; ---- layout -----------------------------------------------------------------------------

(defvar *%pp-out* nil)          ; the pieces written, newest first
(defvar *%pp-col* 0)
(defvar *%pp-margin* 80)
(defvar *%pp-lines* nil)        ; per-line prefixes in force: ((column . prefix) ...), innermost first
(defvar *%pp-breaks* 0)         ; newlines written so far

(defconstant %pp-never 1000000) ; the width of something that cannot be flat

(defun %pp-width (item)
  (cond ((stringp item) (length item))
        ((pp-block-p item)
         (or (pp-block-width item)
             (setf (pp-block-width item)
                   (min %pp-never
                        (let ((w (+ (length (pp-block-prefix item)) (length (pp-block-suffix item)))))
                          (dolist (i (pp-block-items item)) (setq w (+ w (%pp-width i))))
                          w)))))
        ((and (eq (car item) :newline) (memq (cadr item) '(:mandatory :literal))) %pp-never)
        (t 0)))

;; the width of ITEMS up to the next newline at this level => (values width
;; newline-found); blanks just before that newline do not count, as a line
;; broken there loses them
(defun %pp-section (items)
  (let ((w 0) (found nil) (blanks 0))
    (while (and items (not found))
      (let ((i (car items)))
        (cond ((and (consp i) (eq (car i) :newline)) (setq found t))
              (t (setq w (+ w (%pp-width i)))
                 (setq blanks (if (stringp i)
                                  (let ((k (length i)))
                                    (while (and (> k 0) (eq (aref i (1- k)) #\Space)) (setq k (1- k)))
                                    (if (= k 0) (+ blanks (length i)) (- (length i) k)))
                                  0)))))
      (setq items (cdr items)))
    (values (if found (- w blanks) w) found)))

(defun %pp-emit (s)
  (when (> (length s) 0)
    (push s *%pp-out*)
    (setq *%pp-col* (+ *%pp-col* (length s)))))

(defun %pp-break (indent)
  ;; no blanks at the end of a line
  (while (and *%pp-out* (> (length (car *%pp-out*)) 0)
              (eq (aref (car *%pp-out*) (1- (length (car *%pp-out*)))) #\Space))
    (setf (car *%pp-out*) (string-right-trim " " (car *%pp-out*)))
    (when (= (length (car *%pp-out*)) 0) (pop *%pp-out*)))
  (push *newline-string* *%pp-out*)
  (setq *%pp-col* 0 *%pp-breaks* (1+ *%pp-breaks*))
  (dolist (p (reverse *%pp-lines*))
    (%pp-emit (spaces (- (car p) *%pp-col*)))
    (%pp-emit (cdr p)))
  (%pp-emit (spaces (- indent *%pp-col*))))

;; lay out block B; TRAILING is the width of what must follow it on the same line
(defun %pp-lay (b trailing)
  (let ((pcol *%pp-col*))
    (%pp-emit (pp-block-prefix b))
    (let* ((start *%pp-col*) (indent start)
           (flat (<= (+ pcol (%pp-width b) trailing) *%pp-margin*))
           (*%pp-lines* (if (pp-block-per-line b) (cons (cons pcol (pp-block-per-line b)) *%pp-lines*) *%pp-lines*))
           (section-breaks *%pp-breaks*)
           (items (pp-block-items b)))
      (while items
        (let ((it (car items)))
          (cond ((stringp it) (%pp-emit it))
                ((pp-block-p it)
                 (multiple-value-bind (w found) (%pp-section (cdr items))
                   (%pp-lay it (if found w (+ w (length (pp-block-suffix b)) trailing)))))
                ((eq (car it) :indent)
                 (setq indent (max 0 (+ (if (eq (cadr it) :current) *%pp-col* start) (caddr it)))))
                ((eq (car it) :newline)
                 (when (case (cadr it)
                         ((:mandatory :literal) t)
                         (:fresh (> *%pp-col* indent))
                         (:linear (not flat))
                         (:fill (and (not flat)
                                     (or (> *%pp-breaks* section-breaks)
                                         (multiple-value-bind (w found) (%pp-section (cdr items))
                                           (> (+ *%pp-col* w (if found 0 (+ (length (pp-block-suffix b)) trailing)))
                                              *%pp-margin*)))))
                         (t nil))
                   (%pp-break indent))
                 (setq section-breaks *%pp-breaks*))))
        (setq items (cdr items)))
      (%pp-emit (pp-block-suffix b)))))

(defun %pp-layout (b column margin)
  (let ((*%pp-out* nil) (*%pp-col* column) (*%pp-margin* margin) (*%pp-lines* nil) (*%pp-breaks* 0))
    (%pp-lay b 0)
    (apply #'string-append (reverse *%pp-out*))))

;;; ---- the pprint dispatch table ---------------------------------------------------------

;; ((type function priority) ...); a type is a type name, or (cons type) for
;; a list whose first element is of that type, with (eql x) and (member ...)
(defvar *print-pprint-dispatch* nil)

(defun copy-pprint-dispatch (&optional (table *print-pprint-dispatch*)) (copy-list table))

(defun set-pprint-dispatch (type function &optional (priority 0) (table nil table-p))
  (let ((entries (remove-if (lambda (e) (equal (car e) type))
                            (if table-p table *print-pprint-dispatch*))))
    (when function (push (list type function priority) entries))
    (setq entries (sort entries (lambda (a b) (> (third a) (third b)))))
    (if table-p entries (setq *print-pprint-dispatch* entries))
    nil))

(defun %pp-type-match (x type)
  (cond ((eq type t) t)
        ((and (consp type) (eq (car type) 'eql)) (eql x (cadr type)))
        ((and (consp type) (eq (car type) 'member)) (and (member x (cdr type)) t))
        ((and (consp type) (eq (car type) 'cons))
         (and (consp x) (or (null (cdr type)) (%pp-type-match (car x) (cadr type)))))
        ((and (consp type) (eq (car type) 'satisfies)) (funcall (cadr type) x))
        (t (ignore-errors (typep x type)))))

(defun pprint-dispatch (object &optional (table *print-pprint-dispatch*))
  (let ((e (find-if (lambda (e) (%pp-type-match object (car e))) table)))
    (if e (values (cadr e) t) (values (lambda (s x) (%pprint-default x s)) nil))))

;;; ---- printing objects --------------------------------------------------------------------

;; how WRITE prints a list or vector when *PRINT-PRETTY* is true
(defun %pprint-object (x stream)
  (let ((e (and *print-pprint-dispatch*
                (find-if (lambda (e) (%pp-type-match x (car e))) *print-pprint-dispatch*))))
    (if e (funcall (cadr e) stream x) (%pprint-default x stream))))

(defun %pprint-default (x stream)
  (cond ((not (consp x)) (%pp-vector stream x))
        ((and (memq (car x) '(quote function)) (consp (cdr x)) (null (cddr x)))
         (write-string (if (eq (car x) 'quote) "'" "#'") stream)
         (write (cadr x) :stream stream))
        ((and (symbolp (car x)) (get (car x) '%pp-style)) (funcall (get (car x) '%pp-style) stream x))
        ((and (car x) (symbolp (car x))) (%pp-call stream x))
        (t (pprint-fill stream x))))

(defun pprint-fill (stream list &optional (colon-p t) at-sign-p)
  (declare (ignore at-sign-p))
  (pprint-logical-block (stream list :prefix (if colon-p "(" "") :suffix (if colon-p ")" ""))
    (pprint-exit-if-list-exhausted)
    (loop (write (pprint-pop) :stream stream)
          (pprint-exit-if-list-exhausted)
          (write-char #\Space stream)
          (pprint-newline :fill stream))))

(defun pprint-linear (stream list &optional (colon-p t) at-sign-p)
  (declare (ignore at-sign-p))
  (pprint-logical-block (stream list :prefix (if colon-p "(" "") :suffix (if colon-p ")" ""))
    (pprint-exit-if-list-exhausted)
    (loop (write (pprint-pop) :stream stream)
          (pprint-exit-if-list-exhausted)
          (write-char #\Space stream)
          (pprint-newline :linear stream))))

(defun pprint-tabular (stream list &optional (colon-p t) at-sign-p tabsize)
  (declare (ignore tabsize))
  (pprint-fill stream list colon-p at-sign-p))

(defun %pp-vector (stream v)
  (pprint-logical-block (stream (coerce v 'list) :prefix "#(" :suffix ")")
    (pprint-exit-if-list-exhausted)
    (loop (write (pprint-pop) :stream stream)
          (pprint-exit-if-list-exhausted)
          (write-char #\Space stream)
          (pprint-newline :fill stream))))

;; a function call: the arguments under the first one
(defun %pp-call (stream form)
  (pprint-logical-block (stream form :prefix "(" :suffix ")")
    (write (pprint-pop) :stream stream)
    (pprint-exit-if-list-exhausted)
    (write-char #\Space stream)
    (pprint-indent :current 0 stream)
    (loop (write (pprint-pop) :stream stream)
          (pprint-exit-if-list-exhausted)
          (write-char #\Space stream)
          (pprint-newline :fill stream))))

;; (op arg1 .. argN body...): the N arguments on the first line if they fit,
;; then the body indented by two. FIRST, if given, prints the first argument.
(defun %pp-body (stream form n &optional first)
  (pprint-logical-block (stream form :prefix "(" :suffix ")")
    (write (pprint-pop) :stream stream)
    (pprint-exit-if-list-exhausted)
    (write-char #\Space stream)
    (pprint-indent :current 0 stream)
    (let ((k 0))
      (while (< k n)
        (let ((a (pprint-pop)))
          (if (and first (= k 0)) (funcall first stream a) (write a :stream stream)))
        (setq k (1+ k))
        (pprint-exit-if-list-exhausted)
        (write-char #\Space stream)
        (when (< k n) (pprint-newline :fill stream))))
    (pprint-indent :block 1 stream)
    (loop (pprint-newline :linear stream)
          (write (pprint-pop) :stream stream)
          (pprint-exit-if-list-exhausted)
          (write-char #\Space stream))))

;; ((a 1) (b 2)): one to a line when they do not fit
(defun %pp-bindings (stream list)
  (if (listp list) (pprint-linear stream list) (write list :stream stream)))

;; ((name args body...) ...), as FLET and LABELS have them
(defun %pp-local-functions (stream list)
  (if (listp list)
      (pprint-logical-block (stream list :prefix "(" :suffix ")")
        (pprint-exit-if-list-exhausted)
        (loop (let ((f (pprint-pop))) (if (consp f) (%pp-body stream f 1) (write f :stream stream)))
              (pprint-exit-if-list-exhausted)
              (write-char #\Space stream)
              (pprint-newline :linear stream)))
      (write list :stream stream)))

;; (op x clause...) and (cond clause...): clauses one to a line
(defun %pp-clauses (stream form n)
  (pprint-logical-block (stream form :prefix "(" :suffix ")")
    (write (pprint-pop) :stream stream)
    (pprint-exit-if-list-exhausted)
    (write-char #\Space stream)
    (pprint-indent :current 0 stream)
    (dotimes (k n)
      (write (pprint-pop) :stream stream)
      (pprint-exit-if-list-exhausted)
      (write-char #\Space stream))
    (when (> n 0)                       ; (case x ⏎ clauses...), but (cond clause ⏎ clause...)
      (pprint-indent :block 1 stream)
      (pprint-newline :linear stream))
    (loop (let ((c (pprint-pop))) (if (consp c) (pprint-linear stream c) (write c :stream stream)))
          (pprint-exit-if-list-exhausted)
          (write-char #\Space stream)
          (pprint-newline :linear stream))))

;; (if test then else): then and else under the test
(defun %pp-if (stream form)
  (pprint-logical-block (stream form :prefix "(" :suffix ")")
    (write (pprint-pop) :stream stream)
    (pprint-exit-if-list-exhausted)
    (write-char #\Space stream)
    (pprint-indent :current 0 stream)
    (loop (write (pprint-pop) :stream stream)
          (pprint-exit-if-list-exhausted)
          (write-char #\Space stream)
          (pprint-newline :linear stream))))

;; DEFMETHOD: the name, any qualifiers and the lambda list on the first line
(defun %pp-defmethod (stream form)
  (let ((n 2) (rest (cddr form)))
    (while (and (consp rest) (atom (car rest))) (setq n (1+ n) rest (cdr rest)))
    (%pp-body stream form n)))

(defun %pp-style (symbols function)
  (dolist (s symbols) (putprop s function '%pp-style)))

(%pp-style '(defun defmacro defgeneric define-condition defclass destructuring-bind multiple-value-bind
             define-command deftype)
           (lambda (s x) (%pp-body s x 2)))
(%pp-style '(lambda when unless dolist dotimes block catch unwind-protect handler-bind restart-case
             with-open-file with-output-to-string with-input-from-string with-lock with-open-stream
             defstruct eval-when return-from while pprint-logical-block print-unreadable-object
             multiple-value-prog1 prog1 dolist-reverse defpackage)
           (lambda (s x) (%pp-body s x 1)))
(%pp-style '(progn tagbody loop-finish) (lambda (s x) (%pp-body s x 0)))
(%pp-style '(let let* prog prog* symbol-macrolet) (lambda (s x) (%pp-body s x 1 #'%pp-bindings)))
(%pp-style '(flet labels macrolet) (lambda (s x) (%pp-body s x 1 #'%pp-local-functions)))
(%pp-style '(do do*) (lambda (s x) (%pp-body s x 2 #'%pp-bindings)))
(%pp-style '(defmethod) #'%pp-defmethod)
(%pp-style '(cond) (lambda (s x) (%pp-clauses s x 0)))
(%pp-style '(case ecase ccase typecase etypecase ctypecase handler-case) (lambda (s x) (%pp-clauses s x 1)))
(%pp-style '(if) #'%pp-if)

;;; ---- PPRINT, and FORMAT's logical blocks ----------------------------------------------------

(defun pprint (object &optional stream)
  (let ((s (resolve-stream stream)))
    (terpri s)
    (write object :stream s :pretty t :escape t))
  (values))

;; FORMAT's output so far, PIECES (strings, newline and indent items, blocks),
;; as items of a block; with FILL, a fill newline after each run of blanks
;; in the literal text (it is marked (:fill-text "..."))
(defun %pp-items (pieces)
  (let ((items nil))
    (dolist (p pieces)
      (cond ((stringp p) (%pp-items-text p nil (lambda (i) (push i items))))
            ((and (consp p) (eq (car p) :fill-text)) (%pp-items-text (cadr p) t (lambda (i) (push i items))))
            (t (push p items))))
    (nreverse items)))

(defun %pp-items-text (s fill emit)
  (let ((start 0) (n (length s)) (i 0))
    (while (< i n)
      (let ((c (aref s i)))
        (cond ((eq c #\Newline)
               (when (> i start) (funcall emit (substring s start i)))
               (funcall emit '(:newline :literal))
               (setq start (1+ i)))
              ((and fill (eq c #\Space) (or (= (1+ i) n) (not (eq (aref s (1+ i)) #\Space))))
               (funcall emit (substring s start (1+ i)))
               (funcall emit '(:newline :fill))
               (setq start (1+ i)))))
      (setq i (1+ i)))
    (when (< start n) (funcall emit (substring s start n)))))

;; FORMAT's pieces => its text, laid out if it has logical blocks in it
(defun %fmt-text (pieces column margin)
  (if (every #'stringp pieces)
      (apply #'string-append pieces)
      (%pp-layout (make-pp-block :prefix "" :suffix "" :items (%pp-items pieces)) column margin)))
