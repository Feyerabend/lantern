;;; -*- Mode: Lisp -*-
;;; cl2.lisp -- Common Lisp, part 3 (M13): the forms and data that larger
;;; programs lean on. DO and DESTRUCTURING-BIND (and destructuring lambda
;;; lists in DEFMACRO), arrays of any rank with fill pointers, COERCE,
;;; *FEATURES* for #+ and #-, and input streams with READ. The condition
;;; system's classes are in signals.lisp.

;;; ---- iteration and assignment ---------------------------------------------------

;; assign in parallel: all the new values are computed first
(defmacro psetq (&rest pairs)
  (let ((temps nil) (sets nil))
    (while pairs
      (let ((g (gensym)))
        (push (list g (cadr pairs)) temps)
        (push `(setq ,(car pairs) ,g) sets)
        (setq pairs (cddr pairs))))
    `(let ,(nreverse temps) ,@(nreverse sets) nil)))

(defmacro psetf (&rest pairs)
  (let ((temps nil) (sets nil))
    (while pairs
      (let ((g (gensym)))
        (push (list g (cadr pairs)) temps)
        (push `(setf ,(car pairs) ,g) sets)
        (setq pairs (cddr pairs))))
    `(let ,(nreverse temps) ,@(nreverse sets) nil)))

(defmacro rotatef (&rest places)
  (when places
    (let ((g (gensym)))
      `(let ((,g ,(car places)))
         ,@(let ((r nil) (p places))
             (while (cdr p) (push `(setf ,(car p) ,(cadr p)) r) (setq p (cdr p)))
             (nreverse r))
         (setf ,(car (last places)) ,g)
         nil))))

(defmacro shiftf (&rest places-and-value)
  (let ((g (gensym)) (places (butlast places-and-value)))
    `(let ((,g ,(car places)))
       ,@(let ((r nil) (p places-and-value))
           (while (cdr p) (push `(setf ,(car p) ,(cadr p)) r) (setq p (cdr p)))
           (nreverse r))
       ,g)))

;; PROG and PROG*: a block named NIL around variables around a TAGBODY
(defmacro prog (vars &body body) `(block nil (let ,vars (tagbody ,@body))))
(defmacro prog* (vars &body body) `(block nil (let* ,vars (tagbody ,@body))))

;; a body with tags in it becomes a TAGBODY (DO's body is one)
(defun %tagbody-if-tags (body)
  (if (some (lambda (x) (and x (or (symbolp x) (integerp x)))) body)
      (list (cons 'tagbody body))
      body))

;; (do ((var init [step])...) (end-test result...) body...)
(defun %do (parallel specs end body)
  (setq body (%tagbody-if-tags body))
  (let ((steps nil))
    (dolist (s specs)
      (when (and (consp s) (cddr s)) (push (car s) steps) (push (caddr s) steps)))
    `(block nil
       (,(if parallel 'let 'let*)
        ,(mapcar (lambda (s) (if (consp s) (list (car s) (cadr s)) (list s nil))) specs)
        (while (not ,(car end))
          ,@body
          (,(if parallel 'psetq 'setq) ,@(nreverse steps)))
        ,@(cdr end)))))

(defmacro do (specs end &body body) (%do t specs end body))
(defmacro do* (specs end &body body) (%do nil specs end body))

;;; ---- destructuring --------------------------------------------------------------
;;;
;;; (destructuring-bind (a (b c) &optional (d 4) &rest more &key x) list body...)
;;; turns into a LET* that takes LIST apart: required parts (patterns nest),
;;; &optional with defaults and supplied-p variables, &rest and &body, &key,
;;; &aux, &whole, and a dotted tail as &rest.

(defun %d-next (list pattern)
  (if (consp list) (car list) (error "Too few elements to destructure into" pattern)))

(defun %d-end (list pattern)
  (when list (error "Too many elements to destructure into" pattern)))

(defun %d-key (list key)
  (let ((r '%none))
    (while (and (consp list) (consp (cdr list)) (eq r '%none))
      (when (eq (car list) key) (setq r (cadr list)))
      (setq list (cddr list)))
    r))

;; => LET* bindings that take apart the list in variable G according to PATTERN
(defun %destructure (pattern g)
  (let ((b nil) (state :required) (whole pattern) (rest-seen nil))
    (when (and (consp pattern) (eq (car pattern) '&whole))
      (push (list (cadr pattern) g) b)
      (setq pattern (cddr pattern)))
    (while pattern
      (if (atom pattern)
          (progn (push (list pattern g) b) (setq rest-seen t pattern nil))   ; a dotted tail
          (let ((p (pop pattern)))
            (cond
              ((memq p '(&optional &key &aux)) (setq state p))
              ((memq p '(&rest &body))
               (let ((v (pop pattern)))
                 (setq rest-seen t)
                 (if (consp v)
                     (let ((g2 (gensym))) (push (list g2 g) b) (setq b (append (reverse (%destructure v g2)) b)))
                     (push (list v g) b))))
              ((eq p '&allow-other-keys) nil)
              ((eq p '&environment) (push (list (pop pattern) nil) b))
              ((eq state :required)
               (if (consp p)
                   (let ((g2 (gensym)))
                     (push (list g2 `(%d-next ,g ',whole)) b)
                     (setq b (append (reverse (%destructure p g2)) b)))
                   (push (list p `(%d-next ,g ',whole)) b))
               (push (list g `(cdr ,g)) b))
              ((eq state '&optional)
               (let ((var (if (consp p) (car p) p)) (default (if (consp p) (cadr p) nil))
                     (sv (if (consp p) (caddr p) nil)))
                 (when sv (push (list sv `(consp ,g)) b))
                 (push (list var `(if (consp ,g) (car ,g) ,default)) b)
                 (push (list g `(cdr ,g)) b)))
              ((eq state '&key)
               (setq rest-seen t)
               (let* ((spec (if (consp p) (car p) p))
                      (var (if (consp spec) (cadr spec) spec))
                      (key (if (consp spec) (car spec) (intern (string-append ":" (symbol-name spec)))))
                      (default (if (consp p) (cadr p) nil))
                      (sv (if (consp p) (caddr p) nil))
                      (v (gensym)))
                 (push (list v `(%d-key ,g ',key)) b)
                 (when sv (push (list sv `(not (eq ,v '%none))) b))
                 (push (list var `(if (eq ,v '%none) ,default ,v)) b)))
              ((eq state '&aux)
               (push (if (consp p) (list (car p) (cadr p)) (list p nil)) b))))))
    (unless rest-seen (push (list (gensym) `(%d-end ,g ',whole)) b))
    (nreverse b)))

(defmacro destructuring-bind (pattern value &body body)
  (let ((g (gensym)))
    `(let* ((,g ,value) ,@(%destructure pattern g))
       ,@body)))

;;; ---- SETF: several places at once, and places of one's own ------------------------
;;;
;;; (setf p1 v1 p2 v2 ...) sets each in turn. Besides the places SETF knew,
;;; (defun (setf name) (new-value args...) ...) and DEFSETF define new ones.

(defvar *setf-one* (macro-function 'setf))      ; one place, one value

(defun %setf-pair (place value)
  (cond ((and (consp place) (eq (car place) 'aref))      ; any number of subscripts
         `(aset ,value ,@(cdr place)))
        ((and (consp place) (symbolp (car place)) (get (car place) 'setf-cl-function))
         `(,(get (car place) 'setf-cl-function) ,value ,@(cdr place)))
        ((and (consp place) (symbolp (car place)) (get (car place) 'setf-expander))
         (apply (get (car place) 'setf-expander) value (cdr place)))
        (t (funcall *setf-one* place value))))

(defmacro setf (&rest pairs)
  (if (and (consp pairs) (consp (cdr pairs)) (null (cddr pairs)))
      (%setf-pair (car pairs) (cadr pairs))
      (let ((forms nil))
        (while pairs
          (unless (consp (cdr pairs)) (error "SETF needs places and values in pairs:" pairs))
          (push (%setf-pair (car pairs) (cadr pairs)) forms)
          (setq pairs (cddr pairs)))
        `(progn ,@(nreverse forms)))))

;; the function a (defun (setf NAME) ...) defines
(defun setf-function-symbol (name)
  (intern (string-append "(SETF " (symbol-name name) ")") (or (symbol-package name) *package*)))

;; (defsetf access update): (setf (access args...) v) => (update args... v)
;; (defsetf access (args...) (store) body...): BODY makes the form that stores STORE
(defmacro defsetf (access &rest more)
  (if (and (car more) (symbolp (car more)))
      `(progn (putprop ',access ',(car more) 'setf-function) ',access)
      (destructuring-bind (ll (store) &body body) more
        (let ((v (gensym)) (args (gensym)) (g (gensym)))
          `(progn
             (putprop ',access
                      (lambda (,v &rest ,args)
                        (let ((,g (gensym)))
                          (list 'let (list (list ,g ,v))
                                (apply (lambda (,@ll ,store) ,@body) (append ,args (list ,g))))))
                      'setf-expander)
             ',access)))))

(defsetf ldb (b place) (v) `(setf ,place (dpb ,v ,b ,place)))

;;; ---- features, for #+ and #- ------------------------------------------------------

(defvar *features* '(:common-lisp :lisp-m :ieee-floating-point))

;;; ---- input streams -------------------------------------------------------------
;;;
;;; An input stream is a stream (streams.lisp) whose data is
;;;   #(:input text position refill)
;;; TEXT is read from POSITION on; when it runs out, REFILL (a function, or
;;; NIL) is asked for more text, or NIL at the end. A Listener's
;;; *STANDARD-INPUT* refills from the keyboard, a line at a time, so (READ)
;;; and (READ-LINE) typed there wait for the user.

(defun make-input-stream (text &optional refill)
  (make-stream (lambda (s string) (error "Cannot write to an input stream" s))
               (lambda (s) nil)
               (vector :input text 0 refill)))

(defun input-stream-p (s)
  (and (streamp s) (vectorp (stream-data s)) (eq (aref (stream-data s) 0) :input)))

(defun output-stream-p (s) (and (streamp s) (not (input-stream-p s))))

(defun make-string-input-stream (string &optional (start 0) end)
  (make-input-stream (subseq string start end)))

(defmacro with-input-from-string (spec &body body)
  (destructuring-bind (var string &key (start 0) end) spec
    `(let ((,var (make-string-input-stream ,string ,start ,end)))
       ,@body)))

(defvar *standard-input* (make-input-stream ""))

(defun %input (stream)
  (let ((s (cond ((or (null stream) (eq stream t)) *standard-input*) (t stream))))
    (unless (input-stream-p s) (error "Not an input stream:" s))
    (stream-data s)))

;; more text at the end of D; NIL at the end of the input
(defun %refill (d)
  (let ((more (and (aref d 3) (funcall (aref d 3)))))
    (when more
      (setf (aref d 1) (string-append (substring (aref d 1) (aref d 2)) more))
      (setf (aref d 2) 0)
      t)))

(defun %eof (eof-error-p eof-value stream)
  (if eof-error-p (error "End of file on" stream) eof-value))

(defun read-char (&optional stream (eof-error-p t) eof-value recursive-p)
  (declare (ignore recursive-p))
  (let ((d (%input stream)))
    (if (or (< (aref d 2) (length (aref d 1))) (%refill d))
        (prog1 (aref (aref d 1) (aref d 2)) (setf (aref d 2) (1+ (aref d 2))))
        (%eof eof-error-p eof-value stream))))

(defun unread-char (char &optional stream)
  (let ((d (%input stream)))
    (when (> (aref d 2) 0) (setf (aref d 2) (1- (aref d 2))))
    (declare-ignored char)
    nil))

;; PEEK-TYPE: NIL the next character, T the next one that is not blank, a
;; character: the next one that is that character
(defun peek-char (&optional peek-type stream (eof-error-p t) eof-value recursive-p)
  (declare (ignore recursive-p))
  (let ((d (%input stream)) (r nil) (done nil))
    (while (not done)
      (if (or (< (aref d 2) (length (aref d 1))) (%refill d))
          (let ((c (aref (aref d 1) (aref d 2))))
            (if (cond ((null peek-type) t)
                      ((eq peek-type t) (not (memq c '(#\Space #\Tab #\Newline #\Return #\Page))))
                      (t (eql c peek-type)))
                (setq r c done t)
                (setf (aref d 2) (1+ (aref d 2)))))
          (setq r (%eof eof-error-p eof-value stream) done t)))
    r))

(defun listen (&optional stream)
  (let ((d (%input stream))) (< (aref d 2) (length (aref d 1)))))

(defun clear-input (&optional stream)
  (let ((d (%input stream))) (setf (aref d 1) "" (aref d 2) 0) nil))

;; => the line without its newline, and T if the input ended before a newline
(defun read-line (&optional stream (eof-error-p t) eof-value recursive-p)
  (declare (ignore recursive-p))
  (let ((d (%input stream)) (r nil))
    (loop
      (let* ((text (aref d 1)) (pos (aref d 2))
             (nl (%char-index #\Newline text pos (length text))))
        (cond (nl (setf (aref d 2) (1+ nl))
                  (setq r (list (substring text pos nl) nil))
                  (return nil))
              ((%refill d) nil)
              ((< pos (length text))
               (setf (aref d 2) (length text))
               (setq r (list (substring text pos) t))
               (return nil))
              (t (setq r (list (%eof eof-error-p eof-value stream) t))
                 (return nil)))))
    (values (first r) (second r))))

;; the next object; more lines are asked for while a form is unfinished
(defun read (&optional stream (eof-error-p t) eof-value recursive-p)
  (declare (ignore recursive-p))
  (let ((d (%input stream)) (result nil) (done nil))
    (while (not done)
      (let ((r (%read-from-string (aref d 1) (aref d 2))))
        (cond ((consp r) (setf (aref d 2) (cdr r)) (setq result (car r) done t))
              ((%refill d) nil)
              ((eq r :incomplete) (error "End of file inside a form on" stream))
              (t (setf (aref d 2) (length (aref d 1)))
                 (setq result (%eof eof-error-p eof-value stream) done t)))))
    result))

(defun read-preserving-whitespace (&rest args) (apply #'read args))

(defun y-or-n-p (&optional control &rest args)
  (loop
    (when control (fresh-line *query-io*) (write-string (apply #'format nil control args) *query-io*)
          (write-string " (y or n) " *query-io*))
    (let ((line (string-trim " " (read-line *query-io*))))
      (cond ((string-equal line "y") (return t))
            ((string-equal line "n") (return nil))))))

(defun yes-or-no-p (&optional control &rest args) (apply #'y-or-n-p control args))

;;; ---- more output -----------------------------------------------------------------

;; streams that pass everything on to *STANDARD-OUTPUT* (and read from
;; *STANDARD-INPUT*), whatever it is at the time
(defun make-synonym-output-stream ()
  (make-stream (lambda (s string) (write-string string *standard-output*))
               (lambda (s) (fresh-line *standard-output*))))

(defvar *error-output* (make-synonym-output-stream))
(defvar *trace-output* (make-synonym-output-stream))
(defvar *debug-io* (make-synonym-output-stream))

;; *QUERY-IO* and *TERMINAL-IO* write to *STANDARD-OUTPUT* and read from *STANDARD-INPUT*
(defun make-two-way-synonym-stream ()
  (make-stream (lambda (s string) (write-string string *standard-output*))
               (lambda (s) (fresh-line *standard-output*))
               (vector :input "" 0 (lambda ()
                                     (let ((d (stream-data *standard-input*)))
                                       (if (%refill-any d) (prog1 (substring (aref d 1) (aref d 2))
                                                             (setf (aref d 2) (length (aref d 1))))
                                           nil))))))

(defun %refill-any (d) (or (< (aref d 2) (length (aref d 1))) (%refill d)))

(defvar *query-io* (make-two-way-synonym-stream))
(defvar *terminal-io* *query-io*)

(defun write-line (string &optional stream)
  (write-string string stream)
  (terpri stream)
  string)


(defun finish-output (&optional stream) (declare-ignored stream) nil)
(defun force-output (&optional stream) (declare-ignored stream) nil)

;;; ---- files ------------------------------------------------------------------------
;;;
;;; Files are the ones the editor sees (editor.lisp): world/*.lisp under
;;; `make serve`, or those kept in the browser. They are read whole, and
;;; written whole when the stream is closed.

(defvar *open-files* nil)    ; output stream => file name

(defun %file-name (name) (if (pathnamep name) (namestring name) (string name)))
(defun pathnamep (x) (declare-ignored x) nil)
(defun namestring (x) (string x))

(defun open (name &key (direction :input) (if-does-not-exist :error) &allow-other-keys)
  (let ((name (%file-name name)))
    (case direction
      (:input (let ((text (if (eq if-does-not-exist :error)
                              (host-file 2 name)
                              (ignore-errors (host-file 2 name)))))
                (and text (make-input-stream text))))
      (:output (let ((s (make-string-output-stream)))
                 (push (cons s name) *open-files*)
                 s))
      (t (error "OPEN: only :input and :output files" direction)))))

(defun close (stream &key abort)
  (let ((entry (assq stream *open-files*)))
    (when entry
      (setq *open-files* (remove entry *open-files* :test #'eq))
      (unless abort (host-file 3 (cdr entry) (get-output-stream-string stream))))
    t))

(defmacro with-open-file (spec &body body)
  (destructuring-bind (var name &rest options) spec
    (let ((ok (gensym)))
      `(let ((,var (open ,name ,@options)) (,ok nil))
         (unwind-protect (multiple-value-prog1 (progn ,@body) (setq ,ok t))
           (when ,var (close ,var :abort (not ,ok))))))))

(defmacro with-open-stream (spec &body body)
  `(let ((,(car spec) ,(cadr spec))) (unwind-protect (progn ,@body) (close ,(car spec)))))

(defmacro multiple-value-prog1 (form &body body)
  (let ((g (gensym)))
    `(let ((,g (multiple-value-list ,form))) ,@body (values-list ,g))))

;; Read and evaluate the forms of a file (or of a stream), in *PACKAGE*,
;; which the file may change with IN-PACKAGE for the rest of it
(defun load (source &key (verbose nil) (print nil) &allow-other-keys)
  (let ((stream (if (streamp source) source (open source)))
        (*package* *package*)
        (eof (list :eof)))
    (when verbose (format t "~&; Loading ~A~%" (if (streamp source) "a stream" source)))
    (loop
      (let ((form (read stream nil eof)))
        (when (eq form eof) (return t))
        (let ((value (eval form)))
          (when print (format t "~&; ~S~%" value)))))))

;;; ---- arrays ------------------------------------------------------------------------
;;;
;;; A simple vector or string is a one-dimensional array already. Arrays of
;;; other ranks, and arrays with a fill pointer or that may grow, are headers
;;; (kernel/array.c) around a vector or string of their elements in
;;; row-major order. AREF and (SETF AREF) take any number of subscripts.

(defconstant ar-dims 0)
(defconstant ar-data 1)
(defconstant ar-fill 2)
(defconstant ar-offset 3)
(defconstant ar-flags 4)

(defun arrayp (x) (or (vectorp x) (stringp x) (%array-header-p x)))

(defun %character-type-p (type)
  (or (memq type '(character base-char standard-char))
      (and (consp type) (memq (car type) '(character base-char)))))

(defun %product (list) (let ((p 1)) (dolist (x list) (setq p (* p x))) p))

;; the elements of nested sequences INITIAL-CONTENTS, RANK deep, in row-major order
(defun %flatten-contents (contents rank)
  (if (= rank 0)
      (list contents)
      (let ((r nil))
        (dolist (x (coerce contents 'list)) (setq r (append r (%flatten-contents x (1- rank)))))
        r)))

(defun make-array (dimensions &key (element-type t) (initial-element nil ie-p) initial-contents
                                   adjustable fill-pointer displaced-to (displaced-index-offset 0))
  (let* ((dims (if (listp dimensions) dimensions (list dimensions)))
         (total (%product dims))
         (chars (%character-type-p element-type))
         (bits (%bit-type-p element-type))
         (data (cond (displaced-to (if (%array-header-p displaced-to)
                                       (%array-field displaced-to ar-data)
                                       displaced-to))
                     (bits (let ((b (%make-bytes total)) (v (if (and ie-p initial-element) initial-element 0)))   ; bytes 0 and 1 (M18)
                             (unless (eql v 0) (dotimes (i total) (aset (code-char v) b i)))
                             b))
                     (chars (make-string total :initial-element (if ie-p initial-element #\Space)
                                         :element-type (if (eq element-type 'base-char) 'base-char 'character)))
                     (t (make-vector total initial-element)))))
    (when (and initial-contents (not displaced-to))
      (let ((i 0))
        (dolist (x (%flatten-contents initial-contents (length dims)))
          (when (< i total) (aset (if bits (code-char x) x) data i))
          (setq i (1+ i)))
        (unless (= i total) (error "MAKE-ARRAY: the initial contents do not fit the dimensions" dims))))
    (when (and displaced-to (%array-header-p displaced-to))
      (setq displaced-index-offset (+ displaced-index-offset (%array-field displaced-to ar-offset))))
    (if (and (= (length dims) 1) (not adjustable) (not fill-pointer) (not displaced-to)
             (not bits) (not (and displaced-to (%bit-array-p displaced-to))))
        data
        (%make-array dims data
                     (cond ((null fill-pointer) nil) ((eq fill-pointer t) total) (t fill-pointer))
                     displaced-index-offset
                     (logior (if adjustable 1 0)
                             (if (or bits (and displaced-to (%bit-array-p displaced-to))) 2 0))))))

;; bit arrays (M18): an array header whose data are bytes 0 and 1 (kernel/array.c)
(defun %bit-type-p (type) (or (eq type 'bit) (equal type '(unsigned-byte 1)) (equal type '(integer 0 1))))
(defun %bit-array-p (x) (and (%array-header-p x) (logbitp 1 (%array-field x ar-flags))))

(defun array-dimensions (a)
  (cond ((%array-header-p a) (copy-list (%array-field a ar-dims)))
        ((or (vectorp a) (stringp a)) (list (length a)))
        (t (error "Not an array:" a))))

(defun array-dimension (a axis) (nth axis (array-dimensions a)))
(defun array-rank (a) (length (array-dimensions a)))
(defun array-total-size (a) (%product (array-dimensions a)))

(defun array-element-type (a)
  (cond ((%bit-array-p a) 'bit)
        ((stringp (if (%array-header-p a) (%array-field a ar-data) a)) 'character)
        (t t)))

(defun adjustable-array-p (a) (and (%array-header-p a) (logbitp 0 (%array-field a ar-flags))))
(defun array-has-fill-pointer-p (a) (and (%array-header-p a) (%array-field a ar-fill) t))
(defun simple-vector-p (x) (and (vectorp x) (not (%array-header-p x))))
(defun simple-string-p (x) (stringp x))
(defun svref (v i) (aref v i))
(defun %set-svref (v i x) (aset x v i))
(defsetf svref %set-svref)
(defun %set-char (s i c) (aset c s i))
(defsetf char %set-char)
(defsetf schar %set-char)

(defun array-in-bounds-p (a &rest subscripts)
  (let ((ok t) (dims (array-dimensions a)))
    (unless (= (length dims) (length subscripts)) (setq ok nil))
    (while (and ok dims)
      (unless (and (integerp (car subscripts)) (<= 0 (car subscripts)) (< (car subscripts) (car dims)))
        (setq ok nil))
      (setq dims (cdr dims) subscripts (cdr subscripts)))
    ok))

(defun array-row-major-index (a &rest subscripts)
  (let ((i 0) (dims (array-dimensions a)))
    (dolist (s subscripts) (setq i (+ (* i (car dims)) s) dims (cdr dims)))
    i))

(defun row-major-aref (a i)
  (cond ((%bit-array-p a) (char-code (aref (%array-field a ar-data) (+ i (%array-field a ar-offset)))))
        ((%array-header-p a) (aref (%array-field a ar-data) (+ i (%array-field a ar-offset))))
        (t (aref a i))))

(defun (setf row-major-aref) (v a i)
  (cond ((%bit-array-p a)
         (unless (member v '(0 1)) (error "Not a bit (0 or 1):" v))
         (aset (code-char v) (%array-field a ar-data) (+ i (%array-field a ar-offset))) v)
        ((%array-header-p a) (aset v (%array-field a ar-data) (+ i (%array-field a ar-offset))))
        (t (aset v a i))))

(defun %need-fill-pointer (v)
  (unless (array-has-fill-pointer-p v) (error "The vector has no fill pointer:" v)))

(defun fill-pointer (v) (%need-fill-pointer v) (%array-field v ar-fill))

(defun (setf fill-pointer) (n v)
  (%need-fill-pointer v)
  (unless (and (integerp n) (<= 0 n (car (%array-field v ar-dims))))
    (error "Fill pointer out of range:" n))
  (%set-array-field v ar-fill n))

;; => the index stored at, or NIL if the vector is full
(defun vector-push (x v)
  (let ((fp (fill-pointer v)))
    (when (< fp (car (%array-field v ar-dims)))
      (aset x v fp)
      (%set-array-field v ar-fill (1+ fp))
      fp)))

(defun vector-push-extend (x v &optional (extension nil))
  (let ((fp (fill-pointer v)) (size (car (%array-field v ar-dims))))
    (when (>= fp size)
      (adjust-array v (+ size (max 1 (or extension size 16)))))
    (vector-push x v)))

(defun vector-pop (v)
  (let ((fp (fill-pointer v)))
    (when (= fp 0) (error "VECTOR-POP: the vector is empty" v))
    (%set-array-field v ar-fill (1- fp))
    (aref v (1- fp))))

;; a new size for A, keeping the elements that still fit (by their subscripts);
;; an array header changes in place, a simple vector gives a new one
(defun adjust-array (a dimensions &key (initial-element nil ie-p) initial-contents fill-pointer
                                    &allow-other-keys)
  (let* ((dims (if (listp dimensions) dimensions (list dimensions)))
         (old-dims (array-dimensions a))
         (new (make-array (%product dims) :element-type (array-element-type a)
                               :initial-element (if ie-p initial-element
                                                    (if (eq (array-element-type a) 'character) #\Space nil))
                               :initial-contents (and initial-contents
                                                      (%flatten-contents initial-contents (length dims))))))
    (unless initial-contents (%copy-overlap a new old-dims dims nil))
    (when (%bit-array-p new) (setq new (%array-field new ar-data)))   ; a bit array's bytes
    (if (%array-header-p a)
        (progn (%set-array-field a ar-dims dims)
               (%set-array-field a ar-data new)
               (%set-array-field a ar-offset 0)
               (when fill-pointer (%set-array-field a ar-fill (if (eq fill-pointer t) (car dims) fill-pointer)))
               (when (and (%array-field a ar-fill) (> (%array-field a ar-fill) (%product dims)))
                 (%set-array-field a ar-fill (%product dims)))
               a)
        (if (or fill-pointer (/= (length dims) 1))
            (make-array dims :displaced-to new :fill-pointer fill-pointer :adjustable t)
            new))))

;; copy the elements whose subscripts are in both shapes; NEW is a simple vector or string
(defun %copy-overlap (a new old-dims new-dims subs &optional (all-new-dims new-dims))
  (if (null old-dims)
      (let ((s (reverse subs)))
        (aset (apply #'aref a s) new (apply #'%row-major-in all-new-dims s)))
      (dotimes (i (min (car old-dims) (car new-dims)))
        (%copy-overlap a new (cdr old-dims) (cdr new-dims) (cons i subs) all-new-dims))))

(defun %row-major-in (dims &rest subs)
  (let ((i 0)) (dolist (s subs) (setq i (+ (* i (car dims)) s) dims (cdr dims))) i))

;;; ---- COERCE ------------------------------------------------------------------------

(defun %sequence-list (x)
  (cond ((listp x) x)
        ((or (vectorp x) (stringp x)) (let ((r nil)) (dotimes (i (length x)) (push (aref x i) r)) (nreverse r)))
        (t (error "Not a sequence:" x))))

(defun coerce (x type)
  (let ((head (if (consp type) (car type) type)))
    (cond ((eq type t) x)
          ((memq head '(list cons)) (%sequence-list x))
          ((memq head '(string simple-string base-string))
           (if (stringp x) x (let ((l (%sequence-list x))) (apply #'string-append (mapcar #'string l)))))
          ((memq head '(vector simple-vector array))
           (if (and (vectorp x) (not (%array-header-p x)) (eq head 'simple-vector))
               x
               (if (and (consp type) (cdr type) (%character-type-p (cadr type)))
                   (coerce x 'string)
                   (apply #'vector (%sequence-list x)))))
          ((memq head '(float double-float single-float short-float long-float real)) (float x))
          ((eq head 'character) (character x))
          ((eq head 'function) (if (functionp x) x (symbol-function x)))
          ((and (memq head '(integer number rational)) (integerp x)) x)
          (t (if (typep x head) x (error "COERCE cannot make one of these:" type))))))

(defun character (x)
  (cond ((characterp x) x)
        ((and (stringp x) (= (length x) 1)) (aref x 0))
        ((and (symbolp x) (= (length (symbol-name x)) 1)) (aref (symbol-name x) 0))
        (t (error "Not a character designator:" x))))
