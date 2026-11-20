;;; -*- Mode: Lisp -*-
;;; cl4.lisp -- numbers and data, the rest (M18): bit vectors, pathnames,
;;; and byte streams. Ratios are in the kernel (kernel/bignum.c) and
;;; cl.lisp.

;;; ---- bit vectors -------------------------------------------------------------------
;;;
;;; A bit array is an array header whose data are bytes 0 and 1 (kernel/array.c);
;;; #*1011 reads and prints one. MAKE-ARRAY with :ELEMENT-TYPE 'BIT makes one.

(defun bit-vector-p (x) (and (%bit-array-p x) (= (array-rank x) 1)))
(defun simple-bit-vector-p (x)
  (and (bit-vector-p x) (not (array-has-fill-pointer-p x)) (not (adjustable-array-p x))))

(defun bit (a &rest subscripts) (apply #'aref a subscripts))
(defun sbit (a &rest subscripts) (apply #'aref a subscripts))
(defun %set-bit (a &rest subscripts-and-value)
  (let ((v (car (last subscripts-and-value))) (subs (butlast subscripts-and-value)))
    (apply #'aset v a subs)))
(putprop 'bit '%set-bit 'setf-function)
(putprop 'sbit '%set-bit 'setf-function)

(defun make-bit-vector (n &optional (initial-element 0))
  (make-array n :element-type 'bit :initial-element initial-element))

;; the bitwise functions: into RESULT (T: into A; NIL or none: a new array)
(defun %bit-op (f a b result)
  (let* ((n (array-total-size a))
         (r (cond ((eq result t) a)
                  ((null result) (make-array (array-dimensions a) :element-type 'bit))
                  (t result))))
    (dotimes (i n)
      (setf (row-major-aref r i)
            (logand 1 (funcall f (row-major-aref a i) (if b (row-major-aref b i) 0)))))
    r))

(defun bit-and (a b &optional result) (%bit-op #'logand a b result))
(defun bit-ior (a b &optional result) (%bit-op #'logior a b result))
(defun bit-xor (a b &optional result) (%bit-op #'logxor a b result))
(defun bit-eqv (a b &optional result) (%bit-op (lambda (x y) (lognot (logxor x y))) a b result))
(defun bit-nand (a b &optional result) (%bit-op (lambda (x y) (lognot (logand x y))) a b result))
(defun bit-nor (a b &optional result) (%bit-op (lambda (x y) (lognot (logior x y))) a b result))
(defun bit-andc1 (a b &optional result) (%bit-op (lambda (x y) (logand (lognot x) y)) a b result))
(defun bit-andc2 (a b &optional result) (%bit-op (lambda (x y) (logand x (lognot y))) a b result))
(defun bit-orc1 (a b &optional result) (%bit-op (lambda (x y) (logior (lognot x) y)) a b result))
(defun bit-orc2 (a b &optional result) (%bit-op (lambda (x y) (logior x (lognot y))) a b result))
(defun bit-not (a &optional result) (%bit-op (lambda (x y) (declare (ignore y)) (lognot x)) a nil result))

;; a bit vector of the elements of LIST
(defun %bits-of (list)
  (make-array (length list) :element-type 'bit :initial-contents list))

;; SUBSEQ, COPY-SEQ, REVERSE and COERCE keep bit vectors bit vectors
(fset '%subseq-plain (symbol-function 'subseq))
(defun subseq (seq start &optional end)
  (if (bit-vector-p seq)
      (%bits-of (%subseq-plain (coerce seq 'list) start end))
      (%subseq-plain seq start end)))

(fset '%copy-seq-plain (symbol-function 'copy-seq))
(defun copy-seq (seq) (if (bit-vector-p seq) (subseq seq 0) (%copy-seq-plain seq)))

(fset '%reverse-plain (symbol-function 'reverse))
(defun reverse (seq) (if (bit-vector-p seq) (%bits-of (reverse (coerce seq 'list))) (%reverse-plain seq)))

(fset '%coerce-plain (symbol-function 'coerce))
(defun coerce (x type)
  (if (or (eq type 'bit-vector) (eq type 'simple-bit-vector))
      (if (bit-vector-p x) x (%bits-of (coerce x 'list)))
      (%coerce-plain x type)))

;;; ---- pathnames ---------------------------------------------------------------------
;;;
;;; A pathname is a structure, written #P"dir/name.type". Lantern's files are
;;; the host's (world/*.lisp under `make serve`, or the browser's): names
;;; without directories mostly, but the parts are kept as Common Lisp keeps
;;; them, and OPEN, LOAD and PROBE-FILE take pathnames.
;;;   directory: NIL, or (:absolute "a" "b") / (:relative "a"), :wild for *
;;;   name, type: strings, NIL, or :wild for *

(defstruct (pathname (:constructor %make-pathname) (:predicate pathnamep) (:copier nil)
                     (:print-object (lambda (p s)
                                      (if *print-escape*
                                          (progn (write-string "#P" s) (prin1 (namestring p) s))
                                          (write-string (namestring p) s)))))
  host device directory name type version)

(defvar *default-pathname-defaults* (%make-pathname))

(defun %wild (s) (if (equal s "*") :wild s))
(defun %unwild (x) (if (eq x :wild) "*" x))

(defun parse-namestring (thing &optional host defaults &key (start 0) end junk-allowed)
  (declare (ignore host defaults junk-allowed))
  (if (pathnamep thing)
      (values thing start)
      (let* ((s (subseq (string thing) start end))
             (slash (position #\/ s :from-end t))
             (dir-part (and slash (subseq s 0 (1+ slash))))
             (file (if slash (subseq s (1+ slash)) s))
             (dot (position #\. file :from-end t))
             (name (cond ((equal file "") nil)
                         ((and dot (> dot 0)) (subseq file 0 dot))   ; .emacs is a name
                         (t file)))
             (type (and dot (> dot 0) (subseq file (1+ dot))))
             (directory (and dir-part
                             (let ((parts (remove "" (%split-on #\/ dir-part) :test #'equal)))
                               (cons (if (eql (aref dir-part 0) #\/) :absolute :relative)
                                     (mapcar (lambda (p) (cond ((equal p "*") :wild) ((equal p "..") :up) (t p)))
                                             parts))))))
        (values (%make-pathname :directory directory :name (and name (%wild name)) :type (and type (%wild type)))
                (length (string thing))))))

(defun %split-on (char s)
  (let ((parts nil) (start 0))
    (dotimes (i (length s))
      (when (eql (aref s i) char) (push (subseq s start i) parts) (setq start (1+ i))))
    (nreverse (cons (subseq s start) parts))))

;; a pathname from a pathname, a string, or a file stream
(defun pathname (x)
  (cond ((pathnamep x) x)
        ((and (streamp x) (assq x *open-files*)) (parse-namestring (cdr (assq x *open-files*))))
        (t (values (parse-namestring x)))))

(defun directory-namestring (p)
  (let ((d (pathname-directory (pathname p))))
    (if d
        (apply #'string-append
               (if (eq (car d) :absolute) "/" "")
               (mapcar (lambda (x) (string-append (cond ((eq x :wild) "*") ((eq x :up) "..") (t x)) "/"))
                       (cdr d)))
        "")))

(defun file-namestring (p)
  (let ((p (pathname p)))
    (string-append (or (%unwild (pathname-name p)) "")
                   (if (pathname-type p) (string-append "." (%unwild (pathname-type p))) ""))))

(defun namestring (p)
  (let ((p (pathname p))) (string-append (directory-namestring p) (file-namestring p))))

(defun host-namestring (p) (declare (ignore p)) "")

(defun enough-namestring (p &optional (defaults *default-pathname-defaults*))
  (let ((p (pathname p)) (d (pathname defaults)))
    (if (and (pathname-directory p) (equal (pathname-directory p) (pathname-directory d)))
        (file-namestring p)
        (namestring p))))

(defun make-pathname (&key host device directory name type version defaults)
  (let ((d (and defaults (pathname defaults))))
    (%make-pathname :host (or host (and d (pathname-host d)))
                    :device (or device (and d (pathname-device d)))
                    :directory (cond ((stringp directory) (list :absolute directory))
                                     ((eq directory :wild) '(:absolute :wild))
                                     (directory directory)
                                     (d (pathname-directory d)))
                    :name (or name (and d (pathname-name d)))
                    :type (or type (and d (pathname-type d)))
                    :version (or version (and d (pathname-version d))))))

;; the missing parts of PATHNAME from DEFAULTS; a relative directory goes on
;; the end of the default one
(defun merge-pathnames (pathname &optional (defaults *default-pathname-defaults*) default-version)
  (let* ((p (pathname pathname)) (d (pathname defaults))
         (pd (pathname-directory p)) (dd (pathname-directory d)))
    (%make-pathname :host (or (pathname-host p) (pathname-host d))
                    :device (or (pathname-device p) (pathname-device d))
                    :directory (cond ((null pd) dd)
                                     ((and (eq (car pd) :relative) dd) (append dd (cdr pd)))
                                     (t pd))
                    :name (or (pathname-name p) (pathname-name d))
                    :type (or (pathname-type p) (pathname-type d))
                    :version (or (pathname-version p) (pathname-version d) default-version))))

(defun wild-pathname-p (p &optional field)
  (let ((p (pathname p)))
    (case field
      (:name (%wild-string-p (pathname-name p)))
      (:type (%wild-string-p (pathname-type p)))
      (:directory (and (member :wild (pathname-directory p)) t))
      (t (or (%wild-string-p (pathname-name p)) (%wild-string-p (pathname-type p))
             (and (member :wild (pathname-directory p)) t))))))

;; a name against a pattern: NIL or :WILD match anything; * in a string any run of characters
(defun %part-matches (part pattern)
  (cond ((or (null pattern) (eq pattern :wild)) t)
        ((not (stringp part)) (null part))
        (t (%glob-match pattern 0 part 0))))

(defun %glob-match (p i s j)
  (cond ((= i (length p)) (= j (length s)))
        ((eql (aref p i) #\*)
         (or (%glob-match p (1+ i) s j)
             (and (< j (length s)) (%glob-match p i s (1+ j)))))
        (t (and (< j (length s)) (eql (aref p i) (aref s j)) (%glob-match p (1+ i) s (1+ j))))))

(defun %wild-string-p (x) (or (eq x :wild) (and (stringp x) (find #\* x) t)))

(defun pathname-match-p (p wildcard)
  (let ((p (pathname p)) (w (pathname wildcard)))
    (and (%part-matches (pathname-name p) (pathname-name w))
         (%part-matches (pathname-type p) (pathname-type w)))))

;; the files the host has: their names, as pathnames
(defun %host-files ()
  (and (%file-server-p)
       (mapcar #'parse-namestring (remove "" (%split-on #\Newline (host-file 1 "")) :test #'equal))))

(defun probe-file (p)
  (let ((name (file-namestring (pathname p))))
    (find name (%host-files) :key #'file-namestring :test #'equal)))

(defun truename (p)
  (or (probe-file p) (error "There is no file" (namestring p))))

(defun directory (pathspec &key &allow-other-keys)
  (let ((w (pathname pathspec)))
    (sort (remove-if-not (lambda (p) (pathname-match-p p w)) (%host-files))
          #'string< :key #'namestring)))

(defun file-author (p) (declare (ignore p)) nil)
(defun file-write-date (p) (declare (ignore p)) nil)

;;; ---- byte streams -------------------------------------------------------------------
;;;
;;; OPEN with :ELEMENT-TYPE '(UNSIGNED-BYTE 8) reads or writes a file's bytes,
;;; as they are: READ-BYTE and WRITE-BYTE. A byte stream's data is
;;;   #(:input-bytes bytes position) or #(:output-bytes bytes-written-in-reverse)

(defun %byte-type-p (type)
  (or (equal type '(unsigned-byte 8)) (equal type '(mod 256)) (eq type 'octet)
      (and (consp type) (eq (car type) 'unsigned-byte))))

(defun %byte-stream-data (s kind)
  (let ((d (and (streamp s) (stream-data s))))
    (and (vectorp d) (> (length d) 0) (eq (aref d 0) kind) d)))

(defun make-byte-input-stream (bytes)        ; BYTES: a narrow string, or a list or vector of octets
  (make-stream (lambda (s string) (declare (ignore string)) (error "Cannot write to an input stream" s))
               (lambda (s) (declare (ignore s)) nil)
               (vector :input-bytes
                       (if (stringp bytes) bytes (%octets-string bytes))
                       0)))

(defun make-byte-output-stream ()
  (make-stream (lambda (s string) (declare (ignore string)) (error "Write bytes to a byte stream:" s))
               (lambda (s) (declare (ignore s)) nil)
               (vector :output-bytes nil)))

(defun %octets-string (octets)
  (let* ((l (coerce octets 'list)) (s (%make-bytes (length l))) (i 0))
    (dolist (b l) (aset (code-char b) s i) (setq i (1+ i)))
    s))

(defun read-byte (stream &optional (eof-error-p t) eof-value)
  (let ((d (%byte-stream-data stream :input-bytes)))
    (unless d (error "Not a byte input stream:" stream))
    (if (< (aref d 2) (length (aref d 1)))
        (prog1 (char-code (aref (aref d 1) (aref d 2))) (setf (aref d 2) (1+ (aref d 2))))
        (%eof eof-error-p eof-value stream))))

(defun write-byte (byte stream)
  (let ((d (%byte-stream-data stream :output-bytes)))
    (unless d (error "Not a byte output stream:" stream))
    (unless (and (integerp byte) (<= 0 byte 255)) (error "Not a byte (0 to 255):" byte))
    (push byte (aref d 1))
    byte))

;; the bytes written so far, as a vector of octets
(defun get-output-stream-bytes (stream)
  (coerce (reverse (aref (%byte-stream-data stream :output-bytes) 1)) 'vector))

(defun read-sequence (seq stream &key (start 0) end)
  (let ((end (or end (length seq))) (i start) (bytes (%byte-stream-data stream :input-bytes)))
    (loop
      (when (>= i end) (return i))
      (let ((x (if bytes (read-byte stream nil :eof) (read-char stream nil :eof))))
        (when (eq x :eof) (return i))
        (setf (elt seq i) x)
        (setq i (1+ i))))))

(defun write-sequence (seq stream &key (start 0) end)
  (let ((bytes (%byte-stream-data stream :output-bytes)))
    (dolist (x (subseq (coerce seq 'list) start end))
      (if bytes (write-byte x stream) (write-char x stream)))
    seq))

(defun stream-element-type (stream)
  (if (or (%byte-stream-data stream :input-bytes) (%byte-stream-data stream :output-bytes))
      '(unsigned-byte 8)
      'character))

(fset '%input-stream-p-text (symbol-function 'input-stream-p))
(defun input-stream-p (s) (or (%input-stream-p-text s) (and (%byte-stream-data s :input-bytes) t)))

(defvar *open-byte-files* nil)               ; byte output stream => file name

;; OPEN with pathnames and byte files; text files as before (cl2.lisp)
(fset '%open-text (symbol-function 'open))
(defun open (name &rest keys &key (direction :input) (element-type 'character) (if-does-not-exist :error)
                  &allow-other-keys)
  (let ((name (namestring (pathname name))))
    (if (%byte-type-p element-type)
        (case direction
          (:input (let ((bytes (if (eq if-does-not-exist :error)
                                   (host-file 2 name "" t)
                                   (ignore-errors (host-file 2 name "" t)))))
                    (and bytes (make-byte-input-stream bytes))))
          (:output (let ((s (make-byte-output-stream)))
                     (push (cons s name) *open-byte-files*)
                     s))
          (t (error "OPEN: only :input and :output files" direction)))
        (apply #'%open-text name keys))))

(fset '%close-text (symbol-function 'close))
(defun close (stream &key abort)
  (let ((entry (assq stream *open-byte-files*)))
    (if entry
        (progn (setq *open-byte-files* (remove entry *open-byte-files* :test #'eq))
               (unless abort
                 (host-file 3 (cdr entry) (%octets-string (reverse (aref (stream-data stream) 1))) t))
               t)
        (%close-text stream :abort abort))))

(fset '%load-text (symbol-function 'load))
(defun load (source &rest keys)
  (apply #'%load-text (if (or (streamp source) (stringp source)) source (namestring (pathname source))) keys))
