;;; -*- Mode: Lisp -*-
;;; cl.lisp -- Common Lisp, part 1 (M11): the data a program needs.
;;;
;;; Multiple values, integer arithmetic beyond + - * (the kernel's integers
;;; have any size), characters and strings, sequences, hash tables, DEFSTRUCT,
;;; FORMAT and LOOP: the subsets that ordinary programs use most. Where this
;;; Lisp already had a function under a Common Lisp name, as in Lisp Machine
;;; Lisp MEMBER, ASSOC and REMOVE compare with EQUAL, that is kept.
;;; Floats come later.

;;; ---- multiple values ----------------------------------------------------------
;;; VALUES is in the kernel; MULTIPLE-VALUE-LIST is a special form of the
;;; compiler (an MVLIST instruction right after the form).

(defmacro multiple-value-bind (vars form &body body)
  (let ((g (gensym)) (i -1))
    `(let ((,g (multiple-value-list ,form)))
       (let ,(mapcar (lambda (v) (setq i (1+ i)) (list v `(nth ,i ,g))) vars)
         ,@body))))

(defmacro multiple-value-call (function &rest forms)
  `(apply ,function (append ,@(mapcar (lambda (f) `(multiple-value-list ,f)) forms))))

(defmacro nth-value (n form) `(nth ,n (multiple-value-list ,form)))

(defmacro multiple-value-setq (vars form)
  (let ((g (gensym)) (i -1))
    `(let ((,g (multiple-value-list ,form)))
       ,@(mapcar (lambda (v) (setq i (1+ i)) `(setq ,v (nth ,i ,g))) vars)
       (car ,g))))

(defun values-list (list) (apply #'values list))

;;; ---- more control ---------------------------------------------------------------

(defmacro ecase (key &body clauses)
  (let ((g (gensym)))
    `(let ((,g ,key))
       (case ,g ,@clauses (t (error "No ECASE clause matches" ,g))))))

;; the type tests TYPECASE and CHECK-TYPE understand, beyond class names
(defun type-test (x type)
  (case type
    ((t) t)
    ((nil) nil)
    (list (listp x))
    ((number real) (numberp x))
    (integer (integerp x))
    (rational (rationalp x))
    (ratio (ratiop x))
    ((bit-vector simple-bit-vector) (bit-vector-p x))
    (bit (and (integerp x) (<= 0 x 1)))
    (real (realp x))
    (fixnum (fixnump x))
    (bignum (and (integerp x) (not (fixnump x))))
    ((float double-float single-float short-float long-float) (floatp x))
    (sequence (or (listp x) (vectorp x) (stringp x)))
    ((base-string simple-base-string) (and (stringp x) (not (%wide-string-p x))))
    (base-char (and (characterp x) (< (char-code x) 256)))
    (standard-char (and (characterp x) (standard-char-p x)))
    (vector (or (vectorp x) (stringp x)))
    (hash-table (hash-table-p x))
    (keyword (keywordp x))
    (atom (atom x))
    (t (typep x type))))

;; TYPEP knows the classes (objects.lisp) and these types of characters
;; and strings (M15)
(fset '%class-typep (symbol-function 'typep))
(defun typep (x type)
  (if (memq type '(base-string simple-base-string base-char standard-char rational real ratio
                   bit-vector simple-bit-vector bit))
      (type-test x type)
      (%class-typep x type)))

(defun fixnump (x) (and (integerp x) (<= -1152921504606846976 x 1152921504606846975)))

(defmacro typecase (key &body clauses)
  (let ((g (gensym)))
    `(let ((,g ,key))
       (cond ,@(mapcar (lambda (c)
                         (if (memq (car c) '(t otherwise))
                             `(t ,@(cdr c))
                             `((type-test ,g ',(car c)) ,@(cdr c))))
                       clauses)))))

(defmacro etypecase (key &body clauses)
  (let ((g (gensym)))
    `(let ((,g ,key))
       (typecase ,g ,@clauses (t (error "No ETYPECASE clause matches" ,g))))))

(defmacro check-type (place type)
  `(unless (type-test ,place ',type)
     (error (string-append "The value of " ,(prin1-to-string place) " is not of type " ,(prin1-to-string type) ":")
            ,place)))

;;; ---- integers -------------------------------------------------------------------------

(defun signum (n) (cond ((> n 0) 1) ((< n 0) -1) (t 0)))

(defun gcd (&rest numbers)
  (let ((g 0))
    (dolist (n numbers)
      (let ((a (abs g)) (b (abs n)))
        (while (/= b 0) (let ((r (rem a b))) (setq a b b r)))
        (setq g a)))
    g))

(defun lcm (&rest numbers)
  (let ((l 1))
    (dolist (n numbers) (setq l (if (or (= l 0) (= n 0)) 0 (/ (abs (* l n)) (gcd l n)))))
    l))

;; Declarations are accepted and ignored; THE trusts the type it is told.
(defmacro declare (&rest specs) (declare-ignored specs) nil)
(defun declare-ignored (x) x)
(defmacro declaim (&rest specs) (declare-ignored specs) nil)
(defmacro the (type form) (declare-ignored type) form)
(defmacro locally (&rest body) `(progn ,@body))

;; the four divisions: quotient and remainder, as two values. With a float
;; the quotient is still an integer, and the remainder a float; with a ratio
;; the remainder is a ratio (M18).
(defun truncate (n &optional (d 1))
  (if (and (integerp n) (integerp d))
      (values (%quotient n d) (rem n d))
      (let ((q (%truncate (/ n d))))
        (values q (- n (* q d))))))

(defun floor (n &optional (d 1))
  (multiple-value-bind (q r) (truncate n d)
    (if (and (/= r 0) (not (eq (minusp r) (minusp d))))
        (values (1- q) (+ r d))
        (values q r))))

(defun ceiling (n &optional (d 1))
  (multiple-value-bind (q r) (truncate n d)
    (if (and (/= r 0) (eq (minusp r) (minusp d)))
        (values (1+ q) (- r d))
        (values q r))))

(defun round (n &optional (d 1))            ; to the nearest, ties to even
  (multiple-value-bind (q r) (floor n d)
    (let ((twice (* 2 r)) (ad (abs d)))
      (if (or (> (abs twice) ad) (and (= (abs twice) ad) (oddp q)))
          (values (1+ q) (- r d))
          (values q r)))))

;; the same, with the quotient as a float
(defun ftruncate (n &optional (d 1)) (multiple-value-bind (q r) (truncate n d) (values (float q) r)))
(defun ffloor (n &optional (d 1)) (multiple-value-bind (q r) (floor n d) (values (float q) r)))
(defun fceiling (n &optional (d 1)) (multiple-value-bind (q r) (ceiling n d) (values (float q) r)))
(defun fround (n &optional (d 1)) (multiple-value-bind (q r) (round n d) (values (float q) r)))

(defun isqrt (n)
  (if (< n 2)
      n
      (let ((x n) (y (%quotient (+ n 1) 2)))
        (while (< y x) (setq x y y (%quotient (+ x (%quotient n x)) 2)))
        x)))

;;; ---- ratios (M18) -----------------------------------------------------------------------
;;;
;;; / on integers that do not divide makes a ratio, 7/2; NUMERATOR,
;;; DENOMINATOR and RATIONALP are in the kernel.

(defun ratiop (x) (and (rationalp x) (not (integerp x))))
(defun realp (x) (or (rationalp x) (floatp x)))

;; a float as the exact rational it is
(defun rational (x)
  (if (floatp x)
      (multiple-value-bind (m e s) (integer-decode-float x)
        (* s (if (>= e 0) (ash m e) (/ m (ash 1 (- e))))))
      x))

;; a float as the simplest rational within its precision
(defun rationalize (x)
  (if (floatp x)
      (let ((r (rational x)))
        (if (integerp r)
            r
            ;; continued fractions until the float is reached
            (let ((p0 0) (q0 1) (p1 1) (q1 0) (y (abs r)) (sign (if (minusp x) -1 1)) (done nil))
              (while (not done)
                (let* ((a (floor y)) (p2 (+ (* a p1) p0)) (q2 (+ (* a q1) q0)))
                  (setq p0 p1 q0 q1 p1 p2 q1 q2)
                  (if (or (= (float (/ p1 q1)) (abs x)) (= y a))
                      (setq done t)
                      (setq y (/ 1 (- y a))))))
              (* sign (/ p1 q1)))))
      x))

(defun expt (base power)
  (cond ((floatp power)
         (if (and (minusp base) (/= power (%truncate power)))
             (error "EXPT of a negative number to a fractional power (no complex numbers):" base)
             (%float-function 8 base power)))
        ((minusp power) (/ 1 (expt base (- power))))       ; a ratio for a rational base (M18)
        (t (let ((result 1))
             (while (> power 0)                       ; by squaring: big powers stay quick
               (when (oddp power) (setq result (* result base)))
               (setq power (%quotient power 2))
               (when (> power 0) (setq base (* base base))))
             result))))

;; => the object and where reading stopped
(defun read-from-string (string &optional (eof-error-p t) eof-value &key (start 0) end)
  (let ((r (%read-from-string (if end (substring string 0 end) string) start)))
    (cond ((consp r)                     ; past the blank that ended the object, as READ takes it
           (let ((end (cdr r)))
             (values (car r) (if (and (< end (length string)) (memq (aref string end) '(#\Space #\Tab #\Newline)))
                                 (1+ end) end))))
          (eof-error-p (error "READ-FROM-STRING: end of string" string))
          (t (values eof-value (length string))))))

;;; ---- floats ----------------------------------------------------------------------------
;;; One kind of float, a double (kernel/float.c). Every float type name means it.

(defconstant pi 3.141592653589793)
(defconstant most-positive-double-float 1.7976931348623157e308)
(defconstant least-positive-double-float 4.9406564584124654e-324)
(defconstant double-float-epsilon 1.1102230246251568e-16)
(defconstant most-positive-fixnum 1152921504606846975)
(defconstant most-negative-fixnum -1152921504606846976)


(defun sqrt (x) (%float-function -1 x))
(defun exp (x) (%float-function 0 x))
(defun log (x &optional base)
  (if base (%quotient (%float-function 1 x) (%float-function 1 base)) (%float-function 1 x)))
(defun sin (x) (%float-function 2 x))
(defun cos (x) (%float-function 3 x))
(defun tan (x) (%float-function 4 x))
(defun asin (x) (%float-function 5 x))
(defun acos (x) (%float-function 6 x))
(defun atan (y &optional (x 1.0)) (%float-function 7 y x))
(defun sinh (x) (%float-function 9 x))
(defun cosh (x) (%float-function 10 x))
(defun tanh (x) (%float-function 11 x))

(defun integer-decode-float (x) (values-list (%integer-decode-float x)))

(defun float-sign (x &optional (y 1.0))
  (if (minusp (third (%integer-decode-float (float x)))) (- (abs y)) (abs y)))

;; x = significand * 2^exponent, with 1/2 <= significand < 1
(defun decode-float (x)
  (if (= x 0)
      (values 0.0 0 (float-sign x))
      (let* ((d (%integer-decode-float x)) (len (integer-length (first d))))
        (values (%quotient (float (first d)) (expt 2.0 len)) (+ (second d) len) (float (third d))))))

;; INTEGER-LENGTH and LOGCOUNT are in the kernel (M14)
(defun lognot (n) (- -1 n))
(defun logbitp (index n) (/= 0 (logand 1 (ash n (- index)))))
(defun logtest (a b) (/= 0 (logand a b)))
(defun logandc1 (a b) (logand (lognot a) b))
(defun logandc2 (a b) (logand a (lognot b)))
(defun lognand (a b) (lognot (logand a b)))
(defun lognor (a b) (lognot (logior a b)))
(defun logeqv (&rest ns) (lognot (apply #'logxor ns)))
(defun byte (size position) (cons size position))
(defun byte-size (b) (car b))
(defun byte-position (b) (cdr b))
(defun ldb (b n) (logand (ash n (- (byte-position b))) (1- (ash 1 (byte-size b)))))
(defun dpb (new b n)
  (let ((mask (ash (1- (ash 1 (byte-size b))) (byte-position b))))
    (logior (logand n (lognot mask)) (logand (ash new (byte-position b)) mask))))
(defun ldb-test (b n) (/= 0 (ldb b n)))
(defun mask-field (b n) (logand n (ash (1- (ash 1 (byte-size b))) (byte-position b))))

(defun scale-float (x k) (* x (expt 2.0 k)))

;; a pseudo-random number below LIMIT, of the same kind (a Park-Miller
;; generator: the products stay within fixnums)
(defvar *random-state* 20261120)

(defun %random-step ()
  (setq *random-state* (rem (* *random-state* 48271) 2147483647)))

(defun random (limit &optional state)
  (declare (ignore state))
  (cond ((floatp limit) (* limit (%quotient (float (%random-step)) 2147483647.0)))
        ((and (integerp limit) (> limit 0))
         (let ((r 0) (span 1))
           (while (< span (* limit 1024))             ; enough bits to be fair
             (setq r (+ (* r 2147483647) (%random-step)) span (* span 2147483647)))
           (rem r limit)))
        (t (error "RANDOM needs a positive integer or float:" limit))))

;;; ---- characters -------------------------------------------------------------------------

;; CHAR-UPCASE, CHAR-DOWNCASE, UPPER-CASE-P, LOWER-CASE-P, BOTH-CASE-P,
;; ALPHA-CHAR-P and GRAPHIC-CHAR-P are in the kernel (kernel/text.c), for
;; Latin, Greek and Cyrillic as well as ASCII (M15).

(defun standard-char-p (c)
  (let ((k (char-code c))) (or (= k 10) (and (>= k 32) (< k 127)))))

;; the names a character has: CHAR-NAME gives the first, NAME-CHAR takes any,
;; and #\U+00E9 for any character by its code
(defvar *char-names*
  '((32 "Space") (10 "Newline" "Linefeed") (9 "Tab") (13 "Return") (12 "Page")
    (8 "Backspace") (127 "Rubout" "Delete") (0 "Null" "Nul") (27 "Escape" "Altmode") (7 "Bell")))

(defun char-name (c)
  (let* ((k (char-code c)) (e (assoc k *char-names*)))
    (cond (e (cadr e))
          ((graphic-char-p c) nil)
          (t (format nil "U+~4,'0X" k)))))

(defun name-char (name)
  (let ((name (string name)) (found nil))
    (dolist (e *char-names*)
      (when (and (null found) (some (lambda (n) (string-equal n name)) (cdr e))) (setq found (code-char (car e)))))
    (or found
        (and (> (length name) 1) (char-equal (aref name 0) #\U)
             (let* ((start (if (eq (aref name 1) #\+) 2 1))
                    (k (ignore-errors (parse-integer name :start start :radix 16))))
               (and k (< k #x110000) (code-char k))))
        (and (= (length name) 1) (aref name 0)))))

;; => the digit's weight, or NIL
(defun digit-char-p (c &optional (radix 10))
  (let* ((k (char-code c))
         (w (cond ((and (>= k 48) (<= k 57)) (- k 48))
                  ((and (>= k 65) (<= k 90)) (- k 55))
                  ((and (>= k 97) (<= k 122)) (- k 87))
                  (t nil))))
    (and w (< w radix) w)))

(defun digit-char (weight &optional (radix 10))
  (and (< weight radix) (code-char (if (< weight 10) (+ 48 weight) (+ 55 weight)))))

(defun alphanumericp (c) (or (alpha-char-p c) (digit-char-p c)))
(defun whitespace-char-p (c) (memq (char-code c) '(32 9 10 13 12)))

(defun char= (c &rest more) (every (lambda (d) (eq c d)) more))
(defun char/= (c &rest more)
  (let ((all (cons c more)) (ok t))
    (while (and ok all) (when (memq (car all) (cdr all)) (setq ok nil)) (setq all (cdr all)))
    ok))
(defun char< (c &rest more) (apply #'< (char-code c) (mapcar #'char-code more)))
(defun char> (c &rest more) (apply #'> (char-code c) (mapcar #'char-code more)))
(defun char<= (c &rest more) (apply #'<= (char-code c) (mapcar #'char-code more)))
(defun char>= (c &rest more) (apply #'>= (char-code c) (mapcar #'char-code more)))
(defun char-equal (c &rest more) (every (lambda (d) (eq (char-downcase c) (char-downcase d))) more))
(defun char (s i) (aref s i))
(defun schar (s i) (aref s i))

;;; ---- strings --------------------------------------------------------------------------------

(defun make-string (n &key (initial-element #\Space) (element-type 'character))
  (let ((s (%make-string n (not (memq element-type '(base-char standard-char))))))   ; any character fits
    (dotimes (i n) (aset initial-element s i))
    s))

;; a string of the characters in LIST, narrow if they allow it
(defun %string-of-chars (list)
  (let ((s (%make-string (length list) (some (lambda (c) (>= (char-code c) 256)) list))) (i 0))
    (dolist (c list) (aset c s i) (setq i (1+ i)))
    s))

;; (aset C S I), making S wide first if C needs it => S
(defun %string-put (c s i)
  (when (and (>= (char-code c) 256) (not (%wide-string-p s))) (setq s (%widen s)))
  (aset c s i)
  s)

(defun spaces (n) (make-string (max 0 n)))

(defun string-map (f s start end)
  (let* ((s (string s)) (r (copy-seq s)) (end (or end (length s))))
    (let ((i start))
      (while (< i end) (setq r (%string-put (funcall f (aref s i)) r i)) (setq i (1+ i))))
    r))

(defun string-upcase (s &key (start 0) end) (string-map #'char-upcase s start end))
(defun string-downcase (s &key (start 0) end) (string-map #'char-downcase s start end))

(defun string-capitalize (s &key (start 0) end)
  (let* ((r (copy-seq (string s))) (end (or end (length r))) (word nil) (i start))
    (while (< i end)
      (let ((c (aref r i)))
        (setq r (%string-put (if word (char-downcase c) (char-upcase c)) r i))
        (setq word (alphanumericp c)))
      (setq i (1+ i)))
    r))

(defun string-left-trim (chars s)
  (let ((s (string s)) (i 0))
    (while (and (< i (length s)) (find (aref s i) chars)) (setq i (1+ i)))
    (substring s i)))

(defun string-right-trim (chars s)
  (let* ((s (string s)) (i (length s)))
    (while (and (> i 0) (find (aref s (1- i)) chars)) (setq i (1- i)))
    (substring s 0 i)))

(defun string-trim (chars s) (string-left-trim chars (string-right-trim chars s)))

;; => the index where they first differ when A < B, else NIL (as Common Lisp)
(defun %string-compare (a b fold)
  (let ((a (string a)) (b (string b)) (i 0))
    (while (and (< i (length a)) (< i (length b))
                (eq (if fold (char-downcase (aref a i)) (aref a i)) (if fold (char-downcase (aref b i)) (aref b i))))
      (setq i (1+ i)))
    (cond ((and (= i (length a)) (= i (length b))) (cons 0 i))
          ((= i (length a)) (cons -1 i))
          ((= i (length b)) (cons 1 i))
          (t (cons (if (< (char-code (if fold (char-downcase (aref a i)) (aref a i)))
                          (char-code (if fold (char-downcase (aref b i)) (aref b i))))
                       -1 1)
                   i)))))

(defun string< (a b) (let ((c (%string-compare a b nil))) (and (< (car c) 0) (cdr c))))
(defun string> (a b) (let ((c (%string-compare a b nil))) (and (> (car c) 0) (cdr c))))
(defun string<= (a b) (let ((c (%string-compare a b nil))) (and (<= (car c) 0) (cdr c))))
(defun string>= (a b) (let ((c (%string-compare a b nil))) (and (>= (car c) 0) (cdr c))))
(defun string/= (a b) (let ((c (%string-compare a b nil))) (and (/= (car c) 0) (cdr c))))
(defun string-equal (a b) (= 0 (car (%string-compare a b t))))
(defun string-lessp (a b) (let ((c (%string-compare a b t))) (and (< (car c) 0) (cdr c))))
(defun string-greaterp (a b) (let ((c (%string-compare a b t))) (and (> (car c) 0) (cdr c))))

;; => the integer, and the index after it
(defun parse-integer (s &key (start 0) end (radix 10) junk-allowed)
  (let* ((end (or end (length s))) (i start) (sign 1) (n 0) (digits 0))
    (while (and (< i end) (whitespace-char-p (aref s i))) (setq i (1+ i)))
    (when (and (< i end) (memq (aref s i) '(#\+ #\-)))
      (when (eq (aref s i) #\-) (setq sign -1))
      (setq i (1+ i)))
    (let ((w nil))
      (while (and (< i end) (setq w (digit-char-p (aref s i) radix)))
        (setq n (+ (* n radix) w) digits (1+ digits) i (1+ i))))
    (let ((j i))
      (while (and (< j end) (whitespace-char-p (aref s j))) (setq j (1+ j)))
      (cond ((and (> digits 0) (or junk-allowed (= j end))) (values (* sign n) (if junk-allowed i j)))
            (junk-allowed (values nil i))
            (t (error "Not an integer in" (substring s start end)))))))


;;; ---- sequences: lists, vectors and strings --------------------------------------------------

;; MAPCAR and MAPC over several lists (the boot versions take one)
(defun mapcar (f list &rest more)
  (if (null more)
      (let ((r nil)) (dolist (x list) (push (funcall f x) r)) (nreverse r))
      (let ((lists (cons list more)) (r nil))
        (while (every #'consp lists)
          (push (apply f (%cars lists)) r)
          (setq lists (%cdrs lists)))
        (nreverse r))))

(defun %cars (lists) (let ((r nil)) (dolist (l lists) (push (car l) r)) (nreverse r)))
(defun %cdrs (lists) (let ((r nil)) (dolist (l lists) (push (cdr l) r)) (nreverse r)))

(defun mapc (f list &rest more)
  (apply #'mapcar f list more)
  list)

(defun sequence-list (seq)
  (cond ((listp seq) seq)
        ((stringp seq) (let ((r nil)) (dotimes (i (length seq)) (push (aref seq i) r)) (nreverse r)))
        ((vectorp seq) (let ((r nil)) (dotimes (i (length seq)) (push (aref seq i) r)) (nreverse r)))
        (t (error "Not a sequence:" seq))))

;; a sequence like LIKE (a list, string or vector) holding the elements of LIST
(defun sequence-like (like list)
  (cond ((listp like) list)
        ((stringp like) (%string-of-chars list))
        (t (list->vector list))))

(defun elt (seq i) (if (listp seq) (nth i seq) (aref seq i)))
(putprop 'elt '%set-elt 'setf-function)
(defun %set-elt (seq i v) (if (listp seq) (setf (car (nthcdr i seq)) v) (aset v seq i)))

(defun copy-seq (seq)
  (cond ((listp seq) (copy-list seq))
        ((%wide-string-p seq) (%widen seq))             ; still able to hold any character
        (t (subseq seq 0))))

(defun subseq (seq start &optional end)
  (cond ((stringp seq) (substring seq start end))
        ((listp seq)
         (let ((r nil) (tail (nthcdr start seq)) (n (if end (- end start) -1)))
           (while (and tail (/= n 0)) (push (car tail) r) (setq tail (cdr tail) n (1- n)))
           (nreverse r)))
        (t (let* ((end (or end (length seq))) (v (make-vector (- end start) nil)))
             (dotimes (i (- end start)) (aset (aref seq (+ start i)) v i))
             v))))

(defun %list-reverse (list) (nreverse (copy-list list)))
(defun reverse (seq) (if (listp seq) (%list-reverse seq) (sequence-like seq (nreverse (sequence-list seq)))))

(defun concatenate (type &rest seqs)
  (let ((all (apply #'append (mapcar #'sequence-list seqs))))
    (case type
      (list all)
      ((string simple-string) (sequence-like "" all))
      (t (list->vector all)))))

(defun map (type f seq &rest more)
  (let ((r (apply #'mapcar f (sequence-list seq) (mapcar #'sequence-list more))))
    (case type
      ((nil) nil)
      (list r)
      ((string simple-string) (sequence-like "" r))
      (t (list->vector r)))))

(defun nconc (&rest lists)
  (let ((result nil) (last-cell nil))
    (dolist (l lists)
      (when l
        (if last-cell (setf (cdr last-cell) l) (setq result l))
        (setq last-cell (last l))))
    result))

(defun butlast (list &optional (n 1)) (subseq list 0 (max 0 (- (length list) n))))
(defun nbutlast (list &optional (n 1)) (butlast list n))

(defun %test (test test-not key)
  (let ((key (or key #'identity)))
    (cond (test-not (lambda (item x) (not (funcall test-not item (funcall key x)))))
          (test (lambda (item x) (funcall test item (funcall key x))))
          (t (lambda (item x) (eql item (funcall key x)))))))

;; => the element and its index, as two values
(defun %find (pred seq from-end start end)
  (let ((i 0) (found nil) (where nil) (end (or end (length seq))))
    (dolist (x (sequence-list seq))
      (when (and (>= i start) (< i end) (or from-end (not where)) (funcall pred x))
        (setq found x where i))
      (setq i (1+ i)))
    (values found where)))

(defun find (item seq &key test test-not key from-end (start 0) end)
  (let ((tst (%test test test-not key)))
    (values (%find (lambda (x) (funcall tst item x)) seq from-end start end))))

(defun find-if (pred seq &key key from-end (start 0) end)
  (values (%find (lambda (x) (funcall pred (if key (funcall key x) x))) seq from-end start end)))

(defun find-if-not (pred seq &key key) (find-if (lambda (x) (not (funcall pred x))) seq :key key))

(defun position (item seq &key test test-not key from-end (start 0) end)
  (let ((tst (%test test test-not key)))
    (nth-value 1 (%find (lambda (x) (funcall tst item x)) seq from-end start end))))

(defun position-if (pred seq &key key from-end (start 0) end)
  (nth-value 1 (%find (lambda (x) (funcall pred (if key (funcall key x) x))) seq from-end start end)))

(defun count-if (pred seq &key key)
  (let ((n 0)) (dolist (x (sequence-list seq)) (when (funcall pred (if key (funcall key x) x)) (setq n (1+ n)))) n))

(defun count (item seq &key test test-not key)
  (let ((tst (%test test test-not key))) (count-if (lambda (x) (funcall tst item x)) seq)))

;; REMOVE keeps its EQUAL default (Lisp Machine Lisp); with :TEST, as Common Lisp
(defun remove (item seq &key test test-not key count)
  (let ((tst (%test (or test (and (not test-not) #'equal)) test-not key)) (left (or count -1)) (r nil))
    (dolist (x (sequence-list seq))
      (if (and (/= left 0) (funcall tst item x)) (setq left (1- left)) (push x r)))
    (sequence-like seq (nreverse r))))

(defun remove-if (pred seq &key key)
  (let ((r nil))
    (dolist (x (sequence-list seq)) (unless (funcall pred (if key (funcall key x) x)) (push x r)))
    (sequence-like seq (nreverse r))))

(defun remove-if-not (pred seq &key key)
  (remove-if (lambda (x) (not (funcall pred x))) seq :key key))

(defun delete (item seq &rest options) (apply #'remove item seq options))
(defun delete-if (pred seq &rest options) (apply #'remove-if pred seq options))
(defun delete-if-not (pred seq &rest options) (apply #'remove-if-not pred seq options))

;; keeps the last of equal elements, as Common Lisp does
(defun remove-duplicates (seq &key test test-not key from-end)
  (let ((tst (%test test test-not key)) (r nil) (items (sequence-list seq)))
    (if from-end
        (dolist (x items) (unless (some (lambda (y) (funcall tst (if key (funcall key x) x) y)) r) (push x r)))
        (let ((rest items))
          (while rest
            (let ((x (car rest)))
              (unless (some (lambda (y) (funcall tst (if key (funcall key x) x) y)) (cdr rest)) (push x r)))
            (setq rest (cdr rest)))))
    (sequence-like seq (nreverse r))))

(defun delete-duplicates (seq &rest options) (apply #'remove-duplicates seq options))

(defun substitute (new old seq &key test key)
  (let ((tst (%test test nil key)))
    (sequence-like seq (mapcar (lambda (x) (if (funcall tst old x) new x)) (sequence-list seq)))))

(defun substitute-if (new pred seq)
  (sequence-like seq (mapcar (lambda (x) (if (funcall pred x) new x)) (sequence-list seq))))

;; where SUB first appears in SEQ, or NIL
(defun search (sub seq &key test key from-end)
  (let* ((tst (%test test nil key)) (a (list->vector (sequence-list sub))) (b (list->vector (sequence-list seq)))
         (n (length a)) (m (length b)) (found nil) (i 0))
    (while (<= (+ i n) m)
      (let ((j 0))
        (while (and (< j n) (funcall tst (aref a j) (aref b (+ i j)))) (setq j (1+ j)))
        (when (and (= j n) (or from-end (not found))) (setq found i)))
      (setq i (1+ i)))
    found))

(defun mismatch (a b &key test key)
  (let ((tst (%test test nil key)) (a (sequence-list a)) (b (sequence-list b)) (i 0))
    (while (and a b (funcall tst (car a) (car b))) (setq a (cdr a) b (cdr b) i (1+ i)))
    (if (or a b) i nil)))

(defun fill (seq item &key (start 0) end)
  (let ((end (or end (length seq))) (i start))
    (while (< i end) (%set-elt seq i item) (setq i (1+ i)))
    seq))

;; REDUCE on any sequence, with :INITIAL-VALUE, :KEY and :FROM-END
(defun reduce (f seq &key key from-end (initial-value '%none))
  (let* ((items (mapcar (lambda (x) (if key (funcall key x) x)) (sequence-list seq)))
         (items (if from-end (reverse items) items))
         (step (if from-end (lambda (acc x) (funcall f x acc)) f)))
    (cond ((not (eq initial-value '%none)) (let ((acc initial-value)) (dolist (x items) (setq acc (funcall step acc x))) acc))
          ((null items) (funcall f))
          (t (let ((acc (car items))) (dolist (x (cdr items)) (setq acc (funcall step acc x))) acc)))))

;; a stable merge sort; vectors and strings are sorted in place, as Common Lisp allows
(defun %merge-lists (a b pred key)
  (let* ((head (list nil)) (tail head))
    (while (and a b)
      (if (funcall pred (funcall key (car b)) (funcall key (car a)))
          (progn (setf (cdr tail) b) (setq b (cdr b)))
          (progn (setf (cdr tail) a) (setq a (cdr a))))
      (setq tail (cdr tail)))
    (setf (cdr tail) (or a b))
    (cdr head)))

(defun %sort-list (list pred key)
  (let ((n (length list)))
    (if (< n 2)
        list
        (let* ((middle (nthcdr (1- (%quotient n 2)) list)) (back (cdr middle)))
          (setf (cdr middle) nil)
          (%merge-lists (%sort-list list pred key) (%sort-list back pred key) pred key)))))

(defun sort (seq pred &key key)
  (let ((key (or key #'identity)))
    (if (listp seq)
        (%sort-list (copy-list seq) pred key)
        (let ((sorted (%sort-list (sequence-list seq) pred key)) (i 0))
          (dolist (x sorted) (aset x seq i) (setq i (1+ i)))
          seq))))

(defun stable-sort (seq pred &key key) (sort seq pred :key key))

(defun merge (type a b pred &key key)
  (sequence-like (if (eq type 'list) nil (if (eq type 'string) "" (vector)))
                 (%merge-lists (copy-list (sequence-list a)) (copy-list (sequence-list b)) pred (or key #'identity))))

(defun every* (pred &rest lists)
  (let ((ok t))
    (while (and ok (every #'consp lists))
      (setq ok (apply pred (mapcar #'car lists)) lists (mapcar #'cdr lists)))
    ok))

(defun getf (plist key &optional default)
  (let ((v (%getf-key plist key))) (if (eq v '%none) default v)))

(defun assoc-if (pred alist) (dolist (pair alist) (when (and pair (funcall pred (car pair))) (return pair))))
(defun rassoc (item alist) (dolist (pair alist) (when (and pair (equal item (cdr pair))) (return pair))))
(defun acons (key value alist) (cons (cons key value) alist))
(defun pairlis (keys values &optional alist) (append (mapcar #'cons keys values) alist))
(defun adjoin (item list) (if (member item list) list (cons item list)))
(defmacro pushnew (item place) `(setf ,place (adjoin ,item ,place)))
(defun union (a b) (let ((r b)) (dolist (x a) (unless (member x r) (push x r))) r))
(defun intersection (a b) (remove-if-not (lambda (x) (member x b)) a))
(defun set-difference (a b) (remove-if (lambda (x) (member x b)) a))
(defun subsetp (a b) (every (lambda (x) (member x b)) a))
(defun list-length (list) (list-length-or-nil list))
(defun endp (list) (null list))
(defun first-n (list n) (subseq list 0 (min n (length list))))
(defun mapc* (f list) (mapc f list) list)
(defun maplist (f list) (let ((r nil)) (while list (push (funcall f list) r) (setq list (cdr list))) (nreverse r)))
(defun mapcon (f list) (apply #'append (maplist f list)))
(defun tree-equal (a b) (equal a b))
(defun copy-tree (x) (if (consp x) (cons (copy-tree (car x)) (copy-tree (cdr x))) x))
(defun subst (new old tree)
  (cond ((equal tree old) new)
        ((consp tree) (cons (subst new old (car tree)) (subst new old (cdr tree))))
        (t tree)))

;;; ---- hash tables ----------------------------------------------------------------------------
;;;
;;; #(:hash-table test count buckets epoch by-address): BUCKETS is a vector of
;;; lists of (key . value). %HASH (kernel/prims.c) hashes by contents where it
;;; can; for a key hashed by its address the table notes BY-ADDRESS, and since
;;; the collector moves objects, it rehashes when a collection has happened
;;; since EPOCH. Nothing is allocated between hashing and using the bucket.

(defun make-hash-table (&key (test 'eql) (size 16))
  (let ((test (cond ((memq test '(eq eql equal)) test)
                    ((eq test #'eq) 'eq) ((eq test #'eql) 'eql) ((eq test #'equal) 'equal)
                    ((memq test (list 'string= #'string=)) 'equal)
                    (t (error "Hash tables test with EQ, EQL or EQUAL, not" test)))))
    (vector :hash-table test 0 (make-vector (max 8 size) nil) (%gc-count) nil)))

(defun hash-table-p (x) (and (vectorp x) (= (length x) 6) (eq (aref x 0) :hash-table)))
(defun hash-table-test (h) (aref h 1))
(defun hash-table-count (h) (aref h 2))

(defun %hash-same (test a b)
  (case test (eq (eq a b)) (eql (eql a b)) (t (equal a b))))

(defun %hash-index (h key size)
  (let ((code (%hash key (aref h 1))))
    (when (< code 0) (setf (aref h 5) t) (setq code (- -1 code)))
    (mod code size)))

;; Move every entry to the bucket for its current hash, in a table of SIZE
;; buckets. Cells are relinked, not copied; if a collection happens meanwhile,
;; the epoch stays old and the next access rehashes again.
(defun %rehash (h size)
  (let ((new (make-vector size nil)) (old (aref h 3)) (g (%gc-count)))
    (setf (aref h 5) nil)
    (dotimes (i (length old))
      (let ((cell (aref old i)))
        (while cell
          (let ((next (cdr cell)) (j (%hash-index h (car (car cell)) size)))
            (setf (cdr cell) (aref new j))
            (aset cell new j)
            (setq cell next)))))
    (setf (aref h 3) new)
    (setf (aref h 4) (if (= g (%gc-count)) g -1))))

(defun %hash-ready (h)
  (while (and (aref h 5) (/= (aref h 4) (%gc-count)))
    (%rehash h (length (aref h 3)))))

;; => the entry (key . value), or NIL. Retried if a collection moved
;; address-hashed keys between hashing and looking.
(defun %hash-entry (h key)
  (let ((test (aref h 1)) (found :retry))
    (while (eq found :retry)
      (%hash-ready h)
      (let* ((g (%gc-count)) (buckets (aref h 3)) (i (%hash-index h key (length buckets))) (hit nil))
        (dolist (pair (aref buckets i))
          (when (and (not hit) (%hash-same test (car pair) key)) (setq hit pair)))
        (setq found (if (or hit (not (aref h 5)) (= g (%gc-count))) hit :retry))))
    found))

;; => the value and T, or DEFAULT and NIL
(defun gethash (key h &optional default)
  (let ((pair (%hash-entry h key)))
    (if pair (values (cdr pair) t) (values default nil))))

(defun %sethash (key h &rest default-and-value)
  (let ((value (car (last default-and-value))) (pair (%hash-entry h key)))
    (if pair
        (setf (cdr pair) value)
        (let ((cell (list (cons key value))))      ; allocate first, then hash and link
          (when (> (aref h 2) (* 2 (length (aref h 3)))) (%rehash h (* 2 (length (aref h 3)))))
          (let ((done nil))
            (while (not done)
              (%hash-ready h)
              (let* ((g (%gc-count)) (buckets (aref h 3)) (i (%hash-index h key (length buckets))))
                (when (or (not (aref h 5)) (= g (%gc-count)))   ; nothing moved since hashing
                  (setf (cdr cell) (aref buckets i))
                  (aset cell buckets i)
                  (setq done t)))))
          (setf (aref h 2) (1+ (aref h 2)))
          value))))
(putprop 'gethash '%sethash 'setf-function)
(defun puthash (key value h) (%sethash key h value))

(defun remhash (key h)
  (let ((pair (%hash-entry h key)))
    (when pair
      (let ((buckets (aref h 3)))                 ; unlink the cell holding PAIR, wherever it is
        (dotimes (i (length buckets))
          (when (memq pair (aref buckets i))
            (aset (remove-if (lambda (p) (eq p pair)) (aref buckets i)) buckets i))))
      (setf (aref h 2) (1- (aref h 2)))
      t)))

(defun clrhash (h)
  (setf (aref h 3) (make-vector (length (aref h 3)) nil))
  (setf (aref h 2) 0)
  h)

(defun %hash-pairs (h)
  (let ((r nil) (buckets (aref h 3)))
    (dotimes (i (length buckets)) (dolist (pair (aref buckets i)) (push pair r)))
    r))

(defun maphash (f h)
  (dolist (pair (%hash-pairs h)) (funcall f (car pair) (cdr pair)))
  nil)

(defun hash-table-keys (h) (mapcar #'car (%hash-pairs h)))
(defun hash-table-values (h) (mapcar #'cdr (%hash-pairs h)))
(defun hash-table-alist (h) (mapcar (lambda (p) (cons (car p) (cdr p))) (%hash-pairs h)))
(defun sxhash (x) (abs (%hash x 'equal)))

(%define-class 'hash-table '(t) nil)
(push '(:hash-table . hash-table) *tag-classes*)

;;; ---- DEFSTRUCT --------------------------------------------------------------------------------
;;;
;;; A structure is an instance of a class made with DEFCLASS, so TYPEP, the
;;; Inspector and DESCRIBE know it. Options: (:conc-name x) (:constructor
;;; name) (:predicate name) (:copier name) (:include parent). A slot is NAME
;;; or (NAME default).

(defun struct-option (options key default)
  (let ((o (dolist (o options) (when (eq (if (consp o) (car o) o) key) (return o)))))
    (cond ((null o) default)
          ((and (consp o) (cdr o)) (cadr o))
          (t default))))

(defun keyword-of (sym) (intern (string-append ":" (symbol-name sym))))

;; where SLOT is in the slot vector of an instance of the class (objects.lisp)
(defun class-slot-position (class-name slot)
  (%position slot (aref (find-class class-name) 7)))

;; every structure is a STRUCTURE-OBJECT; it prints as #S(NAME :SLOT value ...)
;; (printer.lisp), unless :PRINT-FUNCTION or :PRINT-OBJECT says otherwise
(defclass structure-object () ())

(defmacro defstruct (name-and-options &rest slots)
  (let* ((name (if (consp name-and-options) (car name-and-options) name-and-options))
         (options (if (consp name-and-options) (cdr name-and-options) nil))
         (prefix (let ((c (struct-option options :conc-name t)))
                   (cond ((eq c t) (string-append (symbol-name name) "-")) ((null c) "") (t (string c)))))
         (parent (struct-option options :include nil))
         (constructor (struct-option options :constructor (intern (string-append "MAKE-" (symbol-name name)))))
         (predicate (struct-option options :predicate (intern (string-append (symbol-name name) "-P"))))
         (copier (struct-option options :copier (intern (string-append "COPY-" (symbol-name name)))))
         (own (mapcar (lambda (s) (if (consp s) (list (car s) (cadr s)) (list s nil)))
                      (if (stringp (car slots)) (cdr slots) slots)))
         (all (append (and parent (get parent 'struct-slots)) own)))
    `(progn
       (defclass ,name ,(if parent (list parent) '(structure-object))
         ,(mapcar (lambda (s) (list (car s) :initarg (keyword-of (car s)))) own))
       (putprop ',name ',all 'struct-slots)
       (putprop ',name ',constructor 'struct-constructor)       ; for #S(...) (M16)
       (defun ,constructor (&key ,@all)
         (make-instance ',name ,@(apply #'append (mapcar (lambda (s) (list (keyword-of (car s)) (car s))) all))))
       ,@(let ((pf (struct-option options :print-function nil))
               (po (struct-option options :print-object nil)))
           (cond (po `((defmethod print-object ((x ,name) stream) (funcall #',po x stream))))
                 (pf `((defmethod print-object ((x ,name) stream) (funcall #',pf x stream 0))))))
       ,@(when predicate `((defun ,predicate (x) (typep x ',name))))
       ,@(when copier `((defun ,copier (x) (%copy-instance x))))
       ,@(apply #'append
                (mapcar (lambda (s)
                          (let ((reader (intern (string-append prefix (symbol-name (car s)))))
                                (writer (intern (string-append "%SET-" prefix (symbol-name (car s))))))
                            `((let ((i (class-slot-position ',name ',(car s))))   ; found once
                                (defun ,reader (x) (aref (instance-slots x) i))
                                (defun ,writer (x v) (aset v (instance-slots x) i)))
                              (putprop ',reader ',writer 'setf-function))))
                        all))
       ',name)))

;;; ---- FORMAT -----------------------------------------------------------------------------------
;;;
;;; (format destination control &rest args): NIL makes a string, T writes to
;;; *standard-output*, a stream writes to it. Directives: ~A ~S ~D ~B ~O ~X
;;; ~R (radix) with mincol,padchar and the @ and : modifiers (~:D groups
;;; digits), ~C ~% ~& ~~ ~T (column), ~* (skip), ~P (plural), ~? (indirect),
;;; ~{...~} (iteration, with ~^), ~[...~;...~] (choice; ~:[ false~;true~]),
;;; ~(...~) (case: ~:( capitalizes, ~@( first word, ~:@( upper), and
;;; ~<newline> to continue the control string on the next line.

(defvar *format-out* nil)                    ; the string being built, newest pieces first
(defvar *format-column* 0)                   ; the column the output has reached
(defvar *format-stream* nil)                 ; where it goes (for ~& at the start)

(defun %char-index (c s start end)           ; plain scans: FORMAT runs often
  (let ((i start) (found nil))
    (while (and (< i end) (not found)) (if (eq (aref s i) c) (setq found i) (setq i (1+ i))))
    found))

;; FORMAT's pieces as plain text, without the pretty printer's items (M16)
(defun %fmt-plain-text (pieces)
  (apply #'string-append
         (mapcar (lambda (p) (cond ((stringp p) p) ((eq (car p) :fill-text) (cadr p)) (t ""))) pieces)))

(defvar *%fmt-fill* nil)        ; inside ~<...~:@>: blanks in the text may break the line

(defun %fmt-out (s)
  (push (if *%fmt-fill* (list :fill-text s) s) *format-out*)
  (let ((i (length s)) (nl nil))
    (while (and (> i 0) (not nl)) (setq i (1- i)) (when (eq (aref s i) #\Newline) (setq nl i)))
    (setq *format-column* (if nl (- (length s) nl 1) (+ *format-column* (length s))))))

(defun %fmt-pad (s mincol padchar left)
  (if (and mincol (< (length s) mincol))
      (let ((pad (make-string (- mincol (length s)) :initial-element padchar)))
        (if left (string-append pad s) (string-append s pad)))
      s))

(defun %fmt-group (digits)                   ; "1234567" => "1,234,567"
  (let* ((neg (and (> (length digits) 0) (eq (aref digits 0) #\-)))
         (d (if neg (substring digits 1) digits)) (r "") (i (length d)))
    (while (> i 3) (setq r (string-append "," (substring d (- i 3) i) r) i (- i 3)))
    (string-append (if neg "-" "") (substring d 0 i) r)))

(defun %fmt-radix (n radix)
  (if (= n 0)
      "0"
      (let ((neg (< n 0)) (n (abs n)) (r nil))
        (while (> n 0) (push (digit-char (rem n radix) radix) r) (setq n (%quotient n radix)))
        (string-append (if neg "-" "") (sequence-like "" r)))))

;; X with D digits after the point, rounded half up from its printed
;; digits (so 12.345 gives 12.35, as it reads): (%fmt-fixed 3.14159 2) => "3.14"
(defun %fmt-fixed (x d)
  (let* ((plain (%fmt-plain (float x)))
         (neg (eq (aref plain 0) #\-))
         (plain (if neg (substring plain 1) plain))
         (dot (%char-index #\. plain 0 (length plain)))
         (frac (string-append (substring plain (1+ dot)) (make-string (1+ d) :initial-element #\0)))
         (n (parse-integer (string-append (substring plain 0 dot) (substring frac 0 d)))))
    (when (>= (digit-char-p (aref frac d)) 5) (setq n (1+ n)))
    (let ((s (princ-to-string n)))
      (when (<= (length s) d)
        (setq s (string-append (make-string (- (1+ d) (length s)) :initial-element #\0) s)))
      (string-append (if (and neg (/= n 0)) "-" "")
                     (substring s 0 (- (length s) d)) "." (substring s (- (length s) d))))))

;; a float without an exponent, all its digits: 1.0e7 => "10000000.0"
(defun %fmt-plain (x)
  (let* ((s (princ-to-string x)) (e (%char-index #\e s 0 (length s))))
    (if (null e)
        s
        (let* ((neg (eq (aref s 0) #\-))
               (mant (remove #\- (remove #\. (substring s 0 e))))
               (point (+ 1 (parse-integer s :start (1+ e)))))
          (string-append (if neg "-" "")
                         (cond ((<= point 0)
                                (string-append "0." (make-string (- point) :initial-element #\0) mant))
                               ((>= point (length mant))
                                (string-append mant (make-string (- point (length mant)) :initial-element #\0) ".0"))
                               (t (string-append (substring mant 0 point) "." (substring mant point)))))))))

;; d.ddde+x, with D digits after the point if given
(defun %fmt-exponential (x d)
  (if (= x 0)
      (string-append (if d (%fmt-fixed 0.0 d) "0.0") "e+0")
      (let* ((e (floor (log (abs x) 10))) (m (%quotient x (expt 10.0 e))))
        (when (>= (abs m) 10) (setq m (%quotient m 10) e (1+ e)))
        (when (< (abs m) 1) (setq m (* m 10) e (1- e)))
        (let ((ms (if d (%fmt-fixed m d) (princ-to-string m))))
          (when (and d (>= (round (* (abs m) (expt 10 d))) (* 10 (expt 10 d))))    ; rounding carried: 9.99 => 10.0
            (setq e (1+ e) ms (%fmt-fixed (%quotient m 10) d)))
          (string-append ms "e" (if (minusp e) "-" "+") (princ-to-string (abs e)))))))

;; ~mincol,colinc,minpad,padchar<seg~;seg~>: the segments spread over at
;; least MINCOL columns, padding between them (and before the first with :,
;; after the last with @); one segment alone is right-justified
(defun %fmt-justify (texts mincol colinc minpad padchar colon at)
  (let* ((mincol (or mincol 0)) (colinc (or colinc 1)) (minpad (or minpad 0)) (padchar (or padchar #\Space))
         (len (apply #'+ (mapcar #'length texts)))
         (inner (1- (length texts)))
         (gaps (+ inner (if colon 1 0) (if at 1 0)))
         (left (or colon (= gaps 0)))
         (gaps (max gaps 1))
         (width (max mincol (+ len (* gaps minpad)))))
    (when (> width mincol)
      (setq width (+ mincol (* colinc (ceiling (- width mincol) colinc)))))
    (let* ((extra (- width len)) (each (floor extra gaps)) (more (- extra (* each gaps)))
           (pads nil))
      (dotimes (k gaps) (push (make-string (+ each (if (< k more) 1 0)) :initial-element padchar) pads))
      (setq pads (nreverse pads))                     ; the leftmost gaps get the odd spaces
      (let ((out (if left (pop pads) "")) (first t))
        (dolist (tx texts)
          (unless first (setq out (string-append out (pop pads))))
          (setq out (string-append out tx) first nil))
        (when (and at pads) (setq out (string-append out (pop pads))))
        out))))

;; parse ~[params][:][@]X at I (just after the ~) => (directive params colon at end)
(defun %fmt-directive (control i)
  (let ((params nil) (colon nil) (at nil) (n (length control)))
    (loop
      (let ((c (aref control i)))
        (cond ((eq c #\') (push (aref control (1+ i)) params) (setq i (+ i 2)))
              ((or (digit-char-p c) (and (eq c #\-) (digit-char-p (aref control (1+ i)))))
               (multiple-value-bind (v end) (parse-integer control :start i :junk-allowed t)
                 (push v params) (setq i end)))
              ((memq c '(#\v #\V)) (push :next-arg params) (setq i (1+ i)))
              ((eq c #\,) (when (or (= i 0) (eq (aref control (1- i)) #\,) (eq (aref control (1- i)) #\~))
                            (push nil params))
                          (setq i (1+ i)))
              (t (return nil))))
      (when (>= i n) (error "FORMAT: the control string ends inside a directive" control)))
    (loop
      (let ((c (aref control i)))
        (cond ((eq c #\:) (setq colon t i (1+ i)))
              ((eq c #\@) (setq at t i (1+ i)))
              (t (return nil)))))
    (list (char-upcase (aref control i)) (nreverse params) colon at (1+ i))))

;; the index of the directive CLOSE (#\} #\] #\)) matching the one before I, and of each ~; inside
(defun %fmt-find-close (control i close)
  (let ((depth 0) (n (length control)) (seps nil) (found nil))
    (while (and (< i n) (not found))
      (if (eq (aref control i) #\~)
          (let* ((d (%fmt-directive control (1+ i))) (c (first d)))
            (cond ((memq c '(#\{ #\[ #\()) (setq depth (1+ depth)))
                  ((and (memq c '(#\} #\] #\))) (> depth 0)) (setq depth (1- depth)))
                  ((eq c close) (setq found (list i (fifth d) (nreverse seps) (third d) (fourth d))))
                  ((and (eq c #\;) (= depth 0)) (push (list i (fifth d) (third d)) seps)))
            (setq i (fifth d)))
          (setq i (1+ i))))
    (or found (error "FORMAT: no matching ~ directive for" (string close)))))

(defun fifth (l) (nth 4 l))

;; ~<prefix~;body~;suffix~:> under *PRINT-PRETTY*: a logical block over one
;; argument, a list (~@< over the rest of the arguments); ~:< puts it in
;; parentheses, and ~:@> breaks the line at blanks in the body's text. The
;; block goes into FORMAT's output as it is; FORMAT lays everything out at
;; the end. => the arguments left
(defun %fmt-logical-block (control i close colon at args)
  (let* ((segs (let ((b nil) (from i))
                 (dolist (sp (third close)) (push (cons from (first sp)) b) (setq from (second sp)))
                 (push (cons from (first close)) b)
                 (nreverse b)))
         (n (length segs))
         (prefix (cond ((>= n 2) (substring control (car (first segs)) (cdr (first segs)))) (colon "(") (t "")))
         (suffix (cond ((>= n 3) (substring control (car (third segs)) (cdr (third segs)))) (colon ")") (t "")))
         (body (if (>= n 2) (second segs) (first segs)))
         (list (if at (prog1 args (setq args nil)) (pop args)))
         (items (let ((*format-out* nil) (*%fmt-fill* (fifth close)))
                  (if (listp list)
                      (%fmt control (car body) (cdr body) list)
                      (%fmt-out (princ-to-string list)))
                  (%pp-items (reverse *format-out*)))))
    (push (make-pp-block :prefix prefix :suffix suffix :items items) *format-out*)
    args))

;; interpret CONTROL from START to END on the arguments ARGS (a list); => the arguments left
(defun %fmt (control start end args)
  (let ((i start) (all args))
    (block run
      (while (< i end)
        (let ((c (aref control i)))
          (if (not (eq c #\~))
              (let ((j (or (%char-index #\~ control i end) end)))
                (%fmt-out (substring control i j))      ; literal text
                (setq i j))
              (let* ((d (%fmt-directive control (1+ i)))
                     (dir (first d)) (colon (third d)) (at (fourth d)) (next (fifth d))
                     (params (mapcar (lambda (p) (if (eq p :next-arg) (pop args) p)) (second d)))
                     (p1 (first params)) (p2 (second params)))
                (setq i next)
                (case dir
                  ((#\A #\S)
                   (let ((s (if (eq dir #\A) (princ-to-string (pop args)) (prin1-to-string (pop args)))))
                     (let ((*%fmt-fill* nil)) (%fmt-out (%fmt-pad s p1 (or p2 #\Space) at)))))
                  ((#\D #\B #\O #\X)
                   (let* ((n (pop args))
                          (s (if (integerp n)
                                 (%fmt-radix n (case dir (#\D 10) (#\B 2) (#\O 8) (t 16)))
                                 (princ-to-string n))))
                     (when (and colon (integerp n)) (setq s (%fmt-group s)))
                     (when (and at (integerp n) (>= n 0)) (setq s (string-append "+" s)))
                     (%fmt-out (%fmt-pad s p1 (or p2 #\Space) t))))
                  ((#\F #\$ #\G)            ; ~w,dF  fixed point; ~d$ money, two digits by default
                   (let* ((x (pop args))
                          (d (if (eq dir #\$) (or p1 2) p2))
                          (w (if (eq dir #\$) nil p1))
                          (s (cond ((not (numberp x)) (princ-to-string x))
                                   (d (%fmt-fixed x d))
                                   (t (%fmt-plain (float x))))))
                     (when (and at (numberp x) (>= x 0)) (setq s (string-append "+" s)))
                     (%fmt-out (%fmt-pad s w (or (fourth params) #\Space) t))))
                  (#\E (let* ((x (pop args))
                              (s (if (numberp x) (%fmt-exponential (float x) p2) (princ-to-string x))))
                         (when (and at (numberp x) (>= x 0)) (setq s (string-append "+" s)))
                         (%fmt-out (%fmt-pad s p1 #\Space t))))
                  (#\R (let ((n (pop args)))
                         (%fmt-out (%fmt-pad (%fmt-radix n (or p1 10)) p2 (or (third params) #\Space) t))))
                  (#\C (%fmt-out (string (pop args))))
                  (#\% (dotimes (k (or p1 1)) (%fmt-out *newline-string*)))
                  (#\& (if (and (null *format-out*) *format-stream*)
                           (progn (fresh-line *format-stream*)        ; at the start: the stream's own line
                                  (dotimes (k (1- (or p1 1))) (%fmt-out *newline-string*)))
                           (dotimes (k (if (= *format-column* 0) (1- (or p1 1)) (or p1 1)))
                             (%fmt-out *newline-string*))))
                  (#\~ (%fmt-out (make-string (or p1 1) :initial-element #\~)))
                  (#\Newline (let ((j i))           ; continue on the next line, skipping its blanks
                               (while (and (< j end) (memq (aref control j) '(#\Space #\Tab))) (setq j (1+ j)))
                               (setq i j)))
                  (#\T (let ((col (or p1 1)) (inc (or p2 1)))
                         (if (< *format-column* col)
                             (%fmt-out (make-string (- col *format-column*)))
                             (when (> inc 0)
                               (%fmt-out (make-string (- inc (rem (- *format-column* col) inc))))))))
                  (#\* (if colon
                           (setq args (nthcdr (- (length all) (length args) (or p1 1)) all))
                           (dotimes (k (or p1 1)) (pop args))))
                  (#\P (when colon (setq args (nthcdr (- (length all) (length args) 1) all)))
                       (let ((n (pop args)))
                         (if at
                             (%fmt-out (if (eql n 1) "y" "ies"))
                             (unless (eql n 1) (%fmt-out "s")))))
                  (#\? (let ((sub (pop args)) (sub-args (pop args)))
                         (%fmt sub 0 (length sub) sub-args)))
                  (#\^ (when (null args) (return-from run nil)))
                  (#\{ (let* ((close (%fmt-find-close control i #\}))
                              (body-end (first close))
                              (items (if at (prog1 args (setq args nil)) (pop args)))
                              (count 0))
                         (setq i (second close))
                         (while (and items (or (null p1) (< count p1)))
                           (if colon
                               (%fmt control next body-end (pop items))      ; ~:{ each item a list of args
                               (setq items (%fmt control next body-end items)))
                           (setq count (1+ count)))))
                  (#\[ (let* ((close (%fmt-find-close control i #\]))
                              (seps (third close))
                              (bounds (let ((b nil) (from next))
                                        (dolist (s seps) (push (cons from (first s)) b) (setq from (second s)))
                                        (push (cons from (first close)) b)
                                        (nreverse b)))
                              (default (and seps (third (car (last seps)))))   ; ~:; before the last clause
                              (choice (cond (colon (if (pop args) (second bounds) (first bounds)))
                                            (at (if (car args) (first bounds) (progn (pop args) nil)))
                                            (t (let ((n (or p1 (pop args))))
                                                 (or (and (integerp n) (>= n 0) (< n (length bounds))
                                                          (not (and default (= n (1- (length bounds)))))
                                                          (nth n bounds))
                                                     (and default (car (last bounds)))))))))
                         (setq i (second close))
                         (when choice (setq args (%fmt control (car choice) (cdr choice) args)))))
                  (#\W (let ((*%fmt-fill* nil)) (%fmt-out (prin1-to-string (pop args)))))      ; M14
                  (#\_ (when *print-pretty*                         ; conditional newlines (M16)
                         (push (list :newline (cond ((and colon at) :mandatory) (colon :fill) (at :miser) (t :linear)))
                               *format-out*)))
                  (#\I (when *print-pretty* (push (list :indent (if colon :current :block) (or p1 0)) *format-out*)))
                  (#\/ (let* ((slash (%char-index #\/ control i (length control)))
                              (name (string-upcase (substring control i slash)))
                              (f (read-from-string (if (%char-index #\: name 0 (length name))
                                                       name
                                                       (string-append "CL-USER::" name))))
                              (s (make-string-output-stream)))
                         (setq i (1+ slash))
                         (apply f s (pop args) colon at params)
                         (%fmt-out (get-output-stream-string s))))
                  (#\< (if (and *print-pretty* (fourth (%fmt-find-close control i #\>)))
                          (let ((close (%fmt-find-close control i #\>)))
                            (setq args (%fmt-logical-block control i close colon at args))
                            (setq i (second close)))
                        (let* ((close (%fmt-find-close control i #\>))
                              (bounds (let ((b nil) (from next))
                                        (dolist (sp (third close)) (push (cons from (first sp)) b) (setq from (second sp)))
                                        (push (cons from (first close)) b)
                                        (nreverse b)))
                              (block-args (and (fourth close) (list (pop args))))   ; ~<...~:> takes a list
                              (texts (mapcar (lambda (bd)
                                               (let ((*format-out* nil))
                                                 (if block-args
                                                     (setf (car block-args) (%fmt control (car bd) (cdr bd) (car block-args)))
                                                     (setq args (%fmt control (car bd) (cdr bd) args)))
                                                 (%fmt-plain-text (reverse *format-out*))))
                                             bounds)))
                         (setq i (second close))
                         (%fmt-out (if (fourth close)          ; ~:> ends a logical block: just the text
                                       (apply #'string-append texts)
                                       (%fmt-justify texts p1 p2 (third params) (fourth params) colon at))))))
                  (#\> (error "FORMAT: ~> without ~<"))
                  (#\( (let ((close (%fmt-find-close control i #\))) (s nil))
                         (setq i (second close))
                         (let ((*format-out* nil))
                           (setq args (%fmt control next (first close) args))
                           (setq s (%fmt-plain-text (reverse *format-out*))))
                         (%fmt-out (cond ((and colon at) (string-upcase s))
                                         (colon (string-capitalize s))
                                         (at (let ((d (string-downcase s)) (k 0))     ; the first word
                                               (while (and (< k (length d)) (not (alpha-char-p (aref d k))))
                                                 (setq k (1+ k)))
                                               (when (< k (length d)) (aset (char-upcase (aref d k)) d k))
                                               d))
                                         (t (string-downcase s))))))
                  (t (error "FORMAT: unknown directive" (string dir)))))))))
    args))

;; Format everything into a string first, then write it at once: a window
;; redraws once, and an error in the arguments leaves nothing half written.
(defun format (destination control &rest args)
  (let* ((stream (cond ((null destination) nil) ((eq destination t) *standard-output*) (t destination)))
         (text (let ((*format-out* nil) (*format-column* 0) (*format-stream* stream) (*%fmt-fill* nil))
                 (%fmt control 0 (length control) args)
                 (if (every #'stringp *format-out*)
                     (apply #'string-append (reverse *format-out*))
                     (%fmt-text (reverse *format-out*)            ; logical blocks: pretty.lisp (M16)
                                (if stream (%stream-column stream) 0)
                                (if stream (%pp-margin stream) (or *print-right-margin* 80)))))))
    (if stream (progn (write-string text stream) nil) text)))

;;; ---- LOOP -------------------------------------------------------------------------------------
;;;
;;; The extended LOOP, the clauses programs use most:
;;;   for/as  x from a [to|upto|below|downto|above b] [by n]   (also: from a downfrom ...)
;;;           x in list [by fn]   x on list   x across vector   x = a [then b]
;;;           (a b) in list  -- simple destructuring of lists
;;;           k being the hash-keys of h [using (hash-value v)], ... the hash-values of h
;;;   with x = a    repeat n    while c    until c    always c    never c    thereis c
;;;   collect/append/nconc/sum/count/maximize/minimize x [into var]
;;;   when/if/unless c clause [and clause...] [else clause...] [end]
;;;   do forms...   return x   initially forms...   finally forms...   named name
;;; (loop form...) without keywords is still the endless loop.

(defun loop-word-p (x &rest words)
  (and (symbolp x) x (member (loop-word-name x) words)))

;; a loop keyword's name: FOR and :FOR are the same word (M14)
(defun loop-word-name (x)
  (let ((n (symbol-name x)))
    (if (and (> (length n) 1) (eq (aref n 0) #\:)) (substring n 1) n)))

(defmacro loop (&rest clauses)
  (if (or (null clauses) (consp (car clauses)))
      `(block nil (while t ,@clauses))
      (loop-expand clauses)))

;; the state of one expansion
(defvar *loop-vars*)          ; ((var init) ...), in order
(defvar *loop-tests*)         ; forms run first in each pass: may (go out)
(defvar *loop-body*)          ; forms of the body, in order
(defvar *loop-steps*)         ; forms run after the body
(defvar *loop-prologue*)
(defvar *loop-epilogue*)
(defvar *loop-result*)        ; the value at the normal end
(defvar *loop-name*)
(defvar *loop-default*)       ; the accumulator that COLLECT etc. without INTO share

(defun loop-expand (clauses)
  (let ((*loop-vars* nil) (*loop-tests* nil) (*loop-body* nil) (*loop-steps* nil)
        (*loop-prologue* nil) (*loop-epilogue* nil) (*loop-result* nil) (*loop-name* nil)
        (*loop-default* nil))
    (let ((rest clauses))
      (while rest (setq rest (loop-clause rest))))
    (let ((done (gensym)))
      `(block ,*loop-name*
         (let* ,(reverse *loop-vars*)
           ,@(reverse *loop-prologue*)
           (let ((,done nil))
             (macrolet ((loop-finish () '(progn (setq ,done t) (return-from %loop-pass nil))))
             (while (not ,done)
               (block %loop-pass
                 ,@(mapcar (lambda (test) `(when ,test (setq ,done t) (return-from %loop-pass nil)))
                           (reverse *loop-tests*))
                 ,@(reverse *loop-body*)
                 ,@(reverse *loop-steps*)))))
           ,@(reverse *loop-epilogue*)
           ,*loop-result*)))))

(defun loop-var (var init) (push (list var init) *loop-vars*))

;; a pattern like (a (b c) . d) bound from VALUE-FORM each pass
(defun loop-destructure (pattern value-form)
  (if (symbolp pattern)
      (list `(setq ,pattern ,value-form))
      (let ((forms nil) (g (gensym)))
        (loop-var g nil)
        (push `(setq ,g ,value-form) forms)
        (let ((p pattern) (i 0))
          (while (consp p)
            (when (car p)
              (dolist (f (loop-destructure-vars (car p) `(nth ,i ,g))) (push f forms)))
            (setq p (cdr p) i (1+ i)))
          (when (and p (symbolp p)) (loop-var p nil) (push `(setq ,p (nthcdr ,i ,g)) forms)))
        (nreverse forms))))

(defun loop-destructure-vars (pattern form)
  (when (symbolp pattern) (loop-var pattern nil))
  (loop-destructure pattern form))

;; the compound forms after INITIALLY, FINALLY or DO, up to the next keyword
(defmacro loop-forms (rest place)
  `(let ((r ,rest))
     (while (and r (consp (car r))) (push (car r) ,place) (setq r (cdr r)))
     r))

(defun loop-clause (c)
  (let ((word (car c)) (rest (cdr c)))
    (cond ((loop-word-p word "NAMED") (setq *loop-name* (car rest)) (cdr rest))
          ((loop-word-p word "FOR" "AS") (loop-for rest))
          ((loop-word-p word "WITH")             ; with x [type] = v [and y = w]...
           (let ((var (car rest)) (r (loop-skip-type (cdr rest))))
             (if (loop-word-p (car r) "=")
                 (progn (loop-var var (cadr r)) (setq r (cddr r)))
                 (loop-var var nil))
             (if (loop-word-p (car r) "AND") (cons 'with (cdr r)) r)))
          ((loop-word-p word "REPEAT")
           (let ((g (gensym)))
             (loop-var g (car rest))
             (push `(<= ,g 0) *loop-tests*)
             (push `(setq ,g (1- ,g)) *loop-steps*)
             (cdr rest)))
          ((loop-word-p word "WHILE") (push `(not ,(car rest)) *loop-tests*) (cdr rest))
          ((loop-word-p word "UNTIL") (push (car rest) *loop-tests*) (cdr rest))
          ((loop-word-p word "ALWAYS")
           (setq *loop-result* t)
           (push `(unless ,(car rest) (return-from ,*loop-name* nil)) *loop-body*) (cdr rest))
          ((loop-word-p word "NEVER")
           (setq *loop-result* t)
           (push `(when ,(car rest) (return-from ,*loop-name* nil)) *loop-body*) (cdr rest))
          ((loop-word-p word "THEREIS")
           (let ((g (gensym)))
             (push `(let ((,g ,(car rest))) (when ,g (return-from ,*loop-name* ,g))) *loop-body*)
             (cdr rest)))
          ((loop-word-p word "INITIALLY") (loop-forms rest *loop-prologue*))
          ((loop-word-p word "FINALLY") (loop-forms rest *loop-epilogue*))
          (t (let ((r (loop-action c)))
               (push (car r) *loop-body*)
               (cdr r))))))

;; an action clause (do, return, an accumulation, a conditional) => (form . rest)
(defun loop-action (c)
  (let ((word (car c)) (rest (cdr c)))
    (cond ((loop-word-p word "DO" "DOING")
           (let ((forms nil))
             (while (and rest (consp (car rest))) (push (car rest) forms) (setq rest (cdr rest)))
             (cons `(progn ,@(nreverse forms)) rest)))
          ((loop-word-p word "RETURN") (cons `(return-from ,*loop-name* ,(car rest)) (cdr rest)))
          ((loop-word-p word "WHEN" "IF" "UNLESS")
           (let* ((test (if (loop-word-p word "UNLESS") `(not ,(car rest)) (car rest)))
                  (r (loop-action-group (cdr rest)))
                  (then (car r)) (rest (cdr r)) (else nil))
             (when (loop-word-p (car rest) "ELSE")
               (let ((e (loop-action-group (cdr rest)))) (setq else (car e) rest (cdr e))))
             (when (loop-word-p (car rest) "END") (setq rest (cdr rest)))
             (cons `(if ,test (progn ,@then) (progn ,@else)) rest)))
          ((loop-word-p word "COLLECT" "COLLECTING" "APPEND" "APPENDING" "NCONC" "NCONCING"
                        "SUM" "SUMMING" "COUNT" "COUNTING" "MAXIMIZE" "MAXIMIZING" "MINIMIZE" "MINIMIZING")
           (loop-accumulate (loop-word-name word) (car rest) (cdr rest)))
          (t (error "LOOP: I do not understand" word)))))

;; clauses joined by AND => (forms . rest)
(defun loop-action-group (c)
  (let* ((r (loop-action c)) (forms (list (car r))) (rest (cdr r)))
    (while (loop-word-p (car rest) "AND")
      (setq r (loop-action (cdr rest)))
      (push (car r) forms)
      (setq rest (cdr r)))
    (cons (nreverse forms) rest)))

(defun loop-accumulate (kind form rest)
  (let* ((into (and (loop-word-p (car rest) "INTO") (cadr rest)))
         (rest (if into (cddr rest) rest))
         (var (or into *loop-default* (setq *loop-default* (gensym))))
         (listy (member kind '("COLLECT" "COLLECTING" "APPEND" "APPENDING" "NCONC" "NCONCING")))
         (tail (and listy (or (get var 'loop-tail) (let ((g (gensym))) (putprop var g 'loop-tail) g)))))
    (unless (assoc var *loop-vars*)
      (loop-var var (cond (listy nil) ((member kind '("SUM" "SUMMING" "COUNT" "COUNTING")) 0) (t nil)))
      (when listy (loop-var tail nil))
      (unless into (setq *loop-result* var)))
    (cons (cond ((member kind '("COLLECT" "COLLECTING"))
                 `(let ((cell (list ,form)))
                    (if ,tail (setf (cdr ,tail) cell) (setq ,var cell))
                    (setq ,tail cell)))
                ((member kind '("APPEND" "APPENDING" "NCONC" "NCONCING"))
                 `(let ((items (copy-list ,form)))
                    (when items
                      (if ,tail (setf (cdr ,tail) items) (setq ,var items))
                      (setq ,tail (last items)))))
                ((member kind '("SUM" "SUMMING")) `(setq ,var (+ ,var ,form)))
                ((member kind '("COUNT" "COUNTING")) `(when ,form (setq ,var (1+ ,var))))
                ((member kind '("MAXIMIZE" "MAXIMIZING"))
                 `(let ((v ,form)) (when (or (null ,var) (> v ,var)) (setq ,var v))))
                (t `(let ((v ,form)) (when (or (null ,var) (< v ,var)) (setq ,var v)))))
          rest)))

;; past a type after a variable: OF-TYPE type, or one of the simple ones
(defun loop-skip-type (r)
  (cond ((loop-word-p (car r) "OF-TYPE") (cddr r))
        ((and (symbolp (car r)) (member (loop-word-name (car r)) '("FIXNUM" "FLOAT" "T" "NIL"))
              (consp (cdr r)) (symbolp (cadr r)) (cadr r)
              (member (loop-word-name (cadr r)) '("IN" "ON" "FROM" "UPFROM" "DOWNFROM" "TO" "UPTO" "BELOW"
                                                  "DOWNTO" "ABOVE" "ACROSS" "=" "BEING" "BY")))
         (cdr r))
        (t r)))

(defun loop-for (c)
  (let* ((var (car c)) (r (loop-skip-type (cdr c))) (word (car r)) (rest (cdr r)))
    (cond
      ;; for i below 10: FROM 0 is understood
      ((loop-word-p word "TO" "UPTO" "BELOW" "DOWNTO" "ABOVE" "BY")
       (loop-for (cons var (cons 'from (cons 0 (cons word rest))))))
      ((loop-word-p word "FROM" "UPFROM" "DOWNFROM")
       (let ((start (car rest)) (rest (cdr rest)) (limit nil) (test nil) (by 1)
             (down (loop-word-p word "DOWNFROM")))
         (while (loop-word-p (car rest) "TO" "UPTO" "BELOW" "DOWNTO" "ABOVE" "BY")
           (let ((w (loop-word-name (car rest))))
             (cond ((string= w "BY") (setq by (cadr rest)))
                   (t (setq limit (cadr rest) test w)
                      (when (member w '("DOWNTO" "ABOVE")) (setq down t)))))
           (setq rest (cddr rest)))
         (let ((lim (gensym)) (step (gensym)))
           (loop-var var start)
           (loop-var step by)
           (when limit
             (loop-var lim limit)
             (push (cond ((member test '("TO" "UPTO")) `(> ,var ,lim))
                         ((string= test "BELOW") `(>= ,var ,lim))
                         ((string= test "DOWNTO") `(< ,var ,lim))
                         (t `(<= ,var ,lim)))
                   *loop-tests*))
           (push `(setq ,var (,(if down '- '+) ,var ,step)) *loop-steps*))
         rest))
      ((loop-word-p word "IN" "ON")
       (let* ((list (car rest)) (rest (cdr rest)) (by nil) (tail (gensym)))
         (when (loop-word-p (car rest) "BY") (setq by (cadr rest) rest (cddr rest)))
         (loop-var tail list)
         (push `(atom ,tail) *loop-tests*)
         (if (symbolp var) (loop-var var nil) nil)
         (dolist (f (loop-destructure-bind var (if (loop-word-p word "IN") `(car ,tail) tail)))
           (push f *loop-tests*))
         (push `(setq ,tail ,(if by `(funcall ,by ,tail) `(cdr ,tail))) *loop-steps*)
         rest))
      ((loop-word-p word "ACROSS")
       (let ((v (gensym)) (i (gensym)))
         (loop-var v (car rest))
         (loop-var i 0)
         (loop-var var nil)
         (push `(>= ,i (length ,v)) *loop-tests*)
         (push `(progn (setq ,var (aref ,v ,i)) nil) *loop-tests*)
         (push `(setq ,i (1+ ,i)) *loop-steps*)
         (cdr rest)))
      ((loop-word-p word "=")
       (let ((init (car rest)) (rest (cdr rest)))
         (if (loop-word-p (car rest) "THEN")
             (let ((first (gensym)))
               (loop-var var nil)
               (loop-var first t)
               (push `(progn (setq ,var (if ,first ,init ,(cadr rest)) ,first nil) nil) *loop-tests*)
               (cddr rest))
             (progn (loop-var var nil)
                    (push `(progn (setq ,var ,init) nil) *loop-tests*)
                    rest))))
      ((loop-word-p word "BEING")
       ;; k being the hash-keys of h [using (hash-value v)] / the hash-values
       (let* ((r (if (loop-word-p (car rest) "THE" "EACH") (cdr rest) rest))
              (what (loop-word-name (car r))) (table (caddr r)) (rest (cdddr r)) (other nil))
         (when (member what '("ELEMENTS" "ELEMENT"))           ; any sequence
           (return-from loop-for (loop-for (cons var (cons 'across (cons `(coerce ,table 'vector) rest))))))
         (when (loop-word-p (car rest) "USING") (setq other (cadr (cadr rest)) rest (cddr rest)))
         (let ((pairs (gensym)))
           (loop-var pairs `(%hash-pairs ,table))
           (loop-var var nil)
           (when other (loop-var other nil))
           (push `(null ,pairs) *loop-tests*)
           (push `(progn (setq ,var (,(if (member what '("HASH-KEYS" "HASH-KEY")) 'car 'cdr) (car ,pairs)))
                         ,@(when other `((setq ,other (,(if (member what '("HASH-KEYS" "HASH-KEY")) 'cdr 'car) (car ,pairs)))))
                         nil)
                 *loop-tests*)
           (push `(setq ,pairs (cdr ,pairs)) *loop-steps*)
           rest)))
      (t (error "LOOP: I do not understand FOR" var word)))))

;; the forms that set VAR (a symbol or a list pattern) from VALUE-FORM, as tests that are false
(defun loop-destructure-bind (var value-form)
  (mapcar (lambda (f) `(progn ,f nil)) (loop-destructure-vars* var value-form)))

(defun loop-destructure-vars* (pattern form)
  (if (symbolp pattern)
      (list `(setq ,pattern ,form))
      (loop-destructure pattern form)))
