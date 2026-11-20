;;; cl2.lisp -- tests for M12: Common Lisp, part 2 (floats, local functions, packages).
;;;
;;; The first part is the done-test: a small library in a package of its own,
;;; written in ordinary Common Lisp style -- floats, LABELS and FLET, exported
;;; names -- and used from CL-USER. The rest checks each piece.

;;; ---- the program -----------------------------------------------------------------

(defpackage :statistics
  (:use :common-lisp)
  (:nicknames :stats)
  (:export #:mean #:variance #:std-dev #:median #:summary #:newton-sqrt))

(in-package :statistics)

(defun mean (xs) (/ (reduce #'+ xs) (float (length xs))))

(defun variance (xs)
  (let ((m (mean xs)))
    (flet ((square (x) (* x x)))
      (/ (reduce #'+ (mapcar (lambda (x) (square (- x m))) xs)) (length xs)))))

(defun std-dev (xs) (sqrt (variance xs)))

(defun median (xs)
  (let* ((sorted (sort (copy-list xs) #'<)) (n (length sorted)) (mid (floor n 2)))
    (if (oddp n)
        (nth mid sorted)
        (/ (+ (nth (1- mid) sorted) (nth mid sorted)) 2.0))))

;; square roots by Newton's method, to show LABELS and float arithmetic
(defun newton-sqrt (a &optional (tolerance 1e-12))
  (labels ((good-enough-p (guess) (< (abs (- (* guess guess) a)) (* tolerance a)))
           (improve (guess) (/ (+ guess (/ a guess)) 2))
           (try (guess n) (if (or (good-enough-p guess) (> n 100)) guess (try (improve guess) (1+ n)))))
    (try (float a) 0)))

(defun summary (xs)
  (format nil "n=~D mean=~,2F sd=~,3F median=~A"
          (length xs) (mean xs) (std-dev xs) (median xs)))

(defun helper () :internal)                   ; not exported

(in-package :cl-user)

(defvar *data* '(2 4 4 4 5 5 7 9))

(check (stats:mean *data*) 5.0)
(check (statistics:variance *data*) 4.0)
(check (stats:std-dev *data*) 2.0)
(check (stats:median *data*) 4.5)
(check (stats:median '(3 1 2)) 2)
(check (stats:summary *data*) "n=8 mean=5.00 sd=2.000 median=4.5")
(check (< (abs (- (stats:newton-sqrt 2) 1.4142135623730951)) 1e-12) t)
(check (stats::helper) :internal)
(check (symbol-package 'stats:mean) (find-package :statistics))
(check (package-name (symbol-package 'car)) "COMMON-LISP")

;;; ---- floats: reading and printing ----------------------------------------------------

(check (list 1.5 -0.25 1e10 1.0e7 123456.7 0.001 1.5e-5 3.0d0 2.5f0 .5 -.5 1.0E3)
       '(1.5 -0.25 1.0e10 1.0e7 123456.7 0.001 1.5e-5 3.0 2.5 0.5 -0.5 1000.0))
(check (prin1-to-string 0.1) "0.1")
(check (prin1-to-string 100.0) "100.0")
(check (prin1-to-string -0.0) "-0.0")
(check (prin1-to-string 1234567.0) "1234567.0")
(check (prin1-to-string 12345678.0) "1.2345678e7")
(check (prin1-to-string 1.7976931348623157e308) "1.7976931348623157e308")
(check (prin1-to-string 4.9406564584124654e-324) "5.0e-324")
(check (prin1-to-string (/ 1 3.0)) "0.3333333333333333")
(check (prin1-to-string (+ 0.1 0.2)) "0.30000000000000004")
(check (read-from-string "1.5") 1.5)
(check (read-from-string "12.") 12)            ; a trailing dot is an integer
(check (symbolp (read-from-string "1.5x")) t)
(check (symbolp (read-from-string "1e")) t)
(check (let ((x 0.1)) (= x (read-from-string (prin1-to-string x)))) t)

;;; ---- floats: arithmetic ----------------------------------------------------------------

(check (+ 1 2.5) 3.5)
(check (* 2 0.5) 1.0)
(check (- 10 0.5) 9.5)
(check (/ 1 4.0) 0.25)
(check (/ 7 2) 7/2)                            ; integers divide to a ratio (M18)
(check (1+ 1.5) 2.5)
(check (1- 0.5) -0.5)
(check (float 3) 3.0)
(check (float (expt 2 100)) 1.2676506002282294e30)
(check (floatp 1.0) t)
(check (floatp 1) nil)
(check (numberp 1.0) t)
(check (realp 2) t)
(check (integerp 1.0) nil)
(check (= 2 2.0) t)
(check (eql 2 2.0) nil)
(check (eql 1.5 1.5) t)
(check (equal 1.5 1.5) t)
(check (eql 0.0 -0.0) nil)
(check (= 0.0 -0.0) t)
(check (< 1 1.5 2 2.5) t)
(check (> 2.0 1) t)
(check (max 1 2.5 2) 2.5)
(check (min 3 -1.5) -1.5)
(check (abs -2.5) 2.5)
(check (zerop 0.0) t)
(check (minusp -0.1) t)
(check (signum -2.5) -1)
;; exact comparison between floats and large integers
(check (= (expt 2 70) (float (expt 2 70))) t)
(check (< (expt 2 70) (+ (float (expt 2 70)) 1e10)) t)
(check (= (1+ (expt 2 70)) (float (expt 2 70))) nil)
(check (< (float (expt 2 70)) (1+ (expt 2 70))) t)
(check (> 1e300 (expt 10 299)) t)
(check (< 1e300 (expt 10 301)) t)
;; errors, not infinities
(check (handler-case (* 1e300 1e300) (error () :overflow)) :overflow)
(check (handler-case (/ 1.0 0) (error () :zero)) :zero)
(check (handler-case (sqrt -1) (error () :no-complex)) :no-complex)

;;; ---- floats: rounding, functions, format ---------------------------------------------

(check (multiple-value-list (floor 7.5 2)) '(3 1.5))
(check (multiple-value-list (floor -7.5)) '(-8 0.5))
(check (multiple-value-list (ceiling 2.1)) '(3 -0.8999999999999999))
(check (multiple-value-list (truncate -2.7)) '(-2 -0.7000000000000002))
(check (round 2.5) 2)
(check (round 3.5) 4)
(check (round -2.6) -3)
(check (multiple-value-list (ffloor 3.7)) '(3.0 0.7000000000000002))
(check (floor 1e20) 100000000000000000000)
(check (rem 7.5 2) 1.5)
(check (mod -7.5 2) 0.5)
(check (sqrt 16) 4.0)
(check (sqrt 2.0) 1.4142135623730951)
(check (expt 2.0 10) 1024.0)
(check (expt 2 0.5) 1.4142135623730951)
(check (expt 2.0 -2) 0.25)
(check (exp 0) 1.0)
(check (log 1) 0.0)
(check (log 100 10) 2.0)
(check (< (abs (- (sin (/ pi 2)) 1.0)) 1e-15) t)
(check (cos 0) 1.0)
(check (atan 1 1) (/ pi 4))
(check (multiple-value-list (integer-decode-float 1.5)) '(6755399441055744 -52 1))
(check (multiple-value-list (decode-float 8.0)) '(0.5 4 1.0))
(check (float-sign -2.0) -1.0)
(check (typep 1.5 'float) t)
(check (typep 1.5 'number) t)
(check (typep 1 'float) nil)
(check (typecase 2.5 (integer :int) (float :float)) :float)
(check (class-name (class-of 2.0)) 'float)
(check (format nil "~F" 3.14159) "3.14159")
(check (format nil "~,2F" 3.14159) "3.14")
(check (format nil "~8,3F" 2.5) "   2.500")
(check (format nil "~$" 12.345) "12.35")
(check (format nil "~,1F" 0.96) "1.0")
(check (format nil "~,2F" -0.001) "0.00")
(check (format nil "~,2F" 7) "7.00")
(check (format nil "~F" 1e10) "10000000000.0")
(check (format nil "~F" 1.5e-5) "0.000015")
(check (format nil "~E" 12345.678) "1.2345678e+4")
(check (format nil "~,2E" 0.000123) "1.23e-4")
(check (format nil "~@F" 1.5) "+1.5")
(check (format nil "~A ~S" 1.5 2.0) "1.5 2.0")
(check (let ((s 0.0)) (dotimes (i 10) (setq s (+ s 0.1))) (format nil "~,6F" s)) "1.000000")
(check (let ((r (random 1.0))) (and (floatp r) (>= r 0) (< r 1))) t)
(check (let ((r (random 10))) (and (integerp r) (>= r 0) (< r 10))) t)
(check (let ((r (random (expt 10 30)))) (and (integerp r) (>= r 0) (< r (expt 10 30)))) t)

;;; ---- local functions ---------------------------------------------------------------------

(defun even-odd (n)
  (labels ((ev (k) (if (= k 0) t (od (1- k))))
           (od (k) (if (= k 0) nil (ev (1- k)))))
    (list (ev n) (od n))))
(check (even-odd 10) '(t nil))
(check (even-odd 7) '(nil t))

;; FLET's definitions do not see themselves: the inner call is the outer function
(defun shadowed (x) (list :outer x))
(check (flet ((shadowed (x) (shadowed (* x 10)))) (shadowed 4)) '(:outer 40))
;; LABELS' do
(check (labels ((fact (n) (if (< n 2) 1 (* n (fact (1- n)))))) (fact 20)) 2432902008176640000)
(check (flet ((double (x) (* 2 x))) (mapcar #'double '(1 2 3))) '(2 4 6))
(check (flet ((add (&optional (a 1) &rest more) (apply #'+ a more))) (list (add) (add 2 3 4))) '(1 9))
(check (flet ((kw (&key (a 1) b) (list a b))) (kw :b 2)) '(1 2))
;; closures over the surrounding variables, and over each other
(defun make-counter ()
  (let ((count 0))
    (flet ((bump () (setq count (1+ count))) (peek () count))
      (list #'bump #'peek))))
(check (let ((c (make-counter))) (funcall (first c)) (funcall (first c)) (funcall (second c))) 2)
(check (labels ((walk (tree) (if (atom tree) (if tree 1 0) (+ (walk (car tree)) (walk (cdr tree))))))
         (walk '(a (b c) ((d)) e)))
       5)
;; RETURN-FROM the local function's name
(check (flet ((early (x) (when (> x 2) (return-from early :big)) :small)) (list (early 1) (early 5)))
       '(:small :big))
;; a local function can shadow a macro
(check (flet ((when (a b) (list a b))) (when 1 2)) '(1 2))
(check (macrolet ((twice (form) `(progn ,form ,form))) (let ((n 0)) (twice (incf n)) n)) 2)
(check (macrolet ((sq (x) `(* ,x ,x))) (flet ((f (y) (sq y))) (f 7))) 49)
;; the debugger sees the names
(check (labels ((inner () (%function-name (%frame-function (car (%frames)))))) (inner)) '(labels inner))
;; declarations are accepted
(check (flet ((f (x) (declare (type fixnum x) (ignorable x)) (the fixnum (+ x 1)))) (f 1)) 2)

;;; ---- packages ----------------------------------------------------------------------------

(check (package-name *package*) "COMMON-LISP-USER")
(check (package-name (find-package "CL")) "COMMON-LISP")
(check (eq (find-package :cl-user) (find-package "USER")) t)
(check (packagep (find-package :keyword)) t)
(check (find-package :no-such-package) nil)
(check (symbol-package :foo) (find-package :keyword))
(check (symbol-package (make-symbol "X")) nil)
(check (prin1-to-string 'stats:mean) "STATISTICS:MEAN")
(check (prin1-to-string 'stats::helper) "STATISTICS::HELPER")
(check (prin1-to-string 'car) "CAR")
(check (prin1-to-string (make-symbol "G")) "#:G")
(check (prin1-to-string :key) ":KEY")
(check (princ-to-string 'stats:mean) "MEAN")
(check (eq 'stats:mean 'statistics::mean) t)
(check (multiple-value-list (find-symbol "MEAN" :stats)) '(stats:mean :external))
(check (multiple-value-list (find-symbol "HELPER" :stats)) '(stats::helper :internal))
(check (multiple-value-list (find-symbol "CAR" :stats)) '(car :inherited))
(check (multiple-value-list (find-symbol "NO-SUCH-NAME-HERE" :stats)) '(nil nil))
(check (handler-case (read-from-string "stats:helper") (error () :not-external)) :not-external)
(check (handler-case (read-from-string "nopackage:x") (error () :no-package)) :no-package)
(check (symbolp (read-from-string "stats::brand-new")) t)
(check (multiple-value-list (find-symbol "BRAND-NEW" :stats)) '(stats::brand-new :internal))

;; a package of one's own, then USE-PACKAGE
(defpackage :shapes (:use :cl) (:export #:area))
(defun shapes:area (r) (* pi r r))
(check (shapes:area 1.0) pi)
(check (multiple-value-list (find-symbol "AREA")) '(nil nil))
(use-package :shapes)
(check (area 2.0) (* 4 pi))
(check (eq 'area 'shapes:area) t)
(check (member (find-package :cl-user) (package-used-by-list :shapes)) (list (find-package :cl-user)))
(unuse-package :shapes)
(check (multiple-value-list (find-symbol "AREA")) '(nil nil))
;; INTERN, EXPORT, IMPORT, SHADOW
(check (multiple-value-bind (s status) (intern (string-append "FRESH" "-ONE") :shapes)
         (list (symbol-name s) (symbol-package s) status))
       (list "FRESH-ONE" (find-package :shapes) nil))
(check (multiple-value-list (intern "FRESH-ONE" :shapes)) '(shapes::fresh-one :internal))
(export 'shapes::fresh-one :shapes)
(check (prin1-to-string 'shapes::fresh-one) "SHAPES:FRESH-ONE")
(import 'shapes:fresh-one)
(check (eq 'fresh-one 'shapes:fresh-one) t)
(defpackage :mine (:use :cl) (:shadow #:car))
(check (eq (find-symbol "CAR" :mine) 'car) nil)
(check (eq (find-symbol "CDR" :mine) 'cdr) t)
(check (let ((n 0)) (do-external-symbols (s :stats) (setq n (1+ n))) n) 6)
(check (let ((found nil)) (do-symbols (s :shapes) (when (eq s 'shapes:area) (setq found t))) found) t)
(check (not (null (member (find-package :shapes) (list-all-packages)))) t)
;; a package with no USE sees nothing of COMMON-LISP
(defpackage :bare (:use))
(check (multiple-value-list (find-symbol "CAR" :bare)) '(nil nil))
(check (prin1-to-string (let ((*package* (find-package :bare))) (prin1-to-string 'car))) "\"COMMON-LISP:CAR\"")
;; *PACKAGE* rebound: printing follows it
(check (let ((*package* (find-package :stats))) (prin1-to-string 'stats::helper)) "HELPER")
(check (let ((*package* (find-package :stats))) (prin1-to-string 'check)) "COMMON-LISP-USER::CHECK")
(check (in-package :cl-user) (find-package :cl-user))

;;; ---- the reader's other additions ----------------------------------------------------

(check (list #x1F #XfF #b101 #o17 #x-10) '(31 255 5 15 -16))
(check #x10000000000000000 18446744073709551616)
(check (symbol-name '#:foo) "FOO")
(check (eq '#:foo '#:foo) nil)

(report "cl2")
