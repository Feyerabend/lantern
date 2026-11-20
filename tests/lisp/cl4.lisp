;;; cl4.lisp -- tests for M14: Common Lisp, part 4 (TAGBODY and GO, the
;;; printer with PRINT-OBJECT, more FORMAT and LOOP, bit operations on
;;; integers of any size).
;;;
;;; The first part is the done-test: a small program in ordinary Common Lisp
;;; style. It sieves primes with one big integer as its bit set, keeps them
;;; in structures that print as #S(...), has a class with its own
;;; PRINT-OBJECT, loops with PROG and GO, and lays out a report with ~< ~>.

;;; ---- the program -----------------------------------------------------------------

(defpackage :primes (:use :common-lisp) (:export #:sieve #:prime-report #:make-prime-run))
(in-package :primes)

;; bit I of the result is 1 when I is prime, for I below N
(defun sieve (n)
  (let ((bits (- (ash 1 n) 4)))               ; everything from 2 up, 0 and 1 cleared
    (loop for i from 2 to (isqrt n)
          when (logbitp i bits)
            do (loop for j from (* i i) below n by i
                     do (setf bits (logandc2 bits (ash 1 j)))))
    bits))

(defstruct prime-run first last count)

;; the runs of primes between gaps of more than GAP, with PROG and GO
(defun runs (bits n gap)
  (prog ((i 2) (runs nil) (run nil))
   next
     (when (>= i n) (go done))
     (when (logbitp i bits)
       (if (and run (<= (- i (prime-run-last run)) gap))
           (setf (prime-run-last run) i
                 (prime-run-count run) (1+ (prime-run-count run)))
           (progn (when run (push run runs))
                  (setq run (make-prime-run :first i :last i :count 1)))))
     (incf i)
     (go next)
   done
     (when run (push run runs))
     (return (nreverse runs))))

(defclass report ()
  ((title :initarg :title :reader report-title)
   (lines :initarg :lines :reader report-lines)))

(defmethod print-object ((r report) stream)
  (print-unreadable-object (r stream :type t)
    (format stream "~S, ~D line~:P" (report-title r) (length (report-lines r)))))

(defun prime-report (n gap)
  (let* ((bits (sieve n))
         (runs (runs bits n gap)))
    (make-instance 'report
      :title (format nil "Primes below ~D" n)
      :lines (cons (format nil "~30<~A~;~D~>" "Primes" (logcount bits))
                   (loop for run in runs
                         for k from 1
                         collect (format nil "~30<run ~D~;~D..~D~;~D~>"
                                         k (prime-run-first run) (prime-run-last run) (prime-run-count run)))))))

(in-package :cl-user)

(check (logcount (primes:sieve 100)) 25)
(check (logcount (primes:sieve 1000)) 168)
(check (integer-length (primes:sieve 1000)) 998)          ; 997 is the largest prime below 1000
(defvar *report* (primes:prime-report 60 4))
(check (prin1-to-string *report*) "#<REPORT \"Primes below 60\", 6 lines>")
(check (slot-value *report* 'primes::lines)
       '("Primes                      17"
         "run 1          2..23         9"
         "run 2         29..31         2"
         "run 3         37..47         4"
         "run 4         53..53         1"
         "run 5         59..59         1"))
(check (prin1-to-string (primes::runs (primes:sieve 12) 12 2))
       "(#S(PRIMES::PRIME-RUN :FIRST 2 :LAST 7 :COUNT 4) #S(PRIMES::PRIME-RUN :FIRST 11 :LAST 11 :COUNT 1))")

;;; ---- TAGBODY, GO, PROG ----------------------------------------------------------------

(check (let ((i 0) (r nil)) (tagbody top (push i r) (setq i (1+ i)) (when (< i 5) (go top))) r) '(4 3 2 1 0))
(check (prog ((n 0)) again (setq n (1+ n)) (if (< n 10) (go again)) (return n)) 10)
(check (prog* ((a 1) (b (* a 2))) (return (list a b))) '(1 2))
(check (tagbody (go b) a (error "not here") b) nil)
(check (let ((log nil)) (tagbody (unwind-protect (go out) (push :cleanup log)) (push :skipped log) out (push :out log)) log)
       '(:out :cleanup))
(check (let ((f nil) (r nil)) (tagbody (setq f (lambda () (go done))) (funcall f) (push :not r) done (push :done r)) r)
       '(:done))
(check (let ((r nil)) (tagbody 10 (push 10 r) (go 30) 20 (push 20 r) 30 (push 30 r)) r) '(30 10))
(check (let ((r nil)) (do ((i 0 (1+ i))) ((= i 4)) (when (oddp i) (go skip)) (push i r) skip) r) '(2 0))
(check (let ((n 0)) (tagbody (let ((*print-base* 2)) (setq n 1) (go out)) out) (list n *print-base*)) '(1 10))
(check (handler-case (eval '(go nowhere)) (error () :no-tag)) :no-tag)

;;; ---- the printer ------------------------------------------------------------------------

(defstruct point (x 0) (y 0))
(check (prin1-to-string (make-point :x 1 :y 2)) "#S(POINT :X 1 :Y 2)")
(check (prin1-to-string (list 1 (make-point :x (make-point) :y "s"))) "(1 #S(POINT :X #S(POINT :X 0 :Y 0) :Y \"s\"))")
(check (princ-to-string (make-point :x "a")) "#S(POINT :X a :Y 0)")
(check (prin1-to-string (copy-point (make-point :x 5))) "#S(POINT :X 5 :Y 0)")
(check (typep (make-point) 'structure-object) t)
(defstruct (card (:print-function (lambda (c s d) (declare (ignore d)) (format s "<~A of ~A>" (card-rank c) (card-suit c)))))
  rank suit)
(check (prin1-to-string (make-card :rank 'queen :suit 'hearts)) "<QUEEN of HEARTS>")
(defstruct (tag (:print-object (lambda (x s) (format s "[tag ~A]" (tag-name x))))) name)
(check (format nil "~A ~S" (make-tag :name 1) (list (make-tag :name 2))) "[tag 1] ([tag 2])")
(defclass dog () ((name :initarg :name :reader dog-name)))
(defmethod print-object ((d dog) stream) (format stream "#<Dog ~A>" (dog-name d)))
(check (prin1-to-string (list (make-instance 'dog :name "Rex") (vector (make-instance 'dog :name "Fido"))))
       "(#<Dog Rex> #(#<Dog Fido>))")
(check (format nil "~A" (make-instance 'dog :name "A")) "#<Dog A>")
(defclass cat () ())
(check (prin1-to-string (make-instance 'cat)) "#<CAT>")
(defmethod print-object ((c cat) stream) (print-unreadable-object (c stream :type t) (write-string "meow" stream)))
(check (prin1-to-string (make-instance 'cat)) "#<CAT meow>")
(defclass kitten (cat) ())
(defmethod print-object ((k kitten) stream) (write-string "small " stream) (call-next-method))
(check (prin1-to-string (make-instance 'kitten)) "small #<KITTEN meow>")
;; *PRINT-...* and WRITE
(check (write-to-string 255 :base 16 :radix t) "#xFF")
(check (write-to-string 10 :base 2) "1010")
(check (write-to-string 7 :radix t) "7.")
(check (let ((*print-base* 16)) (prin1-to-string '(10 255))) "(A FF)")
(check (write-to-string '(a (b (c (d)))) :level 2) "(A (B #))")
(check (write-to-string '(1 2 3 4 5) :length 3) "(1 2 3 ...)")
(check (write-to-string #(1 2 3 4) :length 2) "#(1 2 ...)")
(check (write-to-string '(foo bar) :case :downcase) "(foo bar)")
(check (write-to-string 'foo-bar :case :capitalize) "Foo-Bar")
(check (write-to-string "s" :escape nil) "s")
(check (write-to-string '(a . b)) "(A . B)")
(check (write-to-string ''x) "'X")
(check (with-output-to-string (s) (write 1.5 :stream s) (write-string " " s) (prin1 #\a s) (princ #\a s)) "1.5 #\\aa")
(check (princ-to-string (make-condition 'simple-error :format-control "x ~A" :format-arguments '(1))) "x 1")
(check (handler-case (car 5) (error (c) (princ-to-string c))) "Not a list: 5")

;;; ---- FORMAT ---------------------------------------------------------------------------

(check (format nil "~10<abc~>") "       abc")
(check (format nil "~10<abc~;def~>") "abc    def")
(check (format nil "~10:<abc~;def~>") "  abc  def")
(check (format nil "~10@<abc~>") "abc       ")
(check (format nil "~10:@<abc~>") "    abc   ")
(check (format nil "~8,,,'*<~A~>" 1) "*******1")
(check (format nil "~3<abcdef~>") "abcdef")
(check (format nil "~<~A-~A~:> ~A" (list 5 6) 7) "5-6 7")
(check (format nil "~W ~W" "s" 'x) "\"s\" X")
(check (format nil "a~_b~2Ic") "abc")
(defun bracket (s arg colon at &rest params) (declare (ignore colon at params)) (format s "[~A]" arg))
(check (format nil "x~/bracket/y" 42) "x[42]y")
(check (format nil "~A" (make-point)) "#S(POINT :X 0 :Y 0)")

;;; ---- LOOP -------------------------------------------------------------------------------

(check (loop for i below 4 collect i) '(0 1 2 3))
(check (loop for i to 3 collect i) '(0 1 2 3))
(check (loop for i from 10 downto 1 by 3 collect i) '(10 7 4 1))
(check (loop with a = 1 and b = 2 return (list a b)) '(1 2))
(check (loop for x of-type fixnum in '(1 2) sum x) 3)
(check (loop for x fixnum in '(1 2) sum x) 3)
(check (loop for (a b) of-type (fixnum fixnum) in '((1 2) (3 4)) collect (+ a b)) '(3 7))
(check (loop :for x :in '(1 2) :collect x) '(1 2))
(check (loop for x being the elements of '(a b) collect x) '(a b))
(check (loop for x being the elements of #(1 2) collect x) '(1 2))
(check (loop for x in '(1 2 3) do (when (= x 2) (loop-finish)) collect x) '(1))
(check (loop for x in '(1 2 3) collect x into l finally (return (length l))) 3)
(check (loop named outer for i below 3
             do (loop for j below 3 when (= (* i j) 2) do (return-from outer (list i j))))
       '(1 2))
(check (loop for (a (b c)) in '((1 (2 3)) (4 (5 6))) collect (list a b c)) '((1 2 3) (4 5 6)))
(check (loop for (k . v) in '((a . 1) (b . 2)) collect k collect v) '(a 1 b 2))
(check (loop for i below 10 if (oddp i) collect i into odds else collect i into evens
             finally (return (list odds evens)))
       '((1 3 5 7 9) (0 2 4 6 8)))

;;; ---- bit operations on integers of any size -------------------------------------------

(check (logand (expt 2 100) (1- (expt 2 101))) (expt 2 100))
(check (logior (expt 2 70) 1) (1+ (expt 2 70)))
(check (logxor -1 (expt 2 64)) (- (1+ (expt 2 64))))
(check (logand -1 (expt 2 80)) (expt 2 80))
(check (logand (- (expt 2 80)) (1- (expt 2 81))) (expt 2 80))
(check (logxor (expt 2 100) (expt 2 100)) 0)
(check (lognot (expt 2 70)) (- -1 (expt 2 70)))
(check (ash 1 100) (expt 2 100))
(check (ash (expt 2 100) -98) 4)
(check (ash -1 -200) -1)
(check (ash (- (expt 2 100)) -99) -2)
(check (ash -5 -1) -3)
(check (ash 3 61) 6917529027641081856)
(check (integer-length (expt 2 100)) 101)
(check (integer-length -1) 0)
(check (integer-length (- (expt 2 64))) 64)
(check (logcount (1- (expt 2 70))) 70)
(check (logcount -8) 3)
(check (logbitp 100 (expt 2 100)) t)
(check (logbitp 99 (expt 2 100)) nil)
(check (logtest 6 3) t)
(check (ldb (byte 8 8) #xABCD) #xAB)
(check (dpb #xFF (byte 8 0) #x1200) #x12FF)
(check (let ((x 0)) (setf (ldb (byte 4 4) x) 15) x) 240)
(check (ldb (byte 8 64) (ash #xAB 64)) #xAB)

(report "cl4")
