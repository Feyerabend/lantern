;;; cl.lisp -- tests for M11: Common Lisp, part 1.
;;;
;;; The first part is the done-test: a small program written in ordinary
;;; Common Lisp style, run unchanged. The rest checks each piece.

;;; ---- the program -----------------------------------------------------------------

(defparameter *text*
  "the quick brown fox jumps over the lazy dog the fox is quick and the dog is lazy")

(defun split-words (string)
  (loop with words = nil
        with start = nil
        for i from 0 to (length string)
        for c = (if (< i (length string)) (char string i) #\Space)
        do (cond ((alpha-char-p c) (unless start (setf start i)))
                 (start (push (string-downcase (subseq string start i)) words)
                        (setf start nil)))
        finally (return (nreverse words))))

(defun word-counts (words)
  (let ((table (make-hash-table :test #'equal)))
    (dolist (w words)
      (incf (gethash w table 0)))
    table))

(defun top-words (table n)
  (let ((pairs (loop for w being the hash-keys of table using (hash-value c)
                     collect (cons w c))))
    (subseq (sort pairs (lambda (a b)
                          (or (> (cdr a) (cdr b))
                              (and (= (cdr a) (cdr b)) (string< (car a) (car b))))))
            0 n)))

(defun word-report (text)
  (format nil "~{~A~5T~2D~%~}"
          (loop for (w . c) in (top-words (word-counts (split-words text)) 4)
                append (list w c))))

(check (word-report *text*) "the   4
dog   2
fox   2
is    2
")

(defun fact (n) (if (< n 2) 1 (* n (fact (1- n)))))
(check (format nil "~D" (fact 100))
       "93326215443944152681699238856266700490715968264381621468592963895217599993229915608941463976156518286253697920827223758251185210916864000000000000000000000000")
(check (length (format nil "~D" (fact 100))) 158)

(defstruct point (x 0) (y 0))
(defun manhattan (p q) (+ (abs (- (point-x p) (point-x q))) (abs (- (point-y p) (point-y q)))))
(check (manhattan (make-point :x 1 :y 2) (make-point :x 4 :y -2)) 7)

(defun divide-with-remainder (a b)
  (multiple-value-bind (q r) (floor a b)
    (format nil "~D = ~D * ~D + ~D" a b q r)))
(check (divide-with-remainder 17 5) "17 = 5 * 3 + 2")
(check (divide-with-remainder -17 5) "-17 = 5 * -4 + 3")

;;; ---- bignums ------------------------------------------------------------------------

(check (* 4294967296 4294967296) 18446744073709551616)
(check (- (* 4294967296 4294967296) 18446744073709551615) 1)
(check (+ 1152921504606846975 1) 1152921504606846976)            ; one past the largest fixnum
(check (- -1152921504606846976 1) -1152921504606846977)
(check (fixnump (- (+ 1152921504606846975 1) 1)) t)               ; back to a fixnum
(check (/ (fact 30) (fact 28)) 870)
(check (rem (fact 30) 1000007) 790627)
(check (mod (- (fact 25)) 7) 0)
(check (mod -7 (fact 22)) (- (fact 22) 7))
(check (expt 2 100) 1267650600228229401496703205376)
(check (expt 3 0) 1)
(check (gcd (fact 20) (expt 2 30)) 262144)
(check (lcm 4 6) 12)
(check (isqrt (expt 10 40)) (expt 10 20))
(check (< (fact 20) (fact 21) (fact 22)) t)
(check (> (- (fact 22)) (- (fact 23))) t)
(check (= (fact 22) (* 22 (fact 21))) t)
(check (eql (fact 22) (fact 22)) t)
(check (equal (list (fact 22)) (list (* 22 (fact 21)))) t)
(check (integerp (fact 40)) t)
(check (abs (- (fact 30))) (fact 30))
(check 123456789012345678901234567890 (* 123456789012345678901234567890 1))
(check (- 0 123456789012345678901234567890) -123456789012345678901234567890)
(check (prin1-to-string (- (expt 2 64))) "-18446744073709551616")
(check (let ((s 0)) (dotimes (i 100) (setq s (+ s (expt 2 i)))) s) (1- (expt 2 100)))
(check (ignore-errors (+ (fact 25) 'x)) nil)
(check (handler-case (/ (fact 25) 0) (error (c) (condition-message c))) "Division by zero")
(check (multiple-value-list (truncate (fact 22) 1000000007)) (list (values (floor (fact 22) 1000000007)) (rem (fact 22) 1000000007)))

;;; ---- multiple values ------------------------------------------------------------------

(check (multiple-value-list (values 1 2 3)) '(1 2 3))
(check (multiple-value-list (values)) nil)
(check (multiple-value-list 7) '(7))
(defun two () (values :a :b))
(check (multiple-value-list (two)) '(:a :b))
(check (multiple-value-list (progn (two) 5)) '(5))             ; not the values of an earlier form
(check (multiple-value-list (let ((x 1)) (two))) '(:a :b))
(check (multiple-value-list (if t (two) 1)) '(:a :b))
(check (list (two)) '(:a))                                      ; only the first, as an argument
(check (multiple-value-bind (a b c) (two) (list a b c)) '(:a :b nil))
(check (nth-value 1 (floor 7 2)) 1)
(check (multiple-value-call #'list (two) (values 1 2)) '(:a :b 1 2))
(check (multiple-value-list (values-list '(x y))) '(x y))
(check (multiple-value-list (round 5 2)) '(2 1))
(check (multiple-value-list (round 7 2)) '(4 -1))
(check (multiple-value-list (ceiling 7 2)) '(4 -1))
(check (multiple-value-list (floor -7 2)) '(-4 1))

;;; ---- hash tables ------------------------------------------------------------------------

(let ((h (make-hash-table)))
  (setf (gethash 'a h) 1)
  (setf (gethash 'b h) 2)
  (check (gethash 'a h) 1)
  (check (multiple-value-list (gethash 'c h)) '(nil nil))
  (check (multiple-value-list (gethash 'c h :none)) '(:none nil))
  (check (hash-table-count h) 2)
  (check (remhash 'a h) t)
  (check (hash-table-count h) 1)
  (check (gethash 'a h) nil))

(let ((h (make-hash-table :test 'equal)))
  (setf (gethash "key" h) :string)
  (setf (gethash '(1 2) h) :list)
  (setf (gethash (fact 30) h) :big)
  (check (gethash (copy-seq "key") h) :string)
  (check (gethash (list 1 2) h) :list)
  (check (gethash (* 30 (fact 29)) h) :big))

;; keys hashed by address survive the collector moving them
(let ((h (make-hash-table :test 'eq)) (keys nil))
  (dotimes (i 200) (let ((k (list i))) (push k keys) (setf (gethash k h) i)))
  (gc)
  (check (every (lambda (k) (eql (gethash k h) (car k))) keys) t)
  (check (hash-table-count h) 200)
  (check (gethash (list 5) h) nil))

(let ((h (make-hash-table :test 'equal)) (sum 0))
  (dotimes (i 1000) (setf (gethash (format nil "k~D" i) h) i))         ; grows as it fills
  (maphash (lambda (k v) (setq sum (+ sum v))) h)
  (check sum 499500)
  (check (gethash "k777" h) 777)
  (check (length (hash-table-keys h)) 1000)
  (clrhash h)
  (check (hash-table-count h) 0))
(check (prin1-to-string (make-hash-table :test 'equal)) "#<Hash-table EQUAL>")
(check (hash-table-p (make-hash-table)) t)

;;; ---- defstruct ----------------------------------------------------------------------------

(defstruct (animal (:conc-name pet-)) name (legs 4))
(defstruct (bird (:include animal)) (wings 2))
(let ((b (make-bird :name "robin" :legs 2)))
  (check (pet-name b) "robin")
  (check (bird-wings b) 2)
  (check (bird-legs b) 2)
  (check (animal-p b) t)
  (check (bird-p (make-animal)) nil)
  (setf (bird-wings b) 3)
  (check (bird-wings b) 3)
  (let ((c (copy-bird b)))
    (setf (bird-wings c) 9)
    (check (list (bird-wings b) (bird-wings c)) '(3 9))))
(check (pet-legs (make-animal)) 4)
(check (typep (make-point) 'point) t)

;;; ---- format ---------------------------------------------------------------------------------

(check (format nil "~A and ~S" "x" "x") "x and \"x\"")
(check (format nil "~5D|~5,'0D|~:D" 42 42 1234567) "   42|00042|1,234,567")
(check (format nil "~B ~O ~X ~@D" 10 64 255 7) "1010 100 FF +7")
(check (format nil "~10A|~10@A|" "left" "right") "left      |     right|")
(check (format nil "~D item~:P, ~D pon~:@P" 1 3) "1 item, 3 ponies")
(check (format nil "~{~A~^, ~}" '(a b c)) "A, B, C")
(check (format nil "~:{~A=~A ~}" '((x 1) (y 2))) "X=1 Y=2 ")
(check (format nil "~@{~A~^-~}" 1 2 3) "1-2-3")
(check (format nil "~[zero~;one~;two~]" 1) "one")
(check (format nil "~[zero~;one~:;many~]" 5) "many")
(check (format nil "~:[no~;yes~]" t) "yes")
(check (format nil "~@[(~A)~]~A" nil "x") "x")
(check (format nil "~(HELLO World~) ~:(hello world~) ~@(hello WORLD~) ~:@(up~)") "hello world Hello World Hello world UP")
(check (format nil "a~%b~&c~&") "a
b
c
")
(check (format nil "~A~10Tx" "ab") "ab        x")
(check (format nil "~R ~2R ~16R" 10 5 255) "10 101 FF")
(check (format nil "~C~~~*~A" #\x 'skipped 'shown) "x~SHOWN")
(check (format nil "~? and ~A" "<~A>" '(inner) 'outer) "<INNER> and OUTER")
(check (format nil "one ~
                    line") "one line")
(check (with-output-to-string (s) (format s "~D" 42)) "42")
(check (handler-case (format nil "~Q" 1) (error (c) (condition-message c))) "FORMAT: unknown directive")

;;; ---- loop -------------------------------------------------------------------------------------

(check (loop for i from 1 to 5 collect i) '(1 2 3 4 5))
(check (loop for i from 0 below 10 by 3 collect i) '(0 3 6 9))
(check (loop for i from 5 downto 1 collect i) '(5 4 3 2 1))
(check (loop for x in '(1 2 3 4) sum x) 10)
(check (loop for x in '(1 2 3 4) when (evenp x) collect x) '(2 4))
(check (loop for x in '(1 2 3 4) unless (evenp x) collect x else collect (- x)) '(1 -2 3 -4))
(check (loop for (a b) in '((1 2) (3 4)) collect (+ a b)) '(3 7))
(check (loop for (k . v) in '((a . 1) (b . 2)) collect k) '(a b))
(check (loop for x on '(1 2 3) collect x) '((1 2 3) (2 3) (3)))
(check (loop for c across "abc" collect (char-upcase c)) '(#\A #\B #\C))
(check (loop for x = 1 then (* x 2) repeat 5 collect x) '(1 2 4 8 16))
(check (loop for x in '(1 2 3) for y = (* x 10) collect y) '(10 20 30))
(check (loop for x in '(3 1 4 1 5) maximize x) 5)
(check (loop for x in '(3 1 4 1 5) minimize x) 1)
(check (loop for x in '(a b nil c) count x) 3)
(check (loop for x in '(1 2 3) append (list x x)) '(1 1 2 2 3 3))
(check (loop for x in '(2 4 6) always (evenp x)) t)
(check (loop for x in '(2 4 7) never (oddp x)) nil)
(check (loop for x in '(2 4 7) thereis (and (oddp x) x)) 7)
(check (loop for i from 1 while (< i 4) collect i) '(1 2 3))
(check (loop for i from 1 until (> i 3) collect i) '(1 2 3))
(check (loop for x in '(1 2 3) collect x into xs finally (return (reverse xs))) '(3 2 1))
(check (loop for x in '(1 2 3 4 5) when (> x 3) return x) 4)
(check (loop with n = 3 repeat n collect n) '(3 3 3))
(check (let ((i 0)) (loop (setq i (1+ i)) (when (= i 5) (return i)))) 5)   ; the simple LOOP
(check (loop named outer for i from 1 do (when (= i 3) (return-from outer :done))) :done)
(check (sort (loop for k being the hash-keys of (word-counts '("a" "b" "a")) collect k) #'string<) '("a" "b"))

;;; ---- sequences and strings ----------------------------------------------------------------------

(check (sort (list 3 1 2) #'<) '(1 2 3))
(check (sort (vector 3 1 2) #'<) #(1 2 3))
(check (sort (list "pear" "apple" "fig") #'string<) '("apple" "fig" "pear"))
(check (sort (list '(b 2) '(a 1) '(c 1)) #'< :key #'cadr) '((a 1) (c 1) (b 2)))   ; stable
(check (subseq '(a b c d) 1 3) '(b c))
(check (subseq "hello" 1) "ello")
(check (subseq #(1 2 3) 1) #(2 3))
(check (concatenate 'string "ab" "cd" '(#\e)) "abcde")
(check (concatenate 'list '(1) #(2) "3") '(1 2 #\3))
(check (map 'list #'+ '(1 2) '(10 20)) '(11 22))
(check (map 'string #'char-upcase "abc") "ABC")
(check (reverse "abc") "cba")
(check (reverse #(1 2 3)) #(3 2 1))
(check (find 3 '(1 2 3 4)) 3)
(check (find "b" '("a" "b") :test #'string=) "b")
(check (find-if #'evenp '(1 3 4 5)) 4)
(check (position #\c "abcabc") 2)
(check (position #\c "abcabc" :from-end t) 5)
(check (position 9 '(1 2)) nil)
(check (count #\a "banana") 3)
(check (count-if #'oddp '(1 2 3)) 2)
(check (remove 2 '(1 2 3 2)) '(1 3))
(check (remove #\a "banana") "bnn")
(check (remove-if #'evenp #(1 2 3 4)) #(1 3))
(check (remove-duplicates '(1 2 1 3 2)) '(1 3 2))
(check (remove-duplicates '(1 2 1 3 2) :from-end t) '(1 2 3))
(check (search "na" "banana") 2)
(check (search '(2 3) '(1 2 3)) 1)
(check (mismatch "abc" "abd") 2)
(check (substitute 0 1 '(1 2 1)) '(0 2 0))
(check (reduce #'+ '(1 2 3) :initial-value 10) 16)
(check (reduce #'list '(1 2 3) :from-end t) '(1 (2 3)))
(check (reduce #'+ #(1 2 3)) 6)
(check (reduce #'+ nil) 0)
(check (elt "abc" 1) #\b)
(check (let ((v (vector 1 2 3))) (setf (elt v 0) 9) v) #(9 2 3))
(check (fill (list 1 2 3) 0) '(0 0 0))
(check (nconc (list 1 2) nil (list 3)) '(1 2 3))
(check (butlast '(1 2 3)) '(1 2))
(check (union '(1 2) '(2 3)) '(1 2 3))
(check (intersection '(1 2 3) '(2 3 4)) '(2 3))
(check (getf '(:a 1 :b 2) :b) 2)
(check (string-upcase "Hello") "HELLO")
(check (string-capitalize "hello big world") "Hello Big World")
(check (string-trim " " "  pad  ") "pad")
(check (string< "apple" "banana") 0)
(check (string< "banana" "apple") nil)
(check (string-equal "Lisp" "LISP") t)
(check (multiple-value-list (parse-integer "  -42  ")) '(-42 7))
(check (multiple-value-list (parse-integer "12abc" :junk-allowed t)) '(12 2))
(check (parse-integer "ff" :radix 16) 255)
(check (parse-integer "123456789012345678901234567890") 123456789012345678901234567890)
(check (digit-char-p #\7) 7)
(check (digit-char-p #\z) nil)
(check (make-string 3 :initial-element #\z) "zzz")
(check (char= #\a #\a #\a) t)
(check (typecase 5 (string :s) (integer :i) (t :other)) :i)
(check (typecase "x" (string :s) (integer :i)) :s)
(check (ecase 'b (a 1) (b 2)) 2)
(check (handler-case (ecase 'z (a 1)) (error (c) :no)) :no)
(check (let ((l nil)) (pushnew 1 l) (pushnew 1 l) l) '(1))

(report "cl")
