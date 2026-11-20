;;; cl6.lisp -- tests for M16: the reader and the printer complete (|...|
;;; symbols, #S(...) reading, #n= and #n# with *PRINT-CIRCLE*, #., and the
;;; pretty printer).
;;;
;;; The first part is the done-test: a small program in ordinary Common Lisp
;;; style. It keeps a graph with cycles in structures, writes it with
;;; *PRINT-CIRCLE* and reads it back with its sharing; it prints its own
;;; objects through PPRINT-LOGICAL-BLOCK; and it pretty-prints a function
;;; definition the way it would be written.

;;; ---- the program -----------------------------------------------------------------

(defpackage :graphs (:use :common-lisp) (:export #:make-graph #:graph-nodes #:node-name #:node-edges
                                                  #:save-graph #:load-graph #:describe-node))
(in-package :graphs)

(defstruct node name edges)
(defstruct graph nodes)

;; a ring a -> b -> c -> a, with a shortcut a -> c
(defun make-ring ()
  (let* ((a (make-node :name "a")) (b (make-node :name "b")) (c (make-node :name "c")))
    (setf (node-edges a) (list b c) (node-edges b) (list c) (node-edges c) (list a))
    (make-graph :nodes (list a b c))))

(defun save-graph (g)
  (with-output-to-string (s)
    (let ((*print-circle* t) (*print-pretty* nil)) (prin1 g s))))

(defun load-graph (text) (read-from-string text))

(defclass task ()
  ((title :initarg :title :reader task-title)
   (steps :initarg :steps :reader task-steps)))

(defmethod print-object ((x task) stream)
  (pprint-logical-block (stream (task-steps x) :prefix "[" :suffix "]")
    (write-string (task-title x) stream)
    (write-string ":" stream)
    (pprint-indent :block 2 stream)
    (loop (pprint-exit-if-list-exhausted)
          (write-char #\Space stream)
          (pprint-newline :linear stream)
          (princ (pprint-pop) stream))))

(defparameter *source*
  '(defun plan (tasks &key (limit 10))
     (let ((done nil) (count 0))
       (dolist (task tasks (nreverse done))
         (when (>= count limit) (return (nreverse done)))
         (if (task-steps task)
             (progn (push (task-title task) done) (incf count))
             (warn "Task ~A has no steps" (task-title task)))))))

(in-package :cl-user)

(let* ((g (graphs::make-ring))
       (text (graphs:save-graph g))
       (h (graphs:load-graph text)))
  (check text "#S(GRAPHS::GRAPH :NODES (#1=#S(GRAPHS::NODE :NAME \"a\" :EDGES (#2=#S(GRAPHS::NODE :NAME \"b\" :EDGES (#3=#S(GRAPHS::NODE :NAME \"c\" :EDGES (#1#)))) #3#)) #2# #3#))")
  (let ((a (first (graphs:graph-nodes h))) (b (second (graphs:graph-nodes h))) (c (third (graphs:graph-nodes h))))
    (check (list (graphs:node-name a) (graphs:node-name b) (graphs:node-name c)) '("a" "b" "c"))
    (check (eq (first (graphs:node-edges a)) b) t)
    (check (eq (second (graphs:node-edges a)) c) t)
    (check (eq (first (graphs:node-edges c)) a) t)
    (check (eq (first (graphs:node-edges b)) c) t)
    (check (graphs:save-graph h) text)))

(check (let ((*print-right-margin* 30))
         (prin1-to-string (make-instance 'graphs::task :title "Bake" :steps '(mix knead proof shape bake))))
       "[Bake: MIX KNEAD PROOF SHAPE BAKE]")
(check (let ((*print-right-margin* 20) (*print-pretty* t))
         (prin1-to-string (make-instance 'graphs::task :title "Bake" :steps '(mix knead proof shape bake))))
       "[Bake:
   MIX
   KNEAD
   PROOF
   SHAPE
   BAKE]")
(check (let ((*print-right-margin* 64) (*package* (find-package :graphs)))
         (with-output-to-string (s) (pprint graphs::*source* s)))
       "
(DEFUN PLAN (TASKS &KEY (LIMIT 10))
  (LET ((DONE NIL) (COUNT 0))
    (DOLIST (TASK TASKS (NREVERSE DONE))
      (WHEN (>= COUNT LIMIT) (RETURN (NREVERSE DONE)))
      (IF (TASK-STEPS TASK)
          (PROGN (PUSH (TASK-TITLE TASK) DONE) (INCF COUNT))
          (WARN \"Task ~A has no steps\" (TASK-TITLE TASK))))))")
(check (let ((*print-right-margin* 50) (*package* (find-package :graphs)))
         (with-output-to-string (s) (pprint graphs::*source* s)))
       "
(DEFUN PLAN (TASKS &KEY (LIMIT 10))
  (LET ((DONE NIL) (COUNT 0))
    (DOLIST (TASK TASKS (NREVERSE DONE))
      (WHEN (>= COUNT LIMIT)
        (RETURN (NREVERSE DONE)))
      (IF (TASK-STEPS TASK)
          (PROGN
            (PUSH (TASK-TITLE TASK) DONE)
            (INCF COUNT))
          (WARN \"Task ~A has no steps\"
                (TASK-TITLE TASK))))))")
(check (let ((*package* (find-package :graphs)))
         (equal (read-from-string (with-output-to-string (s) (pprint graphs::*source* s)))
                graphs::*source*))
       t)

;;; ---- symbols with escapes ------------------------------------------------------------

(check (symbol-name '|Hello World|) "Hello World")
(check (symbol-name 'a\ b) "A B")
(check (symbol-name 'ab\c) "ABc")
(check (symbol-name '|a|b|c|) "aBc")
(check (eq '|FOO| 'foo) t)
(check (symbol-name '||) "")
(check (symbol-name '\() "(")
(check (symbol-name '|x\|y|) "x|y")
(check (symbolp '|123|) t)
(check (eq (read-from-string ":|key word|") (intern "key word" :keyword)) t)
(check (keywordp (read-from-string ":|key word|")) t)
(check (symbol-name (read-from-string "cl-user::|in package|")) "in package")
(check (prin1-to-string '|Hello World|) "|Hello World|")
(check (prin1-to-string 'foo) "FOO")
(check (prin1-to-string '|123|) "|123|")
(check (prin1-to-string '|1.5|) "|1.5|")
(check (prin1-to-string '|a\|b|) "|a\\|b|")
(check (prin1-to-string '||) "||")
(check (prin1-to-string '|.|) "|.|")
(check (prin1-to-string '|a:b|) "|a:b|")
(check (prin1-to-string (intern "with space" :keyword)) ":|with space|")
(check (prin1-to-string '1+) "1+")
(check (prin1-to-string '+) "+")
(check (prin1-to-string '-1x) "-1X")
(check (princ-to-string '|Hello World|) "Hello World")
(check (format nil "~S ~A" '|x y| '|x y|) "|x y| x y")
(check (read-from-string (prin1-to-string '|Odd (name)|)) '|Odd (name)|)
(check (write-to-string '|MiXed| :case :downcase) "|MiXed|")
(check (write-to-string 'plain :case :downcase) "plain")

;;; ---- #S, #n= and #n#, #. ---------------------------------------------------------------

(defstruct pt (x 0) (y 0) (tag :none))
(check (prin1-to-string (read-from-string "#S(pt :x 1 :y 2)")) "#S(PT :X 1 :Y 2 :TAG :NONE)")
(check (pt-tag (read-from-string "#S(pt :x 1)")) :none)          ; the constructor's defaults
(check (pt-y (read-from-string "#S(pt y 7)")) 7)                  ; any symbol names the slot
(check (pt-x '#S(pt :x 5)) 5)
(check (prin1-to-string (read-from-string (prin1-to-string (make-pt :x 3 :tag "t")))) "#S(PT :X 3 :Y 0 :TAG \"t\")")
(check (handler-case (read-from-string "#S(no-such-structure :a 1)") (error () :error)) :error)
(check (let ((x (read-from-string "(#1=(a) #1# #1#)"))) (and (eq (first x) (second x)) (eq (first x) (third x)))) t)
(check (let ((x (read-from-string "#1=(a b . #1#)"))) (eq (cddr x) x)) t)
(check (let ((x (read-from-string "#1=(#1# x)"))) (eq (car x) x)) t)
(check (let ((v (read-from-string "#1=#(1 #1# 3)"))) (eq (aref v 1) v)) t)
(check (let ((x (read-from-string "(#1=\"s\" #1#)"))) (eq (first x) (second x))) t)
(check (let ((x (read-from-string "(#5=(z) #6=(w) #5# #6#)"))) (list (eq (first x) (third x)) (eq (second x) (fourth x)))) '(t t))
(check (read-from-string "#.(+ 1 2)") 3)
(check (read-from-string "(a #.(list 'b 'c) d)") '(a (b c) d))
(check (handler-case (let ((*read-eval* nil)) (read-from-string "#.(+ 1 2)")) (error () :refused)) :refused)
(check (handler-case (read-from-string "(#1# a)") (error () :no-label)) :no-label)
(check (let ((x '#1=(1 . #1#))) (eq (cdr x) x)) t)               ; in code the REPL reads
(check (car '#.(list 7)) 7)

;;; ---- *PRINT-CIRCLE* --------------------------------------------------------------------

(defun circular-list (&rest items) (let ((l (copy-list items))) (setf (cdr (last l)) l) l))
(check (let ((*print-circle* t)) (prin1-to-string (circular-list 1 2 3))) "#1=(1 2 3 . #1#)")
(check (list (list-length (circular-list 1 2 3)) (list-length (circular-list 1)) (list-length '(1 2 3)) (list-length nil))
       '(nil nil 3 0))
(check (let ((*print-circle* t) (x (list 'a))) (prin1-to-string (list x x))) "(#1=(A) #1#)")
(check (let ((*print-circle* nil) (x (list 'a))) (prin1-to-string (list x x))) "((A) (A))")
(check (let ((*print-circle* t) (v (vector 1 2))) (prin1-to-string (list v v))) "(#1=#(1 2) #1#)")
(check (let ((*print-circle* t) (v (vector 1 nil))) (setf (aref v 1) v) (prin1-to-string v)) "#1=#(1 #1#)")
(check (let ((*print-circle* t) (x (list 1)) (y (list 2))) (prin1-to-string (list x y x y))) "(#1=(1) #2=(2) #1# #2#)")
(check (write-to-string (let ((x (list 'q))) (list x x)) :circle t) "(#1=(Q) #1#)")
(check (let ((*print-circle* t)) (format nil "~S" (circular-list 'a))) "#1=(A . #1#)")
(check (let ((*print-circle* t) (l (list 1 2))) (prin1-to-string (cons l (cdr l)))) "((1 . #1=(2)) . #1#)")
(check (let ((x (read-from-string (let ((*print-circle* t)) (prin1-to-string (circular-list 'x 'y))))))
         (list (car x) (cadr x) (eq (cddr x) x)))
       '(x y t))

;;; ---- the pretty printer ------------------------------------------------------------------

(defun pp (x margin) (let ((*print-right-margin* margin)) (write-to-string x :pretty t)))
(check (pp '(a b c) 80) "(A B C)")
(check (pp '(1 2 3 4 5 6 7 8 9 10 11 12) 12) "(1 2 3 4 5 6
 7 8 9 10 11
 12)")
(check (pp '(let ((a 1) (b 2)) (+ a b)) 80) "(LET ((A 1) (B 2)) (+ A B))")
(check (pp '(let ((alpha 1) (beta 2)) (+ alpha beta)) 20) "(LET ((ALPHA 1)
      (BETA 2))
  (+ ALPHA BETA))")
(check (pp '(if (zerop n) (compute-the-base-case) (recurse-on (1- n))) 30) "(IF (ZEROP N)
    (COMPUTE-THE-BASE-CASE)
    (RECURSE-ON (1- N)))")
(check (pp '(cond ((a) 1) ((b) 2)) 14) "(COND ((A) 1)
      ((B) 2))")
(check (pp '(foo-function first-argument second-argument) 30) "(FOO-FUNCTION FIRST-ARGUMENT
              SECOND-ARGUMENT)")
(check (pp ''x 80) "'X")
(check (pp '(function car) 80) "#'CAR")
(check (pp #(1 2 3) 80) "#(1 2 3)")
(check (pp '(a (b (c))) 80) "(A (B (C)))")
(check (let ((*print-length* 3)) (pp '(1 2 3 4 5) 80)) "(1 2 3 ...)")
(check (let ((*print-level* 1)) (pp '(1 (2) 3) 80)) "(1 # 3)")
(check (pp '(a . b) 80) "(A . B)")
(check (pp "string" 80) "\"string\"")
(check (pp 42 80) "42")
(check (let ((*print-circle* t)) (pp (circular-list 1 2) 80)) "#1=(1 2 . #1#)")
(check (with-output-to-string (s) (pprint '(a b) s)) "
(A B)")
(check (multiple-value-list (pprint 1 (make-string-output-stream))) nil)
(check (write-to-string '(defun f (x) x) :pretty t :right-margin 10) "(DEFUN F
       (X)
  X)")

;; PPRINT-LOGICAL-BLOCK directly
(check (with-output-to-string (s)
         (pprint-logical-block (s '(1 2 3) :prefix "<" :suffix ">")
           (loop (princ (pprint-pop) s) (pprint-exit-if-list-exhausted) (write-string ", " s))))
       "<1, 2, 3>")
(check (let ((*print-pretty* t) (*print-right-margin* 10))
         (with-output-to-string (s)
           (pprint-logical-block (s '(aaa bbb ccc ddd) :per-line-prefix ";; ")
             (loop (princ (pprint-pop) s) (pprint-exit-if-list-exhausted)
                   (write-char #\Space s) (pprint-newline :fill s)))))
       ";; AAA BBB
;; CCC DDD")
(check (let ((*print-pretty* t))
         (with-output-to-string (s)
           (pprint-logical-block (s '(a b))
             (princ (pprint-pop) s) (pprint-newline :mandatory s) (princ (pprint-pop) s))))
       "A
B")
(check (let ((*print-pretty* t) (*print-right-margin* 8))
         (with-output-to-string (s)
           (pprint-logical-block (s '(one two three) :prefix "(" :suffix ")")
             (princ (pprint-pop) s) (pprint-indent :block 3 s)
             (loop (pprint-exit-if-list-exhausted) (write-char #\Space s)
                   (pprint-newline :linear s) (princ (pprint-pop) s)))))
       "(ONE
    TWO
    THREE)")
(check (with-output-to-string (s)
         (pprint-logical-block (s '(1 2 . 3))
           (loop (princ (pprint-pop) s) (pprint-exit-if-list-exhausted) (write-char #\Space s))))
       "1 2 . 3")
(check (let ((*print-length* 2))
         (with-output-to-string (s)
           (pprint-logical-block (s '(1 2 3 4))
             (loop (princ (pprint-pop) s) (pprint-exit-if-list-exhausted) (write-char #\Space s)))))
       "1 2 ...")
(check (with-output-to-string (s) (pprint-logical-block (s 'not-a-list) (write-string "never" s)))
       "NOT-A-LIST")
(check (let ((*print-pretty* nil))
         (with-output-to-string (s)
           (pprint-logical-block (s '(a b) :prefix "{" :suffix "}")
             (princ (pprint-pop) s) (pprint-newline :mandatory s) (princ (pprint-pop) s))))
       "{AB}")
(check (let ((*print-pretty* t) (*print-right-margin* 13))
         (with-output-to-string (s) (pprint-fill s '(alpha beta gamma delta))))
       "(ALPHA BETA
 GAMMA DELTA)")
(check (let ((*print-pretty* t) (*print-right-margin* 12))
         (with-output-to-string (s) (pprint-linear s '(alpha beta gamma delta))))
       "(ALPHA
 BETA
 GAMMA
 DELTA)")
(check (let ((*print-pretty* t)) (with-output-to-string (s) (pprint-fill s '(a b) nil))) "A B")

;; the dispatch table
(let ((*print-pprint-dispatch* (copy-pprint-dispatch)))
  (set-pprint-dispatch '(cons (eql my-op)) (lambda (s x) (format s "<my-op ~{~A~^ ~}>" (cdr x))))
  (check (write-to-string '(list (my-op 1 2) 3) :pretty t) "(LIST <my-op 1 2> 3)")
  (check (write-to-string '(list (my-op 1 2) 3) :pretty nil) "(LIST (MY-OP 1 2) 3)")
  (check (nth-value 1 (pprint-dispatch '(my-op))) t)
  (set-pprint-dispatch '(cons (eql my-op)) nil)
  (check (write-to-string '(my-op 1) :pretty t) "(MY-OP 1)"))
(check (nth-value 1 (pprint-dispatch '(my-op))) nil)

;; FORMAT's logical blocks
(defun fmt (margin control &rest args)
  (let ((*print-pretty* t) (*print-right-margin* margin)) (apply #'format nil control args)))
(check (fmt 80 "~<~A, ~A~:>" '(1 2)) "1, 2")
(check (fmt 80 "~:<~A ~A~:>" '(1 2)) "(1 2)")
(check (fmt 80 "~<[~;~A ~A~;]~:>" '(1 2)) "[1 2]")
(check (fmt 80 "~@<~A-~A~:>" 1 2) "1-2")
(check (fmt 20 "~<~@{~A~^ ~}~:@>" '(the quick brown fox jumps over the lazy dog))
       "THE QUICK BROWN FOX
JUMPS OVER THE LAZY
DOG")
(check (fmt 12 "~:<~W~^ ~_~W~^ ~_~W~:>" '(alpha beta gamma)) "(ALPHA
 BETA
 GAMMA)")
(check (fmt 80 "~:<~W~^ ~_~W~^ ~_~W~:>" '(alpha beta gamma)) "(ALPHA BETA GAMMA)")
(check (fmt 10 "~:<~W ~:I~W~^ ~_~W~:>" '(defun foo bar)) "(DEFUN FOO
       BAR)")
(check (fmt 80 "a~:@_b") "a
b")
(check (fmt 80 "~<x~:@_y~:>" nil) "x
y")
(check (let ((*print-pretty* nil)) (format nil "~<~A~_~A~:>" '(1 2))) "12")
(check (fmt 30 "Items: ~<~@{~A~^, ~:_~}~:>." '(apple banana cherry date elderberry fig))
       "Items: APPLE, BANANA, CHERRY,
       DATE, ELDERBERRY, FIG.")

(report "cl6")
