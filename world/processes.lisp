;;; -*- Mode: Lisp -*-
;;; processes.lisp — lightweight processes, in the spirit of the Lisp Machine's
;;; stack groups and scheduler.
;;;
;;; The kernel's scheduler (kernel/kernel.c) runs every process in
;;; *ALL-PROCESSES* a slice at a time, each frame. A process can be switched
;;; out between any two instructions; its stack and special bindings are then
;;; kept in the heap, inside the process object. Waiting is cooperative:
;;; PROCESS-WAIT checks its condition each time the process gets to run, as
;;; the Lisp Machine scheduler checked wait functions.
;;;
;;; A process is the vector
;;;   #(:process name function args state whostate wake-at
;;;     saved-stack saved-bindings saved-registers input-p result)
;;; (field numbers are shared with kernel/lisp.h).

(defvar *all-processes* nil)
(defvar *current-process* nil)

(defconstant process-state-names '(:new :runnable :waiting :sleeping :dead))

(defun processp (x) (and (vectorp x) (= (length x) 13) (eq (aref x 0) :process)))
(defun process-name (p) (aref p 1))
(defun process-state (p) (nth (aref p 4) process-state-names))
(defun process-whostate (p) (aref p 5))
(defun process-result (p) (aref p 11))
(defun process-steps (p) (aref p 12))        ; evaluator steps it has used (kernel/kernel.c)
(defun process-alive-p (p) (/= (aref p 4) 4))
(defun current-process () *current-process*)
(defun all-processes () *all-processes*)

(defun set-process-whostate (p whostate) (setf (aref p 5) whostate))

;; Start a new process running (apply FUNCTION ARGS).
(defun process-run-function (name function &rest args)
  (let ((p (vector :process name function args 0 "Run" 0 nil nil nil nil nil 0)))
    (setq *all-processes* (append *all-processes* (list p)))
    p))

;; Let the other processes run; carry on at the next turn.
(defun process-yield () (%process-yield))

;; Sleep for MS milliseconds (other processes run meanwhile).
(defun process-sleep (ms &optional (whostate "Sleep"))
  (let ((p *current-process*))
    (when p (set-process-whostate p whostate))
    (%process-sleep ms)
    (when p (set-process-whostate p "Run"))))

;; Wait until (apply PREDICATE ARGS) is true.
(defun process-wait (whostate predicate &rest args)
  (let ((p *current-process*))
    (when p (set-process-whostate p whostate))
    (while (not (apply predicate args)) (%process-yield))
    (when p (set-process-whostate p "Run"))))

;; Stop a process. It is dropped at the end of the scheduler's round.
(defun process-kill (p)
  (setf (aref p 4) 4)
  (when (eq p *current-process*) (%process-yield))
  t)

(defun find-process (name)
  (dolist (p *all-processes*)
    (when (string= (process-name p) name) (return p))))

;;; ---- locks ----------------------------------------------------------------
;;;
;;; #(:lock name owner). The owner is set with STORE-CONDITIONAL, which the
;;; scheduler cannot interrupt, so two processes can never both get it.

(defun make-lock (name) (vector :lock name nil))
(defun lock-owner (lock) (aref lock 2))

(defun %try-lock (lock owner)
  (or (eq (aref lock 2) owner)
      (store-conditional lock 2 nil owner)))

(defun acquire-lock (lock)
  (let ((me (or *current-process* :no-process)))
    (unless (%try-lock lock me)
      (process-wait "Lock" #'%try-lock lock me))
    lock))

(defun release-lock (lock)
  (setf (aref lock 2) nil))

(defmacro with-lock (spec &body body)
  (let ((l (gensym)))
    `(let ((,l ,(car spec)))
       (acquire-lock ,l)
       (unwind-protect (progn ,@body)
         (release-lock ,l)))))

;;; ---- looking at them ----------------------------------------------------------

(defun processes ()
  (fresh-line)
  (dolist (p *all-processes*)
    (princ (if (eq p *current-process*) "* " "  "))
    (princ (process-name p))
    (dotimes (i (max 1 (- 18 (length (process-name p))))) (princ " "))
    (prin1 (process-state p))
    (princ "  ")
    (princ (process-whostate p))
    (terpri))
  (length *all-processes*))
