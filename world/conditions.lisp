;;; -*- Mode: Lisp -*-
;;; conditions.lisp — restarts, and the variables the kernel's debugger hook reads.
;;;
;;; A restart is a way out of an error that some caller offered in advance:
;;;
;;;   (restart-case (open-the-file name)
;;;     (use-value (v) :report "Use another value" v)
;;;     (abort () :report "Give up" nil))
;;;
;;; RESTART-CASE puts its restarts on *RESTARTS* (a special variable, so they
;;; are seen by everything called inside it) and catches a unique tag;
;;; INVOKE-RESTART throws to that tag, so control goes back to the
;;; RESTART-CASE, whose clause then runs. The debugger lists *RESTARTS* as they
;;; were when the error happened. CERROR offers a CONTINUE restart.
;;;
;;; When an error is not handled (no HANDLER-CASE takes it), the kernel looks
;;; at *DEBUGGER*: if it is not NIL, it calls %INVOKE-DEBUGGER (debugger.lisp)
;;; on top of the stack, with the failing frames still there. Listeners,
;;; window processes and the Screen Manager bind *DEBUGGER*; elsewhere (the
;;; terminal REPL, the tests) it is NIL and the error just unwinds.

(defvar *restarts* nil)       ; innermost first: #(:restart name report tag index)
(defvar *debugger* nil)       ; a function of the condition, or NIL: no debugger here
(defvar *debug-level* 0)      ; how many debuggers this process is inside

(defun make-restart (name report tag index) (vector :restart name report tag index))
(defun restartp (x) (and (vectorp x) (= (length x) 5) (eq (aref x 0) :restart)))
(defun restart-name (r) (aref r 1))
(defun restart-report (r) (aref r 2))

(defun compute-restarts () *restarts*)

(defun find-restart (name)
  (if (restartp name)
      name
      (dolist (r *restarts*) (when (eq (restart-name r) name) (return r)))))

(defun invoke-restart (restart &rest args)
  (let ((r (find-restart restart)))
    (unless r (error "There is no restart named" restart))
    (throw (aref r 3) (cons (aref r 4) args))))

;; a clause is (name lambda-list [:report string-form] . body)
(defun restart-clause-report (c)
  (let ((rest (cddr c)))
    (if (eq (car rest) :report) (cadr rest) (string-capitalize-symbol (car c)))))

(defun restart-clause-body (c)
  (let ((rest (cddr c)))
    (if (eq (car rest) :report) (cddr rest) rest)))

(defun string-capitalize-symbol (s)
  (let ((name (symbol-name s)) (r "") (up t))
    (dotimes (i (length name))
      (let ((ch (aref name i)))
        (setq r (string-append r (if (eq ch #\-) " "
                                     (string (if up ch (code-char (let ((k (char-code ch)))
                                                                    (if (and (>= k 65) (<= k 90)) (+ k 32) k))))))))
        (setq up (eq ch #\-))))
    r))

(defmacro restart-case (form &rest clauses)
  (let ((tag (gensym)) (r (gensym)) (i -1) (j -1))
    `(let* ((,tag (list :restart-tag))
            (,r (catch ,tag
                  (list :done
                        (let ((*restarts*
                                (append (list ,@(mapcar (lambda (c)
                                                          (setq i (1+ i))
                                                          `(make-restart ',(car c) ,(restart-clause-report c) ,tag ,i))
                                                        clauses))
                                        *restarts*)))
                          ,form)))))
       (if (eq (car ,r) :done)
           (cadr ,r)
           (case (car ,r)
             ,@(mapcar (lambda (c)
                         (setq j (1+ j))
                         `(,j (apply (lambda ,(cadr c) ,@(restart-clause-body c)) (cdr ,r))))
                       clauses))))))

(defmacro with-simple-restart (spec &rest body)
  `(restart-case (progn ,@body)
     (,(car spec) () :report ,(cadr spec) nil)))

;; signal an error the user may continue from, in which case CERROR returns NIL
(defun cerror (continue-report message &rest irritants)
  (restart-case (apply #'error message irritants)
    (continue () :report continue-report nil)))

(defun abort () (invoke-restart 'abort))

(defun continue ()
  (let ((r (find-restart 'continue)))
    (when r (invoke-restart r))))
