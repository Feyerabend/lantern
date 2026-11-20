;;; debugger.lisp — tests for M8: restarts, and the debugger's live frames.
;;;
;;; *DEBUGGER* is NIL in the terminal REPL; here each test binds it to a
;;; function that does what a person at the debugger would do.

;;; ---- restarts ------------------------------------------------------------------

(check (restart-case (+ 1 (invoke-restart 'use-value 41))
         (use-value (v) :report "Use a value" (+ v 1)))
       42)
(check (restart-case 7 (abort () nil)) 7)
(check (restart-case (progn (invoke-restart 'two 5) 0)
         (one (x) (list :one x))
         (two (x) (list :two x)))
       '(:two 5))
(check (restart-case (mapcar #'restart-name (compute-restarts)) (a () 1) (b () 2)) '(a b))
(check (with-simple-restart (skip "Skip it") (invoke-restart 'skip) :not-here) nil)
(check (restart-report (restart-case (find-restart 'retry) (retry () :report "Try again" nil))) "Try again")
(check (ignore-errors (invoke-restart 'no-such-restart)) nil)
;; restarts are dynamic: a function called inside sees them
(defun take-restart (name) (invoke-restart name 3))
(check (restart-case (take-restart 'here) (here (n) (* n 10))) 30)

;;; ---- frames ---------------------------------------------------------------------

(defun dbg-inner (x) (car x))
(defun dbg-outer (x) (+ 1 (dbg-inner x)))
(defvar *seen* nil)

(defun frame-names () (mapcar (lambda (fp) (%function-name (%frame-function fp))) *debug-frames*))

;; the frames are live: return a value from the one that failed
(check (let ((*debugger* (lambda (c)
                           (setq *seen* (list (condition-message c) (frame-names)
                                              (%frame-args (car *debug-frames*))))
                           (%frame-return (car *debug-frames*) 41))))
         (dbg-outer 5))
       42)
(check (first *seen*) "Not a list:")
(check (first (second *seen*)) 'dbg-inner)
(check (second (second *seen*)) 'dbg-outer)
(check (third *seen*) '(5))

;; retry a frame after redefining its function: the new definition runs
(defun dbg-inner2 (x) (car x))
(defun dbg-outer2 (x) (list :got (dbg-inner2 x)))
(check (let ((*debugger* (lambda (c)
                           (fset 'dbg-inner2 (lambda (x) (* x 10)))
                           (let ((*standard-output* (make-string-output-stream))) (retry-frame 0)))))
         (dbg-outer2 5))
       '(:got 50))

;; leaving a frame runs the unwind-protect cleanups inside it
(defvar *cleaned* nil)
(defun dbg-uwp (x) (unwind-protect (car x) (setq *cleaned* :yes)))
(check (let ((*debugger* (lambda (c) (%frame-return (car *debug-frames*) 7))))
         (list (dbg-uwp 5) *cleaned*))
       '(7 :yes))

;; ... and undoes the special bindings made inside it; in the debugger they are still in effect
(defvar *sv* 1)
(defun dbg-bind (x) (let ((*sv* 2)) (car x)))
(check (let ((*debugger* (lambda (c) (%frame-return (car *debug-frames*) *sv*))))
         (list (dbg-bind 5) *sv*))
       '(2 1))

;; a catch inside the abandoned frame is gone afterwards
(defun dbg-catch (x) (catch :inside (car x)))
(check (let ((*debugger* (lambda (c) (%frame-return (car *debug-frames*) :back))))
         (list (dbg-catch 5) (catch :outer (throw :outer :fine))))
       '(:back :fine))

;; return from an outer frame, skipping the inner ones
(check (let ((*debugger* (lambda (c) (%frame-return (cadr *debug-frames*) :outer))))
         (list (dbg-outer 5)))
       '(:outer))

;; a method frame is named after its method (the generic function and the
;; dispatch made tail calls, so their frames are gone)
(defclass dbg-thing () ())
(defgeneric dbg-poke (x))
(defmethod dbg-poke ((x dbg-thing)) (car x))
(check (let ((*debugger* (lambda (c) (setq *seen* (frame-names)) (%frame-return (car *debug-frames*) nil))))
         (dbg-poke (make-instance 'dbg-thing))
         (first *seen*))
       '(method dbg-poke nil dbg-thing))
(check (%function-name #'dbg-poke) 'dbg-poke)
(check (current-definition (find-method-function 'dbg-poke nil 'dbg-thing))
       (find-method-function 'dbg-poke nil 'dbg-thing))

;; restarts offered by callers can be taken from the debugger
(check (let ((*debugger* (lambda (c) (invoke-restart 'continue))))
         (list (cerror "Go on" "Something is odd") :after))
       '(nil :after))
(check (let ((*debugger* (lambda (c) (setq *seen* (mapcar #'restart-name *debug-restarts*))
                           (invoke-restart 'use-value 9))))
         (restart-case (dbg-outer 5) (use-value (v) (* v 2))))
       18)
(check *seen* '(use-value))

;; the debugger level counts up inside, and an error in a debugger gets a deeper one
(check (let ((*debugger* (lambda (c)
                           (if (= *debug-level* 1)
                               (car 'oops)
                               (%frame-return (car (last *debug-frames*)) *debug-level*)))))
         (dbg-outer 5))
       2)

;; after three levels the error just unwinds, to the nearest handler
(check (handler-case (let ((*debugger* (lambda (c) (car c)))) (dbg-outer 5))
         (error (c) :unwound))
       :unwound)

;; a stack overflow can be debugged too: the debugger uses the stack's reserve
(defun dbg-deep (n) (+ 1 (dbg-deep n)))
(check (let ((*debugger* (lambda (c)
                           (setq *seen* (list (condition-message c) (> (length *debug-frames*) 1000)))
                           (%frame-return (car (last *debug-frames*)) :deep))))
         (dbg-deep 1))
       :deep)
(check *seen* '("Stack overflow (recursion too deep)" t))
(check (dbg-outer '(1)) 2)                         ; and everything works afterwards

;; the locals of a frame, named from the source
(defun dbg-locals (a b) (let ((c (+ a b))) (car c)))
(check (let ((*debugger* (lambda (c) (let ((fp (car *debug-frames*)))
                                       (setq *seen* (list (function-local-names (%frame-function fp))
                                                          (%frame-locals fp)))
                                       (%frame-return fp nil)))))
         (dbg-locals 1 2)
         *seen*)
       '((a b) (1 2 3)))

;; frames that are not on the stack are refused
(check (handler-case (%frame-function 999999) (error (c) (condition-message c))) "Not a frame on this stack:")

;;; ---- the editor's buffers ---------------------------------------------------------

(check (memq "windows.lisp" (mapcar #'identity (world-file-names))) nil)  ; strings are not EQ...
(check (and (find-buffer "windows.lisp") t) t)                              ; ...but FIND-BUFFER finds it
(check (let ((p (car (definition-places 'draw-label)))) (line-text (car p) (cdr p))) "(defun draw-label (w)")
(check (buf-name (car (car (definition-places '(method window-refresh :after border-mixin))))) "windows.lisp")
(check (length (definition-places '(method draw-contents nil window))) 1)
(check (lisp-indentation (make-buffer "t1" (list "(defun f (x)" "x)")) 1) 2)
(check (lisp-indentation (make-buffer "t2" (list "(foo bar" "baz)")) 1) 5)
(check (lisp-indentation (make-buffer "t3" (list "(foo" "baz)")) 1) 1)
(check (lisp-indentation (make-buffer "t4" (list "(f \"(\" #\\(" "x)")) 1) 3)
(check (car (defun-text (make-buffer "t5" (list "(defun g ()" "  (list 1" "        2))" "(other)")))) '(defun g () (list 1 2)))

(report "debugger")
