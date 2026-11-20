;;; -*- Mode: Lisp -*-
;;; debugger.lisp — the debugger: live frames, restarts, retrying a frame.
;;;
;;; An error that no HANDLER-CASE takes does not unwind the stack. The kernel
;;; calls %INVOKE-DEBUGGER on top of it (kernel/eval.c), so the frames that
;;; failed are all still there: the debugger lists them (a backtrace), shows
;;; their arguments and locals as presentations, and can leave any of them:
;;;
;;;   :Retry             call the frame's function again, with the same
;;;                      arguments; if it was redefined meanwhile (in the
;;;                      editor, say) the new definition runs
;;;   :Return form       make the frame return the value of FORM
;;;   :Restart n         take one of the restarts offered by the callers
;;;                      (Abort, F1, takes the innermost ABORT restart)
;;;
;;; Leaving a frame runs the unwind-protect cleanups inside it.
;;;
;;; Where the debugger runs: in a Listener, in the Listener itself. A window's
;;; process (a demo, the Inspector) opens a Debugger window and debugs there.
;;; The Screen Manager first hands the keyboard and mouse to a new Screen
;;; Manager (the kernel starts one), so the screen keeps working while you
;;; look at what went wrong; after a restart the old one just ends.

(defvar *debug-condition* nil)
(defvar *debug-frames* nil)      ; the failed frames, innermost first: their fps
(defvar *debug-restarts* nil)    ; the restarts as they were when the error happened
(defvar *debug-session* nil)     ; a fresh cons per debugger, to recognize its frames
(defvar *debug-frame* 0)         ; the current frame, an index into *DEBUG-FRAMES*


;; frames that are just the machinery of calling a generic function
(defvar *debugger-hidden-functions*
  '(%dispatch run-effective-method call-methods run-inner
    error %signal-hook %run-handlers %unhandled invoke-debugger))   ; the condition system (M13)

;;; ---- entering ---------------------------------------------------------------

(defun %invoke-debugger (c)
  (let ((*debug-level* (1+ *debug-level*))
        (*debug-frames* (debugger-visible-frames (cdr (%frames))))
        (*debug-restarts* (compute-restarts))
        (*debug-condition* c)
        (*debug-session* (list c))
        (*debug-frame* 0))
    (funcall *debugger* c)
    (loop (process-sleep 100000 "Debugger done"))))   ; not reached: it leaves by a restart

(defun debugger-visible-frames (frames)
  (remove-if (lambda (fp) (memq (%function-name (%frame-function fp)) *debugger-hidden-functions*))
             frames))

;; in a Listener's process
(defun listener-debugger (c)
  (let ((w *listener*))
    (let ((*standard-output* (ensure-listener-stream w)))
      (debugger-loop w))))

(defclass debugger-window (listener-window) ())

(defmethod window-process-function ((w debugger-window)) nil)   ; the failed process reads it

(defmethod mouse-documentation ((w debugger-window) x y)
  "Debugger: Mouse-L on a frame shows it, on a restart takes it.  Mouse-R: more.  Abort (F1) leaves.")

;; in any other process: talk in a window of its own
(defun background-debugger (c)
  (let* ((p *current-process*)
         (name (if p (process-name p) "Lisp"))
         (n (* 20 (mod (count-debugger-windows) 5)))
         (w (make-instance 'debugger-window
                           :label (string-append "Debugger: " name)
                           :x (+ 40 n) :y (+ 40 n)
                           :width (max 360 (%quotient (* (%screen-width) 3) 5))
                           :height (max 200 (%quotient (* (desktop-height) 3) 5)))))
    (setf (listener-banner-p w) nil)
    (setf (window-process w) p)             ; so that Abort (F1) comes to this process
    (unwind-protect
        (let ((*listener* w) (*standard-output* (ensure-listener-stream w)))
          (add-window w)
          (debugger-loop w))
      (setf (window-process w) nil)           ; close the window, not the process
      (when (memq w *windows*) (kill-window w)))))

(defun count-debugger-windows ()
  (let ((n 0)) (dolist (w *windows*) (when (typep w 'debugger-window) (setq n (1+ n)))) n))

;; in the Screen Manager: hand over the keyboard and mouse first
(defun screen-manager-debugger (c)
  (if (>= (count-debugger-windows) 3)
      (progn (%host-write (string-append ">>Error in the Screen Manager: " (condition-message c)
                                         *newline-string*))
             (invoke-restart 'abort))
      (progn
        (when (eq *screen-manager* *current-process*)
          (setq *screen-manager* nil)
          (setf (aref *current-process* 10) nil)   ; not the input process: the kernel starts a new one
          (setf (aref *current-process* 1) "Old Screen Manager"))
        (background-debugger c))))

;;; ---- the debugger's read-eval-print loop -------------------------------------

(defun debugger-loop (w)
  (fresh-line)
  (report-error *debug-condition*)
  (show-restarts)
  (show-backtrace 8)
  (princ "Mouse-L on a frame shows its locals, on a restart takes it; :Retry, :Return form,") (terpri)
  (princ ":Edit-Frame, :Backtrace, :Frame n, :Restart n.  Abort (F1) takes restart ABORT.") (terpri)
  (loop
    (when (eq (catch :abort (debugger-read-eval-print w) :done) :aborted)
      (debugger-abort))))

(defun debugger-abort ()
  (let ((r (dolist (r *debug-restarts*) (when (eq (restart-name r) 'abort) (return r)))))
    (if r
        (invoke-restart r)
        (progn (fresh-line) (princ "There is no ABORT restart.") (terpri)))))

(defun debugger-read-eval-print (w)
  (fresh-line)
  (princ (string-append "Debug " (princ-to-string *debug-level*) "> "))
  (let ((form (listener-read-form w)))
    (window-changed w)
    ;; a form that fails takes you one debugger deeper; its ABORT comes back here
    (let ((result (restart-case (let ((v (catch :abort (list (eval form)))))
                                  (if (eq v :aborted) (progn (fresh-line) (princ "[Abort]") (terpri) nil) v))
                    (abort () :report (string-append "Return to debugger level " (princ-to-string *debug-level*))
                      nil))))
      (when (and result (not (eq (car result) '%no-value)))
        (setq *** ** ** * * (car result))
        (fresh-line)
        (present (car result))
        (terpri)))))

;; Resume (F3): the CONTINUE restart, if there is one
(defun debugger-resume ()
  (when (> *debug-level* 0)
    (let ((r (dolist (r *debug-restarts*) (when (eq (restart-name r) 'continue) (return r)))))
      (when r (invoke-restart r)))))

;;; ---- showing frames and restarts -----------------------------------------------

;; a frame as a presented object: #(:frame session index)
(defun make-debug-frame (i) (vector :frame *debug-session* i))
(defun debug-frame-p (x) (and (vectorp x) (= (length x) 3) (eq (aref x 0) :frame)))

(defun frame-fp (f)
  (unless (eq (aref f 1) *debug-session*)
    (error "That frame is from a debugger that has been left"))
  (nth (aref f 2) *debug-frames*))

(defun frame-index (f) (frame-fp f) (aref f 2))

;; write TEXT, recording it as a presentation of OBJECT (in a Listener)
(defun present-text (object text)
  (let* ((s (resolve-stream nil)) (w (stream-data s)))
    (if (typep w 'listener-window)
        (let* ((tm (listener-term w)) (serial (term-serial tm)) (start (listener-column tm)))
          (listener-write w text)
          (when (= serial (term-serial tm))
            (push (vector object serial start (listener-column tm)) (listener-records w))))
        (princ text))))

;; a function's name for people
(defun function-display-name (f)
  (let ((name (%function-name f)))
    (cond ((null name) "lambda")
          ((eq name '%top-level-form) "a form typed in")
          ((and (consp name) (eq (car name) 'method))
           (string-append (prin1-to-string (second name))
                          (if (third name) (string-append " " (prin1-to-string (third name))) "")
                          " (" (prin1-to-string (fourth name)) ")"))
          (t (prin1-to-string name)))))

;; (NAME arg ...), the whole a presentation of the frame, each argument one of itself
(defun show-frame-line (i)
  (let* ((s (resolve-stream nil)) (w (stream-data s)) (fp (nth i *debug-frames*))
         (f (%frame-function fp)) (frame (make-debug-frame i)))
    (if (typep w 'listener-window)
        (let* ((tm (listener-term w)) (serial (term-serial tm)) (start (listener-column tm)))
          (listener-write w (string-append "(" (function-display-name f)))
          (let ((n 0))
            (dolist (a (%frame-args fp))
              (listener-write w " ")
              (setq n (1+ n))
              (if (> n 6) (progn (listener-write w "...") (return nil)) (listener-present w a 2))))
          (listener-write w ")")
          (when (= serial (term-serial tm))
            (push (vector frame serial start (listener-column tm)) (listener-records w))))
        (prin1 (cons (%function-name f) (%frame-args fp))))))

(defun show-restarts ()
  (if (null *debug-restarts*)
      (progn (princ "No restarts.") (terpri))
      (let ((i 0) (keyed nil))
        (princ "Restarts:") (terpri)
        (dolist (r *debug-restarts*)
          (princ (if (< i 10) "   " "  ")) (princ i) (princ ": ")
          ;; Abort (F1) and Resume (F3) take the innermost ABORT and CONTINUE
          (present-text r (string-append (restart-report r)
                                         (if (and (memq (restart-name r) '(abort continue))
                                                  (not (memq (restart-name r) keyed)))
                                             (string-append "  [" (if (eq (restart-name r) 'abort) "Abort" "Resume") "]")
                                             "")))
          (push (restart-name r) keyed)
          (terpri)
          (setq i (1+ i))))))

(defun show-backtrace (&optional limit)
  (princ "Backtrace:") (terpri)
  (let ((i 0) (n (length *debug-frames*)))
    (dolist (fp *debug-frames*)
      (when (and limit (>= i limit))
        (princ "   ... ") (princ (- n i)) (princ " more; :Backtrace shows them all") (terpri)
        (return nil))
      (princ (if (= i *debug-frame*) " > " "   ")) (princ i) (princ "  ")
      (show-frame-line i)
      (terpri)
      (setq i (1+ i))))
  (when (null *debug-frames*) (princ "   (no compiled frames)") (terpri)))

;; the names of a function's local slots, from its source, as far as known
(defun function-local-names (f)
  (let* ((name (%function-name f))
         (source (cond ((and name (symbolp name)) (get name 'source))
                       ((and (consp name) (eq (car name) 'method))
                        (find-method-source (second name) (third name) (fourth name)))))
         (ll (cond ((null source) nil)
                   ((eq (car source) 'defmethod)
                    (let ((rest (cddr source)))
                      (when (keywordp (car rest)) (setq rest (cdr rest)))
                      (mapcar (lambda (p) (if (consp p) (car p) p)) (car rest))))
                   (t (third source))))
         (names nil) (key nil))
    (dolist (p ll)
      (cond ((eq p '&key) (setq key t))
            ((memq p '(&optional &rest &body)) nil)
            (key nil)
            (t (push (if (consp p) (car p) p) names))))
    (when (and key (not (memq '&rest ll))) (push '&key-arguments names))
    (nreverse names)))

(defun show-frame (i)
  (let* ((fp (nth i *debug-frames*)) (f (%frame-function fp))
         (names (function-local-names f)) (k 0))
    (setq *debug-frame* i)
    (fresh-line)
    (princ "Frame ") (princ i) (princ ": ") (show-frame-line i) (terpri)
    (dolist (v (%frame-locals fp))
      (princ "   ")
      (princ (if names (symbol-name (car names)) (string-append "local " (princ-to-string k))))
      (princ " = ")
      (present v)
      (terpri)
      (setq names (cdr names) k (1+ k)))
    (princ "   Function: ") (present f) (terpri)
    nil))

;;; ---- leaving a frame -------------------------------------------------------------

;; the definition to run when a frame is retried: the newest one of the same name
(defun current-definition (f)
  (let ((name (%function-name f)))
    (cond ((and name (symbolp name) (fboundp name)) (symbol-function name))
          ((and (consp name) (eq (car name) 'method))
           (or (find-method-function (second name) (third name) (fourth name)) f))
          (t f))))

(defun retry-frame (i)
  (let ((fp (nth i *debug-frames*)))
    (unless fp (error "There is no frame" i))
    (fresh-line) (princ "Retrying ") (show-frame-line i) (terpri)
    (%frame-reinvoke fp (current-definition (%frame-function fp)) (%frame-args fp))))

(defun return-from-frame (i value)
  (let ((fp (nth i *debug-frames*)))
    (unless fp (error "There is no frame" i))
    (%frame-return fp value)))

(defun in-debugger ()
  (unless (> *debug-level* 0) (error "Not in the debugger")))

;;; ---- commands --------------------------------------------------------------------

(define-command "Show Frame" ((f debug-frame-p)) (:output t)
  "Make this frame the current one and show its arguments and locals."
  (show-frame (frame-index f)))

(define-command "Retry Frame" ((f debug-frame-p)) (:output t)
  "Call this frame's function again with the same arguments (the newest definition)."
  (retry-frame (frame-index f)))

(define-command "Return from Frame" ((f debug-frame-p)) (:output t)
  "Make this frame return a value: you type a form for it."
  (let ((i (frame-index f)))
    (fresh-line) (princ "Form for the value to return: ")
    (return-from-frame i (eval (listener-read-form *listener*)))))

(define-command "Edit Function" ((f debug-frame-p)) ()
  "Edit the definition of this frame's function in the editor."
  (edit-definition (%function-name (%frame-function (frame-fp f)))))

(define-command "Invoke Restart" ((r restartp)) (:output t)
  "Take this restart."
  (invoke-restart r))

(define-command "Restart" ((n :integer)) (:output t)
  "Take restart number N."
  (in-debugger)
  (let ((r (nth n *debug-restarts*)))
    (unless r (error "There is no restart" n))
    (invoke-restart r)))

(define-command "Retry" () (:output t)
  "Call the current frame's function again (its newest definition)."
  (in-debugger)
  (retry-frame *debug-frame*))

(define-command "Return" ((value :form)) (:output t)
  "Make the current frame return VALUE."
  (in-debugger)
  (return-from-frame *debug-frame* value))

(define-command "Frame" ((n :integer)) (:output t)
  "Make frame N the current one and show it."
  (in-debugger)
  (unless (nth n *debug-frames*) (error "There is no frame" n))
  (show-frame n))

(define-command "Backtrace" () (:output t)
  "Show all the frames."
  (in-debugger)
  (show-backtrace))

(define-command "Edit Frame" () (:output t)
  "Edit the definition of the current frame's function."
  (in-debugger)
  (edit-definition (%function-name (%frame-function (nth *debug-frame* *debug-frames*))))
  nil)

(define-command "Abort" () (:output t)
  "Take the ABORT restart."
  (in-debugger)
  (debugger-abort))

;; Mouse-L: a frame is shown, a restart is taken
(setq *click-commands* (append *click-commands*
                               (list (cons #'debug-frame-p "Show Frame")
                                     (cons #'restartp "Invoke Restart"))))
