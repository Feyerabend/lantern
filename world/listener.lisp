;;; -*- Mode: Lisp -*-
;;; listener.lisp — Listener windows: a read-eval-print loop in a window.
;;;
;;; Each Listener window has its own process running LISTENER-LOOP, its own
;;; text and its own output stream (bound to *STANDARD-OUTPUT* in that
;;; process). Keys arrive in the window's input queue from the Screen Manager.
;;; The text is kept as a transcript, so it is redrawn when the window is
;;; reshaped and survives a saved world.

;;; ---- the text state: a vector, for speed --------------------------------

(defmacro term-lines (tm) `(aref ,tm 0))        ; finished lines, newest first
(defmacro term-chunks (tm) `(aref ,tm 1))       ; pieces of the current line, newest first
(defmacro term-count (tm) `(aref ,tm 2))        ; number of finished lines kept
(defmacro term-row (tm) `(aref ,tm 3))
(defmacro term-col (tm) `(aref ,tm 4))
(defmacro term-cols (tm) `(aref ,tm 5))
(defmacro term-rows (tm) `(aref ,tm 6))
(defmacro term-pending (tm) `(aref ,tm 7))      ; typed text after the last form read
(defmacro term-reading (tm) `(aref ,tm 8))      ; waiting for a key: show the blinker
(defmacro term-rowmap (tm) `(aref ,tm 9))       ; per visual row: (line-serial . first-column)
(defmacro term-serial (tm) `(aref ,tm 10))      ; serial number of the current line
(defmacro term-x0 (tm) `(aref ,tm 11))          ; where the text starts in the bitmap,
(defmacro term-y0 (tm) `(aref ,tm 12))          ;   and the size of a character cell:
(defmacro term-cw (tm) `(aref ,tm 13))          ;   kept here, so drawing a character
(defmacro term-lh (tm) `(aref ,tm 14))          ;   does not ask the window each time

;;; Every logical line has a serial number that never changes, so that a
;;; presentation (an object shown in the text) can be found again from a
;;; position on the screen: the row map says which line and column each
;;; visual row starts with.

(defvar *listener-max-lines* 400)

(defun make-term () (vector nil nil 0 0 0 10 3 "" nil (make-vector 3 nil) 0 0 0 6 11))

(defclass listener-window (label-mixin border-mixin window)
  ((term :initform (make-term) :accessor listener-term)
   (stream :initform nil :accessor listener-stream)
   (banner :initform t :accessor listener-banner-p)
   (records :initform nil :accessor listener-records)))  ; presentations: #(object serial start end)

;; the Listener whose process is running (bound in each Listener's process)
(defvar *listener* nil)

;; the values of the last three forms, as in Common Lisp
(defvar * nil)
(defvar ** nil)
(defvar *** nil)
(defvar / nil)                  ; ... and all their values, as lists (M17)
(defvar // nil)
(defvar /// nil)

(defun char-cell-width () (char-width #\M *font-fixed*))
(defun line-height () (font-line-height *font-fixed*))

;;; ---- geometry ----------------------------------------------------------------

(defun term-layout (w)
  (let ((tm (listener-term w)) (r (window-inside w)))
    (setf (term-cw tm) (char-cell-width))
    (setf (term-lh tm) (line-height))
    (setf (term-x0 tm) (+ (first r) 2))
    (setf (term-y0 tm) (+ (second r) 1))
    (setf (term-cols tm) (max 10 (%quotient (- (third r) 4) (term-cw tm))))
    (setf (term-rows tm) (max 2 (%quotient (- (fourth r) 2) (term-lh tm))))
    (setf (term-rowmap tm) (make-vector (term-rows tm) nil))))

(defun cell-x (w col) (let ((tm (listener-term w))) (+ (term-x0 tm) (* col (term-cw tm)))))
(defun cell-y (w row) (let ((tm (listener-term w))) (+ (term-y0 tm) (* row (term-lh tm)))))

;;; ---- writing ----------------------------------------------------------------------

(defun draw-cell (w ch &optional (tm (listener-term w)) (b (window-bitmap w)))
  (%draw-char *font-fixed* ch (+ (term-x0 tm) (* (term-col tm) (term-cw tm)))
              (+ (term-y0 tm) (* (term-row tm) (term-lh tm)) 1) alu-ior b))

;; to the next visual line, scrolling the text up with BITBLT at the bottom;
;; ENTRY is what the row map gets for the new row
(defun term-newline (w entry)
  (let* ((tm (listener-term w)) (rm (term-rowmap tm)) (rows (term-rows tm)))
    (setf (term-col tm) 0)
    (if (< (1+ (term-row tm)) rows)
        (progn
          (setf (term-row tm) (1+ (term-row tm)))
          (when (< (term-row tm) (length rm)) (setf (aref rm (term-row tm)) entry)))
        (let ((x (cell-x w 0)) (y (cell-y w 0)) (lh (line-height))
              (width (* (term-cols tm) (char-cell-width))) (b (window-bitmap w)))
          (bitblt alu-seta width (* (1- rows) lh) b x (+ y lh) b x y)
          (erase-rectangle x (+ y (* (1- rows) lh)) width lh b)
          (when (= (length rm) rows)
            (dotimes (i (1- rows)) (setf (aref rm i) (aref rm (1+ i))))
            (setf (aref rm (1- rows)) entry))))))

;; the current line continues on the next visual row
(defun term-wrap (w)
  (let* ((tm (listener-term w)) (rm (term-rowmap tm)) (row (term-row tm))
         (cur (and (< row (length rm)) (aref rm row))))
    (term-newline w (cons (term-serial tm) (+ (if cur (cdr cur) 0) (term-cols tm))))))

;; how many characters fit on a line: the pretty printer's margin (pretty.lisp)
(defun listener-columns (w) (term-cols (listener-term w)))

(defun listener-column (tm)
  (let ((n 0)) (dolist (c (term-chunks tm)) (setq n (+ n (length c)))) n))

(defun term-end-line (tm)
  (push (apply #'string-append (reverse (term-chunks tm))) (term-lines tm))
  (setf (term-chunks tm) nil)
  (setf (term-serial tm) (1+ (term-serial tm)))
  (setf (term-count tm) (1+ (term-count tm)))
  (when (> (term-count tm) (+ *listener-max-lines* 50))
    (setf (cdr (nthcdr (1- *listener-max-lines*) (term-lines tm))) nil)
    (setf (term-count tm) *listener-max-lines*)))

(defun listener-write (w string)
  (let ((tm (listener-term w)) (b (window-bitmap w)) (n (length string)) (i 0) (start 0))
    (while (< i n)
      (let ((ch (aref string i)))
        (if (eq ch #\Newline)
            (progn
              (when (> i start) (push (substring string start i) (term-chunks tm)))
              (term-end-line tm)
              (term-newline w (cons (term-serial tm) 0))
              (setq start (1+ i)))
            (progn
              (when (>= (term-col tm) (term-cols tm)) (term-wrap w))
              (draw-cell w ch tm b)
              (setf (term-col tm) (1+ (term-col tm))))))
      (setq i (1+ i)))
    (when (> n start)
      (push (if (= start 0) string (substring string start n)) (term-chunks tm)))
    (window-changed w)))

(defun listener-fresh-line (w)
  (when (term-chunks (listener-term w)) (listener-write w *newline-string*)))

;; take back the last character typed
(defun listener-rubout (w)
  (let* ((tm (listener-term w)) (c (car (term-chunks tm))))
    (when c
      (if (> (length c) 1)
          (setf (car (term-chunks tm)) (substring c 0 (1- (length c))))
          (pop (term-chunks tm)))
      (if (> (term-col tm) 0)
          (setf (term-col tm) (1- (term-col tm)))
          (when (> (term-row tm) 0)
            (setf (term-row tm) (1- (term-row tm)))
            (setf (term-col tm) (1- (term-cols tm)))))
      (erase-rectangle (cell-x w (term-col tm)) (cell-y w (term-row tm))
                       (char-cell-width) (line-height) (window-bitmap w))
      (window-changed w))))

(defun visual-rows (line cols)
  (let ((n (length line)) (i 0) (rows nil))
    (if (= n 0)
        (list "")
        (progn
          (while (< i n)
            (push (substring line i (min n (+ i cols))) rows)
            (setq i (+ i cols)))
          (nreverse rows)))))

;; a logical line as the visual rows it occupies, each (text serial . offset)
(defun visual-rows-of (line serial cols)
  (let ((offset 0) (r nil))
    (dolist (text (visual-rows line cols))
      (push (cons text (cons serial offset)) r)
      (setq offset (+ offset cols)))
    (nreverse r)))

;; redraw the text from the transcript, newest at the bottom
(defmethod draw-contents ((w listener-window))
  (term-layout w)
  (let* ((tm (listener-term w))
         (b (window-bitmap w))
         (cols (term-cols tm))
         (serial (term-serial tm))
         (visual (visual-rows-of (apply #'string-append (reverse (term-chunks tm))) serial cols))
         (count (length visual))
         (lines (term-lines tm)))
    (while (and lines (< count (term-rows tm)))
      (setq serial (1- serial))
      (let ((v (visual-rows-of (car lines) serial cols)))
        (setq visual (append v visual) count (+ count (length v))))
      (setq lines (cdr lines)))
    (while (> count (term-rows tm))
      (setq visual (cdr visual) count (1- count)))
    (setf (term-row tm) 0)
    (dolist (r visual)
      (setf (term-col tm) 0)
      (setf (aref (term-rowmap tm) (term-row tm)) (cdr r))
      (dotimes (i (length (car r)))
        (draw-cell w (aref (car r) i) tm b)
        (setf (term-col tm) (1+ (term-col tm))))
      (setf (term-row tm) (1+ (term-row tm))))
    (setf (term-row tm) (max 0 (1- (term-row tm))))))

(defmethod window-blinker ((w listener-window))
  (let ((tm (listener-term w)))
    (when (term-reading tm)
      (list (cell-x w (min (term-col tm) (1- (term-cols tm)))) (cell-y w (term-row tm))
            (char-cell-width) (line-height)))))

(defmethod mouse-documentation ((w listener-window) x y)
  "Listener: type Lisp, Return evaluates.  Rubout, c-U clears.  Abort (F1) stops.   Mouse-R: system menu.")

;;; ---- reading ------------------------------------------------------------------------

(defun listener-next-key (w)
  (let ((tm (listener-term w)))
    (setf (term-reading tm) t)
    (window-changed w)
    (process-wait "Tyi" (lambda () (window-input w)))
    (setf (term-reading tm) nil)
    (take-window-input w)))


(defun listener-read-line (w)
  (let ((buf ""))
    (loop
      (let* ((e (listener-next-key w))
             (code (if (vectorp e) (aref e 1) 0))
             (mods (if (vectorp e) (aref e 3) 0)))
        (cond ((consp e)                         ; (:command function args), from the mouse
               (run-listener-command e)
               (write-string buf))               ; show the unfinished input again
              ((= code key-resume)                ; F3: in the debugger, the CONTINUE restart
               (debugger-resume))
              ((= code key-help)
               (fresh-line)
               (show-commands)
               (write-string buf))
              ((and (= code key-tab) (command-line-p buf) (not (position-of #\Space buf)))
               (let* ((c (complete-command-name buf)) (new (car c)))
                 (if (> (length new) (length buf))
                     (progn (write-string (substring new (length buf))) (setq buf new))
                     (when (cdr c)
                       (fresh-line)
                       (princ "Possible commands:")
                       (dolist (name (cdr c)) (princ "  ") (princ name))
                       (terpri)
                       (write-string buf)))))
              ((/= 0 (logand mods bucky-bits))
               (when (and (= (logand mods bucky-bits) mod-control) (or (= code 117) (= code 85)))
                 (dotimes (i (length buf)) (listener-rubout w))      ; c-U: clear the input
                 (setq buf "")))
              ((= code key-return)
               (terpri)
               (return buf))
              ((= code key-rubout)
               (when (> (length buf) 0)
                 (setq buf (substring buf 0 (1- (length buf))))
                 (listener-rubout w)))
              ((or (= code key-tab) (typed-char-p code))
               (let ((s (if (= code key-tab) " " (string (code-char code)))))
                 (setq buf (string-append buf s))
                 (write-string s))))))))

(defun listener-read-form (w)
  (let* ((tm (listener-term w)) (text (term-pending tm)))
    (setf (term-pending tm) "")
    (loop
      (let ((r (handler-case (%read-from-string text 0)
                 (error (c) (report-error c) :error))))
        (cond ((consp r)
               (setf (term-pending tm) (substring text (cdr r)))
               (return (car r)))
              ((memq r '(:eof :error)) (setq text ""))))
      (let ((line (listener-read-line w)))
        (when (and (= (length text) 0) (command-line-p line))
          (return (list '%run-command-line line)))
        (setq text (string-append text line *newline-string*))))))

;;; ---- the loop -------------------------------------------------------------------------

(defun report-error (c)
  (fresh-line)
  (princ ">>Error: ")
  (unless (eq (condition-type c) 'error)
    (prin1 (condition-type c))
    (princ ": "))
  (princ (condition-message c))
  (dolist (i (condition-irritants c)) (princ " ") (prin1 i))
  (terpri))

(defun listener-read-eval-print (w)
  (let ((form (listener-read-form w)))
    (set-process-whostate *current-process* "Run")
    (window-changed w)
    ;; an error goes to the debugger (debugger.lisp), on top of this stack
    (let ((results (multiple-value-list (eval form))))
      (unless (and results (eq (car results) '%no-value))   ; a command printed its own
        (setq *** ** ** * * (car results) /// // // / / results)
        (dolist (v results)                ; every value, on a line of its own; none for (VALUES)
          (fresh-line)
          (present v)                      ; the value stays connected to the object
          (terpri))
        (fresh-line)))                     ; the next form starts on a line of its own
    (%world-changed)))

(defun listener-banner ()
  (princ "Lantern 1.0.  Every window is an object with its own bitmap") (terpri)
  (princ "and process.  Drag a label to move a window, its corner") (terpri)
  (princ "to reshape it; click to select.  Mouse-R: system menu.") (terpri)
  (princ "Select (F2): next window.  Abort (F1) stops this one.") (terpri)
  (princ "Values are presentations: Mouse-L on one inspects it,") (terpri)
  (princ "Mouse-R gives its commands.  Help (F4) lists commands;") (terpri)
  (princ "type :Insp and Tab to complete one.  Errors open the") (terpri)
  (princ "debugger; :Edit-Definition 'name opens the editor (Zmacs).") (terpri)
  (terpri))

;; the body of a Listener window's process
(defun ensure-listener-stream (w)
  (or (listener-stream w)
      (setf (listener-stream w)
            (make-stream (lambda (s string) (listener-write w string))
                         (lambda (s) (listener-fresh-line w))
                         w))))

(defun listener-loop (w)
  (let ((*listener* w)
        (*package* *package*)                   ; IN-PACKAGE here changes only this Listener
        (*standard-input* (make-input-stream ""              ; (read) and (read-line) wait for a line here
                                             (lambda () (string-append (listener-read-line w) *newline-string*))))
        (*debugger* #'listener-debugger)
        (*standard-output* (ensure-listener-stream w)))
    (when (listener-banner-p w)
      (setf (listener-banner-p w) nil)
      (listener-banner))
    (loop
      (restart-case
          (when (eq (catch :abort (listener-read-eval-print w) :done) :aborted)
            (listener-aborted w))
        (abort () :report "Return to Listener top level"
          (listener-aborted w))))))

(defun listener-aborted (w)
  (setf (term-pending (listener-term w)) "")
  (fresh-line)
  (princ "[Abort]")
  (terpri))

(defmethod window-process-function ((w listener-window)) #'listener-loop)

(defmethod window-reshaped ((w listener-window))
  (term-layout w))

;; Make a new Listener window, a little below and to the right of the last one.
(defun make-listener-window ()
  (setq *window-count* (1+ *window-count*))
  (let* ((n (count-listeners))
         (offset (* 24 (mod n 6)))
         (w (make-instance 'listener-window
                           :label (string-append "Listener " (princ-to-string *window-count*))
                           :x (+ 8 offset) :y (+ 6 offset)
                           :width (max 300 (%quotient (* (%screen-width) 2) 3))
                           :height (max 150 (- (desktop-height) 40)))))
    (add-window w)))

(defun count-listeners ()
  (let ((n 0)) (dolist (w *windows*) (when (typep w 'listener-window) (setq n (1+ n)))) n))

;; clear this Listener's text
(defun clear-listener ()
  (let ((w (or *listener* *selected-window*)))
    (when (typep w 'listener-window)
      (let ((tm (listener-term w)))
        (setf (term-lines tm) nil) (setf (term-chunks tm) nil) (setf (term-count tm) 0)
        (setf (listener-records w) nil))
      (window-refresh w)
      (window-changed w)))
  nil)
