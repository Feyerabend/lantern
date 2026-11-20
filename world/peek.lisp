;;; -*- Mode: Lisp -*-
;;; peek.lisp — Peek: a window that watches the machine.
;;;
;;; Three views, redrawn every second by the window's own process:
;;;   Processes  each process, its state, what it waits for, and how much of
;;;              the machine it used since the last look (the kernel counts the
;;;              evaluator steps each process runs)
;;;   Windows    every window, front to back, with its place and its process
;;;   Memory     the heap, the collector, and how many symbols, classes,
;;;              buffers... the world holds
;;; Click a view's name at the top, or type P, W or M. The processes and
;;; windows shown are presentations: Mouse-L inspects one, Mouse-R gives its
;;; commands (Kill Process, Select Window, Kill Window, ...).

(defclass peek-window (label-mixin border-mixin window)
  ((mode :initform :processes :accessor peek-mode)
   (records :initform nil :accessor peek-records)        ; #(object x y width height)
   (steps :initform nil :accessor peek-steps)            ; ((process . steps) ...) at the last look
   (total :initform 0 :accessor peek-total)))            ; all steps at the last look

(defmethod window-process-function ((w peek-window)) #'peek-loop)

(defmethod mouse-documentation ((w peek-window) x y)
  "Peek: click Processes, Windows or Memory (or type P, W, M).  Mouse-L on a process or window inspects it, Mouse-R: its commands.")

(defvar *peek-modes* '((:processes . "Processes") (:windows . "Windows") (:memory . "Memory")))

;; a view's name as a presented object: clicking it switches W to that view
(defun peek-mode-object-p (x) (and (vectorp x) (= (length x) 3) (eq (aref x 0) :peek-mode)))

;;; ---- the process ------------------------------------------------------------------

(defun peek-loop (w)
  (loop
    (peek-redraw w)
    (let ((i 0))
      (while (and (< i 10) (null (window-input w)))
        (process-sleep 100 "Peek")
        (setq i (1+ i))))
    (let ((e (take-window-input w)))
      (cond ((and (consp e) (eq (car e) :mode)) (setf (peek-mode w) (second e)))
            ((vectorp e)
             (case (aref e 1)
               ((112 80) (setf (peek-mode w) :processes))
               ((119 87) (setf (peek-mode w) :windows))
               ((109 77) (setf (peek-mode w) :memory))
               (t nil)))))))

;;; ---- drawing ----------------------------------------------------------------------

(defun string-trim-colon (s) (if (and (> (length s) 0) (eq (aref s 0) #\:)) (substring s 1) s))

(defun peek-cw () (char-cell-width))

;; draw TEXT at X, Y; if OBJECT, record it as a presentation there. => the x after it
(defun peek-text (w text x y &optional object (font *font-fixed*))
  (let ((end (draw-string text x y font alu-ior (window-bitmap w))))
    (when object
      (push (vector object x (1- y) (- end x) (line-height)) (peek-records w)))
    end))

(defun pad (s n)
  (let ((s (if (> (length s) n) (substring s 0 n) s)))
    (string-append s (spaces (- n (length s))))))

(defun pad-left (s n) (string-append (spaces (max 0 (- n (length s)))) s))

(defun peek-redraw (w)
  (let* ((r (window-inside w)) (b (window-bitmap w)) (x (+ (first r) 4)) (y (+ (second r) 3)))
    (erase-rectangle (first r) (second r) (third r) (fourth r) b)
    (setf (peek-records w) nil)
    ;; the views' names, the current one in black
    (dolist (m *peek-modes*)
      (let ((width (string-width (cdr m) *font-bold*)))
        (if (eq (car m) (peek-mode w))
            (progn (draw-rectangle (- x 3) (- y 2) (+ width 6) (+ (line-height) 1) alu-seta 255 b)
                   (draw-string (cdr m) x y *font-bold* alu-andca b)
                   (push (vector (vector :peek-mode (car m) w) (- x 3) (- y 2) (+ width 6) (line-height)) (peek-records w)))
            (peek-text w (cdr m) x y (vector :peek-mode (car m) w) *font-bold*))
        (setq x (+ x width 16))))
    (draw-rectangle (first r) (+ y (line-height) 3) (third r) 1 alu-seta 255 b)
    (let ((x0 (+ (first r) 4)) (y0 (+ y (line-height) 8)))
      (case (peek-mode w)
        (:processes (peek-processes w x0 y0))
        (:windows (peek-windows w x0 y0))
        (t (peek-memory w x0 y0 (- (third r) 8)))))
    (window-changed w)))

(defun peek-processes (w x y)
  (let* ((total (fourth (%heap-stats)))
         (delta (max 1 (- total (peek-total w))))
         (cw (peek-cw)) (lh (line-height)) (now nil))
    (peek-text w (string-append (pad "Process" 22) (pad "State" 10) (pad "Waiting for" 13) " Use  Steps") x y nil *font-bold*)
    (setq y (+ y lh 2))
    (dolist (p *all-processes*)
      (let* ((steps (or (process-steps p) 0))
             (old (cdr (assq p (peek-steps w))))
             (use (if old (%quotient (* 100 (- steps old)) delta) 0)))
        (push (cons p steps) now)
        (peek-text w (pad (process-name p) 21) x y p)
        (peek-text w (string-append (pad (string-downcase (string-trim-colon (symbol-name (process-state p)))) 10)
                                    (pad (or (process-whostate p) "") 12)
                                    (pad-left (princ-to-string (min 100 (max 0 use))) 4) "%"
                                    (pad-left (princ-to-string steps) 11))
                   (+ x (* 22 cw)) y)
        (setq y (+ y lh))))
    (setf (peek-steps w) now)
    (setf (peek-total w) total)))

(defun peek-windows (w x y)
  (let ((cw (peek-cw)) (lh (line-height)))
    (peek-text w (string-append (pad "Window" 22) (pad "Place" 12) (pad "Size" 10) "Process") x y nil *font-bold*)
    (setq y (+ y lh 2))
    (dolist (win *windows*)
      (let ((name (string-append (if (eq win *selected-window*) "* " "  ")
                                 (if (labelled-p win) (window-label win) (prin1-to-string win)))))
        (peek-text w (pad name 21) x y win)
        (peek-text w (string-append (pad (string-append (princ-to-string (window-x win)) "," (princ-to-string (window-y win))) 12)
                                    (pad (string-append (princ-to-string (window-width win)) "x" (princ-to-string (window-height win))) 10))
                   (+ x (* 22 cw)) y)
        (let ((p (window-process win)))
          (when p (peek-text w (process-name p) (+ x (* 44 cw)) y p)))
        (setq y (+ y lh))))))

(defun peek-memory (w x y width)
  (let* ((s (%heap-stats)) (used (first s)) (limit (second s)) (lh (line-height)) (b (window-bitmap w))
         (bar (min 300 (- width 8))) (up (%quotient (get-internal-real-time) 1000)))
    (peek-text w (string-append "Heap: " (princ-to-string used) " of " (princ-to-string limit) " words in use ("
                                (princ-to-string (%quotient (* 100 used) limit)) "%)")
               x y)
    (setq y (+ y lh 2))
    (draw-rectangle x y bar 10 alu-seta 255 b)
    (erase-rectangle (1+ x) (1+ y) (- bar 2) 8 b)
    (draw-rectangle (1+ x) (1+ y) (%quotient (* (- bar 2) used) limit) 8 alu-seta 128 b)
    (setq y (+ y 16))
    (dolist (line (list (string-append "Collections: " (princ-to-string (third s))
                                       "    evaluator steps: " (princ-to-string (fourth s)))
                        (string-append "Up " (two-digits (%quotient up 3600)) ":" (two-digits (mod (%quotient up 60) 60)) ":"
                                       (two-digits (mod up 60)))
                        ""
                        (string-append "Symbols " (princ-to-string (length (%all-symbols)))
                                       "    classes " (princ-to-string (length *classes*))
                                       "    commands " (princ-to-string (length *commands*)))
                        (string-append "Processes " (princ-to-string (length *all-processes*))
                                       "    windows " (princ-to-string (length *windows*))
                                       "    editor buffers " (princ-to-string (length *buffers*)))
                        (string-append "Objects remembered for Use as Input: " (princ-to-string (min 256 *presented-count*)))))
      (peek-text w line x y)
      (setq y (+ y lh)))))

;;; ---- presentations ------------------------------------------------------------------

(defmethod window-presentation-at ((w peek-window) x y)
  (dolist (p (peek-records w))
    (when (and (>= x (aref p 1)) (< x (+ (aref p 1) (aref p 3))) (>= y (aref p 2)) (< y (+ (aref p 2) (aref p 4))))
      (return p))))

(defmethod presentation-rects ((w peek-window) p)
  (list (list (aref p 1) (aref p 2) (aref p 3) (aref p 4))))

;;; ---- commands --------------------------------------------------------------------------

(define-command "Show" ((m peek-mode-object-p)) ()
  "Show this view in Peek."
  (window-key (aref m 2) (list :mode (aref m 1)))
  nil)

(define-command "Kill Process" ((p processp)) ()
  "Stop this process (a window's process starts again)."
  (process-kill p)
  nil)

(define-command "Select Window" ((w window)) ()
  "Bring this window to the front and give it the keyboard."
  (select-window w)
  nil)

(define-command "Bury Window" ((w window)) ()
  "Put this window behind all the others."
  (bury-window w)
  nil)

(define-command "Kill Window" ((w window)) ()
  "Close this window and stop its process."
  (kill-window w)
  nil)

(define-command "Peek" () ()
  "Open Peek, a window on the processes, the windows and memory."
  (peek)
  nil)

(setq *click-commands* (append *click-commands* (list (cons #'peek-mode-object-p "Show"))))

(defun find-peek ()
  (dolist (w *windows*) (when (typep w 'peek-window) (return w))))

;; Open Peek (or bring it to the front), showing MODE.
(defun peek (&optional (mode :processes))
  (let ((w (find-peek)))
    (if w
        (progn (window-key w (list :mode mode)) (expose-window w))
        (progn
          (setq w (make-instance 'peek-window :label "Peek"
                                 :x (max 0 (- (%screen-width) 440)) :y (max 0 (- (desktop-height) 250))
                                 :width (min 430 (%screen-width)) :height (min 240 (desktop-height))))
          (setf (peek-mode w) mode)
          (add-window w nil)))
    w))
