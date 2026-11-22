;;; -*- Mode: Lisp -*-
;;; windows.lisp — the window system.
;;;
;;; A window is an object (world/objects.lisp) with a rectangle on the screen
;;; and a bitmap of its own. Everything a window shows is drawn into its
;;; bitmap, so a window that is covered keeps its contents, as a deexposed
;;; Lisp Machine window kept its saved bitmap. The Screen Manager process
;;; composites the bitmaps onto the screen, back to front, over the desktop,
;;; whenever something changed; then it adds the blinker of the selected
;;; window, any pop-up menu, the mouse documentation line and the who-line.
;;;
;;; The Screen Manager is also the process that gets the keyboard and mouse
;;; from the kernel. Keys go to the selected window's input queue; the mouse
;;; selects, moves and reshapes windows and opens the system menu (right
;;; button). Abort (F1) goes to the selected window's process; Select (F2)
;;; selects the next window.
;;;
;;; Window classes are built from mixins: WINDOW has the rectangle and the
;;; bitmap, BORDER-MIXIN draws a frame, LABEL-MIXIN a title bar (black when
;;; the window is selected).

;;; ---- events and keys (kernel/kernel.h) ----------------------------------

(defconstant ev-mouse-move 1)
(defconstant ev-mouse-down 2)
(defconstant ev-mouse-up 3)
(defconstant ev-key-down 4)
(defconstant ev-blur 7)
(defconstant ev-resize 8)
(defconstant ev-redraw 9)
(defconstant ev-files 10)       ; files dropped on the page (editor.lisp)

;; a typed character is its code; the special keys come after the last
;; character, #x10FFFF (M15)
(defconstant key-return #x110100)
(defconstant key-rubout #x110101)
(defconstant key-tab #x110102)
(defconstant key-altmode #x110103)
(defconstant key-abort #x110104)
(defconstant key-select #x110105)
(defconstant key-resume #x110106)
(defconstant key-help #x110107)
(defconstant key-left #x110108)
(defconstant key-right #x110109)
(defconstant key-up #x11010A)
(defconstant key-down #x11010B)

;; a key that types a character
(defun typed-char-p (code) (and (< code #x110000) (graphic-char-p (code-char code))))

(defconstant mod-control 1)
(defconstant mod-meta 2)
(defconstant mod-super 4)
(defconstant mod-hyper 8)
(defconstant bucky-bits 15)

(defun key-name (code mods)
  (let ((names (list (cons key-return "Return") (cons key-rubout "Rubout") (cons key-tab "Tab")
                     (cons key-altmode "Altmode") (cons key-abort "Abort") (cons key-select "Select")
                     (cons key-resume "Resume") (cons key-help "Help") (cons key-left "Left")
                     (cons key-right "Right") (cons key-up "Up") (cons key-down "Down") (cons 32 "Space"))))
    (string-append (if (/= 0 (logand mods mod-control)) "c-" "")
                   (if (/= 0 (logand mods mod-meta)) "m-" "")
                   (if (/= 0 (logand mods mod-super)) "s-" "")
                   (if (/= 0 (logand mods mod-hyper)) "h-" "")
                   (let ((n (assoc code names)))
                     (if n (cdr n) (string (code-char code)))))))

(defun two-digits (n) (if (< n 10) (string-append "0" (princ-to-string n)) (princ-to-string n)))

(defun remove-eq (item list)
  (let ((r nil))
    (dolist (x list) (unless (eq x item) (push x r)))
    (nreverse r)))

;;; ---- state --------------------------------------------------------------------

(defvar *windows* nil)              ; front to back
(defvar *selected-window* nil)
(defvar *selected-process* nil)     ; the kernel delivers Abort to this process
(defvar *screen-manager* nil)       ; the Screen Manager's process
(defvar *screen-dirty* t)
(defvar *background* nil)           ; the desktop, a screen-sized bitmap
(defvar *mouse-x* 0)
(defvar *mouse-y* 0)
(defvar *drag* nil)                 ; (:move w dx dy) (:reshape w dx dy) (:move-follow w dx dy) (:reshape-follow w dx dy)
(defvar *menu* nil)
(defvar *last-key* "none")
(defvar *doc-shown* "")
(defvar *highlight* nil)            ; (window . presentation) under the mouse
(defvar *window-count* 0)
(defvar *desktop-ink* nil)          ; the desktop's colour (color.lisp); NIL: grey dots
(defvar *label-ink* 255)            ; the selected window's title bar
(defvar *edition* nil)              ; :PLEXI when the page is Plexi (the kernel sets it)

(defconstant label-height 15)
(defconstant doc-height 15)
(defconstant who-height 15)
(defconstant corner-size 10)

(defun desktop-height () (- (%screen-height) doc-height who-height))

;;; ---- the window classes ----------------------------------------------------------

(defclass window ()
  ((x :initarg :x :initform 20 :accessor window-x)
   (y :initarg :y :initform 20 :accessor window-y)
   (width :initarg :width :initform 300 :accessor window-width)
   (height :initarg :height :initform 200 :accessor window-height)
   (bitmap :initform nil :accessor window-bitmap)
   (process :initform nil :accessor window-process)
   (input :initform nil :accessor window-input)))      ; keyboard events, oldest first

(defgeneric window-refresh (w))          ; draw everything into the bitmap
(defgeneric draw-contents (w))           ; the inside, without decorations
(defgeneric window-inside (w))           ; => (x y width height) in the bitmap
(defgeneric mouse-documentation (w x y))
(defgeneric window-click (w button x y))
(defgeneric window-blinker (w))          ; => (x y width height) in the bitmap, or NIL
(defgeneric window-key (w event))
(defgeneric window-reshaped (w))         ; the size changed: a new, empty bitmap
(defgeneric window-process-function (w)) ; what to run in its process, or NIL
(defgeneric window-presentation-at (w x y)) ; the presentation at X, Y in the bitmap, or NIL
(defgeneric presentation-rects (w p))    ; where presentation P is: ((x y width height) ...)

(defmethod window-refresh ((w window))
  (erase-rectangle 0 0 (window-width w) (window-height w) (window-bitmap w))
  (draw-contents w))
(defmethod draw-contents ((w window)) nil)
(defmethod window-inside ((w window))
  (list 0 0 (window-width w) (window-height w)))
(defmethod mouse-documentation ((w window) x y)
  "Mouse-L: select this window.   Mouse-R: system menu.   Select (F2): next window.")
(defmethod window-click ((w window) button x y) nil)
(defmethod window-blinker ((w window)) nil)
(defmethod window-key ((w window) event)
  (add-window-input w event))

;; The Screen Manager adds input while the window's process takes it, and a
;; process can be switched out between reading the list and storing the new
;; one, so both sides store with STORE-CONDITIONAL and try again if the list
;; changed in between; otherwise a key could be lost.
(defun add-window-input (w event)
  (let ((i (slot-index w 'input)) (v (instance-slots w)) (done nil))
    (while (not done)
      (let ((old (aref v i)))
        (setq done (store-conditional v i old (append old (list event))))))))

(defun take-window-input (w)
  (let ((i (slot-index w 'input)) (v (instance-slots w)) (item nil) (done nil))
    (while (not done)
      (let ((old (aref v i)))
        (setq item (car old))
        (setq done (store-conditional v i old (cdr old)))))
    item))
(defmethod window-reshaped ((w window)) nil)
(defmethod window-process-function ((w window)) nil)
(defmethod window-presentation-at ((w window) x y) nil)
(defmethod presentation-rects ((w window) p) nil)

(defclass border-mixin ()
  ((border :initarg :border :initform 1 :accessor window-border)))

(defmethod window-refresh :after ((w border-mixin))
  (let ((b (window-bitmap w)) (wd (window-width w)) (ht (window-height w)))
    (draw-frame 0 0 wd ht (window-border w) alu-seta 255 b)
    ;; the reshape grip in the bottom right corner
    (draw-line (- wd corner-size) (- ht 2) (- wd 2) (- ht corner-size) 1 alu-seta 255 b)
    (draw-line (- wd (%quotient corner-size 2)) (- ht 2) (- wd 2) (- ht (%quotient corner-size 2)) 1 alu-seta 255 b)))

(defmethod window-inside ((w border-mixin))
  (let ((r (call-next-method)) (b (+ 2 (window-border w))))
    (list (+ (first r) b) (+ (second r) b) (- (third r) (* 2 b)) (- (fourth r) (* 2 b)))))

(defclass label-mixin ()
  ((label :initarg :label :initform "Window" :accessor window-label)))

(defun draw-label (w)
  (let ((b (window-bitmap w)) (wd (window-width w)))
    (if (eq w *selected-window*)
        (progn (draw-rectangle 0 0 wd label-height alu-seta *label-ink* b)
               (draw-string (window-label w) 6 3 *font-bold* alu-andca b))
        (progn (erase-rectangle 1 1 (- wd 2) (- label-height 1) b)
               (draw-rectangle 0 (1- label-height) wd 1 alu-seta 255 b)
               (draw-rectangle 0 0 wd 1 alu-seta 255 b)
               (draw-string (window-label w) 6 3 *font-bold* alu-ior b)))))

(defmethod window-refresh :after ((w label-mixin))
  (draw-label w))

(defmethod window-inside ((w label-mixin))
  (let ((r (call-next-method)))
    (list (first r) (+ (second r) (- label-height 2)) (third r) (- (fourth r) (- label-height 2)))))

(defun labelled-p (w) (typep w 'label-mixin))

;;; ---- managing windows ------------------------------------------------------------

(defun wake-screen-manager ()
  (when *screen-manager* (setf (aref *screen-manager* 6) 0)))

;; anything that changes what a window shows calls this
(defun window-changed (&optional w)
  (setq *screen-dirty* t)
  (wake-screen-manager))

(defun clamp (x lo hi) (max lo (min x hi)))

(defun make-window-bitmap (w)
  (setf (window-bitmap w) (make-bitmap (window-width w) (window-height w))))

(defun start-window-process (w)
  (let ((f (window-process-function w)))
    (when f
      (setf (window-process w)
            (process-run-function (if (labelled-p w) (window-label w) "Window") #'window-process-top w f))
      (when (eq w *selected-window*) (setq *selected-process* (window-process w))))))

;; A window's process runs (F W) with a debugger, and starts it again after
;; Abort, or after the ABORT restart in the debugger.
(defun window-process-top (w f)
  (let ((*debugger* #'background-debugger) (done nil))
    (while (not done)
      (restart-case
          (progn (when (eq (catch :abort (funcall f w) :done) :done) (setq done t)))
        (abort () :report (string-append "Start the process of " (if (labelled-p w) (window-label w) "the window")
                                         " again")
          nil)))))

;; Put W on the screen in front, start its process, and select it unless
;; SELECT is NIL (then the keyboard stays where it is).
(defun add-window (w &optional (select t))
  (make-window-bitmap w)
  (window-refresh w)
  (setq *windows* (cons w *windows*))
  (start-window-process w)
  (when select (select-window w))
  (window-changed w)
  w)

(defun select-window (w)
  (let ((old *selected-window*))
    (setq *windows* (cons w (remove-eq w *windows*))
          *selected-window* w
          *selected-process* (window-process w))
    (when (and old (not (eq old w)) (memq old *windows*) (labelled-p old)) (draw-label old))
    (when (labelled-p w) (draw-label w))
    (window-changed w)
    w))

;; bring W to the front without giving it the keyboard
(defun expose-window (w)
  (setq *windows* (cons w (remove-eq w *windows*)))
  (window-changed w)
  w)

(defun select-next-window ()
  (when (cdr *windows*)
    (select-window (car (last *windows*)))))

(defun bury-window (w)
  (setq *windows* (append (remove-eq w *windows*) (list w)))
  (select-window (car *windows*)))

(defun kill-window (w)
  (when (window-process w) (process-kill (window-process w)))
  (setq *windows* (remove-eq w *windows*))
  (when (eq w *selected-window*)
    (setq *selected-window* nil *selected-process* nil)
    (when *windows* (select-window (car *windows*))))
  (window-changed)
  nil)

(defun move-window (w x y)
  (setf (window-x w) (clamp x (- 40 (window-width w)) (- (%screen-width) 40)))
  (setf (window-y w) (clamp y 0 (- (desktop-height) label-height)))
  (window-changed w))

(defun reshape-window (w x y width height)
  (setf (window-x w) x)
  (setf (window-y w) y)
  (setf (window-width w) (clamp width 80 (%screen-width)))
  (setf (window-height w) (clamp height 50 (desktop-height)))
  (make-window-bitmap w)
  (window-reshaped w)
  (window-refresh w)
  (window-changed w))

(defun window-at (x y)
  (dolist (w *windows*)
    (when (and (>= x (window-x w)) (< x (+ (window-x w) (window-width w)))
               (>= y (window-y w)) (< y (+ (window-y w) (window-height w))))
      (return w))))

(defun in-corner-p (w lx ly)
  (and (>= lx (- (window-width w) corner-size)) (>= ly (- (window-height w) corner-size))))

;;; ---- compositing ------------------------------------------------------------------

;; the desktop: a light grey made of dots, built a row at a time
;; (in colour, a plain ground of *DESKTOP-INK*)
(defun make-background ()
  (let* ((w (%screen-width)) (h (%screen-height)) (b (make-bitmap w h)) (x 0) (y 0))
    (cond (*desktop-ink*
           (draw-rectangle 0 0 w h alu-seta *desktop-ink* b))
          (t
           (while (< x w) (draw-point x 0 alu-seta 255 b) (setq x (+ x 2)))
           (while (< y h)
             (bitblt alu-seta w 1 b 0 0 b (if (= 0 (rem (%quotient y 2) 2)) 0 1) y)
             (setq y (+ y 2)))
           (erase-rectangle 0 0 1 1 b)
           (draw-point 0 0 alu-seta 255 b)))
    b))

(defun draw-blinker ()
  (let ((w *selected-window*))
    (when (and w (evenp (%quotient (get-internal-real-time) 500)))
      (let ((r (window-blinker w)))
        ;; only where no other window covers it
        (when (and r (eq w (window-at (+ (window-x w) (first r) (%quotient (third r) 2))
                                      (+ (window-y w) (second r) (%quotient (fourth r) 2)))))
          (draw-rectangle (+ (window-x w) (first r)) (+ (window-y w) (second r))
                          (third r) (fourth r) alu-xor 255))))))

;; outline the presentation under the mouse, where it is not covered
(defun draw-highlight ()
  (let ((w (car *highlight*)))
    (when (memq w *windows*)
      (dolist (r (presentation-rects w (cdr *highlight*)))
        (let ((x (+ (window-x w) (first r))) (y (+ (window-y w) (second r))))
          (when (eq w (window-at (+ x 1) (+ y 1)))
            (draw-frame (- x 1) (- y 1) (+ (third r) 2) (+ (fourth r) 1) 1 alu-xor 255)))))))

(defun draw-drag-outline ()
  (let ((w (second *drag*)))
    (when (memq (car *drag*) '(:reshape :reshape-follow))
      (draw-frame (window-x w) (window-y w)
                  (max 80 (+ (- *mouse-x* (window-x w)) (third *drag*)))
                  (max 50 (+ (- *mouse-y* (window-y w)) (fourth *drag*)))
                  2 alu-xor 255))))

(defun composite ()
  (setq *screen-dirty* nil)
  (unless *background* (setq *background* (make-background)))
  (let ((h (desktop-height)))
    (bitblt alu-seta (%screen-width) h *background* 0 0 :screen 0 0)
    (dolist (w (reverse *windows*))
      (bitblt alu-seta (window-width w) (min (window-height w) (- h (window-y w)))
              (window-bitmap w) 0 0 :screen (window-x w) (window-y w)))
    (draw-blinker)
    (when *highlight* (draw-highlight))
    (when *drag* (draw-drag-outline))
    (when *menu* (draw-menu))
    (draw-doc-line)
    (draw-who-line)))

;;; ---- mouse documentation line and who-line ------------------------------------

(defun doc-string ()
  (cond (*menu* "Click on an item to choose it, or outside the menu to cancel.")
        ((and *drag* (memq (car *drag*) '(:move-follow :reshape-follow)))
         "Mouse-L: put the window here.")
        (*drag* "Release the mouse button to finish.")
        (*highlight* (presentation-documentation (car *highlight*) (cdr *highlight*)))
        (t (let ((w (window-at *mouse-x* *mouse-y*)))
             (if (null w)
                 "Mouse-R: system menu (create a Listener, demos).   Select (F2): next window."
                 (let ((lx (- *mouse-x* (window-x w))) (ly (- *mouse-y* (window-y w))))
                   (cond ((in-corner-p w lx ly) "Mouse-L drag: reshape this window.   Mouse-R: system menu.")
                         ((< ly label-height) "Mouse-L drag: move this window.   Mouse-R: system menu.")
                         (t (mouse-documentation w lx ly)))))))))

(defun draw-doc-line ()
  (let ((y (desktop-height)))
    (erase-rectangle 0 y (%screen-width) doc-height)
    (draw-string *doc-shown* 6 (+ y 3) *font-variable*)))

(defun who-line-text ()
  (let* ((s (%heap-stats))
         (up (%quotient (get-internal-real-time) 1000))
         (p *selected-process*)
         (w *selected-window*))
    (string-append (if p (process-whostate p) "")
                   "     " (if (and w (labelled-p w)) (window-label w) "")
                   "     heap " (princ-to-string (%quotient (* 100 (first s)) (second s))) "%"
                   "     gc " (princ-to-string (third s))
                   "     up " (two-digits (%quotient up 3600)) ":" (two-digits (mod (%quotient up 60) 60)) ":" (two-digits (mod up 60))
                   "     processes " (princ-to-string (length *all-processes*))
                   "     " (princ-to-string (%screen-width)) "x" (princ-to-string (%screen-height))
                   "     last key " *last-key*)))

(defun draw-who-line ()
  (let ((y (- (%screen-height) who-height)))
    (draw-rectangle 0 y (%screen-width) who-height)
    (draw-string (who-line-text) 6 (+ y 3) *font-fixed* alu-andca)))

;;; ---- menus ---------------------------------------------------------------------------
;;;
;;; A pop-up menu: #(items x y width height target); an item is (label . function),
;;; the function is called with the target window (or NIL).

(defconstant menu-line 13)

(defun open-menu (items target)
  (let* ((width (+ 20 (reduce #'max (mapcar (lambda (i) (string-width (car i) *font-variable*)) items))))
         (height (+ 6 (* menu-line (length items))))
         (x (clamp (- *mouse-x* 10) 0 (- (%screen-width) width)))
         (y (clamp (- *mouse-y* 6) 0 (- (desktop-height) height))))
    (setq *menu* (vector items x y width height target))
    (window-changed)))

(defun menu-item-at (x y)
  (let ((m *menu*))
    (when (and m (>= x (aref m 1)) (< x (+ (aref m 1) (aref m 3))))
      (let ((i (%quotient (- y (aref m 2) 3) menu-line)))
        (when (and (>= (- y (aref m 2) 3) 0) (< i (length (aref m 0)))) i)))))

;; screen position of the middle of item I (for tests and demos)
(defun menu-item-position (i)
  (list (+ (aref *menu* 1) 10) (+ (aref *menu* 2) 3 (* i menu-line) 6)))

(defun draw-menu ()
  (let* ((m *menu*) (x (aref m 1)) (y (aref m 2)) (hit (menu-item-at *mouse-x* *mouse-y*)) (i 0))
    (erase-rectangle x y (aref m 3) (aref m 4))
    (draw-frame x y (aref m 3) (aref m 4) 1)
    (draw-frame (+ x 2) (+ y 2) (- (aref m 3) 4) (- (aref m 4) 4) 1)
    (dolist (item (aref m 0))
      (draw-string (car item) (+ x 10) (+ y 5 (* i menu-line)) *font-variable*)
      (when (eql i hit)
        (draw-rectangle (+ x 4) (+ y 3 (* i menu-line)) (- (aref m 3) 8) menu-line alu-xor 255))
      (setq i (1+ i)))))

(defun menu-choose ()
  (let* ((m *menu*) (i (menu-item-at *mouse-x* *mouse-y*)))
    (setq *menu* nil)
    (window-changed)
    (when i (funcall (cdr (nth i (aref m 0))) (aref m 5)))))

(defun system-menu-items (w)
  (append
   (list (cons "Create Listener" (lambda (w) (make-listener-window)))
         (cons "Editor" (lambda (w) (ed)))
         (cons "Peek" (lambda (w) (peek))))
   (when w
     (list (cons "Move window" (lambda (w) (select-window w)
                                 (setq *drag* (list :move-follow w (- *mouse-x* (window-x w)) (- *mouse-y* (window-y w))))))
           (cons "Reshape window" (lambda (w) (select-window w) (setq *drag* (list :reshape-follow w 1 1))))
           (cons "Bury window" #'bury-window)
           (cons "Refresh window" (lambda (w) (window-refresh w) (window-changed w)))
           (cons "Kill window" #'kill-window)))
   (list (cons "Select next window" (lambda (w) (select-next-window)))
         (cons "Demo windows" (lambda (w) (demo)))
         (cons "Stop demo windows" (lambda (w) (stop-demo)))
         (cons "Graphics demo" (lambda (w) (graphics-demo)))
         (cons "Plexi neural networks" (lambda (w) (plexi)))
         (if (eq *color-scheme* :color)
             (cons "Black and white screen" (lambda (w) (set-color-scheme :mono)))
             (cons "Colour screen" (lambda (w) (set-color-scheme :color)))))))

;;; ---- input ----------------------------------------------------------------------------

(defun update-doc ()
  (let ((d (doc-string)))
    (unless (eq d *doc-shown*)
      (setq *doc-shown* d)
      (window-changed))))

(defun mouse-down (button)
  (let ((w (window-at *mouse-x* *mouse-y*)))
    (cond (*menu* (menu-choose))
          ((and *drag* (eq (car *drag*) :move-follow)) (setq *drag* nil))
          ((and *drag* (eq (car *drag*) :reshape-follow)) (finish-reshape))
          ((and *highlight* (memq button '(0 2)))        ; on a presentation
           (let ((pw (car *highlight*)) (p (cdr *highlight*)))
             (if (= button 0)
                 (presentation-click pw p)
                 (open-menu (presentation-menu-items pw p) pw))))
          ((= button 2) (open-menu (system-menu-items w) w))
          ((and w (= button 0))
           (select-window w)
           (let ((lx (- *mouse-x* (window-x w))) (ly (- *mouse-y* (window-y w))))
             (cond ((in-corner-p w lx ly)            ; keep the grip under the mouse
                    (setq *drag* (list :reshape w (- (window-width w) lx) (- (window-height w) ly))))
                   ((< ly label-height) (setq *drag* (list :move w lx ly)))
                   (t (window-click w button lx ly))))))
    (update-doc)))

(defun finish-reshape ()
  (let ((w (second *drag*)) (dx (third *drag*)) (dy (fourth *drag*)))
    (setq *drag* nil)
    (reshape-window w (window-x w) (window-y w)
                    (+ (- *mouse-x* (window-x w)) dx) (+ (- *mouse-y* (window-y w)) dy))))

(defun mouse-up (button)
  (cond ((and *drag* (eq (car *drag*) :reshape)) (finish-reshape))
        ((and *drag* (eq (car *drag*) :move)) (setq *drag* nil)))
  (update-doc)
  (window-changed))

;; the presentation under the mouse, if the mouse is inside a window's contents
(defun find-highlight ()
  (unless (or *drag* *menu*)
    (let ((w (window-at *mouse-x* *mouse-y*)))
      (when w
        (let ((lx (- *mouse-x* (window-x w))) (ly (- *mouse-y* (window-y w))))
          (unless (or (< ly label-height) (in-corner-p w lx ly))
            (let ((p (window-presentation-at w lx ly)))
              (when p (cons w p)))))))))

(defun update-highlight ()
  (let ((h (find-highlight)))
    (unless (and (eq (car h) (car *highlight*)) (eq (cdr h) (cdr *highlight*)))
      (setq *highlight* h)
      (window-changed))))

(defun mouse-moved ()
  (update-highlight)
  (when *drag*
    (let ((kind (car *drag*)) (w (second *drag*)))
      (if (memq kind '(:move :move-follow))
          (move-window w (- *mouse-x* (third *drag*)) (- *mouse-y* (fourth *drag*)))
          (window-changed))))
  (when *menu* (window-changed))
  (update-doc))

(defun handle-key (e)
  (let ((code (aref e 1)) (mods (aref e 3)))
    (setq *last-key* (key-name code mods))
    (if (= code key-select)
        (select-next-window)
        (when *selected-window* (window-key *selected-window* e)))
    (window-changed)))

(defun screen-resized ()
  (setq *background* (make-background))
  (dolist (w *windows*)
    (when (or (> (window-width w) (%screen-width)) (> (window-height w) (desktop-height)))
      (let ((width (min (window-width w) (%screen-width)))
            (height (min (window-height w) (desktop-height))))
        (reshape-window w (clamp (window-x w) 0 (- (%screen-width) width))
                        (clamp (window-y w) 0 (- (desktop-height) height)) width height)))
    (move-window w (window-x w) (window-y w)))
  (window-changed))

(defun handle-event (e)
  (let ((type (aref e 0)))
    (cond ((= type ev-key-down) (handle-key e))
          ((= type ev-mouse-move)
           (setq *mouse-x* (aref e 1) *mouse-y* (aref e 2))
           (mouse-moved))
          ((= type ev-mouse-down) (update-highlight) (mouse-down (aref e 1)) (update-highlight))
          ((= type ev-mouse-up) (mouse-up (aref e 1)))
          ((or (= type ev-resize) (= type ev-redraw)) (screen-resized))
          ((= type ev-files) (files-arrived))
          ((= type ev-blur) (setq *drag* nil)))))

;;; ---- the Screen Manager --------------------------------------------------------------

;; restart the processes of windows whose process has died
(defun maintain-windows ()
  (dolist (w *windows*)
    (let ((p (window-process w)))
      (when (and p (not (process-alive-p p)))
        (start-window-process w)))))

;; The kernel runs this as the process that owns the keyboard and mouse.
;; The Screen Manager runs until another one takes over (after an error, see
;; debugger.lisp); then this one ends.
(defun system-top-level ()
  (let ((*debugger* #'screen-manager-debugger))
    (restart-case (screen-manager-loop)
      (abort () :report "Stop this process (the window system goes on)" nil))))

(defun screen-manager-loop ()
  (setq *screen-manager* *current-process*)
  (setq *standard-output* *host-stream*)
  (setq *background* nil *menu* nil *drag* nil)
  (when (null *windows*)
    (make-listener-window)
    (when (eq *edition* :plexi) (plexi-edition)))
  (maintain-windows)
  (update-doc)
  (window-changed)
  (let ((blink -1) (tick 0))
    (while (eq *screen-manager* *current-process*)
      (let ((e (%next-event)))
        (if e
            (handle-event e)
            (let ((now (%quotient (get-internal-real-time) 500)))
              (unless (= now blink)                ; the blinker and the clock
                (setq blink now *screen-dirty* t)
                (when (> now (+ tick 4)) (setq tick now) (maintain-windows)))
              (when *screen-dirty* (composite))
              (%wait-for-event 250)))))))
