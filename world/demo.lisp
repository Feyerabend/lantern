;;; -*- Mode: Lisp -*-
;;; demo.lisp — demonstration windows, each driven by its own process.
;;;
;;; They are built from the window system's mixins:
;;;   (defclass clock-window (label-mixin border-mixin window) ...)
;;; and draw only into their own bitmap; the Screen Manager puts them on the
;;; screen. Move, reshape, bury or kill them with the mouse like any window.
;;;
;;; (demo) opens a clock that sleeps between ticks, a bouncing ball, and a
;;; prime finder that never sleeps (the scheduler preempts it). (stop-demo)
;;; closes them. (graphics-demo) opens a window with lines, circles, grey
;;; levels and the fonts.

;; a window whose process redraws its inside again and again
(defclass animated-window (label-mixin border-mixin window) ())

(defgeneric animate (w))     ; draw one step into the bitmap and sleep

(defun animation-loop (w)
  (loop
    (animate w)
    (window-changed w)))

(defmethod window-process-function ((w animated-window)) #'animation-loop)

(defun clear-inside (w)
  (let ((r (window-inside w)))
    (erase-rectangle (first r) (second r) (third r) (fourth r) (window-bitmap w))
    r))

;;; ---- a clock --------------------------------------------------------------------

(defclass clock-window (animated-window) ())

;; sine of k*6 degrees, times 100, k = 0..15 (a quarter turn)
(defconstant sine-table '(0 10 21 31 41 50 59 67 74 81 87 91 95 98 99 100))

(defun sine60 (k)
  (let ((k (mod k 60)))
    (cond ((<= k 15) (nth k sine-table))
          ((<= k 30) (nth (- 30 k) sine-table))
          ((<= k 45) (- (nth (- k 30) sine-table)))
          (t (- (nth (- 60 k) sine-table))))))

(defmethod animate ((w clock-window))
  (let* ((b (window-bitmap w))
         (r (clear-inside w))
         (radius (max 8 (- (%quotient (min (third r) (fourth r)) 2) 6)))
         (cx (+ (first r) radius 8))
         (cy (+ (second r) (%quotient (fourth r) 2)))
         (up (%quotient (get-internal-real-time) 1000)))
    (draw-circle cx cy radius alu-seta 255 b)
    (dotimes (k 12)
      (draw-point (+ cx (%quotient (* (- radius 3) (sine60 (* k 5))) 100))
                  (- cy (%quotient (* (- radius 3) (sine60 (+ 15 (* k 5)))) 100)) alu-seta 255 b))
    (draw-line cx cy (+ cx (%quotient (* (- radius 5) (sine60 up)) 100))
               (- cy (%quotient (* (- radius 5) (sine60 (+ 15 up))) 100)) 2 alu-seta 255 b)
    (draw-line cx cy (+ cx (%quotient (* (%quotient radius 2) (sine60 (%quotient up 60))) 100))
               (- cy (%quotient (* (%quotient radius 2) (sine60 (+ 15 (%quotient up 60)))) 100)) 3 alu-seta 255 b)
    (let ((tx (+ cx radius 12)))
      (draw-string "Uptime" tx (+ (second r) 8) *font-bold* alu-ior b)
      (draw-string (string-append (two-digits (%quotient up 3600)) ":" (two-digits (mod (%quotient up 60) 60))
                                  ":" (two-digits (mod up 60)))
                   tx (+ (second r) 22) *font-fixed* alu-ior b)
      (draw-string (string-append (princ-to-string (length *all-processes*)) " processes")
                   tx (+ (second r) 40) *font-variable* alu-ior b)))
  (process-sleep 250 "Tick"))

;;; ---- a bouncing ball --------------------------------------------------------------

(defclass ball-window (animated-window)
  ((bx :initform 20) (by :initform 15) (dx :initform 3) (dy :initform 2) (radius :initform 7)))

(defmethod animate ((w ball-window))
  (let* ((b (window-bitmap w))
         (r (clear-inside w))
         (radius (slot-value w 'radius))
         (bx (+ (slot-value w 'bx) (slot-value w 'dx)))
         (by (+ (slot-value w 'by) (slot-value w 'dy))))
    (when (or (< bx radius) (> bx (- (third r) radius 1)))
      (setf (slot-value w 'dx) (- (slot-value w 'dx)))
      (setq bx (max radius (min bx (- (third r) radius 1)))))
    (when (or (< by radius) (> by (- (fourth r) radius 1)))
      (setf (slot-value w 'dy) (- (slot-value w 'dy)))
      (setq by (max radius (min by (- (fourth r) radius 1)))))
    (setf (slot-value w 'bx) bx)
    (setf (slot-value w 'by) by)
    (draw-filled-circle (+ (first r) bx) (+ (second r) by) radius alu-seta 160 b)
    (draw-circle (+ (first r) bx) (+ (second r) by) radius alu-seta 255 b))
  (process-sleep 30 "Bounce"))

;;; ---- a process that never sleeps ----------------------------------------------------

(defclass primes-window (animated-window)
  ((candidate :initform 2) (found :initform 0) (last :initform 0)))

(defun primep (n)
  (and (> n 1)
       (let ((d 2) (prime t))
         (while (and prime (<= (* d d) n))
           (when (= 0 (rem n d)) (setq prime nil))
           (setq d (1+ d)))
         prime)))

(defmethod animate ((w primes-window))
  (dotimes (i 200)                       ; no sleeping: the scheduler preempts us
    (let ((n (slot-value w 'candidate)))
      (when (primep n)
        (setf (slot-value w 'found) (1+ (slot-value w 'found)))
        (setf (slot-value w 'last) n))
      (setf (slot-value w 'candidate) (1+ n))))
  (let ((r (clear-inside w)) (b (window-bitmap w)))
    (draw-string (string-append "checked " (princ-to-string (slot-value w 'candidate)))
                 (+ (first r) 4) (+ (second r) 4) *font-fixed* alu-ior b)
    (draw-string (string-append "found " (princ-to-string (slot-value w 'found))
                                ", last " (princ-to-string (slot-value w 'last)))
                 (+ (first r) 4) (+ (second r) 17) *font-fixed* alu-ior b)))

;;; ---- starting and stopping them ------------------------------------------------------

(defvar *demo-windows* nil)

(defun demo ()
  (stop-demo)
  (let* ((x (max 0 (- (%screen-width) 262)))
         (clock (make-instance 'clock-window :label "Clock" :x x :y 4 :width 256 :height 100))
         (ball (make-instance 'ball-window :label "Bounce" :x (- x 40) :y 110 :width 256
                              :height (max 80 (- (desktop-height) 180))))
         (primes (make-instance 'primes-window :label "Primes" :x x
                                :y (- (desktop-height) 60) :width 256 :height 52)))
    (setq *demo-windows* (list clock ball primes))
    (dolist (w *demo-windows*) (add-window w nil))
    (length *demo-windows*)))

(defun stop-demo ()
  (dolist (w *demo-windows*) (kill-window w))
  (setq *demo-windows* nil))

;;; ---- the graphics demo in a window ----------------------------------------------------

(defclass graphics-window (label-mixin border-mixin window) ())

(defmethod draw-contents ((w graphics-window))
  (let ((r (window-inside w)) (b (window-bitmap w)))
    (draw-graphics-demo (+ (first r) 6) (+ (second r) 6) b)))

(defun graphics-demo ()
  (add-window (make-instance 'graphics-window :label "Graphics"
                             :x (max 0 (- (%screen-width) 620)) :y 30 :width 600 :height 270)
              nil)
  t)
