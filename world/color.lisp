;;; -*- Mode: Lisp -*-
;;; color.lisp — colour inks and the colour map.
;;;
;;; The screen is a raster of inks, 0 to 255. On the black-and-white screen
;;; an ink is darkness: 0 is paper, 255 full ink. A colour screen shows the
;;; same raster through a colour map, as the colour screens of the Lisp
;;; Machines did: *COLOR-MAP* is a string of 768 bytes, the red, green and
;;; blue of each ink, and the kernel reads it whenever it shows the screen
;;; (kernel/kernel.c). NIL means black and white.
;;;
;;; Programs ask for a colour, (color-ink 240 160 30), or a named one,
;;; (ink :orange), and get an ink. Each colour is given the free ink closest
;;; to its own darkness, so a picture drawn in colour still reads on the
;;; black-and-white screen: orange is a light grey there, navy nearly black.
;;; The multiples of 17 are kept for the sixteen greys and never become
;;; colours; every other ink not taken by a colour is a tint between paper
;;; and ink.
;;;
;;;   (set-color-scheme :color)    the colour screen
;;;   (set-color-scheme :mono)     black and white again

(defvar *color-map* nil)                     ; read by the kernel
(defvar *color-scheme* :mono)
(defvar *ink-colors* (make-vector 256 nil))   ; ink -> (r g b), for inks that are colours
(defvar *named-colors* nil)                  ; ((name . ink) ...)

;; the paper and ink of the colour screen
(defvar *paper-color* '(251 250 244))
(defvar *ink-color* '(22 22 44))

(defun color-darkness (r g b)
  (- 255 (%quotient (+ (* 299 r) (* 587 g) (* 114 b)) 1000)))

(defun grey-ink-p (i) (= 0 (rem i 17)))

;; => an ink showing R G B on the colour screen
(defun color-ink (r g b)
  (let ((rgb (list r g b)))
    (or (dotimes (i 256) (when (equal (aref *ink-colors* i) rgb) (return i)))
        (let ((d (color-darkness r g b)) (best nil))
          (dotimes (i 256)
            (unless (or (grey-ink-p i) (aref *ink-colors* i))
              (when (or (null best) (< (abs (- i d)) (abs (- best d))))
                (setq best i))))
          (cond (best (setf (aref *ink-colors* best) rgb)
                      (update-color-map)
                      best)
                (t (min 254 (max 1 d))))))))           ; all taken: the grey

(defmacro defcolor (name r g b)
  `(%defcolor ',name ,r ,g ,b))

(defun %defcolor (name r g b)
  (let ((ink (color-ink r g b)) (old (assoc name *named-colors*)))
    (if old (setf (cdr old) ink) (push (cons name ink) *named-colors*))
    ink))

;; the ink of a named colour; an integer is an ink already
(defun ink (name)
  (cond ((integerp name) name)
        ((assoc name *named-colors*) (cdr (assoc name *named-colors*)))
        (t (error "No such colour:" name))))

(defun ink-color (i)
  (or (aref *ink-colors* i)
      (let ((p *paper-color*) (k *ink-color*))
        (list (+ (first p) (%quotient (* i (- (first k) (first p))) 255))
              (+ (second p) (%quotient (* i (- (second k) (second p))) 255))
              (+ (third p) (%quotient (* i (- (third k) (third p))) 255))))))

(defun make-color-map ()
  (let ((m (%make-bytes 768)))
    (dotimes (i 256)
      (let ((c (ink-color i)))
        (aset (code-char (first c)) m (* 3 i))
        (aset (code-char (second c)) m (+ 1 (* 3 i)))
        (aset (code-char (third c)) m (+ 2 (* 3 i)))))
    m))

(defun update-color-map ()
  (when (eq *color-scheme* :color)
    (setq *color-map* (make-color-map))))

;; A ramp: N inks running through the colours STOPS, ((r g b) ...), evenly.
;; Its inks are in order of darkness when the stops are.
(defun make-color-ramp (n stops)
  (let ((v (make-vector n 0)) (segments (1- (length stops))))
    (dotimes (i n)
      (let* ((pos (/ (* i segments) (max 1 (1- n))))      ; a rational, 0 .. segments
             (k (min (1- segments) (floor pos)))
             (f (- pos k))
             (a (nth k stops)) (b (nth (1+ k) stops)))
        (setf (aref v i)
              (color-ink (round (+ (first a) (* f (- (first b) (first a)))))
                         (round (+ (second a) (* f (- (second b) (second a)))))
                         (round (+ (third a) (* f (- (third b) (third a)))))))))
    v))

;; the ink of X, from 0.0 to 1.0, on RAMP
(defun ramp-ink (ramp x)
  (let ((n (length ramp)))
    (aref ramp (max 0 (min (1- n) (floor (* x n)))))))

;;; ---- the colours -------------------------------------------------------------

;; after Plexi's brochure (Symbolics, 1989): a cornflower ground, magenta cells,
;; orange and yellow for what is active
(defcolor :desktop-blue 104 136 216)
(defcolor :deep-blue 36 52 130)
(defcolor :magenta 150 32 112)
(defcolor :purple 92 36 120)
(defcolor :orange 244 160 28)
(defcolor :yellow 250 212 48)
(defcolor :red 204 40 40)
(defcolor :green 40 150 70)
(defcolor :cyan 64 180 200)
(defcolor :light-grey 214 214 222)
(defcolor :pale-yellow 252 240 190)

;; activations, low to high, as on the brochure's cover: dark purple cells,
;; through magenta, to orange and yellow
(defvar *activation-ramp*
  (make-color-ramp 24 '((44 28 96) (120 30 110) (200 56 84) (240 140 30) (252 210 70))))

;;; ---- schemes ----------------------------------------------------------------

(defun set-color-scheme (scheme)
  (unless (memq scheme '(:color :mono))
    (error "The colour schemes are :COLOR and :MONO, not" scheme))
  (setq *color-scheme* scheme)
  (setq *color-map* (if (eq scheme :color) (make-color-map) nil))
  (setq *desktop-ink* (if (eq scheme :color) (ink :desktop-blue) nil))
  (setq *label-ink* (if (eq scheme :color) (ink :purple) 255))
  (setq *background* nil)
  (dolist (w *windows*) (window-refresh w))
  (window-changed)
  scheme)

(defun color-screen-p () (eq *color-scheme* :color))
