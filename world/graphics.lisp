;;; -*- Mode: Lisp -*-
;;; graphics.lisp — bitmaps, drawing and fonts, on top of the kernel's three
;;; graphics primitives (kernel/gprims.c):
;;;
;;;   (bitblt alu width height from from-x from-y to to-x to-y)
;;;   (%draw-rectangle width height x y alu ink &optional bitmap)
;;;   (%draw-char font char x y alu &optional bitmap)       => width
;;;
;;; A bitmap is #(:bitmap width height bytes); the screen is :SCREEN.
;;; Ink: 0 is paper (white), 255 is full ink (black).

;;; ---- ALU operations ---------------------------------------------------

(defconstant alu-seta 0)    ; destination = source
(defconstant alu-ior 1)     ; draw ink over what is there
(defconstant alu-xor 2)     ; invert where the source has ink (blinkers)
(defconstant alu-andca 3)   ; erase where the source has ink (white on black)

;;; ---- bitmaps --------------------------------------------------------------

(defun make-bitmap (width height)
  (vector :bitmap width height (%make-bytes (* width height))))

(defun bitmap-width (b) (if (eq b :screen) (%screen-width) (aref b 1)))
(defun bitmap-height (b) (if (eq b :screen) (%screen-height) (aref b 2)))
(defun bitmap-pixel (b x y) (%bitmap-pixel b x y))

(defun set-bitmap-pixel (b x y ink)
  (%draw-rectangle 1 1 x y alu-seta ink b))

(defun screen-width () (%screen-width))
(defun screen-height () (%screen-height))

;;; ---- shapes ---------------------------------------------------------------

(defun draw-rectangle (x y width height &optional (alu alu-seta) (ink 255) (bitmap :screen))
  (%draw-rectangle width height x y alu ink bitmap))

(defun erase-rectangle (x y width height &optional (bitmap :screen))
  (%draw-rectangle width height x y alu-seta 0 bitmap))

(defun clear-screen ()
  (erase-rectangle 0 0 (%screen-width) (%screen-height)))

(defun draw-point (x y &optional (alu alu-seta) (ink 255) (bitmap :screen))
  (%draw-rectangle 1 1 x y alu ink bitmap))

(defun draw-frame (x y width height &optional (thickness 1) (alu alu-seta) (ink 255) (bitmap :screen))
  (%draw-rectangle width thickness x y alu ink bitmap)
  (%draw-rectangle width thickness x (- (+ y height) thickness) alu ink bitmap)
  (%draw-rectangle thickness (- height (* 2 thickness)) x (+ y thickness) alu ink bitmap)
  (%draw-rectangle thickness (- height (* 2 thickness)) (- (+ x width) thickness) (+ y thickness) alu ink bitmap))

;; Bresenham; a thick line is drawn with a square pen
(defun draw-line (x0 y0 x1 y1 &optional (thickness 1) (alu alu-seta) (ink 255) (bitmap :screen))
  (let* ((dx (abs (- x1 x0))) (sx (if (< x0 x1) 1 -1))
         (dy (- (abs (- y1 y0)))) (sy (if (< y0 y1) 1 -1))
         (err (+ dx dy))
         (half (%quotient thickness 2)))
    (loop
      (%draw-rectangle thickness thickness (- x0 half) (- y0 half) alu ink bitmap)
      (when (and (= x0 x1) (= y0 y1)) (return nil))
      (let ((e2 (* 2 err)))
        (when (>= e2 dy) (setq err (+ err dy) x0 (+ x0 sx)))
        (when (<= e2 dx) (setq err (+ err dx) y0 (+ y0 sy)))))))

;; midpoint circle
(defun draw-circle (cx cy r &optional (alu alu-seta) (ink 255) (bitmap :screen))
  (let ((x r) (y 0) (err (- 1 r)))
    (while (>= x y)
      (dolist (p (list (cons x y) (cons y x) (cons (- y) x) (cons (- x) y)
                       (cons (- x) (- y)) (cons (- y) (- x)) (cons y (- x)) (cons x (- y))))
        (%draw-rectangle 1 1 (+ cx (car p)) (+ cy (cdr p)) alu ink bitmap))
      (setq y (1+ y))
      (if (< err 0)
          (setq err (+ err (* 2 y) 1))
          (setq x (1- x) err (+ err (* 2 (- y x)) 1))))))

(defun draw-filled-circle (cx cy r &optional (alu alu-seta) (ink 255) (bitmap :screen))
  (let ((y (- r)))
    (while (<= y r)
      (let ((half 0))
        (while (<= (+ (* (1+ half) (1+ half)) (* y y)) (* r r)) (setq half (1+ half)))
        (%draw-rectangle (1+ (* 2 half)) 1 (- cx half) (+ cy y) alu ink bitmap))
      (setq y (1+ y)))))

;;; ---- fonts ------------------------------------------------------------------
;;;
;;; A font is #(:font name line-height baseline raster x-offsets widths):
;;; all glyphs side by side in one raster bitmap, a line high, and per
;;; character where its glyph starts and how far the pen advances. X-OFFSETS
;;; and WIDTHS are tables of pages: element (ash code -8) is NIL or a vector
;;; of 256, so that a font can have characters from anywhere in Unicode (M15).
;;; A character the font lacks is drawn as the replacement character.

(defun font-name (f) (aref f 1))
(defun font-line-height (f) (aref f 2))
(defun font-baseline (f) (aref f 3))

(defun split-string (string char)
  (let ((parts nil) (start 0) (i 0) (n (length string)))
    (while (< i n)
      (when (eq (aref string i) char)
        (push (substring string start i) parts)
        (setq start (1+ i)))
      (setq i (1+ i)))
    (push (substring string start n) parts)
    (nreverse parts)))

(defun %font-entry (table code)
  (let ((p (ash code -8)))
    (and (< p (length table)) (aref table p) (aref (aref table p) (logand code 255)))))

(defun %font-set (table code value)
  (let ((p (ash code -8)))
    (unless (aref table p) (setf (aref table p) (make-vector 256 nil)))
    (setf (aref (aref table p) (logand code 255)) value)))

;; Build a font from glyph descriptions like those in fonts/5x9.txt.
;; PROPORTIONAL trims each glyph to its ink; BOLD smears it one pixel right.
;; A glyph with ink in the column past GLYPH-WIDTH fills its cell, so that
;; it joins its neighbours (box drawing); it is never trimmed.
(defun build-font (name glyph-lines glyph-width glyph-height line-height baseline
                   &optional proportional bold)
  (declare (ignore glyph-height))
  (let* ((extra (if bold 1 0))
         (cell (+ glyph-width 1 extra))
         (top (let ((m 0)) (dolist (l glyph-lines) (setq m (max m (char-code (aref l 0))))) m))
         (raster (make-bitmap (* (length glyph-lines) cell) line-height))
         (x-offsets (make-vector (1+ (ash top -8)) nil))
         (widths (make-vector (1+ (ash top -8)) nil))
         (k 0))
    (dolist (line glyph-lines)
      (let* ((code (char-code (aref line 0)))
             (base (* k cell))
             (lo glyph-width) (hi -1) (y 0) (full nil))
        (dolist (row (split-string (substring line 2) #\|))
          (dotimes (x (length row))
            (when (eq (aref row x) #\#)
              (set-bitmap-pixel raster (+ base x) y 255)
              (when (and bold (< (+ x 1) cell)) (set-bitmap-pixel raster (+ base x 1) y 255))
              (when (>= x glyph-width) (setq full t))
              (when (< x lo) (setq lo x))
              (when (> (+ x extra) hi) (setq hi (+ x extra)))))
          (setq y (1+ y)))
        (cond (full
               (%font-set x-offsets code base)
               (%font-set widths code cell))
              ((and proportional (>= hi lo))
               (%font-set x-offsets code (+ base lo))
               (%font-set widths code (+ (- hi lo) 2)))
              (t
               (%font-set x-offsets code base)
               (%font-set widths code (if proportional (+ 2 (%quotient glyph-width 2)) cell)))))
      (setq k (1+ k)))
    (vector :font name line-height baseline raster x-offsets widths)))

(defvar *font-fixed*    (build-font "5x9"         *glyphs-5x9* 5 9 11 7))
(defvar *font-variable* (build-font "5x9 variable" *glyphs-5x9* 5 9 11 7 t))
(defvar *font-bold*     (build-font "5x9 bold"    *glyphs-5x9* 5 9 11 7 t t))
(defvar *default-font* *font-fixed*)

(defun char-width (char &optional (font *default-font*))
  (let ((widths (aref font 6)))
    (or (%font-entry widths (char-code char))
        (%font-entry widths #xFFFD)
        (%font-entry widths 63))))

(defun string-width (string &optional (font *default-font*))
  (let ((w 0) (i 0) (n (length string)))
    (while (< i n)
      (setq w (+ w (char-width (aref string i) font)) i (1+ i)))
    w))

(defun draw-char (char x y &optional (font *default-font*) (alu alu-ior) (bitmap :screen))
  (%draw-char font char x y alu bitmap))

;; Y is the top of the line. Returns the x position after the string.
(defun draw-string (string x y &optional (font *default-font*) (alu alu-ior) (bitmap :screen))
  (let ((i 0) (n (length string)))
    (while (< i n)
      (setq x (+ x (%draw-char font (aref string i) x y alu bitmap)) i (1+ i)))
    x))

;;; ---- demonstrations -----------------------------------------------------------
;;; They draw into any bitmap: the screen, or a window's (see demo.lisp).

;; Grey levels, a pixel checker, nested frames, circles and the three fonts.
(defun draw-test-pattern (x y &optional (b :screen))
  (dotimes (i 16)
    (draw-rectangle (+ x (* i 20)) y 20 20 alu-seta (* i 17) b))
  (draw-frame x y 320 20 1 alu-seta 255 b)
  (dotimes (j 20)
    (dotimes (i 40)
      (draw-point (+ x 330 i) (+ y j) alu-seta (if (oddp (+ i j)) 255 0) b)))
  (dotimes (k 4)
    (draw-frame (+ x 380 (* k 3)) (+ y (* k 3)) (- 60 (* k 6)) (- 20 (* k 6)) 1 alu-seta 255 b))
  (dotimes (k 4)
    (draw-circle (+ x 470) (+ y 10) (+ 3 (* k 3)) alu-seta 255 b))
  (let ((ty (+ y 28)) (all ""))
    (dotimes (c 95) (setq all (string-append all (string (code-char (+ c 32))))))
    (draw-string all x ty *font-fixed* alu-ior b)
    (draw-string "The quick brown fox jumps over the lazy dog (variable)." x (+ ty 12) *font-variable* alu-ior b)
    (draw-string "The quick brown fox jumps over the lazy dog (bold)." x (+ ty 24) *font-bold* alu-ior b)
    (draw-rectangle (- x 2) (+ ty 35) 236 13 alu-seta 255 b)
    (draw-string "White ink on a black ground, ALU-ANDCA." x (+ ty 37) *font-variable* alu-andca b)
    (+ ty 50)))

(defun draw-lambda (x y size &optional (thickness 3) (b :screen))
  (let ((half (%quotient size 2)))
    (draw-line (- x (%quotient half 2) (%quotient size 8)) (+ y (%quotient size 16)) (- x (%quotient half 2)) y thickness alu-seta 255 b)
    (draw-line (- x (%quotient half 2)) y (+ x (%quotient half 2)) (+ y size) thickness alu-seta 255 b)
    (draw-line x (+ y half) (- x (%quotient half 2)) (+ y size) thickness alu-seta 255 b)))

;; lines, circles, a lambda and the test pattern, with the top left at X, Y
(defun draw-graphics-demo (x y &optional (b :screen))
  (draw-lambda (+ x 500) (+ y 4) 110 3 b)
  (draw-filled-circle (+ x 300) (+ y 50) 30 alu-seta 136 b)
  (draw-circle (+ x 300) (+ y 50) 40 alu-seta 255 b)
  (dotimes (i 12)
    (draw-line (+ x 14) (+ y 10) (+ x 14 (* i 18)) (+ y 110) 1 alu-seta 255 b))
  (draw-test-pattern x (+ y 130) b))
