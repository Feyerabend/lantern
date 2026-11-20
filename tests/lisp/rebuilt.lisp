;;; rebuilt.lisp — the M9 done-test, second half. `make test` runs it in a
;;; machine built from build/shot-src/, where the screen test's editor saved
;;; its change to draw-label (the selected window's title moves to x = 30).

(check (and (string-search "(draw-string (window-label w) 30 3 *font-bold* alu-andca b)" (%world-source)) t) t)

;; the compiled DRAW-LABEL draws the selected window's title from x = 30 on:
;; before it, the black label has no white pixels; after it, there is text
(defun label-white-pixels (x0 x1)
  (let* ((w (make-instance 'listener-window :label "MMMM" :x 0 :y 0 :width 200 :height 60)) (n 0))
    (make-window-bitmap w)
    (setq *selected-window* w)
    (draw-label w)
    (dotimes (y 12)
      (let ((x x0))
        (while (< x x1)
          (when (/= (%bitmap-pixel (window-bitmap w) x y) 255) (setq n (1+ n)))
          (setq x (1+ x)))))
    n))
(check (label-white-pixels 4 29) 0)
(check (> (label-white-pixels 30 60) 0) t)

(report "rebuilt")
