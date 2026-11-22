;;; plexi.lisp — tests for the colour inks (world/color.lisp) and the neural
;;; networks of Plexi (world/plexi.lisp).

(setq *random-state* 19890101)

;;; colour inks
(let ((i (color-ink 1 2 3)))
  (check (= i (color-ink 1 2 3)) t)                     ; the same colour, the same ink
  (check (equal (aref *ink-colors* i) '(1 2 3)) t)
  (check (grey-ink-p i) nil)
  (check (< (abs (- i (color-darkness 1 2 3))) 20) t))   ; near its darkness
(check (grey-ink-p 136) t)
(check (aref *ink-colors* 136) nil)                     ; the greys stay grey
(check (aref *ink-colors* 0) nil)
(check (aref *ink-colors* 255) nil)
(check (< (ink :yellow) (ink :orange) (ink :magenta)) t) ; light to dark
(check (integerp (ink :desktop-blue)) t)
(check (ink 42) 42)
(check (let ((r *activation-ramp*)) (> (ramp-ink r 0.0) (ramp-ink r 1.0))) t)   ; dark purple to yellow
(check (ramp-ink *activation-ramp* 2.0) (ramp-ink *activation-ramp* 1.0))
(set-color-scheme :color)
(check (length *color-map*) 768)
(check (char-code (aref *color-map* (* 3 255))) (first *ink-color*))
(check (char-code (aref *color-map* (+ 2 (* 3 (ink :orange))))) 28)
(set-color-scheme :mono)
(check *color-map* nil)

;;; matrices
(let ((inv (invert-matrix (vector (vector 4.0 7.0) (vector 2.0 6.0)))))
  (check (round (* 100 (aref (aref inv 0) 0))) 60)
  (check (round (* 100 (aref (aref inv 0) 1))) -70)
  (check (round (* 100 (aref (aref inv 1) 0))) -20)
  (check (round (* 100 (aref (aref inv 1) 1))) 40))

;;; building a network by hand
(let* ((net (make-network "Tiny"))
       (a (add-layer net "A" 1 2 :role :input))
       (b (add-layer net "B" 1 1 :role :output :transfer :linear)))
  (connect net "A" "B")
  (check (layer-size a) 2)
  (check (eq (find-layer net "B") b) t)
  (setf (aref (aref (bundle-weights (first (network-bundles net))) 0) 0) 2.0)
  (setf (aref (aref (bundle-weights (first (network-bundles net))) 0) 1) -1.0)
  (set-activations a '(1 3))
  (propagate net)
  (check (aref (layer-act b) 0) -1.0))

;; a new transfer function is there at once
(deftransfer :test-double (x) (* 2.0 x))
(check (funcall (transfer-function :test-double) 1.5) 3.0)
(check (car (last *transfer-functions*)) (assoc :test-double *transfer-functions*))

;;; Hopfield memory: every letter of PLEXI is stable, and a noisy one is recalled
(defun settle (net)
  (let ((k 0)) (while (and (not (run-step net)) (< k 1000)) (setq k (1+ k)))))

(let ((net (hopfield-memory)))
  (setf (network-noise net) 0.0)
  (dolist (p (network-patterns net))
    (present-noisy net p)
    (settle net)
    (check (network-status net) (string-append "Settled on " (third p))))
  (setf (network-noise net) 0.1)
  (let ((ok 0))
    (dotimes (k 10)
      (present-noisy net (first (network-patterns net)))
      (settle net)
      (when (equal (network-status net) "Settled on P") (setq ok (1+ ok))))
    (check (>= ok 8) t))
  (check (< (first (network-history net)) 0.0) t))     ; the energy, at a minimum

;;; Competition: each letter gets a cell of its own, and the output layer
;;; learns to give the clean letter for a noisy one
(let ((net (competition)))
  (dotimes (k 40) (train-period net))
  (check (< (first (network-history net)) 0.001) t)
  (setf (network-noise net) 0.0)
  (let ((winners nil))
    (dolist (p (network-patterns net))
      (present-noisy net p)
      (run-step net)
      (check (network-status net) (string-append "Output: " (third p)))
      (push (position 1.0 (layer-act (second (network-layers net)))) winners))
    (check (length (remove-duplicates winners)) 5)))

;;; Back propagation: the 8-3-8 encoder learns
(let ((net (back-propagation)))
  (train-period net)
  (let ((first-error (first (network-history net))))
    (dotimes (k 40) (train-period net))
    (check (< (first (network-history net)) (* 0.5 first-error)) t))
  (check (network-period net) 410))

(report "plexi")
