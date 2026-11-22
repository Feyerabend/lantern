;;; -*- Mode: Lisp -*-
;;; plexi.lisp — Plexi: a graphics-based environment for neural networks.
;;;
;;; After Plexi, which Symbolics sold for the 3600 and the XL400 in 1989:
;;; a network is layers of cells joined by bundles of links; each layer has
;;; a transfer function, an update type and an end condition, and each
;;; bundle a connectivity pattern and a learning law. The editor draws the
;;; network and shows it working: activations as coloured cells, link weights
;;; as Hinton diagrams, and the error as a strip chart.
;;;
;;; Three networks of the time, each built with the functions below:
;;;
;;;   Hopfield Memory     one layer of 100 cells, every cell linked to every
;;;                       other; it stores the letters of PLEXI and, given a
;;;                       noisy one, settles on the letter (Hopfield, 1982).
;;;   Competition         the brochure's example: INPUT-IMAGE, COMPETE-LAYER
;;;                       and OUTPUT-LAYER. The competing cells learn the
;;;                       letters from noisy images, untaught; the output
;;;                       layer is taught what each should produce
;;;                       (counter-propagation, Hecht-Nielsen, 1987).
;;;   Back Propagation    the 8-3-8 encoder: eight patterns through three
;;;                       hidden cells, which must find a code for them
;;;                       (Rumelhart, Hinton and Williams, 1986).
;;;
;;; (plexi) opens the editor. Plexi is extensible, as the original was:
;;;
;;;   (deftransfer :name (x) ...)                a transfer function
;;;   (deflearning-law :name (net bundle) ...)   a learning law
;;;
;;; and the new ones are in the attribute menus at once. *PLEXI-NETWORK* is
;;; the network on show; (make-network ...), (add-layer ...) and
;;; (connect ...) build new ones.

;;; ---- transfer functions and learning laws --------------------------------------------

(defvar *transfer-functions* nil)        ; ((name . function) ...), in the order defined
(defvar *learning-laws* nil)

(defun %register (list name function)
  (let ((old (assoc name list)))
    (if old
        (progn (setf (cdr old) function) list)
        (append list (list (cons name function))))))

(defmacro deftransfer (name (x) &body body)
  `(progn (setq *transfer-functions* (%register *transfer-functions* ,name (lambda (,x) ,@body)))
          ,name))

(defmacro deflearning-law (name (net bundle) &body body)
  `(progn (setq *learning-laws* (%register *learning-laws* ,name (lambda (,net ,bundle) ,@body)))
          ,name))

(defun transfer-function (name)
  (or (cdr (assoc name *transfer-functions*)) (error "No such transfer function:" name)))

(defun sigmoid (x) (/ 1.0 (+ 1.0 (exp (- (max -30.0 (min 30.0 x)))))))

(deftransfer :linear (x) x)
(deftransfer :sigmoid (x) (sigmoid x))
(deftransfer :binary-step (x) (if (> x 0.0) 1.0 0.0))
(deftransfer :bipolar-step (x) (if (< x 0.0) -1.0 1.0))
(deftransfer :bipolar-sigmoid (x) (- (* 2.0 (sigmoid x)) 1.0))
(deftransfer :threshold-linear (x) (max 0.0 (min 1.0 x)))

(defconstant update-types '(:synchronous :random :competitive))

;;; ---- the network --------------------------------------------------------------------

(defclass layer ()
  ((name :initarg :name :accessor layer-name)
   (rows :initarg :rows :initform 1 :accessor layer-rows)
   (cols :initarg :cols :initform 1 :accessor layer-cols)
   (role :initarg :role :initform :hidden :accessor layer-role)     ; :input :hidden :output
   (transfer :initarg :transfer :initform :sigmoid :accessor layer-transfer)
   (update :initarg :update :initform :synchronous :accessor layer-update)
   (end :initarg :end :initform :once :accessor layer-end)          ; :once or :settle
   (bipolar :initarg :bipolar :initform nil :accessor layer-bipolar) ; values -1 .. 1, not 0 .. 1
   (act :initform nil :accessor layer-act)                         ; the activations
   (net :initform nil :accessor layer-net)                         ; the summed input
   (bias :initform nil :accessor layer-bias)
   (target :initform nil :accessor layer-target)                   ; what it should give (supervised)
   (delta :initform nil :accessor layer-delta)                     ; error terms (back propagation)
   (wins :initform nil :accessor layer-wins)))                     ; how often each cell wins (competition)

(defun layer-size (l) (* (layer-rows l) (layer-cols l)))

(defclass bundle ()
  ((from :initarg :from :accessor bundle-from)
   (to :initarg :to :accessor bundle-to)
   (pattern :initarg :pattern :initform :full :accessor bundle-pattern)  ; :full, :full-except-self
   (law :initarg :law :initform :none :accessor bundle-law)
   (weights :initform nil :accessor bundle-weights)    ; one vector per cell of TO, over the cells of FROM
   (changes :initform nil :accessor bundle-changes)))  ; the last change of each weight (momentum)

(defclass network ()
  ((name :initarg :name :accessor network-name)
   (layers :initform nil :accessor network-layers)       ; input first
   (bundles :initform nil :accessor network-bundles)
   (patterns :initform nil :accessor network-patterns)   ; ((input target name) ...)
   (period :initform 0 :accessor network-period)
   (history :initform nil :accessor network-history)     ; the error of each period, newest first
   (measure :initarg :measure :initform "Error" :accessor network-measure)
   (rate :initarg :rate :initform 0.3 :accessor network-rate)
   (momentum :initarg :momentum :initform 0.0 :accessor network-momentum)
   (noise :initarg :noise :initform 0.2 :accessor network-noise)
   (settled :initform nil :accessor network-settled)
   (status :initform "" :accessor network-status)
   (trainer :initarg :trainer :initform nil :accessor network-trainer)   ; one period of training
   (runner :initarg :runner :initform nil :accessor network-runner)))    ; present the input, once

(defun make-network (name &rest options)
  (apply #'make-instance 'network :name name options))

(defun add-layer (net name rows cols &rest options)
  (let ((l (apply #'make-instance 'layer :name name :rows rows :cols cols options))
        (n (* rows cols)))
    (setf (layer-act l) (make-array n :initial-element 0.0))
    (setf (layer-net l) (make-array n :initial-element 0.0))
    (setf (layer-bias l) (make-array n :initial-element 0.0))
    (setf (layer-delta l) (make-array n :initial-element 0.0))
    (setf (network-layers net) (append (network-layers net) (list l)))
    l))

(defun find-layer (net name)
  (if (typep name 'layer)
      name
      (or (find-if (lambda (l) (equal (layer-name l) name)) (network-layers net))
          (error "No such layer:" name))))

;; small random weights, -SCALE .. SCALE
(defun random-weight (scale) (- (random (* 2.0 scale)) scale))

(defun connect (net from to &rest options)
  (let* ((from (find-layer net from)) (to (find-layer net to))
         (b (apply #'make-instance 'bundle :from from :to to options))
         (nf (layer-size from)) (nt (layer-size to))
         (w (make-array nt)) (c (make-array nt)))
    (dotimes (j nt)
      (setf (aref w j) (make-array nf :initial-element 0.0))
      (setf (aref c j) (make-array nf :initial-element 0.0)))
    (setf (bundle-weights b) w (bundle-changes b) c)
    (setf (network-bundles net) (append (network-bundles net) (list b)))
    b))

(defun bundles-into (net layer)
  (remove-if-not (lambda (b) (eq (bundle-to b) layer)) (network-bundles net)))

(defun link-p (b i j)
  (not (and (eq (bundle-pattern b) :full-except-self) (= i j))))

(defun randomize-weights (b scale)
  (let ((w (bundle-weights b)))
    (dotimes (j (length w))
      (let ((row (aref w j)))
        (dotimes (i (length row))
          (setf (aref row i) (if (link-p b i j) (random-weight scale) 0.0)))))))

;;; ---- running it ---------------------------------------------------------------------

;; the summed input of cell J of LAYER from all its bundles
(defun cell-input (net layer j)
  (let ((s (aref (layer-bias layer) j)))
    (dolist (b (network-bundles net))
      (when (eq (bundle-to b) layer)
        (let ((w (aref (bundle-weights b) j)) (x (layer-act (bundle-from b))))
          (dotimes (i (length x))
            (setq s (+ s (* (aref w i) (aref x i))))))))
    s))

;; a competing cell's input is its nearness to the input: minus the squared
;; distance between its weights and the activations (Kohonen)
(defun cell-nearness (net layer j)
  (let ((s 0.0))
    (dolist (b (network-bundles net))
      (when (eq (bundle-to b) layer)
        (let ((w (aref (bundle-weights b) j)) (x (layer-act (bundle-from b))))
          (dotimes (i (length x))
            (let ((d (- (aref x i) (aref w i))))
              (setq s (- s (* d d))))))))
    s))

;; how strongly a competing cell's conscience weighs; only while learning
(defvar *conscience* 12.0)
(defvar *learning* nil)

;; Update LAYER by its update type. => the number of cells that changed
(defun update-layer (net layer &optional (count nil))
  (let* ((act (layer-act layer)) (nets (layer-net layer)) (n (length act))
         (f (transfer-function (layer-transfer layer))) (changed 0))
    (case (layer-update layer)
      (:random                                            ; one cell at a time, in no order
       (dotimes (k (or count n))
         (let* ((j (random n)) (s (cell-input net layer j)) (old (aref act j))
                (new (if (= s 0.0) old (funcall f s))))     ; no input: it stays
           (setf (aref nets j) s)
           (unless (= new old) (setf (aref act j) new) (incf changed)))))
      (:competitive                                       ; the nearest cell wins, alone
       ;; with a conscience (DeSieno, 1988): a cell that wins more than its
       ;; share is handicapped, so that one cell cannot take every pattern
       (unless (layer-wins layer)
         (setf (layer-wins layer) (make-array n :initial-element (/ 1.0 n))))
       (let ((best 0) (best-s nil) (wins (layer-wins layer)))
         (dotimes (j n)
           (let ((s (+ (cell-nearness net layer j) (* *conscience* (- (/ 1.0 n) (aref wins j))))))
             (setf (aref nets j) s)
             (when (or (null best-s) (> s best-s)) (setq best j best-s s))))
         (dotimes (j n)
           (let ((new (if (= j best) 1.0 0.0)))
             (when *learning* (setf (aref wins j) (+ (aref wins j) (* 0.05 (- new (aref wins j))))))
             (unless (= new (aref act j)) (setf (aref act j) new) (incf changed))))))
      (t                                                  ; all at once
       (let ((new (make-array n)))
         (dotimes (j n) (setf (aref nets j) (cell-input net layer j)))
         (dotimes (j n) (setf (aref new j) (funcall f (aref nets j))))
         (dotimes (j n)
           (unless (= (aref new j) (aref act j)) (setf (aref act j) (aref new j)) (incf changed))))))
    changed))

(defun input-layer (net) (find-if (lambda (l) (eq (layer-role l) :input)) (network-layers net)))
(defun output-layer (net) (find-if (lambda (l) (eq (layer-role l) :output)) (network-layers net)))

(defun set-activations (layer values)
  (let ((act (layer-act layer)))
    (dotimes (i (length act)) (setf (aref act i) (float (elt values i))))))

;; present INPUT and let every layer after the input compute, in order
(defun propagate (net)
  (dolist (l (network-layers net))
    (unless (eq (layer-role l) :input)
      (update-layer net l))))

(defun output-error (layer)
  (let ((e 0.0) (act (layer-act layer)) (target (layer-target layer)))
    (when target
      (dotimes (i (length act))
        (let ((d (- (aref target i) (aref act i))))
          (setq e (+ e (* d d))))))
    e))

(defun record-period (net value)
  (setf (network-period net) (1+ (network-period net)))
  (push value (network-history net))
  (when (> (length (network-history net)) 400)
    (setf (network-history net) (subseq (network-history net) 0 300)))
  value)

;; a copy of the bits of PATTERN with about FRACTION of them flipped (LOW, HIGH: the two values)
(defun noisy (pattern fraction low high)
  (let ((v (make-array (length pattern))))
    (dotimes (i (length pattern))
      (let ((x (aref pattern i)))
        (setf (aref v i) (if (< (random 1000) (round (* fraction 1000))) (if (= x high) low high) x))))
    v))

;;; ---- the learning laws -------------------------------------------------------------

;; Hebb's rule for a Hopfield memory: links between cells that agree in the
;; stored patterns are strengthened, between cells that differ weakened.
(deflearning-law :hebbian (net b)
  (let* ((w (bundle-weights b)) (n (length w)) (scale (/ 1.0 n)))
    (dotimes (j n) (fill (aref w j) 0.0))
    (dolist (p (network-patterns net))
      (let ((x (first p)))
        (dotimes (j n)
          (let ((row (aref w j)) (xj (aref x j)))
            (dotimes (i n)
              (when (link-p b i j)
                (setf (aref row i) (+ (aref row i) (* scale xj (aref x i))))))))))))

;; The projection rule (Personnaz, Guyon and Dreyfus, 1986): like Hebb's, but
;; corrected for how much the patterns overlap, so that every stored pattern
;; is stable even when they are as alike as P, E and L.
;; W = X (X'X)^-1 X', the diagonal left out.
(deflearning-law :projection (net b)
  (let* ((ps (mapcar #'first (network-patterns net))) (m (length ps))
         (w (bundle-weights b)) (n (length w))
         (c (make-array m)))
    (dotimes (a m)                                      ; C = X'X, then its inverse
      (setf (aref c a) (make-array m :initial-element 0.0))
      (dotimes (bb m)
        (let ((xa (nth a ps)) (xb (nth bb ps)) (s 0.0))
          (dotimes (i n) (setq s (+ s (* (aref xa i) (aref xb i)))))
          (setf (aref (aref c a) bb) s))))
    (let ((ci (invert-matrix c)))
      (dotimes (j n)
        (let ((row (aref w j)))
          (dotimes (i n)
            (let ((s 0.0))
              (when (link-p b i j)
                (dotimes (a m)
                  (let ((xai (aref (nth a ps) i)))
                    (dotimes (bb m)
                      (setq s (+ s (* xai (aref (aref ci a) bb) (aref (nth bb ps) j))))))))
              (setf (aref row i) s))))))))

;; Gauss-Jordan, on a vector of row vectors
(defun invert-matrix (m)
  (let* ((n (length m)) (a (make-array n)))
    (dotimes (i n)
      (setf (aref a i) (make-array (* 2 n) :initial-element 0.0))
      (dotimes (j n) (setf (aref (aref a i) j) (float (aref (aref m i) j))))
      (setf (aref (aref a i) (+ n i)) 1.0))
    (dotimes (col n)
      (let ((pivot col))
        (do ((r (1+ col) (1+ r))) ((>= r n))
          (when (> (abs (aref (aref a r) col)) (abs (aref (aref a pivot) col))) (setq pivot r)))
        (let ((tmp (aref a col))) (setf (aref a col) (aref a pivot)) (setf (aref a pivot) tmp))
        (let* ((row (aref a col)) (p (aref row col)))
          (when (< (abs p) 1e-12) (error "The patterns are not independent"))
          (dotimes (j (* 2 n)) (setf (aref row j) (/ (aref row j) p)))
          (dotimes (r n)
            (unless (= r col)
              (let* ((other (aref a r)) (f (aref other col)))
                (dotimes (j (* 2 n))
                  (setf (aref other j) (- (aref other j) (* f (aref row j)))))))))))
    (let ((inv (make-array n)))
      (dotimes (i n) (setf (aref inv i) (subseq (aref a i) n (* 2 n))))
      inv)))

;; Competitive learning (Kohonen): the winning cell moves its weights
;; toward the input. No teacher.
(deflearning-law :competitive (net b)
  (let* ((to (bundle-to b)) (act (layer-act to)) (x (layer-act (bundle-from b)))
         (rate (network-rate net)))
    (dotimes (j (length act))
      (when (> (aref act j) 0.5)
        (let ((w (aref (bundle-weights b) j)))
          (dotimes (i (length x))
            (setf (aref w i) (+ (aref w i) (* rate (- (aref x i) (aref w i)))))))))))

;; Grossberg's outstar: the links from the winning cell move toward what the
;; layer should produce. Taught.
(deflearning-law :outstar (net b)
  (let* ((from (layer-act (bundle-from b))) (to (bundle-to b)) (target (layer-target to))
         (w (bundle-weights b)) (rate (network-rate net)))
    (when target
      (dotimes (i (length from))
        (when (> (aref from i) 0.5)
          (dotimes (j (length w))
            (let ((row (aref w j)))
              (setf (aref row i) (+ (aref row i) (* rate (- (aref target j) (aref row i))))))))))))

;; Back propagation of error, the generalized delta rule, with momentum. The
;; deltas are computed by BACKPROPAGATE before the bundles learn.
(deflearning-law :back-propagation (net b)
  (let* ((from (layer-act (bundle-from b))) (to (bundle-to b)) (delta (layer-delta to))
         (w (bundle-weights b)) (c (bundle-changes b))
         (rate (network-rate net)) (momentum (network-momentum net)))
    (dotimes (j (length w))
      (let ((row (aref w j)) (crow (aref c j)) (dj (* rate (aref delta j))))
        (dotimes (i (length from))
          (let ((change (+ (* dj (aref from i)) (* momentum (aref crow i)))))
            (setf (aref crow i) change)
            (setf (aref row i) (+ (aref row i) change))))))))

(deflearning-law :none (net b) nil)

(defun learn (net)
  (dolist (b (network-bundles net))
    (funcall (or (cdr (assoc (bundle-law b) *learning-laws*)) (error "No such learning law:" (bundle-law b)))
             net b)))

;; the deltas of every layer, output first (sigmoid layers)
(defun backpropagate (net)
  (let ((layers (reverse (network-layers net))))
    (dolist (l layers)
      (unless (eq (layer-role l) :input)
        (let ((act (layer-act l)) (delta (layer-delta l)))
          (dotimes (j (length act))
            (let ((err (if (eq (layer-role l) :output)
                           (- (aref (layer-target l) j) (aref act j))
                           (let ((s 0.0))
                             (dolist (b (network-bundles net))
                               (when (eq (bundle-from b) l)
                                 (let ((up (layer-delta (bundle-to b))) (w (bundle-weights b)))
                                   (dotimes (k (length up))
                                     (setq s (+ s (* (aref up k) (aref (aref w k) j))))))))
                             s)))
                  (a (aref act j)))
              (setf (aref delta j) (* err a (- 1.0 a))))))))
    ;; the biases learn like links from a cell that is always on
    (dolist (l (network-layers net))
      (unless (eq (layer-role l) :input)
        (let ((bias (layer-bias l)) (delta (layer-delta l)) (rate (network-rate net)))
          (dotimes (j (length bias))
            (setf (aref bias j) (+ (aref bias j) (* rate (aref delta j))))))))))

;;; ---- the patterns -------------------------------------------------------------------

;; the letters of PLEXI, 10 by 10
(defconstant plexi-letters
  '(("P" "#######..."
         "########.."
         "##....###."
         "##.....##."
         "##....###."
         "########.."
         "#######..."
         "##........"
         "##........"
         "##........")
    ("L" "##........"
         "##........"
         "##........"
         "##........"
         "##........"
         "##........"
         "##........"
         "##........"
         "#########."
         "#########.")
    ("E" "#########."
         "#########."
         "##........"
         "##........"
         "#######..."
         "#######..."
         "##........"
         "##........"
         "#########."
         "#########.")
    ("X" "##.....##."
         "###...###."
         ".###.###.."
         "..#####..."
         "...###...."
         "..#####..."
         ".###.###.."
         "###...###."
         "##.....##."
         "#.......#.")
    ("I" ".######..."
         ".######..."
         "...##....."
         "...##....."
         "...##....."
         "...##....."
         "...##....."
         "...##....."
         ".######..."
         ".######...")))

;; a letter as a vector of LOW and HIGH
(defun letter-pattern (name low high)
  (let* ((rows (cdr (assoc name plexi-letters :test #'equal)))
         (v (make-array 100)) (k 0))
    (dolist (r rows)
      (dotimes (c 10)
        (setf (aref v k) (if (eq (aref r c) #\#) high low))
        (setq k (1+ k))))
    v))

;;; ---- the three networks ----------------------------------------------------------------

(defun hopfield-energy (net)
  (let* ((l (first (network-layers net))) (b (first (network-bundles net)))
         (x (layer-act l)) (w (bundle-weights b)) (e 0.0))
    (dotimes (j (length x))
      (let ((row (aref w j)) (xj (aref x j)))
        (dotimes (i (length x))
          (setq e (- e (* 0.5 xj (aref row i) (aref x i)))))))
    e))

(defun hopfield-memory ()
  (let ((net (make-network "Hopfield Memory" :measure "Energy" :noise 0.25)))
    (add-layer net "MEMORY" 10 10 :role :input :transfer :bipolar-step :update :random
                                  :end :settle :bipolar t)
    (connect net "MEMORY" "MEMORY" :pattern :full-except-self :law :projection)
    (setf (network-patterns net)
          (mapcar (lambda (l) (list (letter-pattern (first l) -1.0 1.0) nil (first l))) plexi-letters))
    (learn net)                                       ; Hebbian: stored in one step
    (setf (network-trainer net)
          (lambda (net)                               ; stored at once, by the bundle's law
            (learn net)
            (present-noisy net)
            (setf (network-status net)
                  (string-append "The " (string-downcase (symbol-name (bundle-law (first (network-bundles net)))))
                                 " rule has stored the 5 letters. Run to recall a noisy one."))
            t))
    (setf (network-runner net)
          (lambda (net)                               ; a tenth of a sweep, in no particular order
            (let* ((l (first (network-layers net)))
                   (changed (update-layer net l 10)))
              (if (= changed 0)
                  (setf (network-settled net) (+ (or (network-settled net) 0) 10))
                  (setf (network-settled net) 0))
              (setf (network-status net) (string-append "Recalling, one cell at a time, in no order: "
                                                        (recognized net)))
              (record-period net (hopfield-energy net))
              (when (>= (network-settled net) 200)    ; two sweeps with no change: settled
                (setf (network-status net) (string-append "Settled on " (recognized net)))
                t))))
    (present-noisy net)
    net))

;; the stored pattern nearest the memory's state
(defun recognized (net)
  (let ((x (layer-act (or (output-layer net) (first (network-layers net))))) (best nil) (best-d nil))
    (dolist (p (network-patterns net))
      (let ((target (or (second p) (first p))) (d 0))
        (dotimes (i (length x))
          (when (> (abs (- (aref x i) (aref target i))) 0.5) (incf d)))
        (when (or (null best-d) (< d best-d)) (setq best (third p) best-d d))))
    (cond ((= best-d 0) best)
          ((= best-d 1) (string-append "nearest " best ", 1 cell differs"))
          (t (string-append "nearest " best ", " (princ-to-string best-d) " cells differ")))))

;; put a noisy copy of a random pattern in the input layer
(defun present-noisy (net &optional pattern)
  (let* ((p (or pattern (nth (random (length (network-patterns net))) (network-patterns net))))
         (in (input-layer net)) (bipolar (layer-bipolar in)))
    (set-activations in (noisy (first p) (network-noise net) (if bipolar -1.0 0.0) 1.0))
    (let ((out (output-layer net)))
      (when (and out (second p)) (setf (layer-target out) (second p))))
    (setf (network-settled net) nil)
    (setf (network-status net)
          (string-append "Input: " (third p) ", with " (princ-to-string (round (* 100 (network-noise net))))
                         "% noise"))
    p))

(defun competition ()
  (let ((net (make-network "Competition" :rate 0.15 :noise 0.12)))
    (add-layer net "INPUT-IMAGE" 10 10 :role :input :transfer :linear)
    (add-layer net "COMPETE-LAYER" 1 6 :update :competitive :transfer :binary-step)
    (add-layer net "OUTPUT-LAYER" 10 10 :role :output :transfer :linear)
    (connect net "INPUT-IMAGE" "COMPETE-LAYER" :law :competitive)
    (connect net "COMPETE-LAYER" "OUTPUT-LAYER" :law :outstar)
    (setf (network-patterns net)
          (mapcar (lambda (l) (let ((p (letter-pattern (first l) 0.0 1.0))) (list p p (first l))))
                  plexi-letters))
    ;; each competing cell starts as a noisy example, so that none is left out
    (let ((w (bundle-weights (first (network-bundles net)))) (ps (network-patterns net)))
      (dotimes (j (length w))
        (setf (aref w j) (noisy (first (nth (mod j (length ps)) ps)) 0.15 0.0 1.0))))
    (setf (network-trainer net)
          (lambda (net)                               ; each letter once, freshly noisy
            (let ((e 0.0))
              (dolist (p (network-patterns net))
                (present-noisy net p)
                (let ((*learning* t)) (propagate net))
                (learn net)
                (propagate net)
                (setq e (+ e (output-error (output-layer net)))))
              (record-period net (/ e (* 100 (length (network-patterns net)))))
              (setf (network-status net) (string-append "Training, period " (princ-to-string (network-period net))
                                                        ": " (recognized net)))
              (< e 0.0005))))
    (setf (network-runner net)
          (lambda (net)
            (propagate net)
            (setf (network-status net) (string-append "Output: " (recognized net)))
            t))
    (present-noisy net)
    (propagate net)
    (setf (network-status net) "Train: the competing cells learn the letters from noisy examples.")
    net))

(defun back-propagation ()
  (let ((net (make-network "Back Propagation" :rate 0.6 :momentum 0.8 :noise 0.0)))
    (add-layer net "INPUT" 1 8 :role :input :transfer :linear)
    (add-layer net "HIDDEN" 1 3 :transfer :sigmoid)
    (add-layer net "OUTPUT" 1 8 :role :output :transfer :sigmoid)
    (randomize-weights (connect net "INPUT" "HIDDEN" :law :back-propagation) 0.5)
    (randomize-weights (connect net "HIDDEN" "OUTPUT" :law :back-propagation) 0.5)
    (setf (network-patterns net)
          (let ((ps nil))
            (dotimes (k 8)
              (let ((v (make-array 8 :initial-element 0.0)))
                (setf (aref v k) 1.0)
                (push (list v v (princ-to-string (1+ k))) ps)))
            (nreverse ps)))
    (setf (network-trainer net)
          (lambda (net)                               ; ten epochs: every pattern, ten times
            (dotimes (k 10)
              (let ((e 0.0))
                (dolist (p (network-patterns net))
                  (set-activations (input-layer net) (first p))
                  (setf (layer-target (output-layer net)) (second p))
                  (propagate net)
                  (setq e (+ e (output-error (output-layer net))))
                  (backpropagate net)
                  (learn net))
                (record-period net e)))
            (setf (network-status net) (string-append "Epoch " (princ-to-string (network-period net))
                                                      ", the hidden code for the last pattern: " (hidden-code net)))
            (< (first (network-history net)) 0.02)))
    (setf (network-runner net)
          (lambda (net)
            (propagate net)
            (setf (network-status net) (string-append "Hidden code " (hidden-code net) ", output: " (recognized net)))
            t))
    (present-noisy net)
    (propagate net)
    (setf (network-status net) "Train: the 3 hidden cells must find a code for the 8 patterns.")
    net))

;; the hidden layer's activations as bits, 0 or 1
(defun hidden-code (net)
  (let ((h (layer-act (second (network-layers net)))) (s ""))
    (dotimes (i (length h)) (setq s (string-append s (if (> (aref h i) 0.5) "1" "0"))))
    s))

(defconstant plexi-paradigms
  '(("Hopfield Memory" . hopfield-memory)
    ("Competition" . competition)
    ("Back Propagation" . back-propagation)))

(defvar *plexi-network* nil)

;; one period of training; => T when there is no more to do
(defun train-period (net) (funcall (network-trainer net) net))
;; present the input and run; => T when the network has finished
(defun run-step (net) (funcall (network-runner net) net))

;;; ---- the editor ------------------------------------------------------------------------
;;;
;;; One window: a row of commands; the Network Editor, which draws the layers
;;; as grids of cells (input at the bottom) joined by their bundles; the
;;; attributes of the selected layer; a Hinton diagram of the links into it;
;;; and a strip chart of the error, or of the energy of a Hopfield memory.
;;; The window's process does all the drawing, into a bitmap of its own,
;;; and the computing; clicks come to it as input.

(defclass plexi-window (label-mixin border-mixin window)
  ((networks :initform nil :accessor plexi-networks)    ; (("name" . network) ...)
   (mode :initform nil :accessor plexi-mode)            ; NIL, :TRAIN or :RUN
   (selected :initform nil :accessor plexi-selected)    ; the layer shown in the attributes
   (unit :initform 0 :accessor plexi-unit)              ; the cell whose links are shown
   (hits :initform nil :accessor plexi-hits)            ; ((x y width height . action) ...)
   (back :initform nil :accessor plexi-back)))          ; drawn here, then copied in at once

(defun plexi-net (w) (cdr (assoc (network-name *plexi-network*) (plexi-networks w))))

;; colour on the colour screen, MONO on the black-and-white one
(defun hue (name mono) (if (color-screen-p) (ink name) mono))

;; an activation's colour: on the ramp in colour; in black and white, darker
;; the more active
(defun activation-ink (layer v)
  (let ((x (if (layer-bipolar layer) (* 0.5 (+ v 1.0)) (max 0.0 (min 1.0 v)))))
    (if (color-screen-p)
        (ramp-ink *activation-ramp* x)
        (round (* 255 x)))))

(defun plexi-hit (w x y width height &rest action)
  (push (list* x y width height action) (plexi-hits w)))

;; a framed panel with a title tab
(defun draw-panel (b x y width height title)
  (draw-frame x y width height 1 alu-seta 255 b)
  (let ((tw (+ 8 (string-width title *font-bold*))))
    (draw-rectangle x y tw 12 alu-seta (hue :magenta 255) b)
    (draw-string title (+ x 4) (+ y 1) *font-bold* alu-andca b)))

(defun draw-button (w b x y label on action)
  (let ((width (+ 10 (string-width label *font-variable*))))
    (draw-rectangle x y width 14 alu-seta (if on (hue :orange 255) (hue :pale-yellow 0)) b)
    (draw-frame x y width 14 1 alu-seta 255 b)
    (draw-string label (+ x 5) (+ y 2) *font-variable* (if (and on (not (color-screen-p))) alu-andca alu-ior) b)
    (apply #'plexi-hit w x y width 14 action)
    (+ x width 4)))

(defun fmt (x &optional (digits 3))
  (cond ((integerp x) (princ-to-string x))
        ((< (abs x) 0.0005) "0.0")
        ((> (abs x) 9999) (format nil "~,1F" x))
        (t (format nil (if (= digits 2) "~,2F" "~,3F") x))))

;;; the layers

;; Where each layer goes in a pane at X, Y, WIDTH by HEIGHT: ((layer x y cell) ...)
(defun layout-layers (net x y width height)
  (let* ((layers (network-layers net))
         (rows (reduce #'+ (mapcar #'layer-rows layers)))
         (cols (reduce #'max (mapcar #'layer-cols layers)))
         (overhead (* (length layers) 34))
         (cell (max 3 (min 24 (%quotient (- height overhead 8) rows) (%quotient (- width 70) cols))))
         (total (+ (* rows cell) overhead))
         (bottom (- (+ y height) (max 6 (%quotient (- height total) 2))))
         (places nil))
    (dolist (l layers)
      (let* ((h (* (layer-rows l) cell)) (wd (* (layer-cols l) cell))
             (top (- bottom h)))
        (push (list l (+ x (%quotient (- width wd) 2)) top cell) places)
        (setq bottom (- top 34))))
    (nreverse places)))

(defun draw-arrowhead (b x y up ink)
  (dotimes (k 4)
    (draw-rectangle (- x k) (if up (+ y k) (- y k)) (1+ (* 2 k)) 1 alu-seta ink b)))

(defun draw-bundle (b from to ink)
  (let* ((fl (first from)) (fx (second from)) (fy (third from)) (fc (fourth from))
         (tl (first to)) (tx (second to)) (ty (third to)) (tc (fourth to))
         (fw (* (layer-cols fl) fc)) (tw (* (layer-cols tl) tc)))
    (if (eq fl tl)
        ;; a layer linked to itself: a loop on its right
        (let* ((h (* (layer-rows fl) fc)) (rx (+ fx fw 3)) (y1 (+ fy (%quotient h 4))) (y2 (- (+ fy h) (%quotient h 4))))
          (draw-line rx y2 (+ rx 16) y2 1 alu-seta ink b)
          (draw-line (+ rx 16) y2 (+ rx 16) y1 1 alu-seta ink b)
          (draw-line (+ rx 16) y1 rx y1 1 alu-seta ink b)
          (dotimes (k 4) (draw-rectangle (+ rx k) (- y1 k) 1 (1+ (* 2 k)) alu-seta ink b)))
        ;; a bundle: links fanning in from the top of one layer to the bottom of the next
        (let ((x1 (+ tx (%quotient tw 2))) (y1 (+ ty (* (layer-rows tl) tc) 3)) (y0 (- fy 2)))
          (dolist (f '(1 3 5 7))
            (draw-line (+ fx (%quotient (* f fw) 8)) y0 x1 (+ y1 3) 1 alu-seta ink b))
          (draw-arrowhead b x1 y1 t ink)))))

(defun draw-layer (w b place)
  (let* ((l (first place)) (x (second place)) (y (third place)) (c (fourth place))
         (act (layer-act l)) (cols (layer-cols l)) (frame (>= c 7))
         (selected (eq l (plexi-selected w))))
    ;; its name, on a tab
    (let ((tw (+ 8 (string-width (layer-name l) *font-bold*))))
      (draw-rectangle x (- y 13) tw 12 alu-seta (if selected (hue :purple 255) (hue :yellow 0)) b)
      (unless selected (draw-frame x (- y 13) tw 12 1 alu-seta 255 b))
      (draw-string (layer-name l) (+ x 4) (- y 12) *font-bold* (if selected alu-andca alu-ior) b)
      (plexi-hit w x (- y 13) tw 12 :select l))
    (draw-rectangle (- x 1) (- y 1) (+ 2 (* cols c)) (+ 2 (* (layer-rows l) c)) alu-seta
                    (hue :desktop-blue 255) b)
    (dotimes (i (length act))
      (let ((cx (+ x (* c (rem i cols)))) (cy (+ y (* c (%quotient i cols)))))
        (cond (frame
               (draw-rectangle (+ cx 1) (+ cy 1) (- c 2) (- c 2) alu-seta (hue :magenta 102) b)
               (draw-rectangle (+ cx 2) (+ cy 2) (- c 4) (- c 4) alu-seta (activation-ink l (aref act i)) b))
              (t (draw-rectangle cx cy (max 1 (- c 1)) (max 1 (- c 1)) alu-seta (activation-ink l (aref act i)) b)))))
    (when (and selected (< (plexi-unit w) (length act)))
      (let ((i (plexi-unit w)))
        (draw-frame (+ x (* c (rem i cols)) -1) (+ y (* c (%quotient i cols)) -1) (+ c 2) (+ c 2) 1 alu-seta
                    (hue :red 255) b)))
    (plexi-hit w x y (* cols c) (* (layer-rows l) c) :cell l c)))

(defun draw-network (w b net x y width height)
  (draw-panel b x y width height (string-append "Network Editor: " (network-name net)))
  (let ((places (layout-layers net (+ x 4) (+ y 16) (- width 8) (- height 20))))
    (dolist (bundle (network-bundles net))
      (draw-bundle b (assoc (bundle-from bundle) places) (assoc (bundle-to bundle) places)
                   (hue :deep-blue 255)))
    (dolist (p places) (draw-layer w b p))))

;;; the attributes

(defun cycle (item list)
  (let ((tail (cdr (member item list :test #'equal))))
    (if tail (car tail) (car list))))

(defun layer-law (net l)
  (let ((b (first (bundles-into net l)))) (and b (bundle-law b))))

(defun attribute-name (x) (if (symbolp x) (string-downcase (symbol-name x)) x))

(defun draw-attributes (w b net x y width height)
  (draw-panel b x y width height "Layer Attributes")
  (let* ((l (plexi-selected w)) (ty (+ y 16)) (lx (+ x 6)) (vx (+ x 6 (* 6 18))))
    (flet ((line (label value &optional action)
             (when (< ty (+ y height -10))
               (draw-string label lx ty *font-variable* alu-ior b)
               (let ((end (draw-string value vx ty (if action *font-bold* *font-fixed*) alu-ior b)))
                 (when action
                   (draw-rectangle vx (+ ty 10) (- end vx) 1 alu-seta (hue :magenta 255) b)
                   (plexi-hit w vx ty (- end vx) 11 :attribute action)))
               (setq ty (+ ty 11)))))
      (line "Name" (layer-name l))
      (line "Size" (string-append (princ-to-string (layer-rows l)) " x " (princ-to-string (layer-cols l))
                                  " = " (princ-to-string (layer-size l))))
      (unless (eq (layer-role l) :input)
        (line "Transfer function" (attribute-name (layer-transfer l)) :transfer))
      (when (or (not (eq (layer-role l) :input)) (bundles-into net l))
        (line "Update type" (attribute-name (layer-update l)) :update))
      (line "End condition" (attribute-name (layer-end l)))
      (when (layer-law net l)
        (line "Learning law" (attribute-name (layer-law net l)) :law))
      (line "Learning rate" (fmt (network-rate net) 2) :rate)
      (line "Input noise" (string-append (princ-to-string (round (* 100 (network-noise net)))) "%") :noise))))

(defun change-attribute (w net what)
  (let ((l (plexi-selected w)))
    (case what
      (:transfer (setf (layer-transfer l) (cycle (layer-transfer l) (mapcar #'car *transfer-functions*))))
      (:update (setf (layer-update l) (cycle (layer-update l) update-types)))
      (:law (let ((bd (first (bundles-into net l))))
              (setf (bundle-law bd) (cycle (bundle-law bd) (mapcar #'car *learning-laws*)))))
      (:rate (setf (network-rate net) (cycle (network-rate net) '(0.05 0.1 0.15 0.3 0.6 1.0))))
      (:noise (setf (network-noise net) (cycle (network-noise net) '(0.0 0.05 0.12 0.2 0.25 0.35 0.5)))))
    (setf (network-status net) "Changed. Train again to see what it does.")))

;;; link weights, as a Hinton diagram: a square for each link, its area the
;;; size of the weight; light for positive weights, dark for negative

;; Which links to show for the selected layer: (bundle kind title)
(defun shown-links (w net)
  (let* ((l (plexi-selected w))
         (bd (or (first (bundles-into net l))
                 (find-if (lambda (x) (eq (bundle-from x) l)) (network-bundles net)))))
    (when bd
      (let ((nf (layer-size (bundle-from bd))) (nt (layer-size (bundle-to bd))))
        (cond ((and (> nf 16) (> nt 16)) (list bd :unit (string-append "Links into cell " (princ-to-string (plexi-unit w)))))
              ((> nt 16) (list bd :outstar (string-append "Links from " (layer-name (bundle-from bd)))))
              ((= 1 (layer-rows (bundle-from bd))) (list bd :matrix (string-append "Links " (layer-name (bundle-from bd))
                                                                                    " to " (layer-name (bundle-to bd)))))
              (t (list bd :tiles (string-append "Links into " (layer-name (bundle-to bd))))))))))

;; the tiles of the diagram: ((rows cols weight-function) ...)
(defun hinton-tiles (w shown)
  (let* ((bd (first shown)) (wts (bundle-weights bd))
         (from (bundle-from bd)) (to (bundle-to bd)))
    (case (second shown)
      (:unit (let ((row (aref wts (min (plexi-unit w) (1- (length wts))))))
               (list (list (layer-rows from) (layer-cols from) (lambda (i) (aref row i))))))
      (:outstar (let ((tiles nil))
                  (dotimes (i (layer-size from))
                    (let ((i i))
                      (push (list (layer-rows to) (layer-cols to) (lambda (j) (aref (aref wts j) i))) tiles)))
                  (nreverse tiles)))
      (:matrix (list (list (layer-size to) (layer-size from)
                           (lambda (k) (aref (aref wts (%quotient k (layer-size from))) (rem k (layer-size from)))))))
      (t (let ((tiles nil))
           (dotimes (j (layer-size to))
             (let ((row (aref wts j)))
               (push (list (layer-rows from) (layer-cols from) (lambda (i) (aref row i))) tiles)))
           (nreverse tiles))))))

(defun draw-hinton (w b net x y width height)
  (let ((shown (shown-links w net)))
    (draw-panel b x y width height (if shown (third shown) "Link Weights"))
    (when shown
      (let* ((tiles (hinton-tiles w shown))
             (n (length tiles))
             (trows (first (first tiles))) (tcols (second (first tiles)))
             (aw (- width 12)) (ah (- height 22))
             ;; the largest cell that fits the tiles side by side, wrapping into lines
             (cell (let ((c 16))
                     (while (and (> c 2)
                                 (let* ((per (max 1 (%quotient (+ aw 4) (+ (* tcols c) 4))))
                                        (lines (%quotient (+ n per -1) per)))
                                   (> (- (* lines (+ (* trows c) 4)) 4) ah)))
                       (setq c (1- c)))
                     c))
             (per (max 1 (%quotient (+ aw 4) (+ (* tcols cell) 4))))
             (big 0.0) (bg (hue :light-grey 68)) (pos (hue :orange 0)) (neg (hue :deep-blue 255)))
        (dolist (tile tiles)
          (dotimes (i (* trows tcols)) (setq big (max big (abs (funcall (third tile) i))))))
        (let ((k 0))
          (dolist (tile tiles)
            (let ((tx (+ x 6 (* (rem k per) (+ (* tcols cell) 4))))
                  (ty (+ y 17 (* (%quotient k per) (+ (* trows cell) 4)))))
              (when (<= (+ ty (* trows cell)) (+ y height -2))
                (draw-rectangle tx ty (* tcols cell) (* trows cell) alu-seta bg b)
                (dotimes (i (* trows tcols))
                  (let* ((v (funcall (third tile) i))
                         (side (if (> big 0.0) (round (* (- cell 1) (sqrt (/ (abs v) big)))) 0)))
                    (when (> side 0)
                      (draw-rectangle (+ tx (* cell (rem i tcols)) (%quotient (- cell side) 2))
                                      (+ ty (* cell (%quotient i tcols)) (%quotient (- cell side) 2))
                                      side side alu-seta (if (> v 0.0) pos neg) b))))))
            (setq k (1+ k))))
        (draw-string (string-append "largest " (fmt big)) (- (+ x width) 6 (string-width (string-append "largest " (fmt big)) *font-variable*))
                     (+ y 2) *font-variable* alu-ior b)))))

;;; the strip chart

(defun draw-strip-chart (b net x y width height)
  (draw-panel b x y width height (network-measure net))
  (let* ((n (min (length (network-history net)) (- width 12)))
         (values (reverse (subseq (network-history net) 0 n)))
         (lo (if values (reduce #'min values) 0.0)) (hi (if values (reduce #'max values) 0.0))
         (cx (+ x 6)) (cy (+ y 16)) (ch (- height 40)) (ink (hue :red 255)))
    (draw-rectangle cx cy (- width 12) ch alu-seta (hue :pale-yellow 0) b)
    (draw-frame cx cy (- width 12) ch 1 alu-seta (hue :light-grey 136) b)
    (let ((i 0) (prev nil))
      (dolist (v values)
        (let ((py (+ cy ch -2 (- (if (> hi lo) (round (* (- ch 4) (/ (- v lo) (- hi lo)))) 0)))))
          (if prev
              (draw-line (+ cx i -1) prev (+ cx i) py 1 alu-seta ink b)
              (draw-point (+ cx i) py alu-seta ink b))
          (setq prev py i (1+ i)))))
    (draw-string (string-append "Min: " (fmt lo)) cx (+ cy ch 3) *font-bold* alu-ior b)
    (draw-string (string-append "Max: " (fmt hi)) cx (+ cy ch 13) *font-bold* alu-ior b)
    (draw-string (string-append "Period: " (princ-to-string (network-period net))) (+ cx (%quotient width 2)) (+ cy ch 3)
                 *font-bold* alu-ior b)))

;;; the whole window

(defun plexi-draw (w)
  (let* ((r (window-inside w)) (x (first r)) (y (second r)) (width (third r)) (height (fourth r))
         (net (plexi-net w)) (bx x))
    (unless (and (plexi-back w) (= (bitmap-width (plexi-back w)) (window-width w))
                 (= (bitmap-height (plexi-back w)) (window-height w)))
      (setf (plexi-back w) (make-bitmap (window-width w) (window-height w))))
    (let ((b (plexi-back w)))
      (setf (plexi-hits w) nil)
      (draw-rectangle x y width height alu-seta 0 b)
      ;; the commands
      (dolist (p plexi-paradigms)
        (setq bx (draw-button w b bx (+ y 2) (car p) (equal (car p) (network-name net)) (list :paradigm (car p)))))
      (setq bx (+ bx 10))
      (setq bx (draw-button w b bx (+ y 2) "Train" (eq (plexi-mode w) :train) '(:train)))
      (setq bx (draw-button w b bx (+ y 2) "Run" (eq (plexi-mode w) :run) '(:run)))
      (setq bx (draw-button w b bx (+ y 2) "New input" nil '(:new)))
      (setq bx (draw-button w b bx (+ y 2) "Reset" nil '(:reset)))
      ;; the panes
      (let* ((top (+ y 20)) (bottom (- (+ y height) 14)) (h (- bottom top))
             (left (max 200 (%quotient (* width 11) 20))) (rx (+ x left 6)) (rw (- width left 6))
             (attr-h (min 112 (%quotient h 3))) (chart-h (min 86 (%quotient h 4)))
             (hinton-h (- h attr-h chart-h 8)))
        (draw-network w b net x top left h)
        (draw-attributes w b net rx top rw attr-h)
        (draw-hinton w b net rx (+ top attr-h 4) rw hinton-h)
        (draw-strip-chart b net rx (+ top attr-h hinton-h 8) rw chart-h)
        (draw-string (network-status net) (+ x 2) (+ bottom 2) *font-variable* alu-ior b))
      (bitblt alu-seta width height b x y (window-bitmap w) x y)
      (window-changed w))))

(defmethod draw-contents ((w plexi-window))
  (let ((b (plexi-back w)) (r (window-inside w)))
    (if (and b (= (bitmap-width b) (window-width w)) (= (bitmap-height b) (window-height w)))
        (bitblt alu-seta (third r) (fourth r) b (first r) (second r) (window-bitmap w) (first r) (second r))
        (add-window-input w '(:redraw)))))

(defmethod window-click ((w plexi-window) button x y)
  (when (= button 0) (add-window-input w (list :click x y))))

(defmethod mouse-documentation ((w plexi-window) x y)
  "Mouse-L: a command; on a cell: flip it and show its links; on an attribute: the next choice.  Keys: T, R, N, 1 2 3.")

(defun plexi-switch (w name)
  (let ((old (assoc name (plexi-networks w))))
    (setf (plexi-mode w) nil)
    (unless old
      (setq old (cons name (funcall (cdr (assoc name plexi-paradigms)))))
      (setf (plexi-networks w) (append (plexi-networks w) (list old))))
    (setq *plexi-network* (cdr old))
    (setf (plexi-selected w) (car (last (network-layers (cdr old)))))
    (when (eq (layer-role (plexi-selected w)) :output)      ; the layer the learning happens in
      (let ((ls (network-layers (cdr old)))) (setf (plexi-selected w) (nth (- (length ls) 2) ls))))
    (setf (plexi-unit w) 0)))

(defun plexi-click (w x y)
  (let ((hit (find-if (lambda (h) (and (>= x (first h)) (< x (+ (first h) (third h)))
                                       (>= y (second h)) (< y (+ (second h) (fourth h)))))
                      (plexi-hits w)))
        (net (plexi-net w)))
    (when hit
      (let ((action (nthcdr 4 hit)))
        (if (eq (first action) :cell)
            (let* ((l (second action)) (c (third action))
                   (i (+ (%quotient (- x (first hit)) c) (* (layer-cols l) (%quotient (- y (second hit)) c))))
                   (act (layer-act l)))
              (setf (plexi-selected w) l (plexi-unit w) i)
              (when (eq (layer-role l) :input)           ; draw on the input
                (setf (aref act i) (if (layer-bipolar l) (- (aref act i)) (- 1.0 (aref act i))))
                (unless (equal (network-name net) "Hopfield Memory") (run-step net))))
            (plexi-do w action))))))

;; do ACTION, a command or an attribute to change
(defun plexi-do (w action)
  (let ((net (plexi-net w)))
        (case (first action)
          (:paradigm (plexi-switch w (second action)))
          (:train (setf (plexi-mode w) (if (eq (plexi-mode w) :train) nil :train)))
          (:run (setf (plexi-mode w) (if (eq (plexi-mode w) :run) nil :run))
                (setf (network-settled net) nil))
          (:new (setf (plexi-mode w) nil)
                (present-noisy net)
                (unless (equal (network-name net) "Hopfield Memory") (run-step net)))
          (:reset (let ((name (network-name net)))
                    (setf (plexi-networks w) (remove-if (lambda (p) (equal (car p) name)) (plexi-networks w)))
                    (plexi-switch w name)))
          (:select (setf (plexi-selected w) (second action) (plexi-unit w) 0))
          (:attribute (change-attribute w net (second action))))))

(defun plexi-key (w e)
  (let ((c (and (vectorp e) (aref e 1))))
    (cond ((member c '(116 84)) (plexi-do w '(:train)))
          ((member c '(114 82)) (plexi-do w '(:run)))
          ((member c '(110 78)) (plexi-do w '(:new)))
          ((and (integerp c) (>= c 49) (<= c 51)) (plexi-switch w (car (nth (- c 49) plexi-paradigms)))))))

;; From any process: have the editor do ACTION, as if it were clicked, e.g.
;; (plexi-command '(:paradigm "Competition")), (plexi-command '(:train))
(defun plexi-command (action)
  (let ((w (find-plexi-window)))
    (when w (add-window-input w (list :do action)) t)))

;; one step of what the window is doing
(defun plexi-tick (w)
  (let ((net (plexi-net w)))
    (case (plexi-mode w)
      (:train (when (train-period net) (setf (plexi-mode w) nil)))
      (:run (when (run-step net) (setf (plexi-mode w) nil))))))

(defun plexi-loop (w)
  (unless (and *plexi-network* (plexi-net w)) (plexi-switch w (car (first plexi-paradigms))))
  (plexi-draw w)
  (loop
    (let ((e (take-window-input w)))
      (cond ((and (consp e) (eq (car e) :click)) (plexi-click w (second e) (third e)) (plexi-draw w))
            ((and (consp e) (eq (car e) :do)) (plexi-do w (second e)) (plexi-draw w))
            (e (plexi-key w e) (plexi-draw w))
            ((plexi-mode w) (plexi-tick w) (plexi-draw w) (process-sleep 15 "Compute"))
            (t (process-sleep 100 "Plexi"))))))

(defmethod window-process-function ((w plexi-window)) #'plexi-loop)

(defun find-plexi-window () (find-if (lambda (w) (typep w 'plexi-window)) *windows*))

;; open the editor, or select it
(defun plexi ()
  (let ((w (find-plexi-window)))
    (if w
        (select-window w)
        (let* ((width (min (- (%screen-width) 16) 820)) (height (min (- (desktop-height) 12) 520)))
          (add-window (make-instance 'plexi-window :label "Plexi"
                                     :x (max 0 (%quotient (- (%screen-width) width) 2)) :y 6
                                     :width width :height height))))
    t))

;; The Plexi edition (plexi.html): the colour screen, and the editor open
(defun plexi-edition ()
  (set-color-scheme :color)
  (plexi))
