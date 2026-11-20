;;; -*- Mode: Lisp -*-
;;; editor.lisp — Zmacs, a small Emacs-like editor for Lisp, in a window.
;;;
;;; The files it edits are the world's own source, world/*.lisp. The host
;;; keeps them (host/host.js), in one of three places:
;;;   world/ on disk, under `make serve` (tools/serve.py, M9): c-X c-S saves
;;;     there, and the next `make` builds the change in;
;;;   a folder you chose (Chrome, Edge: m-X Choose Folder): the same, without
;;;     a server;
;;;   the browser's storage (M10), when there is neither: saved files stay in
;;;     the browser; m-X Download File and m-X Upload Files (or dropping .lisp
;;;     files on the page) move them in and out.
;;; A file that was never saved comes from the text the world was built from,
;;; which the kernel keeps (%WORLD-SOURCE), cut into files again at the
;;; ";;;; FILE name" lines tools/catworld.py put there; the mode line says
;;; [built-in]. In the terminal there is no host, and the files are read-only.
;;; Edit a definition and compile it with c-sh-C: the running world changes at
;;; once, windows and all. Buffers live in the heap, so they are saved with the
;;; world too.
;;;
;;;   m-.        find a definition (defun, defmethod, defclass, ...), in any file
;;;   c-sh-C     compile the definition around the cursor
;;;   c-X c-F    visit a file          c-X B   another buffer
;;;   m-X        a command by name (Compile Buffer, List Buffers, ...)
;;;   Help (F4)  all the keys
;;;
;;; (ed) selects the editor, (ed 'name) edits a definition, (ed "file.lisp")
;;; visits a file. In the debugger, :Edit-Frame edits the failing function.

;;; ---- buffers: a vector, for speed ---------------------------------------------

(defmacro buf-name (b) `(aref ,b 1))
(defmacro buf-lines (b) `(aref ,b 2))        ; a vector of strings, with room to grow
(defmacro buf-count (b) `(aref ,b 3))        ; how many of them are lines
(defmacro buf-line (b) `(aref ,b 4))         ; the point: line and column
(defmacro buf-col (b) `(aref ,b 5))
(defmacro buf-top (b) `(aref ,b 6))          ; the first line on the screen
(defmacro buf-modified (b) `(aref ,b 7))
(defmacro buf-mark (b) `(aref ,b 8))         ; (line . col) or NIL
(defmacro buf-file (b) `(aref ,b 9))         ; the file it is saved to, on the host, or NIL
(defmacro buf-origin (b) `(aref ,b 10))      ; :host, :built (built-in text), :upload, or NIL

(defvar *buffers* nil)
(defvar *kill* "")                            ; the last text killed, for c-Y
(defvar *world-files-loaded* nil)

(defun make-buffer (name lines &optional file origin)
  (let ((b (vector :buffer name nil 0 0 0 0 nil nil file origin)))
    (set-buffer-lines b lines)
    (setq *buffers* (append *buffers* (list b)))
    b))

(defun set-buffer-lines (b lines)
  (let* ((n (length lines)) (v (make-vector (max 16 (+ n (%quotient n 4))) "")) (i 0))
    (dolist (l lines) (setf (aref v i) l) (setq i (1+ i)))
    (setf (buf-lines b) v)
    (setf (buf-count b) (max 1 n))
    (setf (buf-line b) (min (buf-line b) (1- (buf-count b))))
    (setf (buf-col b) 0)
    (setf (buf-modified b) nil)
    b))

(defun buffer-p (x) (and (vectorp x) (= (length x) 11) (eq (aref x 0) :buffer)))

;; the buffer's text, as it would be in its file
(defun buffer-text (b)
  (let ((pieces nil))
    (dotimes (i (buf-count b))
      (when (> i 0) (push *newline-string* pieces))
      (push (line-text b i) pieces))
    (apply #'string-append (nreverse pieces))))

(defun find-buffer (name)
  (dolist (b *buffers*) (when (string= (buf-name b) name) (return b))))

(defun line-text (b i) (aref (buf-lines b) i))
(defun set-line-text (b i s) (setf (aref (buf-lines b) i) s) (setf (buf-modified b) t))

(defun insert-line (b i s)
  (let ((n (buf-count b)) (v (buf-lines b)))
    (when (= n (length v))
      (let ((new (make-vector (* 2 n) "")))
        (dotimes (k n) (setf (aref new k) (aref v k)))
        (setf (buf-lines b) new)
        (setq v new)))
    (let ((k n))
      (while (> k i) (setf (aref v k) (aref v (1- k))) (setq k (1- k))))
    (setf (aref v i) s)
    (setf (buf-count b) (1+ n))
    (setf (buf-modified b) t)))

(defun delete-line (b i)
  (let ((n (buf-count b)) (v (buf-lines b)) (k i))
    (while (< k (1- n)) (setf (aref v k) (aref v (1+ k))) (setq k (1+ k)))
    (setf (aref v (1- n)) "")
    (setf (buf-count b) (max 1 (1- n)))
    (setf (buf-modified b) t)))

(defun split-lines (text) (%split-lines text))

;; the world's source, cut into one buffer per file
;;; ---- files on the host (make serve: tools/serve.py) --------------------------------

(defvar *file-reply* nil)                      ; set by the kernel: (id status . text)
(defvar *file-lock* (make-lock "Files"))
(defvar *file-place* "")                       ; where the host keeps them, for the mode line

;; Ask the host (kernel/gprims.c lists the operations): 1 list the files,
;; 2 read NAME, 3 write TEXT to NAME, 4 download TEXT as NAME, 5 upload files,
;; 6 where they are kept, 7 choose a folder, 8 the files dropped on the page. => text
;; The host's replies are bytes, UTF-8 text (M15); uploads and drops hold
;; several files, each read as UTF-8 by UNPACK-FILES.
;; RAW (M18): the reply as bytes, not decoded; and TEXT, a string of bytes, written as it is
(defun host-file (op name &optional (text "") raw)
  (unless (%file-server-p) (error "There is no host for files here (this is the terminal)"))
  (with-lock (*file-lock*)
    (let ((id nil) (start (get-internal-real-time))
          (patience (if (memq op '(5 7)) 600000 10000)))   ; a person is choosing files
      (while (null id)
        (setq id (%file-request (if raw (+ op 256) op) name text))
        (unless id (process-yield)))
      (process-wait "File" (lambda ()
                             (or (and *file-reply* (eql (car *file-reply*) id))
                                 (> (- (get-internal-real-time) start) patience))))
      (let ((r *file-reply*))
        (cond ((not (and r (eql (car r) id))) (error "The file server did not answer"))
              ((= (cadr r) 0) (if (or raw (memq op '(5 8))) (cddr r) (%utf8-decode (cddr r))))
              (t (error (string-append "Could not " (case op (3 "write ") (4 "download ") (5 "upload files") (7 "use a folder")
                                                          (t "read "))
                                       name ":")
                        (%utf8-decode (cddr r)))))))))

;; "name\nlength\nbytes" for each file, the length in bytes => ((name . text) ...)
(defun unpack-files (text)
  (let ((files nil) (i 0) (n (length text)))
    (while (< i n)
      (let* ((a (position-from #\Newline text i)) (b (position-from #\Newline text (1+ a)))
             (len (car (%read-from-string (substring text (1+ a) b) 0))))
        (push (cons (%utf8-decode (substring text i a)) (%utf8-decode (substring text (1+ b) (+ b 1 len)))) files)
        (setq i (+ b 1 len))))
    (nreverse files)))

(defun position-from (char string start)
  (let ((i start) (n (length string)))
    (while (and (< i n) (not (eq (aref string i) char))) (setq i (1+ i)))
    i))

(defun files-on-server-p () (eql (%file-server-p) 1))

(defun read-world-file (name) (host-file 2 name))
(defun write-world-file (name text) (host-file 3 name text))
(defun world-files-on-host ()
  (remove-if (lambda (s) (= (length s) 0)) (split-lines (host-file 1 ""))))

;; The files, one buffer each: from disk when the host serves them; any the
;; host does not have (the generated font) from the text the world was built from.
(defun load-world-files ()
  (unless *world-files-loaded*
    (setq *world-files-loaded* t)
    (when (%file-server-p) (setq *file-place* (or (ignore-errors (host-file 6 "")) "")))
    (let ((on-disk (if (%file-server-p) (ignore-errors (world-files-on-host)) nil))
          (name nil) (lines nil))
      (dolist (l (split-lines (%world-source)))
        (if (and (> (length l) 9) (string= (substring l 0 9) ";;;; FILE"))
            (progn (when name (load-world-file name (nreverse lines) on-disk))
                   (setq name (substring l 10) lines nil))
            (push l lines)))
      (when name
        (when (and lines (string= (car lines) "")) (pop lines))
        (load-world-file name (nreverse lines) on-disk))
      ;; files the host has that the world was not built from (saved uploads)
      (dolist (f on-disk)
        (unless (find-buffer f)
          (ignore-errors (make-buffer f (split-lines (read-world-file f)) f :host)))))))

;; From the host if it has the file; else the built-in text, which can be saved
;; to the browser or a folder, but not to a server (it only takes world files).
(defun load-world-file (name built-lines on-disk)
  (let ((text (and (member name on-disk) (ignore-errors (read-world-file name)))))
    (if text
        (make-buffer name (split-lines text) name :host)
        (make-buffer name built-lines (if (and (%file-server-p) (not (files-on-server-p))) name nil) :built))))

(defun world-file-names ()
  (load-world-files)
  (mapcar (lambda (b) (buf-name b)) *buffers*))

;;; ---- characters and strings ------------------------------------------------------

(defun symbol-char-p (c)
  (let ((k (char-code c)))
    (or (and (>= k 97) (<= k 122)) (and (>= k 65) (<= k 90)) (and (>= k 48) (<= k 57))
        (memq c '(#\- #\* #\% #\+ #\/ #\< #\> #\= #\! #\? #\& #\_ #\: #\.)))))

(defun word-char-p (c)
  (let ((k (char-code c)))
    (or (and (>= k 97) (<= k 122)) (and (>= k 65) (<= k 90)) (and (>= k 48) (<= k 57)))))

(defun leading-spaces (s)
  (let ((i 0)) (while (and (< i (length s)) (eq (aref s i) #\Space)) (setq i (1+ i))) i))

;;; ---- the window ---------------------------------------------------------------------

(defclass editor-window (label-mixin border-mixin window)
  ((buffer :initform nil :accessor editor-buffer)
   (echo :initform "" :accessor editor-echo)              ; the message on the last line
   (minibuffer :initform nil :accessor editor-minibuffer) ; #(prompt text) while reading one
   (goal :initform nil :accessor editor-goal)             ; the column c-N / c-P aim for
   (last :initform nil :accessor editor-last)             ; the last command, for c-K c-K
   (found :initform nil :accessor editor-found)           ; m-.: (name . places not yet visited)
   (search :initform "" :accessor editor-search)
   (paren :initform nil :accessor editor-paren)))         ; matching-paren box drawn, or NIL

(defmethod window-process-function ((w editor-window)) #'editor-loop)

(defun ed-x0 (w) (+ (first (window-inside w)) 3))
(defun ed-y0 (w) (+ (second (window-inside w)) 2))
(defun ed-cw () (char-cell-width))
(defun ed-lh () (line-height))
(defun ed-cols (w) (max 10 (%quotient (- (third (window-inside w)) 6) (ed-cw))))
(defun ed-rows (w) (max 2 (- (%quotient (- (fourth (window-inside w)) 4) (ed-lh)) 2)))   ; text rows
(defun ed-row-y (w row) (+ (ed-y0 w) (* row (ed-lh))))

;;; ---- display -------------------------------------------------------------------------

(defun draw-text-row (w row)
  (let* ((b (editor-buffer w)) (bm (window-bitmap w)) (i (+ (buf-top b) row)) (cols (ed-cols w))
         (y (ed-row-y w row)))
    (erase-rectangle (ed-x0 w) y (* cols (ed-cw)) (ed-lh) bm)
    (when (< i (buf-count b))
      (let ((s (line-text b i)))
        (if (> (length s) cols)
            (progn (draw-string (substring s 0 (1- cols)) (ed-x0 w) (1+ y) *font-fixed* alu-ior bm)
                   (draw-string "!" (+ (ed-x0 w) (* (1- cols) (ed-cw))) (1+ y) *font-bold* alu-ior bm))
            (draw-string s (ed-x0 w) (1+ y) *font-fixed* alu-ior bm))))))

(defun draw-mode-line (w)
  (let* ((b (editor-buffer w)) (bm (window-bitmap w)) (y (ed-row-y w (ed-rows w)))
         (r (window-inside w)) (n (buf-count b))
         (text (string-append "Zmacs (Lisp)  " (buf-name b) (if (buf-modified b) " *" "")
                              (case (buf-origin b)
                                (:built (if (buf-file b) "  [built-in, not saved yet]" "  [built-in]"))
                                (:upload "  [uploaded]")
                                (t ""))
                              "   Line " (princ-to-string (1+ (buf-line b))) " of " (princ-to-string n)
                              "   " (princ-to-string (%quotient (* 100 (buf-top b)) (max 1 n))) "%")))
    (draw-rectangle (first r) y (third r) (ed-lh) alu-seta 255 bm)
    (draw-string text (ed-x0 w) (1+ y) *font-fixed* alu-andca bm)))

(defun echo-text (w)
  (let ((m (editor-minibuffer w)))
    (if m (string-append (aref m 0) (aref m 1)) (editor-echo w))))

(defun draw-echo-line (w)
  (let ((bm (window-bitmap w)) (y (ed-row-y w (1+ (ed-rows w)))) (cols (ed-cols w)) (s (echo-text w)))
    (erase-rectangle (ed-x0 w) y (* cols (ed-cw)) (ed-lh) bm)
    (draw-string (if (> (length s) cols) (substring s (- (length s) cols)) s)
                 (ed-x0 w) (1+ y) *font-fixed* alu-ior bm)))

(defmethod draw-contents ((w editor-window))
  (setf (editor-paren w) nil)
  (when (editor-buffer w)
    (dotimes (row (ed-rows w)) (draw-text-row w row))
    (draw-mode-line w)
    (draw-echo-line w)))

(defmethod window-blinker ((w editor-window))
  (let ((b (editor-buffer w)) (m (editor-minibuffer w)))
    (when b
      (if m
          (list (+ (ed-x0 w) (* (min (1- (ed-cols w)) (length (echo-text w))) (ed-cw)))
                (ed-row-y w (1+ (ed-rows w))) (ed-cw) (ed-lh))
          (list (+ (ed-x0 w) (* (min (buf-col b) (1- (ed-cols w))) (ed-cw)))
                (ed-row-y w (- (buf-line b) (buf-top b))) (ed-cw) (ed-lh))))))

(defmethod mouse-documentation ((w editor-window) x y)
  "Zmacs: Mouse-L moves the cursor.  m-. finds a definition, c-sh-C compiles one, c-X c-F a file.  Help (F4): keys.")

(defmethod window-click ((w editor-window) button x y)
  (window-key w (list :click x y)))                    ; the editor's process moves the cursor

;; keep the point on the screen; => T if the text scrolled
(defun ensure-visible (w)
  (let* ((b (editor-buffer w)) (rows (ed-rows w)) (line (buf-line b)))
    (when (or (< line (buf-top b)) (>= line (+ (buf-top b) rows)))
      (setf (buf-top b) (max 0 (- line (%quotient rows 3))))
      t)))

;; the matching open paren of a close paren just before the point, boxed
(defun update-paren (w)
  (let ((bm (window-bitmap w)) (old (editor-paren w)))
    (when old
      (draw-rectangle (first old) (second old) (third old) (fourth old) alu-xor 255 bm)
      (setf (editor-paren w) nil))
    (let* ((b (editor-buffer w)) (l (buf-line b)) (c (buf-col b)) (s (line-text b l)))
      (when (and (> c 0) (eq (aref s (1- c)) #\)) (null (editor-minibuffer w)))
        (let ((open (car (open-parens b l (1- c)))))
          (when (and open (>= (car open) (buf-top b)) (< (car open) (+ (buf-top b) (ed-rows w)))
                     (< (cdr open) (ed-cols w)))
            (let ((r (list (+ (ed-x0 w) (* (cdr open) (ed-cw))) (ed-row-y w (- (car open) (buf-top b)))
                           (ed-cw) (ed-lh))))
              (draw-rectangle (first r) (second r) (third r) (fourth r) alu-xor 255 bm)
              (setf (editor-paren w) r))))))))

;; redraw after a command: :ALL, :LINE (just the point's line), or NIL
(defun redisplay (w what)
  (let ((b (editor-buffer w)))
    (if (or (eq what :all) (ensure-visible w))
        (progn (window-refresh w))
        (progn
          (let ((old (editor-paren w)))           ; take the box off before redrawing under it
            (when old
              (draw-rectangle (first old) (second old) (third old) (fourth old) alu-xor 255 (window-bitmap w))
              (setf (editor-paren w) nil)))
          (when (eq what :line) (draw-text-row w (- (buf-line b) (buf-top b))))
          (when (eq what :below)
            (let ((row (max 0 (- (buf-line b) (buf-top b) 1))))
              (while (< row (ed-rows w)) (draw-text-row w row) (setq row (1+ row)))))
          (draw-mode-line w)
          (draw-echo-line w)))
    (update-paren w)
    (window-changed w)))

(defun message (w string)
  (setf (editor-echo w) string))

;;; ---- reading Lisp text: parens, strings, comments ---------------------------------

;; the line where the top-level form around line L starts (a "(" in column 0)
(defun defun-start (b l)
  (while (and (> l 0) (not (let ((s (line-text b l))) (and (> (length s) 0) (eq (aref s 0) #\()))))
    (setq l (1- l)))
  l)

;; the parens still open just before line L, column C, innermost first: ((line . col) ...)
(defun open-parens (b l c)
  (let ((stack nil) (line (defun-start b l)) (in-string nil))
    (while (<= line l)
      (let* ((s (line-text b line)) (n (if (= line l) (min c (length s)) (length s))) (i 0))
        (while (< i n)
          (let ((ch (aref s i)))
            (cond (in-string (cond ((eq ch #\\) (setq i (1+ i)))
                                   ((eq ch #\") (setq in-string nil))))
                  ((eq ch #\") (setq in-string t))
                  ((eq ch #\;) (setq i n))
                  ((and (eq ch #\#) (< (1+ i) n) (eq (aref s (1+ i)) #\\)) (setq i (+ i 2)))
                  ((eq ch #\() (push (cons line i) stack))
                  ((eq ch #\)) (pop stack))))
          (setq i (1+ i))))
      (setq line (1+ line)))
    stack))

(defvar *body-forms* '(defun defmacro defmethod defclass defgeneric lambda let let* when unless
                       dolist dotimes while loop progn catch unwind-protect block handler-case
                       restart-case define-command with-output-to-string case cond))

;; where line L should start, in Lisp style
(defun lisp-indentation (b l)
  (let ((open (car (open-parens b l 0))))
    (if (null open)
        0
        (let* ((s (line-text b (car open))) (c (cdr open))
               (head-end (let ((i (1+ c))) (while (and (< i (length s)) (symbol-char-p (aref s i))) (setq i (1+ i))) i))
               (head (substring s (1+ c) head-end))
               (sym (and (> (length head) 0)
                         (let ((r (ignore-errors (%read-from-string head 0)))) (and (consp r) (car r))))))
          (cond ((and (symbolp sym) (memq sym *body-forms*)) (+ c 2))
                ((and (> (length head) 0) (< (1+ head-end) (length s))
                      (not (eq (aref s head-end) #\;)) (not (eq (aref s (1+ head-end)) #\;)))
                 (1+ head-end))                               ; under the first argument
                (t (1+ c)))))))

(defun indent-line (b l)
  (let* ((s (line-text b l)) (old (leading-spaces s)) (new (lisp-indentation b l)))
    (unless (= old new)
      (set-line-text b l (string-append (spaces new) (substring s old))))
    (- new old)))

;; the text of the top-level form around the point, and where it starts
(defun defun-text (b)
  (let* ((start (defun-start b (buf-line b))) (l start) (text "") (r nil))
    (while (and (< l (buf-count b)) (not (consp r)))
      (setq text (string-append text (line-text b l) *newline-string*))
      (setq r (handler-case (%read-from-string text 0) (error (c) :error)))
      (setq l (1+ l)))
    (if (consp r) (cons (car r) start) (error "Unbalanced parentheses in the definition at line" (1+ start)))))

;;; ---- editing ----------------------------------------------------------------------------

(defun point-line-text (w) (let ((b (editor-buffer w))) (line-text b (buf-line b))))

(defun insert-string (w string)
  (let ((b (editor-buffer w)) (multi nil))
    (dolist (piece (let ((p (split-lines string)) (first t) (r nil))
                     (dolist (x p) (unless first (push :newline r)) (push x r) (setq first nil))
                     (nreverse r)))
      (if (eq piece :newline)
          (progn (newline w) (setq multi t))
          (let* ((s (point-line-text w)) (c (buf-col b)))
            (set-line-text b (buf-line b) (string-append (substring s 0 c) piece (substring s c)))
            (setf (buf-col b) (+ c (length piece))))))
    (if multi :all :line)))

(defun newline (w)
  (let* ((b (editor-buffer w)) (s (point-line-text w)) (c (buf-col b)) (l (buf-line b)))
    (set-line-text b l (substring s 0 c))
    (insert-line b (1+ l) (substring s c))
    (setf (buf-line b) (1+ l))
    (setf (buf-col b) 0)
    :below))

(defun delete-backward (w)
  (let* ((b (editor-buffer w)) (s (point-line-text w)) (c (buf-col b)) (l (buf-line b)))
    (cond ((> c 0)
           (set-line-text b l (string-append (substring s 0 (1- c)) (substring s c)))
           (setf (buf-col b) (1- c))
           :line)
          ((> l 0)
           (let ((prev (line-text b (1- l))))
             (set-line-text b (1- l) (string-append prev s))
             (delete-line b l)
             (setf (buf-line b) (1- l))
             (setf (buf-col b) (length prev))
             :all))
          (t nil))))

(defun delete-forward (w)
  (let* ((b (editor-buffer w)) (s (point-line-text w)) (c (buf-col b)) (l (buf-line b)))
    (cond ((< c (length s))
           (set-line-text b l (string-append (substring s 0 c) (substring s (1+ c))))
           :line)
          ((< (1+ l) (buf-count b))
           (set-line-text b l (string-append s (line-text b (1+ l))))
           (delete-line b (1+ l))
           :all)
          (t nil))))

(defun kill-line (w)
  (let* ((b (editor-buffer w)) (s (point-line-text w)) (c (buf-col b)) (l (buf-line b))
         (killed (if (< c (length s)) (substring s c) *newline-string*))
         (append-p (eq (editor-last w) 'kill-line)))
    (setq *kill* (if append-p (string-append *kill* killed) killed))
    (if (< c (length s))
        (progn (set-line-text b l (substring s 0 c)) :line)
        (delete-forward w))))

(defun region-text (b l1 c1 l2 c2)
  (if (= l1 l2)
      (substring (line-text b l1) c1 c2)
      (let ((text (substring (line-text b l1) c1)) (l (1+ l1)))
        (while (< l l2)
          (setq text (string-append text *newline-string* (line-text b l)) l (1+ l)))
        (string-append text *newline-string* (substring (line-text b l2) 0 c2)))))

;; the region from the mark to the point, as (l1 c1 l2 c2), in order
(defun region (b)
  (let ((m (buf-mark b)))
    (unless m (error "There is no mark (c-Space sets it)"))
    (let ((ml (min (car m) (1- (buf-count b)))) (mc (cdr m)) (pl (buf-line b)) (pc (buf-col b)))
      (if (or (< ml pl) (and (= ml pl) (< mc pc))) (list ml mc pl pc) (list pl pc ml mc)))))

(defun kill-region (w copy-only)
  (let* ((b (editor-buffer w)) (r (region b)))
    (setq *kill* (apply #'region-text b r))
    (unless copy-only
      (let ((l1 (first r)) (c1 (second r)) (l2 (third r)) (c2 (fourth r)))
        (set-line-text b l1 (string-append (substring (line-text b l1) 0 c1) (substring (line-text b l2) c2)))
        (dotimes (i (- l2 l1)) (delete-line b (1+ l1)))
        (setf (buf-line b) l1)
        (setf (buf-col b) c1)))
    :all))

;;; ---- moving -------------------------------------------------------------------------------

(defun move-to (w l c)
  (let ((b (editor-buffer w)))
    (setf (buf-line b) (max 0 (min l (1- (buf-count b)))))
    (setf (buf-col b) (max 0 (min c (length (point-line-text w)))))
    nil))

(defun move-vertically (w d)
  (let ((b (editor-buffer w)))
    (unless (memq (editor-last w) '(next-line previous-line)) (setf (editor-goal w) (buf-col b)))
    (move-to w (+ (buf-line b) d) (editor-goal w))))

(defun forward-char (w)
  (let ((b (editor-buffer w)))
    (if (< (buf-col b) (length (point-line-text w)))
        (setf (buf-col b) (1+ (buf-col b)))
        (when (< (1+ (buf-line b)) (buf-count b)) (move-to w (1+ (buf-line b)) 0)))
    nil))

(defun backward-char (w)
  (let ((b (editor-buffer w)))
    (if (> (buf-col b) 0)
        (setf (buf-col b) (1- (buf-col b)))
        (when (> (buf-line b) 0) (move-to w (1- (buf-line b)) 10000)))
    nil))

(defun char-at-point (w)
  (let ((s (point-line-text w)) (c (buf-col (editor-buffer w))))
    (if (< c (length s)) (aref s c) #\Newline)))

(defun at-buffer-end (w) (let ((b (editor-buffer w))) (and (= (1+ (buf-line b)) (buf-count b)) (= (buf-col b) (length (point-line-text w))))))
(defun at-buffer-start (w) (let ((b (editor-buffer w))) (and (= (buf-line b) 0) (= (buf-col b) 0))))

(defun forward-word (w)
  (while (and (not (at-buffer-end w)) (not (word-char-p (char-at-point w)))) (forward-char w))
  (while (and (not (at-buffer-end w)) (word-char-p (char-at-point w))) (forward-char w))
  nil)

(defun char-before-point (w)
  (let ((s (point-line-text w)) (c (buf-col (editor-buffer w))))
    (if (> c 0) (aref s (1- c)) #\Newline)))

(defun backward-word (w)
  (while (and (not (at-buffer-start w)) (not (word-char-p (char-before-point w)))) (backward-char w))
  (while (and (not (at-buffer-start w)) (word-char-p (char-before-point w))) (backward-char w))
  nil)

;; the symbol at (or just before) the point, as a string, or ""
(defun symbol-at-point (w)
  (let* ((s (point-line-text w)) (c (min (buf-col (editor-buffer w)) (length s))) (a c) (z c))
    (while (and (> a 0) (symbol-char-p (aref s (1- a)))) (setq a (1- a)))
    (while (and (< z (length s)) (symbol-char-p (aref s z))) (setq z (1+ z)))
    (substring s a z)))

;;; ---- searching ---------------------------------------------------------------------------

(defun search-forward (w text)
  (let* ((b (editor-buffer w)) (l (buf-line b)) (c (1+ (buf-col b))) (found nil) (needle (string-downcase text)))
    (while (and (not found) (< l (buf-count b)))
      (let* ((s (line-text b l)) (i (and (<= c (length s)) (string-search needle (string-downcase (substring s (min c (length s))))))))
        (if i (setq found (cons l (+ c i))) (setq l (1+ l) c 0))))
    found))

(defun search-backward (w text)
  (let* ((b (editor-buffer w)) (l (buf-line b)) (limit (buf-col b)) (found nil) (needle (string-downcase text)))
    (while (and (not found) (>= l 0))
      (let* ((s (string-downcase (line-text b l))) (i 0) (last nil))
        (while (<= (+ i (length needle)) (min (length s) (+ limit (length needle) -1)))
          (when (string= needle (substring s i (+ i (length needle)))) (setq last i))
          (setq i (1+ i)))
        (if last (setq found (cons l last)) (setq l (1- l) limit 100000))))
    found))

(defun do-search (w forward)
  (let* ((last (editor-search w))
         (text (read-minibuffer w (string-append (if forward "Search" "Reverse search")
                                                 (if (> (length last) 0) (string-append " (" last ")") "") ": ")
                                "")))
    (when (= (length text) 0) (setq text last))          ; Return alone: search again
    (when (> (length text) 0)
      (setf (editor-search w) text)
      (let ((p (if forward (search-forward w text) (search-backward w text))))
        (if p
            (progn (move-to w (car p) (cdr p)) (message w ""))
            (message w (string-append "Not found: " text)))))
    nil))

;;; ---- definitions: m-. --------------------------------------------------------------------

;; the name a line defines, if it starts a definition: (definer name-string rest-of-line)
(defun definition-on-line (s)
  (when (and (> (length s) 5) (eq (aref s 0) #\() (eq (aref s 1) #\d) (eq (aref s 2) #\e))
    (let* ((sp (position-of #\Space s)))
      (when sp
        (let ((i (1+ sp)) (start nil))
          (when (and (< i (length s)) (eq (aref s i) #\")) (setq i (1+ i)))   ; (define-command "Name"
          (setq start i)
          (while (and (< i (length s)) (or (symbol-char-p (aref s i)) (and (eq (aref s (1- start)) #\") (eq (aref s i) #\Space))))
            (setq i (1+ i)))
          (list (substring s 1 sp) (substring s start i) (substring s i)))))))

;; every place NAME (a symbol, or (METHOD name qualifier class)) is defined: ((buffer . line) ...)
(defun definition-places (name)
  (load-world-files)
  (let* ((method (and (consp name) (eq (car name) 'method)))
         (sym (if method (second name) name))
         (target (string-downcase (if (symbolp sym) (symbol-name sym) (princ-to-string sym))))
         (class (and method (string-append " " (string-downcase (symbol-name (fourth name))) ")")))
         (qualifier (and method (third name) (string-downcase (prin1-to-string (third name)))))
         (places nil))
    (dolist (b *buffers*)
      (dotimes (l (buf-count b))
        (let ((d (definition-on-line (line-text b l))))
          (when (and d (string= (string-downcase (second d)) target)
                     (or (not method)
                         (and (string= (first d) "defmethod")
                              (string-search class (string-downcase (third d)))
                              (if qualifier
                                  (string-search qualifier (string-downcase (third d)))
                                  (not (string-search " :" (third d)))))))
            (push (cons b l) places)))))
    (nreverse places)))

(defun show-buffer (w b)
  (unless (eq b (editor-buffer w))
    (setf (editor-buffer w) b)
    (setf (window-label w) (string-append "Zmacs: " (buf-name b))))
  :all)

(defun go-to-place (w place)
  (show-buffer w (car place))
  (let ((b (car place)))
    (move-to w (cdr place) 0)
    (setf (buf-top b) (max 0 (- (cdr place) 2))))
  :all)

;; a definition typed in the Listener has its source on the symbol: show it in a buffer
(defun listener-definition-buffer (name)
  (let ((source (and (symbolp name) (get name 'source))))
    (when source
      (let ((text (with-output-to-string (s) (let ((*standard-output* s)) (pp source 0)))))
        (or (find-buffer (string-append "*" (symbol-name name) "*"))
            (make-buffer (string-append "*" (symbol-name name) "*")
                         (append (list ";;; Defined in a Listener; c-sh-C compiles it again.")
                                 (split-lines text))))))))

(defun find-definition (w name)
  (let ((places (definition-places name)))
    (cond (places
           (setf (editor-found w) (cons name (cdr places)))
           (message w (if (cdr places)
                          (string-append (princ-to-string (length places)) " definitions; c-. goes to the next")
                          ""))
           (go-to-place w (car places)))
          ((listener-definition-buffer name)
           (setf (editor-found w) nil)
           (message w "Defined in a Listener")
           (show-buffer w (listener-definition-buffer name))
           (move-to w 1 0)
           :all)
          (t (message w (string-append "No definition of " (prin1-to-string name) " is known")) nil))))

(defun next-definition (w)
  (let ((f (editor-found w)))
    (if (and f (cdr f))
        (progn (setf (editor-found w) (cons (car f) (cddr f)))
               (message w (string-append (princ-to-string (length (cddr f))) " more"))
               (go-to-place w (cadr f)))
        (progn (message w "No more definitions") nil))))

(defun read-definition-name (w)
  (let* ((default (symbol-at-point w))
         (text (read-minibuffer w (if (> (length default) 0)
                                      (string-append "Edit definition (" default "): ")
                                      "Edit definition: ")
                                "")))
    (when (= (length text) 0) (setq text default))
    (when (> (length text) 0) (car (%read-from-string text 0)))))

;;; ---- compiling -------------------------------------------------------------------------------

(defun form-name (form)
  (if (and (consp form) (consp (cdr form))) (cadr form) form))

;; The package a buffer's text is read in (M12): that of the last
;; (in-package ...) above line L; else COMMON-LISP for a file (the world's own
;; source has no IN-PACKAGE); else the Listener's.
(defun buffer-package (b l)
  (let ((p nil) (i 0))
    (while (< i (min l (buf-count b)))
      (let ((s (line-text b i)))
        (when (and (> (length s) 12) (string-equal (substring s 0 12) "(in-package "))
          (let ((r (ignore-errors (%read-from-string s 0))))
            (when (and (consp r) (consp (car r)) (consp (cdr (car r))))
              (setq p (or (find-package (cadr (car r))) p))))))
      (setq i (1+ i)))
    (or p (if (buf-file b) (find-package "COMMON-LISP") *package*))))

;; c-sh-C: evaluate (compile) the definition around the point
(defun compile-definition (w)
  (let* ((b (editor-buffer w))
         (*package* (buffer-package b (defun-start b (buf-line b))))
         (r (defun-text b)) (form (car r)))
    (message w (string-append "Compiling " (short-printed (form-name form) 40) "..."))
    (redisplay w nil)
    (let ((value (eval form)))
      (message w (string-append "Compiled " (short-printed (form-name form) 40) " => " (short-printed value 40))))
    nil))

(defun compile-buffer (w)
  (let* ((b (editor-buffer w)) (text "") (pos 0) (n 0)
         (*package* (buffer-package b 0)))            ; an (in-package ...) in it changes this

    (dotimes (l (buf-count b)) (setq text (string-append text (line-text b l) *newline-string*)))
    (loop
      (let ((r (%read-from-string text pos)))
        (unless (consp r) (return nil))
        (eval (car r))
        (setq pos (cdr r) n (1+ n))))
    (message w (string-append "Compiled " (princ-to-string n) " forms from " (buf-name b)))
    nil))

;;; ---- the minibuffer -----------------------------------------------------------------------------

;; read a line on the echo line; Tab completes from CHOICES; c-G gives up
(defun read-minibuffer (w prompt initial &optional choices)
  (let ((m (vector prompt initial)))
    (setf (editor-minibuffer w) m)
    (unwind-protect
        (loop
          (redisplay w nil)
          (let* ((e (editor-next-event w))
                 (code (if (vectorp e) (aref e 1) 0))
                 (mods (if (vectorp e) (logand (aref e 3) bucky-bits) 0)))
            (cond ((not (vectorp e)) nil)
                  ((and (= mods mod-control) (memq code '(103 71))) (throw :editor-quit nil))   ; c-G
                  ((= code key-return) (return (aref m 1)))
                  ((= code key-rubout)
                   (when (> (length (aref m 1)) 0)
                     (setf (aref m 1) (substring (aref m 1) 0 (1- (length (aref m 1)))))))
                  ((and (= code key-tab) choices)
                   (setf (aref m 1) (complete-from (aref m 1) choices)))
                  ((and (= mods 0) (typed-char-p code))
                   (setf (aref m 1) (string-append (aref m 1) (string (code-char code))))))))
      (setf (editor-minibuffer w) nil))))

(defun complete-from (text choices)
  (let ((matches (remove-if-not (lambda (c) (and (>= (length c) (length text))
                                                 (string= (string-downcase (substring c 0 (length text)))
                                                          (string-downcase text))))
                                choices)))
    (if (null matches)
        text
        (let ((p (car matches)))
          (dolist (m (cdr matches))
            (let ((i 0))
              (while (and (< i (length p)) (< i (length m)) (eq (char-downcase (aref p i)) (char-downcase (aref m i))))
                (setq i (1+ i)))
              (setq p (substring p 0 i))))
          (if (> (length p) (length text)) p text)))))

(defun read-choice (w prompt choices)
  (let* ((text (read-minibuffer w prompt "" choices))
         (exact (dolist (c choices) (when (string= (string-downcase c) (string-downcase text)) (return c)))))
    (or exact
        (let ((c (complete-from text choices)))
          (dolist (x choices) (when (string= (string-downcase x) (string-downcase c)) (return x)))))))

;;; ---- extended commands (m-X) and help ----------------------------------------------------------

;;; ---- saving, reverting, comparing ------------------------------------------------------

;; File commands report failures (no server, a cancelled dialog, ...) on the
;; echo line rather than in the debugger: they are not bugs.
(defmacro with-file-errors (w &rest body)
  `(handler-case (progn ,@body)
     (error (c) (message ,w (error-text c)) nil)))

(defun error-text (c)
  (let ((s (condition-message c)))
    (dolist (i (condition-irritants c)) (setq s (string-append s " " (princ-to-string i))))
    (if (string-search "cancelled" s) "Cancelled" s)))


(defun need-file (b)
  (or (buf-file b)
      (error (string-append (buf-name b) " has no file to save to: "
                            (cond ((not (%file-server-p)) "there is no host for files here")
                                  ((files-on-server-p) "the server only takes the files in world/")
                                  (t "only .lisp buffers are saved"))))))

(defun save-buffer (w)
  (with-file-errors w
    (let* ((b (editor-buffer w)) (file (need-file b)))
      (if (not (buf-modified b))
          (message w "(No changes need to be saved)")
          (progn
            (write-world-file file (buffer-text b))
            (setf (buf-modified b) nil)
            (setf (buf-origin b) :host)
            (message w (string-append "Wrote " file (if (files-on-server-p) " in world/" (string-append " in " *file-place*))))))
      nil)))

(defun revert-buffer (w)
  (with-file-errors w
    (let ((b (editor-buffer w)))
      (set-buffer-lines b (split-lines (read-world-file (need-file b))))
      (setf (buf-origin b) :host)
      (move-to w (buf-line b) 0)
      (message w (string-append "Read " (buf-file b) " again, from " *file-place*))
      :all)))

;; The lines that differ between OLD and NEW (lists of strings), as hunks
;; (old-start old-lines new-start new-lines): the common beginning and end
;; are skipped, and the rest is matched by a longest common subsequence.
(defun diff-lines (old new)
  (let* ((a (list->vector old)) (b (list->vector new)) (n (length a)) (m (length b)) (p 0) (s 0))
    (while (and (< p n) (< p m) (string= (aref a p) (aref b p))) (setq p (1+ p)))
    (while (and (< s (- n p)) (< s (- m p)) (string= (aref a (- n s 1)) (aref b (- m s 1)))) (setq s (1+ s)))
    (let ((n2 (- n p s)) (m2 (- m p s)))
      (if (or (> n2 400) (> m2 400) (= n2 0) (= m2 0))
          (if (and (= n2 0) (= m2 0)) nil (list (diff-hunk a p (+ p n2) b p (+ p m2))))
          (diff-middle a b p n2 m2)))))

(defun diff-hunk (a a0 a1 b b0 b1)
  (let ((old nil) (new nil) (i a1) (j b1))
    (while (> i a0) (setq i (1- i)) (push (aref a i) old))
    (while (> j b0) (setq j (1- j)) (push (aref b j) new))
    (list a0 old b0 new)))

;; the middle N2 x M2 lines, from P on: lcs[i][j] = longest common run of a[i..], b[j..]
(defun diff-middle (a b p n2 m2)
  (let ((lcs (make-vector (* (1+ n2) (1+ m2)) 0)) (i n2) (hunks nil))
    (while (> i 0)
      (setq i (1- i))
      (let ((j m2))
        (while (> j 0)
          (setq j (1- j))
          (setf (aref lcs (+ (* i (1+ m2)) j))
                (if (string= (aref a (+ p i)) (aref b (+ p j)))
                    (1+ (aref lcs (+ (* (1+ i) (1+ m2)) (1+ j))))
                    (max (aref lcs (+ (* (1+ i) (1+ m2)) j)) (aref lcs (+ (* i (1+ m2)) (1+ j)))))))))
    (let ((i 0) (j 0) (i0 nil) (j0 nil))
      (while (or (< i n2) (< j m2))
        (if (and (< i n2) (< j m2) (string= (aref a (+ p i)) (aref b (+ p j))))
            (progn (when i0 (push (diff-hunk a (+ p i0) (+ p i) b (+ p j0) (+ p j)) hunks) (setq i0 nil))
                   (setq i (1+ i) j (1+ j)))
            (progn (unless i0 (setq i0 i j0 j))
                   (if (and (< j m2) (or (>= i n2) (>= (aref lcs (+ (* i (1+ m2)) (1+ j))) (aref lcs (+ (* (1+ i) (1+ m2)) j)))))
                       (setq j (1+ j))
                       (setq i (1+ i))))))
      (when i0 (push (diff-hunk a (+ p i0) (+ p n2) b (+ p j0) (+ p m2)) hunks)))
    (nreverse hunks)))

(defun compare-with-file (w)
  (with-file-errors w
    (let* ((b (editor-buffer w)) (file (need-file b))
           (old (split-lines (or (ignore-errors (read-world-file file))
                                 (error (string-append file " has not been saved yet: there is nothing to compare with")))))
           (new (let ((r nil)) (dotimes (i (buf-count b)) (push (line-text b i) r)) (nreverse r)))
           (hunks (diff-lines old new))
           (lines (list (string-append "Comparing " file " in " *file-place* " (-) with the buffer (+):") "")))
      (if (null hunks)
          (progn (message w "No differences") nil)
          (progn
            (dolist (h hunks)
              (setq lines (append lines
                                  (list (string-append "Line " (princ-to-string (1+ (third h))) ":"))
                                  (mapcar (lambda (l) (string-append "- " l)) (second h))
                                  (mapcar (lambda (l) (string-append "+ " l)) (fourth h))
                                  (list ""))))
            (let ((old-buffer (find-buffer "*Compare*")))
              (when old-buffer (setq *buffers* (remove-eq old-buffer *buffers*))))
            (message w (string-append (princ-to-string (length hunks)) " place(s) differ"))
            (show-buffer w (make-buffer "*Compare*" lines))
            (move-to w 0 0)
            :all)))))

;;; ---- moving files in and out (M10) -------------------------------------------------------

(defun download-file (w)
  (with-file-errors w
    (let ((b (editor-buffer w)))
      (host-file 4 (if (buf-file b) (buf-file b) (buf-name b)) (buffer-text b))
      (message w (string-append "Downloaded " (buf-name b)))
      nil)))

;; files from the user (Upload Files, or dropped on the page) become buffers,
;; modified: c-X c-S saves them where the files are kept
(defun take-files (w files how)
  (if (null files)
      (progn (message w "No .lisp files") nil)
      (let ((first nil))
        (dolist (f files)
          (let ((b (find-buffer (car f))))
            (if b
                (set-buffer-lines b (split-lines (cdr f)))
                (setq b (make-buffer (car f) (split-lines (cdr f)) (and (%file-server-p) (car f)))))
            (setf (buf-origin b) :upload)
            (setf (buf-modified b) t)
            (unless first (setq first b))))
        (message w (string-append how ": " (reduce (lambda (a b) (string-append a ", " b)) (mapcar #'car files))
                                  "  (c-X c-S saves to " *file-place* ")"))
        (show-buffer w first)
        (move-to w 0 0)
        :all)))

(defun upload-files (w)
  (with-file-errors w
    (take-files w (unpack-files (host-file 5 "")) "Uploaded")))

(defun lisp-name-p (name)
  (and (> (length name) 5) (string= (substring name (- (length name) 5)) ".lisp")))

(defun choose-folder (w)
  (with-file-errors w
    (setq *file-place* (host-file 7 ""))
    (dolist (b *buffers*)
      (when (and (null (buf-file b)) (lisp-name-p (buf-name b))) (setf (buf-file b) (buf-name b))))
    (message w (string-append "Files are saved in " *file-place* " from now on"))
    nil))

(defun where-are-files (w)
  (with-file-errors w
    (message w (if (%file-server-p)
                   (string-append "Files are kept in " (setq *file-place* (host-file 6 "")))
                   "There is no host for files here: the built-in source, read-only"))
    nil))

;; the Screen Manager got EV-FILES: .lisp files were dropped on the page
(defun files-arrived ()
  (let ((w (or (find-editor) (make-editor-window))))
    (window-key w (list :files-arrived))
    (select-window w)))

(defvar *extended-commands*
  '(("Save Buffer" . save-buffer)
    ("Download File" . download-file)
    ("Upload Files" . upload-files)
    ("Choose Folder" . choose-folder)
    ("Where Are Files" . where-are-files)
    ("Revert Buffer" . revert-buffer)
    ("Compare with File" . compare-with-file)
    ("Compile Buffer" . compile-buffer)
    ("List Buffers" . list-buffers)
    ("Kill Buffer" . kill-buffer-command)
    ("Find File" . find-file-command)
    ("Help" . editor-help)))

(defun extended-command (w)
  (let ((name (read-choice w "Extended command: " (mapcar #'car *extended-commands*))))
    (if name
        (funcall (cdr (assoc-string name *extended-commands*)) w)
        (progn (message w "No such command") nil))))

(defun assoc-string (key alist)
  (dolist (x alist) (when (string= (car x) key) (return x))))

(defun find-file-command (w)
  (let ((name (read-choice w "Find file: " (world-file-names))))
    (if name (show-buffer w (find-buffer name)) (progn (message w "No such file") nil))))

(defun switch-buffer-command (w)
  (let ((name (read-choice w "Buffer: " (mapcar (lambda (b) (buf-name b)) *buffers*))))
    (if name (show-buffer w (find-buffer name)) nil)))

(defun kill-buffer-command (w)
  (let ((b (editor-buffer w)))
    (setq *buffers* (remove-eq b *buffers*))
    (when (null *buffers*) (make-buffer "*scratch*" (list "")))
    (message w (string-append "Killed " (buf-name b)))
    (show-buffer w (car *buffers*))))

(defun list-buffers (w)
  (let ((lines (list "Buffers (c-X B visits one):" "")))
    (dolist (b *buffers*)
      (unless (string= (buf-name b) "*Buffer List*")
        (setq lines (append lines (list (string-append (if (buf-modified b) " * " "   ") (buf-name b)
                                                       "  (" (princ-to-string (buf-count b)) " lines)"))))))
    (let ((old (find-buffer "*Buffer List*")))
      (when old (setq *buffers* (remove-eq old *buffers*))))
    (show-buffer w (make-buffer "*Buffer List*" lines))
    (move-to w 0 0)
    :all))

(defvar *editor-help*
  '("Zmacs keys.  c- is Control, m- is Meta (Option, or Altmode/Escape first)."
    ""
    "  c-F c-B c-N c-P, arrows   move           m-F m-B          by words"
    "  c-A c-E                   line start/end m-< m->          buffer start/end"
    "  c-V m-V                   page down/up   c-L              recenter"
    "  Rubout c-D                delete         c-K              kill line"
    "  c-Space                   set the mark   c-W m-W          kill/copy the region"
    "  c-Y                       yank the kill  Tab              indent for Lisp"
    "  c-S c-R                   search         c-G              give up"
    "  m-.                       find a definition (c-. the next one)"
    "  c-sh-C                    compile the definition around the cursor"
    "  c-X c-F  c-X B  c-X K     visit a file, a buffer; kill this buffer"
    "  c-X c-S                   save the buffer"
    "  m-X                       Save Buffer, Revert Buffer, Compare with File,"
    "                            Download File, Upload Files, Choose Folder,"
    "                            Where Are Files, Compile Buffer, List Buffers, ..."
    ""
    "The files are the world's source, world/*.lisp.  Under make serve they"
    "are the files on disk; otherwise saves go to the browser (or to a folder"
    "you chose), and Download File / Upload Files, or dropping .lisp files"
    "on the page, move them in and out.  [built-in] in the mode line: the text"
    "the world was built from.  Compiling changes the running system."))

(defun editor-help (w)
  (let ((old (find-buffer "*Help*")))
    (show-buffer w (or old (make-buffer "*Help*" *editor-help*)))
    (move-to w 0 0)
    :all))

;;; ---- the command loop ------------------------------------------------------------------------------

(defun editor-next-event (w)
  (process-wait "Tyi" (lambda () (window-input w)))
  (take-window-input w))

(defun ctrl-key-p (code mods ch) (and (= mods mod-control) (= code (char-code ch))))

;; do what event E asks; => what to redisplay
(defun editor-command (w e prefix)
  (if (not (vectorp e))
      (case (car e)
        (:click (let* ((b (editor-buffer w)) (row (%quotient (- (third e) (ed-y0 w)) (ed-lh)))
                       (col (%quotient (- (second e) (ed-x0 w)) (ed-cw))))
                  (when (and (>= row 0) (< row (ed-rows w)))
                    (move-to w (+ (buf-top b) row) (max 0 col)))
                  nil))
        (:find-definition (find-definition w (second e)))
        (:files-arrived (with-file-errors w (take-files w (unpack-files (host-file 8 "")) "Dropped")))
        (:find-file (let ((b (progn (load-world-files) (find-buffer (second e)))))
                      (if b (show-buffer w b) (progn (message w "No such file") nil))))
        (t nil))
      (let* ((code (aref e 1)) (raw (aref e 3)) (mods (logand raw bucky-bits)) (b (editor-buffer w)))
        (when (eq prefix :meta) (setq mods (logior mods mod-meta)))
        (setf (editor-echo w) "")
        (cond
          ((eq prefix :c-x)
           (cond ((ctrl-key-p code mods #\f) (find-file-command w))
                 ((and (= mods 0) (memq code '(98 66))) (switch-buffer-command w))       ; B
                 ((and (= mods 0) (memq code '(107 75))) (kill-buffer-command w))       ; K
                 ((ctrl-key-p code mods #\s) (save-buffer w))
                 (t (message w "c-X: not a command") nil)))
          ((= code key-altmode) :altmode)                                          ; Altmode: Meta next
          ((= code key-help) (editor-help w))
          ((= mods 0)
           (cond ((= code key-return) (newline w))
                 ((= code key-rubout) (delete-backward w))
                 ((= code key-tab) (let ((d (indent-line b (buf-line b))))
                                     (move-to w (buf-line b) (max (leading-spaces (point-line-text w)) (+ (buf-col b) d)))
                                     :line))
                 ((= code key-left) (backward-char w)) ((= code key-right) (forward-char w))
                 ((= code key-up) (move-vertically w -1)) ((= code key-down) (move-vertically w 1))
                 ((typed-char-p code) (insert-string w (string (code-char code))))
                 (t nil)))
          ((and (= mods mod-control) (= code 67))                           ; c-sh-C
           (compile-definition w))
          ((= mods mod-control)
           (case code
             (102 (forward-char w)) (98 (backward-char w))
             (110 (move-vertically w 1)) (112 (move-vertically w -1))
             (97 (move-to w (buf-line b) 0)) (101 (move-to w (buf-line b) 10000))
             (100 (delete-forward w)) (107 (kill-line w))
             (121 (let ((r (insert-string w *kill*))) r))
             (119 (kill-region w nil))
             (32 (setf (buf-mark b) (cons (buf-line b) (buf-col b))) (message w "Mark set") nil)
             (115 (do-search w t)) (114 (do-search w nil))
             (118 (move-to w (+ (buf-line b) (- (ed-rows w) 2)) (buf-col b))
                  (setf (buf-top b) (max 0 (- (buf-line b) 1))) :all)
             (108 (setf (buf-top b) (max 0 (- (buf-line b) (%quotient (ed-rows w) 2)))) :all)
             (120 :c-x)
             (46 (next-definition w))                                       ; c-.
             (103 (message w "Quit") nil)                                    ; c-G
             (t nil)))
          ((= mods mod-meta)
           (case code
             (102 (forward-word w)) (98 (backward-word w))
             (60 (move-to w 0 0)) (62 (move-to w 100000 0))
             (118 (move-to w (- (buf-line b) (- (ed-rows w) 2)) (buf-col b))
                  (setf (buf-top b) (max 0 (- (buf-line b) (- (ed-rows w) 3)))) :all)
             (119 (kill-region w t) (message w "Copied") nil)
             (46 (let ((name (read-definition-name w))) (when name (find-definition w name))))
             (120 (extended-command w))
             (t nil)))
          (t nil)))))

(defun command-symbol (e)
  (when (vectorp e)
    (let ((code (aref e 1)) (mods (logand (aref e 3) bucky-bits)))
      (cond ((and (= mods mod-control) (= code 107)) 'kill-line)
            ((or (= code key-up) (and (= mods mod-control) (= code 112))) 'previous-line)
            ((or (= code key-down) (and (= mods mod-control) (= code 110))) 'next-line)
            (t nil)))))

(defun editor-loop (w)
  (load-world-files)
  (unless (editor-buffer w)
    (show-buffer w (or (find-buffer "windows.lisp") (car *buffers*) (make-buffer "*scratch*" (list "")))))
  (let ((*standard-output* (make-stream (lambda (s string)
                                          (let ((lines (split-lines string)))
                                            (setf (editor-echo w) (string-append (editor-echo w) (car (last lines))))))
                                        (lambda (s) (setf (editor-echo w) ""))
                                        w))
        (prefix nil))
    (redisplay w :all)
    (loop
      (let* ((e (editor-next-event w))
             (what (catch :editor-quit (editor-command w e prefix))))
        (setq prefix (if (memq what '(:c-x :altmode)) (if (eq what :altmode) :meta what) nil))
        (setf (editor-last w) (command-symbol e))
        (when (eq what :c-x) (message w "c-X-"))
        (when (eq what :altmode) (message w "Meta-"))
        (redisplay w (if (memq what '(:all :line :below)) what nil))))))

(defmethod window-reshaped ((w editor-window)) nil)

;;; ---- getting to the editor ------------------------------------------------------------------------

(defun find-editor ()
  (dolist (w *windows*) (when (typep w 'editor-window) (return w))))

(defun make-editor-window ()
  (let ((w (make-instance 'editor-window :label "Zmacs"
                          :x (max 0 (- (%screen-width) (min 760 (%screen-width)) 10)) :y 4
                          :width (min 760 (%screen-width))
                          :height (max 200 (- (desktop-height) 20)))))
    (add-window w)
    w))

;; Select the editor (making one if needed), then have it do REQUEST.
(defun ed (&optional what)
  (let ((w (or (find-editor) (make-editor-window))))
    (cond ((stringp what) (window-key w (list :find-file what)))
          (what (window-key w (list :find-definition what))))
    (select-window w)
    w))

(defun edit-definition (name)
  (ed name)
  nil)

(define-command "Edit Definition" ((name function-name-p)) ()
  "Edit the definition of this function in the editor (Zmacs)."
  (edit-definition name))

(define-command "Edit File" ((name :string)) ()
  "Visit one of the world's source files in the editor, e.g. :Edit-File \"windows.lisp\"."
  (ed name)
  nil)

(define-command "Editor" () ()
  "Select the editor (Zmacs), making one if there is none."
  (ed)
  nil)
