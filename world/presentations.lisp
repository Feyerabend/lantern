;;; -*- Mode: Lisp -*-
;;; presentations.lisp — output that stays connected to the objects it shows.
;;;
;;; When the Listener prints a value with PRESENT, it records which characters
;;; show which object: the whole value, and inside a list or vector every
;;; element, recursively. Moving the mouse over such text outlines the
;;; innermost object there; Mouse-L inspects it, Mouse-R offers the commands
;;; that apply to its type. The Inspector shows an object's parts the same
;;; way, so you can click your way through the world.
;;;
;;; Commands (DEFINE-COMMAND) are shared by the mouse and the keyboard: in a
;;; Listener, a line starting with a colon is a command line,
;;;   :Inspect *windows*      :Describe 'car      :Help
;;; and Tab completes the command name. Help (F4) lists the commands.

;;; ---- the objects behind "Use as Input" ---------------------------------

(defvar *presented-objects* (make-vector 256 nil))
(defvar *presented-count* 0)

(defun remember-object (object)
  (let ((n *presented-count*))
    (setf (aref *presented-objects* (mod n 256)) object)
    (setq *presented-count* (1+ n))
    n))

(defun presentation-object (n)
  (if (and (< n *presented-count*) (>= n (- *presented-count* 256)))
      (aref *presented-objects* (mod n 256))
      (error "That presented object is no longer remembered:" n)))

;;; ---- presenting in a Listener ---------------------------------------------

(defun tagged-vector-p (x)
  (and (vectorp x) (> (length x) 0) (keywordp (aref x 0))))

(defun proper-list-p (x) (and (list-length-or-nil x) t))

;; Print OBJECT on STREAM; in a Listener, record presentations for it and its parts.
(defun present (object &optional stream)
  (let* ((s (resolve-stream stream)) (w (stream-data s)))
    (if (typep w 'listener-window)
        (listener-present w object 0)
        (prin1 object s))
    object))

(defun listener-present (w object depth)
  (let* ((tm (listener-term w)) (serial (term-serial tm)) (start (listener-column tm)))
    (cond ((and (consp object) (< depth 6) (proper-list-p object)
                (not (and (eq (car object) 'quote) (consp (cdr object)) (null (cddr object)))))
           (listener-write w "(")
           (let ((first t) (n 0))
             (dolist (x object)
               (unless first (listener-write w " "))
               (setq first nil n (1+ n))
               (if (> n 40)
                   (progn (listener-write w "...") (return nil))
                   (listener-present w x (1+ depth)))))
           (listener-write w ")"))
          ((and (vectorp object) (not (tagged-vector-p object)) (not (%array-header-p object)) (< depth 6))
           (listener-write w "#(")
           (dotimes (i (min 40 (length object)))
             (when (> i 0) (listener-write w " "))
             (listener-present w (aref object i) (1+ depth)))
           (when (> (length object) 40) (listener-write w " ..."))
           (listener-write w ")"))
          (t (listener-write w (prin1-to-string object))))
    ;; recorded only if it stayed on one logical line
    (when (= serial (term-serial tm))
      (push (vector object serial start (listener-column tm)) (listener-records w))
      (when (> (length (listener-records w)) 3000)
        (setf (cdr (nthcdr 1999 (listener-records w))) nil)))))

(defmethod window-presentation-at ((w listener-window) x y)
  (let* ((tm (listener-term w)) (rm (term-rowmap tm)) (x0 (cell-x w 0)) (y0 (cell-y w 0)))
    (when (and (>= x x0) (>= y y0))
      (let ((row (%quotient (- y y0) (line-height))) (col (%quotient (- x x0) (char-cell-width))))
        (when (and (< row (length rm)) (< col (term-cols tm)) (aref rm row))
          (let* ((entry (aref rm row)) (serial (car entry)) (c (+ (cdr entry) col)) (best nil))
            (dolist (p (listener-records w))
              (when (and (= (aref p 1) serial) (<= (aref p 2) c) (< c (aref p 3))
                         (or (null best) (< (- (aref p 3) (aref p 2)) (- (aref best 3) (aref best 2)))))
                (setq best p)))
            best))))))

(defmethod presentation-rects ((w listener-window) p)
  (let* ((tm (listener-term w)) (rm (term-rowmap tm)) (cols (term-cols tm)) (rects nil))
    (dotimes (row (length rm))
      (let ((e (aref rm row)))
        (when (and e (= (car e) (aref p 1)))
          (let ((s (max (aref p 2) (cdr e))) (end (min (aref p 3) (+ (cdr e) cols))))
            (when (< s end)
              (push (list (cell-x w (- s (cdr e))) (cell-y w row)
                          (* (- end s) (char-cell-width)) (line-height))
                    rects))))))
    rects))

;;; ---- commands --------------------------------------------------------------------
;;;
;;; #(:command name argument-types documentation function output-p)
;;; An argument type is a class name, or the name of a predicate. A command
;;; with one argument applies to any presented object of that type. OUTPUT-P
;;; commands run in a Listener's own process, so their output goes there.

(defvar *commands* nil)

(defun command-name (c) (aref c 1))
(defun command-types (c) (aref c 2))
(defun command-documentation (c) (aref c 3))
(defun command-function (c) (aref c 4))
(defun command-output-p (c) (aref c 5))

;; the name as typed: "Show Definition" -> "Show-Definition"
(defun command-typed-name (c)
  (let ((s (command-name c)) (r ""))
    (dotimes (i (length s))
      (setq r (string-append r (if (eq (aref s i) #\Space) "-" (string (aref s i))))))
    r))

(defmacro define-command (name args options documentation &body body)
  `(%define-command ,name ',(mapcar #'second args) ,documentation
                    (lambda ,(mapcar #'first args) ,@body)
                    ,(plist-value options :output)))

(defun %define-command (name types documentation function output)
  (let ((c (vector :command name types documentation function output)))
    (setq *commands* (append (remove-if (lambda (old) (string= (command-name old) name)) *commands*)
                             (list c)))
    name))

(defun find-command (name)
  (dolist (c *commands*) (when (string= (command-name c) name) (return c))))

;; a keyword type (:integer, :form) is for typed commands only: no presentation has it
(defun type-matches (object type)
  (if (keywordp type)
      nil
  (if (find-class type nil) (typep object type) (and (fboundp type) (funcall type object)))))

(defun applicable-commands (object)
  (remove-if-not (lambda (c) (and (= (length (command-types c)) 1)
                                  (type-matches object (car (command-types c)))))
                 *commands*))

;; the Listener that should run a command started from window W
(defun command-listener (w)
  (if (typep w 'listener-window)
      w
      (dolist (x *windows*) (when (typep x 'listener-window) (return x)))))

;; Run command C on ARGS for a click in window W.
(defun run-command (c args w)
  (let ((l (command-listener w)))
    (if (and (command-output-p c) l)
        (window-key l (list :command (command-function c) args))   ; queued for its process
        (handler-case (apply (command-function c) args)
          (error (e) (setq *last-key* (string-append "error: " (condition-message e))))))))

;; in the Listener's process: (:command function args) from its input queue
(defun run-listener-command (item)
  (fresh-line)
  (let ((result (handler-case (list (apply (second item) (third item)))
                  (error (c) (report-error c) nil))))
    (when (and result (car result) (not (eq (car result) '%no-value)))
      (fresh-line)
      (present (car result))
      (terpri)))
  (fresh-line))

;;; ---- what the window system asks about presentations ----------------------------

(defun short-printed (object n)
  (let ((s (cond ((debug-frame-p object) (string-append "frame " (princ-to-string (aref object 2))))
                 ((restartp object) (string-append "restart: " (restart-report object)))
                 ((peek-mode-object-p object) (string-append "the " (string-downcase (string-trim-colon (symbol-name (aref object 1)))) " view"))
                 (t (prin1-to-string object)))))
    (if (> (length s) n) (string-append (substring s 0 (- n 3)) "...") s)))

(defun presentation-documentation (w p)
  (let* ((object (aref p 0))
         (names (mapcar #'command-name (applicable-commands object))))
    (if (eq (presentation-action p) :back)
        "Mouse-L: back to the object inspected before."
        (string-append "Mouse-L: " (command-name (click-command object)) " " (short-printed object 30) ".   Mouse-R: "
                       (if (cdr names)
                           (reduce (lambda (a b) (string-append a ", " b)) (cdr names))
                           "commands")))))

(defun presentation-action (p) (if (= (length p) 6) (aref p 5) nil))

;; what Mouse-L does to a presentation, by type: (predicate . command name); else Inspect
(defvar *click-commands* nil)

(defun click-command (object)
  (or (dolist (c *click-commands*) (when (funcall (car c) object) (return (find-command (cdr c)))))
      (find-command "Inspect")))

(defun presentation-click (w p)
  (if (eq (presentation-action p) :back)
      (inspector-back w)
      (run-command (click-command (aref p 0)) (list (aref p 0)) w)))

(defun presentation-menu-items (w p)
  (let ((object (aref p 0)))
    (mapcar (lambda (c) (cons (command-name c) (lambda (ignore) (run-command c (list object) w))))
            (applicable-commands object))))

;;; ---- the command processor --------------------------------------------------------

(defun command-line-p (s) (and (> (length s) 0) (eq (aref s 0) #\:)))

(defun position-of (char string)
  (let ((i 0) (n (length string)) (r nil))
    (while (and (< i n) (null r))
      (when (eq (aref string i) char) (setq r i))
      (setq i (1+ i)))
    r))

(defun prefix-p (prefix string)
  (and (<= (length prefix) (length string))
       (string= (string-upcase prefix) (string-upcase (substring string 0 (length prefix))))))

;; ":ins" => (":Inspect " "Inspect"); several matches complete as far as they agree
(defun complete-command-name (text)
  (let* ((prefix (substring text 1))
         (names (mapcar #'command-typed-name *commands*))
         (matches (remove-if-not (lambda (n) (prefix-p prefix n)) names)))
    (cond ((null matches) (cons text nil))
          ((null (cdr matches)) (cons (string-append ":" (car matches) " ") matches))
          (t (let ((common (car matches)))
               (dolist (m (cdr matches))
                 (while (not (prefix-p common m)) (setq common (substring common 0 (1- (length common))))))
               (cons (if (> (length common) (length prefix)) (string-append ":" common) text) matches))))))

(defun find-typed-command (name)
  (let ((exact nil) (prefixed nil))
    (dolist (c *commands*)
      (when (string= (string-upcase (command-typed-name c)) (string-upcase name)) (setq exact c))
      (when (prefix-p name (command-typed-name c)) (push c prefixed)))
    (or exact (and prefixed (null (cdr prefixed)) (car prefixed)))))

;; Run a command line like ":Inspect (list 1 2)": the arguments are evaluated.
(defun %run-command-line (line)
  (let* ((text (substring line 1))
         (space (position-of #\Space text))
         (name (if space (substring text 0 space) text))
         (rest (if space (substring text space) ""))
         (c (find-typed-command name))
         (args nil) (pos 0))
    (unless c (error "Unknown command (Help lists them):" name))
    (loop
      (let ((r (%read-from-string rest pos)))
        (if (consp r)
            (progn (push (eval (car r)) args) (setq pos (cdr r)))
            (return nil))))
    (setq args (nreverse args))
    (unless (= (length args) (length (command-types c)))
      (error (string-append (command-name c) " takes " (princ-to-string (length (command-types c)))
                            " argument(s), not") (length args)))
    (or (apply (command-function c) args) '%no-value)))

(defun show-commands ()
  (princ "Commands (type :Name and arguments; Tab completes the name):") (terpri)
  (dolist (c *commands*)
    (princ "  :") (princ (command-typed-name c))
    (dotimes (i (max 1 (- 18 (length (command-typed-name c))))) (princ " "))
    (princ (command-documentation c)) (terpri))
  (princ "Mouse-L on a presented value inspects it; Mouse-R gives its commands.") (terpri)
  nil)

;;; ---- a small pretty printer, for definitions --------------------------------------

(defvar *pp-width* 64)

(defun pp (x indent)
  (let ((flat (prin1-to-string x)))
    (if (or (not (consp x)) (not (proper-list-p x)) (<= (+ indent (length flat)) *pp-width*))
        (princ flat)
        (let* ((head (prin1-to-string (car x))) (inner (+ indent 2)) (args (cdr x)))
          (princ "(") (princ head)
          (when (and args (memq (car x) '(defun defmacro defmethod lambda let let* when unless
                                          dolist dotimes if while catch block handler-case
                                          defclass with-lock)))
            (princ " ")
            (pp (car args) (+ indent 2 (length head)))
            (setq args (cdr args)))
          (dolist (a args)
            (terpri)
            (dotimes (i inner) (princ " "))
            (pp a inner))
          (princ ")")))))

(defun pretty-print (x) (fresh-line) (pp x 0) (terpri) nil)

(defun function-name-p (x) (and (symbolp x) x (fboundp x)))
(defun compiled-code-p (x)
  (or (compiled-function-p x) (and (function-name-p x) (compiled-function-p (symbol-function x)))))

(defun show-definition (name)
  (let ((source (get name 'source)))
    (if source
        (pretty-print source)
        (progn (fresh-line)
               (princ "No source is recorded for ") (prin1 name)
               (princ ": it was defined when the world was built; :Edit-Definition shows it.") (terpri)))
    nil))

;;; ---- the Inspector ------------------------------------------------------------------

(defclass inspector-window (label-mixin border-mixin window)
  ((object :initform nil :accessor inspector-object)
   (history :initform nil :accessor inspector-history)
   (records :initform nil :accessor inspector-records)))  ; #(object x y width height action)

;; the parts of an object, as (label . value)
(defun inspector-parts (x)
  (cond ((instancep x)
         (mapcar (lambda (s) (cons (symbol-name s) (if (slot-boundp x s) (slot-value x s) '|unbound|)))
                 (class-slot-names (class-of x))))
        ((classp x)
         (list (cons "Name" (class-name x))
               (cons "Superclasses" (class-direct-superclasses x))
               (cons "Precedence" (class-precedence-list x))
               (cons "Slots" (class-slot-names x))))
        ((hash-table-p x)                       ; the entries, keys as labels
         (append (list (cons "Test" (hash-table-test x)) (cons "Count" (hash-table-count x)))
                 (let ((r nil) (i 0))
                   (maphash (lambda (k v) (when (< i 200) (push (cons (short-printed k 24) v) r)) (setq i (1+ i))) x)
                   (sort r #'string< :key #'car))))
        ((packagep x)                           ; M12
         (append (list (cons "Name" (package-name x)) (cons "Nicknames" (package-nicknames x))
                       (cons "Uses" (package-use-list x)) (cons "Used by" (package-used-by-list x)))
                 (if (eq x (find-package "COMMON-LISP"))
                     (list (cons "Symbols" "the whole system"))
                     (let ((ext nil) (int nil))
                       (dolist (s (%package-symbols x nil nil))
                         (if (eq (nth-value 1 (find-symbol (symbol-name s) x)) :external) (push s ext) (push s int)))
                       (list (cons "External" (sort ext #'string< :key #'symbol-name))
                             (cons "Internal" (sort int #'string< :key #'symbol-name)))))))
        ((floatp x)
         (multiple-value-bind (m e sign) (integer-decode-float x)
           (list (cons "Value" x) (cons "Significand" m) (cons "Exponent (base 2)" e) (cons "Sign" sign))))
        ((processp x)
         (list (cons "Name" (process-name x)) (cons "State" (process-state x))
               (cons "Whostate" (process-whostate x))))
        ((conditionp x)
         (list (cons "Type" (condition-type x)) (cons "Message" (condition-message x))
               (cons "Irritants" (condition-irritants x))))
        ((and (symbolp x) x)
         (append (list (cons "Name" (symbol-name x)) (cons "Package" (symbol-package x)))
                 (if (boundp x) (list (cons "Value" (symbol-value x))) (list (cons "Value" '%unbound)))
                 (if (fboundp x) (list (cons "Function" (symbol-function x))) nil)
                 (list (cons "Properties" (symbol-plist x)))))
        ((compiled-function-p x)
         (let ((parts (%code-parts x)))
           (list (cons "Name" (third parts)) (cons "Constants" (second parts))
                 (cons "Bytecode bytes" (length (first parts))))))
        ((and (consp x) (proper-list-p x))
         (let ((i 0) (r nil))
           (dolist (e x) (when (< i 200) (push (cons (princ-to-string i) e) r)) (setq i (1+ i)))
           (nreverse r)))
        ((consp x) (list (cons "Car" (car x)) (cons "Cdr" (cdr x))))
        ((stringp x) (list (cons "Length" (length x))))
        ((vectorp x)
         (let ((r nil)) (dotimes (i (min 200 (length x))) (push (cons (princ-to-string i) (aref x i)) r)) (nreverse r)))
        ((integerp x)
         (list (cons "Decimal" (princ-to-string x)) (cons "Character" (if (and (>= x 0) (< x #x110000) (graphic-char-p (code-char x))) (code-char x) nil))))
        (t nil)))

(defun truncate-to-width (s font width)
  (if (<= (string-width s font) width)
      s
      (let ((n (length s)))
        (while (and (> n 0) (> (string-width (string-append (substring s 0 n) "...") font) width))
          (setq n (1- n)))
        (string-append (substring s 0 n) "..."))))

;; draw OBJECT's printed form at X, Y and record it; => the x after it
(defun inspector-present (w object x y font right &optional action label)
  (let* ((text (truncate-to-width (or label (prin1-to-string object)) font (- right x)))
         (end (draw-string text x y font alu-ior (window-bitmap w))))
    (push (vector object x (- y 1) (- end x) 11 action) (inspector-records w))
    end))

(defmethod draw-contents ((w inspector-window))
  (setf (inspector-records w) nil)
  (let* ((r (window-inside w)) (b (window-bitmap w))
         (x (+ (first r) 4)) (y (+ (second r) 4))
         (right (+ (first r) (third r) -4)) (bottom (+ (second r) (fourth r) -14))
         (object (inspector-object w)))
    (inspector-present w object x y *font-bold* right)
    (setq y (+ y 13))
    (let ((after (draw-string "an instance of " x y *font-variable* alu-ior b)))
      (inspector-present w (class-of object) after y *font-variable* right nil
                         (symbol-name (class-name (class-of object)))))
    (setq y (+ y 14))
    (draw-rectangle x y (- right x) 1 alu-seta 255 b)
    (setq y (+ y 4))
    (let* ((parts (inspector-parts object)) (shown 0)
           (column (+ x 8 (reduce #'max (cons 30 (mapcar (lambda (p) (string-width (string-append (car p) ":") *font-variable*))
                                                          parts))))))
      (dolist (part parts)
        (when (< y bottom)
          (draw-string (string-append (car part) ":") x y *font-variable* alu-ior b)
          (inspector-present w (cdr part) (min column (- right 40)) y *font-fixed* right)
          (setq y (+ y 12) shown (1+ shown))))
      (when (< shown (length parts))
        (draw-string (string-append "... and " (princ-to-string (- (length parts) shown)) " more")
                     x y *font-variable* alu-ior b)))
    (when (inspector-history w)
      (inspector-present w (car (inspector-history w)) x (+ bottom 1) *font-variable* right :back
                         (string-append "Back to " (short-printed (car (inspector-history w)) 40))))))

(defmethod window-presentation-at ((w inspector-window) x y)
  (let ((best nil))
    (dolist (p (inspector-records w))
      (when (and (>= x (aref p 1)) (< x (+ (aref p 1) (aref p 3)))
                 (>= y (aref p 2)) (< y (+ (aref p 2) (aref p 4))))
        (setq best p)))
    best))

(defmethod presentation-rects ((w inspector-window) p)
  (list (list (aref p 1) (aref p 2) (aref p 3) (aref p 4))))

(defmethod mouse-documentation ((w inspector-window) x y)
  "Inspector: Mouse-L on a value inspects it, Mouse-R gives its commands.   Mouse-R elsewhere: system menu.")

(defun find-inspector ()
  (dolist (w *windows*) (when (typep w 'inspector-window) (return w))))

;; Show OBJECT in the Inspector, making one if needed. => OBJECT
(defun inspect (object)
  (let ((w (find-inspector)))
    (if w
        (progn
          (push (inspector-object w) (inspector-history w))
          (when (> (length (inspector-history w)) 50)
            (setf (cdr (nthcdr 49 (inspector-history w))) nil))
          (setf (inspector-object w) object)
          (window-refresh w)
          (expose-window w))
        (progn
          (setq w (make-instance 'inspector-window :label "Inspector"
                                 :x (max 0 (- (%screen-width) 340)) :y 20
                                 :width (min 330 (%screen-width)) :height (min 300 (- (desktop-height) 30))))
          (setf (inspector-object w) object)
          (add-window w nil)))
    object))

(defun inspector-back (w)
  (when (inspector-history w)
    (setf (inspector-object w) (pop (inspector-history w)))
    (window-refresh w)
    (window-changed w)))

;;; ---- the commands --------------------------------------------------------------------

(define-command "Inspect" ((object t)) ()
  "Show the object and its parts in the Inspector."
  (inspect object)
  nil)

(define-command "Describe" ((object t)) (:output t)
  "Describe the object."
  (describe object))

(define-command "Show Definition" ((name function-name-p)) (:output t)
  "Print the source of a function defined in the Listener."
  (show-definition name))

(define-command "Disassemble" ((f compiled-code-p)) (:output t)
  "Show the bytecode of a compiled function."
  (disassemble f))

(define-command "Use as Input" ((object t)) ()
  "Type the object into the Listener, to use in the next form."
  (let ((l (command-listener *selected-window*))
        (text (if (readable-p object)
                  (if (or (consp object) (and (symbolp object) object (not (keywordp object))))
                      (string-append "'" (prin1-to-string object))
                      (prin1-to-string object))
                  (string-append "(presentation-object " (princ-to-string (remember-object object)) ")"))))
    (when l
      (dotimes (i (length text))
        (window-key l (vector ev-key-down (char-code (aref text i)) 0 0))))
    nil))

(defun readable-p (x)
  (cond ((or (null x) (integerp x) (stringp x) (characterp x) (symbolp x)) t)
        ((consp x) (and (proper-list-p x) (every #'readable-p x)))
        (t nil)))

(define-command "Clear Listener" () (:output t)
  "Clear this Listener's text."
  (clear-listener))

(define-command "Help" () (:output t)
  "List the commands."
  (show-commands))

(define-command "Processes" () (:output t)
  "List the processes."
  (processes)
  nil)

(define-command "Room" () (:output t)
  "Show how much of the heap is in use."
  (room))

(define-command "Demo" () ()
  "Open the clock, bouncing ball and prime finder windows."
  (demo)
  nil)

(define-command "Stop Demo" () ()
  "Close the demo windows."
  (stop-demo))

;;; ---- finding things on the screen (used by the tests, handy for demos) ---------

(defun window-presentation-list (w)
  (cond ((typep w 'listener-window) (listener-records w))
        ((typep w 'inspector-window) (inspector-records w))
        ((typep w 'peek-window) (peek-records w))           ; peek.lisp
        (t nil)))

;; the screen position of the newest presentation in W whose object passes TEST
(defun presentation-screen-point (w test)
  (let ((p (dolist (r (window-presentation-list w)) (when (funcall test (aref r 0)) (return r)))))
    (when p
      (let ((rect (car (presentation-rects w p))))
        (when rect
          (list (+ (window-x w) (first rect) 2) (+ (window-y w) (second rect) 3)))))))

(defun menu-item-index (label)
  (let ((i 0))
    (dolist (item (aref *menu* 0))
      (when (string= (car item) label) (return i))
      (setq i (1+ i)))))

(defun string-search (needle haystack)
  (let ((n (length needle)) (i 0) (found nil))
    (while (and (not found) (<= (+ i n) (length haystack)))
      (when (string= needle (substring haystack i (+ i n))) (setq found i))
      (setq i (1+ i)))
    found))

(defun transcript-contains (w text)
  (let ((tm (listener-term w)))
    (or (some (lambda (line) (string-search text line)) (term-lines tm))
        (string-search text (apply #'string-append (reverse (term-chunks tm)))))))
