;;; cl5.lisp -- tests for M15: characters beyond ASCII (Unicode characters
;;; and strings, case in other scripts, UTF-8 text, the reader and printer).
;;;
;;; The first part is the done-test: a small program in ordinary Common Lisp
;;; style that works with text in several languages. It counts the words of
;;; a multilingual text in a hash table, ignoring case, keeps names with
;;; accents and Greek and Cyrillic letters, prints a table with FORMAT, and
;;; reads its own output back.

;;; ---- the program -----------------------------------------------------------------

(defpackage :wörter (:use :common-lisp) (:export #:count-words #:word-table #:initials))
(in-package :wörter)

(defparameter *text*
  "Smörgåsbord och SMÖRGÅSBORD. Straße, straße! Ελλάδα και ελλάδα; Москва, МОСКВА — Łódź łódź Łódź.")

;; the words of TEXT: runs of letters, in lower case
(defun words (text)
  (let ((words nil) (start nil))
    (dotimes (i (1+ (length text)))
      (let ((c (if (< i (length text)) (char text i) #\Space)))
        (cond ((alpha-char-p c) (unless start (setq start i)))
              (start (push (string-downcase (subseq text start i)) words)
                     (setq start nil)))))
    (nreverse words)))

(defun count-words (text)
  (let ((counts (make-hash-table :test #'equal)))
    (dolist (w (words text)) (incf (gethash w counts 0)))
    (sort (loop for w being the hash-keys of counts using (hash-value n) collect (cons w n))
          (lambda (a b) (or (> (cdr a) (cdr b))
                            (and (= (cdr a) (cdr b)) (string< (car a) (car b))))))))

(defun word-table (text)
  (with-output-to-string (s)
    (dolist (e (count-words text))
      (format s "~12A ~2D ~A~%" (car e) (cdr e) (string-upcase (car e))))))

(defun initials (names)
  (map 'string (lambda (n) (char-upcase (char n 0))) names))

(in-package :cl-user)

(check (wörter:count-words wörter::*text*)
       '(("łódź" . 3) ("smörgåsbord" . 2) ("straße" . 2) ("ελλάδα" . 2) ("москва" . 2) ("och" . 1)
         ("και" . 1)))
(check (wörter:word-table "Αθήνα αθήνα Ödön")
       (format nil "αθήνα         2 ΑΘΉΝΑ~%ödön          1 ÖDÖN~%"))
(check (wörter:initials '("ärla" "ωμέγα" "яблоко" "ĳssel")) "ÄΩЯĲ")
(check (symbol-name 'wörter:count-words) "COUNT-WORDS")
(check (package-name (symbol-package 'wörter:initials)) "WÖRTER")
(check (read-from-string (prin1-to-string (wörter:count-words "Já já, JÁ!"))) '(("já" . 3)))

;;; ---- characters -------------------------------------------------------------------

(check (char-code #\é) 233)
(check (char-code #\λ) 955)
(check (code-char 8364) #\€)
(check (char-code (code-char #x1F600)) #x1F600)
(check (list #\U+00E9 #\u00e9 #\U+20AC) '(#\é #\é #\€))
(check (char-upcase #\ä) #\Ä)
(check (char-downcase #\Ж) #\ж)
(check (char-upcase #\ω) #\Ω)
(check (char-upcase #\ς) #\ς)                 ; final sigma: no upper case of its own
(check (char-upcase #\ß) #\ß)
(check (char-upcase #\ÿ) #\Ÿ)
(check (char-downcase #\Ÿ) #\ÿ)
(check (char-upcase #\ł) #\Ł)
(check (char-upcase #\ž) #\Ž)
(check (list (upper-case-p #\É) (lower-case-p #\é) (upper-case-p #\é)) '(t t nil))
(check (list (alpha-char-p #\ß) (alpha-char-p #\λ) (alpha-char-p #\→) (alpha-char-p #\1)) '(t t nil nil))
(check (list (both-case-p #\ß) (both-case-p #\Я) (both-case-p #\中)) '(nil t nil))
(check (alpha-char-p #\中) t)
(check (list (graphic-char-p #\é) (graphic-char-p (code-char 133)) (graphic-char-p #\Space)) '(t nil t))
(check (char-equal #\Ä #\ä) t)
(check (char< #\a #\é #\λ) t)
(check (alphanumericp #\ñ) t)
(check (digit-char-p #\٣) nil)               ; digits are ASCII digits
(check (char-name #\Space) "Space")
(check (char-name (code-char 133)) "U+0085")
(check (char-name #\é) nil)
(check (name-char "U+03BB") #\λ)
(check (name-char "linefeed") #\Newline)
(check (prin1-to-string (code-char 133)) "#\\U+0085")
(check (prin1-to-string #\é) "#\\é")
(check (princ-to-string #\λ) "λ")
(check (typep #\é 'base-char) t)
(check (typep #\λ 'base-char) nil)
(check (standard-char-p #\é) nil)

;;; ---- strings -----------------------------------------------------------------------

(check (length "naïve") 5)
(check (length "λx→y") 4)
(check (char "Ελλάδα" 3) #\ά)
(check (string-upcase "straße café λόγια") "STRAßE CAFÉ ΛΌΓΙΑ")
(check (string-downcase "ÅÄÖ ПРИВЕТ") "åäö привет")
(check (string-capitalize "élan VITAL über") "Élan Vital Über")
(check (string-upcase "ÿ") "Ÿ")                       ; a narrow string, a wide result
(check (string= "é" (string #\é)) t)
(check (equal "aλ" (concatenate 'string "a" "λ")) t)
(check (string-equal "ÉTÉ" "été") t)
(check (reverse "αβγ") "γβα")
(check (subseq "Ελλάδα" 1 3) "λλ")
(check (search "ß" "Straße") 4)
(check (position #\λ "abλc") 2)
(check (remove #\é "été") "t")
(check (substitute #\e #\é "été") "ete")
(check (coerce '(#\д #\а) 'string) "да")
(check (coerce "да" 'list) '(#\д #\а))
(check (map 'string #'char-upcase "ñandú") "ÑANDÚ")
(check (string-trim " " "  ü  ") "ü")
(check (let ((s (make-string 3 :initial-element #\a))) (setf (char s 1) #\λ) s) "aλa")
(check (let ((s (copy-seq (make-string 2 :initial-element #\x)))) (setf (char s 0) #\→) s) "→x")
(check (let ((v (make-array 0 :element-type 'character :adjustable t :fill-pointer 0)))
         (dolist (c '(#\Ж #\и #\з #\н #\ь)) (vector-push-extend c v))
         (coerce v 'string))
       "Жизнь")
(check (handler-case (let ((s (make-string 1 :element-type 'base-char))) (setf (char s 0) #\λ) :stored)
         (error () :too-wide))
       :too-wide)
(check (typep "abc" 'base-string) t)
(check (typep (make-string 2) 'base-string) nil)          ; any character fits in it
(check (stringp (make-string 2)) t)
(check (format nil "~A|~5A|~:@(~A~)" "é" "λ" "über") "é|λ    |ÜBER")
(check (format nil "~S" "ƒ\"") "\"ƒ\\\"\"")
(check (format nil "~C" #\€) "€")
(check (with-output-to-string (s) (write-char #\ñ s) (write-string "→" s)) "ñ→")
(check (let ((h (make-hash-table :test #'equal)))
         (setf (gethash "λ" h) 1)
         (gethash (string #\λ) h))
       1)
(check (let ((h (make-hash-table :test #'equal)) (wide (make-string 2 :initial-element #\é)))
         (setf (gethash "éé" h) :yes)
         (gethash wide h))
       :yes)
(check (sxhash (make-string 1 :initial-element #\a)) (sxhash "a"))

;;; ---- the reader and printer ---------------------------------------------------------

(check (read-from-string "λ") 'Λ)
(check (symbol-name (read-from-string "straße")) "STRAßE")
(check (multiple-value-list (read-from-string "(é λ) rest")) '((É Λ) 6))
(check (multiple-value-list (read-from-string "αβ γ" t nil :start 3)) '(Γ 4))
(check (prin1-to-string "a\"λ") "\"a\\\"λ\"")
(check (prin1-to-string '(#\é "ü" ж)) "(#\\é \"ü\" Ж)")
(check (read-from-string (prin1-to-string (list #\U+0085 #\λ "€"))) (list (code-char 133) #\λ "€"))
(check (let ((s (make-string-input-stream "première ligne
δεύτερη"))) (list (read-line s) (read-line s))) '("première ligne" "δεύτερη"))
(check (with-input-from-string (s "(ä . ö)") (read s)) '(Ä . Ö))
(check (eval (read-from-string "(let ((λ 2) (μ 3)) (* λ μ))")) 6)
(defun σ (&rest xs) (apply #'+ xs))
(check (σ 1 2 3) 6)
(check (funcall (intern "Σ") 4) 4)
(check (find-symbol "Σ") 'σ)
(check (eq (intern "ÄÖ") (intern (string-upcase "äö"))) t)

;;; ---- UTF-8 ---------------------------------------------------------------------------

(check (map 'list #'char-code (%utf8-encode "aé€😀"))
       '(97 195 169 226 130 172 240 159 152 128))
(check (%utf8-decode (%utf8-encode "Grüße, Κόσμε, Мир")) "Grüße, Κόσμε, Мир")
(check (%utf8-decode (coerce (list (code-char 233) #\a) 'string)) "éa")   ; not UTF-8: Latin-1
(check (length (%utf8-encode "日本")) 6)

(report "cl5")
