(in-package :cl-fzy)
(defconstant +score-match-slash+ 0.9) ; Define appropriate values for each constant
(defconstant +score-match-word+ 0.8)
(defconstant +score-match-dot+ 0.6)
(defconstant +score-match-capital+ 0.7)

(defun assign-lower (v)
  (let ((arr (make-array 26 :initial-element v)))
    (dotimes (i 26)
      (setf (aref arr i) (code-char (+ (char-code #\a) i))))
    arr))

(defun assign-upper (v)
  (let ((arr (make-array 26 :initial-element v)))
    (dotimes (i 26)
      (setf (aref arr i) (code-char (+ (char-code #\A) i))))
    arr))

(defun assign-digit (v)
  (let ((arr (make-array 10 :initial-element v)))
    (dotimes (i 10)
      (setf (aref arr i) (code-char (+ (char-code #\0) i))))
    arr))

(defparameter *bonus-states*
  (list (make-array 256 :initial-element 0)
    (let ((arr (make-array 256 :initial-element 0)))
      (setf (aref arr (char-code #\/)) +score-match-slash+
        (aref arr (char-code #\-)) +score-match-word+
        (aref arr (char-code #\_)) +score-match-word+
        (aref arr (char-code #\Space)) +score-match-word+
        (aref arr (char-code #\.)) +score-match-dot+)
      arr)
    (let ((arr (make-array 256 :initial-element 0)))
      (setf (aref arr (char-code #\/)) +score-match-slash+
        (aref arr (char-code #\-)) +score-match-word+
        (aref arr (char-code #\_)) +score-match-word+
        (aref arr (char-code #\Space)) +score-match-word+
        (aref arr (char-code #\.)) +score-match-dot+)
      (loop for ch across (concatenate 'string (assign-upper +score-match-capital+)
                            (assign-lower +score-match-capital+))
        do (setf (aref arr (char-code ch)) +score-match-capital+))
      arr)))

(defparameter *bonus-index*
  (let ((arr (make-array 256 :initial-element 0)))
    (loop for ch across (concatenate 'string (assign-upper 2)
                          (assign-lower 1)
                          (assign-digit 1))
      do (setf (aref arr (char-code ch)) (if (upper-case-p ch) 2 1)))
    arr))

(defun compute-bonus (last-ch ch)
  (aref (nth (aref *bonus-index* (char-code ch)) *bonus-states*)
    (char-code last-ch)))



;; working:
;; constants ..

(defconstant +score-gap-leading+ -0.005)
(defconstant +score-gap-trailing+ -0.005)
(defconstant +score-gap-inner+ -0.01)
(defconstant +score-match-consecutive+ 1.0)
(defconstant +score-match-slash+ 0.9)
(defconstant +score-match-word+ 0.8)
(defconstant +score-match-capital+ 0.7)
(defconstant +score-match-dot+ 0.6)

(defconstant +key-timeout+ 25)
;; (defconstant +default-tty+ "/dev/tty")
;; (defconstant +default-prompt+ "> ")
(defconstant +default-num-lines+ 10)
(defconstant +default-workers+ 0)
(defconstant +default-show-info+ 0)
(defparameter +score-match-lower+ 1)
                                        ;working
;; (defvar *bonus-states*
;;   (let ((arr (make-array '(3 256) :initial-element 0)))
;;     (loop for i from 0 to 255 do
;;       (cond ((= i 32) (progn (setf (aref arr 2 i) 0.8)(setf (aref arr 1 i) 0.8)))
;;         ((= i 45) (progn (setf (aref arr 2 i) 0.8)(setf (aref arr 1 i) 0.8)))
;;         ((= i 46) (progn (setf (aref arr 2 i) 0.6)(setf (aref arr 1 i) 0.6)))
;;         ((= i 47) (progn (setf (aref arr 2 i) 0.9)(setf (aref arr 1 i) 0.9)))
;;         ((= i 95) (progn (setf (aref arr 2 i) 0.8)(setf (aref arr 1 i) 0.8)))
;;         ((and (<= 97 i) (<= i 122))
;;           (setf (aref arr 2 i) 0.7))
;;         (t nil)))
;;     arr))

                                        ;working
;; (defvar *bonus-index*
;;   (let ((arr (make-array 256 :initial-element 0)))
;;     (loop for i below 256
;;       do (cond ((and (>= 48 i) (<= i 57))
;;                  (setf (aref arr i) 1))
;;            ((and (>= 65 i) (<= i 90))
;;              (setf (aref arr i) 2))
;;            ((and (>= 97 i) (<= i 122))
;;              (setf (aref arr i) 1))
;;            (t (setf (aref arr i) 0)))
;;       )arr))

;; (loop for i below 256
;;   do (print (format t "~d: ~d \n" i (aref *bonus-index* i))))
;; (loop for i from 0 to 2 do
;;   (loop for j from 0 to 255 do
;;     (print (format t "~d ~d: ~d \n" i j (aref bonusstates i j)))))

                                        ;working
;; (defun compute-bonus (last-ch ch)
;;   ;;   ;; (print (aref *bonus-index* (char-code ch)))
;;   ;;   ;; (print (char-code last-ch))
;;   ;;   ;; (print (char-code ch))
;;   ;;   ;; (print "WTF")
;;   ;; (format t "last-ch: ~a, ch: ~a~&" last-ch ch)
;;   (let ((dim-1 (aref *bonus-index* (if (characterp ch) (char-code ch) ch)))
;;          (dim-2 (if (characterp last-ch) (char-code last-ch) last-ch)))
;;     (print (format t "dim-1: ~a, dim-2: ~a~% : ~a~%" dim-1 dim-2 (aref *bonus-states* dim-1 dim-2)))
;;     ;; (format t "\ndim-1: ~a~%, dim-2: ~a~%" dim-1 dim-2)
;;     (aref *bonus-states* dim-1 dim-2)))
;; (print (aref *bonus-states* (aref *bonus-index* (char-code ch)) (char-code last-ch)))
;; (aref *bonus-states* (aref *bonus-index* (char-code ch)) (char-code last-ch)))
;; (aref (aref *bonus-states* (aref *bonus-index* (char-code ch)) (char-code last-ch)) ))

(defun greet (&optional (name "cbadger"))
  (format t "Hello ~a from ~a!~&" name "cl-fzy"))

(defun help ()
  (format t "~&Usage:

  cl-fzy [name]~&"))

(defun %main (argv)
  "Parse CLI args."
  (when (member "-h" argv :test #'equal)
    ;; To properly parse command line arguments, use a third-party library such as
    ;; clingon, unix-opts, defmain, adopt… when needed.
    (help)
    (uiop:quit))
  (greet  (or (first argv)
            "dear lisp user")))

(defun main ()
  "Entry point for the executable.
  Reads command line arguments."
  ;; uiop:command-line-arguments returns a list of arguments (sans the script name).
  ;; We defer the work of parsing to %main because we call it also from the Roswell script.
  (progn
    ;; (cl-fzy::has-match "a" "a")
    ;; (make-instance 'cl-fzy::choices)
    ;; (make-instance 'cl-fzy::options))
    (let ((default-options (make-instance 'cl-fzy::options))
           (choices (make-instance 'cl-fzy::choices)))
      (cl-fzy::choices-init choices default-options)
      ;; (cl-fzy::choices-add choices "tagsiouso")

      ;; (cl-fzy::choices-add *choices* "tatagsiouso")
      ;; (cl-fzy::choices-add *choices* "gstagsiouso")
      ;; (cl-fzy::choices-add *choices* "iotagssss")
      ;; (cl-fzy::choices-add *choices* "uooaostag")
      ;; (cl-fzy::choices-add *choices* "stagatago")
      ;; (cl-fzy::choices-add *choices* "agtatatag")
      ;; ;; (cl-fzy::choices-add *choices* "taggggi")
      ;; ;; Empty search
      ;; (cl-fzy::choices-search *choices* "tag")

      ;; (defvar *default-options* (make-instance 'cl-fzy::options))
      ;; (defvar *choices* (make-instance 'cl-fzy::choices))

      ;; ;; *choices*
      ;; ;; *default-options*
      (progn
        (cl-fzy::choices-init choices default-options)
        (setf (choices-worker-count choices) 4)
        ;; (choices-add *choices* "test")
        ;; (choices-add *choices* "fouox")
        ;; (choices-add *choices* "barxxux")
        ;; (choices-add *choices* "bauxz")
        ;; (choices-add *choices* "qux")
        ;; (choices-add *choices* "uxfofoo")
        ;; (choices-add *choices* "babarux")
        ;; (choices-add *choices* "babaz")
        ;; (choices-add *choices* "ququx")
        ;; (choices-search *choices* "ux")
        ;; (let ((N 100000) strings)
        (time (let ((N 129000))
                (dotimes (i N)
                  (choices-add choices (format nil "~A" i) ))
                ;; (mapc #'(lambda (str) (cl-fzy::choices-add choices str)) strings)
                (choices-search choices "");; (cl-fzy::choices-search choices "12")
                )))

      ;; *choices*)
      ;; (multi-search "ba" *choices* 4)
      ;; (cl-fzy::choices-search *choices* "")
      (format t "Available choices ~a~%" (cl-fzy::choices-available choices))
      ;; (format t "~a~%" *choices*)
      ;; (cl-fzy::choices-search *choices* "t")
      ;; (is (= 1 (cl-fzy::choices-available *choices*)))
      ;; (is (= 0 (cl-fzy::choices-selection *choices*)))

      ;; (cl-fzy::choices-destroy *choices*)
      ))
  (%main (uiop:command-line-arguments)))
