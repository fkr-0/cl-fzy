(in-package :cl-fzy)

;; constants ..

(defvar *bonus-states*
  (make-array '(3 256) :initial-element 0
              :displaced-to (make-array 256 :element-type 'score-t)
              :displaced-index-offset 0))

(setf (aref *bonus-states* 1 ?/) SCORE-MATCH-SLASH
      (aref *bonus-states* 1 ?-) SCORE-MATCH-WORD
      (aref *bonus-states* 1 ?_) SCORE-MATCH-WORD
      (aref *bonus-states* 1 ?\ ) SCORE-MATCH-WORD
      (aref *bonus-states* 1 ?.) SCORE-MATCH-DOT)

(dotimes (i 26)
  (setf (aref *bonus-states* 2 (+ i (char-code ?a))) SCORE-MATCH-CAPITAL))

(defvar *bonus-index*
  (make-array 256 :initial-element 0
              :displaced-to (make-array 128 :element-type 'size-t)
              :displaced-index-offset 64))

(dotimes (i 26)
  (setf (aref *bonus-index* (+ i (char-code ?a))) 1
        (aref *bonus-index* (+ i (char-code ?A))) 2))

(dotimes (i 10)
  (setf (aref *bonus-index* (+ i (char-code ?0))) 1))

(defun compute-bonus (last-ch ch)
  (aref *bonus-states* (aref *bonus-index* (char-code ch)) (char-code last-ch)))
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
  (%main (uiop:command-line-arguments)))

(defun main- ()
  (let ((options (make-instance 'options :args *command-line-args*))
        (choices (make-instance 'choices)))
    (cond ((options-benchmark options)
           (unless (options-filter options)
             (error "Must specify -e/--show-matches with --benchmark"))
           (choices-fread choices :stream *standard-input* :delimiter (options-input-delimiter options))
           (loop repeat (options-benchmark options)
                 do (choices-search choices (options-filter options))))
          ((options-filter options)
           (choices-fread choices :stream *standard-input* :delimiter (options-input-delimiter options))
           (choices-search choices (options-filter options))
           (loop for i below (choices-available choices)
                 do (when (options-show-scores options)
                      (format t "~f\t" (choices-get-score choices i)))
                    (format t "~a~%" (choices-get choices i))))
          (t ;; interactive
           (when (eq (tty-type *terminal-io*) :terminal)
             (choices-fread choices :stream *standard-input* :delimiter (options-input-delimiter options)))
           (with-open-file (tty (or (options-tty-filename options) "/dev/tty"))
             (unless (eq (tty-type tty) :terminal)
               (choices-fread choices :stream *standard-input* :delimiter (options-input-delimiter options)))
             (when (> (options-num-lines options) (choices-size choices))
               (setf (options-num-lines options) (choices-size choices)))
             (let ((num-lines-adjustment (if (options-show-info options) 2 1)))
               (when (> (+ (options-num-lines options) num-lines-adjustment) (tty-get-height tty))
                 (setf (options-num-lines options) (- (tty-get-height tty) num-lines-adjustment))))
             (let ((tty-interface (make-instance 'tty-interface :tty tty :choices choices :options options)))
               (setf ret (tty-interface-run tty-interface)))))))
  (quit))
