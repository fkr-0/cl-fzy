(in-package :cl-fzy)
(defconstant +score-max+ (float sb-ext:most-positive-word))
(defconstant +score-min+ (float (* -1 sb-ext:most-positive-word)))

(defconstant +match-max-len+ 1024)

(defun strcasechr (s c)
  "Return the position of the first character in s that is equal to c.
The comparison is case-insensitive."
  (let ((accept (string (char-upcase c))))
    (position-if #'(lambda (ch) (find ch accept)) s)))

(defun swap (x y)
  "Swap the values of x and y."
  ;; (declare (type integer x y))
  (let ((temp x))
    (setf x y)
    (setf y temp)))

(defun max_ (a b)
  (if (> a b) a b))

(defstruct match-struct
  "The match struct contains the precomputed data for the match.
The match struct is used to avoid recomputing the bonus for each
character in the haystack."
  (needle-len 0 :type integer)
  (haystack-len 0 :type integer)
  (lower-needle (make-array +match-max-len+ :element-type 'character) :type (satisfies stringp))
  (lower-haystack (make-array +match-max-len+ :element-type 'character) :type (satisfies stringp))
  (match-bonus (make-array +match-max-len+ :element-type 'single-float) :type (array single-float)))

(defun has-match (needle haystack)
  "Return t if the needle is a substring of the haystack."
  (when (and needle haystack)
    (loop for nch across needle
      for hch = (position nch haystack :test #'char-equal)
      when hch do (setf haystack (subseq haystack (1+ hch)))
      else do (return nil)
      finally (return t))))

(defun precompute-bonus (haystack match-bonus)
  "Precompute the bonus for each character in the haystack.
The bonus is the score for a match with a consecutive character."
  (loop for i below (length haystack)
    for last-ch = #\/ then (aref haystack (1- i))
    do (setf (aref match-bonus i) (float (compute-bonus last-ch (aref haystack i))))
    ;; (loop for i from 0
    ;;   for ch across haystack
    ;;   for last-ch = #\/ then ch
    ;;   do (setf (aref match-bonus i) (float (compute-bonus last-ch ch)))
    ;; (format t "[ch,last_ch] = ~a , ~a~%" (aref haystack i) last-ch))
    ))
;; docstrings in common lisp functions:
;; https://stackoverflow.com/questions/1031006/docstrings-in-common-lisp-functions
;;
(defun setup-match-struct (match needle haystack)
  "Setup the match struct.
The match struct contains the precomputed data for the match."
  (let ((needle-len (length needle))
         (haystack-len (length haystack)))
    (setf (match-struct-needle-len match) needle-len)
    (setf (match-struct-haystack-len match) haystack-len)
    (if (or (> haystack-len +match-max-len+)
          (> needle-len haystack-len))
      (return-from setup-match-struct))
    (setf (match-struct-lower-needle match) (string-downcase needle))
    (setf (match-struct-lower-haystack match) (string-downcase haystack))
    (precompute-bonus haystack (match-struct-match-bonus match)))match)

(defun match-row (match row curr-d curr-m last-d last-m)
  "Compute the match score for the given row."
  (let* ((n (match-struct-needle-len match))
          (m (match-struct-haystack-len match))
          (i row)
          (lower-needle (match-struct-lower-needle match))
          (lower-haystack (match-struct-lower-haystack match))
          (match-bonus (match-struct-match-bonus match))
          (prev-score +score-min+)
          (gap-score (if (= i (1- n))
                       +score-gap-trailing+
                       +score-gap-inner+)))
    (loop for j from 0 below m
      for nhj = (aref lower-needle i)
      for hsj = (aref lower-haystack j)
      if (char= nhj hsj)
      do (let ((score +score-min+))
           (cond ((zerop i)
                   (setf score (+ (* j +score-gap-leading+)
                                 (aref match-bonus j))))
             ((and (> i 0) (> j 0))
               (setf score (max (+ (aref last-m (1- j))
                                  (aref match-bonus j))
                             (+ (aref last-d (1- j))
                               +score-match-consecutive+)))))
           (setf (aref curr-d j) score)
           (setf (aref curr-m j) (setf prev-score (max score (+ prev-score gap-score)))))
      else
      do (progn
           (setf (aref curr-d j) +score-min+)
           (setf (aref curr-m j) (setf prev-score (+ prev-score gap-score)))))))

(defun aref-dimension (array dimension)
  "Return the array at the given dimension."
  (let* ((len (array-dimension array 1))
          (arr (make-array (list len))))
    (loop for i from 0 below len
      do (setf (aref arr i) (aref array dimension i)))
    arr))

(defun match (needle haystack)
  "Return the match score for the given needle and haystack."
  (when (string= needle "")
    (return-from match +score-min+))
  (let ((match (make-match-struct)))
    (setup-match-struct match needle haystack)
    (let* ((n (match-struct-needle-len match))
            (m (match-struct-haystack-len match))
            (d (make-array (list 2 +match-max-len+) :initial-element +score-min+))
            (m-array (make-array (list 2 +match-max-len+) :initial-element +score-min+))
            (last-d (aref-dimension d 0))
            (last-m (aref-dimension m-array 0))
            (curr-d (aref-dimension d 1))
            (curr-m (aref-dimension m-array 1)))
      (if (or (> m +match-max-len+) (> n m))
        (return-from match +score-min+)
        (if (= n m)
          (return-from match +score-max+)))
      (loop for i from 0 below n
        do (match-row match i curr-d curr-m last-d last-m)
        (rotatef curr-d last-d)
        (rotatef curr-m last-m))
      (aref last-m (1- m)))))

(defun match-positions (needle haystack positions)
  (if (string= needle "")
    +score-min+
    (let ((match (make-match-struct)))
      (setup-match-struct match needle haystack)
      (let* ((n (match-struct-needle-len match))
              (m (match-struct-haystack-len match)))
        (if (or (> m +match-max-len+) (> n m))
          +score-min+
          (if (= n m)
            (progn
              (when positions
                (loop for i from 0 below n
                  do (setf (elt positions i) i)))
              +score-max+)
            (let* (
                    (d-array (make-instance 'arr2d :arr-length n))
                    (m-array (make-instance 'arr2d :arr-length n))
                    (last-d nil)
                    (last-m nil)
                    (curr-d nil)
                    (curr-m nil))
              ;; Initialize arrays d and m with arrays of size MATCH_MAX_LEN
              (dotimes (i n)
                (set-row d-array i (make-array +match-max-len+ :initial-element 0))
                (set-row m-array i (make-array +match-max-len+ :initial-element 0)))
              ;; Process rows
              (dotimes (i n)
                (setf curr-d (get-row d-array i))
                (setf curr-m (get-row m-array i))
                (match-row match i curr-d curr-m last-d last-m)
                (setf last-d curr-d)
                (setf last-m curr-m))
              (when positions
                (let ((match-required 0))
                  (loop for i from (- n 1) downto 0 do
                    (loop for j from (- m 1) downto 0 do
                      (let ((d-ij (aref (get-row d-array i) j))
                             (m-ij (aref (get-row m-array i) j)))
                        (when (and (/= d-ij +score-min+)
                                (or (/= match-required 0) (= d-ij m-ij)))
                          (setf match-required (if (and (> i 0) (> j 0)
                                                     (= m-ij (+ (aref (get-row d-array (- i 1)) (- j 1)) +score-match-consecutive+)))
                                                 1 0))
                          (setf (aref positions i) j)
                          (decf j)
                          (return)))))))
              (aref last-m (1- m)))))))))
