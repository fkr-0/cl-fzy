(in-package :cl-fzy)
(defun strcasechr (s c)
  (let ((accept (list c (char-upcase c) #\null)))
    (position-if (lambda (x) (find x accept :test #'char-equal))
                 s)))

(defun has-match (needle haystack)
  (loop for nch across needle
        for hch = (strcasechr haystack nch)
        while hch
        do (setf haystack (subseq haystack (1+ hch)))))
(defmacro swap (x y &environment env)
  (let ((tmp (gensym)))
    `(let ((,tmp ,x))
       (setf ,x ,y
             ,y ,tmp))))

(defun max (a b)
  (if (> a b) a b))

(defstruct match-struct
  (needle-len 0 :type integer)
  (haystack-len 0 :type integer)
  (lower-needle (make-array match-max-len :element-type 'character) :type (array character))
  (lower-haystack (make-array match-max-len :element-type 'character) :type (array character))
  (match-bonus (make-array match-max-len :element-type 'single-float) :type (array single-float)))

(defun precompute-bonus (haystack match-bonus)
  (let ((last-ch #\/))
    (loop for ch across haystack
          for i from 0
          do (setf (aref match-bonus i) (compute-bonus last-ch ch)
                   last-ch ch))))
(defun setup-match-struct (match needle haystack)
  (setf (match-struct-needle-len match) (length needle)
        (match-struct-haystack-len match) (length haystack))

  (when (and (<= (match-struct-haystack-len match) match-max-len)
             (<= (match-struct-needle-len match) (match-struct-haystack-len match)))
    (loop for i below (match-struct-needle-len match)
          do (setf (aref (match-struct-lower-needle match) i) (char-downcase (aref needle i))))
    (loop for i below (match-struct-haystack-len match)
          do (setf (aref (match-struct-lower-haystack match) i) (char-downcase (aref haystack i))))
    (precompute-bonus haystack (match-struct-match-bonus match))))


(defun match-row (match row curr-d curr-m last-d last-m)
  (let* ((n (match-struct-needle-len match))
         (m (match-struct-haystack-len match))
         (i row)
         (lower-needle (match-struct-lower-needle match))
         (lower-haystack (match-struct-lower-haystack match))
         (match-bonus (match-struct-match-bonus match))
         (prev-score score-min)
         (gap-score (if (= i (- n 1)) score-gap-trailing score-gap-inner)))
    (loop for j below m
          do (if (char-equal (aref lower-needle i) (aref lower-haystack j))
                 (let ((score score-min))
                   (if (= i 0)
                       (setf score (+ (* j score-gap-leading) (aref match-bonus j)))
                       (when (> j 0)
                         (setf score (max (+ (aref last-m (- j 1)) (aref match-bonus j))
                                          (+ (aref last-d (- j 1)) score-match-consecutive)))))
                   (setf (aref curr-d j) score-min)
                   (setf (aref curr-m j) (max prev-score (+ score gap-score)))
                   (setf prev-score (max score (+ prev-score gap-score))))
                 (progn
                   (setf (aref curr-d j) score-min)
                   (setf (aref curr-m j) (max prev-score (+ prev-score gap-score)))
                   (setf prev-score (+ prev-score gap-score)))))))


(defun match (needle haystack)
  (if (null needle)
      score-min
      (let ((match (make-match-struct needle haystack)))
        (let ((n (match-needle-len match))
              (m (match-haystack-len match)))
          (if (or (> m match-max-len) (> n m))
              ;; Unreasonably large candidate: return no score
              ;; If it is a valid match it will still be returned, it will
              ;; just be ranked below any reasonably sized candidates
              score-min
              (if (= n m)
                  ;; Since this method can only be called with a haystack which
                  ;; matches needle. If the lengths of the strings are equal the
                  ;; strings themselves must also be equal (ignoring case).
                  score-max
                  (let ((D (make-array `(2 ,match-max-len)
                                        :element-type 'score-t
                                        :initial-element score-min)))
                    (let ((M (make-array `(2 ,match-max-len)
                                          :element-type 'score-t
                                          :initial-element score-min)))
                      (let ((last-D (aref D 0))
                            (last-M (aref M 0))
                            (curr-D (aref D 1))
                            (curr-M (aref M 1)))
                        (dotimes (i n)
                          (match-row match i curr-D curr-M last-D last-M)
                          (setf (values curr-D last-D) (values last-D curr-D))
                          (setf (values curr-M last-M) (values last-M curr-M))))
                      (aref last-M (1- m))))))))))
(defun match-positions (needle haystack &optional (positions nil))
  (if (not (string= needle ""))
      (let ((match (make-match-struct)))
        (setup-match-struct match needle haystack)
        (let* ((n (match-needle-len match))
               (m (match-haystack-len match)))
          (cond ((> m MATCH_MAX_LEN) SCORE_MIN)
                ((> n m) SCORE_MIN)
                ((= n m)
                 (if positions
                     (loop for i below n do (setf (elt positions i) i))
                     SCORE_MAX))
                (t (let* ((D (make-array `(2 ,MATCH_MAX_LEN) :element-type 'score-t))
                          (M (make-array `(2 ,MATCH_MAX_LEN) :element-type 'score-t))
                          (last-D (aref D 0))
                          (last-M (aref M 0))
                          (curr-D (aref D 1))
                          (curr-M (aref M 1)))
                     (loop for i below n do
                        (let ((p (elt (elt match :rows) i)))
                          (match-row match i curr-D curr-M last-D last-M)
                          (setf curr-D (aref D i))
                          (setf curr-M (aref M i))
                          (setf (aref D i) last-D)
                          (setf (aref M i) last-M)
                          (setf last-D curr-D)
                          (setf last-M curr-M)))
                     (if positions
                         (let ((match-required nil))
                           (loop for i downfrom (- n 1) to 0 do
                              (loop for j downfrom (- m 1) to 0 do
                                 (when (and (not match-required)
                                            (/= (aref D i j) SCORE_MIN)
                                            (or (= (aref D i j) (aref M i j))
                                                (<= (aref M i j)
                                                    (+ (aref D (1- i) (1- j))
                                                       SCORE_MATCH_CONSECUTIVE))))
                                   (setf match-required
                                         (and (> i 0) (> j 0)
                                              (= (aref M i j)
                                                 (+ (aref D (1- i) (1- j))
                                                    SCORE_MATCH_CONSECUTIVE))))
                                   (setf (elt positions i) j)
                                   (setf j (- j 1))
                                   (break))))))
                     (aref M (1- n) (1- m)))))))))
