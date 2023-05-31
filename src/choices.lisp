(in-package :cl-fzy)
(defconstant +initial-buffer-capacity+ 4096)
(defconstant +initial-choice-capacity+ 128)
(defconstant +batch-size+ 512)
(defstruct scored-result
  (str (make-array 0 :adjustable t :fill-pointer t :element-type 'character) :type (or (vector character) (vector base-char)))
  (score 0.9 :type single-float))

(defstruct choices
  (buffer nil :type (or null (vector (unsigned-byte 8))))
  (buffer-size 0 :type fixnum)
  (capacity 0 :type fixnum)
  (size 0 :type fixnum)
  (strings nil :type (or null (vector t)))
  (results nil :type (or null (vector t)))
  (available 0 :type fixnum)
  (selection 0 :type fixnum)
  (worker-count 0 :type fixnum))
(defstruct result-list
  "A list of scored results"
  ;; (list nil :type (or null (vector scored-result)))
  (list (make-array 0 :element-type 'scored-result :adjustable t :fill-pointer t) :type (or null (vector scored-result)))
  (size 0 :type fixnum))
(defstruct (options (:constructor make-options))
  (workers 0 :type fixnum))

(defstruct worker
  "A worker thread for searching a choices_t structure"
  (thread-id nil :type (or null thread))
  (job nil :type (or null (satisfies search-job-p)))
  (worker-num 0 :type fixnum)
  (result nil :type (or null (satisfies search-result-p))))

(defun make-worker (job result worker-num)
  "Create a worker for JOB with RESULT."
  (let ((worker (make-instance 'worker)))
    (setf (worker-job worker) job)
    (setf (worker-result worker) result)
    (setf (worker-worker-num worker) worker-num)
    worker))

(defun cmpchoice (a b)
  ;; (format t "comparing ~a ~a%" a b)
  (declare (type scored-result a b))
  (let* ((score-a (scored-result-score a))
          (score-b (scored-result-score b))
          (str-a (scored-result-str a))
          (str-b (scored-result-str b)))
    (cond ((= score-a score-b)
            (if (< (sb-kernel:get-lisp-obj-address str-a)
                  (sb-kernel:get-lisp-obj-address str-b))
              1 nil))
      ((< score-a score-b) nil)
      (t 1))))
(defun safe-realloc (buffer size)
  (adjust-array buffer size))
(defun remove-ints-from-result-list (list)
  (remove-if (lambda (x) (integerp  x)) list))


(defun choices-fread (c file input-delimiter)
  (let ((buffer-start (choices-buffer-size c)))
    (setf (choices-buffer c) (safe-realloc (choices-buffer c) (ceiling (choices-buffer-size c) +initial-buffer-capacity+)))
    (loop
      for line = (read-line file nil nil)
      while line
      do (setf (choices-buffer c) (concatenate 'string (choices-buffer c) line input-delimiter)))
    (setf (choices-buffer c) (concatenate 'string (choices-buffer c) (string #\null)))
    (incf (choices-buffer-size c))
    (let ((line-end (+ (choices-buffer c) (choices-buffer-size c)))
           (line (+ (choices-buffer c) buffer-start)))
      (loop
        with nl = (position input-delimiter line :test #'char=)
        while (< line line-end)
        do (progn
             (when nl
               (setf (aref line nl) #\null)
               (incf nl))
             (unless (string= line "")
               (choices-add c line))
             (setf line (+ line nl)))))))

(defun choices-resize (c new-capacity)
  (setf (choices-capacity c) new-capacity)
  (adjust-array (choices-strings c) new-capacity :element-type '(vector character 0) :fill-pointer t))

(defun choices-reset-search (c)
  (setf (choices-results c) nil)
  (setf (choices-available c) 0)
  (setf (choices-selection c) 0))

(defun choices-init (c options)
  (setf (choices-strings c) (make-array +initial-choice-capacity+ :element-type '(vector character 0) :adjustable t :fill-pointer t))
  (setf (choices-results c) nil)
  (setf (choices-buffer-size c) 0)
  (setf (choices-buffer c) nil)
  (setf (choices-capacity c) +initial-choice-capacity+)
  (setf (choices-size c) 0)
  ;; (choices-resize c +initial-choice-capacity+)
  (if (/= (options-workers options) 0)
    (setf (choices-worker-count c) (options-workers options))
    (setf (choices-worker-count c) 4));; (sb-ext:processor-count)))
  (choices-reset-search c))

(defun choices-destroy (c)
  (setf (choices-buffer c) nil)
  (setf (choices-buffer-size c) 0)
  (setf (choices-strings c) nil)
  (setf (choices-capacity c) 0)
  (setf (choices-size c) 0)
  (setf (choices-results c) nil)
  (setf (choices-available c) 0)
  (setf (choices-selection c) 0))

(defun choices-add (c choice)
  (choices-reset-search c)
  (when (= (choices-size c) (choices-capacity c))
    (choices-resize c (* (choices-capacity c) 2)))
  (setf (aref (choices-strings c) (choices-size c)) choice)
  (incf (choices-size c)))

(defun worker-get-next-batch (job start end)
  (sb-thread:with-mutex ((search-job-lock job))
    (setf start (search-job-processed job))
    (setf (search-job-processed job) (+ (search-job-processed job) +batch-size+))
    (when (> (search-job-processed job) (choices-size (search-job-choices job)))
      (setf (search-job-processed job) (choices-size (search-job-choices job))))
    (setf end (search-job-processed job))
    (when (> end (choices-size (search-job-choices job)))
      (setf end (choices-size (search-job-choices job)))))
  (values start end))


(defun make-search-job-thingy (search-str choices)
  "Create a search job for SEARCH-STR in CHOICES."
  (let ((job (make-instance 'cl-fzy::search-job)))
    (setf (search-job-search job)search-str)
    (setf (search-job-choices job) choices)
    (setf (search-job-processed job) 0)
    (setf (search-job-lock job) (sb-thread:make-mutex))
    (setf (search-job-processed job) 0)
    job))
(defun search-job-p (obj)
  "Return t if OBJ is a search job."
  (typep obj 'cl-fzy::search-job))
(defun search-result-p (obj)
  "Return t if OBJ is a search result."
  (typep obj 'cl-fzy::result-list))

(defclass search-result ()
  ((results :initform nil :accessor search-result-results)
    (available :initform 0 :accessor search-result-available)
    (size :initform 0 :accessor search-result-size)
    (selection :initform 0 :accessor search-result-selection)))

(defun make-scored-result (&key score str)
  (let ((result (make-instance 'cl-fzy::scored-result)))
    (setf (scored-result-score result) score)
    (setf (scored-result-str result) str)
    result))
(defun add-result (result scored-result)
  (with-slots (results available) result
    (setf (aref results available) scored-result)
    (incf available)))
(defun worker-run (worker)
  (with-slots (job result) worker
    (let* ((search (search-job-search job))
            (choices (search-job-choices job)))
      (with-slots (buffer buffer-size size available) choices
        (let ((i 0))
          (loop while (and (not (thread-interrupted-p ))
                        (< i available))
            do (let ((string (aref (strings choices) i)))
                 (when (and string (string-match-p search string))
                   (add-result result (make-scored-result
                                        :score (score search string)
                                        :str string))
                   (incf (search-result-size result)))
                 (incf i))))
        ;; (setf (search-result-size result) (size result))
        ))))

(defun make-result-list (r-list r-size )
  (let ((result-list (make-instance 'cl-fzy::result-list)))
    (setf (result-list-list result-list) (or r-list (make-array r-size :element-type 'cl-fzy::scored-result)))
    (setf (result-list-size result-list) r-size)
    result-list))
;; (defun merge2 (list1 list2)
;;   (let ((result-index 0)
;;          (index1 0)
;;          (index2 0)
;;          (result (make-array (+ (result-list-size list1) (result-list-size list2))
;;                    :element-type 'scored-result
;;                    :adjustable t
;;                    :fill-pointer t)))
;;     (loop
;;       while (and (< index1 (result-list-size list1)) (< index2 (result-list-size list2)))
;;       do (setf (aref result result-index)
;;            (if (< (cmpchoice (aref (result-list-list list1) index1)
;;                     (aref (result-list-list list2) index2))
;;                  0)
;;              (prog1 (aref (result-list-list list1) index1)
;;                (incf index1))
;;              (prog1 (aref (result-list-list list2) index2)
;;                (incf index2))))
;;       (incf result-index))
;;     (loop
;;       while (< index1 (result-list-size list1))
;;       do (setf (aref result result-index) (aref (result-list-list list1) index1))
;;       (incf index1)     (incf result-index))
;;     (loop
;;       while (< index2 (result-list-size list2))
;;       do (setf (aref result result-index) (aref (result-list-list list2) index2))
;;       (incf index2)
;;       (incf result-index))
;;     (setf (result-list-list list1) nil)
;;     (setf (result-list-list list2) nil)
;;     (make-result-list :list result :size (+ (result-list-size list1) (result-list-size list2)))))
;; (defun merge2 (list1 list2)
;;   (let* ((size1 (result-list-size list1))
;;           (size2 (result-list-size list2))
;;           (result (make-array (+ size1 size2)
;;                     :element-type 'scored-result
;;                     :initial-element (make-scored-result :score 0 :str nil)
;;                     :adjustable t
;;                     :fill-pointer t)))
;;     (merge 'vector result
;;       (result-list-list list1)
;;       :end1 size1
;;       (result-list-list list2)
;;       :end2 size2
;;       :test (lambda (a b) (< (cmpchoice a b) 0)))
;;     (setf (result-list-list list1) nil)
;;     (setf (result-list-list list2) nil)
;;     (make-result-list result (+ size1 size2))))

(defun merge2 (result1 result2)
  (declare (type result-list result1 result2))
  (let* ((size1 (result-list-size result1))
          (size2 (result-list-size result2))
          (array1 (result-list-list result1))
          (array2 (result-list-list result2))
          (result (make-array (+ size1 size2) :adjustable t))
          (result-index 0)
          (index1 0)
          (index2 0))

    ;; (format t "size1: ~a, size2: ~a~%" size1 size2)
    ;; (format t "array1: ~a, array2: ~a~%" array1 array2)
    ;; Merge elements from both arrays
    (loop while (and (< index1 size1) (< index2 size2)) do
      (if (not(null (funcall #'cmpchoice (aref array1 index1) (aref array2 index2)) ))
        (progn (setf (aref result result-index) (aref array1 index1))
          (incf index1))
        (progn (setf (aref result result-index) (aref array2 index2))
          (incf index2)))
      (incf result-index))

    ;; If there are remaining elements in array1, add them to the result
    (loop for i from index1 below size1 do
      (setf (aref result result-index) (aref array1 i))
      (incf result-index))

    ;; If there are remaining elements in array2, add them to the result
    (loop for i from index2 below size2 do
      (setf (aref result result-index) (aref array2 i))
      (incf result-index))

    (make-result-list result result-index)))

(defun choices-search-worker (w)
  (let* ((job (worker-job w))
          (c (search-job-choices job))
          (result (worker-result w))
          start end)
    (loop
      ;; Get the next batch of work
      (multiple-value-setq (start end) (worker-get-next-batch job start end))

      ;; If start equals end, break the loop
      (when (= start end)
        (return))

      ;; For each item in the batch...
      (loop for i from start below end do
        ;; If it matches the search string...
        ;; (format t "~A search: ~A, choice: ~A~%" i (search-job-search job) (aref (choices-strings c) i))
        ;; (format t "type-choice ~A~%" (type-of (aref (choices-strings c) i)))
        ;; (format t "choices: ~A~%" c)
        (when (has-match (search-job-search job) (aref (choices-strings c) i))
          ;; Add it to the result list
          (setf (aref (result-list-list result) (result-list-size result))
            (make-scored-result :str (aref (choices-strings c) i)
              :score (match (search-job-search job) (aref (choices-strings c) i))))
          ;; Increment the result size
          (incf (result-list-size result)))))

    ;; Sort the partial result
    ;; Note: Lisp's sort function is destructive, so we don't need to reassign the result
    ;; (setf (result-list-list result) (remove-ints-from-result-list ( result-list-list result )))
    ;; (format t "~A~%" (result-list-list result))
    ;; (format t "size: ~A~%" (result-list-size result))

    (setf (result-list-list result) (subseq (result-list-list result) 0 (result-list-size result)))
    ;; (format t "~A~%" (result-list-list result))
    ;; (format t "size: ~A~%" (result-list-size result))
    (sort (result-list-list result) #'cmpchoice )
    ;; (format t "~A~%" (result-list-list result))
    ;; (sort (result-list-list result) #'cmpchoice :key #'scored-result-score)
    ;; (format t "w ~A ~A list ~A~%" (worker-worker-num w)
    ;;   (choices-worker-count c)
    ;;   (result-list-list result))
    ;; (format t "size: ~A~%" (result-list-size result))
    ;; (format t "~A~%" (result-list-list result))
    ;; (format t "~A~%" (result-list-list result))
    ;; (sort '(0 0 ) #'cmpchoice :key #'scored-result-score)
    ;; Fan-in, merging results
    ;; (loop for step from 0 do
    ;;   (when (mod (worker-worker-num w) (expt 2 (1+ step)))
    ;;     ;; (logbitp step (worker-worker-num w))
    ;;     (return))
    ;;   (let ((next-worker (logior (worker-worker-num w) (ash 1 step))))
    ;;     (when (>= next-worker (choices-worker-count c))
    ;;       (return))
    ;;     (handler-case
    ;;       ;; Try to join the next worker's thread
    ;;       (sb-thread:join-thread (aref (search-job-workers job) next-worker))
    ;;       (error (e)
    ;;         ;; If an error occurs, print an error message and abort
    ;;         (format *error-output* "Error: thread join failed: ~A~%" e)
    ;;         (abort)))
    ;;     ;; Merge the results
    ;;     (format t "w ~A merging with ~A~%" (worker-worker-num w) next-worker)
    ;;     (setf (worker-result w) (merge2 (worker-result w)
    ;;                               (worker-result (aref (search-job-workers job) next-worker))))
    ;;     (format t "w ~A merged with ~A~%" (worker-worker-num w) next-worker)

    ;;     ))
    ;; (do ((step 0 (1+ step)))
    ;;   ;; condition for breaking the loop
    ;;   ((or (logbitp step (worker-worker-num w))
    ;;      (>= (logior (worker-worker-num w) (ash 1 step)) (choices-worker-count c))))
    ;;   (let ((next-worker (logior (worker-worker-num w) (ash 1 step))))
    ;;     (handler-case
    ;;       ;; Try to join the next worker's thread
    ;;       (sb-thread:join-thread (aref (search-job-workers job) next-worker))
    ;;       (error (e)
    ;;         ;; If an error occurs, print an error message and abort
    ;;         (format *error-output* "Error: thread join failed: ~A~%" e)
    ;;         (sb-ext:quit))) ; equivalent to exit(EXIT_FAILURE) in C++
    ;;     ;; Merge the results
    ;;     (setf (worker-result w) (merge2 (worker-result w)
    ;;                               (worker-result (aref (search-job-workers job) next-worker))))))
    ;; (let ((*w* w)
    ;;        (*job* job)
    ;;        (*c* c)
    ;;        (*step* 0)
    ;;        (*next-worker* 0))
    ;;   (loop
    ;;     (when (zerop (mod (worker-worker-num *w*) (ash 2 *step*)))
    ;;       (return))
    ;;     (setf *next-worker* (logior (worker-worker-num *w*) (ash 1 *step*)))
    ;;     (when (>= *next-worker* (choices-worker-count *c*))
    ;;       (return))
    ;;     (handler-case
    ;;       (progn (format t "joining thread ~a ~a~%" (worker-worker-num *w*) *next-worker*)
    ;;         (sb-thread:join-thread (aref (search-job-workers *job*) *next-worker*)))
    ;;       (error (e)
    ;;         ;; (format t "Thread join error: ~a~%" e)
    ;;         (format t "Thread join error")
    ;;         (uiop:quit -1)))
    ;;     ;; (format t "Merging ~a (~a) ~a ~a~%" (worker-worker-num *w*) (worker-result *w*) *next-worker* (worker-result (aref (search-job-workers *job*) *next-worker*)))
    ;;     (format t "Merging ~a ~a~%" (worker-worker-num *w*)  *next-worker* )

    ;;     (setf (worker-result *w*) (merge2 (worker-result *w*) (worker-result (aref (search-job-workers *job*) *next-worker*))))
    ;;     (incf *step*)))
    (let ((next-worker 0))
      (loop for step from 0 do
        (when (not (zerop (mod (worker-worker-num w) (ash 2 step))))
          (return))

        (setf next-worker (logior (worker-worker-num w) (ash 1 step)))
        (when (>= next-worker (choices-worker-count c))
          (return))
        (when (= next-worker (worker-worker-num w))
          (return))
        ;; (format t "joining thread ~a ~a~%" (worker-worker-num w) next-worker)
        (handler-case
          (sb-thread:join-thread (worker-thread-id(aref (search-job-workers job) next-worker)) :timeout 3)
          (error (e)
            (format t "Thread join error: ~a~%" (worker-worker-num w))
            (uiop:quit -1)
            )
          )

        ;; (format t "Merging from ~a (~a) to ~a (~a)~%"
        ;; (format t "Merging from ~a  to  ~a~%"
        ;;   (worker-worker-num w)
        ;;   ;; (result-list-list (worker-result w))
        ;;   next-worker)
        ;; (result-list-list (worker-result (aref (search-job-workers job) next-worker))))
        (let* ((res1 (worker-result w))
                (res2 (worker-result (aref (search-job-workers job) next-worker)))
                (res1-list (result-list-list res1))
                (res2-list (result-list-list res2)))
          (cond
            ((or (null res1-list) (= (length res1-list) 0) (null res1))
              (setf (worker-result w) res2))
            ((or (null res2-list) (= (length res2-list) 0) (null res2))
              (setf (worker-result w) res1))
            (t
              (setf (worker-result w) (merge2 res1 res2)))))
        ;;   (setf (worker-result w) (merge2 res1 res2)))
        ;; (setf (worker-result w)
        ;;   (merge2 (worker-result w)
        ;;     (worker-result (aref (search-job-workers job) next-worker))))
        ;; (format t "Worker: ~a Mergeresult: ~a~%" (worker-worker-num w) (worker-result w) )
        ))))



(defun choices-search (c search)
  (choices-reset-search c)
  (let ((job (make-search-job
               :choices c
               :search search
               :workers (make-array (choices-worker-count c) :element-type 'worker :adjustable t
                          :fill-pointer t)))
         (workers nil))
    (setf workers (search-job-workers job))
    (setf (search-job-lock job) (sb-thread:make-mutex))
    (loop for i from (1- (choices-worker-count c)) downto 0 do
      (setf (aref workers i) (make-worker
                               job

                               (make-result-list
                                 (make-array (choices-size c)
                                   :element-type 'scored-result
                                   :adjustable t
                                   :fill-pointer t)
                                 0)i))
      ;; (setf (worker-thread-id (aref workers i)))
      ;; (format t "starting worker ~a~%" i)
      (setf (worker-thread-id (aref workers i))
        (sb-thread:make-thread #'choices-search-worker :arguments (list (aref workers i)))))
    ;; (sb-thread:make-thread #'choices-search-worker :arguments (list (aref workers i))))
    ;; (format t "waiting for workers~%")
    (sb-thread:join-thread (worker-thread-id (aref workers 0)))
    ;; (format t "done waiting for workers~%")
    (setf (choices-results c) (result-list-list (worker-result (aref workers 0))))
    (setf (choices-available c) (result-list-size (worker-result (aref  workers 0))))
    ;; (format t "done waiting for workers, result: ~a ~%" (result-list-list (worker-result (aref workers 0))))
    ;; (dotimes (i (choices-worker-count c))
    ;;   (handler-case
    ;;     ;; (sb-thread:destroy-thread target-thread)
    ;;     (sb-thread:destroy-thread (worker-thread-id (aref workers i)))
    ;;     (error (e)
    ;;       ;; Handle the error here
    ;;       (format t "Error: ~a~% - Thread: ~a~%" e i)
    ;;       ;; (declare (ignore e))
    ;;       )))
    ))





(defun workers-p (x)
  (and (typep x 'array)
    (every (lambda (y) (or (= 0 y) (worker-p y)) )x)))
(defstruct search-job
  "A job to search for a string in a choices structure"
  (lock (sb-thread:make-mutex) :type sb-thread:mutex)
  (choices nil :type (or null (satisfies choices-p)))
  (search nil :type (or null string))
  (processed 0 :type fixnum)
  (workers nil :type (or null (satisfies workers-p))))

;; (make-search-job)
;; (make-instance 'search-job )
;; (make-instance 'options )
(defun result-list-p-or-null (x)
  (or  (null x) (result-list-p x)))
;; (make-result-list :list '(2) :size 1)

(defun choices-get (c n)
  (if (< n (choices-available c))
    (scored-result-str (aref (choices-results c) n))
    nil))

(defun choices-getscore (c n)
  (scored-result-score (aref (choices-results c) n)))

(defun choices-prev (c)
  (when (plusp (choices-available c))
    (setf (choices-selection c) (mod (+ (choices-selection c) (1- (choices-available c)))
                                  (choices-available c)))))

(defun choices-next (c)
  (when (plusp (choices-available c))
    (setf (choices-selection c) (mod (+ (choices-selection c) 1)
                                  (choices-available c)))))

;; (defvar *default-options* (make-instance 'cl-fzy::options))
;; (defvar *choices* (make-instance 'cl-fzy::choices))

;; *choices*
;; *default-options*
;; (progn
;;   (cl-fzy::choices-init *choices* *default-options*)
;;   (setf (choices-worker-count *choices*) 2)
;;   (choices-add *choices* "fouox")
;;   (choices-add *choices* "barxxux")
;;   (choices-add *choices* "bauxz")
;;   (choices-add *choices* "qux")
;;   (choices-add *choices* "uxfofoo")
;;   (choices-add *choices* "babarux")
;;   (choices-add *choices* "babaz")
;;   (choices-add *choices* "ququx")
;;   (choices-search *choices* "ux")
;;   *choices*)
;; (multi-search "ba" *choices* 4)
