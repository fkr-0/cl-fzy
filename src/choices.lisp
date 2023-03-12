(in-package :cl-fzy)
;; Initial size of buffer for storing input in memory
(defparameter *initial-buffer-capacity* 4096)

;; Initial size of choices array
(defparameter *initial-choice-capacity* 128)

(defun cmpchoice (a b)
  (let ((score-a (scored-result-score a))
        (score-b (scored-result-score b)))
    (cond
      ((= score-a score-b)
       (if (< (scored-result-str a) (scored-result-str b))
           -1
           1))
      ((< score-a score-b) 1)
      (t -1))))

(defun safe-realloc (buffer size)
  (let ((new-buffer (sb-ext:realloc buffer size)))
    (if (null new-buffer)
        (progn
          (format t "Error: Can't allocate memory (~a bytes)~%" size)
          (sb-ext:abort)))
    new-buffer))


(defun choices-fread (c file input-delimiter)
  (let ((buffer-start (choices-buffer-size c)))
    (let ((capacity initial-buffer-capacity))
      (loop while (<= capacity (choices-buffer-size c))
        do (setf capacity (* 2 capacity)))
      (setf (choices-buffer c) (safe-realloc (choices-buffer c) capacity)))
    (loop while (= (incf (choices-buffer-size c)
                        (fread (choices-buffer c buffer-start)
                               1
                               (- capacity (choices-buffer-size c))
                               file))
                   capacity)
      do (let ((capacity (* 2 capacity)))
           (setf (choices-buffer c) (safe-realloc (choices-buffer c) capacity))))
    (setf (choices-buffer c)
          (safe-realloc (choices-buffer c)
                        (+ (choices-buffer-size c) 1)))
    (setf (schar (choices-buffer c) (choices-buffer-size c)) #\null)
    (do ((line-end (aref (choices-buffer c) (choices-buffer-size c)) (aref (choices-buffer c) (choices-buffer-size c))))
        ((null line-end))
      (let ((line (aref (choices-buffer c) buffer-start)))
        (let ((nl (position input-delimiter line :from-end t)))
          (when nl
            (setf (schar (subseq line nl) 0) #\null)
            (when (not (string= "" line))
              (choices-add c line)))
          (setf line (subseq line (+ nl 1))))))))


(defun choices-resize (c new-capacity)
  (setf (choices-strings c) (safe-realloc (choices-strings c) (* new-capacity (sizeof 'char))))
  (setf (choices-capacity c) new-capacity))

(defun choices-reset-search (c)
  (free (choices-results c))
  (setf (choices-selection c) 0)
  (setf (choices-available c) 0)
  (setf (choices-results c) nil))

(defun choices-init (c options)
  (setf (choices-strings c) nil)
  (setf (choices-results c) nil)
  (setf (choices-buffer-size c) 0)
  (setf (choices-buffer c) nil)
  (setf (choices-capacity c) 0)
  (choices-resize c INITIAL_CHOICE_CAPACITY)
  (if (options-workers options)
      (setf (choices-worker-count c) (options-workers options))
      (setf (choices-worker-count c) (floor (sysconf _SC_NPROCESSORS_ONLN))))
  (choices-reset-search c))

(defun choices-destroy (c)
  (free (choices-buffer c))
  (setf (choices-buffer c) nil)
  (setf (choices-buffer-size c) 0)
  (free (choices-strings c))
  (setf (choices-strings c) nil)
  (setf (choices-capacity c) 0)
  (free (choices-results c))
  (setf (choices-results c) nil)
  (setf (choices-available c) 0)
  (setf (choices-selection c) 0))


(defun choices-fread (c file input-delimiter)
  (let ((buffer-start (choices-buffer-size c)))
    (let ((capacity initial-buffer-capacity))
      (loop while (<= capacity (choices-buffer-size c))
        do (setf capacity (* 2 capacity)))
      (setf (choices-buffer c) (safe-realloc (choices-buffer c) capacity)))
    (loop while (= (incf (choices-buffer-size c)
                        (fread (choices-buffer c buffer-start)
                               1
                               (- capacity (choices-buffer-size c))
                               file))
                   capacity)
      do (let ((capacity (* 2 capacity)))
           (setf (choices-buffer c) (safe-realloc (choices-buffer c) capacity))))
    (setf (choices-buffer c)
          (safe-realloc (choices-buffer c)
                        (+ (choices-buffer-size c) 1)))
    (setf (schar (choices-buffer c) (choices-buffer-size c)) #\null)
    (do ((line-end (aref (choices-buffer c) (choices-buffer-size c)) (aref (choices-buffer c) (choices-buffer-size c))))
        ((null line-end))
      (let ((line (aref (choices-buffer c) buffer-start)))
        (let ((nl (position input-delimiter line :from-end t)))
          (when nl
            (setf (schar (subseq line nl) 0) #\null)
            (when (not (string= "" line))
              (choices-add c line)))
          (setf line (subseq line (+ nl 1))))))))

(defun choices-add (c choice)
  "Add a choice to a choices_t structure"
  ;; Previous search is now invalid
  (choices-reset-search c)

  (if (= (choices-size c) (choices-capacity c))
      (choices-resize c (* (choices-capacity c) 2)))

  ;; Add the choice to the end of the strings array
  (setf (elt (choices-strings c) (choices-size c)) choice)
  (incf (choices-size c)))

(defun choices-available (c)
  "Return the number of available choices in a choices_t structure"
  (choices-available c))

(defparameter *batch-size* 512)

(defstruct result-list
  "A list of scored results"
  (list nil :type (or list null))
  (size 0 :type fixnum))

(defstruct search-job
  "A job to search for a string in a choices_t structure"
  (lock (make-mutex) :type mutex)
  (choices nil :type (or null (satisfies choices-p)))
  (search nil :type (or null string))
  (processed 0 :type fixnum)
  (workers nil :type (or null (satisfies workers-p))))

(defstruct worker
  "A worker thread for searching a choices_t structure"
  (thread-id nil :type (or null integer))
  (job nil :type (or null (satisfies search-job-p)))
  (worker-num 0 :type fixnum)
  (result (make-result-list) :type (or null (satisfies result-list-p))))


(defun worker-get-next-batch (job)
  "Get the next batch of choices to search"
  (with-mutex ((search-job-lock job))
    (let ((start (search-job-processed job)))
      (setf (search-job-processed job) (+ (search-job-processed job) *batch-size*))
      (when (> (search-job-processed job) (choices-size (search-job-choices job)))
        (setf (search-job-processed job) (choices-size (search-job-choices job))))
      (list start (search-job-processed job)))))

(defun merge2 (list1 list2)
  "Merge two result lists into one"
  (let ((result-index 0)
        (index1 0)
        (index2 0)
        (result (make-result-list :size (+ (result-list-size list1) (result-list-size list2)))))

    (setf (result-list-list result) (make-array (result-list-size result) :element-type 'scored-result))

    (loop while (and (< index1 (result-list-size list1)) (< index2 (result-list-size list2))) do
          (if (< (cmpchoice (aref (result-list-list list1) index1
                             (aref (result-list-list list2) index2)))
                 0)
              (progn
                (setf (aref (result-list-list result) result-index)
                      (aref (result-list-list list1) index1))
                (incf index1))
              (progn
                (setf (aref (result-list-list result) result-index)
                      (aref (result-list-list list2) index2))
                (incf index2)))
          (incf result-index))

    (loop while (< index1 (result-list-size list1)) do
          (setf (aref (result-list-list result) result-index)
                (aref (result-list-list list1) index1))
          (incf index1)
          (incf result-index))

    (loop while (< index2 (result-list-size list2)) do
          (setf (aref (result-list-list result) result-index)
                (aref (result-list-list list2) index2))
          (incf index2)
          (incf result-index))

    (free (result-list-list list1))
    (free (result-list-list list2))

    result))
(defun choices-search (c search)
  (choices-reset-search c)

  (let ((job (make-instance 'search-job :search search :choices c)))
    (unless (zerop (pthread-mutex-init job))
      (error "pthread_mutex_init failed"))

    (let* ((workers (make-array (worker-count c) :initial-element nil :element-type 'worker))
           (worker-nums (reverse (loop for i from (1- (worker-count c)) downto 0 collect i))))

      (loop for i below (worker-count c) do
            (let ((worker (make-instance 'worker :job job :worker-num (pop worker-nums))))
              (setf (result worker) (make-instance 'result :size 0 :list (make-array (size c) :element-type 'scored-result)))

              ;; These must be created last-to-first to avoid a race condition when fanning in
              (unless (zerop (pthread-create (thread-id worker)))
                (error "pthread_create failed"))

              (setf (aref workers (worker-num worker)) worker)))

      (unless (zerop (pthread-join (thread-id (aref workers 0))))
        (error "pthread_join failed"))

      (setf (results c) (result-list (result (aref workers 0))))
      (setf (available c) (result-size (result (aref workers 0))))

      (loop for i below (worker-count c) do
            (let ((worker (aref workers i)))
              (pthread-cancel (thread-id worker))
              (free (result-list (result worker))))))

    (pthread-mutex-destroy job)))
