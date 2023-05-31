
(in-package :cl-fzy-tests)
(def-suite  choices :in cl-fzy)
(in-suite choices)

(defvar *default-options* (make-instance 'cl-fzy::options))
(defvar *choices* (make-instance 'cl-fzy::choices))

(defmacro def-test-case (name &body body)
  `(test ,name ()
     (let ((*default-options* (make-instance 'cl-fzy::options))
            (*choices* (make-instance 'cl-fzy::choices)))
       (cl-fzy::choices-init *choices* *default-options*)
       ,@body
       (cl-fzy::choices-destroy *choices*))))

(def-test-case test-choices-empty
  (is (= 0 (cl-fzy::choices-available *choices*)))
  (is (= 0 (cl-fzy::choices-selection *choices*)))

  (cl-fzy::choices-prev *choices*)
  (is (= 0 (cl-fzy::choices-selection *choices*)))

  (cl-fzy::choices-next *choices*)
  (is (= 0 (cl-fzy::choices-selection *choices*))))

;; (def-test-case test-choices-1
;;   (cl-fzy::choices-add *choices* "tags")

;;   (cl-fzy::choices-search *choices* "")
;;   (is (= 1 (cl-fzy::choices-available *choices*)))
;;   (is (= 0 (cl-fzy::choices-selection *choices*))))

;; rest of your tests go here...

;; (def-test-case test-choices-unicode
;;   (cl-fzy::choices-add *choices* "Edmund Husserl - Méditations cartésiennes - Introduction a la phénoménologie.pdf")
;;   (cl-fzy::choices-search *choices* "e"))


;; (def-test-case test-choices-multi
;;   (cl-fzy::choices-add *choices* "tags")

;;   (cl-fzy::multi-search  "" *choices* 4)
;;   (is (= 1 (cl-fzy::choices-available *choices*)))
;;   (is (= 0 (cl-fzy::choices-selection *choices*)))

;;   (cl-fzy::multi-search "t" *choices*  4)
;;   (is (= 1 (cl-fzy::choices-available *choices*)))
;;   (is (= 0 (cl-fzy::choices-selection *choices*)))

;;   (cl-fzy::choices-prev *choices*)
;;   (is (= 0 (cl-fzy::choices-selection *choices*)))

;;   (cl-fzy::choices-next *choices*)
;;   (is (= 0 (cl-fzy::choices-selection *choices*)))

;;   (is (string= (cl-fzy::choices-get *choices* 0) "tags"))
;;   (is (null (cl-fzy::choices-get *choices* 1))))
(def-test-case test-choices-1
  (cl-fzy::choices-add *choices* "tags")

  (cl-fzy::choices-search *choices* "")
  (is (= 1 (cl-fzy::choices-available *choices*)))
  (is (= 0 (cl-fzy::choices-selection *choices*)))

  (cl-fzy::choices-search *choices* "t")
  (is (= 1 (cl-fzy::choices-available *choices*)))
  (is (= 0 (cl-fzy::choices-selection *choices*)))

  (cl-fzy::choices-prev *choices*)
  (is (= 0 (cl-fzy::choices-selection *choices*)))

  (cl-fzy::choices-next *choices*)
  (is (= 0 (cl-fzy::choices-selection *choices*)))

  (is (string= (cl-fzy::choices-get *choices* 0) "tags"))
  (is (null (cl-fzy::choices-get *choices* 1))))

(def-test-case test-choices-2
  (cl-fzy::choices-add *choices* "tags")
  (cl-fzy::choices-add *choices* "test")

  ;; Empty search
  (cl-fzy::choices-search *choices* "")
  (is (= 0 (cl-fzy::choices-selection *choices*)))
  (is (= 2 (cl-fzy::choices-available *choices*)))

  (cl-fzy::choices-next *choices*)
  (is (= 1 (cl-fzy::choices-selection *choices*)))
  (cl-fzy::choices-next *choices*)
  (is (= 0 (cl-fzy::choices-selection *choices*)))

  (cl-fzy::choices-prev *choices*)
  (is (= 1 (cl-fzy::choices-selection *choices*)))
  (cl-fzy::choices-prev *choices*)
  (is (= 0 (cl-fzy::choices-selection *choices*)))

  ;; Filtered search
  (cl-fzy::choices-search *choices* "te")
  (is (= 1 (cl-fzy::choices-available *choices*)))
  (is (= 0 (cl-fzy::choices-selection *choices*)))
  (is (string= "test" (cl-fzy::choices-get *choices* 0)))

  (cl-fzy::choices-next *choices*)
  (is (= 0 (cl-fzy::choices-selection *choices*)))

  (cl-fzy::choices-prev *choices*)
  (is (= 0 (cl-fzy::choices-selection *choices*)))

  ;; No results
  (cl-fzy::choices-search *choices* "foobar")
  (is (= 0 (cl-fzy::choices-available *choices*)))
  (is (= 0 (cl-fzy::choices-selection *choices*)))

  ;; Different order due to scoring
  (cl-fzy::choices-search *choices* "ts")
  (is (= 2 (cl-fzy::choices-available *choices*)))
  (is (= 0 (cl-fzy::choices-selection *choices*)))
  (is (string= "test" (cl-fzy::choices-get *choices* 0)))
  (is (string= "tags" (cl-fzy::choices-get *choices* 1))))

(def-test-case test-choices-without-search
  ;; Before a search is run, it should return no results
  (is (= 0 (cl-fzy::choices-available *choices*)))
  (is (= 0 (cl-fzy::choices-selection *choices*)))
  (is (= 0 (cl-fzy::choices-size *choices*)))
  (is (null (cl-fzy::choices-get *choices* 0)))

  (cl-fzy::choices-add *choices* "test")

  (is (= 0 (cl-fzy::choices-available *choices*)))
  (is (= 0 (cl-fzy::choices-selection *choices*)))
  (is (= 1 (cl-fzy::choices-size *choices*)))
  (is (null (cl-fzy::choices-get *choices* 0))))

(def-test-case test-choices-unicode
  (cl-fzy::choices-add *choices* "Edmund Husserl - Méditations cartésiennes - Introduction a la phénoménologie.pdf")
  (cl-fzy::choices-search *choices* "e"))

(def-test-case test-choices-large-input
  (let ((N 100000) strings)
    (dotimes (i N)
      (push (format nil "~A" i) strings))
    (mapc #'(lambda (str) (cl-fzy::choices-add *choices* str)) strings)
    (cl-fzy::choices-search *choices* "12")
    (is (= 8146 (cl-fzy::choices-available *choices*)))
    (is (string= "12" (cl-fzy::choices-get *choices* 0)))))
