(in-package :cl-fzy-tests)

(def-suite test-arr2d :description "Test the 2d array class")

(in-suite test-arr2d)

(test set-row-and-get-row
  (let ((arr (make-instance 'cl-fzy::arr2d :arr-length 10))
         (new-row (make-array 10 :initial-element 0)))
    (cl-fzy::set-row arr 0 new-row)
    (is (equal (cl-fzy::get-row arr 0) new-row))))

(test set-aref-and-get-aref
  (let ((arr (make-instance 'cl-fzy::arr2d :arr-length 10))
         (new-row (make-array 10 :initial-element 0)))
    (cl-fzy::set-row arr 0 new-row)
    (cl-fzy::set-aref arr 0 0 42)
    (is (= (cl-fzy::get-aref arr 0 0) 42))))
