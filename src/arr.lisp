(in-package :cl-fzy)
(defclass arr2d ()
  ((data :accessor data :initarg :data)
   (arr-length :accessor arr-length :initarg :arr-length)))

(defmethod initialize-instance :after ((obj arr2d) &key)
  (unless (slot-boundp obj 'data)
    (setf (slot-value obj 'data) (make-array (arr-length obj)))))

(defmethod set-row ((obj arr2d) index new-row)
  (check-type index integer)
  (check-type new-row array)
  (setf (aref (data obj) index) new-row))

(defmethod get-row ((obj arr2d) index)
  (check-type index integer)
  (aref (data obj) index))

(defmethod set-aref ((obj arr2d) index1 index2 value)
  (setf (aref (aref (data obj) index1) index2) value))

(defmethod get-aref ((obj arr2d) index1 index2)
  (aref (aref (data obj) index1) index2))
