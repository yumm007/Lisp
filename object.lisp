
(defclass point3d()(x y z))

(defun make-object ()
  (let ((a-point (make-instance 'point3d)))
    (setf (slot-value a-point 'x) 21)
    (setf (slot-value a-point 'y) 30)
    (setf (slot-value a-point 'z) 0)
    (format t "x = ~a; y = ~a; z = ~a~%" (slot-value a-point 'x) 
	    (slot-value a-point 'y) (slot-value a-point 'z) )))

(defclass point3dn () (
		       (x :reader get-x :writer set-x)
		       (y :reader get-y :writer set-y)
		       (z :accessor point-z :initform 0 :initarg :z)
		       ))

(defun access-object ()
  (let ((a-point (make-instance 'point3dn)))
    (set-x 3 a-point)
    (setf (point-z a-point) 21)
    (format t "x = ~a; z = ~a~%" (get-x a-point) (point-z a-point)))
  (let ((b-point (make-instance 'point3dn :z 3)))
    (format t "init z value = ~a~%" (point-z b-point)))
  (let ((c-point (make-instance 'point3dn)))
    (format t "default z value = ~a~%" (point-z c-point))))

    
