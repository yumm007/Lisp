
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

    
(defclass slot-share ()
  ((top-story :accessor slot-share-story :allocation :class
	      :initarg :top-story)))

(defclass 2d-object()
  ((x :accessor x :initform 1)
   (y :accessor y :initform 1)
   (orientation :accessor orientation :initform 1)))

(defclass 3d-object(2d-object)
  ((h :accessor h :initform 1)))

(defmethod 3d-class-area((obj 3d-object))
  (format t "space = ~a * ~a * ~a = ~a" (x obj) (y obj) (h obj) 
	  (* (x obj) (y obj) (h obj))))

(defun ext-class ()
  (let ((3d (make-instance '3d-object)))
	      (setf (h 3d) 5)
	      (setf (x 3d) 4)
	      (setf (y 3d) 2)
	      (describe 3d)
	      (3d-class-area 3d)))


