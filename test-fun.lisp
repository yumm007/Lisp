(defvar *fun-name*)

(defmacro check (&rest body)
  `(progn 
     ,@(loop for f in body collect `(format t "~:[FAIL~;pass~] ~a ~a ~%" ,f *fun-name* ',f))))

(defmacro defuntest (test-name argc &rest body)
  `(defun ,test-name ,argc
     (let ((*fun-name* ',test-name))
       ,@body)))

(defuntest test-+ ()
  (check 
   (= (+ 1 2) 3)
   (= (+ 2 3) 4)))

(defuntest test-* ()
  (check
   (= (* 1 2) 2)
   (= (* 2 4) 8)))

(defuntest test-- ()
  (check
   (= (- 3 2) 1)
   (= (- 7 5) 2)))

(defun all-test ()
  (test-+)
  (test-*)
  (test-*))




