(defun foo (&key a (b 0) c)
	(list a b c))

(defun aoo (a b &optional c d )
	(list a b c d))

(defun boo (&rest body)
	(dolist (i body)
		(format t "~a~t" i))
	(format t "~%"))

(defun coo (x) (* x x))

(defun plot (fn min max step)
	(loop for i from min to max by step do
		(loop repeat (funcall fn i) do (format t "*"))
		(format t "~%")))

(defun test-any (cmp-fn x y)
	(funcall cmp-fn x y))


(defun test-+ ()
	(and
		(= (+ 1 2) 3)
		(= (+ 4 5) 9)))

(defun test-- ()
	(and 
		(= (- 1 2) -1)
		(= (- 2 5) -3)))

(defun test-al (&rest fns)
	(dolist (fn fns)
		(funcall fn)))

(defun printn99()
	(loop for i from 1 to 9 by 1 do 
		(loop for j from 1 to i by 1 do
			(format t "~a*~a=~2a~t" i j (* i j)))
		(format t "~%")))

(defun make-expr (filter value)
	`(equal (getf cd ,filter) ,value))

(defun makes (filter value)
	(make-expr filter value))














