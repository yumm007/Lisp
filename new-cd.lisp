(defvar *db* nil)

(defun make-cd (title artist rating ripped)
	(list :title title :artist artist :rating rating :ripped ripped))

(defun read-filter(filter)
	(format *query-io* "~a:~13t" filter)
	(force-output *query-io*)
	(read-line *query-io*))

(defun add-record()
	(push (make-cd 
		(read-filter "Title")
		(read-filter "Artist")
		(or (parse-integer (read-filter "Rating")) 0)
		(y-or-n-p "Ripped")) *db*))

(defun dump-db()
	(dolist (cd *db*)
		(format t "~{~a~13t~a~%~}~%" cd)))

;;why is make-expr not macro? 
(defun make-expr(filter value)
	`(equal (getf cd ,filter) ,value))

(defun make-exprs(body)
	(loop while body
		collect (make-expr (pop body) (pop body))))

(defmacro where (&rest filter)
	`#'(lambda (cd) (and ,@(make-exprs filter))))

;; select (where :artist Dinners)
(defun select (where-filter)
	(remove-if-not where-filter *db*))















