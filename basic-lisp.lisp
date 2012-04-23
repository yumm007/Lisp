(defparameter *nur* 0)

;; 计算一个lisp表达式的深度， 方法失败
;; 其实算括号时最简单的，但是不知在Lisp中怎样做
(defun check-nur (exp)
  (if (atom exp)
      (incf *nur*)
      (check-nur(cdr exp))))

;;统计一个表中指定字符出现的次数
;;似乎有点复杂
(defun _check-char (exp c)
  (if (eq exp c)
      (incf *nur*)
      (if (not (atom exp))
	  (progn
	    (_check-char (car exp) c)
	    (_check-char (cdr exp) c)))))

(defun check-char(exp c)
  (setf *nur* 0)
  (_check-char exp c)
  *nur*)

;;写一个函数，需要一个表作为参数。取出该表中的元素也是表的那些元素，若不含表则提示之
(defun find-is-list(exp)
  (dolist (x exp)
    (if (atom x)
	(format t "~a IS NOT LIST~%" x)
	(format t "~a ~%" x))))

(defmacro my-listp (exp)
  `(listp ,@exp))
