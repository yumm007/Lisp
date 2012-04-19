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