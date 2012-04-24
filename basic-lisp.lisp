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


;;第二章 上机练习题 木头人

(defparameter *nerd-states* '("sleeping" "eating" "wait-for-compter" "programming" "debugging"))
;返回下一个状态
(defun nerdus (cur)
  (let ((x (cadr (member cur *nerd-states* :test #'equal))))
	(if (not x)
	    (car *nerd-states*)
	    x)))
;;跳过休眠状态
(defun sleeping-nerd (cur)
  (let ((*nerd-states* (remove "sleeping" *nerd-states* :test #'equal)))
    (nerdus cur)))

;;跳过一级状态
(defun nerd-on-caffeing (cur)
  (nerdus (nerdus cur)))

;;以当前的行为模式（即以上的三个切换状态方式）， 从一个状态到下一个状态需要几步
(defparameter n 0)
(defun nerd-status-transfer (cur-status tar-status nerd-fun)
  (if (equal cur-status tar-status)
      (format t "~a step form ~a to ~a ~%" n cur-status tar-status)
      (progn
	(let ((n (1+ n)))
	      (nerd-status-transfer (funcall nerd-fun cur-status) tar-status nerd-fun)))))



