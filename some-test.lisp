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

;;不能将make-expr定义成宏的原因是，宏生成的代码返回后，会被再求值一次
;;所有会出现cd未定义的错误

(defun make-expr (filter value)
	`(equal (getf cd ,filter) ,value))

(defun makes (filter value)
	(make-expr filter value))

(defvar *name* nil)

;;动态变量
;;动态变量的值可以在某个作用域以及它的子域内发生更改，
;;但退出这个作用域后自动恢复原值
(defun space1()
	(let ((*name* "Dinnel"))
		(format t "*name* = ~a~%" *name*)
		(let ((*name* "Lukas"))
			(format t "*name* = ~a~%" *name*))
		(format t "after change *name* = ~a~%" *name*)))

;;闭包
;;let 表达式的执行体--即匿名函数将作为let的返回值，赋值给 *fn*
;;所以需要使用 (funcall *fn*)的方式使用 *fn*；
;;因为匿名函数使用了其左右范围外的变量 count，导致count可以脱离
;;其原生作用域，成为自由变量，且只能由这个匿名函数访问。
;;这时，此匿名函数-即*fn*就称为闭包。因为它封闭来对变量i的访问。

;;简而言之，闭包就是一个可以访问其私有静态变量的匿名函数。
;;其存在直到没有变量绑定到此闭包；
;;如下定义来2个闭包，fn0和fn1，它们都有自己的count，不会重叠。

(defvar *fn0*
	(let ((count 0)) #'(lambda () (setf count (1+ count)))))

(defvar *fn1*
	(let ((count 0)) #'(lambda () (setf count (1+ count)))))


;;不知道如下是什么意思?
;;定义一个函数，其返回值为一个列表，列表的元素为三个函数对象，它们分别
;;增加和减少或者返回count1的值，通过函数 (funcall (first (xxx)))调用之
;;但不知为何不能累加，闭包没有起作用.
(defun xxx ()
	(let ((count1 0))
		(list 
			#'(lambda () (setf count1 (1+ count1)))
			#'(lambda () (decf count1))
			#'(lambda () count1))))















