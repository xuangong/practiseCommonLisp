;; C-c C-k  - Compile and load the current buffer's file
;; C-c C-c  - Compile the top level form at point

;; M-n      - Slime-next-note
;; M-p      - Slime-previous-note

(defun test(x)
  (* x x))

(defun test-some (&rest nums)
  "Calls rest on the args passed in."
  (mapcar #'test nums))

;; M-.     - Edit the definition of the function called at point.
;; M-,     - Pop the definition stack to go back from a definition.
(defun anther ()
  (test-some 1 2 3 4 5))

;; C-c C-d d    - Describe symbol
;; C-c C-d C-d  - Describe symbol
(+ 1 2 3)

(let ((a 1))
  (cond
    ((> a 0) (print "hi"))
    ((< a 0) (print "bye"))
    (t (print "oh"))))

(eq #'test (car (list #'test)))

(setq test 2)

(test test)

(symbol-function 'test)

(setq x #'append)

(eq (symbol-value 'x) (symbol-function 'append))

(apply #'+ '(1 2))

(apply (symbol-function '+) '(1 2))

(apply #'(lambda(x y) (+ x y)) '(1 2))

(apply #'+ 1 '(2))

(apply #'* '())

(funcall #'+ 1 2)

(mapcar #'(lambda (x) (+ x 10))
        '(1 2 3))

(mapcar #'+
        '(1 2 3)
        '(10 100 1000))

(sort '(1 3 2 4 5 12 7) #'<)

(remove-if #'evenp '(1 2 3 4 5 6 7))

(defun our-remove-if (fn 1st)
  (if (null 1st)
      nil
      (if (funcall fn(car 1st))
          (our-remove-if fn (cdr 1st))
          (cons (car 1st) (our-remove-if fn (cdr 1st))))))

(our-remove-if #'evenp '(1 2 3 4 5 6 7))

(defun behave (animal)
  (funcall(get animal 'behaviour) (get animal 'parameter)))

(setf (get 'dog 'behaviour)
      #'(lambda (param)
          (print param)))

(setf (get 'dog 'parameter)
      "hello world")

(get 'dog 'behaviour)
(get 'dog 'parameter)

(behave 'dog)

(let ((y 7))
  (defun scope-test (x)
    (list x y)))

(defun list+ (1st n)
  (mapcar #'(lambda (x) (+ x n))
          1st))

(list+ '(1 2 3) 10)

(defun make-adder (n)
  #'(lambda (x) (+ x n)))

(setq add2 (make-adder 2)
      add10 (make-adder 10))

(funcall add2 5)
(funcall add10 3)

(defun make-adderb (n)
  #'(lambda (x &optional change)
      (if change
          (setq n x)
          (+ x n))))

(setq addx (make-adderb 1))

(funcall addx 3 t)

(defun make-dbms (db)
  (list
   #'(lambda (key)
       (cdr (assoc key db)))
   #'(lambda (key val)
       (push (cons key val) db)
       key)
   #'(lambda (key)
       (setf db (delete key db :key #'car))
       key)))

(setq cities (make-dbms '((boston . us) (paris . france))))

(funcall (car cities) 'boston)
(funcall (second cities) 'london 'england)
(funcall (car cities) 'london)
(funcall (car cities) 'paris)

(defun lookup (key db)
  (funcall (car db) key))

(labels ((inc (x) (+ 1 x)))
  (inc 3))

(defun count-instances (obj 1sts)
  (labels ((instances-in (1st)
             (if (consp 1st)
                 (+ (if (eq (car 1st) obj) 1 0)
                    (instances-in (cdr 1st)))
                 0)))
    (mapcar #'instances-in 1sts)))

(count-instances 'a '((a b c) (d a r p a) (d a r) (a a)))

(defun our-length (1st)
  (if (null 1st)
      0
      (1+ (our-length (cdr 1st)))))

(defun our-find-if (fn 1st)
  (if (funcall fn (car 1st))
      (car 1st)
      (our-find-if fn (cdr 1st))))

(defun our-length-cl-style (1st)
  (labels ((rec (1st acc)
             (if (null 1st)
                 acc
                 (rec (cdr 1st) (1+ acc)))))
    (rec 1st 0)))

(our-length '(1 2 3 4))

(defun triangle (n)
  (labels ((tri (c n)
             (declare (type fixnum n c))
             (if (zerop n)
                 c
                 (tri (the fixnum (+ n c))
                      (the fixnum (- n 1))))))
    (tri 0 n)))

(triangle 3)

(defun foo (x) (1+ x))

(compiled-function-p #'foo)

(compile 'foo)

(compiled-function-p #'foo)

(compile nil '(lambda (x) (+ x 2)))

(progn (compile 'bar '(lambda (x) (* x 3)))
       (compiled-function-p #'bar))

(let ((y 2))
  (defun foo (x) (+ x y)))

(defun bad-reverse(1st)
  (let* ((len (length 1st))
         (ilimit (truncate (/ len 2))))
    (do ((i 0 (1+ i))
         (j (1- len) (1- j)))
        ((>= i ilimit))
      (rotatef (nth i 1st) (nth j 1st)))))

(defun bad-reverse-inside(1st)
  (let* ((len (length 1st))
         (ilimit (truncate (/ len 2))))
    (do ((i 0 (1+ i))
         (j (1- len) (1- j)))
        ((>= i ilimit))
      (format t "~a and ~a" i j))))

(setq 1st '(a b c d e f))
(bad-reverse 1st)

1st

(nth 2 1st)

(defun good-reverse (1st)
  (labels ((rev (1st acc)
             (if (null 1st)
                 acc
                 (rev (cdr 1st) (cons (car 1st) acc)))))
    (rev 1st nil)))

(good-reverse 1st)

(proclaim ’(inline last1 single append1 conc1 mklist))
(defun last1 (lst)
  (car (last lst)))
(defun single (lst)
  (and (consp lst) (not (cdr lst))))
(defun append1 (lst obj)
  (append lst (list obj)))
(defun conc1 (lst obj)
  (nconc lst (list obj)))
(defun mklist (obj)
  (if (listp obj) obj (list obj)))

(defun longer (x y)
  (labels ((compare (x y)
             (and (consp x)
                  (or (null y)
                      (compare (cdr x) (cdr y))))))
    (if (and (listp x) (listp y))
        (compare x y)
        (> (length x) (length y)))))

(defun filter (fn lst)
  (let ((acc nil))
    (dolist (x lst)
      (let ((val (funcall fn x)))
        (if val (push val acc))))
    (nreverse acc)))

(defun group (source n)
  (if (zerop n) (error ”zero length”))
  (labels ((rec (source acc)
             (let ((rest (nthcdr n source)))
               (if (consp rest)
                   (rec rest (cons (subseq source 0 n) acc))
                   (nreverse (cons source acc))))))
    (if source (rec source nil) nil)))

(setq vals '(1 2 3))
(filter #'(lambda (x) (+ x 2)) vals)

(group vals 3)

(defun flatten (x)
  (labels ((rec (x acc)
             (cond ((null x) acc)
                   ((atom x) (cons x acc))
                   (t (rec (car x) (rec (cdr x) acc))))))
    (rec x nil)))

(defun prune (test tree)
  (labels ((rec (tree acc)
             (cond ((null tree) (nreverse acc))
                   ((consp (car tree))
                    (rec (cdr tree)
                         (cons (rec (car tree) nil) acc)))
                   (t (rec (cdr tree)
                           (if (funcall test (car tree))
                               (rec tree nil)))
                      acc
                      (cons (car tree) acc)))))))

(defun find2 (fn lst)
  (if (null lst)
      nil
      (let ((val (funcall fn (car lst))))
        (if val
            (values (car lst) val)
            (find2 fn (cdr lst))))))

(defun before (x y lst &key (test #’eql))
  (and lst
       (let ((first (car lst)))
         (cond ((funcall test y first) nil)
               ((funcall test x first) lst)
               (t (before x y (cdr lst) :test test))))))

(defun after (x y lst &key (test #’eql))
  (let ((rest (before y x lst :test test)))
    (and rest (member x rest :test test))))

(defun duplicate (obj lst &key (test #’eql))
  (member obj (cdr (member obj lst :test test))
          :test test))

(defun split-if (fn lst)
  (let ((acc nil))
    (do ((src lst (cdr src)))
        ((or (null src) (funcall fn (car src)))
         (values (nreverse acc) src))
      (push (car src) acc))))

(defun map0-n (fn n)
  (mapa-b fn 0 n))

(defun map1-n (fn n)
  (mapa-b fn 1 n))

(defun mapa-b (fn a b &optional (step 1))
  (do ((i a (+ i step))
       (result nil))
      ((> i b) (nreverse result))
    (push (funcall fn i) result)))

(defun map-> (fn start test-fn succ-fn)
  (do ((i start (funcall succ-fn i))
       (result nil))
      ((funcall test-fn i) (nreverse result))
    (push (funcall fn i) result)))

(defun mappend (fn &rest lsts)
  (apply #’append (apply #’mapcar fn lsts)))

(defun mapcars (fn &rest lsts)
  (let ((result nil))
    (dolist (lst lsts)
      (dolist (obj lst)
        (push (funcall fn obj) result)))
    (nreverse result)))

(defun rmapcar (fn &rest args)
  (if (some #’atom args)
      (apply fn args)
      (apply #’mapcar
               #’(lambda (&rest args)
                   (apply #’rmapcar fn args))
                 args)))

(defun readlist (&rest args)
  (values (read-from-string
           (concatenate ’string ”(”
                                  (apply #’read-line args)
                                  ”)”))))

(defun prompt (&rest args)
  (apply #’format *query-io* args)
  (read *query-io*))

(defun break-loop (fn quit &rest args)
  (format *query-io* ”Entering break-loop.’~%”)
  (loop
    (let ((in (apply #’prompt args)))
      (if (funcall quit in)
          (return)
          (format *query-io* ”~A~%” (funcall fn in))))))

(defun mkstr (&rest args)
  (with-output-to-string (s)
    (dolist (a args) (princ a s))))

(defun symb (&rest args)
  (values (intern (apply #’mkstr args))))

(defun reread (&rest args)
  (values (read-from-string (apply #’mkstr args))))

(defun explode (sym)
  (map ’list #’(lambda (c)
                 (intern (make-string 1
                                      :initial-element c)))
        (symbol-name sym)))

(defvar *!equivs* (make-hash-table))

(defun ! (fn)
  (or (gethash fn *!equivs*) fn))

(defun def! (fn fn!)
  (setf (gethash fn *!equivs*) fn!))

(defun memoize (fn)
  (let ((cache (make-hash-table :test #’equal))))
  #’(lambda (&rest args)
      (multiple-value-bind (val win) (gethash args cache)
        (if win val
            (setf (gethash args cache)
                  (apply fn args))))))

(defun compose (&rest fns)
  (if fns
      (let ((fn1 (car (last fns)))
            (fns (butlast fns)))
        #’(lambda (&rest args)
            (reduce #’funcall fns
                      #’identity))
        :from-end t
        :initial-value (apply fn1 args))))



(funcall (compose #'1+ #'find-if) #'oddp '(2 3 4))

(defun foo (&key a b c) (list a b c))

(defvar *x* 10)

(defun foo () (format t "X: ~d~%" *x*))

(defun bar ()
  (foo)
  (let ((*x* 20)) (foo))
  (foo))

(defmacro my-when (condition &rest body)
  `(if ,condition (progn ,@body)))

(my-when (> 3 2)
         (format t "Yup")
         (format t "Nope"))

(when (> 3 0)
  (format t "Yup")
  (format t "Nope"))

(do ((n 0 (1+ n))
     (cur 0 next)
     (next 1 (+ cur next)))
    ((= 10 n) cur))


(let ((in (open "name.txt" :if-does-not-exist nil)))
  (when in
    (loop for line = (read-line in nil)
       while line do (format t "~a~%" line))
    (close in)))

(defun component-present-p (value)
  (and value (not (eql value :unspecific))))

(defun directory-pathname-p (p)
  (and
   (not (component-present-p (pathname-name p)))
   (not (component-present-p (pathname-type p)))
   p))

(defun pathname-as-directory (name)
  (let ((pathname (pathname name)))
    (when (wild-pathname-p pathname)
      (error "Can't reliably convert wild pathnames."))
    (if (not (directory-pathname-p name))
        (make-pathname
         :directory (append (or (pathname-directory pathname) (list :relative))
                            (list (file-namestring pathname)))
         :name nil
         :type nil
         :defaults pathname)
        pathname)))

(defun pathname-as-file (name)
  (let ((pathname (pathname name)))
    (when (wild-pathname-p pathname)
      (error "Can't reliably convert wild pathnames."))
    (if (directory-pathname-p name)
        (let* ((directory (pathname-directory pathname))
               (name-and-type (pathname (first (last directory)))))
          (make-pathname
           :directory (butlast directory)
           :name (pathname-name name-and-type)
           :type (pathname-type name-and-type)
           :defaults pathname))
        pathname)))

(defun file-exists-p (pathname)
  #+ (or sbcl lispworks openmcl)
  (probe-file pathname)

  #+ (or allegro cmu)
  (or (probe-file (pathname-as-directory pathname))
      (probe-file pathname))

  #+clisp
  (or (ignore-errors
        (probe-file (pathname-as-file pathname)))
      (ignore-errors
        (let ((directory-form (pathname-as-directory pathname)))
          (when (ext :probe-directory directory-form)
            directory-form))))

  #- (or sbcl cmu lispworks openmcl allegro clisp)
  (error "list-directory not implementd"))

(defun directory-wildcard (dirname)
  (make-pathname
   :name :wild
   :type #-clisp :wild #+clisp nil
   :defaults (pathname-as-directory dirname)))

#+clisp
(defun clisp-subdirectories-wildcard (wildcard)
  (make-pathname
   :directory (append (pathname-directory wildcard) (list :wild))
   :name nil
   :type nil
   :defaults wildcard))

(defun list-directory (dirname)
  (when (wild-pathname-p dirname)
    (error "Can only list concrete directory names."))
  (let ((wildcard (directory-wildcard dirname)))

    #+ (or sbcl cmu lispworks)
    (directory wildcard)

    #+openmcl
    (directory wildcard :directories t)

    #+allegro
    (directory wildcard :directories-are-files nil)

    #+clisp
    (nconc
     (directory wildcard)
     (directory (clisp-subdirectories-wildcard wildcard)))

    #- (or sbcl cmu lispworks openmcl allegro clisp)
    (error "list-directory not implemented")))

;;; dirname : string
;;; fn : function object
;;; when :directory is t, it means :test as function object applied on each file
;;; e.g. (walk-directory "/home/xuangong/" #'print)
(defun walk-directory (dirname fn &key directories (test (constantly t)))
  (labels
      ((walk (name)
         (cond
           ((directory-pathname-p name)
            (when (and directories (funcall test name))
              (funcall fn name))
            (dolist (x (list-directory name)) (walk x)))
           ((funcall test name) (funcall fn name)))))
    (walk (pathname-as-directory dirname))))


(defun collect-leaves (tree)
  (let ((leaves ()))
    (labels ((walk (tree)
               (cond
                 ((null tree))
                 ((atom tree) (push tree leaves))
                 (t (walk (car tree))
                    (walk (cdr tree))))))
      (walk tree))
    (nreverse leaves)))
;;; inorder traverse of a tree
(defparameter *tree* '(1 (2 (4) (5 () 8)) (3 6 7)))

(with-slots (x y z) foo (list x y z))

(dotimes (i 10)
  (let ((answer (random 100)))
    (print answer)
    (if (> answer 50) (return))))

(tagbody
 a (print 'a) (if (zerop (random 2)) (go c))
 b (print 'b) (if (zerop (random 2)) (go a))
 c (print 'c) (if (zerop (random 2)) (go b)))

;;; ----------block return--------
(defun foo ()
  (format t "Entering foo~%")
  (block a
    (format t " Entering BLOCK~%")
    (bar #'(lambda () (return-from a)))
    (format t " Leaving BLOCK~%"))
  (format t "Leaving foo~%"))

(defun bar (fn)
  (format t " Entering bar~%")
  (baz fn)
  (format t " Leaving bar~%"))

(defun baz (fn)
  (format t "  Entering baz~%")
  (funcall fn)
  (format t "  Leaving baz~%"))

;;; --------throw catch--------
(defparameter *obj* (cons nil nil))

(defun foo ()
  (format t "Entering foo~%")
  (catch *obj*
    (format t " Entering CATCH~%")
    (bar)
    (format t " Leaving CATCH~%"))
  (format t "Leaving foo~%"))

(defun bar ()
  (format t " Entering bar~%")
  (baz)
  (format t " Leaving bar~%"))

(defun baz ()
  (format t "  Entering baz~%")
  (throw *obj* nil)
  (format t "  Leaving baz~%"))

;;; ---------multiple-value----------
(defparameter *x* nil)
(defparameter *y* nil)
(setf (values *x* *y*) (floor (/ 84 34)))

(multiple-value-list (values 1 2))
(values-list (multiple-value-list (values 1 2)))

;;; ---------eval-when----------

;; (in-package "some package")
;;; :compile-toplevel 在编译器求值其子形式
;;; :load-toplevel 把子形式作为顶层形式来编译
;;; 加载fasl设置package是由于:load-toplevel
;;; 加载源码设置package是由于:execute
;; (eval-when (:compile-toplevel :load-toplevel :execute)
;;   (setq *package* (sb-int:find-undeleted-package-or-lose "some package")))

;;; ------------PACKAGE SYMBOL-------------
(find-symbol "setf")

(defpackage :com.gigamonkeys.email-db
  (:use :common-lisp))
;;; 等价于
(defpackage "COM.GIGAMONKEYS.EMAIL-DB"
  (:use "COMMON-LISP"))

;;; 修改*package*的值
;; 编译的时候不会改变*package*的值但是划定了下面函数的包范围
(in-package :common-lisp-user)
(defun hello-world ()
  (format t "hello world."))

(in-package :com.gigamonkeys.email-db)
(defun hello-world ()
  (format t "gigamonkeys hello world."))


;;; text可以使用common-lisp和email导出的符号
(defpackage :com.gigamonkeys.text-db
  (:use :common-lisp)
  (:export :open-db :save :store))

(defpackage :com.gigamonkeys.email-db
  (:use :common-lisp :com.gigamonkeys.text-db))


;;; 导出单独的名字空间
(defpackage :com.gigamonkeys.email-db
  (:use :common-lisp :com.gigamonkeys.text-db)
  (:import-from :com.acme.email :parse-email-address))

;;; shadow表示build-index这个符号不用继承来的名字空间
(defpackage :com.gigamonkeys.email-db
  (:use :common-lisp :com.gigamonkeys.text-db :com.acme.text)
  (:import-from :com.acme.email :parse-email-address)
  (:shadow :build-index))

;;; 发生继承来的名字冲突时，比如save冲突后，用shadowing-import-from来指定使用com.gigamonkeys.text-db的save符号
(defpackage :com.gigamonkeys.email-db
  (:use :common-lisp :com.gigamonkeys.text-db :com.acme.text)
  (:import-from :com.acme.email :parse-email-address)
  (:shadow :build-index)
  (:shadowing-import-from :com.gigamonkeys.text-db :save))
