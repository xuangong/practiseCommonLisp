(defvar *test-name* nil)

(defmacro deftest (name parameters &body body)
  "Define a test function. Within a test function we can call
other test functions or use 'check' to run individual test
cases."
  `(defun ,name ,parameters
     (let ((*test-name* (append *test-name* (list ',name))))
       ,@body)))

(defmacro with-gensyms ((&rest names) &body body)
  `(let ,(loop for n in names collect `(,n (gensym)))
     ,@body))

(defmacro check (&body forms)
  "Run each expression in 'forms' as a test case."
  `(combine-results
     ,@(loop for f in forms collect `(report-result ,f ',f))))

(defmacro combine-results (&body forms)
  "Combine the results (as booleans) of evaluating 'forms' in order."
  (with-gensyms (result)
    `(let ((,result t))
       ,@(loop for f in forms collect `(unless ,f (setf ,result nil)))
       ,result)))

(defun report-result (result form)
  "Report the results of a single test case. Called by 'check'."
  (format t "~:[FAIL~;PASS~] ... ~a: ~a~%" result *test-name* form)
  result)

(deftest test-+ ()
  (check
    (= (+ 1 2) 3)
    (= (+ 1 2 3) 6)
    (= (+ -1 -2) -4)))

(deftest test*/ ()
  (check
    (= (* 1 2) 2)
    (= (* 1 2 3) 3)
    (= (/ -2 -1) 2)))

(deftest test-arithmetic ()
  (combine-results
    (test-+)
    (test*/)))

;; (combine-results
;;   (test-+))

;; (check
;;   (= (+ 1 2) 3)
;;   (= (+ 1 2 3) 6)
;;   (= (+ -1 -2) -4))

;; (DEFUN TEST-ARITHMETIC ()
;;   (LET ((*TEST-NAME* (APPEND *TEST-NAME* (LIST 'TEST-ARITHMETIC))))
;;     (COMBINE-RESULTS
;;       (TEST -+))))
