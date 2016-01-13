;;; C-c C-k - Compile and load the current buffer's file.
;;; C-c C-c - Compile and top-level form at point.
;;; M-n - slime-next-note
;;; M-p - slime-previous-note
;;; M-. - Edit the definition of the function called at point.
;;; M-, - Pop the definition stack to go back from a definition.
;;; C-c C-d C-d - Describe symbol
;;; C-c C-d h - Describe Help
;;; C-c C-d a - Describe in web documents
;;; C-c C-] - slime-close-all-parens-in-sexp
;;; C-c TAB - slime-complete-symbol
;;; C-c C-t - slime-toggle-fancy-trace
;;; C-c C-w C-c slime-who-calls
;;; C-c Ret macroexpand interactively
;;; C-c M-d - Disassemble a function.
;;; C-c I - slime-inspect
;;; C-h m - Holy grail : )

;;; loop macro tut
(loop for i below 5 collect i)
(loop :for i :below 5 :collect i)
(loop for i :from 1 :below 5 :collect i)
(loop :for i :from 1 :upto 5 :collect i)
(loop :for i :from 1 :downto -5 :collect i)
(loop :for i :from 1 :downto -15 :by 2 :collect i)
(loop :for i :from 1 :upto 15 :by 2 :collect i)

(defparameter *numbers* '(1 2 3 4 5 6 7 8 9 10))
(defparameter *rnumbers* '(10 9 8 7 6 5 4 3 2 1))
(defparameter *symbols* '(a b c d e))
(defparameter *pairs* '((a 1) (b 2) (c 3) (d 4) (e 5)))
(defparameter *triples* '((a 1 1) (b 2 4) (c 3 9) (d 4 16) (e 5 25)))

(loop :for i :in *numbers* :collect i)
(loop :for i :in *numbers* :collect (* i i))
(mapcar #'(lambda(x) (* x x)) *numbers*)

(loop :for i :in *numbers* :collect (list i))

(loop :for i :in *numbers*
   :for j :in *rnumbers*
   :collect (list i j))

(loop :for i :in *numbers*
   :for j :in *symbols*
   :collect (list i j))

(loop :for i :in *numbers* :do (print i))

(loop :for i :in *numbers* :do
   (print "hi")
   (print "more here")
   (print "meeee"))

(loop :for i :in *numbers* :do
   (loop :for j :in *symbols* :do (print (list i j))))

(loop :for i :in *numbers* :do
   (loop :for j :in *symbols* :collect j))

(loop :for i :in *numbers* :collect
   (loop :for j :in *symbols* :collect j))

(loop :for i :in *numbers* :append
   (loop :for j :in *symbols* :collect j))

(loop :for i :in *numbers* :if (oddp i) :collect i)

(loop :for i :in *numbers* :if (oddp i) :sum i)

(loop :for i :in *numbers* :if (oddp i) :sum i :into a)

(loop :for i :in *numbers* :if (oddp i) :sum i :into a :finally (return a))

(loop :for i :in *numbers*
   :if (oddp i) :sum i :into odds
   :else :sum i :into evens
   :finally (return (list odds evens)))

(loop :for i :being :the :elements :of #(1 2 3) :collect i)

;;; (loop :for i :being :the :hashkeys :of *hashmap* :collect i)

(loop :for i :in *numbers*
   :for s = (* i i)
   :sum s)

(loop :for (x y) :in *pairs* :collect (list x (* y y)))

(loop :for (x y z) :in *triples* :collect (list x (*y y) z))

(loop :for (x y) :in *triples* :collect (list x (* y y)))

(loop :for (x . y) :in *triples* :collect (list x y))
