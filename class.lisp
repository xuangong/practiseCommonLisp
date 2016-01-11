(defclass back-account ()
  (customer-name
   balance))

(defparameter *account* (make-instance 'back-account))
(setf (slot-value *account* 'customer-name) "John Doe")
(setf (slot-value *account* 'balance) 1000)

(slot-value *account* 'customer-name)
(slot-value *account* 'balance)

(defclass bank-account ()
  ((customer-name
    :initarg :customer-name)
   (balance
    :initarg :balance
    :initform 0)))

(defparameter *account*
  (make-instance 'bank-account) 'customer-name)

(defvar *account-numbers* 0)

(defclass bank-account ()
  ((customer-name
    :initarg :customer-name
    :initform (error "Must supply a customer name."))
   (balance
    :initarg :balance
    :initform 0)
   (account-number
    :initform (incf *account-numbers*))))

(defclass bank-account ()
  ((customer-name
    :initarg :customer-name
    :initform (error "Must supply a customer name."))
   (balance
    :initarg :balance
    :initform 0)
   (account-number
    :initform (incf *account-numbers*))
   account-type))

(defclass bank-account ()
  ((customer-name
    :initarg :customer-name
    :initform (error "Must supply a customer name.")
    :accessor customer-name
    :documentation "Customer's name")
   (balance
    :initarg :balance
    :initform 0
    :reader balance
    :documentation "Current account balance")
   (account-number
    :initform (incf *account-numbers*)
    :reader account-number
    :documentation "Account number, unique within a bank.")
   (account-type
    :reader account-type
    :documentation "Type of account, one of :gold, :silver, or :bronze.")))

(defparameter *acct* (make-instance
                      'bank-account
                      :customer-name "Sally Sue"
                      :balance 1000))

;; (defgeneric balance (account))
;; (defgeneric (setf balance) (value account))
;; (defgeneric customer-name (account))
;; (defgeneric (setf customer-name) (value account))

(defmethod (setf balance) (value (account bank-account))
  (setf (slot-value account 'balance) value))
(setf (balance *acct*) 2016)
(defmethod balance ((account bank-account))
  (slot-value account 'balance))
(balance *acct*)

(defmethod (setf customer-name) (value (account bank-account))
  (setf (slot-value account 'customer-name) value))
(defmethod customer-name ((account bank-account))
  (slot-value account 'customer-name))



(defclass foo ()
  ((a :initarg :a :initform "A" :accessor a)
   (b :initarg :b :initform "B" :accessor b)))

(defclass bar (foo)
  ((a :initform (error "Must supply a value for a"))
   (b :initform :the-b :accessor the-b :allocation :class)))

;;; hierarchy
(defparameter *subcls* (make-instance 'bar :a "bar"))
(slot-value *subcls* 'b)
(setf (the-b *subcls*) "strTheB")
(slot-value *subcls* 'b)
