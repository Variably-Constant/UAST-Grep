;;;; Common Lisp Test File for UAST-Grep
;;;; Tests: functions, macros, classes, conditions, packages

;;; Package definition
(defpackage :UAST-Grep-test
  (:use :common-lisp)
  (:export #:*max-items*
           #:*default-name*
           #:person
           #:make-person
           #:person-name
           #:person-age
           #:processor
           #:process-items
           #:calculate-sum
           #:transform))

(in-package :UAST-Grep-test)

;;; Constants and special variables
(defconstant +version+ "1.0.0"
  "Version of the UAST-Grep test module")

(defparameter *max-items* 100
  "Maximum number of items to process")

(defparameter *default-name* "UAST-Grep"
  "Default name for the processor")

(defvar *global-counter* 0
  "Global counter for processed items")

;;; Simple function definitions
(defun calculate-sum (a b)
  "Calculate the sum of two numbers."
  (+ a b))

(defun calculate-product (a b)
  "Calculate the product of two numbers."
  (* a b))

;;; Function with optional and keyword parameters
(defun greet (&optional (name "World") &key (greeting "Hello") loud)
  "Generate a greeting message."
  (let ((message (format nil "~A, ~A!" greeting name)))
    (if loud
        (string-upcase message)
        message)))

;;; Function with rest parameters
(defun sum-all (&rest numbers)
  "Sum all provided numbers."
  (reduce #'+ numbers :initial-value 0))

;;; Transform function with cond
(defun transform (item)
  "Transform an item based on its type."
  (cond
    ((listp item)
     (mapcar (lambda (x) (* x 2)) item))
    ((stringp item)
     (string-upcase item))
    ((numberp item)
     (* item 2))
    (t item)))

;;; Higher-order functions
(defun map-items (fn items)
  "Apply function fn to each item in items."
  (mapcar fn items))

(defun filter-items (predicate items)
  "Filter items using predicate."
  (remove-if-not predicate items))

(defun reduce-items (fn items initial-value)
  "Reduce items using fn starting with initial-value."
  (reduce fn items :initial-value initial-value))

;;; Structure definition
(defstruct (person (:constructor make-person (name age &optional email)))
  "Structure representing a person."
  (name "" :type string)
  (age 0 :type integer)
  (email nil :type (or string null))
  (active t :type boolean))

;;; Class definition using CLOS
(defclass processor ()
  ((name :initarg :name
         :initform *default-name*
         :accessor processor-name
         :documentation "Name of the processor")
   (count :initform 0
          :accessor processor-count
          :documentation "Number of items processed")
   (cache :initform (make-hash-table :test #'equal)
          :accessor processor-cache
          :documentation "Cache for processed items"))
  (:documentation "A class for processing items."))

;;; Generic function and methods
(defgeneric process-items (processor items)
  (:documentation "Process a list of items using the processor."))

(defmethod process-items ((processor processor) items)
  "Default implementation of process-items."
  (let ((results '()))
    ;; Loop through items
    (dolist (item items)
      (push (transform item) results)
      (incf (processor-count processor)))
    ;; Return reversed results (correct order)
    (nreverse results)))

(defgeneric log-message (processor message)
  (:documentation "Log a message from the processor."))

(defmethod log-message ((processor processor) message)
  "Log a message with processor name."
  (format t "[~A] ~A~%" (processor-name processor) message))

;;; Method with :before, :after, :around
(defmethod process-items :before ((processor processor) items)
  (log-message processor "Starting processing..."))

(defmethod process-items :after ((processor processor) items)
  (log-message processor (format nil "Processed ~D items" (processor-count processor))))

;;; Extended class
(defclass data-processor (processor)
  ((cache-size :initform 0
               :accessor processor-cache-size))
  (:documentation "Extended processor with caching capabilities."))

(defmethod process-items :around ((processor data-processor) items)
  "Override processing with caching."
  (let ((results (call-next-method)))
    (setf (processor-cache-size processor) (length results))
    results))

;;; Condition (exception) handling
(define-condition processing-error (error)
  ((item :initarg :item :reader processing-error-item)
   (reason :initarg :reason :reader processing-error-reason))
  (:report (lambda (condition stream)
             (format stream "Processing error for ~A: ~A"
                     (processing-error-item condition)
                     (processing-error-reason condition)))))

(defun risky-operation (filename)
  "Perform a risky file operation with error handling."
  (handler-case
      (with-open-file (stream filename :direction :input)
        (let ((content (make-string (file-length stream))))
          (read-sequence content stream)
          content))
    (file-error (e)
      (format t "File error: ~A~%" e)
      nil)
    (error (e)
      (format t "General error: ~A~%" e)
      nil)))

;;; Macros
(defmacro with-timing ((&optional label) &body body)
  "Execute body and report execution time."
  (let ((start (gensym "START"))
        (result (gensym "RESULT")))
    `(let ((,start (get-internal-real-time)))
       (let ((,result (progn ,@body)))
         (format t "~@[~A: ~]~,3F seconds~%"
                 ,label
                 (/ (- (get-internal-real-time) ,start)
                    internal-time-units-per-second))
         ,result))))

(defmacro when-let ((var test) &body body)
  "Execute body with var bound to test result if non-nil."
  `(let ((,var ,test))
     (when ,var
       ,@body)))

(defmacro aif (test then &optional else)
  "Anaphoric if - binds test result to 'it'."
  `(let ((it ,test))
     (if it ,then ,else)))

;;; Loop macro examples
(defun loop-examples ()
  "Demonstrate various loop forms."
  ;; Simple counting loop
  (loop for i from 1 to 10 collect i)

  ;; Loop with multiple clauses
  (loop for item in '(1 2 3 4 5)
        for doubled = (* item 2)
        collect doubled)

  ;; Loop with hash table
  (let ((ht (make-hash-table)))
    (setf (gethash :a ht) 1
          (gethash :b ht) 2)
    (loop for key being the hash-keys of ht
          using (hash-value val)
          collect (cons key val)))

  ;; Loop with conditionals
  (loop for x from 1 to 100
        when (evenp x) collect x into evens
        when (oddp x) collect x into odds
        finally (return (values evens odds))))

;;; Multiple values
(defun get-status ()
  "Return multiple values representing status."
  (values :ok 200 "Success"))

(defun use-multiple-values ()
  "Demonstrate multiple-value-bind."
  (multiple-value-bind (status code message) (get-status)
    (format t "Status: ~A, Code: ~D, Message: ~A~%"
            status code message)))

;;; Lambda and closures
(defun make-counter (&optional (initial 0))
  "Create a counter closure."
  (let ((count initial))
    (lambda (&optional (increment 1))
      (incf count increment))))

(defun make-adder (n)
  "Create a function that adds n to its argument."
  (lambda (x) (+ x n)))

;;; Special forms demonstration
(defun control-flow-examples (value)
  "Demonstrate various control flow forms."
  ;; If-then-else
  (if (> value 0)
      "positive"
      "non-positive")

  ;; Case
  (case value
    (1 "one")
    (2 "two")
    ((3 4 5) "three to five")
    (otherwise "other"))

  ;; Typecase
  (typecase value
    (integer "integer")
    (string "string")
    (list "list")
    (t "unknown"))

  ;; Cond
  (cond
    ((< value 0) "negative")
    ((= value 0) "zero")
    ((< value 10) "single digit")
    (t "multiple digits")))

;;; Let and let* forms
(defun let-examples ()
  "Demonstrate let and let* forms."
  ;; Parallel binding
  (let ((x 1)
        (y 2)
        (z 3))
    (+ x y z))

  ;; Sequential binding
  (let* ((x 1)
         (y (+ x 1))
         (z (+ x y)))
    (list x y z)))

;;; Property lists
(defun plist-examples ()
  "Demonstrate property list operations."
  (let ((data '(:name "Alice" :age 30 :active t)))
    (list
     (getf data :name)
     (getf data :age)
     (getf data :missing "default"))))

;;; Format examples
(defun format-examples ()
  "Demonstrate format directive usage."
  (format nil "Integer: ~D" 42)
  (format nil "Float: ~,2F" 3.14159)
  (format nil "Padded: ~5,'0D" 42)
  (format nil "Binary: ~B" 42)
  (format nil "Hex: ~X" 255)
  (format nil "List: ~{~A~^, ~}" '(1 2 3))
  (format nil "Conditional: ~:[no~;yes~]" t))

;;; Main function
(defun main ()
  "Main entry point for testing."
  (let ((processor (make-instance 'data-processor :name "Main")))
    (with-timing ("Processing")
      (let* ((data '(1 2 3 "hello" (4 5)))
             (results (process-items processor data)))
        (log-message processor (format nil "Results: ~A" results))))

    ;; Test other functions
    (format t "Sum: ~D~%" (calculate-sum 5 3))
    (format t "Greeting: ~A~%" (greet "World" :greeting "Hello"))

    ;; Test closures
    (let ((counter (make-counter)))
      (format t "Counter: ~D ~D ~D~%"
              (funcall counter)
              (funcall counter)
              (funcall counter)))

    ;; Test person structure
    (let ((person (make-person "Alice" 30 "alice@example.com")))
      (format t "Person: ~A, Age: ~D~%"
              (person-name person)
              (person-age person)))))

;;; Run tests when loaded
;; (main)
