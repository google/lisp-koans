;;   Copyright 2013 Google Inc.
;;
;;   Licensed under the Apache License, Version 2.0 (the "License");
;;   you may not use this file except in compliance with the License.
;;   You may obtain a copy of the License at
;;
;;       http://www.apache.org/licenses/LICENSE-2.0
;;
;;   Unless required by applicable law or agreed to in writing, software
;;   distributed under the License is distributed on an "AS IS" BASIS,
;;   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;;   See the License for the specific language governing permissions and
;;   limitations under the License.

;; In CLOS we have primary methods and auxiliary methods.
;; By default, methods are primary.
;; An auxiliary method is a method with a qualifier
;; `:before', `:after' or `:around'.
;;
;; The order of evaluation is as follows:
;; First call :before methods from most specific to least specific.
;; Second call the most specific primary method.
;; Finaly call :after methods from least specific to most specific.
;;
;; In other words: 
;; The :before methods are run in most-specific-first order while
;; the :after methods are run in least-specific-first order.
;; The most specific primary method is called after the :before methods
;; and before the :after methods.
;;
;; If only primary methods are used and there is no `call-next-method'
;; calls, only the most specific method is invoked; that is,
;; more specific methods shadow more general ones.
;; http://www.lispworks.com/documentation/HyperSpec/Body/07_ffb.htm

(defclass person ()
  ((words :accessor words :initform ())))

(defmacro pushback (elt seq)
  `(setf ,seq (append ,seq (list ,elt))))

(defgeneric talk (obj))
(defmethod talk ((obj person))
  (print "[person] A person is an individual of the species homo sapiens.")
  (pushback 'homo-sapiens (words obj)) nil)

(defmethod talk :before ((obj person))
  (print "[person :before] A person can talk.")
  (pushback 'talk (words obj)) nil)

(defmethod talk :after ((obj person))
  (print "[person :after] A person can code.")
  (pushback 'code (words obj)) nil)

(define-test test-std-method-combination
    (let ((obj (make-instance 'person)))
      (talk obj)
      (assert-equal '(____  ____  ____) (words obj))))

;; The standard method combination follows the order:
;; First the :before methods in most-specific-first order.
;; Then evaluate the most specific primary method.
;; Finally the :after methods in least-specific-first order.
(defclass developer (person)
  ((code :accessor code
         :initarg :code
         :initform "python")))

(defmethod talk ((obj developer))
  (print "[dev] A developer is a person who write code for a living.")
  (pushback 'living (words obj)))

(define-test test-std-method-combination-override
    (let ((obj (make-instance 'developer)))
      (talk obj)
      (assert-equal '(____  ____  ____) (words obj))))


;; By default the only primary method run is the most specific.
;; You can force to run the primary method of the super class (a.k.a
;; the parent class) by calling `call-next-method'.
(defclass old-school-developer (developer) ())
(defmethod talk ((obj old-school-developer))
  (print "[old-school-dev] Old school developers don't use IDE's")
  (pushback 'ide (words obj))
  (call-next-method) nil)

(define-test test-std-method-combination-old-school
    (let ((obj (make-instance 'old-school-developer)))
      (talk obj)
      (assert-equal '(____  ____  ____  ____) (words obj))))

;; A subclass with auxiliar methods doesn't override the
;; :before/:after auxiliar methods of the super class; all
;; these methods are evaluated.
(defclass cl-developer (developer) ())
(defmethod talk :before ((obj cl-developer))
  (print
   (format nil "[cl-dev :before] I do write ~a code sometimes..."
           (code obj)))
  (pushback 'python (words obj)))

(defmethod talk :after ((obj cl-developer))
  (setf (code obj) "CL")
  (print
   (format nil "[cl-dev :after] ...and I do write ~a code most of the time :-)"
           (code obj)))
  (pushback 'CL (words obj)))

(define-test test-std-method-combination-override-2
    (let ((obj (make-instance 'cl-developer)))
      (talk obj)
      (assert-equal '(____  ____  ____  ____  ____) (words obj))))

;; By default, if an auxiliar method has the keyword :around, then
;; this is the only method executed.
(defclass casual-developer (developer)
  ((clothes :reader clothes :initform (list 'trouser 't-shirt))))

(defmethod talk :around ((obj casual-developer))
  (print "[casual-dev :around] Usually, developers like to dress casual.")
  (pushback 'casual (words obj)))

(define-test test-std-method-combination-around
    (let ((obj (make-instance 'casual-developer)))
      (talk obj)
      (assert-equal '(____) (words obj))))

;; You can use `call-next-method' within an :around
;; method to force the execution of less specific methods.
(defclass good-developer (casual-developer)
  ((prop :reader prop :initform 'do-tests)))

(defmethod talk :around ((obj good-developer))
  (print "[good-dev :around] Good develpers write tests for all their functions.")
  (pushback 'tests (words obj))
  (call-next-method))

(define-test test-std-method-combination-around-2
    (let ((obj (make-instance 'good-developer)))
      (talk obj)
      (assert-equal '(____  ____) (words obj))))

;; You can use `call-next-method' as many times as you like.
(defclass bad-developer (casual-developer)
  ((prop :reader prop :initform 'lazy)))

(defmethod talk :around ((obj bad-developer))
  (print "[bad-dev :around] Bad developers are lazy.")
  (pushback 'lazy (words obj))
  (call-next-method) ; Call :around method from `casual-developer'.
  (call-next-method)) ; Again.

(define-test test-std-method-combination-around-3
    (let ((obj (make-instance 'bad-developer)))
      (talk obj)
      (assert-equal '(____  ____  ____) (words obj))))

;; ----
(defclass rich-developer (developer) ())
(defmethod talk :around ((obj rich-developer))
  (print "[rich-dev :around] Rich developers has lot of money." )
  (pushback 'money (words obj))
  ;; Call auxiliar methods from `person' and primary from `developer'.
  (call-next-method)
  (call-next-method)) ; Again.

(define-test test-std-method-combination-around-4
    (let ((obj (make-instance 'rich-developer)))
      (talk obj)
      (assert-equal
       '(____  ____  ____  ____  ____  ____  ____)
       (words obj))))

