;;; Copyright 2013 Google Inc.
;;;
;;; Licensed under the Apache License, Version 2.0 (the "License");
;;; you may not use this file except in compliance with the License.
;;; You may obtain a copy of the License at
;;;
;;;     http://www.apache.org/licenses/LICENSE-2.0
;;;
;;; Unless required by applicable law or agreed to in writing, software
;;; distributed under the License is distributed on an "AS IS" BASIS,
;;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;;; See the License for the specific language governing permissions and
;;; limitations under the License.

(defclass access-counter ()
  ((value :accessor value :initarg :value)
   (access-count :reader access-count :initform 0)))

;;; The generated reader, writer, and accessor functions are generic functions.
;;; The methods of a generic function are combined using a method combination;
;;; by default, the standard method combination is used.

;;; This allows us to define :BEFORE and :AFTER methods whose code is executed
;;; before or after the primary method, and whose return values are discarded.
;;; The :BEFORE and :AFTER keywords used in this context are called qualifiers.

(defmethod value :after ((object access-counter))
  (incf (slot-value object 'access-count)))

(defmethod (setf value) :after (new-value (object access-counter))
  (incf (slot-value object 'access-count)))

(define-test defmethod-after
  (let ((counter (make-instance 'access-counter :value 42)))
    (assert-equal ____ (access-count counter))
    (assert-equal ____ (value counter))
    (assert-equal ____ (access-count counter))
    (setf (value counter) 24)
    (assert-equal ____ (access-count counter))
    (assert-equal ____ (value counter))
    (assert-equal ____ (access-count counter))
    ;; We read the value three more times and discard the result.
    (value counter)
    (value counter)
    (value counter)
    (assert-equal ____ (access-count counter))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; In addition to :BEFORE and :AFTER methods is also possible to write :AROUND
;;; methods, which execute instead of the primary methods. In such context, it
;;; is possible to call the primary method via CALL-NEXT-METHOD.
;;; In the standard method combination, the :AROUND method, if one exists, is
;;; executed first, and it may choose whether and how to call next methods.

(defgeneric grab-lollipop ()
  (:method () :lollipop))

(defgeneric grab-lollipop-while-mom-is-nearby (was-nice-p)
  (:method :around (was-nice-p) (if was-nice-p (call-next-method) :no-lollipop))
  (:method (was-nice-p) (declare (ignore was-nice-p)) :lollipop))

(define-test lollipop
  (assert-equal ____ (grab-lollipop))
  (assert-equal ____ (grab-lollipop-while-mom-is-nearby t))
  (assert-equal ____ (grab-lollipop-while-mom-is-nearby nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass countdown ()
  ;; The countdown object represents an ongoing countdown. Each time the
  ;; REMAINING-TIME function is called, it should return a number one less than
  ;; the previous time that it returned. If the countdown hits zero, :BANG
  ;; should be returned instead.
  ((remaining-time :reader remaining-time :initarg :time)))

(defmethod remaining-time :around ((object countdown))
  (let ((time (call-next-method)))
    (if (< 0 time)
        ;; DECF is similar to INCF. It decreases the value stored in the place
        ;; and returns the decreased value.
        (decf (slot-value object 'remaining-time))
        :bang)))

(define-test countdown
  (let ((countdown (make-instance 'countdown :time 4)))
    (assert-equal 3 (remaining-time countdown))
    (assert-equal ____ (remaining-time countdown))
    (assert-equal ____ (remaining-time countdown))
    (assert-equal ____ (remaining-time countdown))
    (assert-equal ____ (remaining-time countdown))
    (assert-equal ____ (remaining-time countdown))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; It is possible for multiple :BEFORE, :AFTER, :AROUND, or primary methods to
;;; be executed in a single method call.

(defclass object ()
  ((counter :accessor counter :initform 0)))

(defclass bigger-object (object) ())

(defgeneric frobnicate (x)
  (:method :around ((x bigger-object))
    (incf (counter x) 8)
    (call-next-method))
  (:method :around ((x object))
    (incf (counter x) 70)
    (call-next-method))
  (:method :before ((x bigger-object))
    (incf (counter x) 600))
  (:method :before ((x object))
    (incf (counter x) 5000))
  (:method ((x bigger-object))
    (incf (counter x) 40000)
    (call-next-method))
  (:method ((x object))
    (incf (counter x) 300000))
  (:method :after ((x object))
    (incf (counter x) 2000000))
  (:method :after ((x bigger-object))
    (incf (counter x) 10000000)))

(define-test multiple-methods
  (let ((object (make-instance 'object)))
    (frobnicate object)
    (assert-equal ____ (counter object)))
  (let ((object (make-instance 'bigger-object)))
    (frobnicate object)
    (assert-equal ____ (counter object))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; The method order of the standard combination is as follows:
;;; First, the most specific :AROUND method is executed.
;;; Second, all :BEFORE methods are executed, most specific first.
;;; Third, the most specific primary method is executed.
;;; Fourth, all :AFTER methods are executed, most specific last.

(defgeneric calculate (x)
  (:method :around ((x bigger-object))
    (setf (counter x) 40)
    (call-next-method))
  (:method :around ((x object))
    (incf (counter x) 24)
    (call-next-method))
  (:method :before ((x bigger-object))
    (setf (counter x) (mod (counter x) 6)))
  (:method :before ((x object))
    (setf (counter x) (/ (counter x) 4)))
  (:method ((x bigger-object))
    (setf (counter x) (* (counter x) (counter x)))
    (call-next-method))
  (:method ((x object))
    (decf (counter x) 100))
  (:method :after ((x object))
    (setf (counter x) (/ 1 (counter x))))
  (:method :after ((x bigger-object))
    (incf (counter x) 2)))

(define-test standard-method-combination-order
  (let ((object (make-instance 'object)))
    (calculate object)
    (assert-equal ____ (counter object)))
  (let ((object (make-instance 'bigger-object)))
    (calculate object)
    (assert-equal ____ (counter object))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass programmer () ())

(defclass senior-programmer (programmer) ())

(defclass full-stack-programmer (programmer) ())

(defclass senior-full-stack-programmer (senior-programmer
                                        full-stack-programmer)
  ())

;;; The :BEFORE, :AFTER, and :AROUND methods are only available in the standard
;;; method combination. It is possible to use other method combinations, such as
;;; +.

(defgeneric salary-at-company-a (programmer)
  (:method-combination +)
  (:method + ((programmer programmer)) 120000)
  (:method + ((programmer senior-programmer)) 200000)
  (:method + ((programmer full-stack-programmer)) 48000))

(define-test salary-at-company-a
  (let ((programmer (make-instance 'programmer)))
    (assert-equal ____ (salary-at-company-a programmer)))
  (let ((programmer (make-instance 'senior-programmer)))
    (assert-equal ____ (salary-at-company-a programmer)))
  (let ((programmer (make-instance 'full-stack-programmer)))
    (assert-equal ____ (salary-at-company-a programmer)))
  (let ((programmer (make-instance 'senior-full-stack-programmer)))
    (assert-equal ____ (salary-at-company-a programmer))))

;;; It is also possible to define custom method combinations.

(define-method-combination multiply :operator *)

(defgeneric salary-at-company-b (programmer)
  (:method-combination multiply)
  (:method multiply ((programmer programmer)) 120000)
  (:method multiply ((programmer senior-programmer)) 2)
  (:method multiply ((programmer full-stack-programmer)) 7/5))

(define-test salary-at-company-b
  (let ((programmer (make-instance 'programmer)))
    (assert-equal ____ (salary-at-company-b programmer)))
  (let ((programmer (make-instance 'senior-programmer)))
    (assert-equal ____ (salary-at-company-b programmer)))
  (let ((programmer (make-instance 'full-stack-programmer)))
    (assert-equal ____ (salary-at-company-b programmer)))
  (let ((programmer (make-instance 'senior-full-stack-programmer)))
    (assert-equal ____ (salary-at-company-b programmer))))
