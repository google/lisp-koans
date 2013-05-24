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


;; CLOS stands for Common Lisp Object System.
;; CLOS is common lisps' object oriented framework.

(defclass racecar () (color speed))

(define-test test-defclass
    (let ((car-1 (make-instance 'racecar))
          (car-2 (make-instance 'racecar)))
      (setf (slot-value car-1 'color) :red)
      (setf (slot-value car-1 'speed) 220)
      (setf (slot-value car-2 'color) :blue)
      (setf (slot-value car-2 'speed) 240)
      (assert-equal ____ (slot-value car-1 'color))
      (assert-equal ____ (slot-value car-2 'speed))))

;; CLOS provides functionality for creating getters / setters
;; for defined objects

(defclass spaceship ()
  ((color :reader get-color :writer set-color)
   (speed :reader get-speed :writer set-speed)))

(define-test test-clos-getters-and-setters
    (let ((ship-1 (make-instance 'spaceship)))
      (set-color :orange ship-1)
      (assert-equal ____ (get-color ship-1))
      (set-speed 1000 ship-1)
      (assert-equal ____ (get-speed ship-1))))

;; CLOS also provides functionality to create accessors
;; to object data.

;; stores a value, and a counter which tallies accesses, read or write,
;; to that value
(defclass value-with-access-counter ()
  ((value :reader get-value :writer set-value :initform 0)
   (access-count :reader how-many-value-queries :initform 0)))

(defmethod get-value ((object value-with-access-counter))
           (incf (slot-value object 'access-count))
           (slot-value object 'value))

(defmethod set-value (new-value (object value-with-access-counter))
           (incf (slot-value object 'access-count))
           (setf (slot-value object 'value) new-value))

(define-test test-access-counter
    (let ((x (make-instance 'value-with-access-counter)))
      ; check that no one has ever looked at the x value yet.
      (assert-equal ____ (how-many-value-queries x))
      ; check that the default value is zero.
      (assert-equal ___ (get-value x))
      ; now that we've looked at it, there is a single access.
      (assert-equal ___ (how-many-value-queries x))
      ; check that we can set and read the value
      (set-value 33 x)
      (assert-equal 33 (get-value x))
      (assert-equal ___ (how-many-value-queries x))))


; countdowner has a value which goes down every time you look at it
; and returns "bang" when it hits zero.
(defclass countdowner ()
  ((value :initform 4)))

;; Write the get-value for the countdowner
;; to satisfy the test-countdowner tests.
;; you may be interested in the 'decf function.
(defmethod get-value ((object countdowner))
  :WRITE-ME)


(define-test test-countdowner
    (let ((c (make-instance 'countdowner)))
      (assert-equal 3 (get-value c))
      (assert-equal 2 (get-value c))
      (assert-equal 1 (get-value c))
      (assert-equal "bang" (get-value c))
      (assert-equal "bang" (get-value c))))


;; Classes can inherit data and methods from other classes.
;; Here, the specific CIRCLE class extends the generic SHAPE class
(defclass shape ()
  ((kind :reader get-kind :writer set-kind :initform :default-shape-kind)
   (pos :reader get-pos :writer set-pos :initform '(0 0))))

(defclass circle (shape)
  ((radius :reader get-radius :writer set-radius :initform 0)))

(define-test test-inheritance
    (let ((circle-1 (make-instance 'circle))
          (shape-1 (make-instance 'shape)))
      (assert-equal ____ (type-of shape-1))
      (assert-equal ____ (type-of circle-1))
      (true-or-false? ____ (typep circle-1 'circle))
      (true-or-false? ____ (typep circle-1 'shape))
      (set-kind :circle circle-1)
      (set-pos '(3 4) circle-1)
      (set-radius 5 circle-1)
      (assert-equal ____ (get-pos circle-1))
      (assert-equal ____ (get-radius circle-1))))

;; Classes may also inherit from more than one base class.
;; This is known as multiple inheritance.

;; Color holds an rgb triplet and a transparency alpha value.
;; The RGB stands for the amount of red, green, and blue.
;; the alpha (transparency) value is 0 for completely opaque.
;; Note that color also has a kind, like shape.

(defclass color ()
  ((rgb :reader get-rgb :writer set-rgb :initform '(0 0 0))
   (alpha :reader get-alpha :writer set-alpha :initform 0)
   (kind :reader get-kind :writer set-kind :initform :default-color-kind)))

;; The COLORED-CIRCLE class extends both CIRCLE and COLOR.
;; Of particular interest is which "kind" slot will COLORED-CIRCLE get,
;; since both CIRCLE and COLOR provide the "kind" slot.

(defclass colored-circle (color circle) ())
(defclass circled-color (circle color) ())

(define-test test-multiple-inheritance
    (let ((my-colored-circle (make-instance 'colored-circle))
          (my-circled-color (make-instance 'circled-color)))
      (assert-equal ____ (get-kind my-colored-circle))
      (assert-equal ____ (get-kind my-circled-color))))


(defvar *last-kind-accessor* nil)

(defmethod get-kind ((object shape))
           (setf *last-kind-accessor* :shape)
           (slot-value object 'kind))

(defmethod get-kind ((object circle))
           (setf *last-kind-accessor* :circle)
           (slot-value object 'kind))

(defmethod get-kind ((object color))
           (setf *last-kind-accessor* :color)
           (slot-value object 'kind))

;; Precedence order is similarly a depth first search for methods.

(define-test test-multiple-inheritance-method-order
    (let ((my-colored-circle (make-instance 'colored-circle))
          (my-circled-color (make-instance 'circled-color))
          (my-shape (make-instance 'shape))
          (my-circle (make-instance 'circle))
          (my-color (make-instance 'color)))
      (get-kind my-shape)
      (assert-equal ____ *last-kind-accessor*)
      (get-kind my-circle)
      (assert-equal ____ *last-kind-accessor*)
      (get-kind my-color)
      (assert-equal ____ *last-kind-accessor*)
      (get-kind my-colored-circle)
      (assert-equal ____ *last-kind-accessor*)
      (get-kind my-circled-color)
      (assert-equal ____ *last-kind-accessor*)))


;; Todo: consider adding :before and :after method control instructions.

