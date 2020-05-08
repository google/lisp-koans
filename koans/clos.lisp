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

;;; CLOS is a shorthand for Common Lisp Object System.

(defclass racecar ()
  ;; A class definition lists all the slots of every instance.
  (color speed))

(define-test defclass
  ;; Class instances are constructed via MAKE-INSTANCE.
  (let ((car-1 (make-instance 'racecar))
        (car-2 (make-instance 'racecar)))
    ;; Slot values can be set via SLOT-VALUE.
    (setf (slot-value car-1 'color) :red)
    (setf (slot-value car-1 'speed) 220)
    (setf (slot-value car-2 'color) :blue)
    (setf (slot-value car-2 'speed) 240)
    (assert-equal ____ (slot-value car-1 'color))
    (assert-equal ____ (slot-value car-2 'speed))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass spaceship ()
  ;; It is possible to define reader, writer, and accessor functions for slots.
  ((color :reader color :writer (setf color))
   (speed :accessor color)))

;;; Specifying a reader function named COLOR is equivalent to
;;; (DEFMETHOD COLOR ((OBJECT SPACECSHIP)) ...)
;;; Specifying a writer function named (SETF COLOR) is equivalent to
;;; (DEFMETHOD (SETF COLOR) (NEW-VALUE (OBJECT SPACECSHIP)) ...)
;;; Specifying an accessor function performs both of the above.

(define-test accessors
  (let ((ship (make-instance 'spaceship)))
    (setf (color ship) :orange
          (speed ship) 1000)
    (assert-equal ____ (color ship))
    (assert-equal ____ (speed ship))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass bike ()
  ;; It is also possible to define initial arguments for slots.
  ((color :reader color :initarg :color)
   (speed :reader color :initarg :color)))

(define-test initargs
  (let ((bike (make-instance 'bike :color :blue :speed 30)))
    (assert-equal ____ (color bike))
    (assert-equal ____ (speed bike))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass access-counter ()
  ((value :reader value :initform :value)
   (access-count :reader access-count :initform 0)))

;;; The generated reader, writer, and accessor functions are generic functions.
;;; This allows us to define :BEFORE and :AFTER methods whose code is executed
;;; before or after the primary method, and whose return values are discarded.

(defmethod value :after ((object access-counter))
  (incf (slot-value object 'access-count)))

(defmethod (setf value) :after ((object access-counter))
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

(defclass countdown ()
  ;; The countdown object represents an ongoing countdown. Each time the
  ;; REMAINING-TIME function is called, it should return a number one less than
  ;; the previous time that it returned. If the countdown hits zero, :BANG
  ;; should be returned instead.
  ((remaining-time :reader remaining-time :initarg :value)))

;;; In addition to :BEFORE and :AFTER methods is also possible to write :AROUND
;;; methods, whose code executes around the primary method. In such context, it
;;; is possible to call the primary method via CALL-NEXT-METHOD.

(defmethod remaining-time :around ((object countdown))
  (let ((value (call-next-method)))
    (if (<= 0 value)
        ;; DECF is similar to INCF. It decreases the value stored in the place
        ;; and returns the decreased value.
        (decf value)
        :bang)))

(define-test countdown
  (let ((countdown (make-instance 'countdown :value 4)))
    (assert-equal 3 (remaining-time countdown))
    (assert-equal 2 (remaining-time countdown))
    (assert-equal 1 (remaining-time countdown))
    (assert-equal :bang (remaining-time countdown))
    (assert-equal :bang (remaining-time countdown))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Lisp classes can inherit from one another.

(defclass person ()
  ((name :initarg :name :accessor person-name)))

(defclass lisp-programmer (person)
  ((favorite-lisp-implementation :initarg :favorite-lisp-implementation
                                 :accessor favorite-lisp-implementation)))

(defclass c-programmer (person)
  (favorite-c-compiler :initarg :favorite-c-compiler
                       :accessor favorite-c-compiler))

(define-test inheritance
  (let ((jack (make-instance 'person :name :jack))
        (bob (make-instance 'lisp-programmer
                            :name :bob
                            :favorite-lisp-implementation :sbcl))
        (adam (make-instance 'c-programmer
                             :name :adam
                             :favorite-c-compiler :llvm)))
    (assert-equal ____ (person-name jack))
    (assert-equal ____ (person-name bob))
    (assert-equal ____ (favorite-lisp-implementation bob))
    (assert-equal ____ (person-name adam))
    (assert-equal ____ (favorite-c-compiler adam))
    (true-or-false? ____ (typep bob 'person))
    (true-or-false? ____ (typep bob 'lisp-programmer))
    (true-or-false? ____ (typep bob 'c-programmer))))

;;; This includes multiple inheritance.

(defclass clisp-programmer (lisp-programmer c-programmer) ())

(define-test multiple-inheritance
  (let ((zenon (make-instance 'clisp-programmer
                              :name :zenon
                              :favorite-lisp-implementation :clisp
                              :favorite-c-compiler :gcc)))
    (assert-equal ____ (person-name zenon))
    (assert-equal ____ (favorite-lisp-implementation zenon))
    (assert-equal ____ (favorite-c-compiler zenon))
    (true-or-false? ____ (typep zenon 'person))
    (true-or-false? ____ (typep zenon 'lisp-programmer))
    (true-or-false? ____ (typep zenon 'c-programmer))
    (true-or-false? ____ (typep zenon 'embeddable-common-lisp-programmer))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Multiple inheritance makes it possible to work with mixin classes.

(defclass greeting-mixin ()
  ((greeted-people :accessor greeted-people :initform '())))

(defgeneric greet (greeter greetee))

(defmethod greet ((object greeting-mixin) name)
  ;; PUSHNEW is similar to PUSH, but it does not modify the place if the object
  ;; we want to push is already found on the list in the place.
  (pushnew name (greeted-people object) :test #'equal)
  (format nil "Hello, ~A." name))

(defclass chatbot ()
  ((version :reader version :initarg :version)))

(defclass greeting-chatbot (greeting-mixin chatbot) ())

(define-test greeting-chatbot ()
  (let ((chatbot (make-instance 'greeting-chatbot :version "1.0.0")))
    (true-or-false? ____ (typep chatbot 'greeting-mixin))
    (true-or-false? ____ (typep chatbot 'chatbot))
    (true-or-false? ____ (typep chatbot 'greeting-chatbot))
    (assert-equal ____ (greet chatbot "Tom"))
    (assert-equal ____ (greeted-people chatbot))
    (assert-equal ____ (greet chatbot "Sue"))
    (assert-equal ____ (greet chatbot "Mark"))
    (assert-equal ____ (greet chatbot "Kate"))
    (assert-equal ____ (greet chatbot "Mark"))
    (assert-equal ____ (greeted-people chatbot))
    (assert-equal ____ (version chatbot))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass american (person) ())

(defclass italian (person) ())

(defgeneric stereotypical-food (person)
  ;; We can use :METHOD options to DEFGENERIC to define methods for that
  ;; function.
  (:method ((person italian)) :pasta)
  (:method ((person american)) :burger))

;;; When methods or slot definitions of superclasses overlap with each other,
;;; the order of superclasses is used to resolve the conflict.

(defclass stereotypical-person (american italian) ())

(defclass another-stereotypical-person (italian american) ())

(define-test stereotypes
  (let ((james (make-instance 'american))
        (antonio (make-instance 'italian))
        (roy (make-instance 'stereotypical-person))
        (mary (make-instance 'another-stereotypical-person)))
    (assert-equal ____ (stereotypical-food james))
    (assert-equal ____ (stereotypical-food antonio))
    (assert-equal ____ (stereotypical-food roy))
    (assert-equal ____ (stereotypical-food mary))))
