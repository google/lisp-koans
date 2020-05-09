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
    (assert-equal :red (slot-value car-1 'color))
    (assert-equal 240 (slot-value car-2 'speed))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Common Lisp predefines the symbol SPEED in the COMMON-LISP package, which
;;; means that we cannot define a function named after it. The function SHADOW
;;; creates a new symbol with the same name in the current package and shadows
;;; the predefined one within the current package.

(shadow 'speed)

(defclass spaceship ()
  ;; It is possible to define reader, writer, and accessor functions for slots.
  ((color :reader color :writer (setf color))
   (speed :accessor speed)))

;;; Specifying a reader function named COLOR is equivalent to
;;; (DEFMETHOD COLOR ((OBJECT SPACECSHIP)) ...)
;;; Specifying a writer function named (SETF COLOR) is equivalent to
;;; (DEFMETHOD (SETF COLOR) (NEW-VALUE (OBJECT SPACECSHIP)) ...)
;;; Specifying an accessor function performs both of the above.

(define-test accessors
  (let ((ship (make-instance 'spaceship)))
    (setf (color ship) :orange
          (speed ship) 1000)
    (assert-equal :orange (color ship))
    (assert-equal 1000 (speed ship))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass bike ()
  ;; It is also possible to define initial arguments for slots.
  ((color :reader color :initarg :color)
   (speed :reader speed :initarg :speed)))

(define-test initargs
  (let ((bike (make-instance 'bike :color :blue :speed 30)))
    (assert-equal :blue (color bike))
    (assert-equal 30 (speed bike))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Lisp classes can inherit from one another.

(defclass person ()
  ((name :initarg :name :accessor person-name)))

(defclass lisp-programmer (person)
  ((favorite-lisp-implementation :initarg :favorite-lisp-implementation
                                 :accessor favorite-lisp-implementation)))

(defclass c-programmer (person)
  ((favorite-c-compiler :initarg :favorite-c-compiler
                        :accessor favorite-c-compiler)))

(define-test inheritance
  (let ((jack (make-instance 'person :name :jack))
        (bob (make-instance 'lisp-programmer
                            :name :bob
                            :favorite-lisp-implementation :sbcl))
        (adam (make-instance 'c-programmer
                             :name :adam
                             :favorite-c-compiler :clang)))
    (assert-equal :jack (person-name jack))
    (assert-equal :bob (person-name bob))
    (assert-equal :sbcl (favorite-lisp-implementation bob))
    (assert-equal :adam (person-name adam))
    (assert-equal :clang (favorite-c-compiler adam))
    (true-or-false? t (typep bob 'person))
    (true-or-false? t (typep bob 'lisp-programmer))
    (true-or-false? nil (typep bob 'c-programmer))))

;;; This includes multiple inheritance.

(defclass clisp-programmer (lisp-programmer c-programmer) ())

(define-test multiple-inheritance
  (let ((zenon (make-instance 'clisp-programmer
                              :name :zenon
                              :favorite-lisp-implementation :clisp
                              :favorite-c-compiler :gcc)))
    (assert-equal :zenon (person-name zenon))
    (assert-equal :clisp (favorite-lisp-implementation zenon))
    (assert-equal :gcc (favorite-c-compiler zenon))
    (true-or-false? t (typep zenon 'person))
    (true-or-false? t (typep zenon 'lisp-programmer))
    (true-or-false? t (typep zenon 'c-programmer))
    (true-or-false? t (typep zenon 'clisp-programmer))))

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
    (true-or-false? t (typep chatbot 'greeting-mixin))
    (true-or-false? t (typep chatbot 'chatbot))
    (true-or-false? t (typep chatbot 'greeting-chatbot))
    (assert-equal "Hello, Tom." (greet chatbot "Tom"))
    (assert-equal '("Tom") (greeted-people chatbot))
    (assert-equal "Hello, Sue." (greet chatbot "Sue"))
    (assert-equal "Hello, Mark." (greet chatbot "Mark"))
    (assert-equal "Hello, Kate." (greet chatbot "Kate"))
    (assert-equal "Hello, Mark." (greet chatbot "Mark"))
    (assert-equal '("Kate" "Mark" "Sue" "Tom") (greeted-people chatbot))
    (assert-equal "1.0.0" (version chatbot))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass american (person) ())

(defclass italian (person) ())

(defgeneric stereotypical-food (person)
  ;; The :METHOD option in DEFGENERIC is an alternative to DEFMETHOD.
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
    (assert-equal :burger (stereotypical-food james))
    (assert-equal :pasta (stereotypical-food antonio))
    (assert-equal :burger (stereotypical-food roy))
    (assert-equal :pasta (stereotypical-food mary))))
