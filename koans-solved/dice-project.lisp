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

;;; In this project, we are going to define a CLOS class representing a simple
;;; set of dice. There are only two operations on the dice: reading the dice
;;; values and re-rolling their values.

(defclass dice-set ()
  ;; Fill in the blank with a proper slot definition.
  ((values :accessor dice-values :initform '())))

;;; This method might be unnecessary, depending on how you define the slots of
;;; DICE-SET.

;; (defmethod dice-values ((object dice-set))
;;   ____)

(defmethod roll (count (object dice-set))
  (check-type count (integer 1))
  (setf (dice-values object)
        (loop repeat count collect (1+ (random 6)))))

(define-test make-dice-set
  (let ((dice (make-instance 'dice-set)))
    (assert-true (typep dice 'dice-set))))

(define-test dice-are-six-sided
  (let ((dice (make-instance 'dice-set)))
    (roll 5 dice)
    (assert-true (typep (dice-values dice) 'list))
    (assert-equal 5 (length (dice-values dice)))
    (dolist (die (dice-values dice))
      (assert-true (typep die '(integer 1 6))))))

(define-test dice-values-do-not-change-without-rolling
  (let ((dice (make-instance 'dice-set)))
    (roll 100 dice)
    (let ((dice-values-1 (dice-values dice))
          (dice-values-2 (dice-values dice)))
      (assert-equal dice-values-1 dice-values-2))))

(define-test roll-returns-new-dice-values
  (let* ((dice (make-instance 'dice-set))
         (dice-values (roll 100 dice)))
    (assert-true (equal dice-values (dice-values dice)))))

(define-test dice-values-should-change-between-rolling
  (let* ((dice (make-instance 'dice-set))
         (first-time (roll 100 dice))
         (second-time (roll 100 dice)))
    (assert-false (equal first-time second-time))
    (assert-true (equal second-time (dice-values dice)))))

(define-test different-dice-sets-have-different-values
  (let* ((dice-1 (make-instance 'dice-set))
         (dice-2 (make-instance 'dice-set)))
    (roll 100 dice-1)
    (roll 100 dice-2)
    (assert-false (equal (dice-values dice-1) (dice-values dice-2)))))

(define-test different-numbers-of-dice
  (let ((dice (make-instance 'dice-set)))
    (assert-equal 5 (length (roll 5 dice)))
    (assert-equal 100 (length (roll 100 dice)))
    (assert-equal 1 (length (roll 1 dice)))))

(define-test junk-as-dice-count
  (let ((dice (make-instance 'dice-set)))
    (labels ((dice-failure (count)
               (handler-case (progn (roll count dice)
                                    (error "Test failure"))
                 (error (condition) condition)))
             (test-dice-failure (value)
               (let* ((condition (dice-failure value))
                      (expected-type (type-error-expected-type condition)))
                 (assert-true (typep condition 'type-error))
                 (assert-equal value (type-error-datum condition))
                 (assert-true (subtypep '(integer 1 6) expected-type)))))
      (test-dice-failure 0)
      (test-dice-failure "0")
      (test-dice-failure :zero)
      (test-dice-failure 18.0)
      (test-dice-failure -7)
      (test-dice-failure '(6 6 6)))))
