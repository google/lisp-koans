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


; based on about_dice_project.rb

;; In this project we are going to build a CLOS class representing
;; a simple set of dice.  There are only two operations on the dice,
;; reading the values, and re-rolling.


;;  YOU WRITE THIS PART:
(defclass dice-set ()
  () ;; WRITE DICE-SET CLASS BODY HERE
)

(defmethod get-values ((object dice-set))
  ;; WRITE GET-VALUES METHOD DEFINITION HERE
)

(defmethod roll (how-many (object dice-set))
  ;; WRITE ROLL METHOD DEFINITION HERE
)


(define-test test-create-dice-set
;; tests making an instance of the dice-set
    (let ((dice (make-instance 'dice-set)))
      (assert-true dice)))


(define-test test-rolling-the-dice-returns-a-set-of-integers-between-1-and-6
;; tests rolling the dice
    (let ((dice (make-instance 'dice-set)))
      (roll 5 dice)
      (assert-true (typep (get-values dice) 'list))
      (assert-equal 5 (length (get-values dice)))
      (dolist (x (get-values dice))
        (assert-true (and (>= x 1)
                          (<= x 6)
                          (typep x 'integer))))))


(define-test test-dice-values-do-not-change-unless-explicitly-rolled
;; tests that dice don't change just by looking at them
    (let ((dice (make-instance 'dice-set)))
      (roll 100 dice)
      (let ((first-time (get-values dice))
            (second-time (get-values dice)))
        (assert-equal first-time second-time))))


(define-test test-dice-values-should-change-between-rolls
;; tests that rolling the dice DOES change the values.
    (let ((dice (make-instance 'dice-set))
          (first-time nil)
          (second-time nil))
      (roll 100 dice)
      (setf first-time (get-values dice))
      (roll 100 dice)
      (setf second-time (get-values dice))
      (assert-false (equal first-time second-time))))

(define-test test-you-can-roll-different-numbers-of-dice
;; tests count parameter of how many dice to roll
    (let ((dice (make-instance 'dice-set)))
      (assert-equal 5 (length (roll 5 dice)))
      (assert-equal 100 (length (roll 100 dice)))
      (assert-equal 1 (length (roll 1 dice)))))
