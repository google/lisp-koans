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


;;;;;;;;;;;;;;
;; GREED !! ;;
;;;;;;;;;;;;;;


;; Modified from Ruby Koans: about_scoring_project.rb

; *Greed* is a dice game where you roll up to five dice to accumulate
; points.  The following "score" function will be used to calculate the
; score of a single roll of the dice.
;
; A greed roll is scored as follows:
;
; * A set of three ones is 1000 points
;
; * A set of three numbers (other than ones) is worth 100 times the
;   number. (e.g. three fives is 500 points).
;
; * A one (that is not part of a set of three) is worth 100 points.
;
; * A five (that is not part of a set of three) is worth 50 points.
;
; * Everything else is worth 0 points.
;
;
; Examples:
;
; (score '(1 1 1 5 1)) => 1150 points
; (score '(2 3 4 6 2)) => 0 points
; (score '(3 4 5 3 3)) => 350 points
; (score '(1 5 1 2 4)) => 250 points
;
; More scoring examples are given in the tests below:
;
; Your goal is to write the score method.

;(defun score (dice)
;; 
;)

;; ;; 做法一
;; ;; 版本1remove-once
;; (defun remove-once-version1 (e lst)
;;   (remove-once-g e '() lst))

;; (defun remove-once-g (e alist dlist)
;;   (if (null dlist)
;;       alist
;;       (if (= e (car dlist))
;; 	  (append alist (cdr dlist))
;; 	  (remove-once-g e (append alist (list (car dlist))) (cdr dlist)))))

;; ;; 版本2remove-once
;; ;; TODO

;; (defun three-ones-rule (dice)
;;   (let ((count 0)
;; 	(score 0)
;; 	(r-dice dice))
;;     (dolist (i dice)
;;       (when (and (= i 1) (< count 3))
;; 	(setq count (+ 1 count))
;; 	(setq r-dice (remove-once-version1 1 r-dice))))
;;     (if (= 3 count)
;; 	(progn (setq score 1000) (values score r-dice count))
;; 	(values 0 dice count))))

;; ;; 放弃这种繁琐的做法
;; ;(defun three-other-than-ones-rule (dice)
;; ;  (let ((count 0)
;; ;	(index 0))
;; ;    (if 
;; ;


;; 做法二
(defun count-elements (lst)
  (count-elements-d (make-hash-table) lst))
(defun count-elements-d (hash lst)
  (if (null lst)
      hash
      (let ((key-count (gethash (car lst) hash)))
	(if key-count
	    (setf (gethash (car lst) hash) (1+ key-count))
	    (setf (gethash (car lst) hash) 1))
	(count-elements-d hash (cdr lst)))))

(defun print-hash (hash)
  (loop for k being the hash-keys in hash
     using (hash-value v)
     do (print (list k v))))

(defun score (dice)  ; 假设dice最长是5
  (let ((hash-t (count-elements dice))
	(score 0))
    (loop for k being the hash-keys in hash-t
       using (hash-value v)
       when (> v 3) do (
			cond
			 ((= k 1) (= 4 v) (setq score (+ 1100 score)))
			 ((= k 1) (= 5 v) (setq score (+ 1200 score)))
			 ((= k 5) (= 4 v) (setq score (+ 550 score)))
			 ((= k 5) (= 5 v) (setq score (+ 600 score)))
			 (t (setq score (* 100 k))))
       when (and (= k 1) (= v 3)) do (setq score (+ 1000 score))
       when (and (not (equal k 1)) (= v 3)) do (setq score (+ (* 100 k) score))
       when (and (= k 1) (< v 3)) do (setq score (+ (* 100 v)score))
       when (and (= k 5) (< v 3)) do (setq score (+ (* 50 v)score)))
    score))

;; (defun test-loop (l)
;;   (loop for k being the hash-keys in l
;;      using (hash-value v)
;;      when (equal k "h") do (setq k "avd")
;;        finally return "as"))

;; (defun score (dice)
;;   (loop for x in dice
;;      when (= 1 x) count x into count-1
;;      when (= 5 x) count x into count-5
;;        finally (return (list count-1 count-5))))
       



;; ;; 其他人的答案
;; (defun count-dice (dice)
;;     (loop for die in dice
;;           counting (= 1 die) into ones
;;           counting (= 2 die) into twos
;;           counting (= 3 die) into threes
;;           counting (= 4 die) into fours
;;           counting (= 5 die) into fives
;;           counting (= 6 die) into sixes
;;           finally (return (list ones twos threes fours fives sixes))))

;; (defun score-count (num cnt)
;;     (cond ((= num 1) (+ (* (truncate cnt 3) 1000) 
;;                         (* (mod cnt 3) 100)))
;;           ((= num 5) (+ (* (* (truncate cnt 3) 100) num) 
;;                         (* (mod cnt 3) 50)))
;;           (t            (* (* (truncate cnt 3) 100) num))))

;; (defvar counted-dice)
;; (defun score (dice)
;;   (if (null dice) (return-from score 0))
;;   (setq counted-dice (count-dice dice))
;;   (loop for num from 1 to 6
;;         for cnt in counted-dice
;;         sum (score-count num cnt)))


	   
	
	


(define-test test-score-of-an-empty-list-is-zero
    (assert-equal 0 (score nil)))

(define-test test-score-of-a-single-roll-of-5-is-50
    (assert-equal 50 (score '(5))))


(define-test test-score-of-a-single-roll-of-1-is-100
    (assert-equal 100 (score '(1))))

(define-test test-score-of-multiple-1s-and-5s-is-the-sum-of-individual-scores
    (assert-equal 300 (score '(1 5 5 1))))

(define-test test-score-of-single-2s-3s-4s-and-6s-are-zero
    (assert-equal 0 (score '(2 3 4 6))))


(define-test test-score-of-a-triple-1-is-1000
    (assert-equal 1000  (score '(1 1 1))))

(define-test test-score-of-other-triples-is-100x
    (assert-equal 200  (score '(2 2 2)))
    (assert-equal 300  (score '(3 3 3)))
    (assert-equal 400  (score '(4 4 4)))
    (assert-equal 500  (score '(5 5 5)))
    (assert-equal 600  (score '(6 6 6))))

(define-test test-score-of-mixed-is-sum
    (assert-equal 250  (score '(2 5 2 2 3)))
    (assert-equal 550  (score '(5 5 5 5))))
