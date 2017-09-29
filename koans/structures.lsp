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


;; Lisp structures encapsulate data which belongs together.  They are
;; a template of sorts, providing a way to generate multiple instances of
;; uniformly organized information
;;
;; Defining a struct also interns accessor functions to get and set the fields
;; of the structure.


;; Define a new struct with the defstruct form.  The following call creates a
;; new structure type named basketball-player, with slots named:
;; 'name', 'team', and number.
(defstruct basketball-player name team number)

(define-test test-make-struct
    ;; Create a basketball structure instance, and then read out the values.
  (let ((player-1 (make-basketball-player
                   :name "larry" :team :celtics :number 33)))
    (assert-equal "larry" (basketball-player-name player-1))
    (assert-equal :celtics (basketball-player-team player-1))
    (assert-equal 33 (basketball-player-number player-1))
    (assert-equal 'basketball-player (type-of player-1))
    (setf (basketball-player-team player-1) :RETIRED)
    (assert-equal :RETIRED (basketball-player-team player-1))))


;; Struct fields can have default values
;; fields without explicit defaults default to nil.

(defstruct baseball-player name (position :outfield) (team :red-sox))

(define-test test-struct-defaults
    (let ((player-2 (make-baseball-player)))
      (assert-equal :outfield (baseball-player-position player-2))
      (assert-equal :red-sox (baseball-player-team player-2))
      (assert-equal nil (baseball-player-name player-2))))


;; The accessor names can get pretty long.  It's possible to specify
;; a nickname to make code readable with the :conc-name option.

(defstruct (american-football-player (:conc-name nfl-guy-)) name position team)

(define-test test-abbreviated-struct-access
    (let ((player-3 (make-american-football-player
                     :name "Drew Brees" :position :QB :team "Saints")))
      (assert-equal :QB (nfl-guy-position player-3))))


;; Structs can be defined as EXTENSIONS to previous structures.
;; This form of inheritance allows composition of objects.

(defstruct (nba-contract (:include basketball-player)) salary start-year end-year)

(define-test test-structure-extension
    (let ((contract-1 (make-nba-contract
                       :salary 136000000
                       :start-year 2004
                       :end-year 2011
                       :name "Kobe Bryant"
                       :team :LAKERS
                       :number 24)))
      (assert-equal 2004 (nba-contract-start-year contract-1))
      (assert-equal 'nba-contract (type-of contract-1))
      ;; do inherited structures follow the rules of type hierarchy?
      (true-or-false? t (typep contract-1 'BASKETBALL-PLAYER))
      ;; can you access structure fields with the inherited accessors?
      (assert-equal :LAKERS (nba-contract-team contract-1))
      (assert-equal :LAKERS (basketball-player-team contract-1))))


;; Copying of structs is handled with the copy-{name} form.  Note that
;; copying is shallow.

(define-test test-structure-copying
    (let ((manning-1 (make-american-football-player :name "Manning" :team '("Colts" "Broncos")))
          (manning-2 (make-american-football-player :name "Manning" :team '("Colts" "Broncos"))))
      ;; manning-1 and manning-2 are different objects
      (true-or-false? nil (eq manning-1 manning-2))
      ;; but manning-1 and manning-2 contain the same information
      ;; (note the equalp instead of eq
      (true-or-false? t (equalp manning-1 manning-2))
      ;; copied structs are much the same.
      (true-or-false? t (equalp manning-1 (copy-american-football-player manning-1)))
      (true-or-false? nil (eq     manning-1 (copy-american-football-player manning-1)))
      ;; note that the copying is shallow
      (let ((shallow-copy (copy-american-football-player manning-1)))
        (setf (car (nfl-guy-team manning-1)) "Giants")
        (assert-equal "Giants" (car (nfl-guy-team manning-1)))
        (assert-equal "Giants" (car (nfl-guy-team shallow-copy))))))
