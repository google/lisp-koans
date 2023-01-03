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

;;; Lisp structures encapsulate data which belongs together. They are a template
;;; of sorts, providing a way to generate multiple instances of uniformly
;;; organized information
;;; Defining a structure also interns accessor functions to get and set the
;;; slots of that structure.

;;; The following form creates a new structure class named BASKETBALL-PLAYER
;;; with slots named NAME, TEAM, and NUMBER.
;;; This additionally creates functions MAKE-BASKETBALL-PLAYER,
;;; COPY-BASKETBALL-PLAYER, BASKETBALL-PLAYER-P, BASKETBALL-PLAYER-NAME,
;;; BASKETBALL-PLAYER-TEAM, and BASKETBALL-PLAYER-NUMBER.

(defstruct basketball-player
  name team number)

(define-test make-struct
  (let ((player (make-basketball-player :name "Larry" :team :celtics
                                        :number 33)))
    (true-or-false? ____ (basketball-player-p player))
    (assert-equal ____ (basketball-player-name player))
    (assert-equal ____ (basketball-player-team player))
    (assert-equal ____ (basketball-player-number player))
    (setf (basketball-player-team player) :retired)
    (assert-equal ____ (basketball-player-team player))))

;;; Structure fields can have default values.

(defstruct baseball-player
  name (team :red-sox) (position :outfield))

(define-test struct-defaults
  (let ((player (make-baseball-player)))
    ;; We have not specified a default value for NAME, therefore we cannot
    ;; read it here - it would invoke undefined behaviour.
    (assert-equal ____ (baseball-player-team player))
    (assert-equal ____ (baseball-player-position player))))

;;; The accessor names can get pretty long. It's possible to specify a different
;;; prefix with the :CONC-NAME option.

(defstruct (american-football-player (:conc-name nfl-guy-))
  name position team)

(define-test struct-access
  (let ((player (make-american-football-player
                 :name "Drew Brees" :position :qb :team "Saints")))
    (assert-equal ____ (nfl-guy-name player))
    (assert-equal ____ (nfl-guy-team player))
    (assert-equal ____ (nfl-guy-position player))))

;;; Structs can be defined to include other structure definitions.
;;; This form of inheritance allows composition of objects.

(defstruct (nba-contract (:include basketball-player))
  salary start-year end-year)

(define-test structure-inheritance
  (let ((contract (make-nba-contract :salary 136000000
                                     :start-year 2004 :end-year 2011
                                     :name "Kobe Bryant"
                                     :team :lakers :number 24)))
    (assert-equal ____ (nba-contract-start-year contract))
    (assert-equal ____ (type-of contract))
    ;; Inherited structures follow the rules of type hierarchy.
    (true-or-false? ____ (typep contract 'basketball-player))
    ;; One can access structure fields both with the structure's own accessors
    ;; and with the inherited accessors.
    (assert-equal ____ (nba-contract-team contract))
    (assert-equal ____ (basketball-player-team contract))))

;;; Copying a structure named FOO is handled with the COPY-FOO function.
;;; All such copies are shallow.

(define-test structure-equality-and-copying
  (let ((manning-1 (make-american-football-player
                    :name "Manning" :team (list "Colts" "Broncos")))
        (manning-2 (make-american-football-player
                    :name "Manning" :team (list "Colts" "Broncos"))))
    ;; MANNING-1 and MANNING-2 are different objects...
    (true-or-false? ____ (eq manning-1 manning-2))
    ;; ... but they contain the same information.
    (true-or-false? ____ (equalp manning-1 manning-2))
    (let ((manning-3 (copy-american-football-player manning-1)))
      (true-or-false? ____ (eq manning-1 manning-3))
      (true-or-false? ____ (equalp manning-1 manning-3))
      ;; Setting the slot of one instance does not modify the others...
      (setf (nfl-guy-name manning-1) "Rogers")
      (true-or-false? ____ (string= (nfl-guy-name manning-1)
                                    (nfl-guy-name manning-3)))
      (assert-equal ____ (nfl-guy-name manning-1))
      (assert-equal ____ (nfl-guy-name manning-3))
      ;; ... but modifying shared structure may affect other instances.
      (setf (car (nfl-guy-team manning-1)) "Giants")
      (true-or-false? ____ (string= (car (nfl-guy-team manning-1))
                                    (car (nfl-guy-team manning-3))))
      (assert-equal ____ (car (nfl-guy-team manning-1)))
      (assert-equal ____ (car (nfl-guy-team manning-3))))))
