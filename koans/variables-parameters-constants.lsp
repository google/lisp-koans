(defun test-variable-assignment-with-setf ()
  ;; the let pattern allows us to create local variables with
  ;; lexical scope.
  (let (var_name_1 (var_name_2 "Michael"))
  ;; variables may be defined with or without initial values.
  (and
   (equalp var_name_2 "Michael")
   ; new values may be assigned to variables with setf
   (setf var_name_2 "Janet")
   (equalp var_name_2 "Janet")
   ; setf may assign multiple variables in one form.
   (setf var_name_1 "Tito"
         var_name_2 "Jermaine")
   (equalp var_name_1 "Tito")
   (equalp var_name_2 "Jermaine"))))

(defun test-setf-for-lists ()
  ;; setf also works on list elements
  (let (l)
    (setf l '(1 2 3))
    (equalp l '(1 2 3))
    ; First second and third are convenient accessor functions
    ; referring to the elements of a list
    ; For those interested, they are convenient to car, cadr, and caddr
    (setf (first l) 10)
    (setf (second l) 20)
    (setf (third l) 30)
    (equalp l '(10 20 30))))

(defparameter param_name_1 "Janet")
; defparameter requires an initial form.  It is a compiler error to exclude it
;(defparameter param_no_init)  ;; this will fail
(defconstant additive_identity 0)
; defconstant also requires an initial form
; (defconstant constant_no_init)

; reassigning parameters to new values is also ok, but parameters carry the
; connotation of immutability.  If it's going to change frequently, it should
; be a var.
(setf param_name_1 "The other one")

; reassigning a constant is an error.
; this should result in a compile time error
; (setf additive_identity -1)


;; -------------------------------
;; below is necessary to run tests.
;; -------------------------------

(defvar failed-test-names nil)

(defun run-test (testfun)
  (let ((fun-name (function-name testfun)))
    (if (apply testfun '())
        (format t ".")
        (progn
          (setf failed-test-names (cons fun-name failed-test-names))
          (format t "F")))))

(defun function-name (function) (nth-value 2 (function-lambda-expression function)))


(run-test #'test-variable-assignment-with-setf)
(run-test #'test-setf-for-lists)

(format t "~%")

(defun report-failure (test-name)
  (format t "~S failed.~%" test-name))

(if (endp failed-test-names)  ; no failed tests
    (format t "all tests pass.~%")
    (mapcar #'report-failure failed-test-names))