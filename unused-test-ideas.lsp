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


; todo: add the nconc example somewhere and take care of the warning.

'(define-test test-nconc
    "nconc like append attaches one list to the end of the other, but
     it does so in a more efficient, but potentially destructive way.
     Lisp lists are nil terminated.  A symbol refers to the beginning of
     a list, and then progresses to find the end.  'nconc' simply takes
     the nil pointer at the end of the first list, and points it at the
     beginning of the next list."
  (assert-equal '(:a :b :c) (nconc '(:a :b) '(:c))) ;k

  (let ((abc '(:a :b :c))
        (xyz '(:x :y :z))
        (abcxyz nil))
    (setf abcxyz (nconc abc xyz))
    (assert-equal '(:a :b :c :x :y :z) abcxyz)
    (assert-equal '(:a :b :c :x :y :z) abc)
    (assert-equal '(:x :y :z) xyz)))


