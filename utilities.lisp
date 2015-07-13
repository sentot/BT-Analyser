
;;;======================================================================
;;;                                                                     |
;;;  Copyright (c) 2013, Sentot Kromodimoeljo                           |
;;;  All Rights Reserved.                                               |
;;;                                                                     |
;;;  Redistribution and use in source and binary forms, with or without |
;;;  modification, are permitted provided the following conditions are  |
;;;  met:                                                               |
;;;                                                                     |
;;;  1. Redistributions of source code must retain the above copyright  |
;;;     notice, this list of conditions and the following disclaimer.   |
;;;  2. Redistributions in binary form must reproduce the above         |
;;;     copyright notice, this list of conditions and the following     |
;;;     disclaimer in the documentation and/or other materials provided |
;;;     with the distribution.                                          |
;;;                                                                     |
;;;  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND             |
;;;  CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES,        |
;;;  INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF           |
;;;  MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE           |
;;;  DISCLAIMED. IN NO EVENT SHALL SENTOT KROMODIMOELJO BE LIABLE FOR   |
;;;  ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR           |
;;;  CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT  |
;;;  OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR |
;;;  BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF         |
;;;  LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT          |
;;;  (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE  |
;;;  USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH   |
;;;  DAMAGE.                                                            |
;;;                                                                     |
;;;======================================================================


(in-package "bt")

;;; Remove duplicates from list (using an equal test).

(defun remove-duplicates (list)
  (let ((result nil))
    (loop for el in list
          unless (member-equal el result)
          do (setq result (append result (list el))))
    result))

;;; Flatten a "tree" into a list.

(defun flatten-into-list (list)
  (loop for x in list
        append (cond ((listp x) (flatten-into-list x))
                     (t (list x)))))

;;; Produce a list of all combinations of the elements of list.
;;; A combination is alist of dotted pairs where an element is paired
;;; with either 0 or 1.

(defun combinations (list)
  (cond ((null list) (list nil))
        ((null (cdr list))
         (list (list (cons (car list) 0)) (list (cons (car list) 1))))
        (t (let ((rest (combinations (cdr list))))
             (append (mapcar #'(lambda (x) (cons (cons (car list) 0) x))
                             rest)
                     (mapcar #'(lambda (x) (cons (cons (car list) 1) x))
                             rest))))))

(defun all-positive-combination-index (combinations)
  (loop for i from 0 to (- (length combinations) 1)
        when
        (combination-all-positive (nth i combinations))
        return i))

(defun index-of-combination (combination combinations)
  (loop for i from 0 to (- (length combinations) 1)
        when (equal combination (nth i combinations))
        return i))

(defun combination-all-positive (combination)
  (let ((result t))
    (loop for entry in combination
          when (= (cdr entry) 0)
          do (setq result nil))
    result))

(defun combination-all-negative (combination)
  (let ((result t))
    (loop for entry in combination
          when (= (cdr entry) 1)
          do (setq result nil))
    result))

(defun all-combinations (combination)
  (combinations
    (loop for entry in combination
          collect (car entry))))

;;; Given a list of indices to BDD entries,
;;; perform a BDD "or" operation on the entries.

(defun bdd-or-on-list (list)
  ;; list is a list of BDD entry indices.
  (cond ((null list) *bdd-zero*)
        ((= (length list) 1) (bdd-entry (car list)))
        ((= (length list) 2)
         (bdd-orop (bdd-entry (first list))
                   (bdd-entry (second list))))
        (t
         (let ((index (push-bdd-entry
                        (bdd-orop (bdd-entry (first list))
                                  (bdd-entry (second list))))))
           (loop for i in (cddr list)
                 do (set-bdd-entry index (bdd-orop (bdd-entry index)
                                                   (bdd-entry i))))
           (let ((result (bdd-entry index)))
             (pop-bdd-entry)
             result)))))

;;; Given a list of indices to BDD entries,
;;; perform a BDD "and" operation on the entries.

(defun bdd-and-on-list (list)
  ;; list is a list of BDD entry indices.
  (cond ((null list) *bdd-one*)
        ((= (length list) 1) (bdd-entry (car list)))
        ((= (length list) 2)
         (bdd-andop (bdd-entry (first list))
                    (bdd-entry (second list))))
        (t
         (let ((index (push-bdd-entry
                        (bdd-andop (bdd-entry (first list))
                                   (bdd-entry (second list))))))
           (loop for i in (cddr list)
                 do (set-bdd-entry index (bdd-andop (bdd-entry index)
                                                    (bdd-entry i))))
           (let ((result (bdd-entry index)))
             (pop-bdd-entry)
             result)))))

;;; states is a list of list of BDD indices.

(defun entries-all-false (states)
  (let ((result t))
    (loop for x in states
          do
          (loop for y in x
                do
                (unless (eq (bdd-entry y) *bdd-zero*)
                  (setq result nil))))
    result))

;;; form the entire set of states as one predicate from
;;; the projections

(defun collect-states-as-disjunction (bs-entry-array)
  (bdd-or-on-list
    (loop for x from 0 to (- (length bs-entry-array) 1)
          append (flatten-into-list (aref bs-entry-array x)))))



;;; Create a bs-entry array as a copy of the entry parts of a bs array.
;;; The copy have their own BDD indices created.

(defun make-copy-bs-entry-array (bs-array)
  (let ((result (make-array (length bs-array))))
    (loop for i from 0 to (- (length bs-array) 1)
          do (setf (aref result i)
                   (make-copy-bs-entry (aref bs-array i))))
    result))

(defun make-copy-bs-entry (bs)
  (loop for i in (bs-entry bs)
        collect
        (loop for j in i
              collect (push-bdd-entry (bdd-entry j)))))

(defun make-false-bs-entry-array (bs-entry-array)
  (let ((result (make-array (length bs-entry-array))))
    (loop for i from 0 to (- (length bs-entry-array) 1)
          do (setf (aref result i)
                   (loop for j in (aref bs-entry-array i)
                         collect
                         (loop for k in j
                               collect (push-bdd-entry *bdd-zero*)))))
    result))

(defun make-bs-entry-array-from-bs-array (bs-array)
  (let ((result (make-array (length bs-array))))
    (loop for i from 0 to (- (length bs-array) 1)
          do
          (setf (aref result i)
                (loop for j in (bs-entry (aref bs-array i))
                      collect
                      (loop for k in j
                            collect (push-bdd-entry (bdd-entry k))))))
    result))

(defun set-bs-entry-array (array1 array2)
  (loop for i from 0 to (- (length array1) 1)
        do (set-bs-entry (aref array1 i) (aref array2 i))))

(defun set-bs-entry (bs1 bs2)
  (loop for i from 0 to (- (length bs1) 1)
        do
        (loop for j from 0 to (- (length (nth i bs1)) 1)
              do
              (set-bdd-entry (nth j (nth i bs1))
                             (bdd-entry (nth j (nth i bs2)))))))


(defun combination-to-bdd (combination)
  (let ((index (push-bdd-entry *bdd-one*)))
    (loop for entry in combination
          do (set-bdd-entry index
                            (bdd-andop
                              (cond ((= (cdr entry) 1)
                                     (bdd-entry (car entry)))
                                    (t
                                     (bdd-negate (bdd-entry (car entry)))))
                              (bdd-entry index))))
    (let ((result (bdd-entry index)))
      (pop-bdd-entry)
      result)))

(defun formula-combination-to-bdd (combination)
  (let ((index (push-bdd-entry *bdd-one*)))
    (loop for entry in combination
          do (set-bdd-entry
               index
               (cond ((= (cdr entry) 1)
                      (bdd-andop (bdd-integrate-formula (car entry))
                                 (bdd-entry index)))
                     (t
                      (bdd-andop (bdd-negate
                                   (bdd-integrate-formula (car entry)))
                                 (bdd-entry index))))))
    (let ((result (bdd-entry index)))
      (pop-bdd-entry)
      result)))



;;; Convert internal timing into seconds

(defun time-in-seconds (time)
  (* 1.0 (/ time internal-time-units-per-second)))

(defun equal-bs-arrays (bs-array1 bs-array2)
  (let ((result t))
    (loop for i from 0 to (- (length bs-array1) 1)
          do
          (let ((bs1 (aref bs-array1 i))
                (bs2 (aref bs-array2 i)))
            (loop for j from 0 to (- (length (bs-entry bs1)) 1)
                  do
                  (let ((entries1 (nth j (bs-entry bs1))))
                    (loop for k from 0 to (- (length entries1) 1)
                          do
                          (unless
                            (and (eq (bdd-entry (nth k (nth j (bs-entry bs1))))
                                     (bdd-entry (nth k (nth j (bs-entry bs2)))))
                                 (eq (bdd-entry (nth k (nth j (bs-exit bs1))))
                                     (bdd-entry (nth k (nth j (bs-exit bs2))))))
                            (return (setq result nil))))))))
    result))
