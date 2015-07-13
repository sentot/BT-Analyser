
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


(defconstant bdd-hash-table-size 262144)
(defconstant bdd-hash-table-mask (1- bdd-hash-table-size))
(defconstant recache-threshold 524288)

(defconstant hash-multiplier 5)
(defconstant second-hash-multiplier 3)

;;; Maximum number of non-primed variables

(defconstant max-number-of-variables 1024)

;;; Initial value for next-id is (+ (* 2 1024) 2)
;;; since we reserve 1024 ids for non-primed variables (2-1025)
;;; 1024 ids for primed variables (1026-1049)
;;; 0 is reserved for *bdd-zero*
;;; 1 is reserved for *bdd-one*

(defconstant initial-next-id 2050)

(defvar *atom-list* nil)


;;; BDD entries that are to "survive" GCs are to be saved in *bdd-entries*.
;;; Actually, they are reconstructed during a GC.

(defvar *bdd-entries* (make-array 131072 :initial-element nil))
(defvar *bdd-entries-size* 0)

(defun push-bdd-entry (bdd)
  (setf (aref *bdd-entries* *bdd-entries-size*) bdd)
  (let ((result *bdd-entries-size*))
    (setq *bdd-entries-size* (fixnum-op + *bdd-entries-size* 1))
    result))

;;; To safely refer to a BDD entry spanning GCs, use bdd-entry.
;;; This usually means the index needs to be saved.

(defun bdd-entry (index)
  (aref *bdd-entries* index))

(defun pop-bdd-entry ()
  (setf (aref *bdd-entries* *bdd-entries-size*) nil)
  (setq *bdd-entries-size* (fixnum-op - *bdd-entries-size* 1)))

(defun undo-bdd-entries-back-to (index)
  (setq *bdd-entries-size* index))

(defun next-bdd-entry-index ()
  *bdd-entries-size*)

;;; To set a BDD entry, use set-bdd-entry.

(defun set-bdd-entry (index bdd)
  (setf (aref *bdd-entries* index) bdd))


(defvar *atom-array* (vector))

;;; The *if-hash-table* plays a vital role in ensuring that the BDD
;;; structures are shared.
;;; Each element of *if-hash-table* is a list of entries.
;;; Each entry is of the "if" form: (id test-id left-bdd right-bdd).

(defvar *if-hash-table* (make-array bdd-hash-table-size
                                    :initial-element nil))

(defvar *if-count* 0)
(defvar *recaching* nil)

;;; The *not-hash-table* also plays a vital role in ensuring that
;;; the BDD structures are shared.
;;; Each element of *not-hash-table* is a list of entries.
;;; Each entry is of the "not" form: (id unsigned-bdd),
;;; where the unsigned-bdd represents an "unsigned" formula.

(defvar *not-hash-table* (make-array bdd-hash-table-size
                                     :initial-element nil))

;;; The next two tables are for memoizing subfunction operations.
;;; The tables are not vital in that deleting entries in the
;;; tables does not effect the correct operation of the BDD.

(defvar *sub0-hash-table* (make-array bdd-hash-table-size
                                      :initial-element nil))

(defvar *sub1-hash-table* (make-array bdd-hash-table-size
                                      :initial-element nil))

;;; The *and-hash-table* is also for memoizing only:

(defvar *and-hash-table* (make-array bdd-hash-table-size
                                     :initial-element nil))

;;; The *ignore-hash-table* and *override-hash-table*
;;; are also for memoizing only:

(defvar *ignore-hash-table* (make-array bdd-hash-table-size
                                           :initial-element nil))

(defvar *override-hash-table* (make-array bdd-hash-table-size
                                          :initial-element nil))


(defvar *rename-hash-table* (make-array bdd-hash-table-size
                                        :initial-element nil))

(defvar *product-hash-table* (make-array bdd-hash-table-size
                                         :initial-element nil))


;;; The *recache-table* plays a vital role in ensuring the
;;; correct operation of the "garbage collector".

(defvar *recache-table* (make-array bdd-hash-table-size
                                    :initial-element nil))


(defvar *next-id* initial-next-id)
(defvar *next-id-hwm* initial-next-id)
(defvar *gc-count* 0)

(defvar *bdd-zero* 0)
(defvar *bdd-one* (list 1 0))

(defun bdd-id (formula)
  (if (atom formula) formula (car formula)))

(defun bdd-negated-p (formula)
  (and (consp formula) (= (length formula) 2)))

(defun bdd-top (formula)
  (cond ((atom formula) formula) ; 0
        ((atom (second formula)) (second formula)) ; if or 1
        (t (second (second formula))))) ; not



;;; Hash consing and memoizing

;;; Haven't tried to "optimize" the hashing function to get a good
;;; distribution, except for *not-hash-table*.


;;; "nots" are not really memoized, but rather, they are hash-consed
;;; (to use J Moore's terminology).

(defun lookup-memo-for-not (formula)
  (let ((id (bdd-id formula)))
    (loop for x in
          (unsafe-aref *not-hash-table*
                       (fixnum-op logand id bdd-hash-table-mask))
          when (eq (second x) formula)
          return x)))

(defun memoize-not (formula result)
  (let* ((id (bdd-id formula))
         (index (fixnum-op logand id bdd-hash-table-mask)))
    (setf (unsafe-aref *not-hash-table* index)
          (cons result (unsafe-aref *not-hash-table* index)))))

;;; "ands" are memoized

(defun lookup-memo-for-and (left-id right-id)
  ;; bdd-andop is commutative, so normalize arguments to low, high
  (let ((low-id (fixnum-op min left-id right-id))
        (high-id (fixnum-op max left-id right-id)))
    (loop for x in 
          (unsafe-aref
           *and-hash-table*
           (fixnum-op logand
                      (fixnum-op +
                                 (fixnum-op * low-id hash-multiplier)
                                 high-id)
                      bdd-hash-table-mask))
	  when (and (fixnum-rel = low-id (caar x))
		    (fixnum-rel = high-id (cdar x)))
	  return (cdr x))))

(defun memoize-and (left-id right-id result)
  ;; bdd-andop is commutative, so normalize arguments to low, high
  (let ((low-id (fixnum-op min left-id right-id))
        (high-id (fixnum-op max left-id right-id)))
    (let ((index (fixnum-op
                  logand
                  (fixnum-op + (fixnum-op * low-id hash-multiplier) high-id)
                  bdd-hash-table-mask)))
      (setf (unsafe-aref *and-hash-table* index)
            (cons (cons (cons low-id high-id) result)
		  (unsafe-aref *and-hash-table* index))))))

(defun lookup-memo-for-sub0 (var formula)
  (let ((id (bdd-id formula)))
    (loop for x in 
          (unsafe-aref
           *sub0-hash-table*
           (fixnum-op logand
                      (fixnum-op + (fixnum-op * var hash-multiplier) id)
                      bdd-hash-table-mask))
	  when (and (fixnum-rel = var (caar x))
		    (fixnum-rel = id (cdar x)))
	  return (cdr x))))

(defun memoize-sub0 (var formula result)
  (let ((id (bdd-id formula)))
    (let ((index (fixnum-op
                  logand
                  (fixnum-op + (fixnum-op * var hash-multiplier) id)
                  bdd-hash-table-mask)))
      (setf (unsafe-aref *sub0-hash-table* index)
            (cons (cons (cons var id) result)
		  (unsafe-aref *sub0-hash-table* index))))))

(defun lookup-memo-for-sub1 (var formula)
  (let ((id (bdd-id formula)))
    (loop for x in 
          (unsafe-aref
           *sub1-hash-table*
           (fixnum-op logand
                      (fixnum-op + (fixnum-op * var hash-multiplier) id)
                      bdd-hash-table-mask))
	  when (and (fixnum-rel = var (caar x))
		    (fixnum-rel = id (cdar x)))
	  return (cdr x))))

(defun memoize-sub1 (var formula result)
  (let ((id (bdd-id formula)))
    (let ((index (fixnum-op
                  logand
                  (fixnum-op + (fixnum-op * var hash-multiplier) id)
                  bdd-hash-table-mask)))
      (setf (unsafe-aref *sub1-hash-table* index)
            (cons (cons (cons var id) result)
		  (unsafe-aref *sub1-hash-table* index))))))

;;; Note that for ignore, vars must not be nil when memoizing
;;; or looking up in the memo.

(defun lookup-memo-for-ignore (formula vars negated)
  (let ((id (bdd-id formula)))
    (loop for x in 
          (unsafe-aref
           *ignore-hash-table*
           (fixnum-op logand
                      (fixnum-op + (fixnum-op * (car vars) hash-multiplier) id)
                      bdd-hash-table-mask))
	  when (and (fixnum-rel = id (caar x))
		    (equal vars (cadar x))
                    (eq negated (caddar x)))
	  return (cdr x))))

(defun memoize-ignore (formula vars negated result)
  (let ((id (bdd-id formula)))
    (let ((index (fixnum-op
                  logand
                  (fixnum-op + (fixnum-op * (car vars) hash-multiplier) id)
                  bdd-hash-table-mask)))
      (setf (unsafe-aref *ignore-hash-table* index)
            (cons (cons (list id vars negated) result)
		  (unsafe-aref *ignore-hash-table* index))))))

;;; Note that for override, overrides must not be nil when memoizing
;;; or looking up in the memo.

(defun lookup-memo-for-override (formula overrides negate)
  (let ((id (bdd-id formula)))
    (loop for x in 
          (unsafe-aref
           *override-hash-table*
           (fixnum-op logand
                      (fixnum-op +
                       (fixnum-op * (caar overrides) hash-multiplier) id)
                      bdd-hash-table-mask))
	  when (and (fixnum-rel = id (caar x))
		    (equal overrides (cadar x))
                    (eq negate (caddar x)))
	  return (cdr x))))

(defun memoize-override (formula overrides negate result)
  (let ((id (bdd-id formula)))
    (let ((index (fixnum-op
                  logand
                  (fixnum-op +
                   (fixnum-op * (caar overrides) hash-multiplier) id)
                  bdd-hash-table-mask)))
      (setf (unsafe-aref *override-hash-table* index)
            (cons (cons (list id overrides negate) result)
		  (unsafe-aref *override-hash-table* index))))))


(defun lookup-memo-for-rename (formula vars)
  (let ((id (bdd-id formula)))
    (loop for x in
          (unsafe-aref
           *rename-hash-table*
           (fixnum-op logand
                      (fixnum-op +
                       (fixnum-op * (car vars) hash-multiplier) id)
                      bdd-hash-table-mask))
          when (and (fixnum-rel = id (caar x))
                    (equal vars (cdar x)))
          return (cdr x))))

(defun memoize-rename (formula vars result)
  (let ((id (bdd-id formula)))
    (let ((index (fixnum-op
                  logand
                  (fixnum-op +
                   (fixnum-op * (car vars) hash-multiplier) id)
                  bdd-hash-table-mask)))
      (setf (unsafe-aref *rename-hash-table* index)
            (cons (cons (cons id vars) result)
		  (unsafe-aref *rename-hash-table* index))))))

(defun lookup-memo-for-product (left-id right-id vars)
  (loop for x in 
        (unsafe-aref
         *product-hash-table*
         (fixnum-op logand
                    (fixnum-op +
                               (fixnum-op * left-id hash-multiplier)
                               right-id)
                    bdd-hash-table-mask))
        when (and (fixnum-rel = left-id (first (car x)))
                  (fixnum-rel = right-id (second (car x)))
                  (equal vars (third (car x))))
        return (cdr x)))

(defun memoize-product (left-id right-id vars result)
  (let ((index (fixnum-op
                 logand
                 (fixnum-op + (fixnum-op * left-id hash-multiplier) right-id)
                 bdd-hash-table-mask)))
      (setf (unsafe-aref *product-hash-table* index)
            (cons (cons (list left-id right-id vars) result)
		  (unsafe-aref *product-hash-table* index)))))

;;; Reconstruction of bdds after "garbage collection" are really
;;; "hash-consing" operations.

(defun lookup-memo-for-recache (formula)
  (let ((id (bdd-id formula)))
    (loop for x in (unsafe-aref *recache-table*
                                (fixnum-op logand id bdd-hash-table-mask))
          when (fixnum-rel = id (car x))
          return (cdr x))))

(defun memoize-recache (id formula)
  (let ((index (fixnum-op logand id bdd-hash-table-mask)))
    (setf (unsafe-aref *recache-table* index)
          (cons (cons id formula)
                (unsafe-aref *recache-table* index)))))

;;; The top level function for "garbage collection":

(defun bdd-clear-and-recache ()
  ;(format t "~%Clearing and starting new cache~%")
  (dotimes (i bdd-hash-table-size)
    (setf (aref *if-hash-table* i) nil)
    (setf (aref *recache-table* i) nil)
    (setf (aref *not-hash-table* i) nil)
    (setf (aref *sub0-hash-table* i) nil)
    (setf (aref *sub1-hash-table* i) nil)
    (setf (aref *and-hash-table* i) nil)
    (setf (aref *rename-hash-table* i) nil)
    (setf (aref *ignore-hash-table* i) nil)
    (setf (aref *override-hash-table* i) nil)
    (setf (aref *product-hash-table* i) nil))
  (clear-isop-memo)
  (setq *if-count* 0)
  (incf *gc-count*)
  (unless (> *next-id-hwm* *next-id*)
    (setq *next-id-hwm* *next-id*))
  (setq *next-id* initial-next-id)
  (let ((*recaching* t))
    ;; recaching here is really rehashing
    (let ((results (loop for i from 0 to (- *bdd-entries-size* 1)
                         do (setf (aref *bdd-entries* i)
                                  (bdd-recache (aref *bdd-entries* i))))))
      (dotimes (i bdd-hash-table-size)
               (setf (aref *recache-table* i) nil))
      results)))

;;; Function to create the negation of a BDD node.
;;; GC cannot occur while inside bdd-negate.

(defun bdd-negate (formula)
  (cond ((eq formula *bdd-zero*) *bdd-one*)
        ((eq formula *bdd-one*) *bdd-zero*)
        ((and (consp formula) (= (length formula) 2))
         (second formula))
        (t (or (lookup-memo-for-not formula)
               (let ((result (list *next-id* formula)))
                 (setq *next-id* (+ *next-id* 1))
                 (memoize-not formula result)
                 result)))))

;;; Function to create an "if" node in a BDD.

(defun bdd-make-if (test left right)
  (cond ((equal left right) left)
        ((and (consp right) (= (length right) 2))
         (let ((negated-left (bdd-negate left))
               (negated-right (bdd-negate right)))
           (let ((index (fixnum-op
                         logand
                         (fixnum-op + 
                                    (fixnum-op * (bdd-id test) hash-multiplier)
                                    (fixnum-op
                                     + (fixnum-op
                                        * (bdd-id negated-left)
                                        second-hash-multiplier)
                                     (bdd-id negated-right)))
                         bdd-hash-table-mask)))
             (bdd-negate
              (or (loop for x in (unsafe-aref *if-hash-table* index)
                        when (and (eq test (if-test x))
                                  (eq negated-left (if-left x))
                                  (eq negated-right (if-right x)))
                        return x)
                  (let ((need-to-recache (fixnum-op logand
                                                    recache-threshold
                                                    *if-count*)))
                    (cond
                     ((= need-to-recache 0)
                      (let ((if-form (list *next-id* test negated-left
                                           negated-right)))
                        (unless *recaching*
                          (setq *if-count* (+ *if-count* 1)))
                        (setq *next-id* (+ *next-id* 1))
                        (setf (unsafe-aref *if-hash-table* index)
                              (cons if-form
                                    (unsafe-aref *if-hash-table* index)))
                        if-form))
                     (t
                      (let ((left-index (push-bdd-entry negated-left))
                            (right-index (push-bdd-entry negated-right)))
                        (bdd-clear-and-recache)
                        (let ((if-form (list *next-id* test
                                             (bdd-entry left-index)
                                             (bdd-entry right-index)))
                              (index
                               (fixnum-op
                                logand
                                (fixnum-op
                                 +
                                 (fixnum-op * (bdd-id test) hash-multiplier)
                                 (fixnum-op
                                  + (fixnum-op
                                     * (bdd-id (bdd-entry left-index))
                                     second-hash-multiplier)
                                  (bdd-id (bdd-entry right-index))))
                                bdd-hash-table-mask)))
                          (pop-bdd-entry)
                          (pop-bdd-entry)
                          (unless *recaching*
                            (setq *if-count* (+ *if-count* 1)))
                          (setq *next-id* (+ *next-id* 1))
                          (setf (unsafe-aref *if-hash-table* index)
                                (cons if-form
                                      (unsafe-aref *if-hash-table* index)))
                          if-form))))))))))
	(t
         (let ((index (fixnum-op
                       logand
                       (fixnum-op + 
                                  (fixnum-op * (bdd-id test) hash-multiplier)
                                  (fixnum-op
                                   + (fixnum-op
                                      * (bdd-id left) second-hash-multiplier)
                                   (bdd-id right)))
                       bdd-hash-table-mask)))
           (or (loop for x in (unsafe-aref *if-hash-table* index)
                     when (and (eq test (if-test x))
                               (eq left (if-left x))
                               (eq right (if-right x)))
                     return x)
               (let ((need-to-recache (fixnum-op logand
                                                 recache-threshold
                                                 *if-count*)))
                 (cond
                  ((= need-to-recache 0)
                   (let ((if-form (list *next-id* test left right)))
                     (unless *recaching*
                       (setq *if-count* (+ *if-count* 1)))
                     (setq *next-id* (+ *next-id* 1))
                     (setf (unsafe-aref *if-hash-table* index)
                           (cons if-form
                                 (unsafe-aref *if-hash-table* index)))
                     if-form))
                  (t
                   (let ((left-index (push-bdd-entry left))
                         (right-index (push-bdd-entry right)))
                     (bdd-clear-and-recache)
                     (let ((result (list *next-id* test
                                         (bdd-entry left-index)
                                         (bdd-entry right-index)))
                           (index
                            (fixnum-op
                             logand
                             (fixnum-op
                              +
                              (fixnum-op * (bdd-id test) hash-multiplier)
                              (fixnum-op
                               + (fixnum-op
                                  * (bdd-id (bdd-entry left-index))
                                  second-hash-multiplier)
                               (bdd-id (bdd-entry right-index))))
                             bdd-hash-table-mask)))
                       (pop-bdd-entry)
                       (pop-bdd-entry)
                       (unless *recaching*
                         (setq *if-count* (+ *if-count* 1)))
                       (setq *next-id* (+ *next-id* 1))
                       (setf (unsafe-aref *if-hash-table* index)
                             (cons result
                                   (unsafe-aref *if-hash-table* index)))
                       result))))))))))

;;; Function to "reconstruct" a BDD node after "garbage collection".

(defun bdd-recache (formula)
  (cond ((or (eq formula *bdd-zero*) (eq formula *bdd-one*)) formula)
        (t (or (lookup-memo-for-recache formula)
               (cond
                ((= (length formula) 2)
                 (let ((result (bdd-negate (bdd-recache (second formula)))))
                   (memoize-recache (bdd-id formula) result)
                   result))
                (t
                 (let ((result
                        (bdd-make-if (if-test formula)
                                     (bdd-recache (if-left formula))
                                     (bdd-recache (if-right formula)))))
                   (memoize-recache (bdd-id formula) result)
                   result)))))))



;;; =========== The "merge-sorting" functions ============


;;; All boolean connectives are converted to use the "and"
;;; operation and negation (instead of "nand" or "nor").

(defun bdd-andop-constant (formula negated1 negated2)
  (cond ((not negated1) *bdd-zero*)
        (negated2 (bdd-negate formula))
        (t formula)))

(defun complementary (formula1 formula2)
  (or (and (consp formula1)
           (= (length formula1) 2)
           (eq (second formula1) formula2))
      (and (consp formula2)
           (= (length formula2) 2)
           (eq (second formula2) formula1))))

(defun bdd-unnegated (formula)
  (if (bdd-negated-p formula) (second formula) formula))

(defun bdd-compute-sense (formula negated)
  (if (bdd-negated-p formula) (not negated) negated))

(defun bdd-andop-aux (formula1 formula2 negated1 negated2)
  (cond
   ((eq formula1 *bdd-zero*)
    (bdd-andop-constant formula2 negated1 negated2))
   ((eq formula2 *bdd-zero*)
    (bdd-andop-constant formula1 negated2 negated1))
   ((eq formula1 formula2)
    (if negated1
	(if negated2 (bdd-negate formula1) *bdd-zero*)
      (if negated2 *bdd-zero* formula1)))
   (t
    (let ((expr1 (if negated1 (bdd-negate formula1) formula1))
	  (expr2 (if negated2 (bdd-negate formula2) formula2)))
      (or
       (lookup-memo-for-and (bdd-id expr1) (bdd-id expr2))
       (let ((expr1-index (push-bdd-entry expr1))
             (expr2-index (push-bdd-entry expr2)))
         (let ((result
                (cond
                 ((= (if-test formula1) (if-test formula2))
                  (let ((left1 (bdd-unnegated (if-left formula1)))
                        (left2 (bdd-unnegated (if-left formula2)))
                        (neg1 (bdd-compute-sense (if-left formula1) negated1))
                        (neg2 (bdd-compute-sense (if-left formula2) negated2)))
                    ;; Note that a right branch is never a negation
                    (let ((right1-index (push-bdd-entry (if-right formula1)))
                          (right2-index (push-bdd-entry (if-right formula2)))
                          (left-index
                           (push-bdd-entry
                            (bdd-andop-aux left1 left2 neg1 neg2))))
                      (let ((right-result
                             (bdd-andop-aux (bdd-entry right1-index)
                                        (bdd-entry right2-index)
                                        negated1 negated2)))
                        (let ((left-result (bdd-entry left-index)))
                          (dotimes (i 3) (pop-bdd-entry))
                          (bdd-make-if
                           (if-test formula1)
                           left-result right-result))))))
                 ((< (if-test formula1) (if-test formula2))
                  (let ((left1 (bdd-unnegated (if-left formula1)))
                        (neg1 (bdd-compute-sense (if-left formula1) negated1)))
                    (let ((right1-index (push-bdd-entry (if-right formula1)))
                          (formula2-index (push-bdd-entry formula2))
                          (left-index
                           (push-bdd-entry
                            (bdd-andop-aux left1 formula2 neg1 negated2))))
                      (let ((right-result
                             (bdd-andop-aux (bdd-entry right1-index)
                                        (bdd-entry formula2-index)
                                        negated1 negated2)))
                        (let ((left-result (bdd-entry left-index)))
                          (dotimes (i 3) (pop-bdd-entry))
                          (bdd-make-if
                           (if-test formula1)
                           left-result right-result))))))
                 (t
                  (let ((left2 (bdd-unnegated (if-left formula2)))
                        (neg2 (bdd-compute-sense (if-left formula2) negated2)))
                    (let ((formula1-index (push-bdd-entry formula1))
                          (right2-index (push-bdd-entry (if-right formula2)))
                          (left-index
                           (push-bdd-entry
                            (bdd-andop-aux formula1 left2 negated1 neg2))))
                      (let ((right-result
                             (bdd-andop-aux (bdd-entry formula1-index)
                                        (bdd-entry right2-index)
                                        negated1 negated2)))
                        (let ((left-result (bdd-entry left-index)))
                          (dotimes (i 3) (pop-bdd-entry))
                          (bdd-make-if
                           (if-test formula2)
                           left-result right-result)))))))))
           (memoize-and (bdd-id (bdd-entry expr1-index))
                        (bdd-id (bdd-entry expr2-index))
                        result)
           (dotimes (i 2) (pop-bdd-entry))
           result)))))))

(defun bdd-andop (formula1 formula2)
  (bdd-andop-aux (bdd-unnegated formula1)
             (bdd-unnegated formula2)
             (bdd-negated-p formula1)
             (bdd-negated-p formula2)))

(defun bdd-orop (formula1 formula2)
  (bdd-negate (bdd-andop (bdd-negate formula1) (bdd-negate formula2))))






;;; Function to ignore vars (make them "don't cares")

(defun bdd-ignore (formula vars)
  (bdd-ignore-aux formula vars nil))

;;; If negated is t then we're doing a universal quantification,
;;; otherwise we're doing an existential quantification.

(defun bdd-ignore-aux (formula vars negated)
  (cond ((or (null vars) (eq formula *bdd-zero*) (eq formula *bdd-one*))
         formula)
        ((bdd-negated-p formula)
         (bdd-negate (bdd-ignore-aux
                       (bdd-unnegated formula) vars (not negated))))
        (t
         ;; We know for sure that formula is an "if" bdd
         (or (lookup-memo-for-ignore formula vars negated)
             (cond ((= (if-test formula) (car vars))
                    ;; (if test left right) => (or leftResult rightResult)
                    ;; or, if negated,
                    ;; (if test left right) => (and leftResult rightResult)
                    ;; save things on stack so they will survive gc
                    (let* ((formula-index (push-bdd-entry formula))
                           (left-index
                            (push-bdd-entry
                             (bdd-ignore-aux
                               (if-left formula) (cdr vars) negated)))
                           (right (bdd-ignore-aux
                                    (if-right (bdd-entry formula-index))
                                    (cdr vars)
                                    negated))
                           (result
                            (if negated
                                (bdd-andop (bdd-entry left-index) right)
                                (bdd-orop (bdd-entry left-index) right))))
                       (memoize-ignore
                         (bdd-entry formula-index) vars negated result)
                       (dotimes (i 2) (pop-bdd-entry))
                       result))
                   ((< (if-test formula) (car vars))
                    ;; (if test left right) => (if test leftResult rightResult)
                    ;; save things on stack so they will survive gc
                    (let* ((formula-index (push-bdd-entry formula))
                           (left-index
                             (push-bdd-entry
                               (bdd-ignore-aux
                                 (if-left formula) vars negated)))
                           (right (bdd-ignore-aux
                                    (if-right (bdd-entry formula-index))
                                    vars
                                    negated))
                           (result
                             (bdd-make-if (if-test formula)
                                          (bdd-entry left-index)
                                          right)))
                       (memoize-ignore
                         (bdd-entry formula-index) vars negated result)
                       (dotimes (i 2) (pop-bdd-entry))
                       result))
                   (t
                    ;; (car vars) doesn't matter in this case
                    (bdd-ignore-aux formula (cdr vars) negated)))))))

;;; Function to override a bdd with a sequence of "simple assignments"

(defun bdd-override (formula overrides)
  (bdd-override-aux formula overrides nil))

(defun bdd-override-aux (formula overrides negated)
  (cond ((or (null overrides)
             (and negated (eq formula *bdd-one*))
             (and (not negated) (eq formula *bdd-zero*)))
         formula)
        ((eq formula *bdd-zero*)
         ;; we know negated is t
         (if (= (cdar overrides) 1)
             (bdd-make-if (caar overrides)
                          (bdd-override-aux formula (cdr overrides) t)
                          *bdd-one*)
             (bdd-make-if (caar overrides)
                          *bdd-one*
                          (bdd-override-aux formula (cdr overrides) t))))
        ((eq formula *bdd-one*)
         ;; we know negated is nil
         (if (= (cdar overrides) 1)
             (bdd-make-if (caar overrides)
                          (bdd-override-aux formula (cdr overrides) nil)
                          *bdd-zero*)
             (bdd-make-if (caar overrides)
                          *bdd-zero*
                          (bdd-override-aux formula (cdr overrides) nil))))
        ((bdd-negated-p formula)
         (bdd-negate
           (bdd-override-aux
             (bdd-unnegated formula) overrides (not negated))))
        (t
         ;; We know for sure that formula is an "if" bdd
         (or (lookup-memo-for-override formula overrides negated)
             (cond ((and (= (if-test formula) (caar overrides)) (not negated))
                    ;; save things on stack so they will survive gc
                    (let* ((formula-index (push-bdd-entry formula))
                           (left-index
                            (push-bdd-entry
                             (bdd-override-aux (if-left formula)
                                               (cdr overrides)
                                               nil)))
                           (right
                             (bdd-override-aux
                                (if-right (bdd-entry formula-index))
                                (cdr overrides)
                                nil))
                           (arg (bdd-orop (bdd-entry left-index) right))
                           (result (if (= (cdar overrides) 1)
                                       ;; (if test left right) =>
                                       ;;  (if test
                                       ;;      (or leftResult rightResult)
                                       ;;      false)
                                       (bdd-make-if
                                         (if-test formula) arg *bdd-zero*)
                                       ;; (if test left right) =>
                                       ;;  (if test
                                       ;;      false
                                       ;;      (or leftResult rightResult))
                                       (bdd-make-if
                                         (if-test formula) *bdd-zero* arg))))
                        (memoize-override
                          (bdd-entry formula-index) overrides nil result)
                        (dotimes (i 2) (pop-bdd-entry))
                        result))
                   ((and (= (if-test formula) (caar overrides)) negated)
                    ;; save things on stack so they will survive gc
                    (let* ((formula-index (push-bdd-entry formula))
                           (left-index
                            (push-bdd-entry
                             (bdd-override-aux (if-left formula)
                                               (cdr overrides)
                                               t)))
                           (right
                             (bdd-override-aux
                                (if-right (bdd-entry formula-index))
                                (cdr overrides)
                                t))
                           (arg (bdd-andop (bdd-entry left-index) right))
                           (result (if (= (cdar overrides) 1)
                                       ;; (if test left right) =>
                                       ;;  (if test
                                       ;;      (and leftResult rightResult)
                                       ;;      true)
                                       (bdd-make-if
                                         (if-test formula) arg *bdd-one*)
                                       ;; (if test left right) =>
                                       ;;  (if test
                                       ;;      true
                                       ;;      (and leftResult rightResult))
                                       (bdd-make-if
                                         (if-test formula) *bdd-one* arg))))
                        (memoize-override
                          (bdd-entry formula-index) overrides t result)
                        (dotimes (i 2) (pop-bdd-entry))
                        result))
                   ((< (if-test formula) (caar overrides))
                    ;; (if test left right) => (if test leftResult rightResult)
                    ;; save things on stack so they will survive gc
                    (let* ((formula-index (push-bdd-entry formula))
                           (left-index
                             (push-bdd-entry
                               (bdd-override-aux
                                 (if-left formula) overrides negated)))
                           (right
                             (bdd-override-aux
                              (if-right (bdd-entry formula-index))
                              overrides
                              negated))
                           (result
                             (bdd-make-if
                               (if-test formula)
                               (bdd-entry left-index)
                               right)))
                        (memoize-override
                          (bdd-entry formula-index) overrides negated result)
                        (dotimes (i 2) (pop-bdd-entry))
                        result))
                   ((not negated)
                    ;; save things on stack so they will survive gc
                    (let ((formula-index (push-bdd-entry formula))
                          (result (if (= (cdar overrides) 1)
                                      ;; formula => (if var result false)
                                      (bdd-make-if
                                        (caar overrides)
                                        (bdd-override-aux
                                          formula (cdr overrides) nil)
                                        *bdd-zero*)
                                      ;; formula => (if var false result)
                                      (bdd-make-if
                                        (caar overrides)
                                        *bdd-zero*
                                        (bdd-override-aux formula
                                                          (cdr overrides)
                                                          nil)))))
                        (memoize-override
                          (bdd-entry formula-index) overrides nil result)
                        (pop-bdd-entry)
                        result))
                   (t
                    ;; save things on stack so they will survive gc
                    (let ((formula-index (push-bdd-entry formula))
                          (result (if (= (cdar overrides) 1)
                                      ;; formula => (if var result true)
                                      (bdd-make-if
                                        (caar overrides)
                                        (bdd-override-aux
                                          formula (cdr overrides) t)
                                        *bdd-one*)
                                      ;; formula => (if var true result)
                                      (bdd-make-if
                                        (caar overrides)
                                        *bdd-one*
                                        (bdd-override-aux formula
                                                          (cdr overrides)
                                                          t)))))
                        (memoize-override
                          (bdd-entry formula-index) overrides t result)
                        (pop-bdd-entry)
                        result)))))))


;;; Function to rename the variables vars in a BDD to "primed" variables.
;;; The variables in vars must be sorted.

(defun bdd-rename (formula vars)
  (cond ((or (eq formula *bdd-zero*) (eq formula *bdd-one*) (null vars))
         formula)
        (t (or (lookup-memo-for-rename formula vars)
               (cond
                ((= (length formula) 2)
                 (bdd-negate (bdd-rename (second formula) vars)))
                ((= (if-test formula) (car vars))
                 (let* ((formula-index (push-bdd-entry formula))
                        (left-index
                          (push-bdd-entry (bdd-rename (if-left formula)
                                                      (cdr vars))))
                        (right (bdd-rename (if-right (bdd-entry formula-index))
                                           (cdr vars)))
                        (result
                          (bdd-make-if (+ (if-test formula)
                                          max-number-of-variables)
                                       (bdd-entry left-index)
                                       right)))
                   (memoize-rename (bdd-entry formula-index) vars result)
                   (dotimes (i 2) (pop-bdd-entry))
                   result))
                (t
                 (let* ((formula-index (push-bdd-entry formula))
                        (left-index
                          (push-bdd-entry (bdd-rename (if-left formula) vars)))
                        (right (bdd-rename (if-right (bdd-entry formula-index))
                                           vars))
                        (result
                          (bdd-make-if (if-test formula)
                                       (bdd-entry left-index)
                                       right)))
                   (memoize-rename (bdd-entry formula-index) vars result)
                   (dotimes (i 2) (pop-bdd-entry))
                   result)))))))

;;; Function to produce relational product.

(defun bdd-rel-product (left right vars)
  (cond ((or (eq left *bdd-zero*) (eq right *bdd-zero*)) *bdd-zero*)
        ((and (eq left *bdd-one*) (eq right *bdd-one*)) *bdd-one*)
        ((null vars) (bdd-andop left right))
        (t (or (lookup-memo-for-product (bdd-id left) (bdd-id right) vars)
               (let* ((left-index (push-bdd-entry left))
                      (right-index (push-bdd-entry right))
                      (top-var (bdd-top-var left right))
                      (ll-index (push-bdd-entry
                                   (bdd-subfunction1 top-var left)))
                      (lr-index (push-bdd-entry
                                   (bdd-subfunction0
                                     top-var (bdd-entry left-index))))
                      (rl-index (push-bdd-entry
                                   (bdd-subfunction1
                                     top-var (bdd-entry right-index))))
                      (rr-index (push-bdd-entry
                                   (bdd-subfunction0
                                     top-var (bdd-entry right-index)))))
                 (cond ((= (car vars) top-var)
                        (let* ((lresult-index
                                (push-bdd-entry
                                  (bdd-rel-product (bdd-entry ll-index)
                                                   (bdd-entry rl-index)
                                                   (cdr vars))))
                               (rresult-index
                                (push-bdd-entry
                                  (bdd-rel-product (bdd-entry lr-index)
                                                   (bdd-entry rr-index)
                                                   (cdr vars))))
                               (result (bdd-orop (bdd-entry lresult-index)
                                                 (bdd-entry rresult-index))))
                         (memoize-product (bdd-id (bdd-entry left-index))
                                          (bdd-id (bdd-entry right-index))
                                          vars
                                          result)
                         (dotimes (i 8) (pop-bdd-entry))
                         result))
                       (t
                        (let* ((lresult-index
                                (push-bdd-entry
                                  (bdd-rel-product (bdd-entry ll-index)
                                                   (bdd-entry rl-index)
                                                   vars)))
                               (rresult-index
                                (push-bdd-entry
                                  (bdd-rel-product (bdd-entry lr-index)
                                                   (bdd-entry rr-index)
                                                   vars)))
                               (result (bdd-make-if
                                         top-var
                                         (bdd-entry lresult-index)
                                         (bdd-entry rresult-index))))
                         (memoize-product (bdd-id (bdd-entry left-index))
                                          (bdd-id (bdd-entry right-index))
                                          vars
                                          result)
                         (dotimes (i 8) (pop-bdd-entry))
                         result))))))))

(defun bdd-top-var (left right)
  (cond ((or (eq left *bdd-zero*) (eq left *bdd-one*))
         (cond ((or (eq right *bdd-zero*) (eq right *bdd-one*)) nil)
               ((bdd-negated-p right) (if-test (second right)))
               (t (if-test right))))
        ((or (eq right *bdd-zero*) (eq right *bdd-one*))
         (cond ((bdd-negated-p left) (if-test (second left)))
               (t (if-test left))))
        (t (min (if (bdd-negated-p left) (if-test (second left)) (if-test left))
                (if (bdd-negated-p right)
                    (if-test (second right))
                    (if-test right))))))


;;; ================ Resetting =================


(defun reset-bdd ()
  (setq *atom-list* nil)
  (setq *atom-array* (vector))
  (setq *next-id* initial-next-id)
  (setq *next-id-hwm* initial-next-id)
  (setq *if-count* 0)
  (setq *bdd-entries-size* 0)
  (dotimes (i bdd-hash-table-size)
    (setf (aref *if-hash-table* i) nil)
    (setf (aref *recache-table* i) nil)
    (setf (aref *not-hash-table* i) nil)
    (setf (aref *sub0-hash-table* i) nil)
    (setf (aref *sub1-hash-table* i) nil)
    (setf (aref *and-hash-table* i) nil)
    (setf (aref *rename-hash-table* i) nil)
    (setf (aref *ignore-hash-table* i) nil)
    (setf (aref *override-hash-table* i) nil)
    (setf (aref *product-hash-table* i) nil)))



;;; ================ Ordering =================


(defvar *var-scores* (vector))

(defvar *initial-order* nil)

(defun compute-input-scores (circuit &optional (unit 1.0))
  (cond ((null circuit) nil)
        ((atom circuit)
         (incf (aref *var-scores* (- circuit 2)) unit))
        (t (let ((u (/ unit (length circuit))))
             (loop for c in circuit
                   do (compute-input-scores c u))))))

(defun remove-from-circuit (element circuit)
  (cond ((null circuit) nil)
        ((atom circuit)
         (cond ((= element circuit) nil)
               (t circuit)))
        (t (loop for c in circuit
                 for result = (remove-from-circuit element c)
                 unless (null result)
                 collect result))))

(defun construct-circuit-representation (formula)
  (cond ((or (and-p formula) (or-p formula))
         (loop for arg in (cdr formula)
               for result = (construct-circuit-representation arg)
               unless (null result)
               collect result))
        ((implies-p formula)
         (let ((left-result (construct-circuit-representation
                             (implies-left formula)))
               (right-result (construct-circuit-representation
                              (implies-right formula))))
           (cond ((null left-result) right-result)
                 ((null right-result) left-result)
                 (t (list left-result right-result)))))
        ((not-p formula)
         (construct-circuit-representation (not-expr formula)))
        ((if-p formula)
         (let ((left-result
                (cond ((and-p (if-left formula))
                       (construct-circuit-representation
                        (list* 'and (if-test formula)
                               (cdr (if-left formula)))))
                      (t (construct-circuit-representation
                          (make-and (if-test formula) (if-left formula))))))
               (right-result
                (cond ((and-p (if-right formula))
                       (construct-circuit-representation
                        (list* 'and (if-test formula)
                               (cdr (if-right formula)))))
                      (t (construct-circuit-representation
                          (make-and (if-test formula) (if-right formula)))))))
           (cond ((null left-result) right-result)
                 ((null right-result) left-result)
                 (t (list left-result right-result)))))
        ((not (or (true-p formula) (false-p formula)))
         (bdd-integrate-atomic formula))))


;;; The initial order here is essentially Minato's DWA.

(defun construct-initial-order (formula)
  (setq *initial-order* nil)
  (let* ((circuit (construct-circuit-representation formula))
         (number-of-atoms (length *atom-list*)))
    (reset-atom-array)
    (setq *var-scores* (make-array number-of-atoms :initial-element 0))
    (loop while (not (null circuit))
          do (progn (compute-input-scores circuit)
                    (let ((champ 0)
                          (champ-score (aref *var-scores* 0)))
                      (loop for i from 1 to (- number-of-atoms 1)
                            when (> (aref *var-scores* i) champ-score)
                            do (setq champ-score
                                     (aref *var-scores* (setq champ i))))
                      (push (aref *atom-array* champ) *initial-order*)
                      (setq circuit (remove-from-circuit (+ champ 2) circuit))
                      (loop for i from 0 to (- number-of-atoms 1)
                            do (setf (aref *var-scores* i) 0)))))
    (let ((result (reverse *initial-order*)))
      (setq *initial-order* nil)
      (setq *atom-list* nil)
      (setq *atom-array* (vector))
      (setq *var-scores* (vector))
      result)))


(defvar *input-order* nil)

(defun construct-input-order (formula)
  (let ((*input-order* nil))
    (construct-input-order-aux formula)
    (reverse *input-order*)))

(defun construct-input-order-aux (formula)
  (cond ((or (and-p formula) (or-p formula))
         (loop for arg in (cdr formula)
               do (construct-input-order-aux arg)))
        ((implies-p formula)
         (construct-input-order-aux (implies-left formula))
         (construct-input-order-aux (implies-right formula)))
        ((not-p formula)
         (construct-input-order-aux (not-expr formula)))
        ((if-p formula)
         (construct-input-order-aux (if-test formula))
         (construct-input-order-aux (if-left formula))
         (construct-input-order-aux (if-right formula)))
        ((not (or (true-p formula) (false-p formula)
                  (member-equal formula *input-order*)))
         (push formula *input-order*))))


;;; reorder-bdd is called on each bdd indexed by an entry in *bdd-entries*

(defun reorder-bdd (ordered-substitutions bdd)
  (cond ((or (eq bdd *bdd-zero*) (eq bdd *bdd-one*))
         bdd)
        ((bdd-negated-p bdd)
         (bdd-negate (reorder-bdd ordered-substitutions (second bdd))))
        (t
         (let* ((oldvar (if-test bdd))
                (truncated-substitutions
                  (truncate-substitution-list oldvar ordered-substitutions))
                (newvar (cdr (car truncated-substitutions)))
                (left (reorder-bdd (cdr truncated-substitutions)
                                   (if-left bdd)))
                (right (reorder-bdd (cdr truncated-substitutions)
                                    (if-right bdd))))
           (bdd-orop
             (bdd-andop (bdd-make-if newvar *bdd-one* *bdd-zero*) left)
             (bdd-andop (bdd-negate (bdd-make-if newvar *bdd-one* *bdd-zero*))
                        right))))))

(defun truncate-substitution-list (s l)
  (loop for rest = l then (cdr rest)
        while rest
        do (when (equal s (caar rest)) (return rest))))

;;; The top level function for reordering:

(defun bdd-clear-and-reorder (new-order)
  ;(format t "~%Clearing and reordering BDDs~%")
  (dotimes (i bdd-hash-table-size)
    (setf (aref *if-hash-table* i) nil)
    (setf (aref *recache-table* i) nil)
    (setf (aref *not-hash-table* i) nil)
    (setf (aref *sub0-hash-table* i) nil)
    (setf (aref *sub1-hash-table* i) nil)
    (setf (aref *and-hash-table* i) nil)
    (setf (aref *rename-hash-table* i) nil)
    (setf (aref *ignore-hash-table* i) nil)
    (setf (aref *override-hash-table* i) nil)
    (setf (aref *product-hash-table* i) nil))
  (clear-isop-memo)
  (setq *if-count* 0)
  ;(incf *gc-count*)
  (unless (> *next-id-hwm* *next-id*)
    (setq *next-id-hwm* *next-id*))
  (setq *next-id* initial-next-id)
  (let ((old-atom-list *atom-list*)
        (old-order (reverse *atom-list*)))
    (setq *atom-list* (reverse new-order))
    (reset-atom-array)
    (let ((ordered-substitutions
            (loop for s in new-order
                  for i = 2 then (+ i 1)
                  collect
                  (cons i (+ (length (member-equal s old-atom-list)) 1)))))
      (loop for i from 0 to (- *bdd-entries-size* 1)
            do (setf (aref *bdd-entries* i)
                     (reorder-bdd ordered-substitutions
                                  (aref *bdd-entries* i)))))))


;;; ============== Integrating formulas into the BDD ===============


(defun set-expression (formula)
  (bdd-integrate-atoms formula)
  (reset-atom-array)
  ;; By now, all atoms have been integrated.
  (bdd-integrate-formula formula))

(defun reset-atom-array ()
  (setq *atom-array*
        (make-array (* 2 (length *atom-list*))
                    :initial-contents
                    (append (reverse *atom-list*)
                            (mapcar #'(lambda (x) (list 'n x))
                                    (reverse *atom-list*))))))

         
;;; We assume that all atoms have been integrated when this is called.

(defun bdd-integrate-formula (formula)
  (cond ((true-p formula) *bdd-one*)
	((false-p formula) *bdd-zero*)
	((if-p formula)
         (let ((test-index
                (push-bdd-entry (bdd-integrate-formula (if-test formula))))
               (left-index
                (push-bdd-entry (bdd-integrate-formula (if-left formula))))
               (right-index
                (push-bdd-entry (bdd-integrate-formula (if-right formula)))))
           (let ((first-index
                  (push-bdd-entry
                   (bdd-negate
                    (bdd-andop (bdd-entry test-index)
                               (bdd-entry left-index)))))
                 (second-result
                  (bdd-negate
                   (bdd-andop (bdd-negate (bdd-entry test-index))
                          (bdd-entry right-index)))))
             (let ((first-result (bdd-entry first-index)))
               (dotimes (i 4) (pop-bdd-entry))
               (bdd-negate (bdd-andop first-result second-result))))))
	((not-p formula)
	 (bdd-negate (bdd-integrate-formula (second formula))))
	((and-p formula)
         (bdd-integrate-conjuncts (cdr formula)))
	((or-p formula)
         (bdd-integrate-disjuncts (cdr formula)))
	((implies-p formula)
         (let ((first-index
                (push-bdd-entry
                 (bdd-integrate-formula (second formula))))
               (second-result (bdd-integrate-formula (third formula))))
           (let ((first-result (bdd-entry first-index)))
             (pop-bdd-entry)
             (bdd-negate
              (bdd-andop first-result (bdd-negate second-result))))))
	(t
         ;; Atoms have been integrated, so bdd-integrate-atomic will
         ;; not have any side effects.
         (bdd-make-if (bdd-integrate-atomic formula)
                      *bdd-one*
                      *bdd-zero*))))

(defun bdd-integrate-conjuncts (conjuncts)
  (cond ((> (length conjuncts) 1)
         (let ((first-index
                (push-bdd-entry
                 (bdd-integrate-formula (car conjuncts))))
               (rest-result (bdd-integrate-conjuncts (cdr conjuncts))))
           (let ((first-result (bdd-entry first-index)))
             (pop-bdd-entry)
             (bdd-andop first-result rest-result))))
        (t (bdd-integrate-formula (car conjuncts)))))

(defun bdd-integrate-disjuncts (disjuncts)
  (cond ((> (length disjuncts) 1)
         (let ((first-index
                (push-bdd-entry
                 (bdd-integrate-formula (car disjuncts))))
               (rest-result (bdd-integrate-disjuncts (cdr disjuncts))))
           (let ((first-result (bdd-entry first-index)))
             (pop-bdd-entry)
             (bdd-orop first-result rest-result))))
        (t (bdd-integrate-formula (car disjuncts)))))


(defun bdd-integrate-atomic (formula)
  (let ((tail (member-equal formula *atom-list*)))
    (cond (tail (+ (length tail) 1))
          ((and (listp formula)
                (eq (car formula) 'n))
           (let ((tail2 (member-equal (second formula) *atom-list*)))
             (cond (tail2 (+ (length tail2) max-number-of-variables 1))
                   (t (push (second formula) *atom-list*)
                      ;; **** do we modify *atom-array*?
                      (+ (length *atom-list*) max-number-of-variables 1)))))
          (t (push formula *atom-list*)
             ;; **** do we modify *atom-array*?
             (+ (length *atom-list*) 1)))))

(defun bool-connective-p (formula)
  (or (and-p formula)
      (or-p formula)
      (implies-p formula)))

(defun bdd-integrate-atoms (formula)
  (cond ((or (true-p formula) (false-p formula)) nil)
	((if-p formula)
	 (bdd-integrate-atoms (if-test formula))
	 (bdd-integrate-atoms (if-left formula))
	 (bdd-integrate-atoms (if-right formula)))
	((not-p formula)
	 (bdd-integrate-atoms (second formula)))
	((bool-connective-p formula)
	 (bdd-integrate-atoms (second formula))
	 (bdd-integrate-atoms (third formula)))
	(t (bdd-integrate-atomic formula))))


;;; Some more BDD operations


(defun bdd-subfunction0 (var formula)
  (cond
   ((or (eq formula *bdd-zero*) (eq formula *bdd-one*))
    formula)
   ((bdd-negated-p formula)
    (bdd-negate (bdd-subfunction0 var (second formula))))
   (t (or (lookup-memo-for-sub0 var formula)
          (let* ((formula-index (push-bdd-entry formula))
		 (result
		  (cond
		   ((= (if-test formula) var) (if-right formula))
		   ((< (if-test formula) var)
		    (bdd-make-if (if-test formula)
				 (bdd-subfunction0 var (if-left formula))
				 (bdd-subfunction0
				  var (if-right (bdd-entry formula-index)))))
		   (t formula))))
            (memoize-sub0 var (bdd-entry formula-index) result)
	    (pop-bdd-entry)
            result)))))

(defun bdd-subfunction1 (var formula)
  (cond
   ((or (eq formula *bdd-zero*) (eq formula *bdd-one*))
    formula)
   ((bdd-negated-p formula)
    (bdd-negate (bdd-subfunction1 var (second formula))))
   (t (or (lookup-memo-for-sub1 var formula)
          (let* ((formula-index (push-bdd-entry formula))
		 (result
		  (cond
		   ((= (if-test formula) var) (if-left formula))
		   ((< (if-test formula) var)
		    (bdd-make-if (if-test formula)
				 (bdd-subfunction1 var (if-left formula))
				 (bdd-subfunction1
				  var (if-right (bdd-entry formula-index)))))
		   (t formula))))
            (memoize-sub1 var (bdd-entry formula-index) result)
	    (pop-bdd-entry)
            result)))))

(defun bdd-var (formula)
  (cond ((or (eq formula *bdd-zero*) (eq formula *bdd-one*)) 1000000)
        ((bdd-negated-p formula) (if-test (second formula)))
        (t (if-test formula))))

(defun bdd-compute-subfunctions (var set-indices)
  (let ((result-indices nil))
    (dolist (s-index set-indices)
      (let ((sub0 (bdd-unnegated (bdd-subfunction0 var (bdd-entry s-index)))))
	(unless (loop for r-index in result-indices
		      when (eq sub0 (bdd-entry r-index))
		      return t)
	  (push (push-bdd-entry sub0) result-indices)))
      (let ((sub1 (bdd-unnegated (bdd-subfunction1 var (bdd-entry s-index)))))
	(unless (loop for r-index in result-indices
		      when (eq sub1 (bdd-entry r-index))
		      return t)
	  (push (push-bdd-entry sub1) result-indices))))
    (let ((result (loop for r-index in result-indices
			collect (bdd-entry r-index))))
      (dotimes (i (length result-indices)) (pop-bdd-entry))
      result)))

(defun bdd-choose-order-aux (set-indices result)
  (let ((minvar (bdd-var (bdd-entry (car set-indices))))
        (number-of-atoms (length *atom-list*)))
    (dolist (s-index set-indices)
      (when (< (bdd-var (bdd-entry s-index)) minvar)
	(setq minvar (bdd-var (bdd-entry s-index)))))
    (cond ((> minvar number-of-atoms)
	   (dotimes (i (length set-indices)) (pop-bdd-entry))
	   result)
          (t (let ((champ-indices
		    (loop for c in (bdd-compute-subfunctions
				    minvar set-indices)
			  collect (push-bdd-entry c)))
                   (champvar minvar))
               (loop for var from 2 to (+ number-of-atoms 1)
                     do (unless (or (eq var champvar)
				    (member-eq var result))
                          (let ((chal (bdd-compute-subfunctions
				       var set-indices)))
                            (when (< (length chal) (length champ-indices))
			      (dotimes (i (length champ-indices))
				(pop-bdd-entry))
                              (setq champ-indices
				    (loop for c in chal
					  collect (push-bdd-entry c)))
                              (setq champvar var)))))
	       (let ((champ (loop for c-index in champ-indices
				  collect (bdd-entry c-index))))
		 (dotimes (i (length champ)) (pop-bdd-entry))
		 (dotimes (i (length set-indices)) (pop-bdd-entry))
		 (setq champ-indices
		       (loop for c in champ
			     collect (push-bdd-entry c)))
		 (bdd-choose-order-aux
		  champ-indices (cons champvar result))))))))

(defun bdd-choose-order (formula)
  (unless (or (eq formula *bdd-zero*) (eq formula *bdd-one*))
    (let* ((var (bdd-var formula))
	   (formula-index (push-bdd-entry formula))
	   (subfunctions (bdd-compute-subfunctions var (list formula-index))))
      (pop-bdd-entry)
      (let ((subfunction-indices
	     (loop for s in subfunctions
		   collect (push-bdd-entry s))))
	(bdd-choose-order-aux subfunction-indices (list var))))))




;;; ============ ZBDD and revised ISOP using ZBDD =============



(defconstant zbdd-hash-table-size 65536)
(defconstant zbdd-hash-table-mask (1- zbdd-hash-table-size))

(defvar *zbdd-next-id* 0)

(defvar *zbdd-hash-table* (make-array zbdd-hash-table-size
                                    :initial-element nil))

(defun zbdd-make-if (test left right)
  (cond ((equal left 0) right)
	(t
	 (let ((test-id (bdd-id test))
	       (left-id (bdd-id left))
	       (right-id (bdd-id right)))
	   (let ((index (fixnum-op logand
			 (fixnum-op +
			   (fixnum-op * test-id hash-multiplier)
			   (fixnum-op +
			     (fixnum-op * left-id second-hash-multiplier)
			     right-id))
			 zbdd-hash-table-mask)))
	     (or (loop for x in (unsafe-aref *zbdd-hash-table* index)
		       when (let ((u (car x)))
			      (and (fixnum-rel = (first u) test-id)
				   (fixnum-rel = (second u) left-id)
				   (fixnum-rel = (third u) right-id)))
		       return (cdr x))
		 (let ((result (list *zbdd-next-id* test left right)))
		   (setq *zbdd-next-id* (fixnum-op + *zbdd-next-id* 1))
		   (setf (unsafe-aref *zbdd-hash-table* index)
			 (cons (cons (list test-id left-id right-id) result)
			       (unsafe-aref *zbdd-hash-table* index)))
		   result)))))))

;;; Returns subset of formula such that var = 1

(defun zbdd-subset1 (formula var)
  (cond ((< var (bdd-top formula)) 0)
        ((= var (bdd-top formula)) (if-left formula))
        (t (zbdd-make-if (if-test formula)
                         (zbdd-subset1 (if-left formula) var)
                         (zbdd-subset1 (if-right formula) var)))))

;;; Returns subset of formula such that var = 0

(defun zbdd-subset0 (formula var)
  (cond ((< var (bdd-top formula)) formula)
        ((= var (bdd-top formula)) (if-right formula))
        (t (zbdd-make-if (if-test formula)
                         (zbdd-subset0 (if-left formula) var)
                         (zbdd-subset0 (if-right formula) var)))))

;;; Returns formula with var inverted

(defun zbdd-change (formula var)
  (cond ((or (atom formula) (< var (bdd-top formula)))
	 (zbdd-make-if var formula 0))
        ((= var (bdd-top formula))
         (zbdd-make-if var (if-right formula) (if-left formula)))
        (t (zbdd-make-if (if-test formula)
                         (zbdd-change (if-left formula) var)
                         (zbdd-change (if-right formula) var)))))

;;; Union

(defun zbdd-union (formula1 formula2)
  (cond ((equal formula1 0) formula2)
        ((equal formula2 0) formula1)
        ((equal formula1 formula2) formula1)
        ((equal formula1 1)
         (zbdd-make-if (if-test formula2)
                       (if-left formula2)
                       (zbdd-union formula1 (if-right formula2))))
        ((equal formula2 1)
         (zbdd-make-if (if-test formula1)
                       (if-left formula1)
                       (zbdd-union (if-right formula1) formula2)))
        ((< (bdd-top formula1) (bdd-top formula2))
         (zbdd-make-if (if-test formula1)
                       (if-left formula1)
                       (zbdd-union (if-right formula1) formula2)))
        ((= (bdd-top formula1) (bdd-top formula2))
         (zbdd-make-if (if-test formula1)
                       (zbdd-union (if-left formula1) (if-left formula2))
                       (zbdd-union (if-right formula1) (if-right formula2))))
        (t
         (zbdd-make-if (if-test formula2)
                       (if-left formula2)
                       (zbdd-union formula1 (if-right formula2))))))


;;; Intersection

(defun zbdd-inter (formula1 formula2)
  (cond ((equal formula1 0) 0)
        ((equal formula2 0) 0)
        ((equal formula1 formula2) formula1)
        ((< (bdd-top formula1) (bdd-top formula2))
         (zbdd-inter (if-right formula1) formula2))
        ((= (bdd-top formula1) (bdd-top formula2))
         (zbdd-make-if (if-test formula1)
                       (zbdd-inter (if-left formula1) (if-left formula2))
                       (zbdd-inter (if-right formula1) (if-right formula2))))
        (t
         (zbdd-inter formula1 (if-right formula2)))))

;;; Difference

(defun zbdd-diff (formula1 formula2)
  (cond ((equal formula1 0) 0)
        ((equal formula2 0) formula1)
        ((equal formula1 formula2) 0)
        ((< (bdd-top formula1) (bdd-top formula2))
         (zbdd-make-if (if-test formula1)
                       (if-left formula1)
                       (zbdd-diff (if-right formula1) formula2)))
        ((= (bdd-top formula1) (bdd-top formula2))
         (zbdd-make-if (if-test formula1)
                       (zbdd-diff (if-left formula1) (if-left formula2))
                       (zbdd-diff (if-right formula1) (if-right formula2))))
        (t (zbdd-diff formula1 (if-right formula2)))))

(defun reset-zbdd ()
  (setq *zbdd-next-id* 0)
  (dotimes (i zbdd-hash-table-size)
    (setf (aref *zbdd-hash-table* i) nil)))

(defun zbdd-atom (atom)
  (- (* 2 atom) 2))

(defun zbdd-complement-atom (atom)
  (- (* 2 atom) 1))

(defun zbdd-complemented-p (atom)
  (= (mod atom 2) 1))

(defun zbdd-print-atom (atom)
  (let ((atomic-formula (aref *atom-array* (floor (- atom 2) 2))))
    (if (zbdd-complemented-p atom)
	(make-not atomic-formula)
      atomic-formula)))

(defvar *isop-hash-table* (make-array zbdd-hash-table-size
                                      :initial-element nil))

(defun reset-isop ()
  (setq *zbdd-next-id* 0)
  (dotimes (i zbdd-hash-table-size)
    (setf (aref *zbdd-hash-table* i) nil)
    (setf (aref *isop-hash-table* i) nil)))

(defun clear-isop-memo ()
  (dotimes (i zbdd-hash-table-size)
    (setf (aref *isop-hash-table* i) nil)))


(defun setup-isop ()
  (setq *zbdd-next-id* (+ (* 2 (length *atom-list*)) 2)))

(defun memoize-isop (floor-id ceiling-id result)
  (let ((index (fixnum-op logand
                          (fixnum-op +
                                     (fixnum-op * hash-multiplier floor-id)
                                     ceiling-id)
                          zbdd-hash-table-mask)))
    (setf (unsafe-aref *isop-hash-table* index)
          (cons (cons (cons floor-id ceiling-id) result)
                (unsafe-aref *isop-hash-table* index)))))

(defun lookup-memo-for-isop (floor ceiling)
  (let ((floor-id (bdd-id floor))
        (ceiling-id (bdd-id ceiling)))
    (loop for x in 
          (unsafe-aref *isop-hash-table*
		(fixnum-op logand
			   (fixnum-op + (fixnum-op * hash-multiplier floor-id)
				      ceiling-id)
			   zbdd-hash-table-mask))
	  when (and (fixnum-rel = floor-id (caar x))
		    (fixnum-rel = ceiling-id (cdar x)))
	  return (cdr x))))

(defun isop (flr ceil)
  (cond
   ((eq flr *bdd-zero*) (list *bdd-zero* 0)) ; path ignored
   ((eq ceil *bdd-one*) (list *bdd-one* 1)) ; path collected
   (t (or (lookup-memo-for-isop flr ceil)
          (let ((flr-expr (if (bdd-negated-p flr) (second flr) flr))
                (neg-flr (if (bdd-negated-p flr) t nil))
                (ceil-expr (if (bdd-negated-p ceil) (second ceil) ceil))
                (neg-ceil (if (bdd-negated-p ceil) t nil))
                (flr-index (push-bdd-entry flr))
                (ceil-index (push-bdd-entry ceil)))
            (let ((result
                   (cond
                    ((= (if-test flr-expr) (if-test ceil-expr))
                     (isop-aux (if-test flr-expr)
                               (if neg-flr
                                   (bdd-negate (if-left flr-expr))
                                   (if-left flr-expr))
                               (if neg-ceil
                                   (bdd-negate (if-left ceil-expr))
                                   (if-left ceil-expr))
                               (if neg-flr
                                   (bdd-negate (if-right flr-expr))
                                   (if-right flr-expr))
                               (if neg-ceil
                                   (bdd-negate (if-right ceil-expr))
                                   (if-right ceil-expr))))
                    ((< (if-test flr-expr) (if-test ceil-expr))
                     (isop-aux (if-test flr-expr)
                               (if neg-flr
                                   (bdd-negate (if-left flr-expr))
                                   (if-left flr-expr))
                               ceil
                               (if neg-flr
                                   (bdd-negate (if-right flr-expr))
                                   (if-right flr-expr))
                               ceil))
                    (t (isop-aux (if-test ceil-expr)
                                 flr
                                 (if neg-ceil
                                     (bdd-negate (if-left ceil-expr))
                                     (if-left ceil-expr))
                                 flr
                                 (if neg-ceil
                                     (bdd-negate (if-right ceil-expr))
                                     (if-right ceil-expr)))))))
              (memoize-isop (bdd-id (bdd-entry flr-index))
                            (bdd-id (bdd-entry ceil-index))
                            result)
              (pop-bdd-entry)
              (pop-bdd-entry)
              result))))))

(defun isop-aux (test left-floor left-ceiling right-floor right-ceiling)
  (let ((left-floor-index (push-bdd-entry left-floor))
        ;; the ceiling for f1 becomes the ceiling for f1'
        (left-ceiling-index (push-bdd-entry left-ceiling))
        (right-floor-index (push-bdd-entry right-floor))
        ;; the ceiling for f0 becomes the ceiling for f0'
        (right-ceiling-index (push-bdd-entry right-ceiling))
        (first-index
         ;; this is the floor of f1' in Minato's formulation
         ;; (it is the floor of f1 lowered because of
         ;; f0's coverage).
         (push-bdd-entry (bdd-andop left-floor (bdd-negate right-ceiling)))))
    (let ((second-index
           (push-bdd-entry
            ;; this is the floor of f0' in Minato's formulation
            (bdd-andop (bdd-entry right-floor-index)
                       (bdd-negate (bdd-entry left-ceiling-index))))))
      ;; left is assigned isop1 while right is assigned isop0
      ;; the BDDs for left and right are pointed by left-index and
      ;; right-index respectively.
      (let* ((left (isop (bdd-entry first-index)
                         (bdd-entry left-ceiling-index)))
             (left-index (push-bdd-entry (first left)))
             (right (isop (bdd-entry second-index)
                          (bdd-entry right-ceiling-index)))
             (right-index (push-bdd-entry (first right))))
        (let ((leftdpflr-index
               ;; the floor for f1'' is lowered because of coverage
               ;; by isop1
               (push-bdd-entry
                (bdd-andop (bdd-entry left-floor-index)
                           (bdd-negate (bdd-entry left-index)))))
              (rightdpflr
               ;; similarly with the floor for f0''
               (bdd-andop (bdd-entry right-floor-index)
                          (bdd-negate (bdd-entry right-index)))))
          (let ((floor-index
                 ;; floor for fd
                 (push-bdd-entry
                  (bdd-orop (bdd-andop rightdpflr
                                       (bdd-entry left-ceiling-index))
                            (bdd-andop (bdd-entry leftdpflr-index)
                                       (bdd-entry right-ceiling-index)))))
                (ceiling-index
                 ;; ceiling for fd
                 (push-bdd-entry
                  (bdd-andop (bdd-entry left-ceiling-index)
                             (bdd-entry right-ceiling-index)))))
            ;; compute isopd using floor and ceiling of fd
            (let* ((dontcare (isop (bdd-entry floor-index)
                                   (bdd-entry ceiling-index)))
                   (dontcare-index (push-bdd-entry (first dontcare)))
                   ;; (and v isop1)
                   (res1-index
                    (push-bdd-entry
                     (bdd-andop (bdd-make-if test *bdd-one* *bdd-zero*)
                                (bdd-entry left-index))))
                   ;; (and (not v) isop0)
                   (res2
                    (bdd-andop (bdd-make-if test *bdd-zero* *bdd-one*)
                               (bdd-entry right-index)))
                   ;; isop = (or (and v isop1) (and (not v) isop0) isopd)
                   (result-bdd
                     (bdd-orop (bdd-orop (bdd-entry res1-index) res2)
                               (bdd-entry dontcare-index))))
              (dotimes (i 13) (pop-bdd-entry))
              (list result-bdd
                    ;; the corresponding ZBDD for isop
                    (zbdd-union
                     (zbdd-union
                      (zbdd-change (second left) (zbdd-atom test))
                      (zbdd-change (second right)
                                   (zbdd-complement-atom test)))
                     (second dontcare))))))))))

(defun ipos (flr ceil)
  (cond
   ((equal ceil *bdd-one*) (list *bdd-one* 0)) ; path ignored
   ((equal flr *bdd-zero*) (list *bdd-zero* 1)) ; path collected
   (t (or (lookup-memo-for-isop flr ceil)
          (let ((flr-expr (if (bdd-negated-p flr) (second flr) flr))
                (neg-flr (if (bdd-negated-p flr) t nil))
                (ceil-expr (if (bdd-negated-p ceil) (second ceil) ceil))
                (neg-ceil (if (bdd-negated-p ceil) t nil))
                (flr-index (push-bdd-entry flr))
                (ceil-index (push-bdd-entry ceil)))
            (let ((result
                   (cond
                    ((= (if-test flr-expr) (if-test ceil-expr))
                     (ipos-aux (if-test flr-expr)
                               (if neg-flr
                                   (bdd-negate (if-left flr-expr))
                                   (if-left flr-expr))
                               (if neg-ceil
                                   (bdd-negate (if-left ceil-expr))
                                   (if-left ceil-expr))
                               (if neg-flr
                                   (bdd-negate (if-right flr-expr))
                                   (if-right flr-expr))
                               (if neg-ceil
                                   (bdd-negate (if-right ceil-expr))
                                   (if-right ceil-expr))))
                    ((< (if-test flr-expr) (if-test ceil-expr))
                     (ipos-aux (if-test flr-expr)
                               (if neg-flr
                                   (bdd-negate (if-left flr-expr))
                                   (if-left flr-expr))
                               ceil
                               (if neg-flr
                                   (bdd-negate (if-right flr-expr))
                                   (if-right flr-expr))
                               ceil))
                    (t (ipos-aux (if-test ceil-expr)
                                 flr
                                 (if neg-ceil
                                     (bdd-negate (if-left ceil-expr))
                                     (if-left ceil-expr))
                                 flr
                                 (if neg-ceil
                                     (bdd-negate (if-right ceil-expr))
                                     (if-right ceil-expr)))))))
              (memoize-isop (bdd-id (bdd-entry flr-index))
                            (bdd-id (bdd-entry ceil-index))
                            result)
              (pop-bdd-entry)
              (pop-bdd-entry)
              result))))))

(defun ipos-aux (test left-floor left-ceiling right-floor right-ceiling)
  (let ((left-floor-index (push-bdd-entry left-floor))
        (left-ceiling-index (push-bdd-entry left-ceiling))
        (right-floor-index (push-bdd-entry right-floor))
        (right-ceiling-index (push-bdd-entry right-ceiling))
        (first-index
         ;; this is the ceiling for f1'
         ;; it is the ceiling of f1 raised because of f0's coverage
         (push-bdd-entry (bdd-orop left-ceiling (bdd-negate right-floor)))))
    (let ((second-index
           (push-bdd-entry
            ;; this is the ceiling of f0'
            (bdd-orop (bdd-entry right-ceiling-index)
                      (bdd-negate (bdd-entry left-floor-index))))))
      ;; left is assigned ipos1 while right is assigned ipos0
      ;; the BDDs for left and right are pointed by left-index and
      ;; right-index respectively.
      (let* ((left (ipos (bdd-entry left-floor-index)
                         (bdd-entry first-index)))
             (left-index (push-bdd-entry (first left)))
             (right (ipos (bdd-entry right-floor-index)
                          (bdd-entry second-index)))
             (right-index (push-bdd-entry (first right))))
        (let ((leftdpceil-index
               ;; the ceiling for f1'' is raised because of coverage
               ;; by ipos1
               (push-bdd-entry
                (bdd-orop (bdd-entry left-ceiling-index)
                          (bdd-negate (bdd-entry left-index)))))
              (rightdpceil
               ;; similarly with the ceiling for f0''
               (bdd-orop (bdd-entry right-ceiling-index)
                         (bdd-negate (bdd-entry right-index)))))
          (let ((ceiling-index
                 ;; ceiling for fd
                 (push-bdd-entry
                  (bdd-andop (bdd-orop rightdpceil
                                       (bdd-entry left-floor-index))
                             (bdd-orop (bdd-entry leftdpceil-index)
                                       (bdd-entry right-floor-index)))))
                (floor-index
                 ;; floor for fd
                 (push-bdd-entry (bdd-orop (bdd-entry left-floor-index)
                                           (bdd-entry right-floor-index)))))
            ;; compute iposd using floor and ceiling of fd
            (let* ((dontcare (ipos (bdd-entry floor-index)
                                   (bdd-entry ceiling-index)))
                   (dontcare-index (push-bdd-entry (first dontcare)))
                   ;; (or v isop0)
                   (res1-index
                    (push-bdd-entry
                     (bdd-orop (bdd-make-if test *bdd-one* *bdd-zero*)
                               (bdd-entry right-index))))
                   ;; (or (not v) isop1)
                   (res2
                    (bdd-orop (bdd-make-if test *bdd-zero* *bdd-one*)
                              (bdd-entry left-index)))
                   ;; ipos = (and (or v ipos0) (or (not v) ipos1) iposd)
                   (result-bdd
                    (bdd-andop (bdd-andop (bdd-entry res1-index) res2)
                               (bdd-entry dontcare-index))))
              (dotimes (i 13) (pop-bdd-entry))
              (list result-bdd
                    (zbdd-union
                     (zbdd-change (second right) (zbdd-atom test))
                     (zbdd-union (zbdd-change (second left)
                                              (zbdd-complement-atom test))
                                 (second dontcare)))))))))))

;;; zbdd-to-cube-set simply collects all the paths to 1.

(defvar *zbdd-cube-set* nil)

(defun zbdd-to-cube-set (zbdd)
  (setq *zbdd-cube-set* nil)
  (zbdd-to-cube-set-aux zbdd nil)
  *zbdd-cube-set*)

(defun zbdd-to-cube-set-aux (zbdd path)
  (cond ((equal zbdd 1) (push path *zbdd-cube-set*))
        ((equal zbdd 0))
        (t (zbdd-to-cube-set-aux (if-left zbdd) (cons (if-test zbdd) path))
           (zbdd-to-cube-set-aux (if-right zbdd) path))))

(defun prop-weight (formula order)
  (cond ((not-p formula)
         (length (member-equal (not-expr formula) order)))
        (t (length (member-equal formula order)))))

(defun insert-into-collection (pair collection)
  (cond ((or (null collection) (> (car pair) (caar collection)))
         (cons pair collection))
        (t (cons (car collection)
                 (insert-into-collection pair (cdr collection))))))

(defun prop-rearrange (formula order)
  (cond ((and-p formula)
         (let ((collected nil))
           (loop for expr in (cdr formula)
                 do (let ((r (prop-rearrange expr order)))
                      (setq collected
                            (insert-into-collection
                             (cons (prop-weight r order) r)
                             collected))))
           (cons 'and (loop for r in collected
                            collect (cdr r)))))
        ((or-p formula)
         (let ((collected nil))
           (loop for expr in (cdr formula)
                 do (let ((r (prop-rearrange expr order)))
                      (setq collected
                            (insert-into-collection
                             (cons (prop-weight r order) r)
                             collected))))
           (cons 'or (loop for r in collected
                            collect (cdr r)))))
        ((not-p formula)
         (make-not (prop-rearrange (not-expr formula) order)))
        ((implies-p formula)
         (make-implies (prop-rearrange (implies-left formula) order)
                       (prop-rearrange (implies-right formula) order)))
        ((if-p formula)
         (make-if (prop-rearrange (if-test formula) order)
                  (prop-rearrange (if-left formula) order)
                  (prop-rearrange (if-right formula) order)))
        (t formula)))



;;; ============ Top level functions for DNF and CNF ===============


(defun dnf (formula)
  (let* ((initial-order (construct-initial-order formula))
         (input-order (construct-input-order formula))
         (rearranged (prop-rearrange formula initial-order)))
    (loop for atom in initial-order
          do (bdd-integrate-atomic atom))
    (let ((internal-formula
           (set-expression rearranged)))
      (let ((internal-formula-index (push-bdd-entry internal-formula)))
        (bdd-clear-and-recache)
        (setq internal-formula (bdd-entry internal-formula-index))
        (pop-bdd-entry)
        (setup-isop)
        (let* ((result (isop internal-formula internal-formula))
               (cube-set (zbdd-to-cube-set (second result)))
               (result-formula
                (cond ((null cube-set) *false*)
                      ((= (length cube-set) 1) (dnf-aux (first cube-set)))
                      (t (let ((result nil))
                           (loop for disjunct in cube-set
                                 do (push (dnf-aux disjunct) result))
                           (cons 'or result))))))
          (reset-isop)
          (reset-bdd)
          (prop-rearrange result-formula input-order))))))

(defun dnf-aux (result)
  (cond ((null result) *true*)
	((= (length result) 1)
	 (zbdd-print-atom (first result)))
	(t (let ((res nil))
	     (loop for conjunct in result
		   do (push (zbdd-print-atom conjunct) res))
	     (cons 'and res)))))


(defun cnf (formula)
  (let* ((initial-order (construct-initial-order formula))
         (input-order (construct-input-order formula))
         (rearranged (prop-rearrange formula initial-order)))
    (loop for atom in initial-order
          do (bdd-integrate-atomic atom))
    (let ((internal-formula
           (set-expression rearranged)))
      (let ((internal-formula-index (push-bdd-entry internal-formula)))
        (bdd-clear-and-recache)
        (setq internal-formula (bdd-entry internal-formula-index))
        (pop-bdd-entry)
        (setup-isop)
        (let* ((result (ipos internal-formula internal-formula))
               (cube-set (zbdd-to-cube-set (second result)))
               (result-formula
                (cond ((null cube-set) *true*)
                      ((= (length cube-set) 1) (cnf-aux (first cube-set)))
                      (t (let ((result nil))
                           (loop for disjunct in cube-set
                                 do (push (cnf-aux disjunct) result))
                           (cons 'and result))))))
          (reset-isop)
          (reset-bdd)
          (prop-rearrange result-formula input-order))))))

(defun cnf-aux (result)
  (cond ((null result) *false*)
	((= (length result) 1)
	 (zbdd-print-atom (first result)))
	(t (let ((res nil))
	     (loop for disjunct in result
		   do (push (zbdd-print-atom disjunct) res))
	     (cons 'or res)))))



;;; ========================= New Interface ===========================


;;; To initialize, use setup-bdd.
;;; Note that the atoms (variables) and their order must already be
;;; decided.

(defun set-atoms (atoms)
  (setq *atom-list* (reverse atoms))
  (reset-atom-array))

(defun setup-bdd (atoms)
  (reset-bdd)
  (set-atoms atoms))


;;; To get a BDD representation of a formula,
;;; use (bdd-integrate-formula formula).

;;; To make the BDD representation persistent, use (push-bdd-entry bdd).
;;; The function returns an index.  Because BDDs may be reconstructed
;;; by garbage collection, use (bdd-entry index) to safely access the BDD.
;;; Use (pop-bdd-entry) when done with the BDD.
;;; (next-bdd-entry-index) should be saved as a backup point
;;; and (undo-bdd-entries-back-to index) should be used to
;;; clear the BDD entries from index on.

;;; Sometimes we need to clear and recache after undoing using
;;; (bdd-clear-and-recache)

;;; Operations on BDDs:
;;; - (bdd-negate bdd)
;;; - (bdd-andop bdd1 bdd2)
;;; - (bdd-orop bdd1 bdd2)
;;; - (bdd-ignore bdd vars)
;;; - (bdd-override bdd overrides)
;;; - (bdd-rename bdd vars)
;;; - (bdd-rel-product bdd1 bdd2 vars)

;;; Note that vars is a list of variable ids.
;;; To get a variable's id, use (bdd-integrate-atomic var).
;;; However, make sure that var is already integrated by setup-bdd.

;;; For bdd-override, overrides is an ordered list of dotted pairs:
;;; (var-id . bdd-id).  The var-ids must be strictly increasing.
;;; Each bdd-id must be either 0 or 1 (representing FALSE or TRUE).


;;; Function to get a disjunctive normal form of a BDD

(defun bdd-to-dnf (bdd)
  (reset-isop)
  (setup-isop)
  (let* ((result (isop bdd bdd))
         (cube-set (zbdd-to-cube-set (second result)))
         (result-formula
                (cond ((null cube-set) *false*)
                      ((= (length cube-set) 1) (dnf-aux (first cube-set)))
                      (t (let ((result nil))
                           (loop for disjunct in cube-set
                                 do (push (dnf-aux disjunct) result))
                           (cons 'or result))))))
    (prop-rearrange result-formula
                    (append (reverse *atom-list*)
                            (mapcar #'(lambda (x) (list 'n x))
                                    (reverse *atom-list*))))))


;;; Function to get a conjunctive normal form of a BDD

(defun bdd-to-cnf (bdd)
  (reset-isop)
  (setup-isop)
  (let* ((result (ipos bdd bdd))
         (cube-set (zbdd-to-cube-set (second result)))
         (result-formula
                (cond ((null cube-set) *true*)
                      ((= (length cube-set) 1) (cnf-aux (first cube-set)))
                      (t (let ((result nil))
                           (loop for conjunct in cube-set
                                 do (push (cnf-aux conjunct) result))
                           (cons 'and result))))))
    (prop-rearrange result-formula
                    (append (reverse *atom-list*)
                            (mapcar #'(lambda (x) (list 'n x))
                                    (reverse *atom-list*))))))


;;; Function to get a disjunctive normal form of a formula

(defun formula-to-dnf (formula)
  (reset-isop)
  (setup-isop)
  (let* ((bdd (bdd-integrate-formula formula))
         (result (isop bdd bdd))
         (cube-set (zbdd-to-cube-set (second result)))
         (result-formula
                (cond ((null cube-set) *false*)
                      ((= (length cube-set) 1) (dnf-aux (first cube-set)))
                      (t (let ((result nil))
                           (loop for disjunct in cube-set
                                 do (push (dnf-aux disjunct) result))
                           (cons 'or result))))))
    (prop-rearrange result-formula
                    (append (reverse *atom-list*)
                            (mapcar #'(lambda (x) (list 'n x))
                                    (reverse *atom-list*))))))


;;; Function to get a conjunctive normal form of a BDD

(defun formula-to-cnf (formula)
  (reset-isop)
  (setup-isop)
  (let* ((bdd (bdd-integrate-formula formula))
         (result (ipos bdd bdd))
         (cube-set (zbdd-to-cube-set (second result)))
         (result-formula
                (cond ((null cube-set) *true*)
                      ((= (length cube-set) 1) (cnf-aux (first cube-set)))
                      (t (let ((result nil))
                           (loop for conjunct in cube-set
                                 do (push (cnf-aux conjunct) result))
                           (cons 'and result))))))
    (prop-rearrange result-formula
                    (append (reverse *atom-list*)
                            (mapcar #'(lambda (x) (list 'n x))
                                    (reverse *atom-list*))))))

