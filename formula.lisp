
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

;;; ================ Formulas for Propositional Logic ==================

(defvar *true* '(true))
(defvar *false* '(false))

;;; recognizer for true
(defun true-p (formula) (equal formula *true*))

;;; recognizer for false
(defun false-p (formula) (equal formula *false*))

;;; Constructors and recognizers for propositional connectives.

(defun make-or (left right) `(or ,left ,right))
(defun or-p (formula) (and (listp formula) (eq (car formula) 'or)))

(defun make-and (left right) `(and ,left ,right))
(defun and-p (formula) (and (listp formula) (eq (car formula) 'and)))

;;; simplify nots in obvious cases

(defun make-not (x)
  (cond ((equal x *true*) *false*)
        ((equal x *false*) *true*)
        ((not-p x) (not-expr x))
        (t (list 'not x))))

(defun not-p (formula) (and (listp formula) (eq (car formula) 'not)))
(defun not-expr (formula) (second formula))

(defun make-implies (left right) `(implies ,left ,right))
(defun implies-p (formula) (and (listp formula) (eq (car formula) 'implies)))
(defun implies-left (formula) (second formula))
(defun implies-right (formula) (third formula))

(defun make-if (test left right) `(if ,test ,left ,right))
(defun if-p (formula) (and (listp formula) (eq (car formula) 'if)))
(defun if-test (formula) (second formula))
(defun if-left (formula) (third formula))
(defun if-right (formula) (fourth formula))

(defun make-conjunction (x y)
  (cond ((or (equal x *false*) (equal y *false*)) *false*)
        ((equal x *true*) y)
        ((equal y *true*) x)
        ;; next two cases flattens the and
        ;; comment out if we decide not to flatten
        ((and (listp x) (eq (car x) 'and))
         (cond ((and (listp y) (eq (car y) 'and))
                (cons 'and (append (cdr x) (cdr y))))
               (t (cons 'and (append (cdr x) (list y))))))
        ((and (listp y) (eq (car y) 'and))
         (cons 'and (cons x (cdr y))))
        (t (list 'and x y))))

(defun make-disjunction (x y)
  (cond ((or (equal x *true*) (equal y *true*)) *true*)
        ((equal x *false*) y)
        ((equal y *false*) x)
        ;; next two cases flattens the or
        ;; comment out if we decide not to flatten
        ((and (listp x) (eq (car x) 'or))
         (cond ((and (listp y) (eq (car y) 'or))
                (cons 'or (append (cdr x) (cdr y))))
               (t (cons 'or (append (cdr x) (list y))))))
        ((and (listp y) (eq (car y) 'or))
         (cons 'or (cons x (cdr y))))
        (t (list 'or x y))))

(defun flatten-and (formula)
  (cond ((and-p formula)
         (cons 'and (flatten-and-aux (cdr formula))))
        (t formula)))

(defun flatten-and-aux (formula-list)
  (loop for formula in formula-list
        append (cond ((and-p formula)
                      (flatten-and-aux (cdr formula)))
                     (t (list formula)))))

(defun flatten-or (formula)
  (cond ((or-p formula)
         (cons 'or (flatten-or-aux (cdr formula))))
        (t formula)))

(defun flatten-or-aux (formula-list)
  (loop for formula in formula-list
        append (cond ((or-p formula)
                      (flatten-or-aux (cdr formula)))
                     (t (list formula)))))

(defun make-= (left right)
  (list '= left right))

(defun =-p (formula)
  (and (listp formula) (= (length formula) 3) (eq (car formula) '=)))



;;; Formulas for LTL


;;; X is the "next" operator.

(defun make-x (formula)
  `(x ,formula))

(defun x-p (formula)
  (and (listp formula) (eq (car formula) 'x)))

(defun x-expr (formula)
  (second formula))


;;; F is the "future" operator.

(defun make-f (formula)
  `(f ,formula))

(defun f-p (formula)
  (and (listp formula) (eq (car formula) 'f)))

(defun f-expr (formula)
  (second formula))


;;; G is the "globally" operator.

(defun make-g (formula)
  `(g ,formula))

(defun g-p (formula)
  (and (listp formula) (eq (car formula) 'g)))

(defun g-expr (formula)
  (second formula))


;;; U is the "until" operator.

(defun make-u (left right)
  `(u ,left ,right))

(defun u-p (formula)
  (and (listp formula) (eq (car formula) 'u)))

(defun u-left (formula)
  (second formula))

(defun u-right (formula)
  (third formula))


;;; R is the "release" operator.

(defun make-r (left right)
  `(r ,left ,right))

(defun r-p (formula)
  (and (listp formula) (eq (car formula) 'r)))

(defun r-left (formula)
  (second formula))

(defun r-right (formula)
  (third formula))


(defun modal-p (formula)
  (or (x-p formula) (f-p formula) (g-p formula) (u-p formula) (r-p formula)))

;;; Normalize LTL formula to use only NOT, AND, OR, X and U.
;;; This is the normalization used in the CGH encoding.

(defun cgh-normal-form (formula)
  (cond ((or (atom formula) (true-p formula) (false-p formula))
         formula)
        ((not-p formula)
         (make-not (cgh-normal-form (not-expr formula))))
        ((or-p formula)
         (cons 'or (loop for expr in (cdr formula)
                         collect (cgh-normal-form expr))))
        ((and-p formula)
         ;(make-not
         ;  (cons 'or (loop for expr in (cdr formula)
         ;                  collect (make-not (cgh-normal-form expr)))))
         (cons 'and (loop for expr in (cdr formula)
                         collect (cgh-normal-form expr))))
        ((implies-p formula)
         (make-or (make-not (cgh-normal-form (implies-left formula)))
                  (cgh-normal-form (implies-right formula))))
        ((x-p formula)
         (make-x (cgh-normal-form (x-expr formula))))
        ((f-p formula)
         (make-u *true* (cgh-normal-form (f-expr formula))))
        ((g-p formula)
         (make-not
           (make-u *true*
                   (make-not (cgh-normal-form (g-expr formula))))))
        ((u-p formula)
         (make-u (cgh-normal-form (u-left formula))
                 (cgh-normal-form (u-right formula))))
        ((r-p formula)
         (make-not
           (make-u (make-not (cgh-normal-form (r-left formula)))
                   (make-not (cgh-normal-form (r-right formula))))))
        (t formula)))

;;; Normalize LTL formula to only allow NOT on atomic formulas.
;;; This produces a negation normal form used in the TGBA encoding.

(defun nnf (formula)
  (cond ((not-p formula) (nnf-aux (not-expr formula)))
        ((or (or-p formula) (and-p formula) (x-p formula)
             (f-p formula) (g-p formula) (u-p formula) (r-p formula))
         (cons (car formula)
               (loop for f in (cdr formula)
                     collect (nnf f))))
        ((implies-p formula)
         (make-or (nnf-aux (implies-left formula))
                  (nnf (implies-right formula))))
        (t formula)))

(defun nnf-aux (formula)
  (cond ((not-p formula) (nnf (not-expr formula)))
        ((or-p formula)
         (cons 'and
               (loop for f in (cdr formula)
                     collect (nnf-aux f))))
        ((and-p formula)
         (cons 'or
               (loop for f in (cdr formula)
                     collect (nnf-aux f))))
        ((implies-p formula)
         (make-and (nnf (implies-left formula))
                   (nnf-aux (implies-right formula))))
        ((x-p formula) (make-x (nnf-aux (x-expr formula))))
        ((f-p formula) (make-g (nnf-aux (f-expr formula))))
        ((g-p formula) (make-f (nnf-aux (g-expr formula))))
        ((u-p formula)
         (make-r (nnf-aux (u-left formula)) (nnf-aux (u-right formula))))
        ((r-p formula)
         (make-u (nnf-aux (r-left formula)) (nnf-aux (r-right formula))))
        (t (make-not formula))))


;;; Returns t iff formula is purely a propositional formula.

(defun purely-propositional-p (formula)
  (cond ((or (atom formula) (true-p formula) (false-p formula)) t)
        ((or (not-p formula) (or-p formula) (and-p formula)
             (implies-p formula))
         (let ((result t))
           (loop for expr in (cdr formula)
                 when (not (purely-propositional-p expr))
                 do (setq result nil))
           result))
        ((or (x-p formula) (f-p formula) (g-p formula) (u-p formula)
             (r-p formula))
         nil)
        (t t)))

(defun almost-purely-propositional-p (formula)
  (cond ((or (atom formula) (true-p formula) (false-p formula)) t)
        ((or (not-p formula) (or-p formula) (and-p formula)
             (implies-p formula))
         (let ((result t))
           (loop for expr in (cdr formula)
                 when (not (almost-purely-propositional-p expr))
                 do (setq result nil))
           result))
        ((or (f-p formula) (g-p formula) (u-p formula) (r-p formula))
         nil)
        ;; allows for (X formula)
        (t t)))

;;; Form a list of u-formulas that occur in formula.
;;; Note that the list may contain duplicates.

(defun collect-u-formulas (formula)
  (cond ((or (atom formula) (true-p formula) (false-p formula)) nil)
        ((or (not-p formula) (or-p formula) (and-p formula)
             (implies-p formula) (f-p formula) (g-p formula) (x-p formula)
             (r-p formula))
         (loop for expr in (cdr formula)
               append (collect-u-formulas expr)))
        ((u-p formula)
         (cons formula
               (loop for expr in (cdr formula)
                     append (collect-u-formulas expr))))
        (t nil)))


(defun collect-temporal-formulas (formula)
  (cond ((or (atom formula) (true-p formula) (false-p formula)) nil)
        ((or (not-p formula) (or-p formula) (and-p formula)
             (implies-p formula))
         (loop for expr in (cdr formula)
               append (collect-temporal-formulas expr)))
        ((or (f-p formula) (g-p formula) (x-p formula)
             (u-p formula) (r-p formula))
         (cons formula
               (loop for expr in (cdr formula)
                     append (collect-temporal-formulas expr))))
        (t nil)))

