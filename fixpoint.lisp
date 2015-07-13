
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

;;; Various analyses including reachability, model checking,
;;; counterexample generation and deadlock detection.


;;; The calculations here require that the model
;;; is already represented using elementary blocks.


(defvar *reachability-use-batch-flag* t)




;;; ====================================

;;; *initial-states-formula* is to hold the formula representing
;;; the set of initial states (with equalities already binary-encoded).
;;; A front end to the model checker needs to set *initial-states-formula*.

(defvar *initial-states-formula* *false*)

(defun initialize-reachable-states (block-array)
  (let* ((number-of-blocks (length block-array))
         (reachable (make-array number-of-blocks)))
    ;; initially everything is unreachable
    (loop for i from 0 to (- number-of-blocks 1)
          do (let* ((block (aref block-array i))
                    (number-of-transitions
                      (length (elementary-block-transitions block))))
               (setf (aref reachable i)
                     (make-reachable
                       (push-bdd-entry *bdd-zero*)
                       (loop for i from 1 to number-of-transitions
                             collect (push-bdd-entry *bdd-zero*))
                       (loop for i from 1 to number-of-transitions
                             collect (push-bdd-entry *bdd-zero*))))))
    ;; except initial states (at entrances to initial blocks)
    (loop for i in *initial-block-indices*
          do (set-bdd-entry
               (reachable-pre-entry-states (aref reachable i))
               ;; project set of initial states to initial block
               (bdd-andop
                 (bdd-integrate-formula *initial-states-formula*)
                 (bdd-entry (elementary-block-pc-condition
                              (aref block-array i))))))
    reachable))

;;; We have two alternative strategies for computing reachable states.
;;; So far, our experiments indicate that the incremental strategy
;;; scales up better.  We leave it as an option to use the batch
;;; strategy (by setting *reachability-use-batch-flag* to non-nil).

(defun compute-reachable-states (block-array)
  (if *reachability-use-batch-flag*
      (compute-reachable-states-batch block-array)
      (compute-reachable-states-incremental block-array)))



;;; The incremental version of computing reachability.
;;; This is the "chaotic iteration" approach for computing
;;; fixpoints, mimicking Kildall's algorithm for flow analysis.
;;; Unlike traditional flow analysis, propagation is based on
;;; PCs and other guards rather than a static control flow graph.

(defun compute-reachable-states-incremental (block-array)
  (let ((reachable (initialize-reachable-states block-array))
        (number-of-blocks (length block-array))
        ;; start with initial blocks in work-list
        (work-list *initial-block-indices*))
    ;; main loop to compute reachable states
    ;; processing work-list in FIFO fashion
    (loop while (not (null work-list))
          do
          (let* ((next-index (car work-list))
                 (next (aref reachable next-index))
                 (pre-entry-index (reachable-pre-entry-states next)))
            (setq work-list (cdr work-list))
            ;; iterate over guarded updates
            (loop for i from 0 to (- (length (reachable-entry-states next)) 1)
                  do
                  (let* ((trans (nth i (elementary-block-transitions
                                         (aref block-array next-index))))
                         (condition (bdd-entry (transition-condition trans)))
                         ;; compute (and I guard)
                         (entry-states (bdd-andop (bdd-entry pre-entry-index)
                                                  condition)))
                    (unless (eq entry-states
                                (bdd-entry
                                  (nth i (reachable-entry-states next))))
                      (set-bdd-entry (nth i (reachable-entry-states next))
                                     entry-states)
                      (let ((exit-states
                              ;; compute (override (and I guard) updates)
                              (bdd-override entry-states
                                            (transition-effects trans)))
                            (ns-index (nth i (reachable-exit-states next))))
                        (unless (eq exit-states (bdd-entry ns-index))
                          (set-bdd-entry ns-index exit-states)
                          ;; immediately propagate change
                          ;; i.e., modify work-list
                          ;; note that we still continue the iteration
                          ;; with the remaining guarded updates
                          (setq work-list
                                (propagate-reachable-change-incremental
                                  block-array reachable
                                  ns-index work-list)))))))))
    ;; set exit states when done (is this redundant?)
    (loop for i from 0 to (- number-of-blocks 1)
          do (let ((rs (aref reachable i)))
               (loop for j from 0 to (- (length (reachable-entry-states rs)) 1)
                     do (set-bdd-entry
                          (nth j (reachable-exit-states rs))
                          (bdd-override
                            (bdd-entry (nth j (reachable-entry-states rs)))
                            (transition-effects
                              (nth j (elementary-block-transitions
                                       (aref block-array i)))))))))
    ;; also set predecessors and successors
    ;; note that predecessor/successor can be in a different thread:
    ;; (the execution of a block
    ;;  -  can leave another thread intact:
    ;;     the other thread is still "enabled" at the same point as
    ;;     before the execution,
    ;;  -  can be synchronized with the execution of a block in
    ;;     another thread, and
    ;;  -  can spawn other threads)
    (loop for i from 0 to (- number-of-blocks 1)
          do (let* ((rs (aref reachable i))
                    (pre-entry-index (reachable-pre-entry-states rs))
                    (exit-indices (reachable-exit-states rs))
                    (exit (push-bdd-entry (bdd-or-on-list exit-indices))))
               (setf (reachable-predecessors rs)
                     (loop for j from 0 to (- number-of-blocks 1)
                           ;; states in predecessor's exit states
                           ;; and in current block's pre-entry states
                           when (not (eq (bdd-andop
                                           (bdd-or-on-list
                                             (reachable-exit-states
                                               (aref reachable j)))
                                           (bdd-entry pre-entry-index))
                                         *bdd-zero*))
                           collect j))
               (setf (reachable-successors rs)
                     (loop for j from 0 to (- number-of-blocks 1)
                           ;; states in successor's pre-entry
                           ;; states and in current block's exit states
                           when (not (eq (bdd-andop
                                           (bdd-entry
                                             (reachable-pre-entry-states
                                               (aref reachable j)))
                                           (bdd-entry exit))
                                         *bdd-zero*))
                           collect j))
               (pop-bdd-entry)))
    reachable))



;;; The batch version of computing reachability.
;;; Each iteration at the top level computes images
;;; for all blocks in the work-list.
;;; Propagation is performed once every top level iteration.

(defun compute-reachable-states-batch (block-array)
  (let ((reachable (initialize-reachable-states block-array))
        (number-of-blocks (length block-array))
        ;; start with initial blocks in work-list
        (work-list *initial-block-indices*))
    ;; main loop to compute reachable states
    ;; unlike the incremental version, process everything
    ;; in current work-list in a single top-level iteration
    (loop while (not (null work-list))
          do
          (let ((indices work-list)
                (propagates nil))
            (setq work-list nil)
            ;; iterate over blocks in the work-list
            (loop
              for x in indices
              do
              (let* ((next (aref reachable x))
                     (pre-entry-index (reachable-pre-entry-states next)))
                ;; iterate over guarded updates
                (loop
                  for i from 0 to (- (length (reachable-entry-states next)) 1)
                  do
                  (let* ((trans (nth i (elementary-block-transitions
                                         (aref block-array x))))
                         (condition (bdd-entry (transition-condition trans)))
                         ;; compute (and I guard)
                         (entry-states (bdd-andop (bdd-entry pre-entry-index)
                                                  condition)))
                    (unless (eq entry-states
                                (bdd-entry
                                  (nth i (reachable-entry-states next))))
                      (set-bdd-entry (nth i (reachable-entry-states next))
                                     entry-states)
                      (let ((exit-states
                              ;; compute (override (and I guard) updates)
                              (bdd-override entry-states
                                            (transition-effects trans)))
                            (ns-index (nth i (reachable-exit-states next))))
                        (unless (eq exit-states (bdd-entry ns-index))
                          (set-bdd-entry ns-index exit-states)
                          ;; defer propagation, add to list
                          (push ns-index propagates))))))))
            (unless (null propagates)
              ;; propagate collected changes
              (setq work-list
                    (propagate-reachable-change-batch
                      block-array reachable propagates)))))
    ;; set exit states when done
    (loop for i from 0 to (- number-of-blocks 1)
          do (let ((rs (aref reachable i)))
               (loop for j from 0 to (- (length (reachable-entry-states rs)) 1)
                     do (set-bdd-entry
                          (nth j (reachable-exit-states rs))
                          (bdd-override
                            (bdd-entry (nth j (reachable-entry-states rs)))
                            (transition-effects
                              (nth j (elementary-block-transitions
                                       (aref block-array i)))))))))
    ;; also set predecessors and successors
    (loop for i from 0 to (- number-of-blocks 1)
          do (let* ((rs (aref reachable i))
                    (pre-entry-index (reachable-pre-entry-states rs))
                    (exit-indices (reachable-exit-states rs))
                    (exit (push-bdd-entry (bdd-or-on-list exit-indices))))
               (setf (reachable-predecessors rs)
                     (loop for j from 0 to (- number-of-blocks 1)
                           ;; states in predecessor's exit states
                           ;; and in current block's pre-entry states
                           when (not (eq (bdd-andop
                                           (bdd-or-on-list
                                             (reachable-exit-states
                                               (aref reachable j)))
                                           (bdd-entry pre-entry-index))
                                         *bdd-zero*))
                           collect j))
               (setf (reachable-successors rs)
                     (loop for j from 0 to (- number-of-blocks 1)
                           ;; states in successor's pre-entry
                           ;; states and in current block's exit states
                           when (not (eq (bdd-andop
                                           (bdd-entry
                                             (reachable-pre-entry-states
                                               (aref reachable j)))
                                           (bdd-entry exit))
                                         *bdd-zero*))
                           collect j))
               (pop-bdd-entry)))
    reachable))



;;; *** currently ns-index points to the full set of exit states
;;; (for a guarded update), using deltas might be faster?

(defun propagate-reachable-change-incremental
    (block-array reachable ns-index work-list)
  ;; ns-index points to the BDD for the exit states of a transition
  (let ((result work-list)
        (number-of-blocks (length block-array)))
    ;; iterate over all blocks
    (loop for i from 0 to (- number-of-blocks 1)
          do
          (let* ((block (aref block-array i))
                 ;; project exit states to block (pre-entry)
                 (new-pre (bdd-andop (bdd-entry (elementary-block-pc-condition
                                                block))
                                     (bdd-entry ns-index))))
            (unless (eq new-pre *bdd-zero*)
              (let* ((ps-index
                       (reachable-pre-entry-states (aref reachable i)))
                     (extended-pre (bdd-orop (bdd-entry ps-index)
                                             new-pre)))
                (unless (eq extended-pre (bdd-entry ps-index))
                  (set-bdd-entry ps-index extended-pre)
                  ;; block i needs to be in updated work-list
                  (unless (member-equal i result)
                    (setq result (append result (list i)))))))))
    result))

(defun propagate-reachable-change-batch (block-array reachable ns-indices)
  ;; ns-indices are pointers to the BDDs for the exit states of
  ;; transitions
  (let ((result nil)
        (number-of-blocks (length block-array))
        ;; collect the exit states
        (new-index (push-bdd-entry (bdd-or-on-list ns-indices))))
    ;; iterate over all blocks
    (loop for x from 0 to (- number-of-blocks 1)
          do
          (let* ((block (aref block-array x))
                 (new-pre (bdd-andop (bdd-entry (elementary-block-pc-condition
                                                block))
                                     (bdd-entry new-index))))
            (unless (eq new-pre *bdd-zero*)
              (let* ((ps-index
                       (reachable-pre-entry-states (aref reachable x)))
                     (extended-pre (bdd-orop (bdd-entry ps-index)
                                             new-pre)))
                (unless (eq extended-pre (bdd-entry ps-index))
                  (set-bdd-entry ps-index extended-pre)
                  ;; block i needs to be in updated work-list
                  (unless (member-equal x result)
                    (setq result (append result (list x)))))))))
    (pop-bdd-entry)
    result))



;;; =================== LTL Model Checking =======================


;;; Encoding schemes.
;;; CGH - classic encoding
;;; GBA - NNF, transition constraints based on X formulas,
;;;       direct encoding of eventualities, strict encoding
;;; GBA-LOOSE - loose encoding version of GBA
;;; RGBA - GBA with reduced set of operators, strict encoding
;;; RGBA-LOOSE - loose encoding version of RGBA
;;; TGBA - NNF, more complex transition constraints based on T_{\varphi}
;;;        rules, promise variables for eventualities, strict encoding
;;; TGBA-LOOSE - loose encoding version of TGBA
;;; RTGBA - TGBA with reduced set of operators, strict encoding
;;; RTGBA-LOOSE - loose encoding version of RTGBA

(defvar *encoding-schemes* '(cgh gba gba-loose tgba tgba-loose
                             rgba rgba-loose rtgba rtgba-loose))

;;; Encoding scheme used (default is TGBA-LOOSE).

(defvar *default-encoding-scheme* 'tgba-loose)

(defvar *encoding-scheme* 'tgba-loose)




;;; Given an LTL formula to be satisfiability checked,
;;; construct the augmented model (incorporating the tableau).
;;; This will work regardless of whether reachable states
;;; have been computed. It does "take advantage" of
;;; reachable states.

(defun construct-augmented-model (reachable block-array formula)
  (let* ((number-of-blocks (length block-array))
         ;; variables for elementary formulas
         (v-formulas (remove-duplicates (produce-v-formulas formula)))
         ;; promise variables
         (promise (and (or (eq *encoding-scheme* 'tgba)
                           (eq *encoding-scheme* 'tgba-loose)
                           (eq *encoding-scheme* 'rtgba)
                           (eq *encoding-scheme* 'rtgba-loose))
                       (produce-promise-variables formula)))
         ;; "next" terms
         ;;(next (and (or (eq *encoding-scheme* 'tgba)
         ;;               (eq *encoding-scheme* 'tgba-loose)
         ;;               (eq *encoding-scheme* 'rtgba)
         ;;               (eq *encoding-scheme* 'rtgba-loose))
         ;;           (produce-next-terms formula)))
         (tableau (make-tableau formula v-formulas)))
    ;; NOTE: It is assumed that at this point, the elementary
    ;; formulas, promise variables and next terms have been
    ;; integrated into the BDD (the interface code must ensure this).

    ;; Fairness constraints (in abstract syntax form)
    (setf (tableau-fairness-formulas tableau)
          (construct-fairness-formulas formula))
    ;; These are state variables added by the tableau
    (setf (tableau-x-vars tableau)
          (sort (mapcar #'bdd-integrate-atomic
                        (append v-formulas promise))
                #'<))
    ;; Fairness constraints (in BDD form)
    (setf (tableau-fairness-constraints tableau)
          (loop for c in (tableau-fairness-formulas tableau)
                collect (push-bdd-entry (bdd-integrate-formula c))))
    ;; Construct the tableau cases.
    ;; The constructed tableau cases will be stored in the
    ;; transition-constraints slot of the tableau struct.
    (construct-tableau-cases tableau)
    ;; Now that the tableau is fully constructed,
    ;; construct the augmented model
    (let ((x-vars (tableau-x-vars tableau))
          ;; reserve array for the augmented model
          ;; (to be populated using (make-bs bs-entry bs-exit))
          (bs-array (make-array number-of-blocks))
          (spec (push-bdd-entry
                  (bdd-integrate-formula (produce-sphi-phi formula))))
          (constraints (tableau-transition-constraints tableau))
          (m (- (length (tableau-transition-constraints tableau)) 1)))
      (loop
        for i from 0 to (- number-of-blocks 1)
        do
        (let* ((transitions 
                 (elementary-block-transitions (aref block-array i)))
               (n (- (length transitions) 1))
               (bs-entry (loop for j from 0 to n
                               collect
                               (loop for k from 0 to m
                                     collect (push-bdd-entry *bdd-zero*))))
               (bs-exit (loop for j from 0 to n
                              collect
                              (loop for k from 0 to m
                                    collect (push-bdd-entry *bdd-zero*)))))
           (loop
             for j from 0 to n
             do
             (loop
               for k from 0 to m
               do
               (progn
                 (set-bdd-entry
                   (nth k (nth j bs-entry))
                   (bdd-andop
                     (bdd-entry (transition-condition-index
                                  reachable block-array i j))
                     (bdd-entry (transition-constraint-entry
                                  (nth k constraints)))))
                 ;; extra constraint because of additional effects of
                 ;; transition-constraint-entry for tgba encodings
                 (if (or (eq *encoding-scheme* 'tgba)
                         (eq *encoding-scheme* 'tgba-loose)
                         (eq *encoding-scheme* 'rtgba)
                         (eq *encoding-scheme* 'rtgba-loose))
                     (set-bdd-entry
                       (nth k (nth j bs-exit))
                       (bdd-andop
                         (bdd-andop
                           (bdd-override
                             (bdd-andop
                               (bdd-ignore
                                 (bdd-entry (transition-constraint-entry
                                              (nth k constraints)))
                                 x-vars)
                               (bdd-entry (transition-condition-index
                                            reachable block-array i j)))
                            (transition-effects (nth j transitions)))
                           (bdd-entry (transition-post-index
                                        reachable block-array i j)))
                         (bdd-entry (transition-constraint-exit
                                      (nth k constraints)))))
                     (set-bdd-entry
                       (nth k (nth j bs-exit))
                       (bdd-andop
                         (bdd-entry (transition-post-index
                                      reachable block-array i j))
                         (bdd-entry (transition-constraint-exit
                                      (nth k constraints))))))
                 ;; Now remove states that can't progress
                 ;; (i.e., their transitions produce states that
                 ;;  don't satisfy the "next" requirements).
                 (set-bdd-entry
                   (nth k (nth j bs-entry))
                   (bdd-andop
                     (bdd-ignore
                       (bdd-entry (nth k (nth j bs-exit)))
                       ;; order is important
                       (sort
                         (append
                           (tableau-x-vars tableau)
                           (mapcar #'car
                                   (transition-effects (nth j transitions))))
                         #'<))
                     (bdd-entry (nth k (nth j bs-entry))))))))
           (setf (aref bs-array i) (make-bs bs-entry bs-exit))))
      (make-augmented-model formula spec tableau bs-array))))

;;; These functions produce a BDD index

(defun transition-condition-index (reachable block-array i j)
  (if (null reachable)
      (transition-condition
        (nth j (elementary-block-transitions (aref block-array i))))
      (nth j (reachable-entry-states (aref reachable i)))))

(defun transition-post-index (reachable block-array i j)
  (if (null reachable)
      (transition-post
        (nth j (elementary-block-transitions (aref block-array i))))
      (nth j (reachable-exit-states (aref reachable i)))))



;;; ==================== Encoding of LTL ======================

;;; Normalize LTL formula according to encoding scheme.

(defun normalize-ltl-formula (formula)
  (cond ((eq *encoding-scheme* 'cgh)
         (cgh-normal-form formula))
        ((or (eq *encoding-scheme* 'rgba)
             (eq *encoding-scheme* 'rgba-loose)
             (eq *encoding-scheme* 'rtgba)
             (eq *encoding-scheme* 'rtgba-loose))
         (nnf (cgh-normal-form formula)))
        (t
         (nnf formula))))

;;; Given a normalized LTL formula, produce a list of non-AP elementary
;;; formulas, each of which will be represented as a BDD variable.
;;; The list produced may contain duplicates.

(defun produce-v-formulas (formula)
  (cond ((eq *encoding-scheme* 'cgh) (produce-cgh-formulas formula))
        ((or (eq *encoding-scheme* 'gba) (eq *encoding-scheme* 'gba-loose)
             (eq *encoding-scheme* 'rgba) (eq *encoding-scheme* 'rgba-loose))
         (produce-gba-formulas formula))
        ((or (eq *encoding-scheme* 'tgba) (eq *encoding-scheme* 'tgba-loose)
             (eq *encoding-scheme* 'rtgba) (eq *encoding-scheme* 'rtgba-loose))
         (cond ((has-unnested-x-p formula)
                (cons `(el ,formula)
                      (produce-tgba-formulas formula)))
               (t (produce-tgba-formulas formula))))))

;;; CGH-specific elementary formulas

(defun produce-cgh-formulas (formula)
  (cond ((purely-propositional-p formula) nil)
        ((or (not-p formula) (or-p formula) (and-p formula))
         (loop for expr in (cdr formula)
               append (produce-cgh-formulas expr)))
        ((x-p formula)
         (cons (list 'el formula) (produce-cgh-formulas (x-expr formula))))
        ((u-p formula)
         (cons (list 'el (make-x formula))
               (loop for expr in (cdr formula)
                     append (produce-cgh-formulas expr))))))

;;; GBA-specific elementary formulas

(defun produce-gba-formulas (formula)
  (cond ((purely-propositional-p formula) nil)
        ((or (or-p formula) (and-p formula))
         (loop for expr in (cdr formula)
               append (produce-gba-formulas expr)))
        ((x-p formula)
         (cons (list 'el formula) (produce-gba-formulas (x-expr formula))))
        ((or (f-p formula) (g-p formula) (u-p formula) (r-p formula))
         (cons (list 'el (make-x formula))
               (loop for expr in (cdr formula)
                     append (produce-gba-formulas expr))))))

;;; TGBA-specific elementary formulas

(defun produce-tgba-formulas (formula)
  (cond ((purely-propositional-p formula) nil)
        ((or (or-p formula) (and-p formula))
         (loop for expr in (cdr formula)
               append (produce-tgba-formulas expr)))
        ((and (x-p formula)
              (not (purely-propositional-p (x-expr formula))))
         (cons `(el ,(x-expr formula))
               (produce-tgba-formulas (x-expr formula))))
        ((g-p formula)
         (cons `(el ,formula)
               (cond ((f-p (g-expr formula))
                      (produce-tgba-formulas (f-expr (g-expr formula))))
                     (t (produce-tgba-formulas (g-expr formula))))))
        ((or (f-p formula) (u-p formula) (r-p formula))
         (cons `(el ,formula)
               (loop for expr in (cdr formula)
                     append (produce-tgba-formulas expr))))))


(defun has-unnested-x-p (formula)
  (cond ((x-p formula) t)
        ((or (g-p formula) (f-p formula) (u-p formula) (r-p formula)) nil)
        ((or (or-p formula) (and-p formula))
         (let ((result nil))
           (loop for expr in (cdr formula)
                 when (has-unnested-x-p expr)
                 do (setq result t))
           result))
        (t nil)))


;;; Only certain subformulas need promise variables.

(defun produce-promise-variables (formula)
  (let ((form nil))
    (loop for v in (remove-duplicates (produce-v-formulas formula))
          when
          (progn
            (setq form (second v))
            (or (u-p form)
                (f-p form)
                (and (g-p form) (f-p (g-expr form)))))
          collect `(promise ,form))))

;;; Produce "next" terms (for TGBA and TGBA-LOOSE).
;;; We can determine from traversing \varphi, what the required "next"
;;; terms will be.

(defun produce-next-terms (formula)
  (remove-duplicates (produce-next-terms-aux formula)))

(defun produce-next-terms-aux (formula)
  (cond ((purely-propositional-p formula) nil)
        ((or (or-p formula) (and-p formula))
         (loop for expr in (cdr formula)
               append (produce-next-terms-aux expr)))
        ((x-p formula)
         (if (purely-propositional-p (x-expr formula))
             `((next ,(x-expr formula)))
             (cons `(next (el ,(x-expr formula)))
                   (produce-next-terms-aux (x-expr formula)))))
        ((g-p formula)
         (cons `(next (el ,formula))
               (cond ((f-p (g-expr formula))
                      (produce-next-terms-aux (f-expr (g-expr formula))))
                     (t (produce-next-terms-aux (g-expr formula))))))
        ((or (f-p formula) (u-p formula) (r-p formula))
         (cons `(next (el ,formula))
               (loop for expr in (cdr formula)
                     append (produce-next-terms-aux expr))))))

;;; Produce a list of fairness formulas.
;;; For CGH, GBA GBA-LOOSE, RGBA and RGBA-LOOSE, this
;;; involves general "path commitments" (S\phi).
;;; For TGBA, TGBA-LOOSE, RTGBA and RTGBA-LOOSE, we simply use
;;; "promise" variables.

(defun construct-fairness-formulas (formula)
  (cond ((eq *encoding-scheme* 'cgh)
         (loop for v in (remove-duplicates
                          (collect-temporal-formulas formula))
               when (u-p v)
               collect
               (make-or (make-not (produce-sat-formula v))
                        (produce-sat-formula (car (last v))))))
        ((or (eq *encoding-scheme* 'gba)
             (eq *encoding-scheme* 'gba-loose)
             (eq *encoding-scheme* 'rgba)
             (eq *encoding-scheme* 'rgba-loose))
         (loop for v in (remove-duplicates
                          (collect-temporal-formulas formula))
               when (or (u-p v) (f-p v))
               collect
               (make-or (make-not (produce-sat-formula-gba v))
                        (produce-sat-formula-gba (car (last v))))))
        ((or (eq *encoding-scheme* 'tgba)
             (eq *encoding-scheme* 'tgba-loose)
             (eq *encoding-scheme* 'rtgba)
             (eq *encoding-scheme* 'rtgba-loose))
         (loop for v in (produce-promise-variables formula)
               collect (make-not v)))))




;;; Construct the tableau cases.
;;; The constructed tableau cases will be stored in the
;;; transition-constraints slot of the tableau struct.
;;; For CGH and GBA, the cases follow directly from combinations of X's.
;;; For TGBA, need to "evaluate" transition constraints and convert to
;;; disjunctive normal form: each disjunct is a case with "next" factors
;;; determining things to hold in the next state for the case.

(defun construct-tableau-cases (tableau)
  (let ((combinations (combinations (tableau-v-formulas tableau))))
    (cond ((or (eq *encoding-scheme* 'cgh)
               (eq *encoding-scheme* 'gba)
               (eq *encoding-scheme* 'rgba))
           (setf (tableau-transition-constraints tableau)
                 (loop for c in combinations
                       unless (eq (bdd-integrate-formula
                                    (combination-to-next-formula c))
                                  *bdd-zero*)
                       collect
                       (make-transition-constraint
                         (push-bdd-entry
                           (bdd-integrate-formula
                             (combination-to-formula c)))
                         (push-bdd-entry
                           (bdd-integrate-formula
                             (combination-to-next-formula c)))))))
          ((or (eq *encoding-scheme* 'gba-loose)
               (eq *encoding-scheme* 'rgba-loose))
           (setf (tableau-transition-constraints tableau)
                 (loop for c in combinations
                       unless (eq (bdd-integrate-formula
                                    (loose-combination-to-next-formula c))
                                  *bdd-zero*)
                       collect
                       (make-transition-constraint
                         (push-bdd-entry
                           (bdd-integrate-formula
                             (combination-to-formula c)))
                         (push-bdd-entry
                           (bdd-integrate-formula
                             (loose-combination-to-next-formula c)))))))
          ((or (eq *encoding-scheme* 'tgba)
               (eq *encoding-scheme* 'rtgba)
               (eq *encoding-scheme* 'tgba-loose)
               (eq *encoding-scheme* 'rtgba-loose))
           (let ((constraint-pairs
                   (loop for c in combinations
                         append
                         (construct-transition-pairs-tgba c)))
                 (constraints nil))
             (loop
               for pair in constraint-pairs
               do
               ;; See if the "next state" constraint (cdr of pair)
               ;; has a BDD representation that is already an
               ;; exit part of a transition-constraint in constraints.
               (let ((exit (bdd-integrate-formula (cdr pair)))
                     (found nil))
                 (loop
                   for tc in constraints
                   while (not found)
                   when
                   (eq (bdd-entry (transition-constraint-exit tc)) exit)
                   do
                   (progn (set-bdd-entry
                            (transition-constraint-entry tc)
                            (bdd-orop
                              (bdd-integrate-formula (car pair))
                              (bdd-entry (transition-constraint-entry tc))))
                          (setq found t)))
                 (unless found
                   (setq constraints
                         (append constraints
                                 (list
                                   (make-transition-constraint
                                     (push-bdd-entry
                                       (bdd-integrate-formula (car pair)))
                                     (push-bdd-entry exit))))))))
           (setf (tableau-transition-constraints tableau)
                 constraints))))))

;;; combination is of variables for elementary formulas

(defun construct-transition-pairs-tgba (combination)
  (let* ((entries
           (cond
             ((or (eq *encoding-scheme* 'tgba) (eq *encoding-scheme* 'rtgba))
              (loop
                for entry in combination
                collect
                (if (= (cdr entry) 1)
                    (produce-tphi-tgba (second (car entry)))
                    (make-not
                      (produce-tphi-tgba (second (car entry)))))))
             (t ; loose encoding
              (loop
                for entry in combination
                when (= (cdr entry) 1)
                collect
                (produce-tphi-tgba (second (car entry)))))))
         (dnf-formula
           (formula-to-dnf
             (cond ((null entries) *true*)
                   ((= (length entries) 1) (car entries))
                   (t (cons 'and entries))))))
    (cond
      ((false-p dnf-formula) nil)
      ((or-p dnf-formula)
       (loop
         for c in (cdr dnf-formula)
         collect (construct-single-transition-pair-tgba c combination)))
      (t (list (construct-single-transition-pair-tgba
                 dnf-formula
                 combination))))))

;;; constraint is a transition constraint (a single case) that results from
;;; applying rules for S_{\varphi}.
;;; combination is a combination of variables for elementary formulas
;;; (must be added as a "from" constraint).
;;; the function separates the "from" and "to" constraints
;;; and returns them as a dotted pair.

(defun construct-single-transition-pair-tgba (constraint combination)
  (let ((current-constraints
          (append (loop for entry in combination
                        collect
                        (if (= (cdr entry) 1)
                            (car entry)
                            (make-not (car entry))))
                  (cond ((and-p constraint)
                         (sort
                           (loop for factor in (cdr constraint)
                                 unless (is-next-constraint-p factor)
                                 collect factor)
                           #'alphalessp))
                        ((not (is-next-constraint-p constraint))
                         (list constraint))
                        (t nil))))
        (next-constraints
          (cond ((and-p constraint)
                 (sort
                   (loop for factor in (cdr constraint)
                         when (is-next-constraint-p factor)
                         collect (next-constraint factor))
                   #'alphalessp))
                ((is-next-constraint-p constraint)
                 (list (next-constraint constraint)))
                (t nil))))
    (cons (cond ((null current-constraints) *true*)
                ((= (length current-constraints) 1)
                 (car current-constraints))
                (t (cons 'and current-constraints)))
          (cond ((null next-constraints) *true*)
                ((= (length next-constraints) 1)
                 (car next-constraints))
                (t (cons 'and next-constraints))))))


;;; Recognizer for formulas of the forms (next p) or (not (next p)).

(defun is-next-constraint-p (formula)
  (or (and (listp formula) (eq (car formula) 'next))
      (and (not-p formula)
           (listp (second formula))
           (eq (car (second formula)) 'next))))

;;; (next p) => p
;;; (not (next p)) => (not p)

(defun next-constraint (formula)
  (cond ((and (listp formula) (eq (car formula) 'next))
         (second formula))
        ((and (not-p formula)
              (listp (second formula))
              (eq (car (second formula)) 'next))
         (make-not (second (second formula))))))

(defun entries-all-negated-p (combination)
  (every #'(lambda (u) (= (cdr u) 0)) combination))

;;; Produce formulas associated with combinations
;;; (of negated/unnegated v-formulas).

;;; combination is a conjunction of negated/unnegated entries.

(defun combination-to-formula (combination)
  (cond ((null combination) *true*)
        ((= (length combination) 1)
         (if (= (cdar combination) 1)
             (caar combination)
             (make-not (caar combination))))
        (t
         (cons 'and
               (loop for entry in combination
                     collect (if (= (cdr entry) 1)
                                 (car entry)
                                 (make-not (car entry))))))))

;;; Given a combination that holds at the entry states,
;;; produce the formula that needs to hold at the exit states
;;; (with the help of produce-sat-formula)
;;; Currently, it is used for the CGH and GBA encodings.

(defun combination-to-next-formula (combination)
  (cond ((null combination) *true*)
        ((= (length combination) 1)
         (combination-entry-to-next-formula (car combination)))
        (t
         (cons 'and
               (loop for entry in combination
                     collect (combination-entry-to-next-formula entry))))))

(defun combination-entry-to-next-formula (entry)
  (if (= (cdr entry) 1)
      (cond ((eq *encoding-scheme* 'cgh)
             ;; (car entry) is of the form (el (x p))
             (produce-sat-formula (x-expr (second (car entry)))))
            ((eq *encoding-scheme* 'gba)
             (produce-sat-formula-gba (x-expr (second (car entry))))))
      (cond ((eq *encoding-scheme* 'cgh)
             (make-not (produce-sat-formula (x-expr (second (car entry))))))
            ((eq *encoding-scheme* 'gba)
             (make-not (produce-sat-formula-gba
                         (x-expr (second (car entry)))))))))

;;; The following is used for GBA-LOOSE and RGBA-LOOSE encodings
;;; where we ignore the effects of negated entries in combinations.

(defun loose-combination-to-next-formula (combination)
  (let ((entries (loop for entry in combination
                       when (= (cdr entry) 1)
                       collect
                       (produce-sat-formula-gba
                         (x-expr (second (car entry)))))))
    (cond ((null entries) *true*)
          ((= (length entries) 1) (car entries))
          (t (cons 'and entries)))))




;;; "Evaluate" a path commitment, producing a proposition.

;;; The CGH version of "evaluating" a path commitment.
;;; This corresponds to the rules for S_{\varphi}

(defun produce-sat-formula (formula)
  (cond ((purely-propositional-p formula) formula)
        ((not-p formula)
         (make-not (produce-sat-formula (not-expr formula))))
        ((or-p formula)
         (cons 'or
               (loop for expr in (cdr formula)
                     collect (produce-sat-formula expr))))
        ((and-p formula)
         (cons 'and
               (loop for expr in (cdr formula)
                     collect (produce-sat-formula expr))))
        ((u-p formula)
         (make-disjunction
           (produce-sat-formula (u-right formula))
           (make-conjunction (produce-sat-formula (u-left formula))
                             (list 'el (make-x formula)))))
        ((x-p formula) (list 'el formula))
        (t formula)))

;;; The GBA version of "evaluating" a path commitment
;;; (S_{\varphi}).

(defun produce-sat-formula-gba (formula)
  (cond ((purely-propositional-p formula) formula)
        ((or-p formula)
         (cons 'or
               (loop for expr in (cdr formula)
                     collect (produce-sat-formula-gba expr))))
        ((and-p formula)
         (cons 'and
               (loop for expr in (cdr formula)
                     collect (produce-sat-formula-gba expr))))
        ((f-p formula)
         (make-disjunction
           (produce-sat-formula-gba (f-expr formula))
           (list 'el (make-x formula))))
        ((g-p formula)
         (make-conjunction
           (produce-sat-formula-gba (g-expr formula))
           (list 'el (make-x formula))))
        ((u-p formula)
         (make-disjunction
           (produce-sat-formula-gba (u-right formula))
           (make-conjunction (produce-sat-formula-gba (u-left formula))
                             (list 'el (make-x formula)))))
        ((r-p formula)
         (make-conjunction
           (produce-sat-formula-gba (r-right formula))
           (make-disjunction (produce-sat-formula-gba (r-left formula))
                             (list 'el (make-x formula)))))
        ((x-p formula) (list 'el formula))
        (t formula)))

;;; The TGBA version of "evaluating" a transition constraint.
;;; It corresponds to the rules for T_{\varphi}.

(defun produce-tphi-tgba (formula)
  (cond ((or-p formula)
         (cons 'or
               (loop for expr in (cdr formula)
                     collect (produce-tphi-tgba expr))))
        ((and-p formula)
         (cons 'and
               (loop for expr in (cdr formula)
                     collect (produce-tphi-tgba expr))))
        ((x-p formula)
         (if (purely-propositional-p (x-expr formula))
             ;; ******* should we normalize this?
             `(next ,(x-expr formula))
             `(next ,(produce-sphi-tgba (x-expr formula)))
             ))
        ((and (g-p formula) (f-p (g-expr formula)))
         (make-conjunction
           (produce-tphi-tgba `(x ,formula))
           (make-disjunction
             (produce-tphi-tgba (f-expr (g-expr formula)))
             `(promise ,formula))))
        ((f-p formula)
         (make-disjunction
           (produce-tphi-tgba (f-expr formula))
           (make-conjunction
             `(promise ,formula)
             (produce-tphi-tgba `(x ,formula))
             )))
        ((g-p formula)
         (make-conjunction
           (produce-tphi-tgba (g-expr formula))
           (produce-tphi-tgba `(x ,formula))
           ))
        ((u-p formula)
         (make-disjunction
           (produce-tphi-tgba (u-right formula))
           (make-conjunction
             (produce-tphi-tgba (u-left formula))
             (make-conjunction `(promise ,formula)
                               (produce-tphi-tgba `(x ,formula))
                               ))))
        ((r-p formula)
         (make-conjunction
           (produce-tphi-tgba (r-right formula))
           (make-disjunction (produce-tphi-tgba (r-left formula))
                             (produce-tphi-tgba `(x ,formula))
                             )))
        (t formula)))

(defun produce-sphi-tgba (formula)
  (cond ((or (x-p formula) (g-p formula) (f-p formula)
             (u-p formula) (r-p formula))
         `(el ,formula))
        ((or (or-p formula) (and-p formula))
         (cons (car formula)
               (loop for expr in (cdr formula)
                     collect (produce-sphi-tgba expr))))
        (t formula)))




;;; ==================== Computing Fair States ======================

;;; This is a two-level interacting fixpoint computation.

;;; At the top level it is a relatively simple greatest
;;; fixpoint computation.  At each iteration of the
;;; outermost computation, inner least fixpoint computations
;;; (one for each constraint) are performed, with the current
;;; approximation of the outermost computation (i.e., the variable
;;; of the outer fixpoint operator)  "passed as a parameter".

(defvar *ltl-uses-batch-flag* nil)

(defvar *ltl-uses-reachability-flag* t)

;;; Function to produce S_{\varphi}(\varphi).

(defun produce-sphi-phi (formula)
  (cond ((eq *encoding-scheme* 'cgh)
         (produce-sat-formula formula))
        ((or (eq *encoding-scheme* 'gba)
             (eq *encoding-scheme* 'gba-loose)
             (eq *encoding-scheme* 'rgba)
             (eq *encoding-scheme* 'rgba-loose))
         (produce-sat-formula-gba formula))
        ((or (eq *encoding-scheme* 'tgba)
             (eq *encoding-scheme* 'tgba-loose)
             (eq *encoding-scheme* 'rtgba)
             (eq *encoding-scheme* 'rtgba-loose))
         (cond ((has-unnested-x-p formula) `(el ,formula))
               (t (convert-to-elementary formula))))))


(defun convert-to-elementary (formula)
  (cond ((or (g-p formula) (f-p formula) (u-p formula) (r-p formula))
         `(el ,formula))
        ((or (or-p formula) (and-p formula))
         (cons (car formula)
               (loop for expr in (cdr formula)
                     collect (convert-to-elementary expr))))
        (t formula)))


;;; constraint is a BDD index

(defun constrain-bs-entry-array (bs-entry-array constraint tableau)
  (let ((number-of-blocks (length bs-entry-array))
        (m (- (length (tableau-transition-constraints tableau)) 1)))
    (loop
      for i from 0 to (- number-of-blocks 1)
      do
      (let ((bs (aref bs-entry-array i)))
        (loop
          for j from 0 to (- (length bs) 1)
          do
          (loop
            for k from 0 to m
            do
            (set-bdd-entry
              (nth k (nth j bs))
              (bdd-andop (bdd-entry (nth k (nth j bs)))
                         (bdd-entry constraint)))))))))


;;; Apply augmented forward transfer function for kth tableau case
;;; of jth guarded update of ith elementary block to the proposition
;;; (characterizing a set of augmented states) pointed by bdd-index.
;;; block-array represents the initial model.

(defun forward-transfer-augmented (bdd-index i j k tableau block-array)
  (let ((x-vars (tableau-x-vars tableau))
        (tc (nth k (tableau-transition-constraints tableau)))
        (trans (elementary-block-transitions (aref block-array i))))
    (bdd-andop
      (bdd-override
        (bdd-andop
          (bdd-ignore
            (bdd-andop (bdd-entry bdd-index)
                       (bdd-entry (transition-constraint-entry tc)))
            x-vars)
          (bdd-entry (transition-condition (nth j trans))))
        (transition-effects (nth j trans)))
      (bdd-entry
        (transition-constraint-exit tc)))))

;;; Apply augmented reverse transfer function for kth tableau case
;;; of jth guarded update of ith elementary block to the proposition
;;; (characterizing a set of augmented states) pointed by bdd-index.
;;; block-array represents the initial model.

(defun reverse-transfer-augmented (bdd-index i j k tableau block-array)
  (let ((x-vars (tableau-x-vars tableau))
        (tc (nth k (tableau-transition-constraints tableau)))
        (trans (elementary-block-transitions (aref block-array i))))
    (bdd-andop
      (bdd-andop
        (bdd-ignore
          (bdd-andop
            (bdd-ignore
              (bdd-andop (bdd-entry bdd-index)
                         (bdd-entry (transition-constraint-exit tc)))
              x-vars)
            (bdd-entry (transition-post (nth j trans))))
          (mapcar #'car (transition-effects (nth j trans))))
        (bdd-entry (transition-condition (nth j trans))))
      (bdd-entry
        (transition-constraint-entry tc)))))



(defvar *mc-loop-count* 0)
(defvar *mc-aux-count* 0)
(defvar *mc-aux-loop-count* 0)


;;; Top level function for computing set of fair states.
;;; model is the augmented model.
;;; gc is an index to the BDD for a global constraint.


(defun compute-mc (reachable block-array model gc)
  (unless (null model)
    (let* ((change t)
           (tableau (augmented-model-tableau model))
           (mc (initialize-mc model gc))
           (undo-point (next-bdd-entry-index))
           ;; reserve BDD indices for fixpoint computation
           (mc-constraints
             (loop
               for constraint in (tableau-fairness-constraints tableau)
               collect (make-copy-bs-entry-array (augmented-model-bs model))))
           (mc-next-constraints
             (loop
               for constraint in (tableau-fairness-constraints tableau)
               collect (make-copy-bs-entry-array (augmented-model-bs model)))))
      (setq *mc-loop-count* 0)
      (setq *mc-aux-count* 0)
      (setq *mc-aux-loop-count* 0)
      ;; This outer loop does a greatest fixpoint computation.
      (loop while change
            do
            (progn
              (incf *mc-loop-count*)
              (loop for i from 0 to (- (length mc-constraints) 1)
                    do (compute-mc-aux
                         reachable block-array mc i mc-constraints))
              ;; convert each constraint from mu Y (Z ^ constraint) v (EX Y)
              ;; to EX ( mu Y (Z ^ constraint) v (EX Y))
              (loop for i from 0 to (- (length mc-constraints) 1)
                    do (set-bs-entry-array-as-predecessors
                         (nth i mc-next-constraints)
                         (nth i mc-constraints)
                         model block-array gc))
              ;; take conjunction of EX ( mu Y (Z ^ constraint_i) v (EX Y))
              (setq change
                   (set-bs-entry-array-as-conjunction
                       (mc-top mc)
                       mc-next-constraints
                       model))
             ))
      ;; release unneeded BDD indices
      (undo-bdd-entries-back-to undo-point)
      ;; save stats
      (setf (mc-loop-count mc) *mc-loop-count*)
      (setf (mc-aux-count mc) *mc-aux-count*)
      (setf (mc-aux-loop-count mc) *mc-aux-loop-count*)
      mc)))

(defun initialize-mc (model gc)
  (let ((mc (make-mc gc model *ltl-uses-reachability-flag*))
        (tableau (augmented-model-tableau model)))
    ;; top is initialized with the bs array of the augmented model
    ;; (i.e., we don't have to start with the universe)
    (setf (mc-top mc)
          (make-copy-bs-entry-array (augmented-model-bs model)))
    (constrain-bs-entry-array (mc-top mc) gc tableau)
    ;; set mc-spec to S_{\varphi}(\varphi) \wedge gc
    (setf (mc-spec mc)
          (push-bdd-entry
            (bdd-andop (bdd-entry (augmented-model-spec model))
                       (bdd-entry gc))))
    mc))

;;; compute-mc-aux performs a least fixpoint computation for the
;;; ith constraint: mu Y . (Z ^ constraint_i) v (EX Y)

(defun compute-mc-aux (reachable block-array mc i mc-constraints)
  (if *ltl-uses-batch-flag*
      (compute-mc-aux-batch block-array mc i mc-constraints)
      (compute-mc-aux-incremental reachable block-array mc i mc-constraints)))

(defun compute-mc-aux-incremental (reachable block-array mc i mc-constraints)
  (let ((tableau (augmented-model-tableau (mc-model mc))))
    (let ((number-of-blocks (length block-array))
          (m (- (length (tableau-transition-constraints tableau)) 1))
          (constraint (nth i (tableau-fairness-constraints tableau)))
          (bs-entry-array (nth i mc-constraints))
          (model-bs-array (augmented-model-bs (mc-model mc))))
      (incf *mc-aux-count*)
      ;; bs-entry-array is initialized to a restriction of top to
      ;; the appropriate fairness constraint.
      ;; top represents the current approximation of the outer
      ;; (greatest) fixpoint (Z)
      (set-bs-entry-array bs-entry-array (mc-top mc))
      (constrain-bs-entry-array bs-entry-array constraint tableau)
      (let ((work-list
              (loop for x from 0 to (- number-of-blocks 1)
                    unless (entries-all-false (aref bs-entry-array x))
                    collect x))
            ;;(saved-loop-count *mc-aux-loop-count*)
            (all-indices (loop for i from 0 to (- number-of-blocks 1)
                               collect i)))
        ;;(format t "~%Length of work-list: ~A~%" (length work-list))
        (when (null work-list)
          (format t "~%work-list initially empty~%"))
        (loop
          while (not (null work-list))
          do
          (let* ((x (car work-list))
                 (bdd-index (push-bdd-entry
                             (bdd-or-on-list
                              (flatten-into-list
                                (aref bs-entry-array x))))))
            (setq work-list (cdr work-list))
            (incf *mc-aux-loop-count*)
            (loop
              for y in (cond ((null reachable) all-indices)
                             (t (reachable-predecessors (aref reachable x))))
              do
              (let* ((bs-entry (aref bs-entry-array y))
                     (model-bs (aref model-bs-array y))
                     (n (- (length bs-entry) 1)))
                (loop
                  for j from 0 to n
                  do
                  (loop
                    for k from 0 to m
                    do
                    (let* ((model-exit-index (nth k (nth j (bs-exit model-bs))))
                           (next-bdd-index
                             (push-bdd-entry
                               (bdd-andop (bdd-entry bdd-index)
                                          (bdd-entry model-exit-index))))
                           (bdd (bdd-orop
                                  (bdd-andop
                                    (reverse-transfer-augmented
                                      next-bdd-index y j k tableau
                                      block-array)
                                    (bdd-entry (mc-gc mc)))
                                   (bdd-entry
                                      (nth k (nth j bs-entry))))))
                      (unless
                         (eq bdd (bdd-entry (nth k (nth j bs-entry))))
                        (set-bdd-entry (nth k (nth j bs-entry)) bdd)
                        (unless (member-equal y work-list)
                           (setq work-list (append work-list (list y)))))
                      (pop-bdd-entry))))))
            (pop-bdd-entry)))
        ))))

(defun compute-mc-aux-batch (block-array mc i mc-constraints)
  (let ((tableau (augmented-model-tableau (mc-model mc))))
    (let ((number-of-blocks (length block-array))
          (m (- (length (tableau-transition-constraints tableau)) 1))
          (constraint (nth i (tableau-fairness-constraints tableau)))
          (bs-entry-array (nth i mc-constraints))
          (model-bs-array (augmented-model-bs (mc-model mc)))
          (change t))
      (incf *mc-aux-count*)
      ;; bs-entry-array is initialized to a restriction of top to
      ;; the appropriate fairness constraint.
      ;; top represents the current approximation of the outer
      ;; (greatest) fixpoint (Z)
      (set-bs-entry-array bs-entry-array (mc-top mc))
      (constrain-bs-entry-array bs-entry-array constraint tableau)
      (loop
        while change
        do
        (let ((bdd-index (push-bdd-entry (collect-states-as-disjunction
                                           bs-entry-array))))
          (setq change nil)
          (incf *mc-aux-loop-count*)
          (loop
            for x from 0 to (- number-of-blocks 1)
            do
            (let* ((bs-entry (aref bs-entry-array x))
                   (model-bs (aref model-bs-array x))
                   (n (- (length bs-entry) 1)))
              (loop
                for j from 0 to n
                do
                (loop
                  for k from 0 to m
                  do
                  (let* ((model-exit-index (nth k (nth j (bs-exit model-bs))))
                         (next-bdd-index
                           (push-bdd-entry
                             (bdd-andop (bdd-entry bdd-index)
                                        (bdd-entry model-exit-index))))
                         (bdd (bdd-orop
                                (bdd-andop
                                  (reverse-transfer-augmented
                                    next-bdd-index x j k tableau block-array)
                                  (bdd-entry (mc-gc mc)))
                                (bdd-entry (nth k (nth j bs-entry))))))
                    (unless
                       (eq bdd (bdd-entry (nth k (nth j bs-entry))))
                       (set-bdd-entry (nth k (nth j bs-entry)) bdd)
                       (setq change t))
                    (pop-bdd-entry))))))
          (pop-bdd-entry))))))




;;; Set result (a bs-entry array) to hold the states that are
;;; in the intersection of the states in args (a list of bs-entry arrays).

(defun set-bs-entry-array-as-conjunction (result args model)
  (let* ((tableau (augmented-model-tableau model))
         (m (- (length (tableau-transition-constraints tableau)) 1))
         (change nil))
    (loop
      for x from 0 to (- (length result) 1)
      do
      (let ((result-bs-entry (aref result x))
            (n (- (length (aref result x)) 1)))
        (loop
          for j from 0 to n
          do
          (loop
            for k from 0 to m
            do
            (let ((tmp-entry
                    (push-bdd-entry
                      (bdd-entry
                        (nth k (nth j (aref (car args) x)))))))
              (loop
                for arg in (cdr args)
                do
                (set-bdd-entry
                  tmp-entry
                  (bdd-andop
                    (bdd-entry (nth k (nth j (aref arg x))))
                    (bdd-entry tmp-entry))))
              (unless (eq (bdd-entry (nth k (nth j result-bs-entry)))
                          (bdd-entry tmp-entry))
                (set-bdd-entry
                  (nth k (nth j result-bs-entry))
                  (bdd-entry tmp-entry))
                (setq change t))
              (pop-bdd-entry))))))
    change))

;;; Set bs-entry-array to hold predecessor states of cbs-entry-array.

(defun set-bs-entry-array-as-predecessors
    (bs-entry-array cbs-entry-array model block-array gc)
  (let* ((tableau (augmented-model-tableau model))
         (m (- (length (tableau-transition-constraints tableau)) 1))
         (bdd-index (push-bdd-entry (collect-states-as-disjunction
                                      cbs-entry-array))))
    (loop for x from 0 to (- (length bs-entry-array) 1)
          do
          (let ((bs-entry (aref bs-entry-array x))
                (model-bs (aref (augmented-model-bs model) x))
                (n (- (length (aref bs-entry-array x)) 1)))
            (loop for j from 0 to n
                  do
                  (loop for k from 0 to m
                        do
                        (let* ((model-exit-index
                                 (nth k (nth j (bs-exit model-bs))))
                               (next-bdd-index
                                 (push-bdd-entry
                                   (bdd-andop
                                     (bdd-entry bdd-index)
                                     (bdd-entry model-exit-index)))))
                          (set-bdd-entry
                             (nth k (nth j bs-entry))
                             (bdd-andop
                               (reverse-transfer-augmented
                                 next-bdd-index
                                 x j k tableau
                                 block-array)
                               (bdd-entry gc)))
                          (pop-bdd-entry))))))
    (pop-bdd-entry)))




;;; =================== Counterexample States ===================


;;; Compute states that can be involved in counterexamples.
;;; This is a reachability computation from states in initial
;;; fair states where V(\varphi) is true.


;;; This should be an array of bs.

(defvar *counterexample-use-batch-flag* t)

;;; Initialize the entry states for initial blocks.

(defun initialize-counterexample-states (mc gc)
  (let ((bs-array (make-array (length (mc-top mc))))
        (tableau (augmented-model-tableau (mc-model mc))))
    ;; initially everything is unreachable
    (loop for i from 0 to (- (length bs-array) 1)
          do (let ((mc-bs-entry (aref (mc-top mc) i)))
               (setf (aref bs-array i)
                     (make-bs
                       (loop for x in mc-bs-entry
                             collect
                             (loop for y in x
                                   collect (push-bdd-entry *bdd-zero*)))
                       (loop for x in mc-bs-entry
                             collect
                             (loop for y in x
                                   collect (push-bdd-entry *bdd-zero*)))))))
    ;; except initial states that are in mc that satisfy spec and gc
    (let ((spec (mc-spec mc))
          (m (- (length (tableau-transition-constraints tableau)) 1))
          (init-index (push-bdd-entry
                        (bdd-integrate-formula *initial-states-formula*))))
      (loop
        for i in *initial-block-indices*
        do
        (let ((mc-bs-entry (aref (mc-top mc) i))
              (ctr-bs (aref bs-array i)))
          (loop
            for j from 0 to (- (length mc-bs-entry) 1)
            do
            (loop
              for k from 0 to m
              do
              (set-bdd-entry
                (nth k (nth j (bs-entry ctr-bs)))
                (bdd-andop
                  (bdd-andop
                    (bdd-andop (bdd-entry (nth k (nth j mc-bs-entry)))
                               (bdd-entry init-index))
                    (bdd-entry gc))
                  (bdd-entry spec)))))))
      (pop-bdd-entry)
      bs-array)))

(defun compute-counterexample-states (reachable block-array mc gc)
  (if *counterexample-use-batch-flag*
      (compute-counterexample-states-batch block-array mc gc)
      (compute-counterexample-states-incremental reachable block-array mc gc)))

(defun compute-counterexample-states-incremental (reachable block-array mc gc)
  ;; start with initial blocks in work list
  (let ((tableau (augmented-model-tableau (mc-model mc)))
        (bs-array (initialize-counterexample-states mc gc))
        (work-list *initial-block-indices*)
        (all-indices (loop for i from 0 to (- (length block-array) 1)
                           collect i)))
    ;; main loop to compute counterexample states
    (loop
      while (not (null work-list))
      do
      (let* ((x (car work-list))
             (ctr-bs (aref bs-array x))
             (propagates nil))
        (setq work-list (cdr work-list))
        (loop
          for i from 0 to (- (length (bs-entry ctr-bs)) 1)
          ;; i iterates over the original guarded updates
          do
          (let ((cur-i (nth i (bs-entry ctr-bs)))
                (nxt-i (nth i (bs-exit ctr-bs))))
            (loop
              for j from 0 to (- (length cur-i) 1)
              ;; j iterates over combinations
              do
              (let ((tmp (forward-transfer-augmented
                             (nth j cur-i) x i j tableau block-array)))
                (unless (eq tmp (bdd-entry (nth j nxt-i)))
                  (set-bdd-entry (nth j nxt-i) tmp)
                  (push (nth j nxt-i) propagates))))))
        (unless (null propagates)
          (setq work-list
                (propagate-counterexample-change-incremental
                  propagates work-list
                  (if (null reachable)
                      all-indices
                      (reachable-successors (aref reachable x)))
                  mc bs-array)))))
    bs-array))


(defun compute-counterexample-states-batch (block-array mc gc)
  ;; start with initial blocks in work list
  (let ((tableau (augmented-model-tableau (mc-model mc)))
        (bs-array (initialize-counterexample-states mc gc))
        (work-list *initial-block-indices*))
    ;; main loop to compute counterexample states
    (loop
      while (not (null work-list))
      do
      (let ((indices work-list)
            (propagates nil))
        (setq work-list nil)
        (loop
          for x in indices
          do
          (let ((ctr-bs (aref bs-array x)))
            (loop
              for i from 0 to (- (length (bs-entry ctr-bs)) 1)
              do
              (let ((cur-i (nth i (bs-entry ctr-bs)))
                    (nxt-i (nth i (bs-exit ctr-bs))))
                (loop
                  for j from 0 to (- (length cur-i) 1)
                  do
                  (let ((tmp (forward-transfer-augmented
                                 (nth j cur-i) x i j tableau block-array)))
                    (unless (eq tmp (bdd-entry (nth j nxt-i)))
                      (set-bdd-entry (nth j nxt-i) tmp)
                      (push (nth j nxt-i) propagates))))))))
        (unless (null propagates)
          (setq work-list
                (propagate-counterexample-change-batch
                  propagates mc bs-array)))))
    bs-array))


(defun propagate-counterexample-change-incremental
       (ns-indices work-list block-indices mc bs-array)
  ;; ns-indices are pointers to the BDDs for the exit states of
  ;; transitions
  (let ((result work-list)
        (new-index (push-bdd-entry (bdd-or-on-list ns-indices))))
    (loop for x in block-indices
          do
          ;; *** prescreen using PC?
          (let ((ctr-bs (aref bs-array x))
                (mc-entry (aref (mc-top mc) x)))
            (loop for i from 0 to (- (length mc-entry) 1)
                  do
                  (loop for j from 0 to (- (length (nth i mc-entry)) 1)
                        do
                        (let ((tmp (push-bdd-entry
                                     (bdd-andop
                                       (bdd-entry new-index)
                                       (bdd-entry (nth j (nth i mc-entry))))))
                              (ctr-j (nth j (nth i (bs-entry ctr-bs)))))
                          (unless (eq (bdd-entry tmp) *bdd-zero*)
                            (set-bdd-entry tmp
                                           (bdd-orop (bdd-entry tmp)
                                                     (bdd-entry ctr-j)))
                            (unless (eq (bdd-entry tmp) (bdd-entry ctr-j))
                              (set-bdd-entry ctr-j (bdd-entry tmp))
                              (unless (member-equal x result)
                                (setq result (append result (list x))))))
                          (pop-bdd-entry))))))
    (pop-bdd-entry)
    result))


(defun propagate-counterexample-change-batch (ns-indices mc bs-array)
  ;; ns-indices are pointers to the BDDs for the exit states of
  ;; transitions
  (let ((result nil)
        (new-index (push-bdd-entry (bdd-or-on-list ns-indices))))
    (loop for x from 0 to (- (length bs-array) 1)
          do
          (let ((ctr-bs (aref bs-array x))
                (mc-entry (aref (mc-top mc) x)))
            (loop for i from 0 to (- (length mc-entry) 1)
                  do
                  (loop for j from 0 to (- (length (nth i mc-entry)) 1)
                        do
                        (let ((tmp (push-bdd-entry
                                     (bdd-andop
                                       (bdd-entry new-index)
                                       (bdd-entry (nth j (nth i mc-entry))))))
                              (ctr-j (nth j (nth i (bs-entry ctr-bs)))))
                          (unless (eq (bdd-entry tmp) *bdd-zero*)
                            (set-bdd-entry tmp
                                           (bdd-orop (bdd-entry tmp)
                                                     (bdd-entry ctr-j)))
                            (unless (eq (bdd-entry tmp) (bdd-entry ctr-j))
                              (set-bdd-entry ctr-j (bdd-entry tmp))
                              (unless (member-equal x result)
                                (setq result (append result (list x))))))
                          (pop-bdd-entry))))))
    (pop-bdd-entry)
    result))




;;; ===================================================================
;;; ================= Counterexample Path Generation ==================
;;; ===================================================================


(defvar *find-shortest-cycle* t)

(defvar *visited-depth* 0)

(defvar *max-cycle* 20)

(defvar *verbose* nil)


;;; This is a backward search to find an appropriate starting point
;;; for a counterexample cycle.

(defun counterexample-cycles (blk states gc bs-entry-array tableau block-array)
  (cond
    (*find-shortest-cycle*
     ;;(format t "~%Finding cycle for:~%~A~%"
     ;;        (bdd-to-cnf (bdd-entry states)))
     (let ((undo-point (next-bdd-entry-index))
           (result nil)
           (done nil))
       (loop
         while (not done)
         do
         (let ((visited-entry
                 (construct-initial-visited-entry
                   blk states bs-entry-array tableau)))
           (setq
             result
             (counterexample-cycles-aux
               blk states (list visited-entry) gc bs-entry-array
               tableau block-array))
           (cond
             ((null result)
              ;; (format t "~%Failed counterexample-cycles.~%")
              (undo-bdd-entries-back-to undo-point)
              (setq done t))
             ((= (length result) 1)
              ;; (format t "~%Revising cycle goal to:~%~A~%"
              ;;         (bdd-to-cnf (bdd-entry states)))
              ;; (format t "~%Visited depth:~A~%." *visited-depth*)
              (undo-bdd-entries-back-to undo-point)
              (when (eq (bdd-entry states) *bdd-zero*)
                (setq result nil)
                (setq done t)))
             (t (setq done t)))))
       result))
    (t
     (let ((visited-entry
             (construct-initial-visited-entry
               blk states bs-entry-array tableau)))
       (counterexample-cycles-aux
         blk states (list visited-entry) gc bs-entry-array
         tableau block-array)))))


(defun construct-initial-visited-entry (blk states bs-entry-array tableau)
  (let* ((combinations (combinations (tableau-fairness-constraints tableau)))
         (visited-entry
          (loop
            for c in combinations
            collect (make-false-bs-entry-array bs-entry-array))))
    (loop
      for x from 0 to (- (length combinations) 1)
      do
      (let ((constraint (push-bdd-entry
                          (combination-to-bdd (nth x combinations))))
            (v-partition (nth x visited-entry)))
        (let ((bs-entry (aref v-partition blk))
              (ctr-bs-entry (aref bs-entry-array blk)))
          (loop
            for j from 0 to (- (length bs-entry) 1)
            do
            (loop
              for k from 0 to (- (length (nth j bs-entry)) 1)
              do
              (set-bdd-entry
                (nth k (nth j bs-entry))
                (bdd-andop
                  (bdd-andop
                    (bdd-entry (nth k (nth j ctr-bs-entry)))
                    (bdd-entry states))
                  (bdd-entry constraint))))))
        (pop-bdd-entry)))
    visited-entry))

;;; Workhorse for the cycle search.
;;; ref-states: BDD index to proposition representing "starting" states
;;; visited: a list of states that are ancestors of ref-states.
;;;          The last entry can reach ref-states in 0 steps (i.e.,
;;;          the same as ref-states but partitioned according to
;;;          the combination of constraints satisfied), the second
;;;          last entry are states that can reach ref-states in 1 step
;;;          (partitioned on the combination of constraints satisfied
;;;          in the "paths" to ref-states and distributed to the
;;;          elementary blocks), etc.

(defun counterexample-cycles-aux
        (ref-blk ref-states visited gc ctr-bs-entry-array tableau block-array)
  (let* ((number-of-blocks (length ctr-bs-entry-array))
         (target (car visited))
         (combinations (combinations (tableau-fairness-constraints tableau)))
         (new-entry (loop for c in combinations
                          collect
                          (make-false-bs-entry-array ctr-bs-entry-array)))
         (target-states
           (loop
             for i from 0 to (- (length combinations) 1)
             collect
             (push-bdd-entry
               (bdd-or-on-list
                 (loop
                   for x from 0 to (- number-of-blocks 1)
                   append
                   (flatten-into-list (aref (nth i target) x))))))))
    (unless
       (> (length visited) *max-cycle*)
    ;(format t "~%~%~%=====~%States:~%")
    ;(loop for p in target-states
    ;      do
    ;      (format t "~%~A~%" (bdd-to-cnf (bdd-entry p))))
    (setq *visited-depth* (length visited))
    ;(if (> *visited-depth* 20) (cl:break))
    (loop
      for x from 0 to (- (length combinations) 1)
      ;; Process (nth x new-entry)
      do
      (let ((bs-entry-array (nth x new-entry)))
        (loop
          for y in (indices-of-source-combinations (nth x combinations))
          do
          (let ((constraint (push-bdd-entry
                              (bdd-and-on-list
                                (remaining-constraints
                                  (nth y combinations)
                                  (nth x combinations)))))
                (targ (nth y target-states)))
            (loop
              for i from 0 to (- (length bs-entry-array) 1)
              do
              (let ((bs-entry (aref bs-entry-array i)))
                (loop
                  for j from 0 to (- (length bs-entry) 1)
                  do
                  (loop
                    for k from 0 to (- (length (nth j bs-entry)) 1)
                    do
                    (set-bdd-entry
                      (nth k (nth j bs-entry))
                      (bdd-orop
                        (bdd-andop
                          (reverse-transfer-counterexamples
                            targ i j k gc ctr-bs-entry-array tableau
                            block-array)
                          (bdd-entry constraint))
                        (bdd-entry (nth k (nth j bs-entry)))))))))
            ;; Don't need constraint anymore.
            (pop-bdd-entry)))))
    ;; Don't need target-states anymore
    (loop for x in combinations do (pop-bdd-entry))
    ;; Success or continue.
    (let* ((bs-entry (aref (nth (all-positive-combination-index combinations)
                                new-entry)
                           ref-blk))
           (intersection (bdd-andop
                           (bdd-or-on-list (flatten-into-list bs-entry))
                           (bdd-entry ref-states))))
      (cond ((eq (bdd-entry ref-states) intersection)
             ;; Note that cycle must start with transition through ref-blk.
             ;; Need to clear other entries (to restrict to cycle)
             (loop
               for i from 0 to (- number-of-blocks 1)
               unless (= i ref-blk)
               do
               (let ((other-bs-entry
                       (aref (nth (all-positive-combination-index combinations)
                                  new-entry)
                             i)))
                 (loop
                   for j from 0 to (- (length other-bs-entry) 1)
                   do
                   (loop
                     for k from 0 to (- (length (nth j other-bs-entry)) 1)
                     do
                     (set-bdd-entry (nth k (nth j other-bs-entry))
                                    *bdd-zero*)))))
             (loop
               for x from 0 to (- (length combinations) 1)
               unless (= x (all-positive-combination-index combinations))
               do
               (loop
                 for i from 0 to (- number-of-blocks 1)
                 do
                 (let ((other-bs-entry (aref (nth x new-entry) i)))
                   (loop
                     for j from 0 to (- (length other-bs-entry) 1)
                     do
                     (loop
                       for k from 0 to (- (length (nth j other-bs-entry)) 1)
                       do
                       (set-bdd-entry (nth k (nth j other-bs-entry))
                                      *bdd-zero*))))))
             (list ref-states (cons new-entry visited)))
            ((and *find-shortest-cycle*
                  (not (eq intersection *bdd-zero*)))
             (set-bdd-entry ref-states intersection)
             (list ref-states))
            (t
             (counterexample-cycles-aux
               ref-blk ref-states (cons new-entry visited)
               gc ctr-bs-entry-array tableau block-array)))))))

(defvar *max-prefix* 100)

;;; Find shortest prefix.
;;; ***** Do we really need to constrain transition to blk?

(defun counterexample-prefix
    (blk states gc bs-entry-array tableau block-array spec)
  (let ((initial-states
          (push-bdd-entry
            (bdd-andop
              (bdd-integrate-formula *initial-states-formula*)
              (bdd-or-on-list
                (loop for i in *initial-block-indices*
                      append
                      (flatten-into-list (aref bs-entry-array i))))))))
    (set-bdd-entry initial-states
                   (bdd-andop (bdd-entry initial-states) (bdd-entry spec)))
    (cond 
     ((not (eq (bdd-andop (bdd-entry states) (bdd-entry initial-states))
               *bdd-zero*))
      ;; Special case when there is intersection between
      ;; initial-states and states: null prefix.
      (set-bdd-entry initial-states
                     (bdd-andop (bdd-entry states) (bdd-entry initial-states)))
      (list initial-states nil))
     (t
      (let* ((entry-to-cycle (make-false-bs-entry-array bs-entry-array))
             (bs-entry (aref entry-to-cycle blk))
             (cbs-entry (aref bs-entry-array blk)))
        ;; make entry-to-cycle based on blk and states
        (loop
          for j from 0 to (- (length bs-entry) 1)
          do
          (loop
            for k from 0 to (- (length (nth j bs-entry)) 1)
            do
            (set-bdd-entry
              (nth k (nth j bs-entry))
              (bdd-andop
                (bdd-entry (nth k (nth j cbs-entry)))
                (bdd-entry states)))))
        (counterexample-prefix-aux
          initial-states
          (list entry-to-cycle)
          (list (push-bdd-entry
                  (bdd-or-on-list
                    (loop
                      for x from 0 to (- (length bs-entry-array) 1)
                      append
                      (flatten-into-list (aref entry-to-cycle x))))))
          0 gc bs-entry-array tableau block-array))))))

(defun counterexample-prefix-aux (target-states visited visited-states
                                  depth gc bs-entry-array tableau block-array)
  (let ((number-of-blocks (length bs-entry-array))
        (new-entry (make-false-bs-entry-array bs-entry-array))
        (next-states (car visited-states)))
    ;; Compute states one step prior to next-states and couple the
    ;; states with the corresponding transitions to next-states.
    (loop
      for i from 0 to (- number-of-blocks 1)
      do
      (let ((bs-entry (aref new-entry i)))
        (loop
          for j from 0 to (- (length bs-entry) 1)
          do
          (loop
            for k from 0 to (- (length (nth j bs-entry)) 1)
            do
            (set-bdd-entry
              (nth k (nth j bs-entry))
              (bdd-orop
                (reverse-transfer-counterexamples
                  next-states i j k gc bs-entry-array tableau block-array)
                (bdd-entry (nth k (nth j bs-entry)))))))))
    (let ((new-states
           (push-bdd-entry
             (bdd-or-on-list
               (loop
                 for x from 0 to (- number-of-blocks 1)
                 append
                 (flatten-into-list (aref new-entry x))))))
          (intersection
            (bdd-andop
              (bdd-or-on-list
                (loop for i in *initial-block-indices*
                      append (flatten-into-list (aref new-entry i))))
              (bdd-entry target-states))))
      ;; Success or continue.
      (cond ((eq intersection *bdd-zero*)
             (cond ((> depth *max-prefix*)
                    ;; debugging
                    (format t "~%Prefix search hit limit.~%")
                    nil)
                   (t (let ((already-visited nil))
                        (loop for s in visited-states
                              while (not already-visited)
                              do (when (eq (bdd-entry new-states)
                                           (bdd-entry s))
                                   (setq already-visited t)))
                        (cond (already-visited
                               ;; debugging
                               ;;(format
                               ;;  t "Prefix going in circles, depth ~A.~%"
                               ;;  depth)
                               nil)
                              (t             
                               ;; continue searching.
                               (counterexample-prefix-aux
                                 target-states
                                 (cons new-entry visited)
                                 (cons new-states visited-states)
                                 (+ depth 1)
                                 gc bs-entry-array tableau block-array)))))))
            (t
             ;; success.
             (set-bdd-entry target-states intersection)
             (list target-states (cons new-entry visited)))))))


;;; Given a target combination, produce combinations that can
;;; lead to the target (by satisfying the missing constraints).

(defun source-combinations (target-combination)
  (loop for comb in (all-combinations target-combination)
        when (subset-constraints comb target-combination)
        collect comb))

(defun subset-constraints (source-combination target-combination)
  (let ((result t))
    (loop for i from 0 to (- (length source-combination) 1)
          when (and (= (cdr (nth i source-combination)) 1)
                    (= (cdr (nth i target-combination)) 0))
          do (setq result nil))
    result))

(defun indices-of-source-combinations (target-combination)
  (let ((combinations (all-combinations target-combination)))
    (loop for i from 0 to (- (length combinations) 1)
          when (subset-constraints (nth i combinations) target-combination)
          collect i)))

;;; Produce missing constraints that need to be satisfied to
;;; go from the source combination to the target combination.

(defun remaining-constraints (source-combination target-combination)
  (loop for i from 0 to (- (length source-combination) 1)
        when (and (= (cdr (nth i source-combination)) 0)
                  (= (cdr (nth i target-combination)) 1))
        collect (car (nth i source-combination))))


;;; This is the forward transfer function "projected" to counterexamples.

(defun forward-transfer-counterexamples
    (bdd-index i j k gc ctr-bs-entry-array tableau block-array)
  (let ((x-vars (tableau-x-vars tableau))
        (tc (nth k (tableau-transition-constraints tableau)))
        (trans (elementary-block-transitions (aref block-array i)))
        (ctr-bs-entry (aref ctr-bs-entry-array i)))
    (bdd-andop
      (bdd-andop
        (bdd-override
          (bdd-andop
            (bdd-ignore
              (bdd-andop
                (bdd-andop
                  (bdd-entry bdd-index)
                  (bdd-entry (transition-constraint-entry tc)))
                (bdd-entry (nth k (nth j ctr-bs-entry))))
              x-vars)
            (bdd-entry (transition-condition (nth j trans))))
          (transition-effects (nth j trans)))
        (bdd-entry
          (transition-constraint-exit tc)))
      (bdd-entry gc))))

;;; This is the reverse transfer function "projected" to counterexamples.

(defun reverse-transfer-counterexamples
         (bdd-index i j k gc ctr-bs-entry-array tableau block-array)
  (let ((x-vars (tableau-x-vars tableau))
        (tc (nth k (tableau-transition-constraints tableau)))
        (trans (elementary-block-transitions (aref block-array i)))
        (ctr-bs-entry (aref ctr-bs-entry-array i)))
    (bdd-andop
      (bdd-andop
        (bdd-andop
          (bdd-andop
            (bdd-ignore
              (bdd-andop
                (bdd-ignore
                  (bdd-andop (bdd-entry bdd-index)
                             (bdd-entry (transition-constraint-exit tc)))
                  x-vars)
                (bdd-entry (transition-post (nth j trans))))
              (mapcar #'car (transition-effects (nth j trans))))
            (bdd-entry (transition-condition (nth j trans))))
          (bdd-entry
            (transition-constraint-entry tc)))
        (bdd-entry (nth k (nth j ctr-bs-entry))))
      (bdd-entry gc))))




;;; ===== Top level function for counterexample path generation =====


;;; The trickiest part is efficiently finding a cycle that
;;; goes through block blk and satisfies all fairness constraints.
;;; Once a cycle is found, it is easy to find a suitable prefix.

;;; Parameters
;;; blk: a block index.
;;; cc: BDD index for a constraint for the goal set
;;; gc: BDD index for a global constraint

(defun counterexample-path (blk cc gc ctr-bs mc model block-array)
  (let* ((tableau (augmented-model-tableau model))
         (spec (if (null mc) (augmented-model-spec model) (mc-spec mc)))
         (undo-point (next-bdd-entry-index))
         (bs-entry-array (or (and ctr-bs
                                  (make-bs-entry-array-from-bs-array ctr-bs))
                             (mc-top mc)
                             (make-bs-entry-array-from-bs-array
                               (augmented-model-bs model)))))
  (unless
    ;; Quick check to ensure global constraint satisfied at initial
    ;; counterexample states
    (eq (bdd-andop
          (bdd-andop
            (bdd-andop
               (bdd-or-on-list
                 (flatten-into-list
                   (loop
                     for i in *initial-block-indices*
                     collect (aref bs-entry-array i))))
               (bdd-integrate-formula *initial-states-formula*))
             (bdd-entry spec))
           (bdd-entry gc))
        *bdd-zero*)
    (let* ((ref-states
             (push-bdd-entry
               (bdd-andop
                 (bdd-andop
                   (bdd-or-on-list
                     (flatten-into-list (aref bs-entry-array blk)))
                   (bdd-entry cc))
                 (bdd-entry gc))))
           (result (counterexample-cycles
                     blk ref-states gc bs-entry-array tableau block-array)))
      (cond
        ((null result)
         (undo-bdd-entries-back-to undo-point))
        (t
         (let ((cycle-result
                 (choose-cycle
                   (first result) ref-states (second result) gc
                   bs-entry-array tableau block-array)))
           (cond
             ((null cycle-result)
              (undo-bdd-entries-back-to undo-point)
              (setq result nil))
             (t
              (let ((prefix-intermediate-result
                      (counterexample-prefix
                        blk (first cycle-result) gc bs-entry-array
                        tableau block-array spec)))
                (cond
                  ((null prefix-intermediate-result)
                   (undo-bdd-entries-back-to undo-point)
                   (setq result nil))
                  (t
                   (let ((prefix-result
                           (choose-prefix
                             (first prefix-intermediate-result)
                             (first cycle-result)
                             (second prefix-intermediate-result)
                             gc bs-entry-array tableau block-array)))
                     ;; choose-prefix is guaranteed to succeed
                     (set-bdd-entry
                       (first cycle-result)
                       (bdd-entry
                         (second (car (last (second prefix-result))))))
                     (revise-cycle
                       (first cycle-result)
                       (first cycle-result)
                       (second cycle-result)
                       gc bs-entry-array tableau block-array)
                     (setq result
                       (append prefix-result cycle-result)))))))))))
      (unless (null result)
        (clean-up-counterexample-path result undo-point))))))

;;; Code to free-up BDD indices after a counterexample path is found.

(defun clean-up-counterexample-path (result undo-point)
  (let ((first-result (bdd-entry (first result)))
        (second-result
          (mapcar #'(lambda (x) (list (first x)
                                      (bdd-entry (second x))))
                  (second result)))
        (third-result (bdd-entry (third result)))
        (fourth-result
          (mapcar #'(lambda (x) (list (first x)
                                      (bdd-entry (second x))))
                  (fourth result))))
    (undo-bdd-entries-back-to undo-point)
    (list (push-bdd-entry first-result)
          (mapcar #'(lambda (x) (list (first x)
                                      (push-bdd-entry (second x))))
                  second-result)
          (push-bdd-entry third-result)
          (mapcar #'(lambda (x) (list (first x)
                                      (push-bdd-entry (second x))))
                  fourth-result))))

;;; choose-prefix returns a pair: the symbolic state
;;; at the beginning of the prefix and the prefix "transitions".
;;; If used correctly, ought to be always successful.
;;; initial-states - the provisional starting symbolic state
;;; target-states - the provisional ending symbolic state
;;; targets - a list (of length at least 1) of partitioned symbolic states
;;;           (bs-entry arrays)
;;; gc - a BDD index for the global constraint

(defun choose-prefix
         (initial-states target-states targets gc bs-entry-array
          tableau block-array)
  (let* ((undo-point (next-bdd-entry-index))
         (result (choose-prefix-aux
                   nil initial-states initial-states target-states targets
                   gc bs-entry-array tableau block-array)))
    ;; (format t "~%choose-prefix-aux result: ~%~A~%" result)
    (when (null result)
      ;; **** This should not happen
      ;; (error message handled by choose-prefix-aux).
      (undo-bdd-entries-back-to undo-point))
    result))

;;; blk-path - prefix constructed so far (list of elementary block indices)
;;; states - symbolic state at the end of blk-path
;;; initial-states - provisional starting symbolic state
;;; target-states - the provisional ending symbolic state
;;; targets - a list (of length at least 1) of partitioned symbolic states
;;; gc - a BDD index for the global constraint

(defun choose-prefix-aux (blk-path states initial-states target-states
                          targets gc bs-entry-array tableau block-array)
  (cond
    ((null targets)
     ;; **** This should not happen.
     (format t "~%Problem with choose-prefix-aux (null targets).~%")
     nil)
    ((null (cdr targets)) 
     ;; Last target.  See if we have a prefix.
     (let* ((undo-point (next-bdd-entry-index))
            (start (push-bdd-entry (bdd-entry initial-states)))
            (prefix-path
              (apply-path-transfer-functions
                start target-states blk-path gc bs-entry-array
                tableau block-array)))
       (cond ((null prefix-path)
              ;; **** This should not happen.
              (format t "~%choose-prefix-aux failed with nil.~%")
              (undo-bdd-entries-back-to undo-point)
              nil)
             ((eq (bdd-entry start) *bdd-zero*)
              ;; **** This should not happen.
              (format t "~%choose-prefix-aux failed with false.~%")
              (undo-bdd-entries-back-to undo-point)
              nil)
             (t
              ;; Success!
              (list start prefix-path)))))
    (t
     (let ((next-states (push-bdd-entry *bdd-zero*))
           (next-target (car targets))
           (result nil))
       (loop
         for i from 0 to (- (length bs-entry-array) 1)
         while (null result)
         do
         (let ((bs-entry (aref next-target i)))
           ;; elementary block i may have multiple "cases"
           (loop
             for j from 0 to (- (length bs-entry) 1)
             do
             (loop
               for k from 0 to (- (length (nth j bs-entry)) 1)
               do
               (let ((current-states
                       (push-bdd-entry
                         (bdd-andop
                           (bdd-entry states)
                           (bdd-entry (nth k (nth j bs-entry)))))))
                 (set-bdd-entry
                   next-states
                   (bdd-orop
                     (forward-transfer-counterexamples
                       current-states i j k gc bs-entry-array tableau block-array)
                     (bdd-entry next-states)))
                 ;; Don't need current-states anymore.
                 (pop-bdd-entry))))
           ;; success as soon as elementary block i produces non-empty set
           (unless (eq (bdd-entry next-states) *bdd-zero*)
             ;; We've got a winner: block i.
             ;; Do a recursive call.
             (let ((undo-point (next-bdd-entry-index)))
               (setq result
                     (choose-prefix-aux
                        (append blk-path (list i))
                        next-states initial-states target-states
                        (cdr targets)
                        gc bs-entry-array tableau block-array))
               (when (null result)
                 (set-bdd-entry next-states *bdd-zero*)
                 (undo-bdd-entries-back-to undo-point))))))
       result))))

;;; Once a starting point for a class of cycles has been determined, we
;;; choose a specific cycle by searching forward (a depth first search
;;; of a specific depth - the length of the cycle).

(defun choose-cycle
     (starting-states ref-states targets gc bs-entry-array tableau block-array)
  (let* ((undo-point (next-bdd-entry-index))
         (combinations (combinations (tableau-fairness-constraints tableau)))
         (combination (nth (all-positive-combination-index combinations)
                           combinations))
         (result
           (choose-cycle-aux
             nil starting-states ref-states targets combination gc
             bs-entry-array tableau block-array)))
    ;; (format t "~%choose-cycle-aux result: ~%~A~%" result)
    (when (null result)
      (undo-bdd-entries-back-to undo-point))
    result))

(defun choose-cycle-aux
       (blk-path states ref-states targets target-combination
        gc bs-entry-array tableau block-array)
  (cond
    ((null targets)
     ;; **** This should not happen.
     (format t "~%Problem with choose-cycle-aux (null targets).~%")
     nil)
    ((null (cdr targets))
     ;; See if we have a cycle.
     (let* ((undo-point (next-bdd-entry-index))
            (start (push-bdd-entry (bdd-entry ref-states)))
            (done nil)
            (cycle-path nil))
       (loop
         while (not done)
         do
         (let ((undo-point-2 (next-bdd-entry-index)))
           (setq cycle-path
                 (apply-path-transfer-functions
                   start ref-states blk-path gc bs-entry-array
                   tableau block-array))
           (cond
             ((eq (bdd-entry start)
                  (bdd-entry (second (car (last cycle-path)))))
              ;; Success provided start not empty.
              (setq done t))
             ((eq (bdd-andop (bdd-entry start)
                             (bdd-entry (second (car (last cycle-path)))))
                  (bdd-entry (second (car (last cycle-path)))))
              ;; Not done.
              ;(format t "~%choose-cycle-aux not at fixpoint yet.~%")
              (set-bdd-entry
                start (bdd-entry (second (car (last cycle-path)))))
              ;; Undo back to but not including start.
              (undo-bdd-entries-back-to undo-point-2))
             (t
              ;; Failure.
              (setq cycle-path nil)
              (setq done t)))))
       (cond ((null cycle-path)
              ;; Failure.
              ;(format t "~%choose-cycle-aux failed with nil.~%")
              (undo-bdd-entries-back-to undo-point)
              nil)
             ((eq (bdd-entry start) *bdd-zero*)
              ;; Failure as well.
              ;(format t "~%choose-cycle-aux failed with false.~%")
              (undo-bdd-entries-back-to undo-point)
              nil)
             (t
              ;; Success!
              (list start cycle-path)))))
    (t
     (let ((next-states (push-bdd-entry *bdd-zero*))
           (next-targets (car targets))
           (result nil)
           (combinations (combinations
                           (tableau-fairness-constraints tableau))))
      (loop
        for x in (indices-of-source-combinations target-combination)
        ;; Currently stop after finding a cycle.
        while (null result)
        do
        (let ((constraint (push-bdd-entry
                            (bdd-andop
                              (bdd-and-on-list
                                (remaining-constraints
                                  (nth x combinations) target-combination))
                              (bdd-entry states))))
              (next-target (nth x next-targets)))
          (loop
            for i from 0 to (- (length bs-entry-array) 1)
            while (null result)
            do
            (let ((bs-entry (aref next-target i)))
              (loop
                for j from 0 to (- (length bs-entry) 1)
                do
                (loop
                  for k from 0 to (- (length (nth j bs-entry)) 1)
                  do
                  (let ((current-states
                          (push-bdd-entry
                            (bdd-andop
                              (bdd-entry constraint)
                              (bdd-entry (nth k (nth j bs-entry)))))))
                    (set-bdd-entry
                      next-states
                      (bdd-orop
                        (forward-transfer-counterexamples
                          current-states i j k gc bs-entry-array tableau
                          block-array)
                        (bdd-entry next-states)))
                    ;; Don't need current-states anymore.
                    (pop-bdd-entry))))
              (unless (eq (bdd-entry next-states) *bdd-zero*)
                ;; We've got a winner: block i.
                ;; Do a recursive call.
                (let ((undo-point (next-bdd-entry-index)))
                  (setq result
                        (choose-cycle-aux
                           (append blk-path (list i))
                           next-states ref-states
                           (cdr targets) (nth x combinations)
                           gc bs-entry-array tableau block-array))
                  (when (null result)
                    (set-bdd-entry next-states *bdd-zero*)
                    (undo-bdd-entries-back-to undo-point))))))))
      result))))


;;; Apply the forward transfer function for first block in blk-path
;;; to states, then apply the forward transfer function for the second
;;; block to the result of the first application, etc.
;;; After each recursive call, the value of states is constrained
;;; using the reverse transfer function of the appropriate block.
;;; The returned value is a list of pairs: each consisting of the
;;; block and the result of applying the block's forward transfer
;;; function constrained by the rest of the path.

;;; This is the narrowing operation on a tentative finite symbolic subpath

(defun apply-path-transfer-functions
        (states end-states blk-path gc bs-entry-array tableau block-array)
  (cond
   ((null blk-path)
    (set-bdd-entry
      states
       (bdd-andop
         (bdd-entry end-states)
         (bdd-entry states)))
     nil)
   (t
    (let* ((next-states
             (push-bdd-entry
               (apply-block-forward-transfer-counterexamples
                 (car blk-path) states gc bs-entry-array tableau block-array)))
           (rest
             (apply-path-transfer-functions
               next-states end-states (cdr blk-path)
               gc bs-entry-array tableau block-array)))
      (set-bdd-entry
        states
        (bdd-andop
          (apply-block-reverse-transfer-counterexamples
            (car blk-path) next-states gc bs-entry-array tableau block-array)
          (bdd-entry states)))
      (cons (list (car blk-path) next-states) rest)))))

;;; This corresponds to applying a "chosen transition"
;;; (the transition associated with block blk).

(defun apply-block-forward-transfer-counterexamples
        (blk states gc bs-entry-array tableau block-array)
  (let ((index (push-bdd-entry *bdd-zero*))
        (bs-entry (aref bs-entry-array blk)))
    (loop
      for j from 0 to (- (length bs-entry) 1)
      do
      (loop
        for k from 0 to (- (length (nth j bs-entry)) 1)
        do
        (set-bdd-entry
          index
          (bdd-orop
            (forward-transfer-counterexamples
              states blk j k gc bs-entry-array tableau block-array)
            (bdd-entry index)))))
    (let ((result (bdd-entry index)))
      (pop-bdd-entry)
      result)))

;;; This is used to constrain states that can progress to the
;;; final "goal".

(defun apply-block-reverse-transfer-counterexamples
        (blk states gc bs-entry-array tableau block-array)
  (let ((index (push-bdd-entry *bdd-zero*))
        (bs-entry (aref bs-entry-array blk)))
    (loop
      for j from 0 to (- (length bs-entry) 1)
      do
      (loop
        for k from 0 to (- (length (nth j bs-entry)) 1)
        do
        (set-bdd-entry
          index
          (bdd-orop
            (reverse-transfer-counterexamples
              states blk j k gc bs-entry-array tableau block-array)
            (bdd-entry index)))))
    (let ((result (bdd-entry index)))
      (pop-bdd-entry)
      result)))

(defun revise-cycle
    (states end-states cycle gc bs-entry-array tableau block-array)
  (cond
   ((null cycle)
    (set-bdd-entry
      states
       (bdd-andop
         (bdd-entry end-states)
         (bdd-entry states)))
     nil)
   (t
    (let ((blk (first (car cycle)))
          (next-states (second (car cycle))))
      (set-bdd-entry
        next-states
        (apply-block-forward-transfer-counterexamples
          blk states gc bs-entry-array tableau block-array))
      (revise-cycle next-states end-states (cdr cycle)
                    gc bs-entry-array tableau block-array)
      (set-bdd-entry
        states
        (bdd-andop
          (apply-block-reverse-transfer-counterexamples
            blk next-states gc bs-entry-array tableau block-array)
          (bdd-entry states)))))))



;;; =============== On-the-fly Symbolic LTL Checking =================


;;; This is a double-DFS algorithm for detecting a cycle
;;; in which all fairness constraints are fulfilled.

;;; Imprecise (but safe) tracking of fairness constraints is performed,
;;; using a technique that is fairly standard for converting
;;; a generalized Buchi automaton to a Buchi automaton.

;;; The first DFS searches for a path to an "accepting symbolic state".
;;; The second DFS searches for a cycle from an accepting symbolic state
;;; to itself (an accepting symbolic state is a symbolic state paired
;;; with n - the number of fairness constraints).  Because the next
;;; symbolic state after an accepting state is paired with 0 and because
;;; of the way the DFS search works, all fairness constraints are
;;; fulfilled in any cycle found.

(defun on-the-fly (gc model block-array)
  (let* ((undo-point (next-bdd-entry-index))
         (s (push-bdd-entry
              (bdd-andop (bdd-integrate-formula *initial-states-formula*)
                         (bdd-entry (augmented-model-spec model)))))
         (result (catch 'dfs (dfs1 s 0 nil (list s 0) gc model block-array))))
    (cond ((null result)
           (undo-bdd-entries-back-to undo-point)
           nil)
          (t
           (unless (null *verbose*)
             (format t "~%On-the-fly CTR length prefix: ~A, cycle: ~A.~%"
                     (length (second result)) (length (fourth result))))
           ;; result is a symbolic ctr-path
           (clean-up-counterexample-path result undo-point)))))

(defun on-the-fly-mc (gc mc model block-array)
  (let* ((undo-point (next-bdd-entry-index))
         (tableau (augmented-model-tableau model))
         (s (push-bdd-entry *bdd-zero*))
         (spec (mc-spec mc))
         (m (- (length (tableau-transition-constraints tableau)) 1)))
         ;; compute initial states that are in mc that satisfy spec
    (loop
      for i in *initial-block-indices*
      do
      (let ((mc-bs-entry (aref (mc-top mc) i)))
        (loop
          for j from 0 to (- (length mc-bs-entry) 1)
          do
          (loop
            for k from 0 to m
            do
            (set-bdd-entry
              s
              (bdd-orop (bdd-entry (nth k (nth j mc-bs-entry)))
                        (bdd-entry s)))))))
    (set-bdd-entry
      s
      (bdd-andop
        (bdd-andop (bdd-integrate-formula *initial-states-formula*)
                   (bdd-entry spec))
        (bdd-entry s)))
    (unless (eq (bdd-entry s) *bdd-zero*)
      (let ((result (catch 'dfs
                           (dfs1 s 0 nil (list s 0) gc model block-array))))
        (cond ((null result)
               (undo-bdd-entries-back-to undo-point)
                nil)
              (t
               (unless (null *verbose*)
                 (format t "~%On-the-fly CTR length prefix: ~A, cycle: ~A.~%"
                         (length (second result)) (length (fourth result))))
               ;; result is a symbolic ctr-path
               (clean-up-counterexample-path result undo-point)))))))

;;; Parameters for dfs1:
;;; s - symbolic state
;;; i - natural number indicating fairness constraints fulfilled:
;;;     0 means none fulfilled,
;;;     1 means 1st fairness constraint fulfilled,
;;;     2 means 1st and second fairness constraints fulfilled,
;;;     etc.
;;; p - list of triples (s,i,b) where s is a symbolic state,
;;;     i is a natural number and b is an elementary block.
;;;     Each triple (s,i,b) represents a transition through
;;;     block b to symbolic state s, with the first i
;;;     fairness constraints fulfilled.
;;; init-p - the initial symbolic state paired with 0
;;; gc - BDD index for a global constraint.

(defun dfs1 (s i p init-p gc model block-array)
  (let ((tableau (augmented-model-tableau model)))
    (loop
      for x from 0 to (- (length block-array) 1)
      do
      (let ((next-s (push-bdd-entry
                      (apply-block-forward-transfer-otf
                        x s gc model block-array))))
        (cond
          ((= i (length (tableau-fairness-constraints tableau)))
           (unless
             (or (eq (bdd-entry next-s) *bdd-zero*)
                 (visited-p next-s 0 p init-p))
             (dfs1 next-s 0 (cons (list next-s 0 x) p)
                   init-p gc model block-array))
           (pop-bdd-entry))
          (t
           (let ((ns (push-bdd-entry
                       (bdd-andop
                         (bdd-entry next-s)
                         (bdd-entry (nth i (tableau-fairness-constraints
                                             tableau)))))))
             (unless
               (or (eq (bdd-entry ns) *bdd-zero*)
                   (visited-p ns (+ i 1) p init-p))
               (dfs1 ns (+ i 1) (cons (list ns (+ i 1) x) p)
                     init-p gc model block-array))
             (set-bdd-entry
               ns
               (bdd-andop
                 (bdd-entry next-s)
                 (bdd-negate
                   (bdd-entry
                      (nth i (tableau-fairness-constraints tableau))))))
             (unless
               (or (eq (bdd-entry ns) *bdd-zero*)
                   (visited-p ns i p init-p))
               (dfs1 ns i (cons (list ns i x) p) init-p gc model block-array))
             (pop-bdd-entry)
             (pop-bdd-entry))))))
    (when (= i (length (tableau-fairness-constraints tableau)))
      (dfs2 s i p init-p nil (list s i) gc model block-array))))

;;; Check to see if (s i) has been visited in p or init-p.
;;; p is a list of triples while init-p is a pair consisting
;;; of a symbolic state and a natural number.
;;; A pair (s i) is considered to have been visited if
;;; there is a matching pair (ss i) in init-p or p
;;; where s represents a subset of ss.

(defun visited-p (s i p init-p)
  (let ((success nil))
    (when (and (= i (second init-p))
               (eq (bdd-andop (bdd-entry s)
                              (bdd-entry (first init-p)))
                   (bdd-entry s)))
      (setq success t))
    (loop for entry in p
          while (not success)
          do
          (let ((ss (first entry))
                (ii (second entry)))
            (when (and (= i ii)
                       (eq (bdd-andop (bdd-entry s)
                                      (bdd-entry ss))
                           (bdd-entry s)))
              (setq success t))))
    success))

;;; dfs2 searches for a fulfilling cycle.

(defun dfs2 (s i p init-p c init-c gc model block-array)
  (let ((tableau (augmented-model-tableau model)))
    (loop
      for x from 0 to (- (length block-array) 1)
      do
      (let ((next-s (push-bdd-entry
                      (apply-block-forward-transfer-otf
                        x s gc model block-array))))
        (cond
          ((= i (length (tableau-fairness-constraints tableau)))
           (when (and (not (eq (bdd-entry next-s) *bdd-zero*))
                      (visited-p next-s 0 p init-p))
             ;; (next-s 0) visited in DFS1 - fulfilling cycle found!
             ;; check-cycle will throw ('dfs symbolic-ctr-path)
             ;; if successful
             (check-cycle x next-s 0 p init-p c gc model block-array))
           (unless
             (or (eq (bdd-entry next-s) *bdd-zero*)
                 (visited-p next-s 0 c init-c))
             ;; non-trivial (next-s 0) not yet visited in DFS2
             (dfs2 next-s 0 p init-p (cons (list next-s 0 x) c)
                   init-c gc model block-array))
           (pop-bdd-entry))
          (t
           (let ((ns (push-bdd-entry
                       (bdd-andop
                         (bdd-entry next-s)
                         (bdd-entry (nth i (tableau-fairness-constraints
                                             tableau)))))))
             (when (and (not (eq (bdd-entry ns) *bdd-zero*))
                        (visited-p ns (+ i 1) p init-p))
               ;; (ns (+ i 1)) visited in DFS1 - fulfilling cycle found!
               ;; check-cycle will throw ('dfs symbolic-ctr-path)
               ;; if successful
               (check-cycle x ns (+ i 1) p init-p c gc model block-array))
             (unless
               (or (eq (bdd-entry ns) *bdd-zero*)
                   (visited-p ns (+ i 1) c init-c))
               ;; non-trivial (ns (+ i 1)) not yet visited in DFS2
               (dfs2 ns (+ i 1) p init-p (cons (list ns (+ i 1) x) c)
                     init-c gc model block-array))
             (set-bdd-entry
               ns
               (bdd-andop
                 (bdd-entry ns)
                 (bdd-negate
                   (bdd-entry (nth i (tableau-fairness-constraints tableau))))))
             (when (and (not (eq (bdd-entry ns) *bdd-zero*))
                        (visited-p ns i p init-p))
               ;; (ns i) visited in DFS1 - fulfilling cycle found!
               ;; check-cycle will throw ('dfs symbolic-ctr-path)
               ;; if successful
               (check-cycle x ns i p init-p c gc model block-array))
             (unless
               (or (eq (bdd-entry ns) *bdd-zero*)
                   (visited-p ns i c init-c))
               ;; non-trivial (ns i) not yet visited in DFS2
               (dfs2 ns i p init-p (cons (list ns i x) c)
                     init-c gc model block-array))
             (pop-bdd-entry)
             (pop-bdd-entry))))))
    nil))
  

(defun check-cycle (last-x s i p init-p c gc model block-array)
  (let ((next-s nil)
        (cycle nil)
        (prefix nil)
        (cycle-found nil)
        (mismatch nil)
        (undo-point (next-bdd-entry-index)))
    ;; ***** is c always nil here?
    (push (list last-x s) cycle)
    ;; First, process last transition.
    (setq next-s
          (push-bdd-entry
            (apply-block-reverse-transfer-otf last-x s gc model block-array)))
    ;; (push (list last-x s) cycle)
    ;; Next, consume the c "stack".
    (loop for triple in c
          do
          (let ((ss (first triple))
                (x (third triple)))
            (set-bdd-entry next-s (bdd-andop (bdd-entry next-s)
                                             (bdd-entry ss)))
            (push (list x next-s) cycle)
            (setq next-s
              (push-bdd-entry
                (apply-block-reverse-transfer-otf
                  x next-s gc model block-array)))))
    ;; Now search the p "stack".
    (loop
      for triple in p
      while (null mismatch)
      do
      (let ((ss (first triple))
            (ii (second triple))
            (x (third triple)))
        (set-bdd-entry next-s (bdd-andop (bdd-entry next-s)
                                         (bdd-entry ss)))
        (cond
          (cycle-found
            (push (list x next-s) prefix)
            (setq next-s
              (push-bdd-entry
                (apply-block-reverse-transfer-otf
                  x next-s gc model block-array))))
          (t
           (cond
             ((and (= i ii)
                   (eq (bdd-andop (bdd-entry s) (bdd-entry ss))
                       (bdd-entry s)))
              (setq cycle-found t)
              (or (eq (bdd-entry next-s) (bdd-entry s))
                  (narrow-cycle cycle next-s s gc model block-array)
                  (setq mismatch t))
              (push (list x next-s) prefix)
              (setq next-s
                (push-bdd-entry
                  (apply-block-reverse-transfer-otf
                    x next-s gc model block-array))))
             (t
              (push (list x next-s) cycle)
              (setq next-s
                (push-bdd-entry
                  (apply-block-reverse-transfer-otf
                    x next-s gc model block-array)))))))))
    (cond
      (mismatch (undo-bdd-entries-back-to undo-point))
      (cycle-found
       (set-bdd-entry next-s (bdd-andop (bdd-entry next-s)
                                        (bdd-entry (first init-p))))
       (throw 'dfs (list next-s prefix s cycle)))
      (t
       (set-bdd-entry next-s (bdd-andop (bdd-entry next-s)
                                        (bdd-entry (first init-p))))
       (throw 'dfs (list next-s prefix s cycle))))))


(defun narrow-cycle (cycle start end gc model block-array)
  (let ((done nil)
        (success nil))
    (loop
      while (null done)
      do
      (let ((state (second (car (last cycle))))
            (x (first (car (last cycle)))))
        (set-bdd-entry start (bdd-andop (bdd-entry start) (bdd-entry end)))
        (set-bdd-entry end (bdd-entry start))
        (set-bdd-entry (second (car (last cycle))) (bdd-entry end))
        (loop for pair in (cdr (reverse cycle))
              do
              (let ((ss (second pair)))
                (set-bdd-entry
                  ss (bdd-andop (apply-block-reverse-transfer-otf
                                  x state gc model block-array)
                                (bdd-entry ss)))
                (setq state ss)
                (setq x (first pair))))
        (set-bdd-entry
          start (bdd-andop (apply-block-reverse-transfer-otf
                             x state gc model block-array)
                           (bdd-entry start)))
        (cond ((eq (bdd-entry start) *bdd-zero*)
               (setq done t))
              ((eq (bdd-entry start) (bdd-entry end))
               (setq success t)
               (setq done t)))))
    success))

;;; This corresponds to applying a "chosen transition"
;;; (the transition associated with block blk).

(defun apply-block-forward-transfer-otf (blk states gc model block-array)
  (let ((tableau (augmented-model-tableau model))
        (index (push-bdd-entry *bdd-zero*))
        (s (push-bdd-entry (bdd-andop (bdd-entry states)
                                      (bdd-entry gc))))
        (bs-entry (bs-entry (aref (augmented-model-bs model) blk))))
    (loop
      for j from 0 to (- (length bs-entry) 1)
      do
      (loop
        for k from 0 to (- (length (nth j bs-entry)) 1)
        do
        (set-bdd-entry
          index
          (bdd-orop
            (forward-transfer-augmented s blk j k tableau block-array)
            (bdd-entry index)))))
    (let ((result (bdd-andop (bdd-entry index) (bdd-entry gc))))
      (pop-bdd-entry)
      (pop-bdd-entry)
      result)))

;;; This is used to constrain states that can progress to the
;;; final "goal".

(defun apply-block-reverse-transfer-otf (blk states gc model block-array)
  (let ((tableau (augmented-model-tableau model))
        (index (push-bdd-entry *bdd-zero*))
        (s (push-bdd-entry (bdd-andop (bdd-entry states)
                                      (bdd-entry gc))))
        (bs-exit (bs-exit (aref (augmented-model-bs model) blk))))
    (loop
      for j from 0 to (- (length bs-exit) 1)
      do
      (loop
        for k from 0 to (- (length (nth j bs-exit)) 1)
        do
        (set-bdd-entry
          index
          (bdd-orop
            (reverse-transfer-augmented s blk j k tableau block-array)
            (bdd-entry index)))))
    (let ((result (bdd-andop (bdd-entry index) (bdd-entry gc))))
      (pop-bdd-entry)
      (pop-bdd-entry)
      result)))



;;; ========== Combining on-the-fly with directed CE search =========

(defun combo-counterexample-search (gc model block-array)
  (let ((bs-entry-array
          (make-bs-entry-array-from-bs-array (augmented-model-bs model)))
        (tableau (augmented-model-tableau model))
        (spec (augmented-model-spec model))
        (undo-point (next-bdd-entry-index)))
    (let ((result (on-the-fly gc model block-array)))
      (cond
        ((null result)
         (undo-bdd-entries-back-to undo-point))
        (t
         (let ((rev (bdd-entry (third result)))
               (blk (last-transition result)))
           (undo-bdd-entries-back-to undo-point)
           (let ((rev-states (push-bdd-entry rev)))
             (setq result (counterexample-cycles
                            blk rev-states gc bs-entry-array
                            tableau block-array))
             (cond
               ((null result)
                (undo-bdd-entries-back-to undo-point))
               (t
                (let ((cycle-result
                        (choose-cycle
                          (first result) rev-states (second result)
                          gc bs-entry-array tableau block-array)))
                  (cond
                    ((null cycle-result)
                     (undo-bdd-entries-back-to undo-point)
                     (setq result nil))
                    (t
                     (let ((prefix-intermediate-result
                             (counterexample-prefix
                               blk (first cycle-result) gc
                               bs-entry-array tableau block-array spec)))
                       (cond
                         ((null prefix-intermediate-result)
                          (undo-bdd-entries-back-to undo-point)
                          (setq result nil))
                         (t
                          (let ((prefix-result
                                  (choose-prefix
                                    (first prefix-intermediate-result)
                                    (first cycle-result)
                                    (second prefix-intermediate-result)
                                    gc bs-entry-array tableau block-array)))
                            ;; choose-prefix is guaranteed to succeed
                            (set-bdd-entry
                              (first cycle-result)
                              (bdd-entry
                               (second (car (last (second prefix-result))))))
                            (revise-cycle
                              (first cycle-result)
                              (first cycle-result)
                              (second cycle-result)
                              gc bs-entry-array tableau block-array)
                            (setq result
                              (append prefix-result cycle-result)))))))))))
             (unless (null *verbose*)
               (format t "~%Directed CTR length, prefix: ~A, cycle: ~A.~%"
                       (length (second result)) (length (fourth result))))
             (unless (null result)
               (clean-up-counterexample-path result undo-point)))))))))

(defun combo2-counterexample-search (gc model block-array)
  (let ((bs-entry-array
          (make-bs-entry-array-from-bs-array (augmented-model-bs model)))
        (tableau (augmented-model-tableau model))
        (spec (augmented-model-spec model))
        (undo-point (next-bdd-entry-index)))
    (let ((result (on-the-fly gc model block-array)))
      (cond
        ((null result)
         (undo-bdd-entries-back-to undo-point))
        (t
         (let ((rev (bdd-entry (third result)))
               (cycle-blks (mapcar #'car (fourth result))))
           (undo-bdd-entries-back-to undo-point)
           (let* ((rev-states (push-bdd-entry rev))
                  (cycle-result (create-smaller-cycle
                                  rev-states cycle-blks
                                  gc bs-entry-array tableau block-array)))
             (cond
               ((null cycle-result)
                (undo-bdd-entries-back-to undo-point)
                (setq result nil))
               (t
                (let ((prefix-intermediate-result
                        (counterexample-prefix
                          (first (first (second cycle-result)))
                          (first cycle-result) gc
                          bs-entry-array tableau block-array spec)))
                  (cond
                    ((null prefix-intermediate-result)
                     (undo-bdd-entries-back-to undo-point)
                     (setq result nil))
                    (t
                     (let ((prefix-result
                             (choose-prefix
                               (first prefix-intermediate-result)
                               (first cycle-result)
                               (second prefix-intermediate-result)
                               gc bs-entry-array tableau block-array)))
                       ;; choose-prefix is guaranteed to succeed
                       (set-bdd-entry
                         (first cycle-result)
                         (bdd-entry
                          (second (car (last (second prefix-result))))))
                       (revise-cycle
                         (first cycle-result)
                         (first cycle-result)
                         (second cycle-result)
                         gc bs-entry-array tableau block-array)
                       (setq result
                         (append prefix-result cycle-result))))))))))))
      (unless (null *verbose*)
        (format t "~%Directed CTR length, prefix: ~A, cycle: ~A.~%"
                (length (second result)) (length (fourth result))))
      (unless (null result)
        (clean-up-counterexample-path result undo-point)))))

(defun combo-counterexample-search-mc (gc mc model block-array)
  (let ((bs-entry-array
          (make-bs-entry-array-from-bs-array (augmented-model-bs model)))
        (tableau (augmented-model-tableau model))
        (spec (augmented-model-spec model))
        (undo-point (next-bdd-entry-index)))
    (let ((result (on-the-fly-mc gc mc model block-array)))
      (cond
        ((null result)
         (undo-bdd-entries-back-to undo-point))
        (t
         (let ((rev (bdd-entry (third result)))
               (blk (last-transition result)))
           (undo-bdd-entries-back-to undo-point)
           (let ((rev-states (push-bdd-entry rev)))
             (setq result (counterexample-cycles
                            blk rev-states gc bs-entry-array
                            tableau block-array))
             (cond
               ((null result)
                (undo-bdd-entries-back-to undo-point))
               (t
                (let ((cycle-result
                        (choose-cycle
                          (first result) rev-states (second result)
                          gc bs-entry-array tableau block-array)))
                  (cond
                    ((null cycle-result)
                     (undo-bdd-entries-back-to undo-point)
                     (setq result nil))
                    (t
                     (let ((prefix-intermediate-result
                             (counterexample-prefix
                               blk (first cycle-result) gc
                               bs-entry-array tableau block-array spec)))
                       (cond
                         ((null prefix-intermediate-result)
                          (undo-bdd-entries-back-to undo-point)
                          (setq result nil))
                         (t
                          (let ((prefix-result
                                  (choose-prefix
                                    (first prefix-intermediate-result)
                                    (first cycle-result)
                                    (second prefix-intermediate-result)
                                    gc bs-entry-array tableau block-array)))
                            ;; choose-prefix is guaranteed to succeed
                            (set-bdd-entry
                              (first cycle-result)
                              (bdd-entry
                               (second (car (last (second prefix-result))))))
                            (revise-cycle
                              (first cycle-result)
                              (first cycle-result)
                              (second cycle-result)
                              gc bs-entry-array tableau block-array)
                            (setq result
                              (append prefix-result cycle-result)))))))))))))))
      (unless (null *verbose*)
        (format t "~%Directed CTR length, prefix: ~A, cycle: ~A.~%"
                (length (second result)) (length (fourth result))))
      (unless (null result)
        (clean-up-counterexample-path result undo-point)))))

(defun create-smaller-cycle (s cycle-blks gc bs-entry-array tableau block-array)
  (let ((current-s s)
        (done nil)
        (result nil))
    (loop
      for blk in (reverse cycle-blks)
      while (null done)
      do
      (let ((prev-s
              (push-bdd-entry
                (apply-block-reverse-transfer-counterexamples
                  blk current-s gc bs-entry-array tableau block-array))))
        (push (list blk prev-s) result)
        (when (eq (bdd-entry s)
                  (bdd-andop (bdd-entry prev-s)
                             (bdd-entry s)))
          (setq done t))
        (setq current-s prev-s)))
    (when (null done)
      (format t "Start:~%~S~%End:~%~S~%"
        (bdd-to-cnf (bdd-entry (second (first result))))
        (bdd-to-cnf (bdd-entry s))))
    (list s result)))


(defun second-last-state (ctr)
  (let ((path2 (first ctr))
        (path1 (second ctr)))
    (cond ((= (length path2) 1)
           (first (first path1)))
          (t (first (second path2))))))

(defun last-transition (ctr)
  (first (first (fourth ctr))))



;;; Note that gc must be binary encoded already.

(defun is-proved (mc)
  (and (not (null mc))
       (eq (bdd-andop
             (bdd-andop
               (bdd-or-on-list
                 (flatten-into-list
                   (loop
                     for i in *initial-block-indices*
                     collect (aref (mc-top mc) i))))
               (bdd-integrate-formula *initial-states-formula*))
             (bdd-entry (mc-spec mc)))
           *bdd-zero*)))



;;; ==================== Deadlock Detection =========================

(defun deadlocked-states (reachable block-array)
  (let ((rch (push-bdd-entry *bdd-zero*))
        (can-transition (push-bdd-entry *bdd-zero*)))
    ;; Collect reachable states
    (loop
      for i from 0 to (- (length reachable) 1)
      do (set-bdd-entry
           rch
           (bdd-orop
             (bdd-entry rch)
             (bdd-entry (reachable-pre-entry-states (aref reachable i))))))
    ;; Collect states that can transition
    (loop
      for i from 0 to (- (length block-array) 1)
      do
      (let ((trans (elementary-block-transitions (aref block-array i))))
        (loop
          for j from 0 to (- (length trans) 1)
          do
          (set-bdd-entry
            can-transition
            (bdd-orop
              (bdd-entry can-transition)
              (bdd-entry (transition-condition (nth j trans))))))))
    ;; Deadlocked states are reachable states that cannot transition
    (let ((deadlocked (bdd-andop (bdd-entry rch)
                                 (bdd-negate (bdd-entry can-transition)))))
      (pop-bdd-entry)
      (pop-bdd-entry)
      deadlocked)))

