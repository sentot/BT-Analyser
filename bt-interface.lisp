
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


;;; ***** At some point, we need to define our own read-eval loop.


;;; =================== The history ====================

;;; To support incremental analysis, the tool uses a history.
;;; Each increment is recorded as an event in *history*.
;;; Events can be undone directly or through change of settings.

(defvar *history* nil)

;;; Define structures for history events

;;; Structure for the model event.
;;; There is at most one model event in *history*.
;;; If *history* is not empty, it is the first event pushed.
;;; It is the block-array component of the structure that is used
;;; in analysis.

(defstruct
  (event-model
    (:constructor
      make-event-model (bt-filename token-list bt-root
                        block-root number-of-blocks bt-block-array
                        block-array state-vars pc-infos))
    :named :predicate)
   bt-filename token-list bt-root
   block-root number-of-blocks bt-block-array block-array
   state-vars pc-infos)

(defun print-event-model (event)
  (when (event-model-p event)
    (format t "Model, source file: '~A'.~%" (event-model-bt-filename event))))

(defun print-event-model-detailed (event)
  (when (event-model-p event)
    (print-event-model event)
    (format t "Elementary blocks:~%~%")
    (loop for b in (event-model-block-root event)
          do (print-block-tree b))))

;;; Structure for the reachable states event.
;;; There is at most one reachable states event in *history*.

(defstruct
  (event-reachable-states
    (:constructor
      make-event-reachable-states (states bdd-undo-point))
    :named :predicate)
   states bdd-undo-point)

(defun print-event-reachable-states (event)
  (when (event-reachable-states-p event)
    (format t "Reachable states.~%")))

(defun print-event-reachable-states-detailed (event)
  (when (event-reachable-states-p event)
    (print-event-reachable-states event)
    (unless (null *history*)
      (let ((bt-block-array (event-model-bt-block-array
                              (car (last *history*))))
            (reachable (event-reachable-states-states event)))
        (loop for i from 0 to (- (length bt-block-array) 1)
              do (print-reachable-states
                    reachable (aref bt-block-array i)))))))

;;; Structure for the LTL specification event.
;;; There is at most one reachability event in *history*.

(defstruct
  (event-ltl-specification
    (:constructor
      make-event-ltl-specification (formula model bdd-undo-point))
    :named :predicate)
   formula model bdd-undo-point)

(defun print-event-ltl-specification (event)
  (when (event-ltl-specification-p event)
    (format t "LTL specification:~%~S~%"
      (event-ltl-specification-formula event))))

(defun print-event-ltl-specification-detailed (event)
  (when (event-ltl-specification-p event)
    (print-event-ltl-specification event)
    (print-tableau
      (augmented-model-tableau (event-ltl-specification-model event)))))

;;; Structure for "fair states" event.
;;; gc - nil or state formula.
;;; gc-idx - index to BDD.
;;; reachable - t or nil.
;;; mc - mc struct (where the fair states are stored).
;;; bdd-undo-point - index to BDD,

(defstruct
  (event-fair-states
    (:constructor
      make-event-fair-states (gc gc-idx reachable mc bdd-undo-point))
    :named :predicate)
  gc gc-idx reachable mc bdd-undo-point)

(defun print-event-fair-states (event)
  (when (event-fair-states-p event)
    (let ((gc (if (or (null (event-fair-states-gc event))
                      (eq (bdd-entry (event-fair-states-gc-idx event))
                          *bdd-one*))
                  "and no global constraint"
                  (format nil "and with global constraint~%~S"
                          (event-fair-states-gc event))))
          (reachable (if (null (event-fair-states-reachable event))
                         "with no reachability constraint"
                         "with reachability constraint")))
      (format t "Fair states ~S ~S.~%" reachable gc))))

(defun print-event-fair-states-detailed (event)
  (when (event-fair-states-p event)
    (print-event-fair-states event)
    (unless (null *history*)
      (let ((bt-block-array (event-model-bt-block-array
                              (car (last *history*))))
            (mc (event-fair-states-mc event)))
        (loop for i from 0 to (- (length bt-block-array) 1)
              do (print-fair-states mc (aref bt-block-array i)))))))

;;; Structure for "counterexample states" event.
;;; gc - nil or state formula.
;;; gc-idx - index to BDD.
;;; mc - mc struct (where the fair states are stored).
;;; bs - array of bs structs.
;;; bdd-undo-point - index to BDD,

(defstruct
  (event-counterexample-states
    (:constructor
      make-event-counterexample-states (gc gc-idx mc bs bdd-undo-point))
    :named :predicate)
  gc gc-idx mc bs bdd-undo-point)

(defun print-event-counterexample-states (event)
  (when (event-counterexample-states-p event)
    (let ((gc (if (or (null (event-counterexample-states-gc event))
                      (eq (bdd-entry (event-counterexample-states-gc-idx event))
                          *bdd-one*))
                  "with no global constraint"
                  (format nil "with global constraint~%~S"
                              (event-counterexample-states-gc event)))))
      (format t "Counterexample states ~S.~%" gc))))

(defun print-event-counterexample-states-detailed (event)
  (when (event-counterexample-states-p event)
    (print-event-counterexample-states event)
    (unless (null *history*)
      (let ((bt-block-array (event-model-bt-block-array
                              (car (last *history*))))
            (bs-array (event-counterexample-states-bs event))
            (model (mc-model (event-counterexample-states-mc event))))
        (loop for i from 0 to (- (length bt-block-array) 1)
              do (print-counterexample-states
                   bs-array model (aref bt-block-array i)))))))

;;; Structure for counterexample path.
;;; gc - nil or state formula.
;;; gc-idx - index to BDD.
;;; cc - nil or state formula.
;;; cc-idx - index to BDD.
;;; path - a symbolic counterexample path.
;;; bdd-undo-point - index to BDD,

(defstruct
  (event-counterexample-path
    (:constructor
      make-event-counterexample-path (gc gc-idx cc cc-idx path bdd-undo-point))
    :named :predicate)
  gc gc-idx cc cc-idx path bdd-undo-point)

(defun print-event-counterexample-path (event)
  (when (event-counterexample-path-p event)
    (let ((gc (if (or (null (event-counterexample-path-gc event))
                      (eq (bdd-entry (event-counterexample-path-gc-idx event))
                          *bdd-one*))
                  "with no global constraint"
                  (format nil "with global constraint~%~S"
                              (event-counterexample-path-gc event))))
          (cc (if (or (null (event-counterexample-path-cc event))
                      (eq (bdd-entry (event-counterexample-path-cc-idx event))
                          *bdd-one*))
                  "and no cycle constraint"
                  (format nil "with cycle constraint~%~S"
                              (event-counterexample-path-cc event)))))
      (format t "Counterexample path ~S~%~S.~%" gc cc))))

(defun print-event-counterexample-path-detailed (event)
  (when (event-counterexample-path-p event)
    (print-event-counterexample-path event)
    (unless (null *history*)
      (let ((bt-block-array (event-model-bt-block-array
                              (car (last *history*))))
            (path (event-counterexample-path-path event)))
        (print-counterexample-path path bt-block-array)))))



;;; Command to undo the last event

(defun undo-event ()
  (unless (null *history*)
    (let ((event (car *history*)))
      (setq *history* (cdr *history*))
      (cond ((event-ltl-specification-p event)
             (undo-bdd-entries-back-to
               (event-ltl-specification-bdd-undo-point event)))
            ((event-reachable-states-p event)
             (undo-bdd-entries-back-to
               (event-reachable-states-bdd-undo-point event)))
            ((event-fair-states-p event)
             (undo-bdd-entries-back-to
               (event-fair-states-bdd-undo-point event)))
            ((event-counterexample-states-p event)
             (undo-bdd-entries-back-to
               (event-counterexample-states-bdd-undo-point event)))
            ((event-counterexample-path-p event)
             (undo-bdd-entries-back-to
               (event-counterexample-path-bdd-undo-point event))))
      t)))

;;; undo back to (and not including) event i

(defun undo-back-to-event (i)
  (when (and (integerp i) (>= i 1) (< i (length *history*)))
    (let* ((n (- (length *history*) i))
           (event (nth (- n 1) *history*)))
      (setq *history* (nthcdr n *history*))
      (cond ((event-ltl-specification-p event)
             (undo-bdd-entries-back-to
               (event-ltl-specification-bdd-undo-point event)))
            ((event-reachable-states-p event)
             (undo-bdd-entries-back-to
               (event-reachable-states-bdd-undo-point event)))
            ((event-fair-states-p event)
             (undo-bdd-entries-back-to
               (event-fair-states-bdd-undo-point event)))
            ((event-counterexample-states-p event)
             (undo-bdd-entries-back-to
               (event-counterexample-states-bdd-undo-point event)))
            ((event-counterexample-path-p event)
             (undo-bdd-entries-back-to
               (event-counterexample-path-bdd-undo-point event))))
      t)))

;;; undo back through (and including) event i

(defun undo-back-through-event (i)
  (when (and (integerp i) (>= i 1) (<= i (length *history*)))
    (let* ((n (- (length *history*) i))
           (event (nth n *history*)))
      (setq *history* (nthcdr (+ n 1) *history*))
      (cond ((event-ltl-specification-p event)
             (undo-bdd-entries-back-to
               (event-ltl-specification-bdd-undo-point event)))
            ((event-reachable-states-p event)
             (undo-bdd-entries-back-to
               (event-reachable-states-bdd-undo-point event)))
            ((event-fair-states-p event)
             (undo-bdd-entries-back-to
               (event-fair-states-bdd-undo-point event)))
            ((event-counterexample-states-p event)
             (undo-bdd-entries-back-to
               (event-counterexample-states-bdd-undo-point event)))
            ((event-counterexample-path-p event)
             (undo-bdd-entries-back-to
               (event-counterexample-path-bdd-undo-point event))))
      t)))

;;; Commands to print the history and the events

(defun print-history ()
  (format t "===== History ====~%~%")
  (loop for event in (reverse *history*)
        for i from 1
        do (progn (format t "Event ~S: " i)
                  (cond ((event-model-p event)
                         (print-event-model event))
                        ((event-ltl-specification-p event)
                         (print-event-ltl-specification event))
                        ((event-reachable-states-p event)
                         (print-event-reachable-states event))
                        ((event-fair-states-p event)
                         (print-event-fair-states event))
                        ((event-counterexample-states-p event)
                         (print-event-counterexample-states event))
                        ((event-counterexample-path-p event)
                         (print-event-counterexample-path event))))))

(defun print-event (i)
  (when (and (integerp i)
             (>= i 1)
             (<= i (length *history*)))
    (format t "Event ~S: " i)
    (let ((event (nth (- (length *history*) i) *history*)))
      (cond ((event-model-p event)
             (print-event-model-detailed event))
            ((event-ltl-specification-p event)
             (print-event-ltl-specification-detailed event))
            ((event-reachable-states-p event)
             (print-event-reachable-states-detailed event))
            ((event-fair-states-p event)
             (print-event-fair-states-detailed event))
            ((event-counterexample-states-p event)
             (print-event-counterexample-states-detailed event))
            ((event-counterexample-path-p event)
             (print-event-counterexample-path-detailed event))))))

(defun print-last-event ()
  (print-event (length *history*)))




;;; ================== Constructing a model ====================

;;; The user-level function calls are:
;;; - (process-bt-file "filename")
;;; - (set-priority &optional t-or-nil)
;;; - (set-goto-semantics &optional t-or-nil)


(defvar *bt-filename* nil)
(defvar *prioritizing* t)
(defvar *reorder* t)

;;; The main user-level function for constructing a model.
;;; It parses a TextBT file and creates an internal representation of
;;; the BT and the elementary blocks required for analysis, including
;;; the "transitions" (i.e., it creates the model M).

(defun process-bt-file (filename)
  (clear-database)
  (let* ((time1 (get-internal-run-time))
         (token-list (tokenise-file filename))
         (bt-root (parse-top-level token-list)))
    (unless (null bt-root)
      (process-bt-initial-phases bt-root)
      (unless *fatal-error*
        (let ((result (process-bt-final-phases bt-root)))
          (unless *fatal-error*
            (let ((block-root (first result))
                  (bt-block-array (second result))
                  (block-array (third result))
                  (state-vars (fourth result))
                  (pc-infos (fifth result))
                  (time (time-in-seconds (- (get-internal-run-time) time1))))
              (push (make-event-model
                      filename token-list bt-root block-root
                      (length bt-block-array) bt-block-array
                      block-array state-vars pc-infos)
                    *history*)
              (format t "Loading of BT file ~S took ~3$ seconds.~%"
                        filename time)
              t)))))))

(defun process-bt (input-string)
  (clear-database)
  (let* ((time1 (get-internal-run-time))
         (token-list (tokenise-input-string input-string))
         (bt-root (parse-top-level token-list)))
    (unless (null bt-root)
      (process-bt-initial-phases bt-root)
      (unless *fatal-error*
        (let ((result (process-bt-final-phases bt-root)))
          (unless *fatal-error*
            (let ((block-root (first result))
                  (bt-block-array (second result))
                  (block-array (third result))
                  (state-vars (fourth result))
                  (pc-infos (fifth result))
                  (time (time-in-seconds (- (get-internal-run-time) time1))))
              (push (make-event-model
                      "STREAM" token-list bt-root block-root
                      (length bt-block-array) bt-block-array
                      block-array state-vars pc-infos)
                    *history*)
              (format t "Loading of BT file took ~3$ seconds.~%" time)
              t)))))))

(defun clear-database ()
  (setq *history* nil)
  (setq *initial-block-indices* nil))

(defun process-bt-initial-phases (bt-root)
  (unless (null bt-root)
    (expand-references bt-root)
    (expand-quantifications bt-root)
    (assign-pc bt-root 1 1 2 2)
    (set-targets bt-root)
    (set-io-partners bt-root)
    (set-sync-targets bt-root)))

(defun process-bt-final-phases (bt-root)
  (unless (null bt-root)
    (clear-bt-node-blocks bt-root)
    (let ((result (process-conditions-and-effects bt-root)))
      (let ((block-root (first result))
            (bt-block-array (second result))
            (state-vars (collect-state-vars bt-root))
            (pc-infos (collect-pc-info (second result))))
        (initialize-bdd-for-model pc-infos state-vars block-root)
        (when *prioritizing*
          (modify-external-events block-root))
        (let ((block-array (create-elementary-blocks
                             pc-infos state-vars bt-block-array block-root)))
          (list block-root bt-block-array block-array
                state-vars pc-infos))))))

(defun process-bt-final-phases-with-order (bt-root order)
  (unless (null bt-root)
    (clear-bt-node-blocks bt-root)
    (let ((result (process-conditions-and-effects bt-root)))
      (let ((block-root (first result))
            (bt-block-array (second result))
            (state-vars (collect-state-vars bt-root))
            (pc-infos (collect-pc-info (second result))))
        (override-order order)
        (when *prioritizing*
          (modify-external-events block-root))
        (let ((block-array (create-elementary-blocks
                             pc-infos state-vars bt-block-array block-root)))
          (list block-root bt-block-array block-array
                state-vars pc-infos))))))

(defun clear-bt-node-blocks (node)
  (setf (bt-node-blocks node) nil)
  (loop for n in (bt-node-children node)
        do (clear-bt-node-blocks n)))

(defun initialize-bdd-for-model (pc-infos state-vars block-root)
  (let ((initial-order (generate-atoms pc-infos state-vars))
        (dwa-order (construct-initial-order
                     (make-or
                       (convert-formula
                         pc-infos state-vars
                         (collect-prioritized-conditions block-root))
                       (collect-effects pc-infos state-vars block-root)))))
    (let ((order (append dwa-order
                         (loop for entry in initial-order
                               when (not (member-equal entry dwa-order))
                               collect entry))))
      (setup-bdd order))))

(defun override-order (order)
  (let* ((old-order (reverse *atom-list*))
         (new-order (append order
                            (loop for entry in old-order
                                  when (not (member-equal entry order))
                            collect entry))))
    (setup-bdd new-order)))



;;; Another user-level function.
;;; Set priority to nil if we don't want prioritization,
;;; otherwise set it to t.
;;; Note that if there is a change, we need to regenerate the
;;; "transitions", causing all analysis results to be thrown away.
;;; Prioritization is the default when the tool starts.

(defun set-priority (&optional value)
  (let ((time1 (get-internal-run-time))
        (new-value (if value t nil)))
    (unless (eq *prioritizing* new-value)
      (setq *prioritizing* new-value)
      (setq *ltl-uses-reachability-flag* new-value)
      (setq *ltl-uses-batch-flag* (not new-value))
      (setq *reachability-use-batch-flag* new-value)
      (setq *counterexample-use-batch-flag* new-value)
      (unless (null *history*)
        (let ((bt-root (event-model-bt-root (car (last *history*))))
              (filename (event-model-bt-filename (car (last *history*))))
              (token-list (event-model-token-list (car (last *history*)))))
          (let ((result (process-bt-final-phases bt-root)))
            (setq *history* nil)
            (unless *fatal-error*
              (let ((block-root (first result))
                    (bt-block-array (second result))
                    (block-array (third result))
                    (state-vars (fourth result))
                    (pc-infos (fifth result))
                    (time (time-in-seconds (- (get-internal-run-time) time1))))
                (push (make-event-model
                        filename token-list bt-root block-root
                        (length bt-block-array) bt-block-array
                        block-array state-vars pc-infos)
                      *history*)
                (format t "Modifying BT model took ~3$ seconds.~%" time)))))))
    new-value))

;;; Another user-level function.
;;; Set to nil if we want  a copy semantics for all references.
;;; Set to t if we want a goto semantics for references to same thread.
;;; Note that if there is a change, we need to regenerate the model,
;;; causing all analysis results to be thrown away.
;;; Using goto semantics for references to the same thread is the
;;; default when the tool starts.

(defun set-goto-semantics (&optional value)
  (let ((time1 (get-internal-run-time))
        (new-value (if value t nil)))
    (unless (eq *goto-semantics* new-value)
      (setq *goto-semantics* new-value)
      (unless (null *history*)
        (let ((filename (event-model-bt-filename (car (last *history*))))
              (token-list (event-model-token-list (car (last *history*)))))
          (clear-database)
          (let ((bt-root (parse-top-level token-list)))
            (unless (null bt-root)
              (process-bt-initial-phases bt-root)
              (unless *fatal-error*
                (let ((result (process-bt-final-phases bt-root)))
                  (unless *fatal-error*
                    (let ((block-root (first result))
                          (bt-block-array (second result))
                          (block-array (third result))
                          (state-vars (fourth result))
                          (pc-infos (fifth result))
                          (time (time-in-seconds
                                  (- (get-internal-run-time) time1))))
                      (push (make-event-model
                              filename token-list bt-root block-root
                              (length bt-block-array) bt-block-array
                              block-array state-vars pc-infos)
                            *history*)
                      (format t "Modifying BT model took ~3$ seconds.~%"
                                time))))))))))
    new-value))


(defun set-working-directory (pathname-string)
  (setf cl:*default-pathname-defaults* (cl:truename pathname-string)))




;;; ================ Computing general reachability ==================

;;; The user-level function call is - (reachable-states)
;;; General reachability can be computed only if there is a model,
;;; indicated by a non-empty *history*.
;;; General reachability computation can occur at most once in *history*.

(defun reachable-states ()
  (unless (null *history*)
    (let ((found (find-reachable-states)))
      (when (null found)
        (let* ((time1 (get-internal-run-time))
               (bdd-undo-point (next-bdd-entry-index))
               (block-array (event-model-block-array (car (last *history*))))
               (reachable (compute-reachable-states block-array))
               (time (time-in-seconds (- (get-internal-run-time) time1))))
          (push (make-event-reachable-states reachable bdd-undo-point)
                *history*)
          (format t "Computing reachable states took ~3$ seconds.~%" time)
          t)))))

(defun find-reachable-states ()
  (loop for event in *history*
        when (event-reachable-states-p event)
        return event))

(defun show-reachable-states (&optional formula)
  (let ((event (find-reachable-states)))
    (when event
      (cond ((null formula)
             (format t "Reachable states:~%"))
            (t (format t "Reachable states satisfying ~%~S:~%" formula)))
      (let* ((bt-block-array (event-model-bt-block-array
                              (car (last *history*))))
             (pc-infos (event-model-pc-infos (car (last *history*))))
             (state-vars (event-model-state-vars (car (last *history*))))
             (formula-entry
                (push-bdd-entry
                   (if (null formula)
                       *bdd-one*
                       (bdd-integrate-formula
                         (convert-formula pc-infos state-vars formula)))))
             (result-entry (push-bdd-entry *bdd-zero*))
             (reachable (event-reachable-states-states event)))
        (loop for i from 0 to (- (length bt-block-array) 1)
              do
              (let ((rs (aref reachable i)))
                (set-bdd-entry
                  result-entry
                  (bdd-andop
                    (bdd-entry formula-entry)
                    (bdd-entry (reachable-pre-entry-states rs))))
                (unless (eq (bdd-entry result-entry) *bdd-zero*)
                  (format t "Pre-entry states:~%~S~%"
                          (print-unencoded (bdd-entry result-entry))))))
       (pop-bdd-entry)
       (pop-bdd-entry)
       t))))

;;; ================ Enter LTL specification ==================

;;; The user-level function calls are:
;;; - (ltl-specification formula)
;;; - (set-encoding-scheme &optional scheme)

;;; An LTL specification can be entered only if there is a model,
;;; indicated by a non-empty *history*.
;;; An LTL specification can occur at most once in *history*.

(defun ltl-specification (formula &optional reorder)
  (cond ((null *history*)
         (format *error-output* "~%There is no model.~%")
         nil)
        ((ltl-specification-already-exists formula)
         (format *error-output* "~%LTL specification already exists.~%")
         nil)
        ((wfcheck-formula formula)
         (let ((event (find-ltl-specification))
               (pc-infos (event-model-pc-infos (car (last *history*))))
               (state-vars (event-model-state-vars (car (last *history*))))
               (block-array (event-model-block-array (car (last *history*)))))
           (unless (null event)
             ;; need to undo
             (undo-bdd-entries-back-to
               (event-ltl-specification-bdd-undo-point event))
             (setq *history* (cdr (member-eq event *history*))))
           ;; *** new ***
           (when (or reorder *reorder*)
           (let ((order (bdd-order-from-ltl-spec pc-infos state-vars formula))
                 (bt-root (event-model-bt-root (car (last *history*))))
                 (filename (event-model-bt-filename (car (last *history*))))
                 (token-list (event-model-token-list (car (last *history*)))))
             (let ((result (process-bt-final-phases-with-order bt-root order)))
               (setq *history* nil)
               (unless *fatal-error*
                 (setq pc-infos (fifth result))
                 (setq state-vars (fourth result))
                 (setq block-array (third result))
                 (let ((block-root (first result))
                       (bt-block-array (second result)))
                  (push (make-event-model
                          filename token-list bt-root block-root
                          (length bt-block-array) bt-block-array
                          block-array state-vars pc-infos)
                        *history*)))))
           )
           ;; *** end of new ***
           (when *ltl-uses-reachability-flag* (reachable-states))
           (let* ((time1 (get-internal-run-time))
                  (bdd-undo-point (next-bdd-entry-index))
                  (reachable (and *ltl-uses-reachability-flag*
                                  (find-reachable-states)
                                  (event-reachable-states-states
                                    (find-reachable-states))))
                  (normalized-formula
                    (normalize-ltl-formula (make-not formula)))
                  (converted-formula
                    (convert-formula pc-infos state-vars normalized-formula))
                  (model (construct-augmented-model
                           reachable block-array converted-formula))
                  (time2 (get-internal-run-time)))
             (push (make-event-ltl-specification formula model bdd-undo-point)
                   *history*)
             (format t "Constructing augmented model took ~3$ seconds.~%"
                       (time-in-seconds (- time2 time1)))
             t)))
        (t
         (format *error-output* "~%Formula not well-formed.~%")
         nil)))

(defun ltl-specification-already-exists (formula)
  (let ((event (find-ltl-specification)))
    (and event (equal (event-ltl-specification-formula event) formula))))

(defun find-ltl-specification ()
  (loop for event in *history*
        when (event-ltl-specification-p event)
        return event))

(defvar *atom-ratio-threshold* 0.1)

(defun bdd-order-from-ltl-spec (pc-infos state-vars formula)
  (let* ((normalized-formula (normalize-ltl-formula (make-not formula)))
         (converted-formula
            (convert-formula pc-infos state-vars normalized-formula))
         (v-formulas (remove-duplicates
                       (produce-v-formulas converted-formula)))
         ;; promise variables
         (promise (and (or (eq *encoding-scheme* 'tgba)
                           (eq *encoding-scheme* 'tgba-loose)
                           (eq *encoding-scheme* 'rtgba)
                           (eq *encoding-scheme* 'rtgba-loose))
                       (produce-promise-variables converted-formula)))
         ;; "next" terms
         (next (and (or (eq *encoding-scheme* 'tgba)
                        (eq *encoding-scheme* 'tgba-loose)
                        (eq *encoding-scheme* 'rtgba)
                        (eq *encoding-scheme* 'rtgba-loose))
                    (produce-next-terms converted-formula))))
    (append v-formulas promise next
            (let ((atoms (collect-atoms pc-infos state-vars formula)))
              (and (< (/ (length atoms) (length *atom-list*))
                      *atom-ratio-threshold*)
                   (loop for atom in (reverse *atom-list*)
                         when (member-equal atom atoms)
                         collect atom)))
            )))

(defun atom-ratio ()
  (let ((event (find-ltl-specification)))
    (cond ((null event) 0)
          (t (let* ((formula (event-ltl-specification-formula event))
                    (pc-infos (event-model-pc-infos (car (last *history*))))
                    (state-vars (event-model-state-vars (car (last *history*))))
                    (normalized-formula
                      (normalize-ltl-formula (make-not formula)))
                    (converted-formula
                      (convert-formula pc-infos state-vars normalized-formula))
                    (v-formulas
                      (remove-duplicates
                        (produce-v-formulas converted-formula)))
                    (promise (and (or (eq *encoding-scheme* 'tgba)
                                      (eq *encoding-scheme* 'tgba-loose)
                                      (eq *encoding-scheme* 'rtgba)
                                      (eq *encoding-scheme* 'rtgba-loose))
                                  (produce-promise-variables
                                     converted-formula)))
                    (next (and (or (eq *encoding-scheme* 'tgba)
                                   (eq *encoding-scheme* 'tgba-loose)
                                   (eq *encoding-scheme* 'rtgba)
                                   (eq *encoding-scheme* 'rtgba-loose))
                               (produce-next-terms converted-formula))))
               (* 1.0 (/ (length (collect-atoms pc-infos state-vars formula))
                         (- (length *atom-list*)
                            (length (append v-formulas promise next))))))))))


;;; If scheme is not specified, set to default.

(defun set-encoding-scheme (&optional scheme)
  (let ((new-scheme (or (and (member-eq scheme *encoding-schemes*)
                             scheme)
                        *default-encoding-scheme*)))
    (unless (eq *encoding-scheme* new-scheme)
      (setq *encoding-scheme* new-scheme)
      (let ((event (find-ltl-specification)))
        (unless (null event)
          ;; need to undo
          (undo-bdd-entries-back-to
            (event-ltl-specification-bdd-undo-point event))
          (setq *history* (cdr (member-eq event *history*)))
          (when *ltl-uses-reachability-flag* (reachable-states))
          (let* ((time1 (get-internal-run-time))
                 (bdd-undo-point (next-bdd-entry-index))
                 (pc-infos (event-model-pc-infos (car (last *history*))))
                 (state-vars (event-model-state-vars (car (last *history*))))
                 (reachable (and *ltl-uses-reachability-flag*
                                 (find-reachable-states)
                                 (event-reachable-states-states
                                   (find-reachable-states))))
                 (block-array (event-model-block-array (car (last *history*))))
                 (formula (event-ltl-specification-formula event))
                 (normalized-formula
                   (normalize-ltl-formula
                     (make-not (convert-formula pc-infos state-vars formula))))
                 (model (construct-augmented-model
                          reachable block-array normalized-formula))
                 (time2 (get-internal-run-time)))
            (push (make-event-ltl-specification
                    formula model bdd-undo-point)
                  *history*)
            (format t "Reconstructing augmented model took ~3$ seconds.~%"
                      (time-in-seconds (- time2 time1)))))))
    new-scheme))



;;; ================ Compute fair states ==================

;;; The user-level function call is - (fair-states &optional gc).

(defun fair-states (&optional gc)
  (let ((event (find-ltl-specification)))
    (when (and event (or (null gc) (wfcheck-proposition gc)))
      (let ((block-array (event-model-block-array (car (last *history*))))
            (pc-infos (event-model-pc-infos (car (last *history*))))
            (state-vars (event-model-state-vars (car (last *history*)))))
        (let ((gc-bdd (if (null gc)
                          *bdd-one*
                          (bdd-integrate-formula
                            (convert-formula pc-infos state-vars gc))))
              (model (event-ltl-specification-model event)))
          (let ((found (find-fair-states gc-bdd)))
            (when (null found)
              (when *ltl-uses-reachability-flag* (reachable-states))
              (let* ((time1 (get-internal-run-time))
                     (reachable (and *ltl-uses-reachability-flag*
                                     (find-reachable-states)
                                     (event-reachable-states-states
                                       (find-reachable-states))))
                     (bdd-undo-point (next-bdd-entry-index))
                     (gc-index (push-bdd-entry gc-bdd))
                     (mc (compute-mc reachable block-array model gc-index))
                     (time2 (get-internal-run-time)))
                (push (make-event-fair-states
                        gc gc-index *ltl-uses-reachability-flag*
                        mc bdd-undo-point)
                      *history*)
                (format t "Computing fair states took ~3$ seconds.~%"
                          (time-in-seconds (- time2 time1)))
                t))))))))

(defun find-fair-states (gc-bdd)
  (loop for event in *history*
        when (and (event-fair-states-p event)
                  (eq gc-bdd (bdd-entry (event-fair-states-gc-idx event))))
        return event))

(defun find-fair-states-loose (gc-bdd)
  (let* ((gc (push-bdd-entry gc-bdd))
         (result
           (loop
             for event in *history*
             when
             (and (event-fair-states-p event)
                  (eq (bdd-entry gc)
                      (bdd-andop
                        (bdd-entry (event-fair-states-gc-idx event))
                        (bdd-entry gc))))
             return event)))
    (pop-bdd-entry)
    result))



;;; ================ Compute counterexample states ==================

;;; The user-level function call is - (counterexample-states &optional gc).

(defun counterexample-states (&optional gc)
  (when (and (find-ltl-specification) (or (null gc) (wfcheck-proposition gc)))
    (fair-states gc)
    (let* ((block-array (event-model-block-array (car (last *history*))))
           (pc-infos (event-model-pc-infos (car (last *history*))))
           (state-vars (event-model-state-vars (car (last *history*))))
           (reachable (and *ltl-uses-reachability-flag*
                                     (find-reachable-states)
                                     (event-reachable-states-states
                                       (find-reachable-states))))
           (gc-bdd (if (null gc)
                       *bdd-one*
                       (bdd-integrate-formula
                         (convert-formula pc-infos state-vars gc))))
           (found (find-counterexample-states gc-bdd)))
      (when (null found)
        (let* ((time1 (get-internal-run-time))
               (event (find-fair-states gc-bdd))
               (bdd-undo-point (next-bdd-entry-index))
               (gc-index (push-bdd-entry gc-bdd))
               (mc (event-fair-states-mc event))
               (bs (compute-counterexample-states
                     reachable block-array mc gc-index))
               (time2 (get-internal-run-time)))
          (push (make-event-counterexample-states
                  gc gc-index mc bs bdd-undo-point)
                *history*)
          (format t "Computing counterexample states took ~3$ seconds.~%"
                    (time-in-seconds (- time2 time1)))
          t)))))

(defun find-counterexample-states (gc-bdd)
  (loop for event in *history*
        when (and (event-counterexample-states-p event)
                  (eq gc-bdd
                      (bdd-entry (event-counterexample-states-gc-idx event))))
        return event))

(defun find-counterexample-states-loose (gc-bdd)
  (let* ((gc (push-bdd-entry gc-bdd))
         (result
           (loop
             for event in *history*
             when
             (and (event-counterexample-states-p event)
                  (eq (bdd-entry gc)
                      (bdd-andop
                        (bdd-entry (event-counterexample-states-gc-idx event))
                        (bdd-entry gc))))
             return event)))
    (pop-bdd-entry)
    result))




;;; ================== Counterexample path generation ====================


;;; The user-level function call is -
;;; (find-counterexample-path &optional label cc gc)


(defun counterexample-path-matching (label)
  (find-counterexample-path label nil nil))

(defun find-counterexample-path (&optional label cc gc)
  (let ((block-indices
         (or (and (not (null label)) (indices-of-blocks-matching label))
             (indices-of-reversion-blocks)))
        (bdd-undo-point (next-bdd-entry-index))
        (spec-event (find-ltl-specification)))
    (when (and spec-event
               block-indices
               (or (null cc) (wfcheck-proposition cc))
               (or (null gc) (wfcheck-proposition gc)))
      (let ((time1 (get-internal-run-time))
            (block-array (event-model-block-array (car (last *history*))))
            (pc-infos (event-model-pc-infos (car (last *history*))))
            (state-vars (event-model-state-vars (car (last *history*)))))
        (let ((cc-idx (push-bdd-entry
                        (if (null cc)
                            *bdd-one*
                            (bdd-integrate-formula
                              (convert-formula pc-infos state-vars cc)))))
              (gc-bdd (if (null gc)
                          *bdd-one*
                          (bdd-integrate-formula
                            (convert-formula pc-infos state-vars gc)))))
          (let ((gc-idx (push-bdd-entry gc-bdd))
                (ctr-event (find-counterexample-states-loose gc-bdd))
                (mc-event (find-fair-states-loose gc-bdd)))
            (let ((ctr-bs (and ctr-event
                               (event-counterexample-states-bs ctr-event)))
                  (mc (and mc-event (event-fair-states-mc mc-event)))
                  (model (event-ltl-specification-model spec-event)))
              (let ((result (counterexample-path
                              (first block-indices) cc-idx gc-idx
                              ctr-bs mc model block-array)))
                (format
                  t "Searching for a counterexample path took ~3$ seconds.~%"
                  (time-in-seconds (- (get-internal-run-time) time1)))
                (cond ((null result)
                       (undo-bdd-entries-back-to bdd-undo-point)
                       nil)
                      (t
                       (push (make-event-counterexample-path
                               gc gc-idx cc cc-idx result bdd-undo-point)
                             *history*)
                       t))))))))))



;;; ==================== High-level commands ======================

;;; The user-level functions are -
;;;  (ltl-check formula &optional reorder)
;;;  (theorem-is-proved &optional gc)
;;;  (print-deadlocked-states)


;;; Function to perform LTL model checking

(defun ltl-check (formula &optional reorder)
  (ltl-specification formula reorder)
  (fair-states))

;;; Verify that spec is proved

(defun theorem-is-proved (&optional gc)
  (when (or (null gc) (wfcheck-proposition gc))
    (fair-states gc)
    (let* ((pc-infos (event-model-pc-infos (car (last *history*))))
           (state-vars (event-model-state-vars (car (last *history*))))
           (gc-bdd (if (null gc)
                       *bdd-one*
                       (bdd-integrate-formula
                          (convert-formula pc-infos state-vars gc))))
           (mc (event-fair-states-mc (find-fair-states gc-bdd))))
      (unless (null mc)
      (let ((result (is-proved mc)))
        (cond ((null result)
               (cond ((null gc)
                      (format t "Specification is not proved.~%"))
                     (t
                      (format t "Specification with global constraint~%~S"
                              gc)
                      (format t "~%is not proved.~%"))))
              (t
               (cond ((null gc)
                      (format t "Specification is proved.~%"))
                     (t
                      (format t "Specification with global constraint~%~S"
                              gc)
                      (format t "~%is proved.~%")))))
        result)))))

;;; Find deadlocked states.

(defun print-deadlocked-states ()
  (unless (null *history*)
    (reachable-states)
    (let ((reachable (event-reachable-states-states (find-reachable-states)))
          (block-array (event-model-block-array (car (last *history*)))))
      (format t "~%Deadlocked states:~%~S~%"
              (print-unencoded (deadlocked-states reachable block-array))))))




;;; ==================== on-the-fly (DFS) commands ======================


;;; The user-level functions are -
;;;  (on-the-fly-ltl-check formula &optional gc)
;;;  (on-the-fly-ltl-check-mc formula &optional gc)
;;;  (combo-ltl-check formula &optional gc)
;;;  (combo2-ltl-check formula &optional gc)
;;;  (combo-ltl-check-mc formula &optional gc)

;;; gc is an optional global constraint state formula

(defun on-the-fly-ltl-check (formula &optional gc)
  (ltl-specification formula)
  (let ((time1 (get-internal-run-time))
        (event (find-ltl-specification))
        (bdd-undo-point (next-bdd-entry-index)))
    (when (and event (or (null gc) (wfcheck-proposition gc)))
      (let ((block-array (event-model-block-array (car (last *history*))))
            (pc-infos (event-model-pc-infos (car (last *history*))))
            (state-vars (event-model-state-vars (car (last *history*)))))
        (let ((gc-idx (push-bdd-entry
                      (if (null gc)
                          *bdd-one*
                          (bdd-integrate-formula
                            (convert-formula pc-infos state-vars gc)))))
              (cc-idx (push-bdd-entry *bdd-one*))
              (model (event-ltl-specification-model event)))
          (let ((result (on-the-fly gc-idx model block-array)))
            (format t "On-the-fly checking took ~3$ seconds.~%"
                    (time-in-seconds (- (get-internal-run-time) time1)))
            (cond ((null result)
                   (undo-bdd-entries-back-to bdd-undo-point)
                   nil)
                  (t
                   (push (make-event-counterexample-path
                           gc gc-idx nil cc-idx result bdd-undo-point)
                         *history*)
                   t))))))))

(defun on-the-fly-ltl-check-mc (formula &optional gc)
  (ltl-specification formula)
  (let ((event (find-ltl-specification)))
    (when (and event (or (null gc) (wfcheck-proposition gc)))
      (fair-states gc)
      (let ((time1 (get-internal-run-time))
            (block-array (event-model-block-array (car (last *history*))))
            (pc-infos (event-model-pc-infos (car (last *history*))))
            (state-vars (event-model-state-vars (car (last *history*)))))
        (let ((gc-bdd (if (null gc)
                          *bdd-one*
                          (bdd-integrate-formula
                            (convert-formula pc-infos state-vars gc))))
              (bdd-undo-point (next-bdd-entry-index)))
          (let ((mc (event-fair-states-mc (find-fair-states gc-bdd)))
                (model (event-ltl-specification-model event))
                (gc-idx (push-bdd-entry gc-bdd))
                (cc-idx (push-bdd-entry *bdd-one*)))
            (let ((result (on-the-fly-mc gc-idx mc model block-array)))
              (format t "On-the-fly checking took ~3$ seconds.~%"
                      (time-in-seconds (- (get-internal-run-time) time1)))
              (cond ((null result)
                     (undo-bdd-entries-back-to bdd-undo-point)
                     nil)
                    (t
                     (push (make-event-counterexample-path
                             gc gc-idx nil cc-idx result bdd-undo-point)
                           *history*)
                     t)))))))))


(defun combo-ltl-check (formula &optional gc)
  (ltl-specification formula)
  (let ((event (find-ltl-specification)))
    (when (and event (or (null gc) (wfcheck-proposition gc)))
      (let ((bdd-undo-point (next-bdd-entry-index))
            (block-array (event-model-block-array (car (last *history*))))
            (pc-infos (event-model-pc-infos (car (last *history*))))
            (state-vars (event-model-state-vars (car (last *history*)))))
        (let ((time1 (get-internal-run-time))
              (model (event-ltl-specification-model event))
              (gc-idx (push-bdd-entry
                        (if (null gc)
                            *bdd-one*
                            (bdd-integrate-formula
                              (convert-formula pc-infos state-vars gc)))))
              (cc-idx (push-bdd-entry *bdd-one*)))
          (let ((result (combo-counterexample-search gc-idx model block-array)))
            (format t "Combination on-the-fly checking took ~3$ seconds.~%"
                    (time-in-seconds (- (get-internal-run-time) time1)))
            (cond ((null result)
                   (undo-bdd-entries-back-to bdd-undo-point)
                   nil)
                  (t
                   (push (make-event-counterexample-path
                           gc gc-idx nil cc-idx result bdd-undo-point)
                         *history*)
                   t))))))))

(defun combo2-ltl-check (formula &optional gc)
  (ltl-specification formula)
  (let ((event (find-ltl-specification)))
    (when (and event (or (null gc) (wfcheck-proposition gc)))
      (let ((bdd-undo-point (next-bdd-entry-index))
            (block-array (event-model-block-array (car (last *history*))))
            (pc-infos (event-model-pc-infos (car (last *history*))))
            (state-vars (event-model-state-vars (car (last *history*)))))
        (let ((time1 (get-internal-run-time))
              (model (event-ltl-specification-model event))
              (gc-idx (push-bdd-entry
                        (if (null gc)
                            *bdd-one*
                            (bdd-integrate-formula
                              (convert-formula pc-infos state-vars gc)))))
              (cc-idx (push-bdd-entry *bdd-one*)))
          (let ((result (combo2-counterexample-search
                          gc-idx model block-array)))
            (format t "Combination on-the-fly checking took ~3$ seconds.~%"
                    (time-in-seconds (- (get-internal-run-time) time1)))
            (cond ((null result)
                   (undo-bdd-entries-back-to bdd-undo-point)
                   nil)
                  (t
                   (push (make-event-counterexample-path
                           gc gc-idx nil cc-idx result bdd-undo-point)
                         *history*)
                   t))))))))

(defun combo-ltl-check-mc (formula &optional gc)
  (ltl-specification formula)
  (let ((event (find-ltl-specification)))
    (when (and event (or (null gc) (wfcheck-proposition gc)))
      (let ((block-array (event-model-block-array (car (last *history*))))
            (pc-infos (event-model-pc-infos (car (last *history*))))
            (state-vars (event-model-state-vars (car (last *history*)))))
        (fair-states gc)
        (let* ((bdd-undo-point (next-bdd-entry-index))
               (model (event-ltl-specification-model event))
               (gc-bdd (if (null gc)
                          *bdd-one*
                          (bdd-integrate-formula
                            (convert-formula pc-infos state-vars gc))))
               (mc (event-fair-states-mc (find-fair-states gc-bdd)))
               (gc-idx (push-bdd-entry gc-bdd))
               (cc-idx (push-bdd-entry *bdd-one*))
               (time1 (get-internal-run-time)))
          (let ((result (combo-counterexample-search-mc
                          gc-idx mc model block-array)))
            (format t "Combination on-the-fly checking took ~3$ seconds.~%"
                    (time-in-seconds (- (get-internal-run-time) time1)))
            (cond ((null result)
                   (undo-bdd-entries-back-to bdd-undo-point)
                   nil)
                  (t
                   (push (make-event-counterexample-path
                           gc gc-idx nil cc-idx result bdd-undo-point)
                         *history*)
                   t))))))))




;;; ==================== Utility functions ======================


;;; find BT nodes whose requirement labels match.
;;; Only nodes in the subtree rooted at bt-node are searched.

(defun find-matching (bt-node requirement-label)
  (append (and (equal (bt-node-requirement-label bt-node)
                      requirement-label)
               (list bt-node))
          (loop for node in (bt-node-children bt-node)
                append (find-matching node requirement-label))))


;;; print elementary blocks that correspond to BT nodes whose
;;; requirement labels match.

(defun print-blocks-matching (requirement-label)
  (unless (null *history*)
    (let ((bt-block-array (event-model-bt-block-array (car (last *history*)))))
      (loop for i in (indices-of-blocks-matching requirement-label)
            do (print-bt-block (aref bt-block-array i))))))

(defun indices-of-blocks-matching (requirement-label)
  (unless (null *history*)
    (let ((bt-block-array (event-model-bt-block-array (car (last *history*)))))
      (loop for i from 0 to (- (length bt-block-array) 1)
            when (let ((nodes (bt-block-bt-nodes (aref bt-block-array i)))
                       (found nil))
                   (loop for n in nodes
                         when (equal (bt-node-requirement-label n)
                                     requirement-label)
                         do (setq found t))
                   found)
            collect i))))

(defun indices-of-reversion-blocks ()
  (unless (null *history*)
    (let ((bt-block-array (event-model-bt-block-array (car (last *history*)))))
      (loop for i from 0 to (- (length bt-block-array) 1)
            when (let ((nodes (bt-block-bt-nodes (aref bt-block-array i)))
                       (found nil))
                   (loop for n in nodes
                         when (eq (bt-node-flag n) 'reversion)
                         do (setq found t))
                   found)
            collect i))))


(defun print-block-label (i)
  (unless (null *history*)
    (let ((bt-block-array (event-model-bt-block-array (car (last *history*)))))
      (format t "[~A"
	      (bt-node-requirement-label
	       (car (bt-block-bt-nodes (aref bt-block-array i)))))
      (loop for node in (cdr (bt-block-bt-nodes (aref bt-block-array i)))
	    do (format t ",~A" (bt-node-requirement-label node)))
      (format t "]"))))

(defun print-test-paths (paths)
  (format t "~%(~S paths)~%" (length paths))
  (loop for path in paths
        do
        (cond ((check-test-path path)
               (format t "~%Valid path:~%")
	       (loop for i in path
		     do (print-block-label i))
	       (format t "~%")
               (print-test-path-summary path))
              (t
               (format t "~%Invalid path:~%")
	       (loop for i in path
		     do (print-block-label i))
	       (format t "~%")
               (print-test-path-summary path)))))




(defvar *print-in-dnf* nil)

(defun print-in-dnf ()
  (setq *print-in-dnf* t))

(defun print-in-cnf ()
  (setq *print-in-dnf* nil))

(defun print-normal-form (bdd)
  (if *print-in-dnf*
      (bdd-to-dnf bdd)
      (bdd-to-cnf bdd)))

(defun print-unencoded (bdd)
  (unless (null *history*)
    (let ((pc-infos (event-model-pc-infos (car (last *history*))))
          (state-vars (event-model-state-vars (car (last *history*)))))
      ;; unencode-formula currently only works with dnf
      (unencode-formula pc-infos state-vars (bdd-to-dnf bdd)))))

;;; Various print routines to examine states.
;;; The print ... -with-matching routines are handy for debugging.
;;; The others are called by print-event.

(defun print-elementary-blocks-matching (requirement-label)
  (let ((nodes (find-matching *bt-root* requirement-label))
        (block-array (event-model-block-array (car (last *history*)))))
    (loop for bt-block in (remove-duplicates
                            (loop for node in nodes
                                  append
                                  (loop for bt-blk in (bt-node-blocks node)
                                        collect bt-blk)))
          do
          (let ((block (aref block-array (bt-block-index bt-block))))
             (print-bt-block bt-block)
             (format t "~%Transitions~%")
             (loop for transition in (elementary-block-transitions block)
                   do
                   (let ((entry (transition-condition transition))
                         (exit (transition-post transition)))
                     (format  t "--~%Guard:~%~S~%"
                              (print-unencoded (bdd-entry entry)))
                     (format  t "Post condition:~%~S~%--~%"
                              (print-unencoded (bdd-entry exit)))))))))

(defun print-reachable-states-matching (requirement-label)
  (let ((event (find-reachable-states))
        (nodes (find-matching *bt-root* requirement-label)))
    (unless (null event)
      (let ((reachable (event-reachable-states-states event)))
        (loop
          for bt-block in (remove-duplicates
                            (loop for node in nodes
                                  append
                                  (loop for bt-blk in (bt-node-blocks node)
                                        collect bt-blk)))
          do
          (let ((rs (aref reachable (bt-block-index bt-block))))
            (print-bt-block bt-block)
            (loop
              for i from 0 to (- (length (reachable-entry-states rs)) 1)
              do
              (progn
                (format t "Entry:~%~S~%"
                          (print-unencoded
                            (bdd-entry (nth i (reachable-entry-states rs)))))
                (format t "Exit:~%~S~%"
                          (print-unencoded
                            (bdd-entry (nth i (reachable-exit-states rs)))))
              ))))))))

(defun print-model-matching (requirement-label)
  (let ((event (find-ltl-specification))
        (nodes (find-matching *bt-root* requirement-label)))
    (unless (null event)
      (let* ((model (event-ltl-specification-model event))
             (tableau (augmented-model-tableau model))
             (bs-array (augmented-model-bs model))
             (m (- (length (tableau-transition-constraints tableau)) 1)))
        (loop
          for bt-block in (remove-duplicates
                            (loop for node in nodes
                                  append
                                  (loop for bt-blk in (bt-node-blocks node)
                                        collect bt-blk)))
          do
          (let ((bs (aref bs-array (bt-block-index bt-block))))
            (print-bt-block bt-block)
            (loop
              for j from 0 to (- (length (bs-entry bs)) 1)
              do
              (loop for k from 0 to m
                    do
                    (progn
                      (format t "~%Entry:~%")
                      (format t "~%~S~%"
                        (print-unencoded
                          (bdd-entry (nth k (nth j (bs-entry bs))))))
                      (format t "~%Exit:~%")
                      (format t "~%~S~%"
                        (print-unencoded
                          (bdd-entry
                            (nth k (nth j (bs-exit bs)))))))))))))))

(defun print-fair-states-matching (requirement-label &optional gc)
  (let ((ltl-event (find-ltl-specification))
        (nodes (find-matching *bt-root* requirement-label)))
    (when (and ltl-event
               nodes
               (or (null gc) (wfcheck-proposition gc)))
      (let ((bt-blocks (remove-duplicates
                          (loop for node in nodes
                                append
                                (loop for bt-block in (bt-node-blocks node)
                                      collect bt-block))))
            (pc-infos (event-model-pc-infos (car (last *history*))))
            (state-vars (event-model-state-vars (car (last *history*)))))
        (let ((m (- (length (tableau-transition-constraints
                              (augmented-model-tableau
                                (event-ltl-specification-model ltl-event))))
                    1))
              (gc-bdd (if (null gc)
                          *bdd-one*
                          (bdd-integrate-formula
                            (convert-formula pc-infos state-vars gc)))))
          (let ((fair-event (find-fair-states gc-bdd)))
            (unless (null fair-event)
              (loop
                for bt-block in bt-blocks
                do
                (let ((bs-entry (aref (mc-top (event-fair-states-mc fair-event))
                                      (bt-block-index bt-block))))
                  (print-bt-block bt-block)
                  (loop
                    for j from 0 to (- (length bs-entry) 1)
                    do
                    (loop for k from 0 to m
                          do
                          (progn
                            (format t "~%Entry:~%")
                            (format t "~%~S~%"
                              (print-unencoded
                                (bdd-entry
                                  (nth k (nth j bs-entry)))))))))))))))))

(defun print-reachable-states (reachable block)
  (let ((rs (aref reachable (bt-block-index block))))
    (unless (null *history*)
      (print-bt-block block)
      (format t "Pre-entry states:~%~S~%"
              (print-unencoded
                (bdd-entry (reachable-pre-entry-states rs)))))))

(defun print-entry-and-exit-states-matching (reachable requirement-label)
  (let ((nodes (find-matching *bt-root* requirement-label)))
    (loop for block in (remove-duplicates
                         (loop for node in nodes
                               append
                               (loop for bt-blk in (bt-node-blocks node)
                                     collect bt-blk)))
          do (print-entry-and-exit-states reachable block))))

(defun print-entry-and-exit-states (reachable block)
  (let ((rs (aref reachable (bt-block-index block))))
    (unless (null *history*)
     (print-bt-block block)
     (loop for i from 1 to (length (reachable-entry-states rs))
           do
           (progn
             (format t "====~%Transition ~S~%" i)
             (format t "Entry States:~%~S~%"
               (print-unencoded
                 (bdd-entry (nth (- i 1) (reachable-entry-states rs)))))
             (format t "~%Exit States:~%~S~%"
               (print-unencoded
                 (bdd-entry
                   (nth (- i 1) (reachable-exit-states rs))))))))))


(defun print-tableau (tableau)
  (unless (null tableau)
    (format t "=====~%Tableau~%====~%")
    ;;(format t "Formula:~%~S~%" (tableau-formula tableau))
    (format t "~%Fairness constraints~%")
    (loop for fc in (tableau-fairness-constraints tableau)
          do (format t "---~%~S~%" (print-unencoded (bdd-entry fc))))
    (format t "~%Transition constraints~%")
    (loop for tc in (tableau-transition-constraints tableau)
          do
          (let ((entry (transition-constraint-entry tc))
                (exit (transition-constraint-exit tc)))
            (format  t "--~%Guard:~%~S~%"
              (print-unencoded (bdd-entry entry)))
            (format  t "Post condition:~%~S~%--~%"
              (print-unencoded (bdd-entry exit)))))))

(defun print-fair-states (mc block)
  (let ((bs-entry (aref (mc-top mc) (bt-block-index block)))
        (m (- (length (tableau-transition-constraints
                        (augmented-model-tableau (mc-model mc))))
              1)))
    (print-bt-block block)
    (loop for j from 0 to (- (length bs-entry) 1)
          do
          (loop for k from 0 to m
                do
                (progn
                  (format t "~%Entry:~%")
                  (format t "~%~S~%"
                    (print-unencoded
                      (bdd-entry (nth k (nth j bs-entry))))))))))

(defun print-counterexample-states (bs-array model block)
  (let ((bs (aref bs-array (bt-block-index block)))
        (m (- (length (tableau-transition-constraints
                        (augmented-model-tableau model)))
              1)))
    (unless (null *history*)
      (print-bt-block block)
      (loop
        for j from 0 to (- (length (bs-entry bs)) 1)
        do
        (loop
          for k from 0 to m
          do
          (progn
            (format t "~%Entry:~%")
            (format t "~%~S~%"
                    (print-unencoded
                      (bdd-entry (nth k (nth j (bs-entry bs))))))
            (format t "~%Exit:~%")
            (format t "~%~S~%"
                    (print-unencoded
                      (bdd-entry (nth k (nth j (bs-exit bs))))))))))))



(defun print-counterexample-path (counter bt-block-array)
  (let ((prefix-start (first counter))
        (prefix-transitions (second counter))
        (cycle-start (third counter))
        (cycle-transitions (fourth counter)))
    (format t "~%Counterexample path:~%")
    (format t "~%~%==== Prefix ====~%")
    (format t "~%Starting states:~%~S~%"
            (print-unencoded (bdd-entry prefix-start)))
    (loop for transition in prefix-transitions
          do
          (let ((blk (first transition))
                (states (second transition)))
            (format t "~%==== Transition through~%")
            (print-bt-block-summary (aref bt-block-array blk))
            (format t "~%to states ====>~%~S~%"
                    (print-unencoded (bdd-entry states)))))
    (format t "~%==== End Prefix ====~%")
    (format t "~%~%==== Cycle ====~%")
    (format t "~%Starting states:~%~S~%"
            (print-unencoded (bdd-entry cycle-start)))
    (loop for transition in cycle-transitions
          do
          (let ((blk (first transition))
                (states (second transition)))
            (format t "~%==== Transition through~%")
            (print-bt-block-summary (aref bt-block-array blk))
            (format t "~%to states ====>~%~S~%"
                    (print-unencoded (bdd-entry states)))))
    (format t "~%==== End Cycle ====~%")))




;;; New version of find-test-paths

(defun find-test-paths (source intermediates target blocks)
  (unless (null *history*)
    (let ((block-array (event-model-block-array (car (last *history*))))
          (reachability-event (find-reachable-states)))
      (let ((result (find-paths-new
                      source target blocks intermediates block-array
                      (and reachability-event
                           (event-reachable-states-states
                             reachability-event)))))
	(remove-duplicate-reversion-paths result)
	;result
	))))

(defun find-test-paths-with-cycles (intermediates blocks)
  (unless (null *history*)
    (let ((block-array (event-model-block-array (car (last *history*))))
          (reachability-event (find-reachable-states))
	  (reversions
	   (loop for i in (indices-of-reversion-blocks)
		 unless (member-equal i blocks)
		 collect i)))
      (remove-duplicate-reversion-paths
        (loop for rev in reversions
              append (find-cyclic-paths
                       rev blocks intermediates block-array
                       (and reachability-event
                            (event-reachable-states-states
                              reachability-event))))))))

(defun find-test-prefixes (source intermediates blocks)
  (unless (null *history*)
    (let ((block-array (event-model-block-array (car (last *history*))))
          (reachability-event (find-reachable-states)))
      (let ((result (find-intermediate-paths
                      source blocks intermediates block-array
                      (and reachability-event
                           (event-reachable-states-states
                             reachability-event)))))
	;(remove-duplicate-reversion-paths result)
	result
	))))

(defun find-test-paths-with-noi-sequence (source targets intermediates blocks)
  (let ((prefixes (find-test-prefixes source intermediates blocks))
	(block-array (event-model-block-array (car (last *history*))))
        (reachability-event (find-reachable-states)))
    (unless (null prefixes)
      (let* ((reachable
	      (and reachability-event
	           (event-reachable-states-states reachability-event)))
             (allowed-blocks
               (loop for i from 0 to (- (length block-array) 1)
                     unless (or (member-equal i blocks)
                                (member-equal i intermediates))
                     collect i))
	     (suffix-start (car (last (car prefixes))))
	     (suffix-start-post
	      (push-bdd-entry
	        (path-postcondition (car prefixes) block-array reachable))))
        (loop for target in targets
              append
              (let* ((visited
                       (list (push-bdd-entry
			      (block-transition-pre
			       target block-array reachable))))
		     (suffixes
		      (find-paths-new-aux
		       suffix-start suffix-start-post allowed-blocks nil
		       visited block-array reachable)))
		(loop for suffix in suffixes
		      collect (append (car prefixes)
			              (cdr suffix)
				      (list target)))))))))

(defun remove-duplicate-reversion-paths (paths)
  (loop for path in paths
	unless
	(and (consp path)
	     (is-reversion-block (car path))
	     (let ((found nil))
	       (loop for p in paths
		     do (when (and (not (is-reversion-block (car p)))
				   (equal (cdr path) (cdr p)))
			  (setq found t)))
	       found))
	collect path))

(defun check-test-path (path)
  (unless (null *history*)
    (let ((block-array (event-model-block-array (car (last *history*))))
          (reachability-event (find-reachable-states)))
      (let ((result (path-is-valid path block-array
                                   (and reachability-event
                                        (event-reachable-states-states
                                           reachability-event)))))
        result))))

(defun test-path-precondition (path)
  (unless (null *history*)
    (let ((block-array (event-model-block-array (car (last *history*))))
          (reachability-event (find-reachable-states)))
      (let ((result
              (path-precondition path block-array
                                 (and reachability-event
                                      (event-reachable-states-states
                                         reachability-event)))))
        (print-unencoded result)))))

(defun test-path-postcondition (path)
  (unless (null *history*)
    (let ((block-array (event-model-block-array (car (last *history*))))
          (reachability-event (find-reachable-states)))
      (let ((result
              (path-postcondition path block-array
                                  (and reachability-event
                                       (event-reachable-states-states
                                          reachability-event)))))
        (print-unencoded result)))))

(defun test-path-preamble (start path)
  (unless (null *history*)
    (let ((block-array (event-model-block-array (car (last *history*))))
          (reachability-event (find-reachable-states)))
      (path-preamble start path block-array
                     (and reachability-event
                          (event-reachable-states-states
                            reachability-event))))))

(defun test-path-postamble (start path)
  (unless (null *history*)
    (let ((block-array (event-model-block-array (car (last *history*))))
          (reachability-event (find-reachable-states)))
      (path-postamble start path block-array
                      (and reachability-event
                           (event-reachable-states-states
                             reachability-event))))))

(defun test-path-state (prefix suffix)
  (unless (null *history*)
    (let ((block-array (event-model-block-array (car (last *history*))))
          (reachability-event (find-reachable-states)))
      (print-unencoded
        (post-prefix-and-pre-suffix
          prefix suffix block-array
          (and reachability-event
               (event-reachable-states-states reachability-event)))))))

(defun print-test-path-summary (path)
  (unless (or (null *history*) (atom path))
    (loop for b in path
          do (print-block-summary b))))

(defun map-index-to-tag (path)
  (unless (null *history*)
    (let ((bt-block-array (event-model-bt-block-array (car (last *history*)))))
      (loop for i in path
            collect (bt-node-requirement-label
                       (car (bt-block-bt-nodes (aref bt-block-array i))))))))

(defun index-to-tag-mapping ()
  (unless (null *history*)
    (let ((bt-block-array (event-model-bt-block-array (car (last *history*)))))
      (loop for i from 0 to (- (length bt-block-array) 1)
            collect
            (list i (bt-node-requirement-label
                      (car (bt-block-bt-nodes (aref bt-block-array i)))))))))

;;; component and behaviour are strings

(defun indices-of-update-blocks (component &optional behaviour)
  (unless (null *history*)
    (let ((bt-block-array (event-model-bt-block-array (car (last *history*)))))
      (loop for i from 0 to (- (length bt-block-array) 1)
            when (let ((nodes (bt-block-bt-nodes (aref bt-block-array i)))
                       (special (bt-block-special (aref bt-block-array i)))
                       (found nil))
                   (unless special
                     (loop for n in nodes
                           when
                           (and (eq (bt-node-type n) 'update)
                                (equal (bt-node-component n) component)
                                (or (null behaviour)
                                    (equal (bt-node-behaviour n) behaviour)))
                           do (setq found t)))
                   found)
            collect i))))

(defun indices-of-event-blocks (component &optional behaviour)
  (unless (null *history*)
    (let ((bt-block-array (event-model-bt-block-array (car (last *history*)))))
      (loop for i from 0 to (- (length bt-block-array) 1)
            when (let ((nodes (bt-block-bt-nodes (aref bt-block-array i)))
                       (special (bt-block-special (aref bt-block-array i)))
                       (found nil))
                   (unless special
                     (loop for n in nodes
                           when
                           (and (eq (bt-node-type n) 'event)
                                (equal (bt-node-component n) component)
                                (or (null behaviour)
                                    (equal (bt-node-behaviour n) behaviour)))
                           do (setq found t)))
                   found)
            collect i))))

(defun sv-name-atring (sv)
  (substring sv 3))

(defun val-name-string (val)
  (substring val 3))

(defun print-block (index)
  (unless (null *history*)
    (let ((bt-block-array (event-model-bt-block-array (car (last *history*)))))
      (cond ((and (>= index 0) (<= index (length bt-block-array)))
             (print-bt-block (aref bt-block-array index)))
            ((bt-block-p index) (print-bt-block index))))))

(defun print-block-summary (index)
  (unless (null *history*)
    (let ((bt-block-array (event-model-bt-block-array (car (last *history*)))))
      (cond ((and (>= index 0) (<= index (length bt-block-array)))
             (print-bt-block-summary (aref bt-block-array index)))
            ((bt-block-p index) (print-bt-block-summary index))))))

(defun print-block-summary-first-node (index)
  (unless (null *history*)
    (let ((bt-block-array (event-model-bt-block-array (car (last *history*)))))
      (cond ((and (>= index 0) (<= index (length bt-block-array)))
	     (print-bt-node-summary
	      (car (bt-block-bt-nodes (aref bt-block-array index)))))
            ((bt-block-p index)
	     (print-bt-node-summary (car (bt-block-bt-nodes index))))))))

(defun number-of-bt-nodes ()
  (unless (null *history*)
    (let ((bt-block-array (event-model-bt-block-array (car (last *history*))))
          (result 0))
      (loop
        for i from 0 to (- (length bt-block-array) 1)
        do (setq result
                 (+ result
                    (length (bt-block-bt-nodes (aref bt-block-array i))))))
      result)))

(defun number-of-blocks ()
  (unless (null *history*)
    (length (event-model-bt-block-array (car (last *history*))))))

(defun is-reversion-block (block-index)
  (when (and (not (null *history*))
             (integerp block-index)
             (<= 0 block-index)
             (< block-index
                (length (event-model-bt-block-array (car (last *history*))))))
    (let ((result nil)
          (bt-block (aref (event-model-bt-block-array (car (last *history*)))
                          block-index)))
      (loop for bt-node in (bt-block-bt-nodes bt-block)
            when (eq (bt-node-flag bt-node) 'reversion)
            do (setq result t))
      result)))


(defun is-subsequence (s1 s2)
  (and (listp s1)
       (listp s2)
       (<= (length s1) (length s2))
       (or (null s1)
	   (and (equal (car s1) (car s2))
		(is-subsequence-aux (cdr s1) (cdr s2)))
	   (is-subsequence-aux s1 (cdr s2)))))

(defun is-subsequence-aux (s1 s2)
  (and (<= (length s1) (length s2))
       (or (null s1)
	   (null s1)
	   (and (equal (car s1) (car s2))
		(is-subsequence-aux (cdr s1) (cdr s2)))
	   (is-subsequence-aux s1 (cdr s2)))))

(defun filter-using-subsequence (sequences subsequence noi)
  (let ((others (loop for el in noi
		      unless (member-equal el subsequence)
		      collect el)))
    (loop for sequence in sequences
	  when (and (null (intersection-equal sequence others))
		    (is-subsequence subsequence sequence))
	  collect sequence)))

(defun shortest-sequence (sequences)
  (let ((result (car sequences)))
    (loop for seq in sequences
	  when (< (length seq) (length result))
	  do (setq result seq))
    result))

(defun shortest-with-subsequence (subsequence sequences noi)
  (shortest-sequence (filter-using-subsequence sequences subsequence noi)))

(defun get-subsequence (noi sequence)
  (loop for element in sequence
	when (member-equal element noi)
	collect element))

(defun get-subsequences (noi sequences)
  (remove-duplicates
    (loop for sequence in sequences
	  collect (get-subsequence noi sequence))))

(defun shortest-paths-for-each-combination (noi paths)
  (loop for combination in (get-subsequences noi paths)
	collect (shortest-with-subsequence combination paths noi)))
