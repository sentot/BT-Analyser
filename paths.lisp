
;;;======================================================================
;;;                                                                     |
;;;  Copyright (c) 2014, Sentot Kromodimoeljo                           |
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

(defun apply-block-forward-transfer (blk state block-array reachable)
  (let ((index (push-bdd-entry *bdd-zero*))
        (transitions (elementary-block-transitions (aref block-array blk))))
    (loop for i from 0 to (- (length transitions) 1)
          do
          (let* ((trans (nth i transitions))
                 (condition
                   (if (null reachable)
                       (bdd-entry (transition-condition trans))
                       (bdd-andop (bdd-entry (transition-condition trans))
                                  (bdd-entry
                                    (nth i (reachable-entry-states
                                             (aref reachable blk)))))))
                 ;; compute (and state guard)
                 (entry-states (bdd-andop (bdd-entry state) condition)))
            ;; compute (override (and state guard) updates) and collect
            (set-bdd-entry
              index
              (bdd-orop (bdd-override entry-states (transition-effects trans))
                        (bdd-entry index)))))
    (let ((result (bdd-entry index)))
      (pop-bdd-entry)
      result)))

(defun apply-block-reverse-transfer (blk state block-array reachable)
  (let ((index (push-bdd-entry *bdd-zero*))
        (transitions (elementary-block-transitions (aref block-array blk))))
    (loop for i from 0 to (- (length transitions) 1)
          do
          (let* ((trans (nth i transitions))
                 (post
                   (if (null reachable)
                       (bdd-entry (transition-post trans))
                       (bdd-entry (nth i (reachable-exit-states
                                            (aref reachable blk))))))
                 ;; compute (and state post)
                 (exit-states (bdd-andop (bdd-entry state) post)))
            ;; compute (override (and state guard) updates) and collect
            (set-bdd-entry
              index
              (bdd-orop
                (bdd-andop
                  (bdd-ignore exit-states
                              (mapcar #'car (transition-effects trans)))
                  (bdd-entry (transition-condition trans)))
                (bdd-entry index)))))
    (let ((result (bdd-entry index)))
      (pop-bdd-entry)
      result)))


;;; ===================== Old version of finding test paths ===================

;;; This version is applicable if the BT is considered to be
;;; a complete BT (a fully integrated BT).

(defun find-paths (state block target blocks block-array reachable)
  (let ((next (apply-block-forward-transfer block state block-array reachable)))
    (unless (eq next *bdd-zero*)
      (let* ((next-state (push-bdd-entry next))
             (result (find-paths-aux
                       block next-state target blocks block-array (list block)
                       reachable)))
        (pop-bdd-entry)
        result))))

(defun find-paths-aux (blk state target blocks block-array prefix reachable)
  (if (null reachable)
      (loop for i from 0 to (- (length block-array) 1)
            append
            (let ((next (apply-block-forward-transfer i state block-array nil)))
              (unless (eq next *bdd-zero*)
                (cond ((member-equal i target)
                       (list (append prefix (list i))))
                      ((and (not (member-equal i blocks))
                            (not (member-equal i prefix)))
                       (let* ((next-state (push-bdd-entry next))
                              (result (find-paths-aux
                                        i next-state target blocks
                                        block-array (append prefix (list i))
                                        nil)))
                         (pop-bdd-entry)
                         result))))))
      (loop for i in (reachable-successors (aref reachable blk))
            append
            (let ((next (apply-block-forward-transfer i state block-array
                                                      reachable)))
              (unless (eq next *bdd-zero*)
                (cond ((member-equal i target)
                       (list (append prefix (list i))))
                      ((and (not (member-equal i blocks))
                            (not (member-equal i prefix)))
                       (let* ((next-state (push-bdd-entry next))
                              (result (find-paths-aux
                                        i next-state target blocks
                                        block-array (append prefix (list i))
                                        reachable)))
                         (pop-bdd-entry)
                         result))))))))

(defun find-single-path (state block target blocks block-array reachable)
  (let ((next (apply-block-forward-transfer block state block-array reachable)))
    (unless (eq next *bdd-zero*)
      (let* ((next-state (push-bdd-entry next))
             (result (find-single-path-aux
                       block target next-state blocks block-array (list block)
                       reachable)))
        (pop-bdd-entry)
        result))))

(defun find-single-path-aux
          (blk target state blocks block-array prefix reachable)
  (if (null reachable)
      (loop for i from 0 to (- (length block-array) 1)
            do
            (let ((next (apply-block-forward-transfer i state block-array nil)))
              (unless (eq next *bdd-zero*)
                (cond ((member-equal i target)
                       (return (append prefix (list i))))
                      ((member-equal i blocks)
                       (return nil))
                      ((member-equal i prefix)
                       (return nil))
                      (t
                       (let* ((next-state (push-bdd-entry next))
                              (result (find-single-path-aux
                                        i target next-state blocks
                                        block-array (append prefix (list i))
                                        nil)))
                         (pop-bdd-entry)
                         (unless (null result)
                           (return result))))))))
      (loop for i in (reachable-successors (aref reachable blk))
            do
            (let ((next (apply-block-forward-transfer i state block-array
                                                      reachable)))
              (unless (eq next *bdd-zero*)
                (cond ((member-equal i target)
                       (return (append prefix (list i))))
                      ((member-equal i blocks)
                       (return nil))
                      ((member-equal i prefix)
                       (return nil))
                      (t
                       (let* ((next-state (push-bdd-entry next))
                              (result (find-single-path-aux
                                        i target next-state blocks
                                        block-array (append prefix (list i))
                                        reachable)))
                         (pop-bdd-entry)
                         (unless (null result)
                           (return result))))))))))


(defun find-paths-with-intermediates
         (state block intermediates target blocks block-array reachable)
  (let ((next (apply-block-forward-transfer block state block-array reachable)))
    (unless (eq next *bdd-zero*)
      (let* ((next-state (push-bdd-entry next))
             (result (find-paths-with-intermediates-aux
                       next-state block intermediates target blocks
                       block-array (list block) reachable)))
        (pop-bdd-entry)
        result))))

(defun find-paths-with-intermediates-aux
         (state block intermediates target blocks block-array prefix reachable)
  (append
    (loop for i in intermediates
          append
          (find-paths-intermediate state block i (remove-equal i intermediates)
                                   target blocks block-array prefix reachable))
    (let ((no-intermediates
            (find-single-path-aux
              block target state (append intermediates blocks)
              block-array prefix reachable)))
      (and no-intermediates
           (list no-intermediates)))))

(defun find-paths-intermediate
         (state block next-intermediate intermediates target
          blocks block-array prefix reachable)
  (if (null reachable)
      (loop for i from 0 to (- (length block-array) 1)
            do
            (let ((next (apply-block-forward-transfer i state block-array nil)))
              (unless (eq next *bdd-zero*)
                (cond ((= i next-intermediate)
                       (let* ((next-state (push-bdd-entry next))
                              (result (find-paths-with-intermediates-aux
                                        next-state i intermediates target
                                        blocks block-array
                                        (append prefix (list i)) reachable)))
                         (pop-bdd-entry)
                         (unless (null result) (return result))))
                      ((and (not (member-equal i intermediates))
                            (not (member-equal i blocks))
                            (not (member-equal i prefix)))
                       (let* ((next-state (push-bdd-entry next))
                              (result (find-paths-intermediate
                                        next-state i next-intermediate
                                        intermediates target blocks
                                        block-array (append prefix (list i))
                                        nil)))
                         (pop-bdd-entry)
                         (unless (null result)
                           (return result))))))))
      (loop for i in (reachable-successors (aref reachable block))
            do
            (let ((next (apply-block-forward-transfer i state block-array
                                                      reachable)))
              (unless (eq next *bdd-zero*)
                (cond ((= i next-intermediate)
                       (let* ((next-state (push-bdd-entry next))
                              (result (find-paths-with-intermediates-aux
                                        next-state i intermediates target
                                        blocks block-array
                                        (append prefix (list i)) reachable)))
                         (pop-bdd-entry)
                         (unless (null result) (return result))))
                      ((and (not (member-equal i intermediates))
                            (not (member-equal i blocks))
                            (not (member-equal i prefix)))
                       (let* ((next-state (push-bdd-entry next))
                              (result (find-paths-intermediate
                                        next-state i next-intermediate
                                        intermediates target blocks
                                        block-array (append prefix (list i))
                                        reachable)))
                         (pop-bdd-entry)
                         (unless (null result)
                           (return result))))))))))


;;; =======================================================================
;;; =============== Checking path and computing precondition ==============


(defun path-is-valid (path block-array reachable)
  (and (listp path)
       (let* ((state (push-bdd-entry *bdd-one*))
              (result (path-is-valid-aux state path block-array reachable)))
         (pop-bdd-entry)
         result)))

(defun path-is-valid-aux (state path block-array reachable)
  (or (null path)
      (let ((next (apply-block-forward-transfer
                    (car path) state block-array reachable)))
        (unless (eq next *bdd-zero*)
          (let* ((next-state (push-bdd-entry next))
                 (result (path-is-valid-aux next-state (cdr path) block-array
                                            reachable)))
            (pop-bdd-entry)
            result)))))

(defun path-precondition (path block-array reachable)
  (and (listp path)
       (let* ((state (push-bdd-entry *bdd-one*))
              (result (path-precondition-aux
                        state (reverse path) block-array reachable)))
         (pop-bdd-entry)
         result)))

;;; path is already reversed

(defun path-precondition-aux (state path block-array reachable)
  (cond
    ((null path) (bdd-entry state))
    (t
     (let ((next (apply-block-reverse-transfer
                   (car path) state block-array reachable)))
       (cond
         ((eq next *bdd-zero*) *bdd-zero*)
         (t (let* ((next-state (push-bdd-entry next))
                   (result (path-precondition-aux
                             next-state (cdr path) block-array reachable)))
              (pop-bdd-entry)
              result)))))))

(defun path-postcondition (path block-array reachable)
  (and (listp path)
       (let* ((state (push-bdd-entry *bdd-one*))
              (result (path-postcondition-aux
                        state path block-array reachable)))
         (pop-bdd-entry)
         result)))

(defun path-postcondition-aux (state path block-array reachable)
  (cond
    ((null path) (bdd-entry state))
    (t
     (let ((next (apply-block-forward-transfer
                   (car path) state block-array reachable)))
       (cond
         ((eq next *bdd-zero*) *bdd-zero*)
         (t (let* ((next-state (push-bdd-entry next))
                   (result (path-postcondition-aux
                             next-state (cdr path) block-array reachable)))
              (pop-bdd-entry)
              result)))))))

;;; Computing path preamble.

(defun path-preamble (start path block-array reachable)
  (and (path-is-valid path block-array reachable)
       (let ((undo-point (next-bdd-entry-index))
             (allowed-blocks
               (loop for i from 0 to (- (length block-array) 1)
                     collect i))
             (precondition (path-precondition path block-array reachable)))
         (let ((visited (list (push-bdd-entry precondition)))
               (start-post
                 (push-bdd-entry
                   (block-transition-post start block-array reachable))))
           (let ((result (path-preamble-aux
                           start start-post allowed-blocks visited
                           block-array reachable)))
             (undo-bdd-entries-back-to undo-point)
             result)))))

(defun path-preamble-aux (target target-post allowed-blocks
                                 visited block-array reachable)
  (let* ((undo-point (next-bdd-entry-index))
         (found (bdd-andop (bdd-entry (car visited)) (bdd-entry target-post)))
         (result (if (eq found *bdd-zero*)
                     (let ((prev (compute-previous-states
                                   (car visited) allowed-blocks block-array
                                   reachable)))
                       (unless (or (eq (bdd-entry prev) *bdd-zero*)
                                   (states-already-visited prev visited))
                         (path-preamble-aux
                            target target-post allowed-blocks
                            (cons prev visited) block-array reachable)))
                     (cons target
                           (choose-path-new-aux
                             (push-bdd-entry found)
                             (cdr visited) block-array reachable)))))
    (undo-bdd-entries-back-to undo-point)
    result))


(defun path-postamble (start path block-array reachable)
  (and (path-is-valid path block-array reachable)
       (not (null path))
       (let ((undo-point (next-bdd-entry-index))
             (target (car (last path)))
             (allowed-blocks
               (loop for i from 0 to (- (length block-array) 1)
                     collect i))
             (precondition (block-transition-pre start block-array reachable)))
         (let ((visited (list (push-bdd-entry precondition)))
               (target-post
                 (push-bdd-entry
                   (path-postcondition path block-array reachable))))
           (let ((result (path-preamble-aux
                           target target-post allowed-blocks visited
                           block-array reachable)))
             (undo-bdd-entries-back-to undo-point)
             (cdr result))))))

(defun post-prefix-and-pre-suffix (prefix suffix block-array reachable)
  (let ((result (path-postcondition prefix block-array reachable)))
    (unless (null result)
      (let ((post (push-bdd-entry result)))
        (setq result (path-precondition suffix block-array reachable))
        (unless (null result)
          (setq result (bdd-andop (bdd-entry post) result)))
        (pop-bdd-entry)))
    result))


;;; ====================================================================
;;; =============== New version of finding test paths ==================


;;; New strategy for finding paths which first performs backward
;;; searches for starting points.

(defun find-paths-new (from to blocks intermediates block-array reachable)
  (let ((undo-point (next-bdd-entry-index))
        (target from)
        (allowed-blocks
          (loop for i from 0 to (- (length block-array) 1)
                unless (or (member-equal i blocks)
                           (member-equal i intermediates))
                collect i)))
    (let ((target-post
            (push-bdd-entry
              (block-transition-post from block-array reachable)))
          (visited
            (list (push-bdd-entry
                    (block-transition-pre to block-array reachable)))))
      (let ((result
              (find-paths-new-aux
                target target-post allowed-blocks intermediates visited
                (list to) block-array reachable)))
        (undo-bdd-entries-back-to undo-point)
        result))))


;;; The workhorse for the backward-search-first strategy.
;;; visited must not be nil ((car visited) is "current" set of states).

(defun find-paths-new-aux (target target-post allowed-blocks intermediates
                           visited suffix block-array reachable)
  (let* ((undo-point (next-bdd-entry-index))
         (found (bdd-andop (bdd-entry (car visited)) (bdd-entry target-post)))
         (result
           (append
             (if (eq found *bdd-zero*)
                 (let ((prev (compute-previous-states
                               (car visited) allowed-blocks block-array
                               reachable)))
                   (unless (or (eq (bdd-entry prev) *bdd-zero*)
                               (states-already-visited prev visited))
                     (find-paths-new-aux
                       target target-post allowed-blocks nil (cons prev visited)
                       suffix block-array reachable)))
                 (list (cons target
                             (append (choose-path-new-aux
                                       (push-bdd-entry found)
                                       (cdr visited) block-array reachable)
                                     suffix))))
             (loop
               for i in intermediates
               append
               (find-paths-new-aux-intermediate
                 i
                 (push-bdd-entry
                   (block-transition-post i block-array reachable))
                 target target-post allowed-blocks
                 (remove-equal i intermediates)
                 visited suffix block-array reachable)))))
    (undo-bdd-entries-back-to undo-point)
    result))

(defun find-paths-new-aux-intermediate (int int-post target target-post
                                        allowed-blocks intermediates visited
                                        suffix block-array reachable)
  (let ((found (bdd-andop (bdd-entry (car visited)) (bdd-entry int-post))))
    (cond ((eq found *bdd-zero*)
           ;; must continue search for the intermediate transition
           (let ((from-states (compute-previous-states
                                (car visited) allowed-blocks
                                block-array reachable)))
             (unless (or (eq (bdd-entry from-states) *bdd-zero*)
                         (states-already-visited from-states visited))
               (find-paths-new-aux-intermediate
                 int int-post target target-post allowed-blocks
                 intermediates (cons from-states visited) suffix
                 block-array reachable))))
          (t
           ;; found the intermediate transition
           ;; can now continue with normal search with a fresh visited
           ;; but first choose a subpath to prepend to suffix
           (let ((new-suffix
                   (cons int
                     (append (choose-path-new visited block-array reachable)
                             suffix))))
             (find-paths-new-aux
               target target-post allowed-blocks intermediates
               ;; fresh visited is the precondition for the suffix
               (list (push-bdd-entry
                       (path-precondition new-suffix block-array reachable)))
               new-suffix block-array reachable))))))

;;; membership of states in visited through bdd-entry

(defun states-already-visited (states visited)
  (let ((already-visited nil))
    (loop for v in visited
          while (null already-visited)
          do (when (eq (bdd-entry states) (bdd-entry v))
               (setq already-visited t)))
    already-visited))

;;; Computes states that can transition to states in to-states
;;; through blocks in allowed-blocks.

(defun compute-previous-states (to-states allowed-blocks block-array reachable)
  (let ((previous-states (push-bdd-entry *bdd-zero*)))
    (loop for blk in allowed-blocks
          do (set-bdd-entry previous-states
                            (bdd-orop (apply-block-reverse-transfer
                                        blk to-states block-array reachable)
                                      (bdd-entry previous-states))))
    previous-states))

;;; visited cannot be nil

(defun choose-path-new (visited block-array reachable)
  (choose-path-new-aux (car visited) (cdr visited) block-array reachable))

(defun choose-path-new-aux (states visited block-array reachable)
  (unless (null visited)
    (let ((blk nil)
          (possible-next-states (car visited))
          (next-states nil))
      (loop
        for i from 0 to (- (length block-array) 1)
        while (null blk)
        do
        (let ((next (bdd-andop
                      (apply-block-forward-transfer
                        i states block-array reachable)
                      (bdd-entry possible-next-states))))
          (unless (eq next *bdd-zero*)
            (setq blk i)
            (setq next-states (push-bdd-entry next)))))
      (unless (null blk)
        (cons blk
              (choose-path-new-aux
                next-states (cdr visited) block-array reachable))))))

(defun block-transition-pre (blk block-array reachable)
  (let ((index (push-bdd-entry *bdd-zero*))
        (transitions (elementary-block-transitions (aref block-array blk))))
    (loop for i from 0 to (- (length transitions) 1)
          do
          (let* ((trans (nth i transitions))
                 (condition
                   (if (null reachable)
                       (bdd-entry (transition-condition trans))
                       (bdd-andop (bdd-entry (transition-condition trans))
                                  (bdd-entry
                                    (nth i (reachable-entry-states
                                             (aref reachable blk))))))))
            (set-bdd-entry index (bdd-orop condition (bdd-entry index)))))
    (let ((result (bdd-entry index)))
      (pop-bdd-entry)
      result)))

(defun block-transition-post (blk block-array reachable)
  (let ((index (push-bdd-entry *bdd-zero*))
        (transitions (elementary-block-transitions (aref block-array blk))))
    (loop for i from 0 to (- (length transitions) 1)
          do
          (let* ((trans (nth i transitions))
                 (post
                   (if (null reachable)
                       (bdd-entry (transition-post trans))
                       (bdd-entry (nth i (reachable-exit-states
                                            (aref reachable blk)))))))
            (set-bdd-entry index (bdd-orop post (bdd-entry index)))))
    (let ((result (bdd-entry index)))
      (pop-bdd-entry)
      result)))



;;; ====================================================================
;;; ============ A second version of finding test paths ================

;;; The intermediates are specified as a sequence (that must be a
;;; "subsequence" of the test path).

(defun find-paths-2 (from to blocks intermediates block-array reachable)
  (let ((undo-point (next-bdd-entry-index))
        (target from)
        (allowed-blocks
          (loop for i from 0 to (- (length block-array) 1)
                unless (or (member-equal i blocks)
                           (member-equal i intermediates))
                collect i)))
    (let ((target-post
            (push-bdd-entry
              (block-transition-post from block-array reachable)))
          (visited
            (list (push-bdd-entry
                    (block-transition-pre to block-array reachable)))))
      (let ((result
              (find-paths-2-aux
                target target-post allowed-blocks intermediates visited
                (list to) block-array reachable)))
        (undo-bdd-entries-back-to undo-point)
        result))))

;;; The workhorse for the second version.
;;; visited must not be nil ((car visited) is "current" set of states).

(defun find-paths-2-aux (target target-post allowed-blocks intermediates
                           visited suffix block-array reachable)
  (let* ((undo-point (next-bdd-entry-index))
         (found (bdd-andop (bdd-entry (car visited)) (bdd-entry target-post)))
         (result
           (append
             (if (eq found *bdd-zero*)
                 (let ((prev (compute-previous-states
                               (car visited) allowed-blocks block-array
                               reachable)))
                   (unless (or (eq (bdd-entry prev) *bdd-zero*)
                               (states-already-visited prev visited))
                     (find-paths-new-aux
                       target target-post allowed-blocks nil (cons prev visited)
                       suffix block-array reachable)))
                 (list (cons target
                             (append (choose-path-new-aux
                                       (push-bdd-entry found)
                                       (cdr visited) block-array reachable)
                                     suffix))))
             (loop
               for i in intermediates
               append
               (find-paths-new-aux-intermediate
                 i
                 (push-bdd-entry
                   (block-transition-post i block-array reachable))
                 target target-post allowed-blocks
                 (remove-equal i intermediates)
                 visited suffix block-array reachable)))))
    (undo-bdd-entries-back-to undo-point)
    result))
