
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

;;; Structure for BT-specific fields of elementary blocks.
;;; (elementary-block-source b) should be set to something
;;; of this structure.

;;; - bt-nodes: list of bt-nodes (in correct order) in the block
;;; - children: (blocks inherits tree structure from BTs)
;;; - select: indicates whether there is a select in the block (t or nil)
;;; - synchronise: list of nodes to be synchronised
;;; - input: list of nodes that are input partners
;;; - output: list of nodes that are output partners
;;; - condition: condition for the block to be executed
;;; - event: representative event for the block
;;; - pc: PC for the block is PC for all nodes in the block
;;; - pc-value: value of above PC at entry to the block
;;; - pc-effects: effects on PCs from executing the block
;;; - main-effects: effects on components/attributes
;;; - conditional-effects: to be used as basis for transitions
;;; - prioritized-blocks: blocks that have higher prioritization
;;; - special: indicates whether the block is artificial (t or nil)
;;; - index: unique index number for block

(defstruct
  (bt-block
    (:constructor make-bt-block (bt-nodes children select
                                         synchronise input output
                                         condition event pc pc-value
                                         pc-effects main-effects special))
    :named :predicate)
   bt-nodes children parent select synchronise input output
   condition event pc pc-value
   pc-effects main-effects conditional-effects prioritized-blocks
   special index)




;;; Structure for effects.
;;; An effect is simply a pair:
;;; - l-value: the variable whose value is affected,
;;; - r-value: the new value for the variable.

(defstruct (effect (:constructor make-effect (l-value r-value))
                   :named :predicate)
           l-value r-value)


(defvar *empty-effects* nil)

;;; Effects in a set of effects are ordered by l-value.
;;; Insert effect into a set of effects.  If an effect
;;; with the same l-value is already in the set of effects,
;;; it is overriden.

(defun insert-effect (effect effects)
  (let ((rest effects)
        (l-value (effect-l-value effect)))
    (loop for e in effects
          while (alphalessp (effect-l-value e) l-value)
          collecting e into front
          do (setq rest (cdr rest))
          finally
          (return (cond ((and (not (null rest))
                              (equal (effect-l-value (car rest))
                                     l-value))
                         (append front (cons effect (cdr rest))))
                        (t (append front (cons effect rest))))))))

;;; Override a set of effects with overriding-effects.

(defun override-effects (effects overriding-effects)
  (let ((result effects))
    (loop for effect in (copy-effects overriding-effects)
          do (setq result (insert-effect effect result)))
    result))

;;; Return t iff there is a conflict (i.e., there is an l-value
;;; that is in both sets, that is assigned to different r-values
;;; in the two sets).

(defun conflicting-effects (effects1 effects2)
  (let ((conflict nil))
    (loop for e1 in effects1
          do (loop for e2 in effects2
                   do (when (and (equal (effect-l-value e1)
                                        (effect-l-value e2))
                                 (not (equal (effect-r-value e1)
                                             (effect-r-value e2))))
                        (setq conflict t))))
    conflict))

;;; Structure for conditional effect.
;;; It is simply a pair consisting of a condition and an effect.

(defstruct (conditional-effects
              (:constructor make-conditional-effects (condition effects))
              :named :predicate)
           condition effects)




;;; ===================== Expand References ====================

(defvar *goto-semantics* t)

;;; Code to expand references to other nodes.
;;; If *goto-semantics* is t, then only references to nodes in
;;; other threads are expanded.  Otherwise all references are
;;; expanded. A referencing node is replaced by a copy of the
;;; subtree referred.

(defun expand-references (bt-node)
  (cond ((and (eq (bt-node-flag bt-node) 'reference)
              (null (bt-node-children bt-node)))
         (let ((target-node (find-reference-target bt-node)))
           (unless (null target-node)
             (let ((parent-node (bt-node-parent bt-node))
                   (replacement-node (copy-bt-tree target-node)))
               (setf (bt-node-parent replacement-node)
                     (bt-node-parent bt-node))
               (setf (bt-node-children parent-node)
                     (loop for node in (bt-node-children parent-node)
                           collect
                           (if (eq node bt-node)
                               replacement-node
                               node)))))))
        (t (loop for node in (bt-node-children bt-node)
                 do (expand-references node)))))

(defun expand-quantifications (bt-node)
  ;; bottom-up expansion
  (loop for node in (bt-node-children bt-node)
        do (expand-quantifications node))
  (cond
    ((and (eq (bt-node-type bt-node) 'quantification)
          (not (bt-node-atomic bt-node))
          (null (bt-node-branch-type bt-node))
          (bt-node-children bt-node))
     (let* ((behaviour (bt-node-behaviour bt-node))
            (parameters (is-universal-quantification behaviour)))
       (cond (parameters
              ;; BT node is a universal quantification
              (setf (bt-node-children bt-node)
                    (loop for par in (cdr parameters)
                          collect
                          (let ((child (copy-bt-tree
                                         (car (bt-node-children bt-node)))))
                            (substitute-attribute child (car parameters) par)
                            (setf (bt-node-parent child) bt-node)
                            child)))
              (setf (bt-node-l-value bt-node) nil)
              (setf (bt-node-r-value bt-node) nil)
              (setf (bt-node-branch-type bt-node) 'parallel))
             ((setq parameters (is-existential-quantification behaviour))
              ;; BT node is an existential quantification
              (setf (bt-node-children bt-node)
                    (loop for par in (cdr parameters)
                          collect
                          (let ((child (copy-bt-tree
                                         (car (bt-node-children bt-node)))))
                            (substitute-attribute child (car parameters) par)
                            (setf (bt-node-parent child) bt-node)
                            child)))
              (setf (bt-node-l-value bt-node) nil)
              (setf (bt-node-r-value bt-node) nil)
              (setf (bt-node-branch-type bt-node) 'alternative)))))
    ((bt-node-atomic bt-node)
     (let* ((behaviour (bt-node-behaviour bt-node))
            (parameters (is-universal-node-quantification behaviour)))
       (unless (null parameters)
         (let ((children (expand-universal-node-quantification
                            (car (bt-node-children bt-node)) parameters)))
           (setf (bt-node-l-value bt-node) nil)
           (setf (bt-node-r-value bt-node) nil)
           (setf (bt-node-children bt-node) children)
           (loop for child in children
                 do (setf (bt-node-parent child) bt-node))))))))

;;; Recursive function to expand BT nodes within an atomic block
;;; based on a "forall" quantification.

(defun expand-universal-node-quantification (bt-node parameters)
  (let ((children (if (bt-node-atomic bt-node)
                      ;; more nodes in the atomic composition
                      (expand-universal-node-quantification
                        (car (bt-node-children bt-node)) parameters)
                      ;; last node in the atomic composition
                      (bt-node-children bt-node)))
        (is-last t))
    (loop for par in (reverse (cdr parameters))
          do
          (let ((new-node (make-node-instance-with-substitution
                            bt-node (car parameters) par children is-last)))
            (setq is-last nil)
            (setq children (list new-node))))
    children))

(defun make-node-instance-with-substitution (bt-node attr new-attr children
                                             is-last)
  (let ((new-node (make-bt-node (bt-node-type bt-node))))
    (setf (bt-node-requirement-label new-node)
          (bt-node-requirement-label bt-node))
    (setf (bt-node-requirement-status new-node)
          (bt-node-requirement-status bt-node))
    (setf (bt-node-component new-node)
          (substitute-attribute-in-string
            (bt-node-component bt-node) attr new-attr))
    (setf (bt-node-behaviour new-node)
          (substitute-attribute-in-string
            (bt-node-behaviour bt-node) attr new-attr))
    (setf (bt-node-behaviour-type new-node)
          (bt-node-behaviour-type bt-node))
    (setf (bt-node-flag new-node) (bt-node-flag bt-node))
    (setf (bt-node-label new-node) (bt-node-label bt-node))
    (if is-last
        (setf (bt-node-branch-type new-node) (bt-node-branch-type bt-node))
        (setf (bt-node-branch-type new-node) nil))
    ;; need to handle atomicity specially
    (if is-last
        (setf (bt-node-atomic new-node) (bt-node-atomic bt-node))
        (setf (bt-node-atomic new-node) t))
    (setf (bt-node-event new-node) (bt-node-event bt-node))
    (setf (bt-node-children new-node) children)
    (loop for child in children
          do (setf (bt-node-parent child) new-node))
    (set-l-value-and-r-value-event new-node)
    new-node))

;;; This is for a new kind of forall: "||&s:{s1,...,sn}"

(defun is-universal-node-quantification (behaviour)
  (when (and (> (length behaviour) 7)
             (equal (substring behaviour 0 3) "||&")
             (equal (substring behaviour (- (length behaviour) 1)) "}"))
    (let ((i (string-search= ":" behaviour)))
      (when (and i
                 (equal (substring behaviour (+ i 1) (+ i 2)) "{")
                 (< i (- (length behaviour) 3)))
        (let ((parameters
                (substring behaviour (+ i 2) (- (length behaviour) 1)))
              (done nil))
          (cons (substring behaviour 3 i)
                (loop while (null done)
                      collect
                      (let ((j (string-search= "," parameters)))
                        (cond ((null j)
                               (setq done t)
                               parameters)
                              (t
                               (let ((par (substring parameters 0 j)))
                                 (setq parameters
                                       (substring parameters (+ j 1)))
                                 par)))))))))))

;;; This is for the "normal" forall: "||s:{s1,...,sn}"

(defun is-universal-quantification (behaviour)
  (when (and (> (length behaviour) 6)
             (equal (substring behaviour 0 2) "||")
             (equal (substring behaviour (- (length behaviour) 1)) "}"))
    (let ((i (string-search= ":" behaviour)))
      (when (and i
                 (equal (substring behaviour (+ i 1) (+ i 2)) "{")
                 (< i (- (length behaviour) 3)))
        (let ((parameters
                (substring behaviour (+ i 2) (- (length behaviour) 1)))
              (done nil))
          (cons (substring behaviour 2 i)
                (loop while (null done)
                      collect
                      (let ((j (string-search= "," parameters)))
                        (cond ((null j)
                               (setq done t)
                               parameters)
                              (t
                               (let ((par (substring parameters 0 j)))
                                 (setq parameters
                                       (substring parameters (+ j 1)))
                                 par)))))))))))

;;; Existential here is for one arbitrary: "[]s:{s1,...,sn}"

(defun is-existential-quantification (behaviour)
  (when (and (> (length behaviour) 6)
             (equal (substring behaviour 0 2) "[]")
             (equal (substring behaviour (- (length behaviour) 1)) "}"))
    (let ((i (string-search= ":" behaviour)))
      (when (and i
                 (equal (substring behaviour (+ i 1) (+ i 2)) "{")
                 (< i (- (length behaviour) 3)))
        (let ((parameters
                (substring behaviour (+ i 2) (- (length behaviour) 1)))
              (done nil))
          (cons (substring behaviour 2 i)
                (loop while (null done)
                      collect
                      (let ((j (string-search= "," parameters)))
                        (cond ((null j)
                               (setq done t)
                               parameters)
                              (t
                               (let ((par (substring parameters 0 j)))
                                 (setq parameters
                                       (substring parameters (+ j 1)))
                                 par)))))))))))

(defun substitute-attribute (bt-node attr new-attr)
  (setf (bt-node-component bt-node)
        (substitute-attribute-in-string
          (bt-node-component bt-node) attr new-attr))
  (setf (bt-node-behaviour bt-node)
        (substitute-attribute-in-string
          (bt-node-behaviour bt-node) attr new-attr))
  (unless (eq (bt-node-type bt-node) 'quantification)
    (set-l-value-and-r-value-event bt-node))
  (loop for node in (bt-node-children bt-node)
        do (substitute-attribute node attr new-attr)))


(defun substitute-attribute-in-string (str attr new-attr)
  (let ((i (string-search= attr str)))
    (cond ((null i) str)
          ((or (and (= i 0)
                    (or (= (length attr) (length str))
                        (let ((follows (substring str
                                                  (+ i (length attr))
                                                  (+ i (length attr) 1))))
                          (or (equal follows ":")
                              (equal follows "=")))))
               (and (> i 0)
                    (> (length str) (+ i (length attr)))
                    (equal (substring str (- i 1) i) "(")
                    (or (equal (substring str (+ i (length attr))
                                          (+ i (length attr) 1))
                               ")")
                        (equal (substring str (+ i (length attr))
                                          (+ i (length attr) 1))
                               "="))))
           (string-append (substring str 0 i)
                          new-attr
                          (substring str (+ i (length attr)))))
          (t str))))

;;; Code to copy a tree (or subtree)

(defun copy-bt-tree (bt-node)
  (let ((new-node (make-bt-node (bt-node-type bt-node))))
    (setf (bt-node-requirement-label new-node)
          (bt-node-requirement-label bt-node))
    (setf (bt-node-requirement-status new-node)
          (bt-node-requirement-status bt-node))
    (setf (bt-node-component new-node) (bt-node-component bt-node))
    (setf (bt-node-behaviour new-node) (bt-node-behaviour bt-node))
    (setf (bt-node-behaviour-type new-node)
          (bt-node-behaviour-type bt-node))
    (setf (bt-node-flag new-node) (bt-node-flag bt-node))
    (setf (bt-node-label new-node) (bt-node-label bt-node))
    (setf (bt-node-branch-type new-node)
          (bt-node-branch-type bt-node))
    (setf (bt-node-atomic new-node) (bt-node-atomic bt-node))
    (setf (bt-node-l-value new-node) (bt-node-l-value bt-node))
    (setf (bt-node-r-value new-node) (bt-node-r-value bt-node))
    (setf (bt-node-event new-node) (bt-node-event bt-node))
    (setf (bt-node-children new-node)
          (loop for node in (bt-node-children bt-node)
                collect
                (let ((child (copy-bt-tree node)))
                  (setf (bt-node-parent child) new-node)
                  child)))
    new-node))

;;; Code to find the target of a reference (bt-node is the
;;; referencing BT node).  The target must be to the "left"
;;; of the referencing node.

(defun find-reference-target (bt-node)
  (find-reference-target-aux bt-node bt-node (bt-node-parent bt-node) t))

;;; This function is called to go up the tree during the search.

(defun find-reference-target-aux (bt-node child-node node same-thread)
  (unless (null node)
    (let ((same (and same-thread
                     (not (eq (bt-node-branch-type node) 'parallel))))
          (children (bt-node-children node))
          (target-node nil))
      (loop while (and (null target-node)
                       (not (null children))
                       (not (eq (car children) child-node)))
            do (progn (setq target-node
                            (find-reference-target-aux-aux
                               bt-node (car children) same))
                      (setq children (cdr children))))
      (or target-node
          (find-reference-target-aux
            bt-node node (bt-node-parent node) same)))))

;;; This function searches in a subtree.  It is expected that
;;; the subtree is to the "left" of the referencing node.

(defun find-reference-target-aux-aux (bt-node node same-thread)
  (cond ((and (equal (bt-node-component node)
                     (bt-node-component bt-node))
              (equal (bt-node-behaviour node)
                     (bt-node-behaviour bt-node))
              (equal (bt-node-label node)
                     (bt-node-label bt-node))
              (not (null (bt-node-children node)))
              (or (null same-thread)
                  (null *goto-semantics*)))
          node)
        (t (let ((target-node nil)
                 (same (and same-thread
                            (not (eq (bt-node-branch-type node) 'parallel)))))
             (loop for child-node in (bt-node-children node)
                   while (null target-node)
                   do (setq target-node (find-reference-target-aux-aux
                                             bt-node child-node same)))
             target-node))))





;;; ===================== Assign PCs ====================

;;; Code to assign PCs and PC values to nodes.
;;; pc-index uniquely determines the PC to be assigned to the node
;;; pc-value is the value of the PC at the entrance of the node
;;; next-index is index for the next PC to be created
;;; next-value is the value of the PC at the exit of the node

(defun assign-pc (bt-node pc-index pc-value next-index next-value)
  (setf (bt-node-pc bt-node)
        (intern (string-append "PC" (format nil "~A" pc-index))
                *package*))
  (setf (bt-node-pc-value bt-node) pc-value)
  (setf (bt-node-pc-next bt-node) next-value)
  (cond ((eq (bt-node-branch-type bt-node) 'alternative)
         (let ((next-i next-index)
               (next-v (+ next-value 1)))
           (loop for node in (bt-node-children bt-node)
                 ;; the branches use the same PC and starts the same
                 ;; however, each branch will have a different next
                 ;; PC value using next-v to keep track of what the
                 ;; next PC value must be
                 do (multiple-value-setq (next-i next-v)
                       (assign-pc node pc-index next-value next-i next-v)))
           (values next-i next-v)))
        ((eq (bt-node-branch-type bt-node) 'parallel)
         ;; next index must be incremented by the number of children
         (let ((next-i (+ next-index (length (bt-node-children bt-node))))
               (next-v (+ next-value 1)))
           (loop for node in (bt-node-children bt-node)
                 for index from next-index
                 ;; Each branch starts a new thread with a newly created
                 ;; PC so can safely use PC value of 1 and next PC value
                 ;; of 2.  next-v is irrelevant here.
                 do (multiple-value-setq (next-i next-v)
                       (assign-pc node index 1 next-i 2)))
           ;; next value returned must be different from "suspended" value
           (values next-i (+ next-value 1))))
        ((null (bt-node-children bt-node))
         ;; next value returned must be different from "suspended" value
         (values next-index (+ next-value 1)))
        (t
         ;; next value for child is simply one more than its PC value
         (assign-pc (first (bt-node-children bt-node)) pc-index
                    next-value next-index (+ next-value 1)))))




;;; ================ Control Flow Directives (Part 1) ===============

;;; Code to set the targets of kills, reversions and references.
;;; At the top level, set-targets is to be called with *bt-root*.

(defun set-targets (bt-node)
  (cond ((eq (bt-node-flag bt-node) 'kill)
         ;; we also take this opportunity to clear the "behaviour"
         ;; of the kill node
         (setf (bt-node-l-value bt-node) nil)
         (setf (bt-node-r-value bt-node) nil)
         (setf (bt-node-event bt-node) nil)
         (unless (set-kill-target bt-node)
           (target-error 'kill bt-node)))
        ((eq (bt-node-flag bt-node) 'reversion)
         (cond ((not (null (bt-node-children bt-node)))
                ;; source of reversion must be a leaf
                (source-error 'reversion bt-node))
               ((not (set-revert-target bt-node))
                (target-error 'reversion bt-node))))
        ((eq (bt-node-flag bt-node) 'reference)
         (cond ((not (null (bt-node-children bt-node)))
                ;; source of reference must be a leaf
                (source-error 'reference bt-node))
               ((not (set-reference-target bt-node))
                (target-error 'reference bt-node)))))
  (loop for node in (bt-node-children bt-node)
        do (set-targets node)))

;;; Target for kill must be in a different parallel branch.
;;; First find closest ancestor that is a parallel branching node.
;;; Search the other branches for a matching BT node.
;;; If not found find next closest ancestor that is a parallel
;;; branching node and repeat the process until matching BT node
;;; is found or we can't find any more ancestor that is a parallel
;;; branching node.

(defun set-kill-target (bt-node)
  (set-kill-target-aux bt-node bt-node (bt-node-parent bt-node)))

(defun set-kill-target-aux (bt-node child-node node)
  (cond ((null node)
         ;; already past the root, no match found
         nil)
        ;; If node is a parallel branching, look at other
        ;; branches first.  If target not found, go up the tree
        ;; to find another parallel branching point.
        ((eq (bt-node-branch-type node) 'parallel)
         (or (loop for n in (bt-node-children node)
                   when (and (not (eq n child-node))
                             (set-kill-target-aux-aux bt-node n))
                   do (return t))
             (set-kill-target-aux bt-node node (bt-node-parent node))))
        ;; Go up the tree if node is not a parallel branching node.
        (t (set-kill-target-aux bt-node node (bt-node-parent node)))))

(defun set-kill-target-aux-aux (bt-node node)
  (cond ((and (equal (bt-node-component node) (bt-node-component bt-node))
              (equal (bt-node-behaviour node) (bt-node-behaviour bt-node))
              (equal (bt-node-label node) (bt-node-label bt-node))
              ;; target must not have a kill flag
              (not (eq (bt-node-flag node) 'kill)))
         (setf (bt-node-target bt-node) node))
        ;; If node doesn't match, search the children (and their
        ;; descendants).
        (t (loop for n in (bt-node-children node)
                 when (set-kill-target-aux-aux bt-node n)
                 do (return t)))))



;;; Target for reversion must be an ancestor and not atomically linked
;;; with its children.

(defun set-revert-target (bt-node)
  (let ((component (bt-node-component bt-node))
        (behaviour (bt-node-behaviour bt-node))
        (label (bt-node-label bt-node))
        (is-atomic (and (bt-node-parent bt-node)
                        (bt-node-atomic (bt-node-parent bt-node)))))
    (loop for node = (bt-node-parent bt-node) then (bt-node-parent node)
          while (not (null node))
          ;; target node must not be atomically composed with its
          ;; children and must not have a kill flag
          when (and (equal (bt-node-component node) component)
                    (equal (bt-node-behaviour node) behaviour)
                    (equal (bt-node-label node) label)
                    (or (not (bt-node-atomic node))
                        ;; new special case when reversion node is
                        ;; not in an atomic composition but the target is.
                        ;; need to ensure that the target is the first node
                        ;; in an atomic composition
                        (and (not is-atomic)
                             (or (null (bt-node-parent node))
                                 (not (bt-node-atomic (bt-node-parent node))))))
                    (not (eq (bt-node-flag node) 'kill)))
          do (return (setf (bt-node-target bt-node) node)))))

;;; Target for reference must have children, must not be atomically
;;; linked with its children and must be in the same thread as the source.

(defun set-reference-target (bt-node)
  ;; Go up the tree.
  (set-reference-target-aux bt-node bt-node (bt-node-parent bt-node)))

(defun set-reference-target-aux (bt-node child-node node)
  (cond ((or (null node) (eq (bt-node-branch-type node) 'parallel))
          ;; must not go through parallel branching
          ;; (must be in same thread)
          nil)
        ;; target node must not be atomically composed with its children
        ;; and must not have a kill flag
        ((and (equal (bt-node-component node) (bt-node-component bt-node))
              (equal (bt-node-behaviour node) (bt-node-behaviour bt-node))
              (equal (bt-node-label node) (bt-node-label bt-node))
              (not (bt-node-atomic node))
              (not (eq (bt-node-flag node) 'kill)))
         (setf (bt-node-target bt-node) node))
        (t (or (loop for n in (bt-node-children node)
                     when (and (not (eq n child-node))
                               (set-reference-target-aux-aux bt-node n))
                     do (return t))
               (set-reference-target-aux
                 bt-node node (bt-node-parent node))))))

(defun set-reference-target-aux-aux (bt-node node)
  (cond ((null (bt-node-children node)) nil)
        ;; target node must not be atomically composed with its children
        ;; and must not have a kill flag
        ((and (equal (bt-node-component node) (bt-node-component bt-node))
              (equal (bt-node-behaviour node) (bt-node-behaviour bt-node))
              (equal (bt-node-label node) (bt-node-label bt-node))
              (not (bt-node-atomic node))
              (not (eq (bt-node-flag node) 'kill)))
         (setf (bt-node-target bt-node) node))
        ((not (eq (bt-node-branch-type node) 'parallel))
         ;; must not go through parallel branching
         (loop for n in (bt-node-children node)
                 when (set-reference-target-aux-aux bt-node n)
                 do (return t)))))



;;; ===================== Synchronization (Part 1)====================


(defun set-sync-targets (bt-node)
  (when (and (eq (bt-node-flag bt-node) 'synchronise)
             (null (bt-node-target bt-node)))
    (let ((sync-nodes (collect-sync-nodes *bt-root* bt-node)))
      ;(when (< (length sync-nodes) 2) (sync-error 'no-target bt-node))
      (loop for node in sync-nodes
            do (setf (bt-node-target node) sync-nodes))))
  (loop for node in (bt-node-children bt-node)
        do (set-sync-targets node)))

(defun collect-sync-nodes (bt-node ref-node)
  (let ((err nil)
        (node-is-sync-node
          (and (equal (bt-node-component bt-node)
                      (bt-node-component ref-node))
               (equal (bt-node-behaviour bt-node)
                      (bt-node-behaviour ref-node))
               (equal (bt-node-label bt-node) (bt-node-label ref-node))
               (eq (bt-node-flag bt-node) 'synchronise))))
    (let ((result-from-children
            (cond ((null (bt-node-children bt-node)) nil)
                  ((eq (bt-node-branch-type bt-node) 'parallel)
                   (loop for n in (bt-node-children bt-node)
                         append (collect-sync-nodes n ref-node)))
                  (t
                   ;; if not a parallel branching node,
                   ;; only 1 branch can have matching sync nodes
                   (let ((empty t))
                     (loop for n in (bt-node-children bt-node)
                           append
                             (let ((result
                                     (collect-sync-nodes n ref-node)))
                               (when result
                                 (when (not empty) (setq err t))
                                 (setq empty nil))
                               result)))))))
      (when (and node-is-sync-node result-from-children) (setq err t))
      (when err (sync-error 'same-thread ref-node))
      (if node-is-sync-node
          (cons bt-node result-from-children)
          result-from-children))))



;;; ===================== I/O (Part 1)====================

;;; Code to set input and output partners
;;; Each input node must have at least one output partner in a parallel thread.
;;; In addition, the input node may have other input nodes as partners
;;; in parallel threads.
;;; An output node may have input nodes as partners in parallel threads.

(defun set-io-partners (bt-node)
  (cond ((and (eq (bt-node-type bt-node) 'input)
              (not (eq (bt-node-flag bt-node) 'kill)))
         ;; for input node, we look for both input and output partners
         (setf (bt-node-input-partners bt-node)
               (collect-input-partners bt-node))
         (setf (bt-node-output-partners bt-node)
               (collect-output-partners bt-node))
         (when (null (bt-node-output-partners bt-node))
           (output-partner-error bt-node)))
        ((and (eq (bt-node-type bt-node) 'output)
              (not (eq (bt-node-flag bt-node) 'kill)))
         ;; for output node, we only look for input partners
         (setf (bt-node-input-partners bt-node)
               (collect-input-partners bt-node))))
  (loop for node in (bt-node-children bt-node)
        do (set-io-partners node)))

(defun collect-output-partners (bt-node)
  ;; go up the tree
  (collect-output-partners-aux
    (bt-node-parent bt-node) bt-node (bt-node-event bt-node)))

(defun collect-output-partners-aux (node child-node event)
  (cond ((null node) nil)
        ;; for parallel branching,
        ;; need to search children of node except child-node
        ;; in addition, also need to go up the tree
        ((eq (bt-node-branch-type node) 'parallel)
         (append
           (loop for n in (bt-node-children node)
                 append (cond ((eq n child-node) nil)
                              (t (collect-output-partners-aux-aux n event))))
           (collect-output-partners-aux (bt-node-parent node) node event)))
        ;; not a parallel-branching node, keep going up the tree
        (t
         (collect-output-partners-aux (bt-node-parent node) node event))))

(defun collect-output-partners-aux-aux (node event)
  (cond ((and (eq (bt-node-type node) 'output)
              (eq (bt-node-event node) event)
              (not (eq (bt-node-flag node) 'kill)))
         (cons node
               (loop for n in (bt-node-children node)
                     append (collect-output-partners-aux-aux n event))))
        (t (loop for n in (bt-node-children node)
                 append (collect-output-partners-aux-aux n event)))))

(defun collect-input-partners (bt-node)
  ;; go up the tree
  (collect-input-partners-aux
    (bt-node-parent bt-node) bt-node (bt-node-event bt-node)))

(defun collect-input-partners-aux (node child-node event)
  (cond ((null node) nil)
        ;; for parallel branching,
        ;; need to search children of node except child-node
        ;; in addition, also need to go up the tree
        ((eq (bt-node-branch-type node) 'parallel)
         (append
           (loop for n in (bt-node-children node)
                 append (cond ((eq n child-node) nil)
                              (t (collect-input-partners-aux-aux n event))))
           (collect-input-partners-aux (bt-node-parent node) node event)))
        ;; not a parallel-branching node, keep going up the tree
        (t
         (collect-input-partners-aux (bt-node-parent node) node event))))

(defun collect-input-partners-aux-aux (node event)
  (cond ((and (eq (bt-node-type node) 'input)
              (eq (bt-node-event node) event)
              (not (eq (bt-node-flag node) 'kill)))
         (cons node
               (loop for n in (bt-node-children node)
                     append (collect-input-partners-aux-aux n event))))
        (t (loop for n in (bt-node-children node)
                 append (collect-input-partners-aux-aux n event)))))



;;; ===================== Defaults ====================

;;; Code to assign default conditions and effects to BT nodes.

(defun assign-default-condition-and-effects (bt-node)
  ;; default pc-effects apply to all BT nodes
  (setf (bt-node-pc-effects bt-node)
        (insert-effect (make-effect (bt-node-pc bt-node)
                                    (bt-node-pc-next bt-node))
                    *empty-effects*))
  (let ((node-type (bt-node-type bt-node)))
    ;; default condition and main-effects depend on
    ;; node class
    (cond ((eq node-type 'update)
           ;; update node class
           (setf (bt-node-condition bt-node) *true*)
           (setf (bt-node-main-effects bt-node)
                 (insert-effect (make-effect (bt-node-l-value bt-node)
                                             (bt-node-r-value bt-node))
                                *empty-effects*)))
          ((or (eq node-type 'guard) (eq node-type 'select))
           ;; guard node class
           (setf (bt-node-condition bt-node) (bt-node-r-value bt-node))
           (setf (bt-node-main-effects bt-node) *empty-effects*))
          ((or (eq node-type 'event)
               (eq node-type 'input)
               (eq node-type 'output))
           ;; event node class
           (setf (bt-node-condition bt-node) *true*)
           (setf (bt-node-main-effects bt-node) *empty-effects*))
          (t
           (setf (bt-node-condition bt-node) *true*)
           (setf (bt-node-main-effects bt-node) *empty-effects*))))
  (loop for node in (bt-node-children bt-node)
        do (assign-default-condition-and-effects node)))



;;; ============================ Branching ===========================
;;; ================ Control Flow Directives (Part 2) ================

;;; modify-condition-and-effects handles branching,
;;; kill, reversion & reference

(defun modify-condition-and-effects (bt-node)
  (when (eq (bt-node-branch-type bt-node) 'parallel)
    (setf (bt-node-pc-effects bt-node)
          (override-effects
            (bt-node-pc-effects bt-node)
            (parallel-branch-effects (bt-node-children bt-node)))))
  (let ((flag (bt-node-flag bt-node)))
    (cond ((eq flag 'kill)
           ;; kill target, there may be active subthreads
           (setf (bt-node-pc-effects bt-node)
                 (override-effects
                   (bt-node-pc-effects bt-node)
                   (kill-effects (bt-node-target bt-node))))
           ;; also remove "behaviour" from node
           ;; note that generate-blocks takes care of
           ;; the extra "behaviour" of selection
           (setf (bt-node-condition bt-node) *true*)
           (setf (bt-node-main-effects bt-node) *empty-effects*))
          ((eq flag 'reversion)
           ;; revert to target
           ;; after killing target (including subthreads)
           ;; however, "behaviour" is performed at the node.
           ;; node inherits pc-effects of target
           (cond ((not (bt-node-atomic (bt-node-target bt-node)))
                  (setf (bt-node-pc-effects bt-node)
                        (override-effects
                          (override-effects
                            (bt-node-pc-effects bt-node)
                            (kill-effects (bt-node-target bt-node)))
                          (bt-node-pc-effects (bt-node-target bt-node)))))
                 ((car (bt-node-children (bt-node-target bt-node)))
                  ;; case where target is an atomic composition
                  ;; copy the rest of the composition in the target
                  (let ((child (copy-atomic (car (bt-node-children
                                                   (bt-node-target bt-node))))))
                    (setf (bt-node-atomic bt-node) t)
                    (setf (bt-node-children bt-node) (list child))
                    (setf (bt-node-parent child) bt-node)
                    (let ((leaf (find-leaf child)))
                      (setf (bt-node-pc-effects leaf)
                            (override-effects
                              (kill-effects (bt-node-target bt-node))
                              (bt-node-pc-effects leaf)))))))
          )
          ((and (eq flag 'reference)
                (bt-node-p (bt-node-target bt-node)))
           ;; go to target
           ;; in this case simply inherit pc-effects of target,
           ;; since main-effects already the same as target's
           (setf (bt-node-pc-effects bt-node)
                 (override-effects
                   (bt-node-pc-effects bt-node)
                   (bt-node-pc-effects (bt-node-target bt-node)))))))
  (loop for node in (bt-node-children bt-node)
        do (modify-condition-and-effects node)))

(defun copy-atomic (bt-node)
  (let ((new-node (make-bt-node (bt-node-type bt-node))))
    (setf (bt-node-requirement-label new-node)
          (bt-node-requirement-label bt-node))
    (setf (bt-node-requirement-status new-node)
          (bt-node-requirement-status bt-node))
    (setf (bt-node-component new-node) (bt-node-component bt-node))
    (setf (bt-node-behaviour new-node) (bt-node-behaviour bt-node))
    (setf (bt-node-behaviour-type new-node)
          (bt-node-behaviour-type bt-node))
    (setf (bt-node-flag new-node) (bt-node-flag bt-node))
    (setf (bt-node-label new-node) (bt-node-label bt-node))
    (setf (bt-node-branch-type new-node)
          (bt-node-branch-type bt-node))
    (setf (bt-node-atomic new-node) (bt-node-atomic bt-node))
    (setf (bt-node-l-value new-node) (bt-node-l-value bt-node))
    (setf (bt-node-r-value new-node) (bt-node-r-value bt-node))
    (setf (bt-node-event new-node) (bt-node-event bt-node))
    (setf (bt-node-condition new-node) (bt-node-condition bt-node))
    ;; need copies of structs
    (setf (bt-node-pc-effects new-node)
          (copy-effects (bt-node-pc-effects bt-node)))
    (setf (bt-node-main-effects new-node)
          (copy-effects (bt-node-main-effects bt-node)))
    (unless (null (bt-node-atomic bt-node))
      (setf (bt-node-children new-node)
            (loop for node in (bt-node-children bt-node)
                  collect
                  (let ((child (copy-atomic node)))
                    (setf (bt-node-parent child) new-node)
                    child))))
    new-node))

(defun copy-effects (effects)
  (loop for effect in effects
        collect (make-effect (effect-l-value effect) (effect-r-value effect))))

(defun find-leaf (bt-node)
  (if (null (bt-node-children bt-node))
      bt-node
      (find-leaf (car (bt-node-children bt-node)))))



;;; Function to collect effects of killing bt-node.
;;; It includes killing the subthreads.

(defun kill-effects (bt-node)
  (let ((effects (insert-effect (make-effect (bt-node-pc bt-node) 0)
                                *empty-effects*))
        (node bt-node))
    ;; can safely skip while non-branching
    (loop while (and (null (bt-node-branch-type node))
                     (not (null (bt-node-children node))))
          do (setq node (car (bt-node-children node))))
    (cond ((eq (bt-node-branch-type node) 'parallel)
           (loop for n in (bt-node-children node)
                 do (setq effects
                          (override-effects effects (kill-effects n)))))
          ((eq (bt-node-branch-type node) 'alternative)
           (loop for n in (bt-node-children node)
                 do (setq effects
                         (override-effects
                            effects (kill-effects-aux n))))))
    effects))

(defun kill-effects-aux (bt-node)
  (let ((effects *empty-effects*)
        (node bt-node))
    ;; can safely skip while non-branching
    (loop while (and (null (bt-node-branch-type node))
                     (not (null (bt-node-children node))))
          do (setq node (car (bt-node-children node))))
    (cond ((eq (bt-node-branch-type node) 'parallel)
          (loop for n in (bt-node-children node)
                do (setq effects
                         (override-effects effects (kill-effects n)))))
          ((eq (bt-node-branch-type node) 'alternative)
           (loop for n in (bt-node-children node)
                 do (setq effects
                         (override-effects
                            effects (kill-effects-aux n))))))
    effects))

(defun parallel-branch-effects (children)
  (let ((effects *empty-effects*))
    (loop for node in children
          do (setq effects
                   (override-effects
                      effects
                      (insert-effect (make-effect (bt-node-pc node) 1)
                                     *empty-effects*))))
    effects))



;;; ===================== Atomic Composition ====================

;;; generate-blocks generates the elementary blocks from nodes
;;; normally, one BT node becomes one elementary block
;;; however, BT nodes in an atomic composition become a single block

;;; Because of combined alternative branching and atomic composition,
;;; a BT node may correspond to multiple blocks, thus we
;;; return a list of blocks.

(defun generate-blocks (bt-node)
  (let ((block-children
          (loop for node in (bt-node-children bt-node)
                append (generate-blocks node))))
    (cond ((bt-node-atomic bt-node)
           ;; Note that node can not have parallel branching
           ;; and be atomic at the same time
           (let ((blocks (loop for b in block-children
                               collect
                               (merge-atomic-block bt-node b))))
             (setf (bt-node-blocks bt-node) blocks)
             blocks))
          (t (let ((block
                     (make-bt-block
                       (list bt-node)
                       block-children
                       (and (eq (bt-node-type bt-node) 'select)
                            (not (eq (bt-node-flag bt-node) 'kill)))
                       (and (eq (bt-node-flag bt-node) 'synchronise)
                            (bt-node-target bt-node))
                       (and (or (eq (bt-node-type bt-node) 'input)
                                (eq (bt-node-type bt-node) 'output))
                            (bt-node-input-partners bt-node))
                       (and (eq (bt-node-type bt-node) 'input)
                            (bt-node-output-partners bt-node))
                       (bt-node-condition bt-node)
                       (bt-node-event bt-node)
                       (bt-node-pc bt-node)
                       (bt-node-pc-value bt-node)
                       (bt-node-pc-effects bt-node)
                       (bt-node-main-effects bt-node)
                       nil)))
                (loop for b in block-children
                      do (setf (bt-block-parent b) block))
                (setf (bt-node-blocks bt-node) (list block))
                (list block))))))

(defun merge-atomic-block (bt-node block)
  (let ((merged-block block))
    ;; note that the nodes merged have the same PC
    ;; the PC value comes from the first node
    (setf (bt-block-bt-nodes merged-block)
          (cons bt-node (bt-block-bt-nodes merged-block)))
    (when (and (eq (bt-node-type bt-node) 'select)
               (not (eq (bt-node-flag bt-node) 'kill)))
      (setf (bt-block-select merged-block) t))
    (when (eq (bt-node-flag bt-node) 'synchronise)
      ;; check for more than 1 synchronise in the block?
      (setf (bt-block-synchronise merged-block)
            (bt-node-target bt-node)))
    (when (eq (bt-node-type bt-node) 'input)
      (setf (bt-block-input merged-block)
            (bt-node-input-partners bt-node))
      (setf (bt-block-output merged-block)
            (bt-node-output-partners bt-node)))
    (when (eq (bt-node-type bt-node) 'output)
      (setf (bt-block-input merged-block)
            (bt-node-input-partners bt-node)))
    ;; need substitution on second conjunct using effects
    ;; need only use main effects of bt-node
    (setf (bt-block-condition merged-block)
          (make-conjunction
            (bt-node-condition bt-node)
            (sublis-equal
              (loop for effect in (bt-node-main-effects bt-node)
                    collect (cons (effect-l-value effect)
                                  (effect-r-value effect)))
              (bt-block-condition merged-block))))

    ;; ***** for now
    (setf (bt-block-event merged-block)
          (or (bt-block-event merged-block) (bt-node-event bt-node)))

    (setf (bt-block-pc merged-block)
          (bt-node-pc bt-node))
    (setf (bt-block-pc-value merged-block)
          (bt-node-pc-value bt-node))
    (setf (bt-block-pc-effects merged-block)
          (override-effects
            (bt-node-pc-effects bt-node)
            (bt-block-pc-effects merged-block)))
    (setf (bt-block-main-effects merged-block)
          (override-effects
            (bt-node-main-effects bt-node)
            (bt-block-main-effects merged-block)))
    merged-block))



;;; ===================== Reassign PC Values ========================



(defstruct (pc-values (:constructor make-pc-values (name values))
                      :named :predicate)
           name values)

(defun list-of-bt-blocks (blocks)
  (append blocks (loop for block in blocks
                       append (list-of-bt-blocks (bt-block-children block)))))

(defun collect-pc-names (block-root)
  (let ((collected nil))
    (loop for bt-block in (list-of-bt-blocks block-root)
          do
          (let ((pc (bt-block-pc bt-block)))
            (unless (member-equal pc collected)
              (setq collected (cons pc collected)))))
    (reverse collected)))

(defun insert-pc-value (value values)
  (cond ((member-equal value values) values)
        (t (insert-pc-value-aux value nil values))))

(defun insert-pc-value-aux (value prefix values)
  (cond ((or (null values) (< value (car values)))
         (append prefix (cons value values)))
        (t (insert-pc-value-aux value
                                (append prefix (list (car values)))
                                (cdr values)))))

(defun insert-into-pc-values (pc value pc-values)
  (loop for entry in pc-values
        when (eq (pc-values-name entry) pc)
        do (setf (pc-values-values entry)
                 (insert-pc-value value (pc-values-values entry)))))

(defun collect-pc-values (block-root)
  (let ((collected (mapcar #'(lambda (x) (make-pc-values x (list 0)))
                           (collect-pc-names block-root))))
    (loop for bt-block in (list-of-bt-blocks block-root)
          do
          (progn
            (insert-into-pc-values (bt-block-pc bt-block)
                                   (bt-block-pc-value bt-block)
                                   collected)
            (loop for effect in (bt-block-pc-effects bt-block)
                  do (insert-into-pc-values (effect-l-value effect)
                                            (effect-r-value effect)
                                            collected))))
    collected))

;;; Function to call to reassign PC values (to minimize the encoding).

(defun reassign-pc-values (block-root)
  (loop for entry in (collect-pc-values block-root)
        do
        (let ((pc (pc-values-name entry))
              (values (pc-values-values entry)))
          (let ((alist (loop for value in values
                             for i from 0 by 1
                             collect (cons value i))))
            (reassign-pc-values-aux pc alist block-root)))))

(defun reassign-pc-values-aux (pc alist block-root)
  (loop for bt-block in (list-of-bt-blocks block-root)
        do
        (progn
          (when (eq (bt-block-pc bt-block) pc)
            (setf (bt-block-pc-value bt-block)
                  (sublis-equal alist (bt-block-pc-value bt-block))))
          (loop for effect in (bt-block-pc-effects bt-block)
                when (eq (effect-l-value effect) pc)
                do (setf (effect-r-value effect)
                         (sublis-equal alist (effect-r-value effect)))))))



;;; =================== Synchronization (Part 2) ====================

(defun process-synchronisation (block)
  (when (and (not (null (bt-block-synchronise block)))
             (> (length (bt-block-synchronise block)) 1)
             (bt-node-p (car (bt-block-synchronise block))))
    (let ((pc-effects *empty-effects*)
          (main-effects *empty-effects*)
          (condition *true*)
          (event nil)
          (blocks nil))
      (loop for node in (bt-block-synchronise block)
            do
            (let ((b (car (bt-node-blocks node))))
              (when (> (length (bt-node-blocks node)) 1)
                (synchronisation-error 'multiple node))
              (when (conflicting-effects (bt-block-pc-effects b)
                                         pc-effects)
                (synchronisation-error 'pc-effects node))
              ;; if there is no conflict, override is equivalent to union
              (setq pc-effects
                    (override-effects pc-effects
                                      (bt-block-pc-effects b)))
              (when (conflicting-effects (bt-block-main-effects b)
                                         main-effects)
                (synchronisation-error 'main-effects node))
              ;; if there is no conflict, override is equivalent to union
              (setq main-effects
                    (override-effects main-effects
                                      (bt-block-main-effects b)))
              (setq condition
                    (make-conjunction
                       (bt-block-condition b)
                       (make-conjunction `(= ,(bt-block-pc b)
                                             ,(bt-block-pc-value b))
                                         condition)))
              (cond ((null event) (setq event (bt-block-event b)))
                    ((and (not (null (bt-block-event b)))
                          (not (eq event (bt-block-event b))))
                     (synchronisation-error 'event node)))
              (setq blocks (append blocks (list b)))))
      (loop for b in blocks
            do (progn
                 (setf (bt-block-pc-effects b) pc-effects)
                 (setf (bt-block-main-effects b) main-effects)
                 (setf (bt-block-condition b) condition)
                 (setf (bt-block-event b) event)
                 (setf (bt-block-synchronise b) blocks)))))
  (loop for b in (bt-block-children block)
        do (process-synchronisation b)))



;;; ===================== Selection ====================

;;; Currently an "else" selection block is created even if
;;; the selections are already exhaustive.  *** May want to
;;; check for exhaustive selections and not produce an "else"
;;; if already exhaustive (may need a simplifier).

(defun process-selection (block-list)
  (when (not (null block-list))
    (let ((result nil))
      (cond ((blocks-have-same-pc block-list)
             (when (blocks-have-selection block-list)
               (cond ((block-has-else-selection (car (last block-list)))
                      (let ((condition *true*))
                        (loop for b in (butlast block-list)
                              do (setq condition
                                       (make-conjunction
                                         condition
                                         (make-not (bt-block-condition b)))))
                        (setf (bt-block-condition (car (last block-list)))
                              condition)))
                     (t
                      (let ((block (make-bt-block
                                     (bt-block-bt-nodes (car block-list))
                                     nil
                                     nil
                                     nil
                                     nil
                                     nil
                                     *true*
                                     nil
                                     (bt-block-pc (car block-list))
                                     (bt-block-pc-value (car block-list))
                                     (insert-effect
                                       (make-effect
                                         (bt-block-pc (car block-list))
                                         0)
                                       *empty-effects*)
                                     *empty-effects*
                                     t))
                            (condition *true*)
                            (parent (bt-block-parent (car block-list))))
                         ;; need to collect negation of condition and
                         ;; take the conjunction
                         (loop for b in block-list
                               do (setq condition
                                        (make-conjunction
                                          condition
                                          (make-not (bt-block-condition b)))))
                         (setf (bt-block-condition block) condition)
                         (setf (bt-block-parent block) parent)
                         (cond ((bt-block-p parent)
                                (setf (bt-block-children parent)
                                      (append block-list (list block))))
                               (t (setq result (list block)))))))))
            (t (let ((blocks nil))
                 (loop for b in block-list
                       do
                       (cond ((bt-block-select b)
                              ;; selection in a parallel branch
                              (let ((block
                                     (make-bt-block
                                       (bt-block-bt-nodes b)
                                       nil
                                       nil
                                       nil
                                       nil
                                       nil
                                       (make-not (bt-block-condition b))
                                       nil
                                       (bt-block-pc b)
                                       (bt-block-pc-value b)
                                       (insert-effect
                                         (make-effect (bt-block-pc b) 0)
                                         *empty-effects*)
                                       *empty-effects*
                                       t)))
                                (setq blocks (append blocks (list b block)))
                                (setf (bt-block-parent block)
                                      (bt-block-parent b))))
                               (t (setq blocks (append blocks (list b))))))
                 (when (not (null blocks))
                    (let ((parent (bt-block-parent (car block-list))))
                       (when (bt-block-p parent)
                         ;; should always be the case that b has a parent
                         (setf (bt-block-children parent) blocks)))))))
      (loop for b in block-list
            do (process-selection (bt-block-children b)))
      result)))

(defun blocks-have-same-pc (block-list)
  (let ((pc (bt-block-pc (car block-list)))
        (result t))
    (loop for b in (cdr block-list)
          do (when (not (eq (bt-block-pc b) pc))
               (setq result nil)))
    result))

(defun blocks-have-selection (block-list)
  (loop for b in block-list
        do (when (bt-block-select b) (return t))))

(defun block-has-else-selection (bt-block)
  (loop for bt-node in (bt-block-bt-nodes bt-block)
        do (when (bt-node-is-else-selection bt-node) (return t))))




;;; ===================== I/O (Part 2) ====================

;;; process internal inputs and internal outputs.
;;; For each block that has an input or output,
;;; create a list of conditional effects.

(defun process-io (block)
  (cond ((bt-block-output block)
         ;; block is definitely an input block (it has output partners)
         (let ((output-blocks (collect-blocks (bt-block-output block)))
               (input-blocks (collect-blocks (bt-block-input block))))
           (process-input-block block output-blocks input-blocks)))
        ((bt-block-input block)
         ;; block is most likely an output block (unless there is
         ;; an error whereby some input blocks do not have an
         ;; output partner)
         (let ((input-blocks (collect-blocks (bt-block-input block))))
           (process-output-block block input-blocks))))
  (loop for b in (bt-block-children block)
        do (process-io b)))

;;; For an input block, there are m * 2^n cases, where m is the number
;;; of output partners and n is the number of input partners

(defun process-input-block (block output-blocks input-blocks)
  (let ((condition (bt-block-condition block))
        (effects (override-effects
                    (bt-block-pc-effects block)
                    (bt-block-main-effects block))))
    (setf (bt-block-conditional-effects block)
          (loop for b in output-blocks
                append
                (let ((c (make-conjunction
                           (make-conjunction
                             condition
                             (bt-block-condition b))
                           `(= ,(bt-block-pc b)
                               ,(bt-block-pc-value b)))))
                   (when (conflicting-effects (bt-block-pc-effects b)
                                              effects)
                     (io-error 'pc-effects b))
                   (when (conflicting-effects (bt-block-main-effects b)
                                              effects)
                     (io-error 'main-effects b))
                   (collect-conditional-effects
                     c
                     (override-effects
                       (override-effects
                         effects (bt-block-pc-effects b))
                       (bt-block-main-effects b))
                     input-blocks))))))

;;; For an output block, there are 2^n cases, where n is the number
;;; of input block partners

(defun process-output-block (block input-blocks)
  (let ((condition (bt-block-condition block))
        (effects (override-effects
                    (bt-block-pc-effects block)
                    (bt-block-main-effects block))))
  (setf (bt-block-conditional-effects block)
        (collect-conditional-effects condition effects input-blocks))))

(defun collect-conditional-effects (condition effects input-blocks)
  (let ((cmbs (combinations input-blocks)))
   (loop for cmb in cmbs
         collect
         (let ((c condition)
               (e effects))
           (loop for pair in cmb
                 do
                 (let ((b (car pair)))
                   (cond ((= (cdr pair) 1)
                          (setq c
                                (make-conjunction
                                 c
                                 (make-conjunction
                                  (bt-block-condition b)
                                  `(= ,(bt-block-pc b)
                                      ,(bt-block-pc-value b)))))
                          (when (conflicting-effects
                                  (bt-block-pc-effects b)
                                  e)
                            (io-error 'pc-effects b))
                          (setq e
                                (override-effects
                                  e
                                  (bt-block-pc-effects b)))
                          (when (conflicting-effects
                                  (bt-block-main-effects b)
                                  e)
                            (io-error 'main-effects b))
                          (setq e
                                (override-effects
                                  e
                                  (bt-block-main-effects b))))
                         (t
                          (setq c
                                (make-conjunction
                                 c
                                 (make-not
                                  (make-conjunction
                                   (bt-block-condition b)
                                   `(= ,(bt-block-pc b)
                                       ,(bt-block-pc-value b))))))))))
          (make-conditional-effects c e)))))


(defun collect-blocks (nodes)
  (loop for n in nodes
        append (bt-node-blocks n)))





;;; ===================== Prioritization ====================

;;; Collect prioritization info.  This is done whether or not
;;; prioritization is set, since it only collects the information
;;; and doesn't actually modify conditions to take into account
;;; prioritization.  

(defun collect-prioritized-conditions (block-root)
  (let ((conditions (collect-prioritized-conditions-aux block-root)))
    (cond ((null conditions) *false*)
          ((= (length conditions) 1) (car conditions))
          (t (cons 'or conditions)))))

(defun collect-prioritized-conditions-aux (blocks)
  (loop for block in blocks
        append (collect-prioritized-conditions-aux-aux block)))

;;; ****** must include conditions from conditional-effects

(defun collect-prioritized-conditions-aux-aux (block)
  (cond ((or (null (bt-block-event block))
             ;(input-block-p block)
             (output-block-p block))
         (cons (construct-block-condition block)
               (collect-prioritized-conditions-aux (bt-block-children block))))
        (t (collect-prioritized-conditions-aux (bt-block-children block)))))

(defun construct-block-condition (block)
  (let ((collected-conditions
          (loop for ce in (bt-block-conditional-effects block)
                collect (conditional-effects-condition ce))))
    (cond ((null collected-conditions)
           (make-conjunction
             `(= ,(bt-block-pc block) ,(bt-block-pc-value block))
             (bt-block-condition block)))
          ((= (length collected-conditions) 1)
           (make-conjunction
             `(= ,(bt-block-pc block) ,(bt-block-pc-value block))
             (make-conjunction (bt-block-condition block)
                               (car collected-conditions))))
          (t
           (make-conjunction
             `(= ,(bt-block-pc block) ,(bt-block-pc-value block))
             (make-conjunction (bt-block-condition block)
                               (cons 'or collected-conditions)))))))

(defun input-block-p (block)
  (let ((result nil))
    (loop for node in (bt-block-bt-nodes block)
          when (eq (bt-node-type node) 'input)
          do (setq result t))
    result))

(defun output-block-p (block)
  (let ((result nil))
    (loop for node in (bt-block-bt-nodes block)
          ;;when (eq (bt-node-type node) 'output)
          when (or (eq (bt-node-type node) 'output)
                   (eq (bt-node-behaviour-type node) 'eo-token))
          do (setq result t))
    result))



;;; Code to modify conditions because of prioritization of
;;; internal transitions (transitions that do not involve
;;; external events).

;;; Only call this function if prioritizing internal transitions.

(defun modify-external-events (block-root)
  (let ((condition (make-not (collect-prioritized-conditions block-root))))
    (modify-external-events-aux block-root condition)))

(defun modify-external-events-aux (blocks condition)
  (loop for block in blocks
        do (modify-external-events-aux-aux block condition)))

(defun modify-external-events-aux-aux (block condition)
  (unless (or (null (bt-block-event block))
              (input-block-p block)
              (output-block-p block))
    (setf (bt-block-condition block)
          (make-conjunction (bt-block-condition block)
                            condition)))
  (modify-external-events-aux (bt-block-children block) condition))
    



;;; =================== Error Messages ===================

(defun source-error (type bt-node)
  (setq *fatal-error* t)
  (format *error-output* "~%Source error (~A):~%" type)
  (print-bt-node bt-node))

(defun target-error (type bt-node)
  (setq *fatal-error* t)
  (format *error-output* "~%Target error (~A):~%" type)
  (print-bt-node bt-node))

(defun sync-error (error-type bt-node)
  (setq *fatal-error* t)
  (format *error-output* "~%Synchronisation error (~A):~%" error-type)
  (print-bt-node bt-node))

(defun output-partner-error (bt-node)
  (setq *fatal-error* t)
  (format *error-output* "~%Input node error (No corresponding output node):~%")
  (print-bt-node bt-node))


(defun wf-error (error-type args)
  (setq *fatal-error* t)
  (cond ((eq error-type 'multiple-events)
         (format *error-output*
                 "WF error (multiple events in atomic composition).~%")
         (format *error-output* "~A and ~A.~%" (first args) (second args)))
        (t (format *error-output* "WF error (~A).~%" error-type))))

(defun synchronisation-error (type bt-node)
  (setq *fatal-error* t)
  (format *error-output* "~%Synchronisation error (~A):~%" type)
  (print-bt-node bt-node))

(defun io-error (type block)
  (setq *fatal-error* t)
  (format *error-output* "~%IO error (~A):~%" type)
  (print-bt-block block))



(defun print-block-tree (block)
  (print-bt-block block)
  (loop for b in (bt-block-children block)
        do (print-block-tree b)))


(defun print-bt-block (block)
  (format t "~%===== Block =====~%")
  (loop for n in (bt-block-bt-nodes block)
        do (print-bt-node n))
  (format t "~%===== End BT Node =====~%")
  (format t "Index: ~A~%" (bt-block-index block))
  (when (bt-block-special block)
    (format t "SELECTION ELSE~%"))
  (format t "Condition: ~A~%" (bt-block-condition block))
  (format t "PC Effects: ~A~%" (bt-block-pc-effects block))
  (format t "Main Effects: ~A~%" (bt-block-main-effects block))
  (when (bt-block-conditional-effects block)
    (format t "Conditional Effects:~%~A~%"
            (bt-block-conditional-effects block)))
  (let ((prioritized (length (bt-block-prioritized-blocks block))))
    (when (>= prioritized 1)
      (format t "Number of prioritize blocks: ~A~%" prioritized)))
  (when (bt-block-select block)
    (format t "Selection~%"))
  (format t "Number of Children: ~A~%"
          (length (bt-block-children block)))
  (format t "PC: ~A ~A~%"
          (bt-block-pc block) (bt-block-pc-value block)))

(defun print-bt-block-summary (block)
  (format t "~%-----~%")
  (when (bt-block-special block)
    (format t "SELECTION ELSE~%---~%"))
  (loop for n in (bt-block-bt-nodes block)
        do (print-bt-node-summary n))
  (format t "-----~%"))


(defvar *number-of-blocks* 0)

(defun assign-indices-to-blocks (block-root)
  (setq *number-of-blocks* 0)
  (assign-indices-to-blocks-aux block-root)
  (let ((bt-block-array (make-array *number-of-blocks*)))
    (set-block-array-indices block-root bt-block-array)
    bt-block-array))

(defun assign-indices-to-blocks-aux (blocks)
  (loop for block in blocks
        do (progn (setf (bt-block-index block) *number-of-blocks*)
                  (incf *number-of-blocks*)
                  (assign-indices-to-blocks-aux
                    (bt-block-children block)))))

(defun set-block-array-indices (blocks bt-block-array)
  (loop for block in blocks
        do (progn (setf (aref bt-block-array (bt-block-index block))
                        block)
                  (set-block-array-indices
                    (bt-block-children block) bt-block-array))))

;;; translating BT to elementary blocks
;;; (after expansion of references and assigning PCs)

(defun process-conditions-and-effects (bt-node)
  ;; assign defaults
  (assign-default-condition-and-effects bt-node)
  ;; branches, control flow directives
  (modify-condition-and-effects bt-node)
  ;; atomic compositions
  (let ((block-root (generate-blocks bt-node)))
    ;; reassign pc values to make the domains contiguous
    (reassign-pc-values block-root)
    ;; synchronization
    (loop for block in block-root
          do (process-synchronisation block))
    ;; selection
    (setq block-root (append block-root (process-selection block-root)))
    (let ((bt-block-array (assign-indices-to-blocks block-root)))
      ;; i/o
      (loop for block in block-root
            do (process-io block))
      ;; return block-root and bt-block-array
      (list block-root bt-block-array))))



;;; ===================== Binary Encoding =======================

;;; Structure for state variable info.
;;; A state variable info is simply a pair:
;;; - name: the name of the state variable,
;;; - values: a list of possible values for the state variable.

(defstruct (state-var (:constructor make-state-var (name values))
                   :named :predicate)
           name values)

;;; Function to collect state variable information.
;;; We are intested in the set of possible values for each state variable.

(defun collect-state-vars (bt-node)
  (let ((lr-pairs (collect-l-value-r-value-pairs bt-node))
        (state-vars nil))
    (loop for pair in lr-pairs
          do (let ((found nil)
                   (name (car pair)))
                (loop for sv in state-vars
                      when (equal name (state-var-name sv))
                      do (setq found sv))
                (cond ((null found)
                       (setq state-vars
                             (append state-vars
                                     (list (make-state-var
                                              name
                                              (list (cdr pair)))))))
                      (t
                       (let ((exists nil)
                             (value (cdr pair)))
                         (loop for val in (state-var-values found)
                               when (equal value val)
                               do (setq exists t))
                         (when (null exists)
                           (setf (state-var-values found)
                                 (append (state-var-values found)
                                         (list value)))))))))
    state-vars))

;;; Functions to collect l-value - r-value pairs from BT.
;;; The result may contain duplicates and is not necessarily sorted.

(defun collect-l-value-r-value-pairs (bt-node)
  (loop for entry in (collect-l-value-r-value-pairs-aux bt-node)
        when (not (equal entry '(nil)))
        collect
        (if (null (car entry))
            (cond ((not-p (cdr entry))
                   (cons (second (not-expr (cdr entry)))
                         (third (not-expr (cdr entry)))))
                   (t (cons (third entry) (fourth entry))))
            entry)))

(defun collect-l-value-r-value-pairs-aux (bt-node)
  (cons (cons (bt-node-l-value bt-node) (bt-node-r-value bt-node))
        (loop for node in (bt-node-children bt-node)
              append (collect-l-value-r-value-pairs-aux node))))



;;; Structure for PC info.
;;; A PC info is simply a pair:
;;; - name: the name of the PC,
;;; - max-value: the maximum value for the PC.

(defstruct (pc-info (:constructor make-pc-info (name max-value))
                   :named :predicate)
           name max-value)

;;; Function to collect state PC information.
;;; We are intested in the maximum possible value for each PC.

(defun collect-pc-info (bt-block-array)
  (let ((collected nil))
    (loop for i from 0 to (- (length bt-block-array) 1)
          do
          (let ((bt-block (aref bt-block-array i)))
            (let ((pc (bt-block-pc bt-block))
                  (pc-value (bt-block-pc-value bt-block))
                  (found nil))
              (loop for effect in (bt-block-pc-effects bt-block)
                    when (and (eq (effect-l-value effect) pc)
                              (> (effect-r-value effect) pc-value))
                    do (setq pc-value (effect-r-value effect)))
              (loop for info in collected
                    when (eq (pc-info-name info) pc)
                    do (progn (setq found t)
                              (when (> pc-value (pc-info-max-value info))
                                (setf (pc-info-max-value info) pc-value))))
              (unless found
                (setq collected
                      (cons (make-pc-info pc pc-value) collected))))))
    (reverse collected)))



;;; Function to collect events

(defun collect-events (bt-node)
  (reverse (collect-events-aux bt-node nil)))

(defun collect-events-aux (bt-node collected-events)
  (let ((event (bt-node-event bt-node))
        (collected collected-events))
    (unless (or (null event) (member-eq event collected-events))
      (setq collected (cons event collected)))
    (loop for node in (bt-node-children bt-node)
          do (setq collected (collect-events-aux node collected)))
    collected))


(defun state-variable-name-p (symbol)
  (and (symbolp symbol)
       (let ((str (string symbol)))
         (and (> (string-length str) 3)
              (string= "SV-" (substring str 0 3))))))

(defun pc-name-p (symbol)
  (and (symbolp symbol)
       (let ((str (string symbol)))
         (and (> (string-length str) 2)
              (string= "PC" (substring str 0 2))))))

(defun size-in-bits (n)
  (if (>= n 2)
      (+ 1 (size-in-bits (ash n -1)))
      1))



;;; Based on state variables (components and attributes),
;;; PC values and events, generate the atoms that can be
;;; used to binary-encode values.

(defun generate-atoms (pc-infos state-vars)
  (let ((atoms nil))
    (loop for pc in pc-infos
          do (let* ((name (pc-info-name pc))
                    (size (size-in-bits (pc-info-max-value pc)))
                    (pc-names (loop for i = 0 then (+ i 1)
                                    while (< i size)
                                    collect
                                    (make-name
                                      (list  name "-" (format nil "~A" i))))))
               (setq atoms (append atoms pc-names))))
    (loop for sv in state-vars
          do (let* ((name (state-var-name sv))
                    (size (size-in-bits (- (length (state-var-values sv)) 1)))
                    (sv-names (loop for i = 0 then (+ i 1)
                                    while (< i size)
                                    collect
                                    (make-name
                                      (list name "-" (format nil "~A" i))))))
               (setq atoms (append atoms sv-names))))
    atoms))



;;; Encode a PC value (as a conjunction of unnegated/negated atoms).

(defun pc-value-to-atoms (pc-infos name value)
  (let* ((pc (loop for pc-info in pc-infos
                   when (eq (pc-info-name pc-info) name)
                   do (return pc-info)))
         (size (size-in-bits (pc-info-max-value pc)))
         (pc-names (loop for i = 0 then (+ i 1)
                         while (< i size)
                         collect
                         (make-name (list  name "-" (format nil "~A" i)))))
         (conjuncts (loop for pc-name in pc-names
                          for v = value then (ash v -1)
                          collect (if (zerop (mod v 2))
                                      (list 'not pc-name)
                                      pc-name))))
    (if (> (length conjuncts) 1)
        (cons 'and conjuncts)
        (car conjuncts))))

;;; Encode a state variable value.

(defun sv-value-to-atoms (state-vars name value)
  (let* ((sv (loop for state-var in state-vars
                   when (eq (state-var-name state-var) name)
                   do (return state-var)))
         (size (size-in-bits (- (length (state-var-values sv)) 1)))
         (sv-names (loop for i = 0 then (+ i 1)
                         while (< i size)
                         collect
                         (make-name (list name "-" (format nil "~A" i)))))
         (nvalue (- (length (state-var-values sv))
                    (length (member-equal value (state-var-values sv)))))
         (conjuncts (loop for sv-name in sv-names
                          for v = nvalue then (ash v -1)
                          collect (if (zerop (mod v 2))
                                      (list 'not sv-name)
                                      sv-name))))
    (if (> (length conjuncts) 1)
        (cons 'and conjuncts)
        (car conjuncts))))




;;; Function to convert a formula to use the binary encodings
;;; of "literals".

(defun convert-formula (pc-infos state-vars formula)
  (cond ((true-p formula) formula)
	((false-p formula) formula)
	((if-p formula)
         (make-if (convert-formula pc-infos state-vars (if-test formula))
                  (convert-formula pc-infos state-vars (if-left formula))
                  (convert-formula pc-infos state-vars (if-right formula))))
	((not-p formula)
         (make-not (convert-formula pc-infos state-vars (not-expr formula))))
	((and-p formula)
         (cons 'and
               (mapcar #'(lambda (x) (convert-formula pc-infos state-vars x))
                       (cdr formula))))
	((or-p formula)
         (cons 'or
               (mapcar #'(lambda (x) (convert-formula pc-infos state-vars x))
                       (cdr formula))))
	((implies-p formula)
         (make-implies (convert-formula
                         pc-infos state-vars (implies-left formula))
                       (convert-formula
                         pc-infos state-vars (implies-right formula))))
        ((or (x-p formula) (f-p formula) (g-p formula)
             (u-p formula) (r-p formula))
         (cons (car formula)
               (mapcar #'(lambda (x) (convert-formula pc-infos state-vars x))
                       (cdr formula))))
        ((and (listp formula)
              (eq (car formula) '=))
         (let ((left (second formula))
               (right (third formula)))
           (cond ((state-variable-name-p left)
                  (sv-value-to-atoms state-vars left right))
                 ((pc-name-p left)
                  (pc-value-to-atoms pc-infos left right))
                 ((equal left right)
                  *true*)
                 (t *false*))))
        ;; *** Need new way of handling events
        (t (format t "~%~%~S~%~%" formula) 'error)))

(defun collect-equalities (formula)
  (cond ((true-p formula) nil)
        ((false-p formula) nil)
        ((or (if-p formula) (not-p formula) (and-p formula) (or-p formula)
             (implies-p formula)(x-p formula) (f-p formula) (g-p formula)
             (u-p formula) (r-p formula))
         (loop for expr in (cdr formula)
               append (collect-equalities expr)))
        ((and (listp formula)
              (eq (car formula) '=)
              (atom (second formula))
              (atom (third formula)))
         (list formula))))

(defun collect-atoms (pc-infos state-vars formula)
  (let ((equalities (remove-duplicates (collect-equalities formula))))
    (remove-duplicates
      (loop for equality in equalities
            append
            (let ((literals
                    (cond ((state-variable-name-p (second equality))
                           (sv-value-to-list-of-atoms
                             state-vars (second equality) (third equality)))
                          ((pc-name-p (second equality))
                           (pc-value-to-list-of-atoms
                             pc-infos (second equality) (third equality))))))
              (loop for literal in literals
                    collect
                    (cond ((not-p literal) (not-expr literal))
                          (t literal))))))))

;;; Well-formedness check of formula.
;;; Returns t if formula is well-formed.
;;; Otherwise returns nil.

(defun wfcheck-formula (formula)
  (cond ((true-p formula) t)
	((false-p formula) t)
	((if-p formula)
         (and (wfcheck-formula (if-test formula))
              (wfcheck-formula (if-left formula))
              (wfcheck-formula (if-right formula))))
	((not-p formula) (wfcheck-formula (not-expr formula)))
	((and-p formula)
         (every #'wfcheck-formula (cdr formula)))
	((or-p formula)
         (every #'wfcheck-formula (cdr formula)))
	((implies-p formula)
         (and (wfcheck-formula (implies-left formula))
              (wfcheck-formula (implies-right formula))))
        ((or (x-p formula) (f-p formula) (g-p formula)
             (u-p formula) (r-p formula))
         (every #'wfcheck-formula (cdr formula)))
        ((and (listp formula)
              (eq (car formula) '=)
              (atom (second formula))
              (atom (third formula)))
         ;; might change to a more elaborate check
         t)))

(defun wfcheck-proposition (formula)
  (cond ((true-p formula) t)
	((false-p formula) t)
	((if-p formula)
         (and (wfcheck-formula (if-test formula))
              (wfcheck-formula (if-left formula))
              (wfcheck-formula (if-right formula))))
	((not-p formula) (wfcheck-formula (not-expr formula)))
	((and-p formula)
         (every #'wfcheck-formula (cdr formula)))
	((or-p formula)
         (every #'wfcheck-formula (cdr formula)))
	((implies-p formula)
         (and (wfcheck-formula (implies-left formula))
              (wfcheck-formula (implies-right formula))))
        ((and (listp formula)
              (eq (car formula) '=)
              (atom (second formula))
              (atom (third formula)))
         ;; might change to a more elaborate check
         t)))

(defun effects-to-atoms (pc-infos state-vars effects)
  (loop for effect in effects
        append (effect-to-atoms pc-infos state-vars effect)))

(defun effect-to-atoms (pc-infos state-vars effect)
  (let ((left (effect-l-value effect))
        (right (effect-r-value effect)))
    (cond ((state-variable-name-p left)
           (sv-value-to-list-of-atoms state-vars left right))
          ((pc-name-p left)
           (pc-value-to-list-of-atoms pc-infos left right))
          ;;(t
           ;; *** events are no longer dealt using effects
          ;; (event-value-to-list-of-atoms right))))
          )))


;;; Encode a PC value as a list of unnegated/negated atoms

(defun pc-value-to-list-of-atoms (pc-infos name value)
  (let* ((pc (loop for pc-info in pc-infos
                   when (eq (pc-info-name pc-info) name)
                   do (return pc-info)))
         (size (size-in-bits (pc-info-max-value pc)))
         (pc-names (loop for i = 0 then (+ i 1)
                         while (< i size)
                         collect
                         (make-name (list  name "-" (format nil "~A" i))))))
    (loop for pc-name in pc-names
          for v = value then (ash v -1)
          collect (if (zerop (mod v 2))
                      (list 'not pc-name)
                      pc-name))))

;;; Encode a state variable value as a list of unnegated/negated atoms

(defun sv-value-to-list-of-atoms (state-vars name value)
  (let* ((sv (loop for state-var in state-vars
                   when (eq (state-var-name state-var) name)
                   do (return state-var)))
         (size (size-in-bits (- (length (state-var-values sv)) 1)))
         (sv-names (loop for i = 0 then (+ i 1)
                         while (< i size)
                         collect
                         (make-name (list name "-" (format nil "~A" i)))))
         (nvalue (- (length (state-var-values sv))
                    (length (member-equal value (state-var-values sv))))))
    (loop for sv-name in sv-names
          for v = nvalue then (ash v -1)
          collect (if (zerop (mod v 2))
                      (list 'not sv-name)
                      sv-name))))




;;; Unencode formula from binary encoding.
;;; Encoded formula must be in DNF.

(defun unencode-formula (pc-infos state-vars formula)
  (cleanup-unencode
  (cond ((or-p formula)
         (cons 'or (loop for disjunct in (cdr formula)
                         collect
                         (unencode-disjunct pc-infos state-vars disjunct))))
        (t (unencode-disjunct pc-infos state-vars formula)))))

(defun cleanup-unencode (formula)
  (cond ((or-p formula)
         (cons 'or
               (remove-duplicates
                  (loop for disjunct in (cdr formula)
                        append (cond ((or-p disjunct)
                                      (cdr disjunct))
                                     (t (list disjunct)))))))
       (t formula)))

(defun unencode-disjunct (pc-infos state-vars disjunct)
  (cond ((and-p disjunct)
         (let ((result (unencode-list-of-literals
                         pc-infos state-vars (cdr disjunct))))
           (cond ((= (length result) 1) (car result))
                 (t (cons 'and result)))))
        (t (car (unencode-list-of-literals
                   pc-infos state-vars (list disjunct))))))


(defun unencode-list-of-literals (pc-infos state-vars list-of-literals)
  (let ((rest-of-literals list-of-literals)
        (pc-expressions nil)
        (sv-expressions nil))
    (loop for pc-info in pc-infos
          do
          (let ((pc-bits (collect-bits-for-pc pc-infos
                                              (pc-info-name pc-info)
                                              rest-of-literals)))
            (setq rest-of-literals
                  (remove-bits-for-pc pc-infos
                                      (pc-info-name pc-info)
                                      rest-of-literals))
            (unless (null pc-bits)
              (setq pc-expressions
                    (cons (unencode-pc-bits pc-info pc-bits)
                          pc-expressions)))))
    (loop for state-var in state-vars
          do
          (let ((sv-bits (collect-bits-for-sv state-vars
                                              (state-var-name state-var)
                                              rest-of-literals)))
            (setq rest-of-literals
                  (remove-bits-for-sv state-vars
                                      (state-var-name state-var)
                                      rest-of-literals))
            (unless (null sv-bits)
              (let ((new (unencode-sv-bits state-var sv-bits)))
                (unless (eq new *true*)
                  (setq sv-expressions
                        (cons new sv-expressions)))))))
    (append (reverse pc-expressions)
            (reverse sv-expressions)
            (unencode-rest-of-literals
              pc-infos state-vars rest-of-literals))))

;;; Each literal in the list is a possibly negated elementary
;;; formula or promise expression.

(defun unencode-rest-of-literals (pc-infos state-vars list-of-literals)
  (let ((list-of-results nil))
    (loop
      for literal in list-of-literals
      do
      (cond
        ((not-p literal)
         (cond
           ((or (eq (car (second literal)) 'el)
                (eq (car (second literal)) 'promise))
            (push
              (make-not (list (car (second literal))
                              (unconvert pc-infos state-vars
                                         (second (second literal)))))
              list-of-results))
           (t (format *error-output*
                      "Unencoding error for literal ~A.~%" literal))))
       ((or-p literal)
        (cleanup-unencode
         (cons 'or (loop for disjunct in (cdr literal)
                         collect
                         (unencode-disjunct pc-infos state-vars disjunct)))))
       (t
         (cond
           ((or (eq (car literal) 'el) (eq (car literal) 'promise))
            (push
              (list (car literal)
                    (unconvert pc-infos state-vars (second literal)))
              list-of-results))
           ((or (equal literal *true*) (equal literal *false*))
            (push literal list-of-results))
           (t (format *error-output*
                      "Unencoding error for literal ~A.~%" literal))))))
    (reverse list-of-results)))

(defun unconvert (pc-infos state-vars formula)
  (cond ((or (f-p formula) (g-p formula) (x-p formula))
         (list (car formula)
               (unconvert pc-infos state-vars (second formula))))
        ((or (u-p formula) (r-p formula) (or-p formula))
         (list (car formula)
               (unconvert pc-infos state-vars (second formula))
               (unconvert pc-infos state-vars (third formula))))
        ((or-p formula)
         (cons (car formula)
               (mapcar #'(lambda (x) (unconvert pc-infos state-vars x))
                       (cdr formula))))
        ((and-p formula)
         (let ((result (unconvert-list pc-infos state-vars (cdr formula))))
           (cond ((= (length result) 1) (car result))
                 (t (cons 'and result)))))
        ((not-p formula)
         (cond ((symbolp (second formula))
                (car (unconvert-list pc-infos state-vars (list formula))))
               (t (make-not
                    (unconvert pc-infos state-vars (second formula))))))
        ((symbolp formula)
         (car (unconvert-list pc-infos state-vars (list formula))))
        ((or (eq formula *true*) (eq formula *false*)) formula)
        (t
         (format *error-output* "Unencoding error in unconvert ~A.~%" formula)
         nil)))

(defun unconvert-list (pc-infos state-vars list-of-literals)
  (let ((rest-of-literals list-of-literals)
        (pc-expressions nil)
        (sv-expressions nil))
    (loop for pc-info in pc-infos
          do
          (let ((pc-bits (collect-bits-for-pc pc-infos
                                              (pc-info-name pc-info)
                                              rest-of-literals)))
            (setq rest-of-literals
                  (remove-bits-for-pc pc-infos
                                      (pc-info-name pc-info)
                                      rest-of-literals))
            (unless (null pc-bits)
              (setq pc-expressions
                    (cons (unencode-pc-bits pc-info pc-bits)
                          pc-expressions)))))
    (loop for state-var in state-vars
          do
          (let ((sv-bits (collect-bits-for-sv state-vars
                                              (state-var-name state-var)
                                              rest-of-literals)))
            (setq rest-of-literals
                  (remove-bits-for-sv state-vars
                                      (state-var-name state-var)
                                      rest-of-literals))
            (unless (null sv-bits)
              (setq sv-expressions
                    (cons (unencode-sv-bits state-var sv-bits)
                          sv-expressions)))))
    (append (reverse pc-expressions)
            (reverse sv-expressions)
            (loop for literal in rest-of-literals
                  collect (unconvert pc-infos state-vars literal)))))

(defun unencode-pc-bits (pc-info pc-bits)
  (let* ((name (pc-info-name pc-info))
         (size (size-in-bits (pc-info-max-value pc-info)))
         (values (loop for i = 0 then (+ i 1)
                       while (<= i (pc-info-max-value pc-info))
                       collect i))
         (pc-names (loop for i = 0 then (+ i 1)
                         while (< i size)
                         collect
                         (make-name (list  name "-" (format nil "~A" i))))))
    (loop for pc-bit in pc-bits
          do
          (let* ((negated (not-p pc-bit))
                 (bit (if (not-p pc-bit) (not-expr pc-bit) pc-bit))
                 (power (loop for i = 0 then (+ i 1)
                              for pc-name in pc-names
                              while (< i size)
                              when (equal bit pc-name)
                              return i))
                 (mask (expt 2 power)))
            (if negated
                (setq values
                      (loop for value in values
                            unless (= (logand mask value) mask)
                            collect value))
                (setq values
                      (loop for value in values
                            when (= (logand mask value) mask)
                            collect value)))))
    (cond ((= (length values) 1)
           (list '= name (car values)))
          (t (cons 'or (loop for value in values
                             collect (list '= name value)))))))


(defun unencode-sv-bits (state-var sv-bits)
  (let* ((name (state-var-name state-var))
         (size (size-in-bits (- (length (state-var-values state-var)) 1)))
         (values (loop for i = 0 then (+ i 1)
                       while (< i (length (state-var-values state-var)))
                       collect i))
         (sv-values (state-var-values state-var))
         (sv-names (loop for i = 0 then (+ i 1)
                         while (< i size)
                         collect
                         (make-name (list  name "-" (format nil "~A" i))))))
    (loop for sv-bit in sv-bits
          do
          (let* ((negated (not-p sv-bit))
                 (bit (if (not-p sv-bit) (not-expr sv-bit) sv-bit))
                 (power (loop for i = 0 then (+ i 1)
                              for sv-name in sv-names
                              while (< i size)
                              when (equal bit sv-name)
                              return i))
                 (mask (expt 2 power)))
            (if negated
                (setq values
                      (loop for value in values
                            unless (= (logand mask value) mask)
                            collect value))
                (setq values
                      (loop for value in values
                            when (= (logand mask value) mask)
                            collect value)))))
    (cond ((= (length values) 1)
           (list '= name (nth (car values) (state-var-values state-var))))
          ((= (length (state-var-values state-var)) 1)
           ;; special case for when there is a state variable can only
           ;; have one value
           *true*)
          (t (cons 'or (loop for value in values
                             collect
                             (list '= name (nth value sv-values))))))))


(defun collect-bits-for-sv (state-vars name list-of-literals)
  (let* ((sv (loop for state-var in state-vars
                   when (eq (state-var-name state-var) name)
                   do (return state-var)))
         (size (size-in-bits (- (length (state-var-values sv)) 1)))
         (sv-names (loop for i = 0 then (+ i 1)
                         while (< i size)
                         collect
                         (make-name (list name "-" (format nil "~A" i))))))
    (loop for literal in list-of-literals
          when (member-equal (cond ((not-p literal) (not-expr literal))
                                   (t literal))
                             sv-names)
          collect literal)))

(defun remove-bits-for-sv (state-vars name list-of-literals)
  (let* ((sv (loop for state-var in state-vars
                   when (eq (state-var-name state-var) name)
                   do (return state-var)))
         (size (size-in-bits (- (length (state-var-values sv)) 1)))
         (sv-names (loop for i = 0 then (+ i 1)
                         while (< i size)
                         collect
                         (make-name (list name "-" (format nil "~A" i))))))
    (loop for literal in list-of-literals
          unless (member-equal (cond ((not-p literal) (not-expr literal))
                                     (t literal))
                               sv-names)
          collect literal)))

(defun collect-bits-for-pc (pc-infos name list-of-literals)
  (let* ((pc (loop for pc-info in pc-infos
                   when (eq (pc-info-name pc-info) name)
                   do (return pc-info)))
         (size (size-in-bits (pc-info-max-value pc)))
         (pc-names (loop for i = 0 then (+ i 1)
                         while (< i size)
                         collect
                         (make-name (list  name "-" (format nil "~A" i))))))
    (loop for literal in list-of-literals
          when (member-equal (cond ((not-p literal) (not-expr literal))
                                   (t literal))
                             pc-names)
          collect literal)))

(defun remove-bits-for-pc (pc-infos name list-of-literals)
  (let* ((pc (loop for pc-info in pc-infos
                   when (eq (pc-info-name pc-info) name)
                   do (return pc-info)))
         (size (size-in-bits (pc-info-max-value pc)))
         (pc-names (loop for i = 0 then (+ i 1)
                         while (< i size)
                         collect
                         (make-name (list  name "-" (format nil "~A" i))))))
    (loop for literal in list-of-literals
          unless (member-equal (cond ((not-p literal) (not-expr literal))
                                     (t literal))
                               pc-names)
          collect literal)))





;;; ====================== Elementary Blocks =======================


(defun create-block-transitions (pc-infos state-vars block)
  (cond ((null (bt-block-conditional-effects block))
         (list (create-transition
                 pc-infos state-vars
                 (make-conjunction
                    `(= ,(bt-block-pc block)
                        ,(bt-block-pc-value block))
                    (bt-block-condition block))
                 (append (bt-block-pc-effects block)
                         (bt-block-main-effects block)))))
        (t
         (loop for ce in (bt-block-conditional-effects block)
               collect
               (create-transition
                 pc-infos state-vars
                 (make-conjunction
                    `(= ,(bt-block-pc block)
                        ,(bt-block-pc-value block))
                    (conditional-effects-condition ce))
                 (conditional-effects-effects ce))))))

(defun create-transition (pc-infos state-vars condition effects)
  (let ((effect-atoms (effects-to-atoms pc-infos state-vars effects))
        (c (push-bdd-entry
             (bdd-integrate-formula
               (convert-formula pc-infos state-vars condition))))
        (overrides nil))
    (loop for atom in effect-atoms
          do
          (setq overrides
                (insert-override
                  (cond ((not-p atom)
                         (cons (bdd-integrate-atomic (not-expr atom)) 0))
                        (t (cons (bdd-integrate-atomic atom) 1)))
                  overrides)))
    (make-transition
      ;; condition
      c
      ;; effects
      overrides
      ;; post
      (push-bdd-entry (bdd-override (bdd-entry c) overrides)))))

;;; overrides must be sorted
;;; no entry in overrides has the same l-value as new
;;; the result is also sorted

(defun insert-override (new overrides)
  (cond ((null overrides) (list new))
        ((< (car new) (caar overrides))
         (cons new overrides))
        (t (cons (car overrides)
                 (insert-override new (cdr overrides))))))

;;; Function to construct the formula representing the default
;;; set of initial states.

(defun default-initial-states (pc-infos state-vars)
  (let ((condition *true*))
    (unless (null pc-infos)
      (setq condition `(= ,(pc-info-name (car pc-infos)) 1)))
    (loop for pc-info in (cdr pc-infos)
          do (setq condition
                   (make-conjunction condition
                                     `(= ,(pc-info-name pc-info) 0))))
    (convert-formula pc-infos state-vars condition)))

;;; Top level function for creating elementary blocks
;;; (using information stored in BT blocks).
;;; This makes everything ready for the generic part of the analysis,
;;; thus *initial-states-formula*, *bdd-atom-list-before-analysis*
;;; and *initial blocks-indices* are set.

(defun create-elementary-blocks (pc-infos state-vars bt-block-array block-root)
  (let* ((number-of-blocks (length bt-block-array))
         (block-array (make-array number-of-blocks)))
    (loop for i from 0 to (- number-of-blocks 1)
          do (let ((bt-block (aref bt-block-array i)))
               (setf (aref block-array i)
                     (make-elementary-block
                       bt-block
                       (create-block-transitions pc-infos state-vars bt-block)
                       (push-bdd-entry
                         (bdd-integrate-formula
                           (convert-formula
                             pc-infos state-vars
                             `(= ,(bt-block-pc bt-block)
                                 ,(bt-block-pc-value bt-block)))))
                       (push-bdd-entry
                         (bdd-integrate-formula
                           (let ((atoms
                                   (effects-to-atoms
                                     pc-infos state-vars
                                     (append (bt-block-pc-effects bt-block)
                                             (bt-block-main-effects
                                               bt-block)))))  
                             (cond ((= (length atoms) 1) atoms)
                                   (t (cons 'and atoms))))))))))
    ;; set *initial-states-formula*
    (setq *initial-states-formula* (default-initial-states pc-infos state-vars))
    (setq *bdd-atom-list-before-analysis* *atom-list*)
    ;; set *initial blocks-indices* (a list of indices)
    (setq *initial-block-indices* nil)
    (loop for b in block-root
          do (setq *initial-block-indices*
                   (append *initial-block-indices*
                           (list (bt-block-index b)))))
    block-array))


;;; Might be useful for determining BDD ordering.

(defun collect-effects (pc-infos state-vars block-root)
  (let ((effects (collect-effects-aux pc-infos state-vars block-root)))
    (cond ((null effects) *false*)
          ((= (length effects) 1) (car effects))
          (t (cons 'or effects)))))

(defun collect-effects-aux (pc-infos state-vars blocks)
  (loop for block in blocks
        append
        (collect-effects-aux-aux pc-infos state-vars block)))

(defun collect-effects-aux-aux (pc-infos state-vars block)
  (let ((effects (effects-to-atoms
                   pc-infos state-vars
                   (append (bt-block-pc-effects block)
                           (bt-block-main-effects block)))))
    (cond ((null effects)
           (collect-effects-aux pc-infos state-vars (bt-block-children block)))
          (t
           (cond ((= (length effects) 1)
                  (cons (car effects)
                        (collect-effects-aux
                          pc-infos state-vars (bt-block-children block))))
                (t
                 (cons (cons 'and effects)
                       (collect-effects-aux
                         pc-infos state-vars (bt-block-children block)))))))))
