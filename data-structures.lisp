
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

;;; Data Structures for the generic framework.


;;; =========================== Part 1 =================================

;;; The structures in this part must be populated by a front end
;;; that processes a specific modelling notation.

;;; Structure for elementary blocks
;;; - source: points to modelling notation specific source block
;;; - transitions: a list of transitions.
;;; - pc-condition: index to BDD of (= pc pc-value)
;;; - exit-condition: index to BDD of "result of update"
;;;   common to all transitions through the block.

(defstruct
  (elementary-block
    (:constructor
      make-elementary-block (source transitions pc-condition exit-condition))
     :named :predicate)
   source transitions pc-condition exit-condition)


;;; Structure for a transition (guarded update)
;;; - condition: an index to the BDD representation of the transition
;;;   guard.
;;; - effects: a list of "overrides" (each override is (var . 1) or
;;;   (var . 0)).
;;; - post: an index to the BDD representation of the "state"
;;;   representing effects.

(defstruct (transition (:constructor make-transition (condition effects post))
                       :named :predicate)
           condition effects post)

;;; *initial-block-indices* is to hold a list of indices for the
;;; initial blocks.
;;; A front end to the model checker needs to set *initial-block-indices*.

(defvar *initial-block-indices* nil)

;;; The variable to save *atom-list* after the model is constructed
;;; but before analysis is performed.
;;; The front end needs to set it after elementary blocks and
;;; their guarded updates (with their BDD structures)
;;; have been generated.

(defvar *bdd-atom-list-before-analysis* nil)




;;; =========================== Part 2 =================================

;;; The structures in this part are used in fixpoint.lisp for
;;; various fixpoint computations.



;;; Structure for reachability computation (one for each block).
;;; pre-entry-states: index to BDD entry for reachable states
;;;                   just before the block
;;; entry-states: indices to BDD for states that can transition through
;;;               the block (one for each guarded update)
;;; exit-states: indices to BDD for states that result from transitions
;;;              through the block (one for each guarded update)
;;; predecessors: indices to possible predecessor blocks
;;; successors: indices to possible successor blocks

(defstruct
  (reachable
    (:constructor make-reachable (pre-entry-states
                                  entry-states exit-states))
    :named :predicate)
   pre-entry-states entry-states exit-states
   predecessors successors)


;;; Structure for representing a "tableau".
;;; The tableau applies to an augmented model where
;;; state variables for "elementary formulas" (v-formulas)
;;; and promise variables (in the case of TGBA encoding) are
;;; added to the model.  The addition of the state variables
;;; as part of the state places constraints on the transitions.
;;;
;;; formula: the formula being checked for satisfiability
;;;          (negation of formula being model checked, normalized)
;;; v-formulas: the "elementary" formulas
;;; fairness-formulas: a list of fairness constraint formulas that
;;;                    correspond to eventualities
;;; x-vars: BDD variables for the added state variables
;;;         in the case of TGBA encoding, x-vars includes
;;;         the "promise" variables
;;; transition-constraints: a list of transition constraints
;;;                         (one transition-constraint for each
;;;                         tableau case)
;;; fairness-constraints: a list of indices to BDDs for fairness
;;;                       constraints

(defstruct
  (tableau
    (:constructor make-tableau (formula v-formulas))
    :named :predicate)
  formula v-formulas fairness-formulas x-vars
  transition-constraints fairness-constraints)

;;; A pair structure for projections of (symbolic) states to
;;; elementary blocks used in LTL model checking.
;;;
;;; Usage:
;;; The pair represents the states at the entry and exit points
;;; of an elementary block.  The states at the entry point are
;;; those that are guaranteed to be able to transition through
;;; the elementary block becoming states at the exit point.
;;; Each projection is further "partitioned" according to guarded
;;; updates and tableau cases (the "partitions" are not
;;; necessarily mutually disjoint).  The "partitioning" is
;;; reflected by a list of lists of indices to BDDs: the outer
;;; list represents the different guarded updates while the inner
;;; list represents the tableau cases.

(defstruct
  (bs (:constructor make-bs (entry exit))
      :named :predicate)
  entry exit)


;;; Structure for an augmented model.
;;; formula: the formula being checked for satisfiability
;;;          (the negation of the formula being model-checked).
;;; tableau: the tableau (case structure) for checking satisfiability.
;;; bs: array of bs structs
;;; Note: bs may be constrained to reachable states.

(defstruct
  (augmented-model
    (:constructor make-augmented-model (formula spec tableau bs))
    :named :predicate)
  formula spec tableau bs)


;;; Structure for fixpoint computation of fair states in LTL checking.
;;; The fixpoint computation is for
;;;  \nu Z . gc \wedge
;;;     \bigwedge_{c \in C_{\varphi}}
;;;        EX(\mu Y (Z \wedge c) \vee (gc \wedge EX Y))
;;; gc: BDD index to global constraint (global constraint defaults to "true")
;;; model: augmented model
;;; reachable: t or nil depending on reachability strategy used.
;;; top: an array of bs-entrys for the outermost fixpoint
;;;      computation (i.e., Z)
;;; spec: BDD index to the "spec"
;;;       for CGH, GBA and GBA-LOOSE this corresponds to sat(formula)
;;;       for TGBA and TGBA-LOOSE this corresponds to el(formula)
;;; loop-count: stat for main loop in fixpoint computation.
;;; aux-count: stat
;;; aux-loop-count: stat

(defstruct
  (mc
    (:constructor make-mc (gc model reachable))
    :named :predicate)
  gc model reachable top spec loop-count aux-count aux-loop-count)

;;; Structure for transition constraints (used in the tableau).
;;; entry: BDD index for entry constraint
;;; exit: BDD index for exit constraint

(defstruct
  (transition-constraint
    (:constructor make-transition-constraint (entry exit))
    :named :predicate)
  entry exit)

