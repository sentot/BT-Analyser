
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

;;; Parser for BT Textual Representation (TextBE input format).

;;; Structure for BT nodes
;;; - type: update, select, guard, event, input, output
;;; - requirement-label: string
;;; - requirement-status: "+", "-", "--", "+-", "++"
;;; - component: component name
;;; - behaviour: raw behaviour - determines l-value, r-value, event
;;; - behaviour-type: s-token (update), l-token (select), g-token
;;;                   (guard), e-token (event), ii-token (internal
;;;                   input), io-token (internal output), ei-token
;;;                   (external input), eo-token (external output)
;;; - flag: kill, reversion, reference, synchronise or nil
;;; - label: label for flag operation (string)
;;; - branch-type: alternative, parallel or nil
;;; - parent: parent for BT node
;;; - children: list of children
;;; - atomic: atomic composition with child (t or nil)
;;; - l-value: for update
;;; - r-value: for update, select, guard
;;; - event: event name or nil
;;; ---- fields for BT analysis:
;;; - target: target node/group for flag
;;; - pc: PC for innermost thread that includes the BT node
;;; - pc-value: value for above PC at the entrance of the BT node
;;; - pc-next: value for above PC after BT node is "executed"
;;; - target-pc: PC of target for reference, reversion
;;; - target-pc-value: value for target PC
;;; - condition: condition on components and attributes
;;; - pc-effects: effects of transition on PCs
;;; - main-effects: effects of transition on components and attributes
;;; - blocks
;;; - input-partners
;;; - output-partners


(defstruct
  (bt-node
    (:constructor make-bt-node (type))
    :named :predicate)
  type requirement-label requirement-status component
  behaviour behaviour-type flag label branch-type parent children atomic
  l-value r-value event
  target pc pc-value pc-next target-pc target-pc-value
  condition input-partners output-partners pc-effects main-effects
  blocks)

(defvar *bt-root* nil)



;;; A token consists of its class, its string and its source line number

(defstruct (token (:constructor make-token (class string line-number))
            :named :predicate)
  class string line-number)

;;; In constructing a BT, we build a hash table linking reference
;;; names to requirement labels.

(defvar *bt-requirement-labels* (make-hash-table :test #'equal))

;;; In constructing a BT, we build a hash table linking reference
;;; names to components.  Each component has a name, an indicator
;;; indicating whether or not it is a system component, and an
;;; alist linking reference names to its behaviours.

(defvar *bt-components* (make-hash-table :test #'equal))

(defstruct (component (:constructor make-component
                                    (name system-p behaviours))
            :named :predicate)
  name system-p behaviours)


(defvar *fatal-error* nil)

;;; Code to reset the data structures for parsing

(defun reset-parser ()
  (setq *fatal-error* nil)
  (clrhash *bt-requirement-labels*)
  (clrhash *bt-components*))



;;; Function to tokenise a string.
;;; It returns a list of tokens.
;;; The special token strings are ";;", ";", "{", "}"
;;; and the operators.

(defun tokenise-string (str line-number)
  ;; trim leading white space
  (let ((st (string-trim '(#\Space #\Tab #\Return) str)))
    (loop while (and (>= (string-length st) 1)
                     ;; discard ComBE-style comments
                     (not (and (>= (string-length st) 2)
                               (string= "//" (substring st 0 2)))))
          collect
          ;; detect special tokens
          (cond ((and (>= (string-length st) 2)
                      (string= ";;" (substring st 0 2)))
                 (setq st (string-trim '(#\Space #\Tab #\Return)
                                       (substring st 2)))
                 (make-token 'double-semicolon-token ";;"
                             line-number))
                ((string= ";" (substring st 0 1))
                 (setq st (string-trim '(#\Space #\Tab #\Return)
                                       (substring st 1)))
                 (make-token 'semicolon-token ";" line-number))
                ((string= "{" (substring st 0 1))
                 (setq st (string-trim '(#\Space #\Tab #\Return)
                                       (substring st 1)))
                 (make-token 'open-brace-token "{" line-number))
                ((string= "}" (substring st 0 1))
                 (setq st (string-trim '(#\Space #\Tab #\Return)
                                       (substring st 1)))
                 (make-token 'close-brace-token "}" line-number))
                (t (let ((i nil)
                         (tk-string nil))
                     ;; token is not a special token
                     ;; thus it can be terminated by white space or
                     ;; the start of a special token
                     (loop for p in '(#\Space #\Tab #\Return ";" "{" "}")
                           do (let ((j (string-search= p st)))
                                (cond ((null i) (setq i j))
                                      ((not (null j))
                                       (setq i (min i j))))))
                     (cond ((and i (>= i 1))
                            (setq tk-string (substring st 0 i))
                            (setq st (string-trim '(#\Space #\Tab #\Return)
                                                  (substring st i))))
                           (t (setq tk-string st)
                              (setq st "")))
                     (make-token (classify-token-string tk-string)
                                 tk-string
                                 line-number)))))))

(defun classify-token-string (str)
  (cond ((equal str "#RT") 'rt-token)
        ((equal str "#C") 'c-token)
        ((equal str "#SC") 'sc-token)
        ((equal str "#S") 's-token)
        ((equal str "#L") 'l-token)
        ((equal str "#E") 'e-token)
        ((equal str "#G") 'g-token)
        ((equal str "#II") 'ii-token)
        ((equal str "#IO") 'io-token)
        ((equal str "#EI") 'ei-token)
        ((equal str "#EO") 'eo-token)
        ((equal str "#A") 'q-token)
        ((equal str "#T") 't-token)
        ((equal str "#N") 'n-token)
        ((equal str "#P") 'p-token)
        ((equal str "^") 'caret-token)
        ((equal str "=") 'equal-token)
        ((equal str "=>") 'equal-greater-token)
        ((equal str "+") 'plus-token)
        ((equal str "-") 'minus-token)
        ((equal str "++") 'plus-plus-token)
        ((equal str "+-") 'plus-minus-token)
        ((equal str "--") 'minus-minus-token)
        (t 'value-token)))

(defun tokenise-file (filename)
  (with-open-file (stream filename)
    (cond ((null stream)
           (format *error-output* "File not found: ~A~%" filename)
           nil)
          (t
           (let ((str "")
                 (eof nil))
             (loop while (progn (multiple-value-setq (str eof)
                                  (read-line stream nil))
                                (not eof))
                   for n from 1
                   append (tokenise-string str n)))))))

(defun tokenise-input-string (input-string)
  (with-input-from-string (stream input-string)
    (cond ((not (stringp input-string))
           (format *error-output* "Invalid input string")
           nil)
          (t
           (let ((str "")
                 (eof nil))
             (loop while (progn (multiple-value-setq (str eof)
                                  (read-line stream nil))
                                (not eof))
                   for n from 1
                   append (tokenise-string str n)))))))



(defun is-behaviour-keyword-token (token)
  (let ((tk-class (token-class token)))
    (or (eq tk-class 's-token)
        (eq tk-class 'l-token)
        (eq tk-class 'e-token)
        (eq tk-class 'g-token)
        (eq tk-class 'ii-token)
        (eq tk-class 'io-token)
        (eq tk-class 'ei-token)
        (eq tk-class 'eo-token)
        (eq tk-class 'q-token))))

(defun behaviour-token-class-to-type (cls)
  (cond ((eq cls 's-token) 'update)
        ((eq cls 'l-token) 'select)
        ((eq cls 'e-token) 'event)
        ((eq cls 'g-token) 'guard)
        ((eq cls 'ii-token) 'input)
        ((eq cls 'io-token) 'output)
        ((eq cls 'ei-token) 'event)
        ((eq cls 'eo-token) 'event)
        ((eq cls 'q-token) 'quantification)))

(defun is-requirement-status-token (token)
  (let ((tk-class (token-class token)))
    (or (eq tk-class 'plus-token)
        (eq tk-class 'minus-token)
        (eq tk-class 'plus-plus-token)
        (eq tk-class 'minus-minus-token)
        (eq tk-class 'plus-minus-token))))

(defun is-operation-token (token)
  (let ((tk-class (token-class token)))
    (or (eq tk-class 'equal-greater-token)
        (eq tk-class 'caret-token)
        (eq tk-class 'minus-minus-token)
        (eq tk-class 'equal-token))))

(defun operation-token-class-to-flag (cls)
  (cond ((eq cls 'equal-greater-token) 'reference)
        ((eq cls 'caret-token) 'reversion)
        ((eq cls 'minus-minus-token) 'kill)
        ((eq cls 'equal-token) 'synchronise)))



(defun parse-requirement-labels (token-list)
  (let ((remaining-token-list token-list))
    (loop while
          (and (>= (length remaining-token-list) 2)
               (eq (token-class (car remaining-token-list))
                   'value-token)
               (eq (token-class (cadr remaining-token-list))
                   'value-token))
     do (let ((ref (token-string (car remaining-token-list)))
              (val (token-string (cadr remaining-token-list))))
          (multiple-value-bind (v found)
            (gethash ref *bt-requirement-labels*)
            (declare (ignore v))
            (cond (found (parse-error 'duplicate-requirement
                                      (car remaining-token-list)))
                  (t (setf (gethash ref *bt-requirement-labels*) val)))
             (setq remaining-token-list
                   (cddr remaining-token-list)))))
    remaining-token-list))


(defun parse-component (token-list system-p)
  (cond ((>= (length token-list) 2)
         (cond ((not (eq (token-class (car token-list)) 'value-token))
                (parse-error 'component-reference (car token-list))
                token-list)
               ((not (eq (token-class (cadr token-list)) 'value-token))
                (parse-error 'component-value (cadr token-list))
                (cdr token-list))
               (t
                (let ((ref (token-string (car token-list)))
                      (val (token-string (cadr token-list)))
                      (remaining-token-list (cddr token-list)))
                  (multiple-value-bind (c found)
                    (gethash ref *bt-components*)
                    (declare (ignore c))
                    (cond (found 
                           (parse-error 'duplicate-component
                                        (car token-list))
                           token-list)
                          (t (let ((component (make-component
                                                 val system-p nil)))
                               (setf (gethash ref *bt-components*)
                                     component)
                               (parse-component-behaviours
                                  remaining-token-list component)))))))))
         (t
          (parse-error 'unexpected-eof)
          nil)))



(defun parse-component-behaviours (token-list component)
  (let ((remaining-token-list token-list))
    (loop while (and (not (null remaining-token-list))
                     (is-behaviour-keyword-token
                       (car remaining-token-list)))
          do (setq remaining-token-list
                   (parse-component-behaviours-aux
                      (cdr remaining-token-list)
                      component
                      (token-class (car remaining-token-list)))))
    remaining-token-list))

(defun parse-component-behaviours-aux (token-list component
                                       behaviour-class)
  ;; behaviour-class is one of
  ;; s-token, l-token, e-token, g-token,
  ;; ii-token, io-token, ei-token or eo-token
  (let ((remaining-token-list token-list))
    (loop
      while
      (and (>= (length remaining-token-list) 2)
           (eq (token-class (car remaining-token-list)) 'value-token)
           (eq (token-class (cadr remaining-token-list))
               'value-token))
      do (let* ((ref (token-string (car remaining-token-list)))
                (val (token-string (cadr remaining-token-list)))
                (pair (assoc-equal ref (component-behaviours component))))
           (cond ((null pair)
                  (when (and (>= (length val) 4)
                             (or (equal (substring val 0 2) "||")
                                 (equal (substring val 0 2) "[]"))
                             (>= (length remaining-token-list) 5)
                             (eq (token-class (third remaining-token-list))
                                 'open-brace-token)
                             (eq (token-class (fourth remaining-token-list))
                                 'value-token)
                             (eq (token-class (fifth remaining-token-list))
                                 'close-brace-token))
                    (setq val
                          (string-append
                            val "{" (token-string (fourth remaining-token-list))
                            "}"))
                    (setq remaining-token-list (cdddr remaining-token-list)))
                  (setf (component-behaviours component)
                        (cons (cons ref (cons behaviour-class val))
                              (component-behaviours component))))
                 (t (parse-error 'duplicate-behaviour
                                 (car remaining-token-list))))
           (setq remaining-token-list
                 (cddr remaining-token-list))))
    remaining-token-list))



;;; parse-bt parses a subtree.
;;; It returns multiple values (the root node and the remaining
;;; token list).

(defun parse-bt (token-list)
  (let ((root-node nil)
        (bt-node nil)
        (remaining-token-list token-list)
        (branching nil)
        (is-leaf nil))
    (multiple-value-setq (bt-node remaining-token-list branching is-leaf)
      (parse-bt-node token-list))
    (setq root-node bt-node)
    (loop while (and (not (null bt-node))
                     (not (null remaining-token-list))
                     (not is-leaf)
                     (not branching))
          do (let ((prev-node bt-node))
               (multiple-value-setq
                    (bt-node remaining-token-list branching is-leaf)
                 (parse-bt-node remaining-token-list))
               (when (not (null bt-node))
                 (setf (bt-node-parent bt-node) prev-node)
                 (setf (bt-node-children prev-node) (list bt-node)))))
    (when branching
      (setq remaining-token-list
            (parse-bt-branches remaining-token-list bt-node)))
    (values root-node remaining-token-list)))



;;; A BT node in TextBE notation starts with
;;; an optional requirement reference,
;;; followed by an optional requirement status (+, -, etc.),
;;; followed by a reference to the component name,
;;; followed by a reference to a behaviour of the component,
;;; followed by an optional operation (=>, ^, etc.),
;;; followed by an optional label for operation,
;;; followed by an optional flow specification.
;;; The flow specification is one of ";", ";;", "#P {", "#N {" or
;;; ";; #N {".

;;; parse-bt-node returns multiple values (BT node, remaining token
;;; list, and branching indicator)

(defun parse-bt-node (token-list)
  (let ((remaining-token-list token-list)
        (bt-node (make-bt-node nil))
        (done nil)
        (value nil)
        (found nil)
        (component nil)
        (branching nil)
        (is-leaf nil))
    ;; First find tag (optional reference to requirement label
    ;; followed by optional requirement status).  If a reference
    ;; to component is found first then skip parsing tag.
    (when (and (not (null remaining-token-list))
               (eq (token-class (car remaining-token-list))
                   'value-token))
      (let ((token (car remaining-token-list)))
        (multiple-value-setq (value found)
           (gethash (token-string token) *bt-requirement-labels*))
        (when found
          (setf (bt-node-requirement-label bt-node) value)
          (setq remaining-token-list (cdr remaining-token-list)))))
    ;; Next, parse optional requirement status
    (when (and (not (null remaining-token-list))
               (is-requirement-status-token (car remaining-token-list)))
      (setf (bt-node-requirement-status bt-node)
            (token-string (car remaining-token-list)))
      (setq remaining-token-list (cdr remaining-token-list)))
    ;; Next, parse reference to component name.
    (setq done nil)
    (loop while (and (not done) (not (null remaining-token-list)))
          do
          (let ((token (car remaining-token-list)))
            (setq remaining-token-list (cdr remaining-token-list))
            (cond
              ((eq (token-class token) 'value-token)
               (multiple-value-setq (component found)
                   (gethash (token-string token) *bt-components*))
               (cond
                 (found
                  (setf (bt-node-component bt-node)
                        (component-name component))
                  (setq done t))
                 (t (parse-error 'component-reference token))))
              (t (parse-error 'invalid-component-reference token)))))
    ;; Next, parse reference to behaviour.
    (setq done nil)
    (loop while (and (not done) (not (null remaining-token-list)))
          do
          (let ((token (car remaining-token-list)))
            (setq remaining-token-list (cdr remaining-token-list))
            (cond
              ((eq (token-class token) 'value-token)
               (let ((pair (assoc-equal
                             (token-string token)
                             (component-behaviours component))))
                 (cond ((null pair)
                        (parse-error 'behaviour-reference token))
                       (t (setf (bt-node-behaviour bt-node) (cddr pair))
                          (setf (bt-node-type bt-node)
                                (behaviour-token-class-to-type
                                   (cadr pair)))
                          (setf (bt-node-behaviour-type bt-node)
                                (cadr pair))
                          (set-l-value-and-r-value-event bt-node)
                          (setq done t)))))
              (t (parse-error 'invalid-behaviour-reference token)))))
    ;; Next, parse optional label.
    (when (and (not (null remaining-token-list))
               (eq (token-class (car remaining-token-list)) 'value-token)
               (not (token-starts-node-p (car remaining-token-list))))
      (setf (bt-node-label bt-node)
            (token-string (car remaining-token-list)))
      (setq remaining-token-list (cdr remaining-token-list)))
    ;; Next, parse optional operation (flag).
    (when (and (not (null remaining-token-list))
               (is-operation-token (car remaining-token-list)))
      (setf (bt-node-flag bt-node)
            (operation-token-class-to-flag
               (token-class (car remaining-token-list))))
      (setq remaining-token-list (cdr remaining-token-list)))
    ;; Next, parse flow.
    (cond ((and (>= (length remaining-token-list) 3)
                (eq (token-class (car remaining-token-list))
                    'double-semicolon-token)
                (eq (token-class (cadr remaining-token-list))
                    'n-token)
                (eq (token-class (caddr remaining-token-list))
                    'open-brace-token))
           ;; atomic alternative branching
           (setf (bt-node-branch-type bt-node) 'alternative)
           (setf (bt-node-atomic bt-node) t)
           (setq branching t)
           (setq remaining-token-list (cdddr remaining-token-list)))
          ((and (>= (length remaining-token-list) 2)
                (eq (token-class (car remaining-token-list))
                    'n-token)
                (eq (token-class (cadr remaining-token-list))
                    'open-brace-token))
           ;; alternative branching
           (setf (bt-node-branch-type bt-node) 'alternative)
           (setf (bt-node-atomic bt-node) nil)
           (setq branching t)
           (setq remaining-token-list (cddr remaining-token-list)))
          ((and (>= (length remaining-token-list) 2)
                (eq (token-class (car remaining-token-list))
                    'p-token)
                (eq (token-class (cadr remaining-token-list))
                    'open-brace-token))
           ;; parallel branching
           (setf (bt-node-branch-type bt-node) 'parallel)
           (setf (bt-node-atomic bt-node) nil)
           (setq branching t)
           (setq remaining-token-list (cddr remaining-token-list)))
          ((and (not (null remaining-token-list))
                (eq (token-class (car remaining-token-list))
                    'semicolon-token))
           ;; normal sequencing
           (setf (bt-node-branch-type bt-node) nil)
           (setf (bt-node-atomic bt-node) nil)
           (setq remaining-token-list (cdr remaining-token-list)))
          ((and (not (null remaining-token-list))
                (eq (token-class (car remaining-token-list))
                    'double-semicolon-token))
           ;; atomic sequencing
           (setf (bt-node-branch-type bt-node) nil)
           (setf (bt-node-atomic bt-node) t)
           (setq remaining-token-list (cdr remaining-token-list)))
          (t
           ;; no flow
           (setq is-leaf t)))
    (cond (done (values bt-node remaining-token-list branching is-leaf))
          (t (values nil remaining-token-list nil nil)))))



;;; This is an expression mini-parser.
;;; Depending on the node type, the expression is one of
;;; assignment (state realisation), condition (guard) or
;;; event.  It then sets the l-value, r-value and event
;;; fields of the BT node accordingly.

(defun set-l-value-and-r-value-event (bt-node)
  (let ((type (bt-node-type bt-node))
        (component-name (bt-node-component bt-node))
        (behaviour (bt-node-behaviour bt-node))
        (token-class (bt-node-behaviour-type bt-node)))
    (cond
      ((eq type 'update)
       (cond
         ((or (and (> (length behaviour) 7)
                   (equal (substring behaviour 0 3) "||&"))
              (and (> (length behaviour) 6)
                   (equal (substring behaviour 0 2) "[]"))
              (and (> (length behaviour) 6)
                   (equal (substring behaviour 0 2) "||")))
          ;; **** For now allow quantification be specified using #S
          ;; convert to quantification and q-token (#A)
          (setf (bt-node-type bt-node) 'quantification)
          (setf (bt-node-behaviour-type bt-node) 'q-token)
          (setf (bt-node-l-value bt-node) nil)
          (setf (bt-node-r-value bt-node) nil)
          (setf (bt-node-event bt-node) nil))
         (t
          (let ((i (string-search= ":=" behaviour)))
            (cond ((null i)
                   (setf (bt-node-l-value bt-node)
                         (make-name (list "SV-" component-name)))
                   (setf (bt-node-r-value bt-node)
                         (make-name (list "VAL-" behaviour))))
                  (t
                   (setf (bt-node-l-value bt-node)
                         (make-name (list "SV-" component-name "."
                                          (substring behaviour 0 i))))
                   (setf (bt-node-r-value bt-node)
                         (make-name (list "VAL-" (substring
                                                   behaviour (+ i 2))))))))
           (setf (bt-node-event bt-node) nil))))
      ((or (eq type 'select) (eq type 'guard))
       (setf (bt-node-l-value bt-node) nil)
       (cond
         ((behaviour-is-negated behaviour)
          (let* ((beh (negated-behaviour behaviour))
                 (i (string-search= "=" beh)))
            (cond
              ((null i)
               (setf (bt-node-r-value bt-node)
                     `(not (= ,(make-name (list "SV-" component-name))
                              ,(make-name (list "VAL-" beh))))))
              (t
               (setf (bt-node-r-value bt-node)
                     `(not (= ,(make-name
                                (list "SV-" component-name "."
                                      (substring beh 0 i)))
                              ,(make-name
                                (list "VAL-" (substring
                                               beh (+ i 1)))))))))))
         ((bt-node-is-else-selection bt-node)
          (setf (bt-node-r-value bt-node) *true*))
         (t
          (let ((i (string-search= "=" behaviour)))
            (cond ((null i)
                   (setf (bt-node-r-value bt-node)
                         `(= ,(make-name (list "SV-" component-name))
                             ,(make-name (list "VAL-" behaviour)))))
                  (t
                   (setf (bt-node-r-value bt-node)
                         `(= ,(make-name
                                (list "SV-" component-name "."
                                      (substring behaviour 0 i)))
                             ,(make-name
                                (list "VAL-" (substring
                                               behaviour (+ i 1)))))))))))
       (setf (bt-node-event bt-node) nil))
      ((eq type 'event)
       (setf (bt-node-l-value bt-node) nil)
       (setf (bt-node-r-value bt-node) nil)
       (setf (bt-node-event bt-node)
             (make-name (list (cond ((eq token-class 'e-token) "EV-")
                                    ((eq token-class 'ei-token) "EI-")
                                    ((eq token-class 'eo-token) "EO-"))
                               component-name "." behaviour))))
      ((or (eq type 'input) (eq type 'output))
       (setf (bt-node-l-value bt-node) nil)
       (setf (bt-node-r-value bt-node) nil)
       (setf (bt-node-event bt-node)
             (make-name (list "IO-" behaviour))))
      ((eq type 'quantification)
       (setf (bt-node-l-value bt-node) nil)
       (setf (bt-node-r-value bt-node) nil)
       (setf (bt-node-event bt-node) nil)))))

(defun bt-node-is-else-selection (bt-node)
  (and (eq (bt-node-type bt-node) 'select)
       (null (bt-node-atomic bt-node))

       ;; must be the last of alternative branches
       (not (null (bt-node-parent bt-node)))
       (null (bt-node-atomic (bt-node-parent bt-node)))
       (eq (bt-node-branch-type (bt-node-parent bt-node)) 'alternative)
       (eq (car (last (bt-node-children (bt-node-parent bt-node)))) bt-node)

       (or (equal (bt-node-behaviour bt-node) "ELSE")
           (equal (bt-node-behaviour bt-node) "else")
           (equal (bt-node-behaviour bt-node) "Else"))))

(defun behaviour-is-negated (behaviour)
  (and (>= (length behaviour) 6)
       (equal (string-upcase (substring behaviour 0 4)) "NOT(")
       (equal (substring behaviour (- (length behaviour) 1)) ")")))

(defun negated-behaviour (behaviour)
  (substring behaviour 4 (- (length behaviour) 1)))

(defun make-name (list)
  (let ((str ""))
    (loop for s in list
          do (setq str (string-append str s)))
    (intern str *package*)))




;;; Predicate to determine whether token can start a BT node
;;; in the lexical structure of TextBT

(defun token-starts-node-p (token)
  (or (is-requirement-status-token token)
      (and (eq (token-class token) 'value-token)
           (or (multiple-value-bind (value found)
                    (gethash (token-string token)
                             *bt-requirement-labels*)
                  value found)
               (multiple-value-bind (value found)
                    (gethash (token-string token) *bt-components*)
                  value found)))))

;;; Function to parse branches after a BT node with branching is parsed.
;;; The branches are terminated by a matching close-brace.
;;; Note that branches may be nested.

(defun parse-bt-branches (token-list parent-node)
  (let ((bt-node nil)
        (remaining-token-list token-list))
    (let ((children
           (loop while
                 (and (not (null remaining-token-list))
                      (not (eq (token-class (car remaining-token-list))
                               'close-brace-token)))
                 collect
                 (progn
                   (multiple-value-setq (bt-node remaining-token-list)
                     (parse-bt remaining-token-list))
                   bt-node))))
      (setf (bt-node-children parent-node) children)
      (loop for node in children
            do (setf (bt-node-parent node) parent-node))
      ;; cdr of nil is nil, so works out ok
      (cdr remaining-token-list))))

;;; Print a parsing error message.

(defun parse-error (error-type &optional token)
  (cond ((null token)
         ;; this can only happen if error-type is unexpected-eof
         (setq *fatal-error* t)
         (format *error-output* "Fatal parsing error, unexpected EOF.~%"))
        (t (format *error-output*
                   "WARNING: parsing error (~A): ~A at line ~A.~%"
                   error-type (token-string token)
                   (token-line-number token)))))



(defun parse-top-level (token-list)
  (reset-parser)
  (let ((remaining-token-list token-list))
    (loop
      while (not (null remaining-token-list))
      do
      (let* ((token (car remaining-token-list))
             (token-class (token-class token)))
        ;; always reset the tree if parsing other than the tree
        (cond ((eq token-class 'rt-token)
               (setq *bt-root* nil)
               (setq remaining-token-list
                     (parse-requirement-labels
                       (cdr remaining-token-list))))
              ((eq token-class 'c-token)
               (setq *bt-root* nil)
               (setq remaining-token-list
                     (parse-component (cdr remaining-token-list) nil)))
              ((eq token-class 'sc-token)
               (setq *bt-root* nil)
               (setq remaining-token-list
                     (parse-component (cdr remaining-token-list) t)))
              ((eq token-class 't-token)
               (setq *bt-root* nil)
               (multiple-value-setq (*bt-root* remaining-token-list)
                  (parse-bt (cdr remaining-token-list))))
              (t (parse-error 'top-level token)
                 (setq remaining-token-list
                       (cdr remaining-token-list)))))))
  (when (and (null *bt-root*) (not *fatal-error*))
    (parse-error 'unexpected-eof))
  (when *fatal-error* (setq *bt-root* nil))
  *bt-root*)



(defun print-bt-node (bt-node)
  (format t "~%===== BT Node =====~%")
  (format t "Type: ~A~%" (bt-node-type bt-node))
  (if (null (bt-node-requirement-status bt-node))
      (format t "Tag: ~A~%" (bt-node-requirement-label bt-node))
      (format t "Tag: ~A ~A~%" (bt-node-requirement-label bt-node)
                               (bt-node-requirement-status bt-node)))
  (format t "Component: ~A~%" (bt-node-component bt-node))
  (format t "Behaviour: ~A~%" (bt-node-behaviour bt-node))
  ;(format t "Behaviour Type: ~A~%" (bt-node-behaviour-type bt-node))
  ;(unless (null (bt-node-l-value bt-node))
  ;  (format t "L-value: ~A~%" (bt-node-l-value bt-node)))
  ;(unless (null (bt-node-r-value bt-node))
  ;  (format t "R-value: ~A~%" (bt-node-r-value bt-node)))
  (unless (null (bt-node-event bt-node))
    (format t "Event: ~A~%" (bt-node-event bt-node)))
  (unless (null (bt-node-label bt-node))
    (format t "Label: ~A~%" (bt-node-label bt-node)))
  (unless (null (bt-node-flag bt-node))
    (format t "Flag: ~A~%" (bt-node-flag bt-node)))
  (unless (null (bt-node-branch-type bt-node))
    (format t "Branch Type: ~A~%" (bt-node-branch-type bt-node)))
  (format t "Number of Children: ~A~%" (length (bt-node-children bt-node)))
  (when (bt-node-atomic bt-node)
    (format t "Atomic~%")))

(defun print-bt-node-summary (bt-node)
  (if (null (bt-node-requirement-status bt-node))
      (format t "~A | " (bt-node-requirement-label bt-node))
      (format t "~A ~A | " (bt-node-requirement-label bt-node)
                               (bt-node-requirement-status bt-node)))
  (format t "~A " (bt-node-component bt-node))
  (format t (bt-behaviour-open bt-node))
  (format t "~A" (bt-node-behaviour bt-node))
  (format t (bt-behaviour-close bt-node))
  (format t (print-bt-node-flag bt-node))
  (format t "~%"))

(defun bt-behaviour-open (bt-node)
  (let ((token (bt-node-behaviour-type bt-node)))
    (cond ((eq token 's-token) "[")
          ((eq token 'l-token) "?")
          ((eq token 'g-token) "???")
          ((eq token 'e-token) "??")
          ((eq token 'ii-token) ">")
          ((eq token 'io-token) "<")
          ((eq token 'ei-token) ">>")
          ((eq token 'eo-token) "<<")
          (t ""))))

(defun bt-behaviour-close (bt-node)
  (let ((token (bt-node-behaviour-type bt-node)))
    (cond ((eq token 's-token) "]")
          ((eq token 'l-token) "?")
          ((eq token 'g-token) "???")
          ((eq token 'e-token) "??")
          ((eq token 'ii-token) "<")
          ((eq token 'io-token) ">")
          ((eq token 'ei-token) "<<")
          ((eq token 'eo-token) ">>")
          (t ""))))

(defun print-bt-node-flag (bt-node)
  (let ((flag (bt-node-flag bt-node)))
    (cond ((eq flag 'kill) " --")
          ((eq flag 'reversion) " ^")
          ((eq flag 'reference) " =>")
          ((eq flag 'synchronise) " =")
          (t ""))))
