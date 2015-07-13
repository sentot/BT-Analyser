
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



(defun xml-print-formula (formula)
  (string-append "<formula>"
                 #\Newline
                 (xml-print-formula-content formula)
                 #\Newline
                 "</formula>"))

(defun xml-print-formula-content (formula)
  (cond ((numberp formula)
         (string-append "<constant>" (format nil "~S" formula) "</constant>"))
        ((atom formula)
         (string-append "<variable>" (format nil "~S" formula) "</variable>"))
        ((and-p formula)
         (string-append
           "<and>"
           #\Newline
           (apply #'string-append
                  (loop for expr in (cdr formula)
                        collect
                        (string-append (xml-print-formula-content expr)
                                       #\Newline)))
           "</and>"))
        ((or-p formula)
         (string-append
           "<or>"
           #\Newline
           (apply #'string-append
                  (loop for expr in (cdr formula)
                        collect
                        (string-append (xml-print-formula-content expr)
                                       #\Newline)))
           "</or>"))
        ((implies-p formula)
         (string-append
           "<implies>"
           #\Newline
           (apply #'string-append
                  (loop for expr in (cdr formula)
                        collect
                        (string-append (xml-print-formula-content expr)
                                       #\Newline)))
           "</implies>"))
        ((not-p formula)
         (string-append
           "<not>"
           #\Newline
           (xml-print-formula-content (second formula))
           #\Newline
           "</not>"))
        ((true-p formula) "<constant>TRUE</constant>")
        ((false-p formula) "<constant>FALSE</constant>")
        ((=-p formula)
         (string-append
           "<equality>"
           (xml-print-formula-content (second formula))
           (xml-print-constant (third formula))
           "</equality>"))
        ((modal-p formula)
         (string-append
           "<temporal-operation>"#\Newline
           "<temporal-operator>"
           (format nil "~S" (car formula))
           "</temporal-operator>"
           #\Newline
           (apply #'string-append
                  (loop for expr in (cdr formula)
                        collect
                        (string-append (xml-print-formula-content expr)
                                       #\Newline)))
           "</temporal-operation>"))
           
        (t
         (string-append
           "<function-application>"
           #\Newline
           "<function>"
           (format nil "~S" (car formula))
           "</function>"
           #\Newline
           (apply #'string-append
                  (loop for expr in (cdr formula)
                        collect
                        (string-append (xml-print-formula-content expr)
                                       #\Newline)))
           "</function-application>"))))

(defun xml-print-constant (formula)
  (string-append "<constant>" (format nil "~S" formula) "</constant>"))


(defun xml-print-path (path)
  (string-append
    "<path>"
    #\Newline
    (apply #'string-append
           (loop for i in path
                 collect
                 (string-append "<block-index>"
                                (format nil "~S" i)
                                "</block-index>"
                                #\Newline)))
    "</path>"))

          
(defun xml-print-block (bt-block)
  (string-append "<block>"
                 #\Newline
                 "<block-index>"
                 (format nil "~S" (bt-block-index bt-block))
                 "</block-index>"
                 "<block-type>"
                 (if (bt-block-special bt-block)
                     "ELSE"
                     "NORMAL")
                 "</block-type>"
                 #\Newline
                 (apply #'string-append
                        (loop for bt-node in (bt-block-bt-nodes bt-block)
                              collect
                              (string-append (xml-print-node bt-node)
                                             #\Newline)))
                 (let ((last-node (car (last (bt-block-bt-nodes bt-block)))))
                   (string-append "<branch-type>"
                                  (if (null (bt-node-branch-type last-node))
                                      "NONE"
                                      (format nil "~A"
                                              (bt-node-branch-type last-node)))
                                  "</branch-type>"))
                 "<children>"
                 (apply #'string-append
                        (loop for child in (bt-block-children bt-block)
                              collect
                              (string-append "<block-index>"
                                             (format nil "~S"
                                                     (bt-block-index child))
                                             "</block-index>")))
                 "</children>"
                 #\Newline
                 "<parent>"
                 (if (null (bt-block-parent bt-block))
                     ""
                     (string-append
                       "<block-index>"
                       (format nil "~S"
                               (bt-block-index (bt-block-parent bt-block)))
                       "</block-index>"))
                 "</parent>"
                 #\Newline
                 "</block>"))

(defun xml-print-node (bt-node)
  (string-append "<node>"
                 #\Newline
                 "<tag>"
                 (if (null (bt-node-requirement-status bt-node))
                     (format nil "~A" (bt-node-requirement-label bt-node))
                     (format nil "~A ~A"
                             (bt-node-requirement-label bt-node)
                             (bt-node-requirement-status bt-node)))
                 "</tag>"
                 #\Newline
                 "<component>"
                 (format nil "~A" (bt-node-component bt-node))
                 "</component>"
                 "<behaviour-type>"
                 (xml-behaviour-type bt-node)
                 "</behaviour-type>"
                 "<behaviour>"
                 (format nil "~A" (bt-node-behaviour bt-node))
                 "</behaviour>"
                 "<flag>"
                 (if (null (bt-node-flag bt-node))
                     ""
                     (format nil "~A" (bt-node-flag bt-node)))
                 "</flag>"
                 #\Newline
                 "</node>"))

(defun xml-behaviour-type (bt-node)
  (let ((token (bt-node-behaviour-type bt-node)))
    (cond ((eq token 's-token) "STATE-REALISATION")
          ((eq token 'l-token) "SELECTION")
          ((eq token 'g-token) "GUARD")
          ((eq token 'e-token) "EVENT")
          ((eq token 'ii-token) "INTERNAL-INPUT")
          ((eq token 'io-token) "INTERNAL-OUTPUT")
          ((eq token 'ei-token) "EXTERNAL-INPUT")
          ((eq token 'eo-token) "EXTERNAL-OUTPUT")
          (t "UNKNOWN"))))

(defun xml-print-event (event)
  (string-append
    "<event>"
    (cond ((event-model-p event)
           (string-append "<bt>"
                          (event-model-bt-filename event)
                          "</bt>"))
          ((event-reachable-states-p event)
           "<reachable-states></reachable-states>")
          ((event-ltl-specification-p event)
           (string-append #\Newline
                          "<ltl-specification>"
                          #\Newline
                          (xml-print-formula
                            (event-ltl-specification-formula event))
                          #\Newline
                          "</ltl-specification>"
                          #\Newline))
          ((event-fair-states-p event)
           (string-append
             #\Newline
             "<fair-states>"
             (if (null (event-fair-states-gc event))
                 ""
                 (string-append #\Newline
                                "<global-constraint>"
                                #\Newline
                                (xml-print-formula (event-fair-states-gc event))
                                #\Newline
                                "</global-constraint>"
                                #\Newline))
             "</fair-states>"))
          ((event-counterexample-states-p event)
           (string-append
             #\Newline
             "<counterexample-states>"
             (if (null (event-counterexample-states-gc event))
                 ""
                 (string-append
                   #\Newline
                   "<global-constraint>"
                   #\Newline
                   (xml-print-formula (event-counterexample-states-gc event))
                   #\Newline
                   "</global-constraint>"
                   #\Newline))
             "</counterexample-states>"))
          ((event-counterexample-path-p event)
           (string-append
             #\Newline
             "<counterexample-path>"
             #\Newline
             (if (null (event-counterexample-path-gc event))
                 ""
                 (string-append
                   "<global-constraint>"
                   #\Newline
                   (xml-print-formula (event-counterexample-path-gc event))
                   #\Newline
                   "</global-constraint>"
                   #\Newline))
             (if (null (event-counterexample-path-cc event))
                 ""
                 (string-append
                   "<cycle-constraint>"
                   #\Newline
                   (xml-print-formula (event-counterexample-path-cc event))
                   #\Newline
                   "</cycle-constraint>"
                   #\Newline))
             (let* ((ctr-path (event-counterexample-path-path event))
                    (prefix (mapcar #'car (second ctr-path)))
                    (cycle (mapcar #'car (fourth ctr-path))))
               (string-append "<prefix>"
                              #\Newline
                              (xml-print-path prefix)
                              #\Newline
                              "</prefix>"
                              #\Newline
                              "<cycle>"
                              #\Newline
                              (xml-print-path cycle)
                              #\Newline
                              "</cycle>"
                              #\Newline))
             "</counterexample-path>"
             #\Newline)))
    "</event>"))

(defun xml-do-process-bt-file (arguments)
  (let ((filename (car arguments)))
    (cond ((null (probe-file filename))
           (string-append "<result>File '"
                          filename
                          "' does not exists.</result>"))
          (t
           (let ((result nil))
             (let ((error-messages
                    (with-output-to-string (s)
                      (let ((*error-output* s))
                        (setq result
                              (apply #'process-bt-file arguments))))))
               (cond ((null result)
                      ;; fatal error
                      (string-append "<result><error>"
                                     error-messages
                                     "</error></result>"))
                     ((> (length error-messages) 0)
                      (string-append "<result><warning>"
                                     error-messages
                                     "</warning></result>"))
                     (t "<result>SUCCESS</result>"))))))))

(defun xml-do-process-bt (arguments)
  (let ((input-string (car arguments)))
    (cond ((not (stringp input-string))
           "<result>Invalid input string</result>")
          (t
           (let ((result nil))
             (let ((error-messages
                    (with-output-to-string (s)
                      (let ((*error-output* s))
                        (setq result
                              (apply #'process-bt arguments))))))
               (cond ((null result)
                      ;; fatal error
                      (string-append "<result><error>"
                                     error-messages
                                     "</error></result>"))
                     ((> (length error-messages) 0)
                      (string-append "<result><warning>"
                                     error-messages
                                     "</warning></result>"))
                     (t "<result>SUCCESS</result>"))))))))

(defun xml-do-reachable-states ()
  (let ((result (reachable-states)))
    (cond ((null result) "<result>FAILED</result>")
          (t "<result>SUCCESS</result>"))))

(defun xml-do-find-test-paths (arguments)
  (let ((result (apply #'find-test-paths arguments)))
    (string-append "<result>"
                   #\Newline
                   (apply #'string-append
                          (loop for path in result
                                collect
                                (string-append (xml-print-path path)
                                             #\Newline)))
                   "</result>")))

(defun xml-do-check-test-path (arguments)
  (let ((result (apply #'check-test-path arguments)))
    (cond ((null result) "<result>INFEASIBLE</result>")
          (t "<result>FEASIBLE</result>"))))

(defun xml-do-indices-of-update-blocks (arguments)
  (let ((result (apply #'indices-of-update-blocks arguments)))
    (string-append "<result>"
                   (apply #'string-append
                          (loop for index in result
                                collect
                                (string-append "<block-index>"
                                               (format nil "~A" index)
                                               "</block-index>")))
                   "</result>")))

(defun xml-do-test-path-precondition (arguments)
  (let ((result (apply #'test-path-precondition arguments)))
    (string-append "<result>"
                   #\Newline
                   (xml-print-formula result)
                   #\Newline
                   "</result>")))

(defun xml-do-test-path-postcondition (arguments)
  (let ((result (apply #'test-path-postcondition arguments)))
    (string-append "<result>"
                   #\Newline
                   (xml-print-formula result)
                   #\Newline
                   "</result>")))

(defun xml-do-test-path-state (arguments)
  (let ((result (apply #'test-path-state arguments)))
    (string-append "<result>"
                   #\Newline
                   (xml-print-formula result)
                   #\Newline
                   "</result>")))

(defun xml-do-test-path-preamble (arguments)
  (let ((result (apply #'test-path-preamble arguments)))
    (cond ((null result)
           "<result>FAILED</result>")
          (t
           (string-append "<result>"
                          #\Newline
                          (xml-print-path result)
                          #\Newline
                          "</result>")))))

(defun xml-do-test-path-postamble (arguments)
  (let ((result (apply #'test-path-postamble arguments)))
    (cond ((null result)
           "<result>FAILED</result>")
          (t
           (string-append "<result>"
                          #\Newline
                          (xml-print-path result)
                          #\Newline
                          "</result>")))))

(defun xml-do-print-block (arguments)
  (cond
    ((null *history*)
     "<result><error>No Behavior Tree loaded.</error></result>")
    (t
     (let ((index (car arguments))
           (bt-block-array (event-model-bt-block-array (car (last *history*)))))
       (cond ((and (integerp index) (>= index 0)
                   (<= index (length bt-block-array)))
              (string-append "<result>"
                             #\Newline
                             (xml-print-block (aref bt-block-array index))
                             #\Newline
                             "</result>"))
             (t "<result><error>Invalid block index.</error></result>"))))))

(defun xml-do-print-bt ()
  (cond
    ((null *history*)
     "<result><error>No Behavior Tree loaded.</error></result>")
    (t
     (let ((bt-block-array (event-model-bt-block-array (car (last *history*)))))
       (string-append
         "<result>"
         #\Newline
         (apply #'string-append
                (loop for index from 0 to (- (length bt-block-array) 1)
                      collect
                      (string-append
                        (xml-print-block (aref bt-block-array index))
                        #\Newline)))
         "</result>")))))

(defun xml-do-ltl-specification (arguments)
  (let ((result nil))
    (let ((error-messages
            (with-output-to-string (s)
              (let ((*error-output* s))
                    (setq result (apply #'ltl-specification arguments))))))
      (cond ((null result)
             ;; fatal error
             (string-append "<result><error>"
                            error-messages
                            "</error></result>"))
            ((> (length error-messages) 0)
             (string-append "<result><warning>"
                            error-messages
                            "</warning></result>"))
            (t "<result>SUCCESS</result>")))))

(defun xml-do-fair-states (arguments)
  (let ((result (apply #'fair-states arguments)))
    (cond ((not (null result))
           "<result>SUCCESS</result>")
          (t
           "<result>FAILED</result>"))))

(defun xml-do-counterexample-states (arguments)
  (let ((result (apply #'counterexample-states arguments)))
    (cond ((not (null result))
           "<result>SUCCESS</result>")
          (t
           "<result>FAILED</result>"))))

(defun xml-do-find-counterexample-path (arguments)
  (let ((label (first arguments))
        (cc (second arguments))
        (gc (third arguments)))
    (cond ((and (not (null label))
                (not (stringp label)))
           "<result><error>Invalid label.</error></result>")
          ((and (not (null cc))
                (not (wfcheck-proposition cc)))
           "<result><error>Invalid cycle constraint.</error></result>")
          ((and (not (null gc))
                (not (wfcheck-proposition gc)))
           "<result><error>Invalid global constraint.</error></result>")
          (t (let ((result (apply #'find-counterexample-path arguments)))
               (cond ((null result) "<result>FAILED</result>")
                     (t "<result>SUCCESS</result>")))))))

(defun xml-do-print-event (arguments)
  (cond ((and (listp arguments)
              (= (length arguments) 1)
              (integerp (car arguments))
              (>= (car arguments) 1)
              (<= (car arguments) (length *history*)))
         (string-append "<result>"
                        #\Newline
                        (xml-print-event
                          (nth (- (length *history*) (car arguments))
                               *history*))
                        #\Newline
                        "</result>"))
         (t "<result><error>Invalid index for print-event.</error></result>")))

(defun xml-do-print-history ()
  (string-append "<result>"
                 #\Newline
                 (apply #'string-append
                        (loop for event in (reverse *history*)
                              collect
                              (string-append (xml-print-event event)
                                             #\Newline)))
                 "</result>"))

(defun xml-do-command (command)
  (cond ((not (listp command))
         "<result><error>Syntax error.</error></result>")
        ((and (eq (car command) 'process-bt-file)
              (= (length command) 2)
              (stringp (second command)))
         (xml-do-process-bt-file (cdr command)))
        ((and (eq (car command) 'process-bt)
              (= (length command) 2))
         (xml-do-process-bt (cdr command)))
        ((and (eq (car command) 'reachable-states)
              (= (length command) 1))
         (xml-do-reachable-states))
        ((and (eq (car command) 'find-test-paths)
              (= (length command) 5))
         (cond ((and (check-indices (list (second command)))
                     (check-indices (third command))
                     (check-indices (list (fourth command)))
                     (check-indices (fifth command)))
                (xml-do-find-test-paths (cdr command)))
               (t "<result><error>Index error.</error></result>")))
        ((and (eq (car command) 'check-test-path)
              (= (length command) 2))
         (cond ((check-indices (second command))
                (xml-do-check-test-path (cdr command)))
               (t "<result><error>Index error.</error></result>")))
        ((and (eq (car command) 'indices-of-update-blocks)
              (or (and (= (length command) 2) (stringp (second command)))
                  (and (= (length command) 3)
                       (stringp (second command))
                       (stringp (third command)))))
         (xml-do-indices-of-update-blocks (cdr command)))
        ((and (eq (car command) 'test-path-precondition)
              (= (length command) 2))
         (cond ((check-indices (second command))
                (xml-do-test-path-precondition (cdr command)))
               (t "<result><error>Index error.</error></result>")))
        ((and (eq (car command) 'test-path-postcondition)
              (= (length command) 2))
         (cond ((check-indices (second command))
                (xml-do-test-path-postcondition (cdr command)))
               (t "<result><error>Index error.</error></result>")))
        ((and (eq (car command) 'test-path-state)
              (= (length command) 3))
         (cond ((and (check-indices (second command))
                     (check-indices (third command)))
                (xml-do-test-path-state (cdr command)))
               (t "<result><error>Index error.</error></result>")))
        ((and (eq (car command) 'test-path-preamble)
              (= (length command) 3))
         (cond ((integerp (second command))
                (check-indices (list (second command)))
                (check-indices (third command))
                (xml-do-test-path-preamble (cdr command)))
               (t "<result><error>Index error.</error></result>")))
        ((and (eq (car command) 'test-path-postamble)
              (= (length command) 3))
         (cond ((integerp (second command))
                (check-indices (list (second command)))
                (check-indices (third command))
                (xml-do-test-path-postamble (cdr command)))
               (t "<result><error>Index error.</error></result>")))
        ((and (eq (car command) 'print-block)
              (= (length command) 2))
         (cond ((check-indices (cdr command))
                (xml-do-print-block (cdr command)))
               (t "<result><error>Index error.</error></result>")))
        ((and (eq (car command) 'print-bt)
              (= (length command) 1))
         (xml-do-print-bt))
        ((and (eq (car command) 'ltl-specification)
              (= (length command) 2))
         (xml-do-ltl-specification (cdr command)))
        ((and (eq (car command) 'fair-states)
              (<= (length command) 2))
         (cond ((and (not (null (second command)))
                     (not (wfcheck-proposition (second command))))
                "<result><error>Invalid global constraint.</error></result>")
               (t (xml-do-fair-states (cdr command)))))
        ((and (eq (car command) 'counterexample-states)
              (<= (length command) 2))
         (cond ((and (not (null (second command)))
                     (not (wfcheck-proposition (second command))))
                "<result><error>Invalid global constraint.</error></result>")
               (t (xml-do-counterexample-states (cdr command)))))
        ((and (eq (car command) 'find-counterexample-path)
              (<= (length command) 4))
         (xml-do-find-counterexample-path (cdr command)))
        ((and (eq (car command) 'print-event)
              (= (length command) 2))
         (xml-do-print-event (cdr command)))
        ((and (eq (car command) 'print-history)
              (= (length command) 1))
         (xml-do-print-history))
        ((and (eq (car command) 'set-priority)
              (<= (length command) 2))
         (apply #'set-priority (cdr command))
         "<result>SUCCESS</result>")
        ((and (eq (car command) 'set-goto-semantics)
              (<= (length command) 2))
         (apply #'set-goto-semantics (cdr command))
         "<result>SUCCESS</result>")
        (t
         "<result><error>Syntax error.</error></result>")))

(defun check-indices (indices)
  (unless (or (null *history*) (not (listp indices)))
    (let ((size (length (event-model-bt-block-array (car (last *history*))))))
      (every #'(lambda (x) (and (integerp x) (>= x 0) (< x size)))
             indices))))
