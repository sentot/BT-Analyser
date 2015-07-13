(in-package "bt")

(defvar *failures-list*
  '((= |SV-System.distyFailed| |VAL-true|)
    (= |SV-System.distgFailed| |VAL-true|)
    (= |SV-System.distbFailed| |VAL-true|)
    (= |SV-System.E1Failed| |VAL-true|)
    (= |SV-System.E2Failed| |VAL-true|)
    (= |SV-System.PTUFailed| |VAL-true|)
    (= |SV-System.EDPyFailed| |VAL-true|)
    (= |SV-System.EDPgFailed| |VAL-true|)
    (= |SV-System.EMPbFailed| |VAL-true|)
    (= |SV-System.EMPyFailed| |VAL-true|)
    (= |SV-System.RATFailed| |VAL-true|)))


(defvar *active-list*
  '((= |SV-Yellow| |VAL-active|)
    (= |SV-Green| |VAL-active|)
    (= |SV-Blue| |VAL-active|)))

(defun at-least-n-aux (n list)
  (cond ((= n 1) (mapcar #'(lambda (x) (list x)) list))
        ((= n (length list)) (list list))
        ((< n (length list))
         (append (mapcar #'(lambda (x) (cons (car list) x))
                         (at-least-n-aux (- n 1) (cdr list)))
                 (at-least-n-aux n (cdr list))))))

(defun at-least-n (n list)
  (cond ((< n 1) *true*)
        ((= n 1)
         (cond ((> (length list) 1) (cons 'or list))
               ((= (length list) 1) (car list))
               (t *false*)))
        ((= n (length list))
         (cond ((> n 1) (cons 'and list))
               (t *false*)))
        ((< n (length list))
         (cons 'or
               (loop for entry in (at-least-n-aux n list)
                     collect (cons 'and entry))))))

(defvar *at-least-two-active* (at-least-n 2 *active-list*))

(defvar *all-three-active* (at-least-n 3 *active-list*))

(defvar *ssr2v*
        `(g (implies (and (= pc1 3) ,*all-three-active*)
                     (g ,*at-least-two-active*))))

(defvar *no-failures-list*
  '((= |SV-System.distyFailed| |VAL-false|)
    (= |SV-System.distgFailed| |VAL-false|)
    (= |SV-System.distbFailed| |VAL-false|)
    (= |SV-System.E1Failed| |VAL-false|)
    (= |SV-System.E2Failed| |VAL-false|)
    (= |SV-System.PTUFailed| |VAL-false|)
    (= |SV-System.EDPyFailed| |VAL-false|)
    (= |SV-System.EDPgFailed| |VAL-false|)
    (= |SV-System.EMPbFailed| |VAL-false|)
    (= |SV-System.EMPyFailed| |VAL-false|)
    (= |SV-System.RATFailed| |VAL-false|)))


(defun less-one-failure (cs)
  (cond ((= (length cs) 1)
         (list '= (second (car cs)) '|VAL-false|))
        ((> (length cs) 1)
         (cons 'or
               (loop for x in cs
                     collect (list '= (second x) '|VAL-false|))))))

(defun generate-cs-test (cs)
  (let ((rest (loop for x in *no-failures-list*
                    when (not (member-equal (list '= (second x) '|VAL-true|)
                                            cs))
                    collect x)))
    (cond ((null rest)
           (less-one-failure cs))
          (t
           (cons 'and (append rest (list (less-one-failure cs))))))))

(defun generate-cut-set-from-ctr (ctr-event)
  (when (event-counterexample-path-p ctr-event)
    (let ((bdd-index (third (event-counterexample-path-path ctr-event)))
          (pc-infos (event-model-pc-infos (car (last *history*))))
          (state-vars (event-model-state-vars (car (last *history*)))))
      (let ((formula (unencode-formula pc-infos state-vars
                                       (bdd-to-cnf (bdd-entry bdd-index)))))
        (when (and-p formula)
          (loop for conjunct in (cdr formula)
                when (member-equal conjunct *failures-list*)
                collect conjunct))))))

(defun generate-constraint-from-cut-set (cut-set)
  (make-not (cons 'and cut-set)))

(process-bt-file "examples/a320.bt")

(ltl-check *ssr2v*)

(counterexample-states)

(defun find-cut-sets ()
  (let ((cut-sets nil)
        (constraint *true*)
        (done nil))
    (loop
      while (not done)
      do
      (let ((ctr-found (find-counterexample-path nil nil constraint)))
        (cond
          ((null ctr-found)
           (theorem-is-proved constraint)
           (setq done t))
          (t
           (let ((cut-set (generate-cut-set-from-ctr (car *history*)))
                 (is-cut-set nil))
             (loop
               while (null is-cut-set)
               do
               (let* ((test (generate-cs-test cut-set))
                      (proved (theorem-is-proved test)))
                 (cond
                   ((null proved)
                    (let ((ctr (find-counterexample-path nil nil test)))
                      (cond
                        ((null ctr)
                         ;; *** this should not happen
                         (format t "Problem with CS generation.~%")
                         (setq is-cut-set t)
                         (setq done t))
                        (t
                         (setq cut-set
                               (generate-cut-set-from-ctr (car *history*)))))))
                   (t
                    (if (equal constraint *true*)
                        (setq constraint
                              (generate-constraint-from-cut-set cut-set))
                        (setq constraint
                              (make-and
                                constraint
                                (generate-constraint-from-cut-set cut-set))))
                    (push cut-set cut-sets)
                    (setq is-cut-set t))))))))))
    (loop for i from 1 to (length *failures-list*)
          do
          (let ((cs-list (loop for cs in cut-sets
                               when (= (length cs) i)
                               collect cs)))
            (unless (null cs-list)
              (format t "~S-element cut-sets:~%" i)
              (loop for cs in (reverse cs-list)
                    do
                    (format t "~S~%" cs)))))))

(find-cut-sets)
