(in-package "bt")

;;; This file simply contains definitions for the minimal cut set
;;; problem. The files a320-version-1.lisp and a320-version-2.lisp
;;; are already self-contained and do not need this file.

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

(defvar *at-least-two-failures*
        (at-least-n 2 *failures-list*))

(defvar *at-least-three-failures*
        (at-least-n 3 *failures-list*))

(defvar *total-loss* '(and (= |SV-Yellow| |VAL-off|)
                           (= |SV-Green| |VAL-off|)
                           (= |SV-Blue| |VAL-off|)))

(defvar *at-least-one-active* (at-least-n 1 *active-list*))

(defvar *at-least-two-active* (at-least-n 2 *active-list*))

(defvar *all-three-active* (at-least-n 3 *active-list*))

(defvar *exactly-one-active*
        '(or (and (= |SV-Yellow| |VAL-active|)
                  (= |SV-Green| |VAL-off|)
                  (= |SV-Blue| |VAL-off|))
             (and (= |SV-Yellow| |VAL-off|)
                  (= |SV-Green| |VAL-active|)
                  (= |SV-Blue| |VAL-off|))
             (and (= |SV-Yellow| |VAL-off|)
                  (= |SV-Green| |VAL-off|)
                  (= |SV-Blue| |VAL-active|))))

(defvar *ssr2*
        `(g (implies (and (= pc1 3) ,*all-three-active*)
                     (g (implies ,*exactly-one-active*
                                 ,*at-least-two-failures*)))))

(defvar *ssr2v*
        `(g (implies (and (= pc1 3) ,*all-three-active*)
                     (g ,*at-least-two-active*))))

(defvar *test* `(g (or (= pc1 1) (not ,*all-three-active*))))

