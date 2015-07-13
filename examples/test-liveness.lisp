
(in-package "bt")

(set-priority nil)

(process-bt-file "examples/example2.bt")

(defvar *thm* '(and (g (f (= |SV-Light| |VAL-green|)))
                    (g (f (= |SV-Light| |VAL-red|)))))

(ltl-check *thm*)

(find-counterexample-path "5")

(find-counterexample-path "10")

(print-deadlocked-states)
