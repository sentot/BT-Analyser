
(in-package "bt")

(process-bt-file "examples/model3_120119.bt")

(defvar *thm* '(g (not (and (not (= |SV-Calculator| |VAL-calculateDS|))
                            (= |SV-MetData| |VAL-pending|)
                            (x (= |SV-Calculator| |VAL-calculateDS|))))))

(ltl-check *thm*)

(theorem-is-proved)
