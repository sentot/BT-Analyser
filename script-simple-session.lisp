(load "load-all.lisp")

(in-package "bt")

(process-bt-file "examples/model2_120119.bt")

(ltl-check '(g (not (and (not (= |SV-Calculator| |VAL-calculateDS|))
                         (= |SV-MetData| |VAL-pending|)
                         (x (= |SV-Calculator| |VAL-calculateDS|))))))

(find-counterexample-path)

(print-history)

(print-event 5)
