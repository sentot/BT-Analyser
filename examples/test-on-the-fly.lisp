
(in-package "bt")

(process-bt-file "examples/model2_120119.bt")

(defvar *thm* '(g (not (and (not (= |SV-Calculator| |VAL-calculateDS|))
                                 (= |SV-MetData| |VAL-pending|)
                                 (x (= |SV-Calculator| |VAL-calculateDS|))))))

(setq *ltl-uses-reachability-flag* nil)

(on-the-fly-ltl-check *thm*)

(defvar *gc1* '(not (= |SV-DMP.Type| |VAL-Scheduled|)))

(on-the-fly-ltl-check *thm* *gc1*)

(defvar *gc2* `(and (not (and (= |SV-MetData| |VAL-pending|)
                              (or (= pc7 13) (= pc7 20))))
                    ,*gc1*))

(on-the-fly-ltl-check *thm* *gc2*)
