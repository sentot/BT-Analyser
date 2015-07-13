
(in-package "bt")

(process-bt-file "examples/model2_120119.bt")

(defvar *thm* '(g (not (and (not (= |SV-Calculator| |VAL-calculateDS|))
                                 (= |SV-MetData| |VAL-pending|)
                                 (x (= |SV-Calculator| |VAL-calculateDS|))))))

(combo2-ltl-check *thm*)

(defvar *gc1* '(not (= |SV-DMP.Type| |VAL-Scheduled|)))

(combo2-ltl-check *thm* *gc1*)

(defvar *gc2* `(and (not (and (= |SV-MetData| |VAL-pending|)
                              (or (= pc7 13) (= pc7 20))))
                    ,*gc1*))

(combo2-ltl-check *thm* *gc2*)

(defvar *gc3* `(and (not (and (= |SV-MetData| |VAL-pending|)
                              (= pc7 10)))
                    ,*gc2*))

(theorem-is-proved *gc3*)
