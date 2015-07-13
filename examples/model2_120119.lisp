
(in-package "bt")

(process-bt-file "examples/model2_120119.bt")

(defvar *thm* '(g (not (and (not (= |SV-Calculator| |VAL-calculateDS|))
                            (= |SV-MetData| |VAL-pending|)
                            (x (= |SV-Calculator| |VAL-calculateDS|))))))

(ltl-check *thm*)

(find-counterexample-path)

(defvar *gc1* '(not (= |SV-DMP.Type| |VAL-Scheduled|)))

(find-counterexample-path nil nil *gc1*)

(defvar *gc2* `(and (not (and (= |SV-MetData| |VAL-pending|)
                              (or (= pc7 13) (= pc7 20))))
                    ,*gc1*))

(find-counterexample-path nil nil *gc2*)

(defvar *gc3* `(and (not (and (= |SV-MetData| |VAL-pending|)
                              (= pc7 10)))
                    ,*gc2*))

(find-counterexample-path nil nil *gc3*)

(theorem-is-proved *gc3*)
