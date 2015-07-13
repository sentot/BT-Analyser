
(in-package "bt")

(process-bt-file "examples/model2_120119.bt")

(defvar *thm* '(g (not (and (not (= |SV-Calculator| |VAL-calculateDS|))
                                 (= |SV-MetData| |VAL-pending|)
                                 (x (= |SV-Calculator| |VAL-calculateDS|))))))

(on-the-fly-ltl-check-mc *thm*)

(defvar *gc1* '(not (= |SV-DMP.Type| |VAL-Scheduled|)))

(on-the-fly-ltl-check-mc *thm* *gc1*)

(defvar *gc2* `(and (not (and (= |SV-MetData| |VAL-pending|)
                              (or (= pc7 13) (= pc7 20))))
                    ,*gc1*))

(on-the-fly-ltl-check-mc *thm* *gc2*)

(defvar *gc3* `(and (not (and (= |SV-MetData| |VAL-pending|)
                              (= pc7 10)))
                    ,*gc2*))

(on-the-fly-ltl-check-mc *thm* *gc3*)
