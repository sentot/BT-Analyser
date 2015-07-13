
(in-package "bt")

(process-bt-file "examples/ATM140726.btc")

(reachable-states)

(defvar *atm-ready* (indices-of-update-blocks "ATM" "Ready"))
(defvar *show-trans-types*
        (indices-of-update-blocks "Display" "ShowTransTypes"))
(defvar *show-acc-types* (indices-of-update-blocks "Display" "ShowAccTypes"))
(defvar *show-wd-amounts* (indices-of-update-blocks "Display" "ShowWdAmounts"))
(defvar *another-transaction*
        (indices-of-update-blocks "Display" "AnotherTransaction?"))

(defvar *blocks*
        (append (indices-of-update-blocks "ATM" "Ready")
                (indices-of-update-blocks "Display")))

(defvar *all-noi* nil)

(defun generate-test-paths-new (sources targets intermediates blocks)
  (loop for source in sources
        append
        (loop for target in targets
              append
              (find-test-paths source intermediates target blocks))))

(defun print-paths (paths)
  (loop for path in paths
        do
        (cond ((check-test-path path)
               (format t "~%Valid path:~%")
               (format t "~S~%" path)
               (print-test-path-summary path))
              (t
               (format t "~%Invalid path:~%~S" (map-index-to-tag path))
               nil))))

(defun generate-and-print-test-paths-new (intermediates)
  (cond ((null intermediates)
         (format t "~%Generating test paths"))
        (t
         (format t "~%Generating test paths with NOIs:~%")))
  (loop for i in intermediates
        do (print-block-summary i))
  (format t "~%========== Paths from ATM Ready to ATM Ready:~%")
  (let ((result (generate-test-paths-new
                  *atm-ready* *atm-ready* intermediates *blocks*)))
    (print-paths result))
  (format t "~%========== Paths from ATM Ready to showtranstypes:~%")
  (let ((result (generate-test-paths-new
                  *atm-ready* *show-trans-types* intermediates *blocks*)))
    (print-paths result))
  (format t "~%========== Paths from ATM Ready to showacctypes:~%")
  (let ((result (generate-test-paths-new
                  *atm-ready* *show-acc-types* intermediates *blocks*)))
    (print-paths result))
  (format t "~%========== Paths from ATM Ready to showwdamounts:~%")
  (let ((result (generate-test-paths-new
                  *atm-ready* *show-wd-amounts* intermediates *blocks*)))
    (print-paths result))
  (format t "~%========== Paths from ATM Ready to Another Trans:~%")
  (let ((result (generate-test-paths-new
                  *atm-ready* *another-transaction* intermediates *blocks*)))
    (print-paths result))
  (format t "~%========== Paths from showtranstypes to ATM Ready:~%")
  (let ((result (generate-test-paths-new
                  *show-trans-types* *atm-ready* intermediates *blocks*)))
    (print-paths result))
  (format t "~%========== Paths from showtranstypes to showtranstypes:~%")
  (let ((result (generate-test-paths-new
                  *show-trans-types* *show-trans-types* intermediates
                  *blocks*)))
    (print-paths result))
  (format t "~%========== Paths from showtranstypes to showacctypes:~%")
  (let ((result (generate-test-paths-new
                  *show-trans-types* *show-acc-types* intermediates *blocks*)))
    (print-paths result))
  (format t "~%========== Paths from showtranstypes to showwdamounts:~%")
  (let ((result (generate-test-paths-new
                  *show-trans-types* *show-wd-amounts* intermediates *blocks*)))
    (print-paths result))
  (format t "~%========== Paths from showtranstypes to Another Trans:~%")
  (let ((result (generate-test-paths-new
                  *show-trans-types* *another-transaction* intermediates
                  *blocks*)))
    (print-paths result))
  (format t "~%========== Paths from showacctypes to ATM Ready:~%")
  (let ((result (generate-test-paths-new
                  *show-acc-types* *atm-ready* intermediates *blocks*)))
    (print-paths result))
  (format t "~%========== Paths from showacctypes to showtranstypes:~%")
  (let ((result (generate-test-paths-new
                  *show-acc-types* *show-trans-types* intermediates *blocks*)))
    (print-paths result))
  (format t "~%========== Paths from showacctypes to showacctypes:~%")
  (let ((result (generate-test-paths-new
                  *show-acc-types* *show-acc-types* intermediates *blocks*)))
    (print-paths result))
  (format t "~%========== Paths from showacctypes to showwdamounts:~%")
  (let ((result (generate-test-paths-new
                  *show-acc-types* *show-wd-amounts* intermediates *blocks*)))
    (print-paths result))
  (format t "~%========== Paths from showacctypes to Another Trans:~%")
  (let ((result (generate-test-paths-new
                  *show-acc-types* *another-transaction* intermediates
                  *blocks*)))
    (print-paths result))
  (format t "~%========== Paths from showwdamounts to ATM Ready:~%")
  (let ((result (generate-test-paths-new
                  *show-wd-amounts* *atm-ready* intermediates *blocks*)))
    (print-paths result))
  (format t "~%========== Paths from showwdamounts to showtranstypes:~%")
  (let ((result (generate-test-paths-new
                  *show-wd-amounts* *show-trans-types* intermediates *blocks*)))
    (print-paths result))
  (format t "~%========== Paths from showwdamounts to showacctypes:~%")
  (let ((result (generate-test-paths-new
                  *show-wd-amounts* *show-acc-types* intermediates *blocks*)))
    (print-paths result))
  (format t "~%========== Paths from showwdamounts to showwdamounts:~%")
  (let ((result (generate-test-paths-new
                  *show-wd-amounts* *show-wd-amounts* intermediates *blocks*)))
    (print-paths result))
  (format t "~%========== Paths from showwdamounts to Another Trans:~%")
  (let ((result (generate-test-paths-new
                  *show-wd-amounts* *another-transaction* intermediates
                  *blocks*)))
    (print-paths result))
  (format t "~%========== Paths from Another Trans to ATM Ready:~%")
  (let ((result (generate-test-paths-new
                  *another-transaction* *atm-ready* intermediates *blocks*)))
    (print-paths result))
  (format t "~%========== Paths from Another Trans to showtranstypes:~%")
  (let ((result (generate-test-paths-new
                  *another-transaction* *show-trans-types* intermediates
                  *blocks*)))
    (print-paths result))
  (format t "~%========== Paths from Another Trans to showacctypes:~%")
  (let ((result (generate-test-paths-new
                  *another-transaction* *show-acc-types* intermediates
                  *blocks*)))
    (print-paths result))
  (format t "~%========== Paths from Another Trans to showwdamounts:~%")
  (let ((result (generate-test-paths-new
                  *another-transaction* *show-wd-amounts* intermediates
                  *blocks*)))
    (print-paths result))
  (format t "~%========== Paths from Another Trans to Another Trans:~%")
  (let ((result (generate-test-paths-new
                  *another-transaction* *another-transaction* intermediates
                  *blocks*)))
    (print-paths result)))

(defun run-test (intermediates)
  (let ((noi (loop for i in intermediates
                   when (not (member-equal i *blocks*))
                   collect i)))
    (time (generate-and-print-test-paths-new noi))))

(defun example-paths ()
  (print-paths
    (find-test-paths (car *show-wd-amounts*)
                     *all-noi*
                     (car *another-transaction*)
                     *blocks*)))

;(example-paths)

;(test-path-preamble 0 '(7 8 9 10 11 12))

;(print-test-path-summary '(0 1 2 3 4 5 6 7 8 9 10 11 12))

;(run-test *all-noi*)
