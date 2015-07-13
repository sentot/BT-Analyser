
(in-package "bt")

(process-bt-file "examples/s1SSM.btc")

(reachable-states)

(defvar *blocks* (indices-of-update-blocks "SSM"))

(defvar *stopped* (indices-of-update-blocks "SSM" "Stopped"))
(defvar *unknown* (indices-of-update-blocks "SSM" "Unknown"))
(defvar *ok* (indices-of-update-blocks "SSM" "Ok"))
(defvar *failedst* (indices-of-update-blocks "SSM" "FailedST"))
(defvar *failedsensors* (indices-of-update-blocks "SSM" "FailedSensors"))

(defvar *all-noi*
        (remove-duplicates
          (append (indices-of-event-blocks "Filter" "invalid(s1)")
                  (indices-of-event-blocks "Filter" "invalidST")
                  (indices-of-event-blocks "TimerST" "timeout")
                  (indices-of-event-blocks "Timer" "timeout(s1)"))))

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
  (format t "~%========== Paths from SSM stopped to SSM stopped:~%")
  (let ((result (generate-test-paths-new
                  *stopped* *stopped* intermediates *blocks*)))
    (print-paths result))
  (format t "~%========== Paths from SSM stopped to SSM unknown:~%")
  (let ((result (generate-test-paths-new
                  *stopped* *unknown* intermediates *blocks*)))
    (print-paths result))
  (format t "~%========== Paths from SSM stopped to SSM ok:~%")
  (let ((result (generate-test-paths-new
                  *stopped* *ok* intermediates *blocks*)))
    (print-paths result))
  (format t "~%========== Paths from SSM stopped to SSM failedst:~%")
  (let ((result (generate-test-paths-new
                  *stopped* *failedst* intermediates *blocks*)))
    (print-paths result))
  (format t "~%========== Paths from SSM stopped to SSM failedsensors:~%")
  (let ((result (generate-test-paths-new
                  *stopped* *failedsensors* intermediates *blocks*)))
    (print-paths result))
  (format t "~%========== Paths from SSM unknown to SSM stopped:~%")
  (let ((result (generate-test-paths-new
                  *unknown* *stopped* intermediates *blocks*)))
    (print-paths result))
  (format t "~%========== Paths from SSM unknown to SSM unknown:~%")
  (let ((result (generate-test-paths-new
                  *unknown* *unknown* intermediates *blocks*)))
    (print-paths result))
  (format t "~%========== Paths from SSM unknown to SSM ok:~%")
  (let ((result (generate-test-paths-new
                  *unknown* *ok* intermediates *blocks*)))
    (print-paths result))
  (format t "~%========== Paths from SSM unknown to SSM failedst:~%")
  (let ((result (generate-test-paths-new
                  *unknown* *failedst* intermediates *blocks*)))
    (print-paths result))
  (format t "~%========== Paths from SSM unknown to SSM failedsensors:~%")
  (let ((result (generate-test-paths-new
                  *unknown* *failedsensors* intermediates *blocks*)))
    (print-paths result))
  (format t "~%========== Paths from SSM ok to SSM stopped:~%")
  (let ((result (generate-test-paths-new
                  *ok* *stopped* intermediates *blocks*)))
    (print-paths result))
  (format t "~%========== Paths from SSM ok to SSM unknown:~%")
  (let ((result (generate-test-paths-new
                  *ok* *unknown* intermediates *blocks*)))
    (print-paths result))
  (format t "~%========== Paths from SSM ok to SSM ok:~%")
  (let ((result (generate-test-paths-new
                  *ok* *ok* intermediates *blocks*)))
    (print-paths result))
  (format t "~%========== Paths from SSM ok to SSM failedst:~%")
  (let ((result (generate-test-paths-new
                  *ok* *failedst* intermediates *blocks*)))
    (print-paths result))
  (format t "~%========== Paths from SSM ok to SSM failedsensors:~%")
  (let ((result (generate-test-paths-new
                  *ok* *failedsensors* intermediates *blocks*)))
    (print-paths result))
  (format t "~%========== Paths from SSM failedst to SSM stopped:~%")
  (let ((result (generate-test-paths-new
                  *failedst* *stopped* intermediates *blocks*)))
    (print-paths result))
  (format t "~%========== Paths from SSM failedst to SSM unknown:~%")
  (let ((result (generate-test-paths-new
                  *failedst* *unknown* intermediates *blocks*)))
    (print-paths result))
  (format t "~%========== Paths from SSM failedst to SSM ok:~%")
  (let ((result (generate-test-paths-new
                  *failedst* *ok* intermediates *blocks*)))
    (print-paths result))
  (format t "~%========== Paths from SSM failedst to SSM failedst:~%")
  (let ((result (generate-test-paths-new
                  *failedst* *failedst* intermediates *blocks*)))
    (print-paths result))
  (format t "~%========== Paths from SSM failedst to SSM failedsensors:~%")
  (let ((result (generate-test-paths-new
                  *failedst* *failedsensors* intermediates *blocks*)))
    (print-paths result))
  (format t "~%========== Paths from SSM failedsensors to SSM stopped:~%")
  (let ((result (generate-test-paths-new
                  *failedsensors* *stopped* intermediates *blocks*)))
    (print-paths result))
  (format t "~%========== Paths from SSM failedsensors to SSM unknown:~%")
  (let ((result (generate-test-paths-new
                  *failedsensors* *unknown* intermediates *blocks*)))
    (print-paths result))
  (format t "~%========== Paths from SSM failedsensors to SSM ok:~%")
  (let ((result (generate-test-paths-new
                  *failedsensors* *ok* intermediates *blocks*)))
    (print-paths result))
  (format t "~%========== Paths from SSM failedsensors to SSM failedst:~%")
  (let ((result (generate-test-paths-new
                  *failedsensors* *failedst* intermediates *blocks*)))
    (print-paths result))
  (format t "~%========== Paths from SSM failedsensors to SSM failedsensors:~%")
  (let ((result (generate-test-paths-new
                  *failedsensors* *failedsensors* intermediates *blocks*)))
    (print-paths result)))

(defun run-test (intermediates)
  (let ((noi (loop for i in intermediates
                   when (not (member-equal i *blocks*))
                   collect i)))
    (time (generate-and-print-test-paths-new noi))))

(defun example-paths ()
  (print-paths
    (find-test-paths
      (first (indices-of-blocks-matching "13"))
      (list (first (indices-of-blocks-matching "42")))
      (first (indices-of-blocks-matching "48"))
      *blocks*)))

;(example-paths)

;(run-test *all-noi*)
