
(in-package "bt")

(process-bt-file "examples/BRL150518.btc")

(reachable-states)

(defvar *blocks* (indices-of-update-blocks "Vector"))

(defvar *vector-null* (indices-of-update-blocks "Vector" "Null"))
(defvar *vector-dynamic* (indices-of-update-blocks "Vector" "dynamic"))
(defvar *vector-fix* (indices-of-update-blocks "Vector" "fixed"))

(defvar *intermediates*
        (append (indices-of-blocks-matching "10")
                (indices-of-blocks-matching "12")
                (indices-of-blocks-matching "16")))

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
               (format t "Precondition:~%~S~%" (test-path-precondition path))
               (print-test-path-summary path))
              (t
               (format t "~%Invalid path:~%~S" (map-index-to-tag path))
               nil))))

(defun generate-and-print-test-paths ()
  (format t "~%========== Paths from Vector null to Vector null:~%")
  (let ((result (generate-test-paths-new
                  *vector-null* *vector-null* *intermediates* *blocks*)))
    (print-paths result))
  (format t "~%========== Paths from Vector null to Vector dynamic:~%")
  (let ((result (generate-test-paths-new
                  *vector-null* *vector-dynamic* *intermediates* *blocks*)))
    (print-paths result))
  (format t "~%========== Paths from Vector null to Vector fixed:~%")
  (let ((result (generate-test-paths-new
                  *vector-null* *vector-fix* *intermediates* *blocks*)))
    (print-paths result))
  (format t "~%========== Paths from Vector dynamic to Vector null:~%")
  (let ((result (generate-test-paths-new
                  *vector-dynamic* *vector-null* *intermediates* *blocks*)))
    (print-paths result))
  (format t "~%========== Paths from Vector dynamic to Vector dynamic:~%")
  (let ((result (generate-test-paths-new
                  *vector-dynamic* *vector-dynamic* *intermediates* *blocks*)))
    (print-paths result))
  (format t "~%========== Paths from Vector dynamic to Vector fixed:~%")
  (let ((result (generate-test-paths-new
                  *vector-dynamic* *vector-fix* *intermediates* *blocks*)))
    (print-paths result))
  (format t "~%========== Paths from Vector fixed to Vector null:~%")
  (let ((result (generate-test-paths-new
                  *vector-fix* *vector-null* *intermediates* *blocks*)))
    (print-paths result))
  (format t "~%========== Paths from Vector fixed to Vector dynamic:~%")
  (let ((result (generate-test-paths-new
                  *vector-fix* *vector-dynamic* *intermediates* *blocks*)))
    (print-paths result))
  (format t "~%========== Paths from Vector fixed to Vector fixed:~%")
  (let ((result (generate-test-paths-new
                  *vector-fix* *vector-fix* *intermediates* *blocks*)))
    (print-paths result)))

;(time (generate-and-print-test-paths))
