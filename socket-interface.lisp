
;;;======================================================================
;;;                                                                     |
;;;  Copyright (c) 2013, Sentot Kromodimoeljo                           |
;;;  All Rights Reserved.                                               |
;;;                                                                     |
;;;  Redistribution and use in source and binary forms, with or without |
;;;  modification, are permitted provided the following conditions are  |
;;;  met:                                                               |
;;;                                                                     |
;;;  1. Redistributions of source code must retain the above copyright  |
;;;     notice, this list of conditions and the following disclaimer.   |
;;;  2. Redistributions in binary form must reproduce the above         |
;;;     copyright notice, this list of conditions and the following     |
;;;     disclaimer in the documentation and/or other materials provided |
;;;     with the distribution.                                          |
;;;                                                                     |
;;;  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND             |
;;;  CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES,        |
;;;  INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF           |
;;;  MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE           |
;;;  DISCLAIMED. IN NO EVENT SHALL SENTOT KROMODIMOELJO BE LIABLE FOR   |
;;;  ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR           |
;;;  CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT  |
;;;  OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR |
;;;  BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF         |
;;;  LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT          |
;;;  (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE  |
;;;  USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH   |
;;;  DAMAGE.                                                            |
;;;                                                                     |
;;;======================================================================


(in-package "bt")

;;; Socket server mode

(defvar *default-socket-port* 12)

(defun socket-server-mode (&optional port)
  (let* ((socket-port (or port *default-socket-port*))
         (listener (open-socket-server socket-port))
         (socket-stream (socket-accept listener))
         (eof nil))
    (loop while (null eof)
          do
          (let ((input (read socket-stream)))
            (cond ((equal input '(close-connection))
                   (setq eof t))
                  (t
                   ;; show command from client
                   (format t "~S~%" input)
                   (let ((result (xml-do-command input)))
                     ;; send result to client
                     (format socket-stream "~A~%" result)
                     (force-output socket-stream))))))
    (socket-server-close listener)))

;;; Socket client mode (for testing)

(defun socket-client-mode (&optional port host)
  (let* ((socket-port (or port *default-socket-port*))
         (socket-host (or host "localhost"))
         (socket-stream (open-socket socket-host socket-port))
         (eof nil))
    (loop while (null eof)
          do
          (let ((input (read)))
            (format socket-stream "~S~%" input)
            (force-output socket-stream)
            (cond ((equal input '(close-connection))
                   (setq eof t))
                  (t
                   (let ((done nil))
                     (loop while (null done)
                           do
                           (let ((line (read-line socket-stream)))
                             (format t "~A~%" line)
                             (if (string-search-equal
                                   "</result>" line)
                                 (setq done t)))))))))
    (close socket-stream)))




