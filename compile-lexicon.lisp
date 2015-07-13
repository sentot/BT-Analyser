
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


(make-package "bt" :use nil)

#+CCL(defvar *object-extension* ".wx32fsl")
#+SBCL(defvar *object-extension* ".fasl")
#+GCL(defvar *object-extension* ".o")
#+ABCL(defvar *object-extension* ".abcl")
#+LISPWORKS(defvar *object-extension* ".ofasl")
#+CORMANLISP(defvar *object-extension* ".fasl")

;;; Change if necessary

(defvar *source-directory* "./")

#+CCL(defvar *subfolder* "CCL/")
#+SBCL(defvar *subfolder* "SBCL/")
#+GCL(defvar *subfolder* "GCL/")
#+ABCL(defvar *subfolder* "ABCL/")
#+LISPWORKS(defvar *subfolder* "LispWorks/")
#+CORMANLISP(defvar *subfolder* "CormanLisp/")

(defvar *object-directory*
        (concatenate 'string *source-directory* *subfolder*))

(defun bt-load-file (filename)
  (load (concatenate 'string *object-directory* filename *object-extension*)))

(defun bt-compile-file (filename)
  (compile-file (concatenate 'string *source-directory* filename ".lisp")
                :output-file
                (concatenate 'string *object-directory* filename
                             *object-extension*)))

(defun bt-compile-and-load-file (filename)
  (bt-compile-file filename)
  (bt-load-file filename))

(bt-compile-file "lexicon")
