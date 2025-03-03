;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; FILE
;;; cl-ris.asd
;;;
;;; NAME
;;; system
;;;
;;; DESCRIPTION
;;; System definition for cl-ris. 
;;;
;;; AUTHOR
;;; Ruben Philipp <me@rubenphilipp.com>
;;;
;;; CREATED
;;; 2025-03-03
;;;
;;; $$ Last modified:  23:57:23 Mon Mar  3 2025 CET
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defsystem "cl-ris"
  :description "Common Lisp parsing library for RIS-files."
  :version "0.0.1"
  :author "Ruben Philipp <me@rubenphilipp.com>"
  :license "GPL Version 2.0 or later"
  :serial t
  ;; :in-order-to ((test-op (test-op "colporter/tests")))
  :depends-on ("alexandria"
               "cl-ppcre")
  :pathname "src/"
  :components ((:file "package")
               (:file "utilities")
               (:file "spec")
               ;; to be cont'd...
               ))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EOF cl-ris.asd
