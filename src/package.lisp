;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; FILE
;;; package.lisp
;;;
;;; NAME
;;; package
;;;
;;; DESCRIPTION
;;; Package definition of cl-ris.
;;;
;;; AUTHOR
;;; Ruben Philipp <me@rubenphilipp.com>
;;;
;;; CREATED
;;; 2025-03-03
;;;
;;; $$ Last modified:  20:47:23 Tue Mar  4 2025 CET
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :cl-user)

(defpackage :cl-ris
  (:use :common-lisp)
  (:nicknames :ris)
  (:import-from
   :alexandria
   :assoc-value
   :alist-hash-table
   :hash-table-alist
   :hash-table-keys
   :read-file-into-string)
  (:import-from
   :cl-ppcre
   :scan
   :split))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EOF package.lisp
