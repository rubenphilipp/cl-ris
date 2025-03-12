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
;;; $$ Last modified:  11:59:50 Wed Mar 12 2025 CET
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
   :split)
  (:export
   #:parse-from-string
   #:parse-from-file
   #:make-resource
   #:make-resource-from-string
   #:resource->string
   #:get-data
   #:set-data))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EOF package.lisp
