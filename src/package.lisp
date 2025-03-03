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
;;; $$ Last modified:  22:05:03 Mon Mar  3 2025 CET
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :cl-user)

(defpackage :cl-ris
  (:use :common-lisp)
  (:nicknames :ris)
  (:import-from
   :alexandria
   :assoc-value
   :read-file-into-string))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EOF package.lisp
