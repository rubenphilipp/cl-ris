;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****h* utilities
;;; NAME
;;; utilities
;;; 
;;; AUTHOR
;;; Ruben Philipp <me@rubenphilipp.com>
;;;
;;; CREATED
;;; 2025-03-03
;;; 
;;; PURPOSE
;;; Utility functions. 
;;;
;;; CLASS HIERARCHY
;;;
;;;
;;; $$ Last modified:  21:31:59 Tue Mar  4 2025 CET
;;; ****
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :cl-ris)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; test if thing is of type alist
(defun alist-p (thing)
  (and (listp thing)
       (every #'consp thing)
       (every #'(lambda (x)
                  (not (listp (cdr x))))
              thing)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; tests if a given string matches a RIS tag pattern
(defun match-ris-tag? (string &key (tag-pattern *tag-pattern*))
  (cl-ppcre:scan tag-pattern string))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EOF utilities.lisp
