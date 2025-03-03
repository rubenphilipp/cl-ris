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
;;; $$ Last modified:  22:55:49 Mon Mar  3 2025 CET
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
;;; EOF utilities.lisp
