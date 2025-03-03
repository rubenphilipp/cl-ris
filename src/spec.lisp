;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****h* spec
;;; NAME
;;; spec
;;; 
;;; AUTHOR
;;; Ruben Philipp <me@rubenphilipp.com>
;;;
;;; CREATED
;;; 2025-03-03
;;; 
;;; PURPOSE
;;; Implementation of the RIS specifications, in particular data types and
;;; related fields.
;;;
;;;
;;; $$ Last modified:  23:34:08 Mon Mar  3 2025 CET
;;; ****
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :cl-ris)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; a list of supported TYPES
;;; 
;;; NB: Order is important as on export, the topmost item will be selected
;;;     by default (this is the case as of redundancies in the allocations).
;;; 
;;; Cf. https://en.wikipedia.org/wiki/RIS_(file_format)#Type_of_reference
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defparameter *import-type-map*
  '(("ABST" . "abstract")
    ("ADVS" . "audiovisual-material")
    ("AGGR" . "aggregated-database")
    ("ANCIENT" . "ancient-text")
    ("ART" . "artwork")
    ("BILL" . "bill-resolutions")
    ("BLOG" . "blog")
    ("BOOK" . "book-whole")
    ("CASE" . "legal-case")
    ("CHAP" . "book-section")
    ("CHART" . "chart")
    ("CLSWK" . "classical-work")
    ("COMP" . "computer-program")
    ("CONF" . "conference-proceedings")
    ("CPAPER" . "conference-paper")
    ("CTLG" . "catalog")
    ("DATA" . "data-file-dataset")
    ("DBASE" . "online-database")
    ("DICT" . "dictionary")
    ("EBOOK" . "electronic-book")
    ("ECHAP" . "electronic-book-section")
    ("EDBOOK" . "edited-book")
    ("EJOUR" . "electronic-article")
    ("ELEC" . "web-page")
    ("ENCYC" . "encyclopedia")
    ("EQUA" . "equation")
    ("FIGURE" . "figure")
    ("GEN" . "generic")
    ("GOVDOC" . "government-document")
    ("GRNT" . "grant")
    ("HEAR" . "hearing")
    ("ICOMM" . "internet-communication")
    ("INPR" . "in-press-article")
    ("INTV" . "interview")
    ("JFULL" . "journal-full")
    ("JOUR" . "journal-article")
    ("LEGAL" . "legal-rule")
    ("MANSCPT" . "manuscript")
    ("MAP" . "map")
    ("MGZN" . "magazine-article")
    ("MPCT" . "film-or-broadcast")
    ("MULTI" . "online-multimedia")
    ("MUSIC" . "music-score")
    ("NEWS" . "newspaper-article")
    ("PAMP" . "pamphlet")
    ("PAT" . "patent")
    ("PCOMM" . "personal-communication")
    ("POD" . "podcast")
    ("PRESS" . "press-release")
    ("RPRT" . "report")
    ("SER" . "serial-book-monograph")
    ("SLIDE" . "slide")
    ("SOUND" . "sound-recording")
    ("STAND" . "standard")
    ("STAT" . "statute")
    ("STD" . "generic")
    ("THES" . "thesis-dissertation")
    ("UNBILL" . "unenacted-bill")
    ("UNPB" . "unpublished-work")
    ("UNPD" . "unpublished-work")
    ("VIDEO" . "video-recording")
    ("WEB" . "web-page")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; derive export type map from import type map
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defparameter *export-type-map*
  (mapcar #'(lambda (ty)
              (cons (cdr ty) (car ty)))
          *import-type-map*))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; helper functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun translate-type (typestring &key
                                    map
                                    ;; assoc test-function
                                    (test #'equal)
                                    (warn? t))
  (unless (alist-p map)
    (error "translate-type: map is not of type alist, but a ~a." (type-of map)))
  (let ((found (alexandria:assoc-value map typestring :test test)))
    (if (null found)
        (progn
          (when warn?
            (warn "translate-type: typestring \"~a\" not found in map."
                  typestring))
          typestring)
        found)))


;;; more specific
#|
(import-type "WEB")
;; => "web-page"
|#
(defun import-type (typestring &key (warn? t))
  (translate-type typestring
                  :warn? warn?
                  :map *import-type-map*))

#|
(export-type "generic")
;; => "GEN"
|#
(defun export-type (typestring &key (warn? t))
  (translate-type typestring
                  :warn? warn?
                  :map *export-type-map*))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;







;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EOF spec.lisp
