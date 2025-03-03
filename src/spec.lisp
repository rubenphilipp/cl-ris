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
;;; $$ Last modified:  00:02:02 Tue Mar  4 2025 CET
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
  '(("ABST" "abstract")
    ("ADVS" "audiovisual-material")
    ("AGGR" "aggregated-database")
    ("ANCIENT" "ancient-text")
    ("ART" "artwork")
    ("BILL" "bill-resolutions")
    ("BLOG" "blog")
    ("BOOK" "book-whole")
    ("CASE" "legal-case")
    ("CHAP" "book-section")
    ("CHART" "chart")
    ("CLSWK" "classical-work")
    ("COMP" "computer-program")
    ("CONF" "conference-proceedings")
    ("CPAPER" "conference-paper")
    ("CTLG" "catalog")
    ("DATA" "data-file-dataset")
    ("DBASE" "online-database")
    ("DICT" "dictionary")
    ("EBOOK" "electronic-book")
    ("ECHAP" "electronic-book-section")
    ("EDBOOK" "edited-book")
    ("EJOUR" "electronic-article")
    ("ELEC" "web-page")
    ("ENCYC" "encyclopedia")
    ("EQUA" "equation")
    ("FIGURE" "figure")
    ("GEN" "generic")
    ("GOVDOC" "government-document")
    ("GRNT" "grant")
    ("HEAR" "hearing")
    ("ICOMM" "internet-communication")
    ("INPR" "in-press-article")
    ("INTV" "interview")
    ("JFULL" "journal-full")
    ("JOUR" "journal-article")
    ("LEGAL" "legal-rule")
    ("MANSCPT" "manuscript")
    ("MAP" "map")
    ("MGZN" "magazine-article")
    ("MPCT" "film-or-broadcast")
    ("MULTI" "online-multimedia")
    ("MUSIC" "music-score")
    ("NEWS" "newspaper-article")
    ("PAMP" "pamphlet")
    ("PAT" "patent")
    ("PCOMM" "personal-communication")
    ("POD" "podcast")
    ("PRESS" "press-release")
    ("RPRT" "report")
    ("SER" "serial-book-monograph")
    ("SLIDE" "slide")
    ("SOUND" "sound-recording")
    ("STAND" "standard")
    ("STAT" "statute")
    ("STD" "generic")
    ("THES" "thesis-dissertation")
    ("UNBILL" "unenacted-bill")
    ("UNPB" "unpublished-work")
    ("UNPD" "unpublished-work")
    ("VIDEO" "video-recording")
    ("WEB" "web-page")))

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

;; does the type exist?
(defun ty-type? (ty &key
                      (types *import-type-map*)
                      (test #'equal))
  (assoc ty types :test test))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Tag list
;;; The last element contains commentary.

(defparameter *tag-list*
  '(("A1" "Interviewee. (Primary) author.")
    ("A2" "Secondary author.")
    ("A3" "Tertiary author.")
    ("A4" "Quaternary author.")
    ("A5" "Quinary author.")
    ("A6" "Senary author.")
    ("AB" "Abstract.")
    ("AD" "Author address.")
    ("AN" "Accession number.")
    ("AU" "Author.")
    ("AV" "Availability.")
    ("BT" "Title of work.")
    ("C1" "Custom 1.")
    ("C2" "Custom 2.")
    ("C3" "Custom 3.")
    ("C4" "Custom 4.")
    ("C5" "Custom 5.")
    ("C6" "Custom 6.")
    ("C7" "Custom 7.")
    ("C8" "Custom 8.")
    ("CA" "Caption.")
    ("CL" "Classification.")
    ("CN" "Call number.")
    ("CP" "CP - This field is used for archival data.")
    ("CR" "Cited reference.")
    ("CT" "Title of unpublished reference.")
    ("CY" "Place published.")
    ("DA" "Date.")
    ("DB" "Name of database.")
    ("DI" "DOI.")
    ("DO" "DOI.")
    ("DP" "Database provider.")
    ("ED" "Editor.")
    ("EP" "End page.")
    ("ET" "Edition.")
    ("ID" "Reference ID.")
    ("IS" "Issue number.")
    ("J1" "Journal name (alternate).")
    ("J2" "Alternate title (e.g., ISO abbreviation).")
    ("JA" "Journal abbreviation.")
    ("JF" "Journal full title.")
    ("JO" "Journal title.")
    ("KW" "Keywords.")
    ("L1" "Link to full-text.")
    ("L2" "Link to secondary source.")
    ("L3" "Related records.")
    ("L4" "Images.")
    ("LA" "Language.")
    ("LB" "Label.")
    ("LK" "Link.")
    ("LL" "Location in archives.")
    ("M1" "Miscellaneous 1.")
    ("M2" "Miscellaneous 2.")
    ("M3" "Type of work.")
    ("N1" "Notes.")
    ("N2" "Notes (other).")
    ("NV" "Number of volumes.")
    ("OP" "Original publication.")
    ("PB" "Publisher.")
    ("PP" "Place of publication.")
    ("PY" "Publication year.")
    ("RI" "Reviewed item.")
    ("RN" "Research notes.")
    ("RP" "Reprint edition.")
    ("SE" "Section.")
    ("SN" "ISSN/ISBN.")
    ("SP" "Start page.")
    ("ST" "Short title.")
    ("T1" "Primary title.")
    ("T2" "Secondary title (e.g., journal title or series title).")
    ("T3" "Tertiary title.")
    ("TA" "Translated author.")
    ("TI" "Title.")
    ("TT" "Translated title.")
    ("TY" "Reference type.")
    ("U1" "User-defined 1.")
    ("U2" "User-defined 2.")
    ("U3" "User-defined 3.")
    ("U4" "User-defined 4.")
    ("U5" "User-defined 5.")
    ("UR" "URL.")
    ("VL" "Volume number.")
    ("VO" "Published standard number.")
    ("Y1" "Primary date.")
    ("Y2" "Access date.")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; helper functions

;; does the tag exist?
(defun tag? (tag &key (tag-list *tag-list*)
                   (test #'equal))
  (assoc tag tag-list :test test))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EOF spec.lisp
