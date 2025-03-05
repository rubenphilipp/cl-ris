;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****c* parser
;;; NAME
;;; parser
;;; 
;;; AUTHOR
;;; Ruben Philipp <me@rubenphilipp.com>
;;;
;;; CREATED
;;; 2025-03-03
;;; 
;;; PURPOSE
;;; Implementation of the RIS parser.
;;;
;;; CLASS HIERARCHY
;;;
;;;
;;; $$ Last modified:  15:16:44 Wed Mar  5 2025 CET
;;; ****
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :cl-ris)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****f* parser/parse-from-string
;;; AUTHOR
;;; Ruben Philipp <me@rubenphilipp.com>
;;;
;;; CREATED
;;; 2025-03-03
;;; 
;;; DESCRIPTION
;;; This function parses a string containing RIS resource information (in
;;; RIS-syntax) to a list of resource-objects. 
;;;
;;; ARGUMENTS
;;; - The string containing valid RIS data. 
;;; 
;;; OPTIONAL ARGUMENTS
;;; - :warn?. When T, warnings will be issued during parsing (e.g. when a type
;;;   does not exist in the RIS standard). 
;;; 
;;; RETURN VALUE
;;; A list with resource-objects.
;;;
;;; EXAMPLE
#|
(parse-from-string "TY  - JOUR
TI  - »Aus barer organischer Bedürftigkeit«. Die Wüste als Indikator menschlicher Intelligenz
AU  - Noll, Chaim
T2  - Sinn und Form
DA  - 2022///
PY  - 2022
VL  - 74
IS  - 3
SP  - 419
EP  - 422
LA  - de
KW  - #nosource
ER  - 

TY  - THES
TI  - Carceri d'Invenzione von Brian Ferneyhough. Kompositionstechnische und höranalytische Aspekte
AU  - Pätzold, Cordula
CY  - Freiburg i. Br.
DA  - 2002///
PY  - 2002
LA  - de
M3  - Dissertation
PB  - Albert-Ludwigs-Universität zu Freiburg i. Br.
ER  - ")

;; =>
(
RESOURCE: type: JOUR
NAMED-OBJECT: id: NIL, tag: NIL, 
data: #<HASH-TABLE :TEST EQUAL :COUNT 11 {7013A23303}>
**********

 
RESOURCE: type: THES
NAMED-OBJECT: id: NIL, tag: NIL, 
data: #<HASH-TABLE :TEST EQUAL :COUNT 8 {7013A27CF3}>
**********
)
|#
;;; SYNOPSIS
(defun parse-from-string (string &key (warn? t))
  ;;; ****
  (let (;; split string by resource delimiter (ER - )
        (ress (split (format nil "~a~a~a"
                             *end-record-tag*
                             *tag-end*
                             *end-hyphen*)
                     string)))
    (loop for r in ress
          ;; remove empty lines from string
          for r-proc = (cl-ppcre:regex-replace-all
                        (format nil "^\\s*$*(~a)" *newline*) r "")
          when (> (length r-proc) 7)
            collect
            (make-resource-from-string r-proc :warn? warn?))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****f* package/parse-from-file
;;; AUTHOR
;;; Ruben Philipp <me@rubenphilipp.com>
;;;
;;; CREATED
;;; 2025-03-03
;;; 
;;; DESCRIPTION
;;; Parse resource objects from an .ris file.
;;; For more detail cf. parse-from-string. 
;;;
;;; ARGUMENTS
;;; - The path to an .ris-file.
;;; 
;;; OPTIONAL ARGUMENTS
;;; - :warn?. When T, warnings will be issued during parsing (e.g. when a type
;;;   does not exist in the RIS standard). 
;;; 
;;; RETURN VALUE
;;; A list with resource-objects.
;;;
;;; EXAMPLE
#|
(parse-from-file "examples/ex1.ris")

;; =>
(
RESOURCE: type: JOUR
NAMED-OBJECT: id: NIL, tag: NIL, 
data: #<HASH-TABLE :TEST EQUAL :COUNT 11 {7036B920E3}>
**********

 
RESOURCE: type: THES
NAMED-OBJECT: id: NIL, tag: NIL, 
data: #<HASH-TABLE :TEST EQUAL :COUNT 8 {7036D1A893}>
**********

 
RESOURCE: type: CHAP
NAMED-OBJECT: id: NIL, tag: NIL, 
data: #<HASH-TABLE :TEST EQUAL :COUNT 12 {7036D1F7B3}>
**********

 
RESOURCE: type: CHAP
NAMED-OBJECT: id: NIL, tag: NIL, 
data: #<HASH-TABLE :TEST EQUAL :COUNT 16 {7036D37843}>
**********
)
|#
;;; SYNOPSIS
(defun parse-from-file (file &key (warn? t))
  ;;; ****
  (let ((string (read-file-into-string file)))
    (parse-from-string string :warn? warn?)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EOF parser.lisp
