;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****c* resource
;;; NAME
;;; resource
;;; 
;;; AUTHOR
;;; Ruben Philipp <me@rubenphilipp.com>
;;;
;;; CREATED
;;; 2025-03-03
;;; 
;;; PURPOSE
;;; Implementation of the resource class which is the class for RIS resources
;;; (e.g. a CHAP, or BOOK).
;;; The data slot contains the actual resource information (e.g. the title/TI,
;;; author/AU etc...) in form of a hash table. 
;;;
;;; CLASS HIERARCHY
;;; named-object -> resource
;;;
;;; $$ Last modified:  23:36:05 Tue Mar  4 2025 CET
;;; ****
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :cl-ris)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass resource (named-object)
  (;; the resource type (e.g. CHAP or JOUR...)
   (type :accessor type :initarg :type :initform nil)))

(defmethod initialize-instance :after ((rs resource) &rest initargs)
  (declare (ignore initargs))
  (update rs))

(defmethod print-object :before ((rs resource) stream)
  (format stream "~%RESOURCE: type: ~a" (type rs)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****m* resource/update
;;; AUTHOR
;;; Ruben Philipp <me@rubenphilipp.com>
;;;
;;; CREATED
;;; 2025-03-03
;;; 
;;; DESCRIPTION
;;; This method updates the slots of a resource object
;;;
;;; ARGUMENTS
;;; A resource object. 
;;; 
;;; RETURN VALUE
;;; The updated resource object. 
;;;
;;; SYNOPSIS
(defmethod update ((rs resource))
  ;;; ****
  ;; test if type exists
  (unless (ty-type? (type rs))
    (warn "resource::update: The type ~a does not exist." (type rs)))
  ;; parse alist to hash-table
  (cond ((alist-p (data rs))
         (setf (slot-value rs 'data) (alist-hash-table (data rs) :test 'equal)))
        ((hash-table-p (data rs))
         (setf (slot-value rs 'data) (data rs)))
        (t (error "resource::update: The data must be of type hash-table."))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod (setf type) :after (value (rs resource))
  (unless (ty-type? value)
    (warn "resource::update: The type ~a does not exist." value))
  value)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****f* resource/make-resource
;;; AUTHOR
;;; Ruben Philipp <me@rubenphilipp.com>
;;;
;;; CREATED
;;; 2025-03-03
;;; 
;;; DESCRIPTION
;;; Helper function. 
;;;
;;; ARGUMENTS
;;; - The type of the object as a string. E.g. "CHAP". 
;;; 
;;; OPTIONAL ARGUMENTS
;;; - :data. The data of the resource. Either a hash-table or an alist. 
;;; 
;;; RETURN VALUE
;;; The new resource object. 
;;;
;;; EXAMPLE
#|
(make-resource "CHAP" :data '(("AU" . "Müller, Herta") ; ; ; ;
("TI" . "Das Auge täusche im Lidschlag"))) ; ; ; ;
|#
;;; SYNOPSIS
(defun make-resource (type &key data)
;;; ****
  (make-instance 'resource :type type
                           :data data))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****m* resource/get-data
;;; AUTHOR
;;; Ruben Philipp <me@rubenphilipp.com>
;;;
;;; CREATED
;;; 2025-03-03
;;; 
;;; DESCRIPTION
;;; Returns the data of a given data field in the resource data-slot. 
;;;
;;; ARGUMENTS
;;; - The resource object.
;;; - The key to the data field. 
;;; 
;;; RETURN VALUE
;;; The value of the data field (if exists). 
;;;
;;; EXAMPLE
#|
(let ((res (make-resource "CHAP"
                          :data '(("AU" . "Müller, Herta")
                                  ("TI" . "Das Auge täusche im Lidschlag")))))
  (get-data res "TI"))
;; => "Das Auge täusche im Lidschlag"
|#
;;; SYNOPSIS
(defmethod get-data ((obj resource) key)
;;; ****
  (if (equal key "TY")
      (type obj)
      (gethash key (data obj))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****m* resource/set-data
;;; AUTHOR
;;; Ruben Philipp <me@rubenphilipp.com>
;;;
;;; CREATED
;;; 2025-03-03
;;; 
;;; DESCRIPTION
;;; Sets the data of the resource object. 
;;;
;;; ARGUMENTS
;;; - A resource object.
;;; - The key of the data field.
;;; - The new value.
;;; 
;;; RETURN VALUE
;;; The new value. 
;;;
;;; EXAMPLE
#|
(let ((res (make-resource
         "CHAP"
         :data '(("AU" . "Müller, Herta")
                 ("TI" . "Das Auge täusche im Lidschlag")))))
  (set-data res "DA" "1991")
  (get-data res "DA"))
;; => "1991"
|#
;;; SYNOPSIS
(defmethod set-data ((obj resource) key value)
;;; ****
  (if (equal key "TY")
      (setf (type obj) value)
      (setf (gethash key (data obj)) value)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****f* resource/make-resource-from-string
;;; AUTHOR
;;; Ruben Philipp <me@rubenphilipp.com>
;;;
;;; CREATED
;;; 2025-03-03
;;; 
;;; DESCRIPTION
;;; 
;;;
;;; ARGUMENTS
;;; 
;;; 
;;; OPTIONAL ARGUMENTS
;;; 
;;; 
;;; RETURN VALUE
;;; 
;;;
;;; EXAMPLE
#|
(let ((res (make-resource-from-string "TY  - JOUR
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
ER  - ")))
  (get-data res "T2"))

;; => "Sinn und Form"
|#
;;; SYNOPSIS
(defun make-resource-from-string (string &key (warn? t))
  ;;; ****
  (unless (stringp string)
    (error "resource::make-resource-from-string: string is not a string."))
  (when (> 6 (length string))
    (error "resource::make-resource-from-string: Malformatted string. "))
  ;; parse lines
  (loop for line in (split *newline* string)
        for i from 0
        for match? = (match-ris-tag? line)
        with data-hash = (make-hash-table :test 'equal)
        with type = nil
        do
           (when (and warn? (not match?))
               (warn "resource::make-resource-from-string: The line \"~a\" ~
                      does not contain a valid RIS tag. Skipping." line))
           (when match?
             (let ((tag (subseq line 0 2))
                   (data (subseq line 6)))
               (when (and (= i 0) (not (equal tag "TY")))
                 (error "resource::make-resource-from-string: The first line ~
                       does not designate a resource type (TY) but a ~a." tag))
               (cond ((= i 0) (setf type data))
                     ((equal tag *end-record-tag*)
                      ;; end when end tag (ER  - ) is reached. 
                      (loop-finish))
                     (t (when (and warn? (not (tag? tag)))
                          (warn "resource::make-resource-from-string: The tag ~
                               \"~a\" does not designate a valid RIS tag. ~
                               Using this tag anyway." tag))
                        (setf (gethash tag data-hash) data)))))
        finally
           (if type
               (return (make-resource type
                                      :data
                                      ;; this conversion is necessary for
                                      ;; maintaining the order of the data
                                      (hash-table-alist data-hash))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****m* resource/resource->string
;;; AUTHOR
;;; Ruben Philipp <me@rubenphilipp.com>
;;;
;;; CREATED
;;; 2025-03-03
;;; 
;;; DESCRIPTION
;;; This method returns a RIS string from a resource object. 
;;;
;;; ARGUMENTS
;;; A resource object. 
;;; 
;;; RETURN VALUE
;;; A string containing the RIS data. 
;;;
;;; EXAMPLE
#|
(let ((res (make-resource-from-string "TY  - JOUR
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
ER  - ")))
  (resource->string res))

;; =>
"TY  - JOUR
TI  - »Aus barer organischer Bedürftigkeit«. Die Wüste als Indikator menschlicher Intelligenz
AU  - Noll, Chaim
T2  - Sinn und Form
DA  - 2022///
PY  - 2022
VL  - 74
IS  - 3
SP  - 419
EP  -...[sly-elided string of length 237]"
|#
;;; SYNOPSIS
(defmethod resource->string ((rs resource))
  ;;; ****
  (let ((data (hash-table-alist (data rs)))
        (result (format nil "TY  - ~a~%" (type rs))))
    (loop for d in data
          do
             (setf result (concatenate 'string
                                       result
                                       (format nil "~a  - ~a~%"
                                               (car d) (cdr d))))
          finally
             (setf result
                   (concatenate 'string
                                result
                                (format nil "ER  - ~%"))))
    result))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EOF resource.lisp
