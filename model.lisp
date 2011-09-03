;;; -*- Package: org.datagraph.dydra.implementation; -*-

(in-package :org.datagraph.dydra.implementation)

;;; This file defines a simple interning protocol for the common-lisp interface to the Dydra storage service
;;; Copyright 2011 Datagraph Inc. (info@datagraph.org)
;;;

(defparameter dydra::*uris* nil
  "A registry for a document's uris. The map-query operator interns uris if this is bound.
 The default global value is nil, which causes each map-query invocation to construct a fresh registry.")

(defparameter dydra::*blank-nodes* nil
  "A registry for a document's blank nodes. The map-query operator interns blank nodes if this is bound.
 The default global value is nil, which causes each map-query invocation to construct a fresh registry.")

(defun intern-literal (literal type language)
  "Intern RDF literals as lisp values.
 - URI yield puri:uri instances
 - blank nodes yield uninterned symbols
 - simple literals yield strings
 - numeric types yield numeric values
 - plain strings yield a tagged string (:lang <string> <tag>)
 - unsupported types yield a cons (<type> . <string>)

 If a conversion error occurs, a tagged string and the condition instance are returned."

  (flet ((type-error () (error "Invalid lexical form for type: ~s ~s." literal type))
         (parse-decimal (string)
           (let* ((dot (position #\. string))
                  (minusp (eql (char string 0) #\-))
                  (plusp (eql (char string 0) #\+))
                  (start (if (or plusp minusp) 1 0)))
             (if dot
               (let* ((whole (parse-integer string :end dot :start start))
                      (fraction-digits (- (length string) (1+ dot)))
                      (fraction-value (if (plusp fraction-digits)
                                        (parse-integer string :start (1+ dot))
                                        0))
                      (factor (if (plusp fraction-digits) (expt 10 fraction-digits) 1))
                      (value (/ (+ (* factor whole) fraction-value) factor)))
                 (if minusp (- value) value))
               (let ((value (parse-integer string :start start)))
                 (if minusp (- value) value))))))
  (multiple-value-bind (value error)
                       (ignore-errors (cond ((null type)
                                             nil)
                                            ((or (equal type "uri") (equal type "iri"))
                                             (if dydra::*uris*
                                               (or (gethash literal dydra::*uris*)
                                                   (setf (gethash literal dydra::*uris*) (puri:uri literal)))
                                               (puri:uri literal)))
                                            ((equal type "literal")
                                             (if language 
                                               `(:|XML:LANG| ,literal lamguage)
                                               literal))
                                            ((equal type "bnode")
                                             (if dydra::*blank-nodes*
                                               (or (gethash literal dydra::*blank-nodes*)
                                                   (setf (gethash literal dydra::*blank-nodes*) (make-symbol literal)))
                                               (make-symbol literal)))
                                            ((equal type "http://www.w3.org/2001/XMLSchema#boolean")
                                             (cond ((or (equal literal "true") (equal literal "1")) t)
                                                   ((or (equal literal "false") (equal literal "0")) nil)
                                                   (t (type-error))))
                                            ((equal type "http://www.w3.org/2001/XMLSchema#dateTime")
                                             `(:date-time ,literal))
                                            ((equal type "http://www.w3.org/2001/XMLSchema#date")
                                             `(:date ,literal))
                                            ((equal type "http://www.w3.org/2001/XMLSchema#decimal")
                                             (parse-decimal literal))
                                            ((equal type "http://www.w3.org/2001/XMLSchema#double")
                                             (let ((*read-default-float-format* 'double-float))
                                               (read-from-string literal)))
                                            ((equal type "http://www.w3.org/2001/XMLSchema#float")
                                             (read-from-string literal))
                                            ((member type '("http://www.w3.org/2001/XMLSchema#integer"
                                                            "http://www.w3.org/2001/XMLSchema#nonPositiveInteger"
                                                            "http://www.w3.org/2001/XMLSchema#negativeInteger"
                                                            "http://www.w3.org/2001/XMLSchema#long"
                                                            "http://www.w3.org/2001/XMLSchema#int"
                                                            "http://www.w3.org/2001/XMLSchema#short"
                                                            "http://www.w3.org/2001/XMLSchema#byte"
                                                            "http://www.w3.org/2001/XMLSchema#nonNegativeInteger"
                                                            "http://www.w3.org/2001/XMLSchema#unsignedLong"
                                                            "http://www.w3.org/2001/XMLSchema#unsignedInt"
                                                            "http://www.w3.org/2001/XMLSchema#unsignedShort"
                                                            "http://www.w3.org/2001/XMLSchema#unsignedByte"
                                                            "http://www.w3.org/2001/XMLSchema#positiveInteger")
                                                     :test #'equal)
                                             (parse-integer literal :junk-allowed nil))
                                            ((equal type "http://www.w3.org/2001/XMLSchema#string")
                                             literal)
                                            (t
                                             `(,type . ,literal))))
    (if (typep error 'error)
      (values `(,type . ,literal) error)
      value))))
