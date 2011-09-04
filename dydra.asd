;;; -*- Mode: lisp; Syntax: ansi-common-lisp; Base: 10; Package: common-lisp-user; -*-

(in-package :common-lisp-user)

;;;
;;;  This file is the system definition for the Dydra Common Lisp sdk.
;;;
;;;  Copyright 2011 [Datagraph Inc.](mailto:info@datagraph.org) All Rights Reserved
;;;

(pushnew :sesame-2.0 *features*)

;;; main system definition

(asdf:defsystem :dydra
  :depends-on (;; not yet
               ;; :cxml-rpc
               ;; :closure-html
               ;; :cxml-stp
               :cl-json
               :puri)
  :description "Common Lisp SDK for Dydra.
 It includes http rest and xml-rpc based operators to work with a Dydra store."
  :serial t
  :components ((:file "package")
               (:file "model")
               (:file "api")
               (:file "json-rest")
               ;; (:file "json-rpc")
               ;; (:file "xml-rpc")
               ))


#|

# loading
$ sbcl
* (load "org/quicklisp/setup.lisp")
;;; (ql:quickload "cl-json")
;;; (ql:quickload "cxml-rpc")           ; required for xml rpc based services
;;; (ql:quickload "closure-html")       ; ''
;;; (ql:quickload "cxml-stp")           ; ''
;;;
;;; to use a local (non-quicklicp) version, some form of ...
* (pushnew "/development/source/library/asdf-registry/" asdf:*central-registry*)
* (ql:quickload "dydra")

* #+sbcl(save-lisp-and-die "sbcl-dydra.core")
$ sbcl --core sbcl-dydra.core  --no-userinit --no-sysinit

|#
