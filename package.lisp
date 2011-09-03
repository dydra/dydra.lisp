;;; -*- Package: cl-user; -*-

;;; This file defines the packages for the common-lisp
;;; interface to the Dydra storage service
;;;
;;;  Copyright 2011 [Datagraph Inc.](mailto:info@datagraph.org) All Rights Reserved
;;;

(in-package :cl-user)

(defpackage :dydra
  (:use)
  (:nicknames :org.datagraph.dydra)
  (:intern :repository-clear
           :repository-create
           :repository-count
           :repository-delete
           :repository-import
           :repository-info
           :repository-info-cache
           :repository-query
           :service-account-name
           :service-info
           :service-uri)
  (:export :*blank-nodes*
           :*repository*
           :*repository-class*
           :*service*
           :*service-class*
           :*uris*
           :authenticate
           :clear
           :count
           :create
           :delete
           :import
           :import-status
           :info
           :json-rpc-service
           :map-query
           :query
           :repositories
           :repository
           :repository-account-name
           :repository-id
           :repository-name
           :repository-service
           :rest-service
           :rpc-service
           :service
           :xml-rpc-service
           :json-rpc-ervice
           :json-rest-service)
  (:documentation "This is the home package for interface symbols for
    the Dydra storage service library. The exported symbols constitute the
    primary interface. in addition some lower level operator names are
    included, but unexported, in case it should be necessary to specialize
    interface objects."))

(defpackage :org.datagraph.dydra.implementation
  (:nicknames :dydra.i)
  (:use :common-lisp)
  (:documentation "This is the home package for the implementation of
    the Dydra storage service library."))

