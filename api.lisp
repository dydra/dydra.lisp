;;; -*- Package: org.datagraph.dydra.implementation; -*-

(in-package :org.datagraph.dydra.implementation)

;;; This file defines the API for a common-lisp interface to the Dydra storage service.
;;;
;;;  Copyright 2011 [Datagraph Inc.](mailto:info@datagraph.org) All Rights Reserved
;;;
;;; The highlevel operators create, load, query, and delete repositories and their statement contents
;;; through an api which operates on an repository instance and operation-specific arguments.
;;; This interface relies on a protocol-specific layer, which implements the operators in terms of a service
;;; and a repository instance. The library includes specialized implementations for
;;; - rest/json/http
;;; - rpc/xml/http
;;; - rpc/json/http
;;; services. This file defines the generic operator interface only.
;;;
;;;   clear (&key repository host id)         : delete all statements from a repository
;;;   count (&key repository host id)         : return the count of statements in a repository
;;;   create (&key repository host id)        : create a repository in the store
;;;   delete (&key repository host id)        : delete the repository from the store
;;;   import (url &key repository host id base-uri context)    : import the resource's statement into the repository
;;;   import-status (&key repository host id) : return the status of the latest import to the repository
;;;   info (subject)                  : return a description of the repository or service
;;;   query (query-form &key repository host id)   : apply the query form to the repository and return the results
;;;   map-query (type operator query  &key repository host id)  ; maps the operator of the interned query results.
;;;   repositories (&key service host)     : return descriptions of the service's account's repositories
;;;
;;; The optional values default to the global value for *service* or *repository* respective the
;;; intended target, and or additional specific arguments.


;;;
;;; parameters

(defvar dydra::*service* nil
  "A global value to serve as the default service instance when instantiating new repositories.")

(defvar dydra::*service-class* 'dydra::json-rest-service
  "The default class used by the service constructor. The initial value is json-rest-service.")

(defvar dydra::*rpc-service-class* 'dydra::json-rpc-service
  "The default class used by the rpc-service constructor. The initial value is json-rpc-service.")

(defvar dydra::*repository* nil
  "A global value to serve as the default srepository argument for store operations.")

(defvar dydra::*repository-class* 'dydra::repository
  "The default class used by the repository constructor. The initial value is repository.")

(defvar *authentication-token*)
(setf (documentation '*authentication-token* 'variable)
      "The current authentication token. Can be bound for incorporation in service instances.
 If it is not bound at the point where it would be required, value for the environment variable 'DYDRA_TOKEN'
 and the content of the file '~/.dydra/token' are examined and the first extant is used.")

(defvar *authentication-token-environment-variable* "DYDRA_TOKEN")

(defvar *authentication-token-pathname*
  (merge-pathnames (make-pathname :directory '(:relative ".dydra") :name "token")
                   (user-homedir-pathname)))

(defparameter *file-type-mime-types*
  '(("ttl" . "text/turtle")             ; http://www.w3.org/TeamSubmission/turtle/#sec-mediaReg
    ("n3" . "text/n3")                  ; http://www.w3.org/TeamSubmission/n3/#mimetype
    ("nt" . "text/plain")               ; http://www.w3.org/TR/rdf-testcases/#ntriples
    ("json" . "application/json")       ; http://www.ietf.org/rfc/rfc4627.txt
    ("rdf" . "application/rdf+xml")     ; http://www.w3.org/TR/REC-rdf-syntax/
    ("xml" . "application/trix")))      ; http://www.hpl.hp.com/techreports/2003/HPL-2003-268.html

(defparameter dydra::*trace-output* nil
  "If non-null, emits json response trace output to the indicated stream.")


;;;
;;; utility operators

(defgeneric file-type-mime-type (type)
  (:method ((pathname pathname))
    (file-type-mime-type (pathname-type pathname)))
  (:method ((type string))
    (or (rest (assoc type *file-type-mime-types* :test #'string-equal))
        (error "Unsupported file type: ~s." type))))


(defun dydra::authenticate (&optional (new-token nil nt-s))
  "Set the authentication token as follows:
 - iff a value is provided, constraint it to be (or string null) and set the token to it.
 - otherwise
   - use the DYDRA_TOKEN environment variable, if present
   - use the content of the ~/.dydra/token file, if present
 if no value is provided, and none is found, set the token to nil, which permits just
 unauthenticated access."

  (setq *authentication-token* 
        (cond (nt-s
               (etypecase new-token ((or null string) new-token)))
              ((asdf:getenv *authentication-token-environment-variable*))
              ((with-open-file (stream *authentication-token-pathname*
                                       :direction :input :if-does-not-exist nil)
                 (when stream (read-line stream))))
              (t
               nil))))

(defun authentication-token ()
  "Iff a token is set, return it. Otherwise search for a default value, cache and return it."
  (if (boundp '*authentication-token*)
    *authentication-token*
    (dydra::authenticate)))

(defgeneric uri-string (uri)
  (:method ((uri string)) uri)
  #+org.datagraph.spocq  (:method ((uri spocq:iri)) (spocq:iri-lexical-form uri))
  (:method ((uri puri:uri)) (with-output-to-string (stream) (puri:render-uri uri stream))))


;;;
;;; classes

(defclass dydra::service ()
  ((url
    :initarg :url :initform (error "url is required.")
    :reader dydra::service-url
    :type puri:uri
    :documentation "The root url to access the storage service, specialized
     for the respective protocol. If *authentication-token* is non-null, the
     'user_auth' query parameter is added to each request url.")
   (account-name
    :initarg :account-name :initform (error "account-name is required.")
    :reader dydra::service-account-name
    :type string
    :documentation "Specifies the scope for account-wide operations such as listing
     repositories. For repository-specific operations, on the other hand, the repository
     id determines the scope and the authentcation status the access within that scope.")
   (token
    :initarg :token :initarg :authentication-token
    :initform (authentication-token)
    :reader service-token :reader service-authentication-token
    :type (or null string)
    :documentation "An authentication token is held separate from the URL and passed with other
     URL query parameters through the rpc/http interface.")
   (protocol
    :initform (error "protocol is required.")
    :reader dydra::service-protocol
    :documentation "A keyword for print-object"))
  (:documentation "A service instance comprises the location, identification, and authentication
 information for a Dydra storage service. Three specializations are defined:
 - json-rest-service : performs operations through the http rest interface
 - json-rpc-service : effects operations theough the store's json rpc interface
 - xml-rpc-service : effects operations theough the store's xml rpc interface"))

(defclass dydra::rest-service (dydra::service)
  ((protocol
    :initform :rest/http))
  (:default-initargs :path "")
  (:documentation "A rest service instance specializes the service class to implement the operations
 in terms of an http-rest service. The resource are mostly drawn from the sesame 2.0 standard
 [http://www.openrdf.org/doc/sesame2/system/ch08.html#d0e168],  with extensions for Dydra-specific
 functions."))

(defclass dydra::rpc-service (dydra::service)
  ((request-id
    :initform 0
    :accessor service-request-id
    :documentation "Caches the auto-increment xml rpc request id."))
  (:default-initargs :path "/rpc")
  (:documentation "An rpc-service instance specializes the service class to implement operations in
 terms of an xml-rpc service."))

(defclass dydra::amqp-rpc-service (dydra::rpc-service)
 ()
 (:documentation "NYI"))

(defclass dydra::repository ()
  ((service
    :initarg :service :initform (or dydra::*service* (error "service is required."))
    :reader dydra::repository-service
    :type dydra::service)
   (id
    :initarg :id :initform (error "id is required.")
    :reader dydra::repository-id
    :type string)
   (info-cache
    :initarg :info-cache :initarg :info         ; no initform to trigger retrieval
    :reader repository-info-cache))
  (:documentation "A repository comprises an identifier (combined account and repository names), with a
 service instance to mediate operations on a Dydra store. The service instance specializes the operation
 methods in terms of rest or rpc access. In addition various metadata are cached in the repository
 instance."))

(define-condition dydra::json-error (simple-error)
  ((code :initarg :code :initform 0 :reader condition-code)
   (message :initarg :message :initform "" :reader condition-message)
   (data :initarg :data :initform nil :reader condition-data))
  (:report (lambda (condition stream)
             (format stream "json error: code ~s, data ~s~&~a"
                     (condition-code condition)
                     (condition-data condition)
                     (condition-message condition)))))


(defmethod initialize-instance ((instance dydra::service) &rest args &key
                                host port url path)
  (declare (dynamic-extent args))
  (cond (url
         (setf url (puri:uri url)))
        (host
         (setf url (make-instance 'puri:uri :scheme :http :host host :port port :path path)))
        (t
         (error "Some combination of host and url is required.")))
  (apply #'call-next-method instance :url url args))

(defmethod initialize-instance :after ((instance dydra::repository) &key base-uri context query-language infer)
  "Declare validity for api arguments possible passed to repository constructor."
  (declare (ignore base-uri context query-language infer))
  nil)


(defmethod print-object ((object dydra::service) stream)
  (print-unreadable-object (object stream :identity t :type t)
    (with-slots (url account-name protocol) object
      (format stream "~a @ ~a[~a]"
              (when (slot-boundp object 'account-name) account-name)
              (when (slot-boundp object 'url) (puri:uri-host url))
              (when (slot-boundp object 'protocol) protocol)))))

(defmethod print-object ((object dydra::repository) stream)
  (print-unreadable-object (object stream :identity t :type t)
    (with-slots (id service ) object
      (format stream "~a @ ~a[~a]"
              (when (slot-boundp object 'id) id)
              (when (slot-boundp object 'service) (puri:uri-host (dydra::service-url service)))
              (when (slot-boundp object 'service) (dydra::service-protocol service))))))


(defmethod dydra::repository-service ((service dydra::service))
  "Given a service instance, just return it."
  service)


(defgeneric repository-name-components (repository)
  (:documentation "Parse a composite repository id into the constituent account and repository name
 according to the pattern:
   <account name>/<repository name>
 and return them as two values.")

  (:method ((repository-id string))
    (let ((slash (or (position #\/ repository-id) (error "invalid repository id: ~s." repository-id))))
      (values (subseq repository-id 0 slash) (subseq repository-id (1+ slash)))))

  (:method ((repository dydra::repository))
    (repository-name-components (dydra::repository-id repository))))


(defun dydra::repository-account-name (repository)
  (nth-value 0 (repository-name-components repository)))

(defun dydra::repository-name (repository)
  (nth-value 1 (repository-name-components repository)))


;;;
;;; constructors

(defun dydra:service (&rest args &key host port url account-name token)
  "Instantiate a Dydra service instance given location, identification, and authentication parameters.
 :HOST : string : the service's host. Used to construct the URI if none is provided.
 :PORT : string : the service's port. Used to construct the URI if none is provided.
 :ACCOUNT-NAME : string : a Dydra account name
 :TOKEN : string : a Dydra authentication token. Defaults to *authentication-token*
 :URI : (or string puri:uri) : an absolute URI to locate the service

 The class is specified by the current dydra:*service-class* value."

  (declare (ignore host port url account-name token)
           (dynamic-extent args))
  (apply #'make-instance dydra:*service-class* args))


(defun dydra:repository (&rest args &key authority host account-name repository-name id service)
  (declare (ignore authority host account-name repository-name id service)
           (dynamic-extent args))
  (apply #'make-instance dydra:*repository-class* args))


;;;
;;; external library interfaces for requests

(defgeneric http-request (service url &rest args &key parameters &allow-other-keys)
  (declare (dynamic-extent args))
  (:method ((service dydra::service) url &rest args &key parameters &allow-other-keys)
    (declare (dynamic-extent args))
    (let ((token (service-token service)))
      (apply #'drakma:http-request url
             :parameters (if token (cons `("auth_token" . ,token) parameters) parameters)
             args))))

(defgeneric rpc-call (service method rpc-arguments &rest args &key parameters &allow-other-keys)
  (declare (dynamic-extent args)))


;;;
;;; service-specialized operators

(defgeneric dydra::account-repositories (service name)
  (:documentation "Given a SERVICE and an ACCOUNT-NAME, return information about the
 repositories registered under the account.

  SERVICE : service : a concrete storage service
  ACCOUNT-NAME : (or repository service string)
  VALUE ; list :  as a list of association lists.

 Each repository is described by an association list with the following entries:
 :byte--size
 :description
 :name
 :homepage
 :summary
 :triple--count

 In addition to a literal account name string, the second argument may be a service or a repository, in
 which case it designates the respective service;s account.")

  (:method ((service dydra::service) (name dydra::repository))
    (dydra::account-repositories service (dydra::repository-service name)))

  (:method ((service dydra::service) (name dydra::service))
    (dydra::account-repositories service (dydra::service-account-name name)))

  (:method ((service dydra::rpc-service) (name string))
    ;; rewrite the results to match the rest result structure
    (let ((response (rpc-call service "dydra.repository.list" `(:string ,name))))
      (loop for (account-name repository-name) in  response
            if (string-equal name account-name)
            collect `((:name . ,repository-name))
            else do (error "invalid account in repositories result: ~s." account-name)))))


(defgeneric dydra::repository-clear (service repository)
  (:documentation "Given a SERVICE and a REPOSITORY, remove the designated repository's statements
    from the store.

  SERVICE : service : a concrete storage service
  REPOSITORY : (or repository string)

 Given an rpc service, call the method dydra.repository.clear
 Given an http service, :delete the resource http://<authority>/repositories/<repository-id>/statements")

  (:method ((service t) (repository dydra::repository))
    (dydra::reset-repository-info repository)
    (dydra::repository-clear service (dydra::repository-id repository)))

  (:method ((service dydra::rpc-service) (repository-id string))
    (rpc-call service "dydra.repository.clear" `(:string ,repository-id))))


(defgeneric dydra::repository-create (service repository)
  (:documentation "Create a new repository.

  SERVICE : service : a storage service instance
  REPOSITORY : (or repository string) : designates the new repository

 Given an rpc service, call the method dydra.repository.create
 Given an http service, :post the repository metadata to http://<authority>/repositories/<repository-id>")

  (:method ((service t) (repository dydra::repository))
    (dydra::reset-repository-info repository)
    (dydra::repository-create service (dydra::repository-id repository)))

  (:method ((service dydra::rpc-service) (repository-id string))
    (rpc-call service "dydra.repository.create" `(:string ,repository-id))))


(defgeneric dydra::repository-delete (service repository)
  (:documentation "Delete an existing repository.

  SERVICE : service
  REPOSITORY : (or repository string) : designates an extant repository

 Given an rpc service, call the method dydra.repository.destroy
 Given an http service, :delete the resource http://<authority>/repositories/<repository-id>")
  
  (:method ((service t) (repository dydra::repository))
    ;; do not clear cached info
    (dydra::repository-delete service (dydra::repository-id repository)))

  (:method ((service dydra::rpc-service) (repository-id string))
    (rpc-call service "dydra.repository.destroy" `(:string ,repository-id))))


(defgeneric dydra::repository-import (service repository url &key context base-uri)
  (:documentation "Import a designated resource into an existing repository.

  SERVICE : service
  REPOSITORY : (or repository string) : designates an extant repository

 Given an rpc service, call the method dydra.repository.import
 Given an http service, :put the source resource to http://<authority>/repositories/<repository-id>/statements")
  
  (:method ((service t) (repository dydra::repository) url &rest args)
    (dydra::reset-repository-info repository)
    (apply #'dydra::repository-import service (dydra::repository-id repository) url args))
  
  (:method ((service t) (repository t) (url string) &rest args)
    (apply #'dydra::repository-import service repository (puri::uri url) args))

  (:method ((service dydra::rpc-service) (repository-id string) (resource-uri puri:uri) &key
            (context "") (base-uri ""))
    (rpc-call service "dydra.repository.import" 
              `(:string ,repository-id :string ,(uri-string resource-uri) :string ,context :string ,base-uri))))


(defgeneric dydra::repository-import-status (service repository)
  (:documentation "Import a designated resource into an existing repository.

  SERVICE : service
  REPOSITORY : (or repository string) : designates an extant repository

 Given an rpc service, call the method dydra.repository.import
 Given an http service, :put the source resource to http://<authority>/repositories/<repository-id>/statements")

  (:method ((service t) (repository dydra::repository))
    (dydra::repository-import-status service (dydra::repository-id repository)))

  (:method ((service dydra::rpc-service) (repository-id string))
    (rpc-call service "dydra.repository.info" `(:string ,repository-id))))


(defgeneric dydra::repository-info (service repository)
  (:documentation "Return the status of the latest import to a repository.

  SERVICE : service
  REPOSITORY : (or repository string) : designates an extant repository

 Given an rpc service, call the method dydra.repository.info
 Given an http service, :get http://<authority>/repositories/<repository-id>/status

 The result is cached in the repository instance to be returned by future invocations.")

  (:method ((service t) (repository dydra::repository))
    (if (slot-boundp repository 'info-cache)
      (repository-info-cache repository)
      (let ((info (dydra::repository-info service (dydra::repository-id repository))))
        (when info
          (setf (slot-value repository 'info-cache) info)))))

  (:method ((service dydra::rpc-service) (repository-id string))
    (rpc-call service "dydra.repository.info" `(:string ,repository-id))))

(defgeneric dydra::reset-repository-info (repository-instance)
  (:documentation "Clear cached info from a repository instance.")

  (:method ((repository dydra::repository))
    (slot-makunbound repository 'info-cache)))


(defgeneric dydra::repository-query (service repository query &rest args
                                             &key query-language infer &allow-other-keys)
  (:documentation "Return the status of the latest import to a repository.

  SERVICE : service
  REPOSITORY : (or repository string) : designates an extant repository
  QUERY : string : the query expression
  :QUERY-LANGUAGE : string : 
  :INFER : boolean : 
 Given an rpc service, call the method dydra.repository.import-status
 Given an http service, :get http://<authority>/repositories/<repository-id>/status")

  (:method ((service t) (repository dydra::repository) query &rest args)
    (declare (dynamic-extent args))
    (apply #'dydra::repository-query service (dydra::repository-id repository) query args))

  (:method ((service dydra::rpc-service) (repository-id string) query &rest args
            &key (query-language nil ql-s) (infer nil i-s) &allow-other-keys)
    (apply #'rpc-call service "dydra.repository.query"
           `(:string ,repository-id :string ,query
                     ,@(when (or ql-s i-s) `(:array
                                             ,@(when ql-s `(:array (:string ,"queryLn"
                                                                            :string ,query-language)))
                                             ,@(when i-s `(:array (:string ,"infer"
                                                                           :string ,(if infer "true" "false")))))))
           args)))


(defgeneric dydra::service-info (service)
  (:method ((repository dydra::repository))
    (dydra::service-info (dydra::repository-service repository)))

  (:method ((service dydra::rpc-service))
    (rpc-call service "dydra.account.info" `(:string ,(dydra::service-account-name service)))))


;;;
;;; interface operators

(defun call-with-effective-repository (function &rest args &key (repository dydra::*repository*)
                                                (service dydra::*service*) id
                                                &allow-other-keys)
  (cond (id
         (funcall function (dydra:repository :id id :service service)))
        (repository
         (funcall function repository))
        (t
         (funcall function (apply #'dydra::repository args)))))

(defun remove-property (list &rest keywords)
  (declare (dynamic-extent keywords))
  (flet ((maybe-remf (maybe-list property)
           (when (getf list property)
             (when (eq maybe-list list)
               (setf list (copy-list list)))
             (remf list property))))
    (dolist (keyword keywords) (maybe-remf list keyword))
    list))

(defmacro with-effective-repository ((repository args) &rest body)
  (let ((op (gensym )))
    `(flet ((,op (,repository) ,@body))
       (declare (dynamic-extent #',op))
       (apply #'call-with-effective-repository #',op ,args))))


(defun dydra::clear (&rest args &key repository service host port id)
  "Given a REPOSITORY, delete all statements from it."
  (declare (dynamic-extent args)
           (ignore repository service host port id))
  (with-effective-repository (the-repository args)
    (dydra::repository-clear (dydra::repository-service the-repository) the-repository)))


(defun dydra::count (&rest args &key repository service host port id)
  "Given a REPOSITORY, return the count of its statements."
  (declare (dynamic-extent args)
           (ignore repository service host port id))
  (with-effective-repository (the-repository args)
    (rest (assoc :triple--count (dydra::repository-info (dydra::repository-service the-repository) the-repository)))))


(defun dydra::create (&rest args &key repository service host port id metadata)
  "Given a REPOSITORY instance, create it in the respective service."
  (declare (dynamic-extent args)
           (ignore repository service host port id metadata))
  (with-effective-repository (the-repository args)
    (dydra::repository-create (dydra::repository-service the-repository) the-repository)))


(defun dydra::delete (&rest args &key repository service host port id)
  "Given a REPOSITORY, remove it from the store."
  (declare (dynamic-extent args)
           (ignore repository service host port id))
  (with-effective-repository (the-repository args)
    (dydra::repository-delete (dydra::repository-service the-repository) the-repository)))


(defun dydra::import (url &rest args &key repository service host port id base-uri context)
  "Given a URL and a REPOSITORY, import the resource's statements into the repository.
 Permit keyword arguments for context and base-uri tp be applied during the import process."
  (declare (ignore repository service host port id)
           (dynamic-extent args))
  (with-effective-repository (the-repository args)
    (dydra::repository-import (dydra::repository-service the-repository) the-repository url
                              :base-uri base-uri :context context)))


(defun dydra::import-status (&rest args &key repository service host port id)
  "Given a REPOSITORY, return the status information for its most recent import operation."
  (declare (ignore repository service host port id)
           (dynamic-extent args))
  (with-effective-repository (the-repository args)
    (dydra::repository-import-status (dydra::repository-service the-repository) the-repository)))


(defgeneric dydra:info (subject &rest args)
  (:documentation "Return metadata about the Given repository or service.")
  (:method ((repository dydra::repository) &key)
    (dydra::repository-info (dydra::repository-service repository) repository))
  (:method ((arg symbol) &rest args)
    (dydra:info (apply #'dydra::repository arg args)))
  (:method ((service dydra::service) &rest args)
    (if args
      (dydra:info (apply #'dydra::repository :service service args))
      (dydra::service-info service))))

(defun dydra:query (query-string &rest args &key repository service host port id query-language infer
                                  &allow-other-keys)
  "Given a QUERY-STRING and a REPOSITORY, apply the duery to the repository."
  (declare (ignore repository service host port id query-language infer)
           (dynamic-extent args))
  (with-effective-repository (the-repository args)
    (unless (search "select" query-string :test #'char-equal)
      (dydra::reset-repository-info the-repository))
    (apply #'dydra::repository-query (dydra::repository-service the-repository) the-repository query-string
           (remove-property args :repository :service :host :port :id))))


(defun dydra:map-query (type function query-string &rest args &key
                              ((:blank-nodes dydra::*blank-nodes*) (or dydra::*blank-nodes* (make-hash-table :test 'equal)))
                              ((:uris dydra::*uris*) (or dydra::*uris* (make-hash-table :test 'equal)))
                              &allow-other-keys)
  "Given a QUERY-STRING and a REPOSITORY, apply the duery to the repository."
  (declare (dynamic-extent args))
  (setf args (remove-property args :blank-nodes :uris))
  (let* ((solution-field (apply #'dydra::query query-string (remove-property args :uris :blank-nodes))))
    (when solution-field
      (let* ((variable-names (rest (first solution-field)))
             (variables (mapcar #'make-symbol variable-names))
             (filter (compile nil `(lambda (solution)
                                           (destructuring-bind ,variables solution
                                             (funcall ,function
                                                      ,@(mapcar #'(lambda (variable)
                                                                    `(intern-literal (rest (assoc :value ,variable))
                                                                                     (rest (or (assoc :datatype ,variable)
                                                                                               (assoc :type ,variable)))
                                                                                     (rest (assoc :|XML:LANG| ,variable))))
                                                                variables)))))))
        (map type filter (rest (second solution-field)))))))

(defun dydra:query-values (query-string &rest args)
  (apply #'dydra:map-query 'list #'list query-string args))


(defun dydra:repositories (&optional (repository (or dydra::*repository* (error "repository is required."))))
  "Given a REPOSITORY return descriptions of the repositories present in the same account
 on the repository's service."
  (dydra::account-repositories (dydra::repository-service repository) repository))


