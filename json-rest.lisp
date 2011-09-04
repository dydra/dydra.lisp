;;; -*- Package: org.datagraph.dydra.implementation; -*-

(in-package :org.datagraph.dydra.implementation)

;;; This file implements rest / json / http service operators for the common-lisp
;;; interface to the Dydra storage service
;;;
;;;  Copyright 2011 [Datagraph Inc.](mailto:info@datagraph.org) All Rights Reserved
;;;

;;;
;;; class

(defclass dydra:json-rest-service (dydra:rest-service)
  ((protocol
    :initform :rest/json/http))
  (:documentation "A rest service instance which uses json to encode requests and responses."))

(defun rest-request-accept-json (service url &rest args &key (accept "application/json") &allow-other-keys)
  (declare (dynamic-extent args))
  (multiple-value-bind (result-stream code)
                       (apply #'http-request service url :accept accept :want-stream t args)
    (unwind-protect
      (case code
        ((200 201 302)
         (values (when (open-stream-p result-stream)
                   (when dydra::*trace-output*
                     (let ((body (make-array 1024 :element-type 'character :fill-pointer 0 :adjustable t))
                           (char #\0)
                           (body-stream nil))
                       (loop (unless (setf char (read-char result-stream nil nil))
                               (return))
                             (vector-push-extend char body))
                       (write-string body dydra::*trace-output*)
                       (setf body-stream (make-string-input-stream body))
                       (close result-stream)
                       (setf result-stream body-stream)))
                   (case (peek-char t result-stream nil nil)
                     ((nil) nil)
                     ((#\[ #\{) (json:decode-json result-stream))
                     (t (warn "invalid response to request: ~s." url)
                        (let ((body (make-array 1024 :element-type 'character :fill-pointer 0 :adjustable t))
                              (char #\0))
                          (loop (unless (setf char (read-char result-stream nil nil))
                                  (return body))
                                (vector-push-extend char body))))))
                 code))
        (t
         (values nil code)))
      (when (open-stream-p result-stream)
        (close result-stream)))))

(defun dydra:json-rest-service (&rest args)
  "Instantiate a JSON REST service instance given location, identification, and authentication parameters.
 :HOST : string : the service's host. Used to construct the URI if none is provided.
 :PORT : string : the service's port. Used to construct the URI if none is provided.
 :ACCOUNT-NAME : string : a Dydra account name
 :TOKEN : string : a Dydra authentication token. Defaults to *authentication-token*
 :URI : (or string puri:uri) : an absolute URI to locate the service"

  (declare (dynamic-extent args))
  (let ((dydra:*service-class* 'dydra:json-rest-service))
    (apply #'dydra:service args)))

;;;
;;; service-specialized operators

#-sesame-2.0
(defmethod dydra::account-repositories ((service dydra:json-rest-service) (name string))
  (rest-request-accept-json service
                            (format nil "~a/~a/repositories"
                                    (dydra::service-url service) name)))
#+sesame-2.0
(defmethod dydra::account-repositories ((service dydra:json-rest-service) (name string))
  (rest-request-accept-json service
                            (format nil "~a/repositories/~a"
                                    (dydra::service-url service) name)))


#-sesame-2.0                            ; 404 for "new"
(defmethod dydra::repository-clear ((service dydra:json-rest-service) (repository-id string))
  (multiple-value-bind (account-name repository-name) (repository-name-components repository-id)
    (rest-request-accept-json service (format nil "~a/~a/~a/clearRepository"
                                              (dydra::service-url service) account-name repository-name)
                            :method :delete)))

#+sesame-2.0
(defmethod dydra::repository-clear ((service dydra:json-rest-service) (repository-id string))
  (rest-request-accept-json service (format nil "~a/repositories/~a/statements"
                                            (dydra::service-url service) repository-id)
                            :method :delete))


#-sesame-2.0
(defmethod dydra::repository-create ((service dydra:json-rest-service) (repository-id string))
  (multiple-value-bind (account-name repository-name) (repository-name-components repository-id)
    (rest-request-accept-json service (format nil "~a/~a/repositories"
                                              (dydra::service-url service) account-name)
                              :method :post
                              :content-type "application/json"
                              :content #'(lambda (stream)
                                           (format stream "~%~%{'repository': {'name': \"~a\"}}"
                                                   repository-name)))))

#+sesame-2.0
(defmethod dydra::repository-create((service dydra:json-rest-service) (repository-id string))
  (rest-request-accept-json service (format nil "~a/repositories/~a"
                                            (dydra::service-url service) repository-id)
                            :method :put
                            :content nil))


(defmethod dydra::repository-delete ((service dydra:json-rest-service) (repository-id string))
  (multiple-value-bind (account-name repository-name) (repository-name-components repository-id)
    (rest-request-accept-json service (format nil "~a/~a/~a"
                                              (dydra::service-url service) account-name repository-name)
                              :method :delete)))


#-sesame-2.0
(defmethod dydra::repository-import ((service dydra:json-rest-service)
                                     (repository-id string) (resource-uri puri:uri) &key
                                     (context nil) (base-uri nil))
  "POST http://dydra.com/jhacker/clhs/uploadData?auth_token=xxxxx HTTP/x.x, Content-Type: <as per file>"
  (multiple-value-bind (account-name repository-name) (repository-name-components repository-id)
    (ecase (puri:uri-scheme resource-uri)
      (:file
       (error "file upload is not yet supported."))
      (:http
       (rest-request-accept-json service (format nil "~a/~a/~a/uploadURL"
                                                 (dydra::service-url service) account-name repository-name)
                                 :method :post
                                 :content-type "application/json"
                                 :content nil
                                 :parameters `(("url" . ,(uri-string resource-uri))
                                               ,@(when base-uri `("base_uri" . ,base-uri))
                                               ,@(when context `("context" . ,context))))))))
#+sesame-2.0
(defmethod dydra::repository-import ((service dydra:json-rest-service)
                                     (repository-id string) (resource-uri puri:uri) &key
                                     (context nil) (base-uri nil))
  "POST http://dydra.com/jhacker/clhs/uploadData?auth_token=xxxxx HTTP/x.x, Content-Type: <as per file>"
  (ecase (puri:uri-scheme resource-uri)
    (:file
     (let ((pathname (file-namestring (puri:uri-path resource-uri))))
       (rest-request-accept-json service (format nil "~a/repositories/~a/statements"
                                                 (dydra::service-url service) repository-id)
                                 :method :post
                                 :content-type (file-type-mime-type pathname)
                                 :content pathname
                                 :parameters `(,@(when base-uri `("base_uri" . ,base-uri))
                                               ,@(when context `("context" . ,context))))))
    (:http
     (rest-request-accept-json service (format nil "~a/repositories/~a/statements"
                                               (dydra::service-url service) repository-id)
                               :method :post
                               :content-type "application/json"
                               :content nil
                               :parameters `(("url" . ,(uri-string resource-uri))
                                             ,@(when base-uri `("base_uri" . ,base-uri))
                                             ,@(when context `("context" . ,context)))))))


#-sesame-2.0
(defmethod dydra::repository-import-status ((service dydra:json-rest-service) (repository-id string))
  (multiple-value-bind (account-name repository-name) (repository-name-components repository-id)
    (rest-request-accept-json service (format nil "~a/~a/~a/status"
                                              (dydra::service-url service) account-name repository-name))))

#+sesame-2.0
(defmethod dydra::repository-import-status ((service dydra:json-rest-service) (repository-id string))
  (rest-request-accept-json service (format nil "~a/repositories/~a/status"
                                            (dydra::service-url service) repository-id)))


#-sesame-2.0
(defmethod dydra::repository-info ((service dydra:json-rest-service) (repository-id string))
  (multiple-value-bind (account-name repository-name) (repository-name-components repository-id)
    (rest-request-accept-json service (format nil "~a/~a/~a/meta"
                                            (dydra::service-url service) account-name repository-name))))
#+sesame-2.0
(defmethod dydra::repository-info ((service dydra:json-rest-service) (repository-id string))
  (rest-request-accept-json service (format nil "~a/repositories/~a/meta"
                                            (dydra::service-url service) repository-id)))


#-sesame-2.0
(defmethod dydra::repository-query ((service dydra:json-rest-service) (repository-id string) (query string) &rest args
                                    &key (query-language nil) (infer nil) &allow-other-keys)
  (multiple-value-bind (account-name repository-name) (repository-name-components repository-id)
    (apply #'rest-request-accept-json service (format nil "~a/~a/~a/sparql"
                                                      (dydra::service-url service)
                                                      account-name repository-name)
           :content-type "application/json"
           :content #'(lambda (stream)
                        (when dydra::*trace-output*
                          (setf stream (make-broadcast-stream stream dydra::*trace-output*)))
                        (format stream "~%~%{\"query\": ~s~@[,~% \"queryLn\": '~a'~]~@[,~% \"infer\": \"~:[false~;true~]\"~]}"
                                query query-language infer))
           (remove-property args :query-language :infer))))

;;; two versions. one for get w/ url encoded parameters and one for post with json-encoded parameters in the request body
#+sesame-2.0+get-only
(defmethod dydra::repository-query ((service dydra:json-rest-service) (repository-id string) (query string) &rest args
                                    &key query-language infer parameters &allow-other-keys)
  (apply #'rest-request-accept-json service (format nil "~a/repositories/~a" (dydra::service-url service) repository-id)
         :method :get
         :parameters `(("query" . ,query)
                       ,@(when query-language `("queryLn" . ,query-language))
                       ,@(when infer `("infer" . ,(if infer "true" "false")))
                       ,@parameters)
         (remove-property args :query-language :infer)))

#+sesame-2.0
(defmethod dydra::repository-query ((service dydra:json-rest-service) (repository-id string) (query string) &rest args
                                    &key query-language infer &allow-other-keys)
  (apply #'rest-request-accept-json service (format nil "~a/repositories/~a" (dydra::service-url service) repository-id)
         :method :post
         :content-type "application/json"
           :content #'(lambda (stream)
                        (when dydra::*trace-output*
                          (setf stream (make-broadcast-stream stream dydra::*trace-output*)))
                        (format stream "~%~%{\"query\": ~s~@[,~% \"queryLn\": \"~a\"~]~@[,~% \"infer\": \"~:[false~;true~]\"~]}"
                                query query-language infer))
           (remove-property args :query-language :infer)))


#-sesame-2.0
(defmethod dydra::service-info ((service dydra:json-rest-service))
  (rest-request-accept-json service (format nil "~a/~a/repositories" (dydra::service-url service) (dydra::service-account-name service))))
#+sesame-2.0
(defmethod dydra::service-info ((service dydra:json-rest-service))
  (rest-request-accept-json service (format nil "~a/accounts/~a/meta" (dydra::service-url service) (dydra::service-account-name service))))


#|
(trace drakma:http-request)             ; if you want to watch
(dydra:authenticate "xxxx")

(setq dydra:*service* (dydra:json-rest-service :host "hetzner.dydra.com" :account-name "jhacker"))
(dydra:info dydra:*service*)           ; service info 

(setq dydra:*repository* (dydra:repository :id "jhacker/api-test"))

(dydra:repositories dydra:*service*)   ; list the account's repositories

(dydra:create :repository dydra:*repository*)          ; create a repository
(find "api-test" (dydra:repositories dydra:*service*)       ; check just its info from the account
      :test #'equal :key #'(lambda (i) (rest (assoc :name i))))
(dydra:info dydra:*repository*)        ; check info on the repository itself
(dydra:info :id "jhacker/api-test")

(dydra:import "http://rdf.dydra.com/jhacker/foaf.nt")     ; import data
(dydra:import-status )
(dydra:count )                         ; check the count

;;; add an alternative home page
(dydra:query "insert data  {<http://datagraph.org/jhacker/#self> <http://xmlns.com/foaf/0.1/homepage> <http://dydra.com/jhacker/>}" )
(dydra:query "select * where {?s <http://xmlns.com/foaf/0.1/homepage> ?o}" )
(dydra:query "select (count (*) as ?count) where {?s ?p ?o}" )

(dydra:clear )                         ; clear it
(dydra:count )                         ; check the count again

;;; delete
(dydra:delete :id "jhacker/api-test")

;;; query (imported w/ context
;;; uses application/json encoding rather than the standard www-form-urlencoded
(dydra:query "select * where {graph ?g {?s ?p ?o}}" )          ; should be nil as there is no context
(dydra:query "select * where {?s ?p ?o}" )     ; should be 10 solutions

(dydra:map-query 'list #'(lambda (s p o) (declare (ignore p o)) s)         ; similar, but just the first 10 subjects, interned
                 "select * where {?s ?p ?o} limit 10")



(defparameter *repo* (dydra:repository :id "jhacker/foaf"))
(dydra:info *repo*)
(dydra:query "select * where {?s ?p ?o}" :repository *repo*)

|#
