Dydra.com Software Development Kit (SDK) for Common Lisp
========================================================

I am curious about the open-source ecosystsm.
Particularly open-source Lisp. In the past, I have accumulated dependency information from
system definitions and even active runtimes, but that process can demand significant resources
to develop a detailed view of changes. A store like Dydra simplifies ths immensely.

Rather than require the application to load hundreds of ASDF systems and hold the definitions in order to
examine relationships or to be limited to those relations which
one could express in advance, an RDF store permits to explore the relationships as one proceeds
and to develop insights over time.

Take the example, which files to compile and load, in which succession, in order to
build a given system. Rather simple. For example, for `dydra`, a method which comes to mind
immediately is to

   (load :org.datagraph.dydra)

It is after all `asdf`. On the other hand, how does one

- determine the files to check for susceptibility to changes to changes definitions in a given file?
- get the full list of files necessary to build a system?
- see what files were added to or removed between code revisions?
- see which dependencies have changed between code revisions?
- visualize the file and/or system dependncies for a system?
- decide which regression tests to run for a new revision?

or, more generally, how does one support any decisions or processes about a systems' components'
relations, but extend beyond just `compile-file` and `load` relations. The only presently available method is to
specialize `asdf` operation classes to extend or re-purpose the system operation machinery.
Which is not the direct way to solve the problem unless one already understands well the internals of asdf machinery.


A better approach is to factor the dependency analysis mechanism from the operation
mechanims and use the result of just the former to drive alternative tasks.
An easy way to do that is to use a data store well-suited to represent relationships
to make the dependency data available to domain-specific application logic.

System Visualization
--------------------

One example is, how to enumerate the constituent systems required to build a given software package
and visualize their dependencies. The necessary information is encoded in system
declarations in the respective `depends-on` property. As it happens, each `quicklisp` release distills
this data into a single file, `dists/quicklisp/systems.txt`. A simple transformation, on the order of

    (loop for system-dependencies = (read source nil nil)
          while system-dependencies
          do (destructuring-bind (system-name . dependencies)
                                 (asdf:split-string system-dependencies)
               (dolist (dep-name dependencies)
                 (format destination "<~a~a> <~a> <~a~a> .~%"
                         *system-base-uri-string* system-name
                         *depends-on-uri-string*
                         *system-base-uri-string* dep-name))))

renders it in to an RDF encoding suitable for import into a data store, as follows

(quicklisp-to-n3 #p"LIBRARY:org;quicklisp;dists;quicklisp;systems.txt"
                 #p"LIBRARY:org;quicklisp;dists;quicklisp;systems.n3")

(defparameter *ql* (dydra:repository :id "quicklisp/asdf"
                                     :service (dydra:rest-service :host "dydra.com"
                                                                  :account-name "quicklisp")))
(dydra:import #p"LIBRARY:org;quicklisp;dists;quicklisp;systems.n3"
              :repository *ql*)

(setq dydra:*nodes* (make-hash-table :test 'equal))
(defparameter *repository* (dydra:repository :id "jhacker/quicklisp"
                                             :service (dydra:rest-service :host "hetzner.dydra.com"
                                                                          :account-name "jhacker"
                                                                          :token nil)))

From which a straight-forward query retrieves a single dependency tree


(defun match-sp (subject predicate &key (repository *repository*) (uris *nodes*) object-type)
  (dydra:map-query 'list #'identity
                   (format nil "select ?o where { <~a> <~a> ?o }"
                           subject predicate)
                   :uris uris
                   :repository repository))
(defstruct (dydra::blank-node (:conc-name dydra::blank-node-)) (label))

(defgeneric dydra::format-term (stream object colon at)
  (:method (stream (object puri:uri) colon at)
    (declare (ignore colon at))
    (format stream "<~a>" object))
  (:method (stream (object symbol) colon at)
    (declare (ignore colon at))
    (case object
      ((t nil) (format stream "~:[false~;true~]" object))
      (t (format stream "?~a" (symbol-name object)))))
  (:method (stream (object dydra::blank-node) colon at)
    (declare (ignore colon at))
    (format stream "_:~a" (dydra::blank-node-label)))
  (:method (stream (object t) colon at)
    (declare (ignore colon at))
    (format stream "~s" object)))

(defun dydra::write-triple-pattern (stream pattern colon at)
  (declare (ignore colon at))
  (format stream "~{~/dydra:write-term/ ~/dydra:write-term/ ~/dydra:write-term/~}" pattern))

(defun dydra::write-graph-pattern (stream pattern colon at)
  (declare (ignore colon at))
  (format stream "~{~/dydra::write-triple-pattern/ .~^ ~}" pattern))

(defun dydra::get (patterns &rest args &key repository)
  (declare (ignore repository))
  (apply #'dydra:select (format nil "select ~{?~a }~% where {~{~%~% ~}~%~% }"
                                (dydra:expression-variables patterns)
                                patterns)
         args)

(defun match-po (predicate object &key (repository *repository*) (uris *nodes*))
  (dydra:get `((?::s ,predicate ,object)) 
  (dydra:map-query 'list #'identity
                   (format nil "select ?s where { ?s <~a> <~a> }"
                           predicate object)
                   :uris uris
                   :repository repository))

(defun system-dependencies (system repository)
  (let ((nodes (make-hash-table :test 'equal)))
    (labels ((dependencies (system)
               (let ((prerequisites (match-sp system *depends-on-uri-string*)))
                 (append (reduce #'append prerequisites :key #'dependencies)
                         (mapcar #'(lambda (prerequisite) (cons system prerequisite))
                                 prerequisites)))))
    (remove-duplicates (dependencies system) :from-end nil :key #'rest))))

    > (system-dependencies (puri:uri "http://www.quicklisp.org/system/hunchentoot")
                           *ql*)
    ((#<PURI:URI http://www.quicklisp.org/system/hunchentoot>
      . #<PURI:URI http://www.quicklisp.org/system/usocket>)
     ...
     (#<PURI:URI http://www.quicklisp.org/system/bordeaux-threads>
      . #<PURI:URI http://www.quicklisp.org/system/alexandria>))     

(defgeneric graph-system-dependencies (system output &key repository)
  (:method ((system t) (destination pathname) &rest args)
    (with-open-file (stream destination :direction :output :if-exists :supersede :if-does-not-exist :create)
      (apply #'graph-system-dependencies system stream args)))

  (:method ((system-name string) (destination t) &rest args)
    (apply #'graph-system-dependencies (puri:uri (format nil "~a~a" *system-base-uri-string* system-name))
           destination
           args))

  (:method ((system-uri puri:uri) (destination stream) &key (repository *repository*)
            (label "systems") ranksep)
    (flet ((uri-label (uri)
             (first (last (asdf:split-string (puri:uri-path uri) :separator '(#\/ #\#)))))
           (id (system)
               (cond ((gethash system ids))
                     (t
                      (setf (gethash system ids) (gensym "SYSTEM"))))))
      (let ((dependencies (system-dependencies system-uri repository)))
        (format destination "~&digraph ~a { ~@[ranksep=\"~d\";~]"
                (or label (uri-label system-id))
                ranksep)
        (loop for (system-uri . nil) in dependencies
              do (format destination "~&~a [ label=\"~a\" ]"
                         (id system-uri) (uri-label system-uri)))
        (loop for (system-uri . prerequisite-uri) in dependencies
              do (format destination "~&~a -> ~a"
                         (id system-uri) (id prerequisite-uri))))
      (format destination "~% }~%"))))

(defun encode-dependency-graph (dependencies destination &key (graph-id "untitled") graph-attributes
                                             (graph-nodes (make-hash-table)) (edge-label nil))
  (flet ((uri-label (uri)
           (first (last (asdf:split-string (puri:uri-path uri) :separator '(#\/ #\#)))))
         (id (system)
           (cond ((gethash system graph-nodes))
                 (t
                  (setf (gethash system graph-nodes) (gensym "SYSTEM"))))))
    (format destination "~&digraph ~a {" graph-id)
    (loop for (attribute-id . attribute-value) in graph-attributes
          do (format destination "~% ~a=\"~a\";" attribute-id attribute-value))
    (format destination "~%~%// systems")
    (loop for (system-uri . prerequisite-uri) in dependencies
          unless (gethash system-uri graph-nodes)
          do (format destination "~& ~a [ label=\"~a\" ];"
                     (id system-uri) (uri-label system-uri))
          unless (gethash prerequisite-uri graph-nodes)
          do (format destination "~& ~a [ label=\"~a\" ];"
                     (id prerequisite-uri) (uri-label prerequisite-uri)))
    (format destination "~%~%// dependencies")
    (loop for (system-uri . prerequisite-uri) in dependencies
          do (format destination "~&~a -> ~a~@[ [ label=\"~a\" ]~];"
                     (id system-uri) (id prerequisite-uri) edge-label))
    (format destination "~% }~%")))

#+(or)
(let ((s1 (puri:uri "/s1")) (s2 (puri:uri "/s2")) (s3 (puri:uri "/s3")))
  (encode-dependency-graph `((,s1 . ,s2) (,s1 . ,s3)) t :edge-label "a"))


(defun flatten-dependency-list (dependencies)
  (remove-duplicates (append (mapcar #'rest dependencies) (mapcar #'first dependencies)) :from-end t))

(defun dependency-list-dependents (dependency-list)
  (mapcar #'first dependency-list))

(defun dependency-prerequisites (dependency-list)
  (mapcar #'rest dependency-list))

(defun revision-changed-files (&key (old-revision "^HEAD") (new-revision "HEAD"))
  (let ((files ())
        (git-stream
         #+sbcl (sb-ext:process-output (process sb-ext:run-program *git-binary-pathname*
                                                `("diff" "--name-only" old-revision new-revision)
                                                :input nil :output :stream))
         #+clozure (ccl:external-process-output-stream (ccl:run-program *git-binary-pathname*
                                                `("diff" "--name-only" old-revision new-revision)
                                                :input nil :output :stream))))
    (unwind-protect (loop (let ((line (read-line git-stream nil nil)))
                            (if line (push line files) (return (reverse files)))))
      (when (open-stream-p git-stream)
        (close git-stream)))))




(defun definition-regression-tests (definition)
  "Match the tests which invoke the given definition from the current *repository* and return a list of 
 designators for the matching tests.

 DEFINITION : uri : The designator for the definition
 value : (list uri) : A list of test designators."

  (dydra:collect (?::test) (dydra:match `((?::test ,*invokes-uri* ,definition)))))


(defun file-definitions (file)
  "Match the definitions in a given file from the current *repository* and return a list of 
 designators for the matching definitions.

 DEFINITION : uri : The designator for the file
 value : (list uri) : A list of definition designators."

  (dydra:collect (?::definition) (dydra:match `((?::definition ,*component-uri* ,file)))))


(defun revision-regression-tests (old-revision new-revision &key ((:repository *repository*) *repository*))
  "Given two revision identifiers, return a list of identifiers for those regression
 tests which could be affected by the changes.

 The general mechansim is to propagate dependency from the files, to the functions, to those regressions
 active at some point when the function was active."

  (let* ((delta-files (revision-changed-files old-revision new-revision))
         (file-definitions (reduce delta-files #'append :key #'file-definitions))
         (deinition-tests (remove-duplicates (reduce file-definitions #'append :key #'definition-regression-tests)
                                             :from-end t)))
    definition-tests))


(defun run-revision-tests (
#+(or)
(let ((s1 (puri:uri "/s1")) (s2 (puri:uri "/s2")) (s3 (puri:uri "/s3")))
  (compute-operation-plan `((,s1 . ,s2) (,s1 . ,s3)) 'do-it))

;;; system ->
;;;   systems[] -> system file
;;;  read to extract
;;;   documentation
;;;   module/file dependency