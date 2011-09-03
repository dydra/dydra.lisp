Dydra.com Software Development Kit (SDK) for Common Lisp
========================================================

This is the Common Lisp software development kit
([SDK](https://github.com/dydra/dydra-cl)) for
[Dydra.com][], the cloud-hosted graph store.

Examples
--------


Three service classes are implemented: 
* `dydra:json-rpc-service` : uses an xml-rpc interface via json and http transport
* `dydra:xml-rpc-service` : uses an xml-rpc interface via xml and http transport
* `dydra:json-rest-service` : uses an http client interface via json and Dydra's Sesame endpoint.

The json rest service is the first to be released.
It appears in this example.
The results for the other interfaces would be analogous.


### Authenticate with an API token

First, in order to operate on a private repository, each request must include an authentication token.
In order to provide an explicit authentication token, call `authenticate` with the token. It is then
bound to the global `*authentication-token*` to be used as the default value for service instances.
The value may be reset or rebound for use in a dynamic context.

    * (dydra:authenticate "my token")
    "my token"

A call to `authenticate` with a `nil` token, disables authentication, which limits access read-only
to just public repositories. A call without an argument attempts to locate a token, first
in the environment variable "DYDRA_TOKEN" and then in the file `~/.dydra/token`. If neither is
present, the token is set to `nil`, which permits operations on public repositories only.

### Set up the service

Next, esablish a service to mediate the connections:

    * (setq dydra:*service* (dydra:json-rest-service :host "api.dydra.com" :account-name "jhacker"))
    #<DYDRA:JSON-REST-SERVICE jhacker @ api.dydra.com[REST/JSON/HTTP] {13B38359}>

### Take a look at the account's repositories

    * (dydra:repositories dydra:*service*)

    (((:BYTE--SIZE . 1480) (:DESCRIPTION . "") (:HOMEPAGE . "") (:NAME . "foaf")
      (:SUMMARY . "A test repository") (:TRIPLE--COUNT . 10)
      (:REPOSITORY--ID . "jhacker/foaf"))
     ...)
    200
    * (setq dydra:*repository* (dydra:repository :id "jhacker/foaf"))
    
    #<ORG.DATAGRAPH.DYDRA:REPOSITORY jhacker/foaf @ api.dydra.com[REST/JSON/HTTP] {1005206FA1}>
    * (dydra:info dydra:*repository*)

    ((:BYTE--SIZE . 1480) (:DESCRIPTION . "") (:HOMEPAGE . "") (:NAME . "foaf")
     (:SUMMARY . "A test repository") (:TRIPLE--COUNT . 10)
     (:REPOSITORY--ID . "jhacker/foaf"))
    *
    

### Create a new reposiory

    * (setq dydra:*repository* (dydra:repository :id "jhacker/api-test"))
    #<ORG.DATAGRAPH.DYDRA:REPOSITORY jhacker/new @ hetzner.dydra.com {13676C91}>

    * (setq dydra:*repository* (dydra:repository :id "jhacker/api-test"))

    #<ORG.DATAGRAPH.DYDRA:REPOSITORY jhacker/api-test @ api.dydra.com[REST/JSON/HTTP] {1005231AF1}>
    * (dydra::create :repository dydra:*repository*)

    ((:BYTE--SIZE . 0) (:DESCRIPTION) (:HOMEPAGE) (:NAME . "api-test") (:SUMMARY)
     (:TRIPLE--COUNT . 0) (:REPOSITORY--ID . "jhacker/api-test"))
    201
    *

### Import data into the repository

    * (dydra:import "http://rdf.dydra.com/jhacker/foaf.nt")
    
    NIL
    204
    * (dydra:info dydra:*repository*)
    
    ((:BYTE--SIZE . 5585) (:DESCRIPTION) (:HOMEPAGE) (:NAME . "api-test")
     (:SUMMARY) (:TRIPLE--COUNT . 10) (:REPOSITORY--ID . "jhacker/api-test"))
    * (dydra:import-status )
    
    ((:UUID . "247c0c50-b875-012e-b85e-123138102df0") (:STATUS . "completed")
     (:WORKING) (:TIME . "2011-09-03T16:10:19+00:00")
     (:MESSAGE . "Completed at 2011-09-03 16:14:17 +0000") (:PCT . 100))
    200
    * 


### Retrieve data from a repository

The `dydra:query` operators returns the raw decoded query result, in this case json/rest.

    * (dydra:query "select * where {?s ?p ?o} limit 3")
    
    ((:COLUMNS "s" "p" "o")
     (:ROWS
      (((:TYPE . "uri") (:VALUE . "http://datagraph.org/jhacker/foaf"))
       ((:TYPE . "uri")
        (:VALUE . "http://www.w3.org/1999/02/22-rdf-syntax-ns#type"))
       ((:TYPE . "uri")
        (:VALUE . "http://xmlns.com/foaf/0.1/PersonalProfileDocument")))
      (((:TYPE . "uri") (:VALUE . "http://datagraph.org/jhacker/#self"))
       ((:TYPE . "uri") (:VALUE . "http://xmlns.com/foaf/0.1/knows"))
       ((:TYPE . "uri") (:VALUE . "http://ar.to/#self")))
      (((:TYPE . "uri") (:VALUE . "http://datagraph.org/jhacker/#self"))
       ((:TYPE . "uri") (:VALUE . "http://xmlns.com/foaf/0.1/mbox"))
       ((:TYPE . "uri") (:VALUE . "mailto:jhacker@example.org"))))
     (:TOTAL . 3))
    200

while the `dydra:query-values` operator returns interned values

    * (dydra:query-values "select * where {?s ?p ?o} limit 3")
    
    ((#<PURI:URI http://datagraph.org/jhacker/foaf>
      #<PURI:URI http://www.w3.org/1999/02/22-rdf-syntax-ns#type>
      #<PURI:URI http://xmlns.com/foaf/0.1/PersonalProfileDocument>)
     (#<PURI:URI http://datagraph.org/jhacker/#self>
      #<PURI:URI http://xmlns.com/foaf/0.1/knows> #<PURI:URI http://ar.to/#self>)
     (#<PURI:URI http://datagraph.org/jhacker/#self>
      #<PURI:URI http://xmlns.com/foaf/0.1/mbox>
      #<PURI:URI mailto:jhacker@example.org>))
    * (length (remove-duplicates (mapcar #'first *)))

    2
    *

### Add an alternative home page

    * (dydra:query "insert data  {<http://datagraph.org/jhacker/#self> <http://xmlns.com/foaf/0.1/homepage> <http://dydra.com/jhacker/>}" )
    
    ((:COLUMNS "result") (:ROWS (T)) (:TOTAL . 1))
    200
    * (dydra:query-values "select * where {?s <http://xmlns.com/foaf/0.1/homepage> ?o}" )
    
    ((#<PURI:URI http://datagraph.org/jhacker/#self>
      #<PURI:URI http://dydra.com/jhacker/>)
     (#<PURI:URI http://datagraph.org/jhacker/#self>
      #<PURI:URI http://example.org/~jhacker/>))
    * (dydra:query "select (count (*) as ?count) where {?s ?p ?o}" )
    
    ((:COLUMNS "count")
     (:ROWS
      (((:TYPE . "typed-literal")
        (:DATATYPE . "http://www.w3.org/2001/XMLSchema#decimal")
        (:VALUE . "11.0"))))
     (:TOTAL . 1))
    200
    *

### Clear and/or delete the repository

    * (dydra:clear )                         ; clear it
    
    NIL
    204
    * (dydra:count )
    
    0
    * (dydra:delete :id "jhacker/api-test")
    
    NIL
    200
    * 

Dependencies
------------

If one requres the xml rpc service interface, roughly fifty systems contribute to a `dydra-cl` build.
The immediate dependencies include

* [cxml-rpc](https://github.com/antifuchs/cxml-rpc)
* [closure-html](http://common-lisp.net/project/closure/closure-html/)
* [cxml-stp](http://www.lichteblau.com/cxml-stp/)
* [cl-json](http://common-lisp.net/project/cl-json/)
* [puri](http://puri.b9.com/)

The json interfaces suffice with `cl-json` and `puri`.

The simplest way to start is to let `quicklisp` resolve the
dependencies and load everything from its curated sources:

    > (load "org/quicklisp/setup.lisp")
    > (ql:quickload "dydra")

Status
------

The API implementation is still in progress. Work remains to align it with the SDK APIs for other languages.
In particular, this release includes the json-rest interface only. json-rpc and xml-rpc remain to be
reconciled.

Download
--------

If one needs to work with copy of the development repository, do:

    $ git clone git://github.com/dydra/dydra-cl.git


Mailing List
------------

* <http://groups.google.com/group/dydra>

Authors
-------

* [James Anderson](https://github.com/lisp) - <http://dydra.com/james>

Contributing
------------

* Do your best to adhere to the existing coding conventions and idioms.
* Don't use hard tabs, and don't leave trailing whitespace on any line.
* Don't touch the `package.lisp`, `VERSION`, or `AUTHORS` files. If you need
  to change them, do so on your private branch only.
* Do feel free to add yourself to the `CREDITS` file and the corresponding
  list in the `README`. Alphabetical order applies.

Note: the instructions in this README, and the operation of the library
itself, implicitly assume a Unix system (Mac OS X, Linux, or *BSD) at
present. Patches improving Windows support are most welcome.

License
-------

This is free and unencumbered public domain software. For more information,
see <http://unlicense.org/> or the accompanying `UNLICENSE` file.

[RDF]:        http://www.w3.org/RDF/
[PDD]:        http://unlicense.org/#unlicensing-contributions
[Dydra.com]:  http://dydra.com/
