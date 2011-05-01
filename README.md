Dydra.com Software Development Kit (SDK) for Common Lisp
========================================================

This is the Common Lisp software development kit
([SDK](https://github.com/dydra/dydra-cl)) for
[Dydra.com][], the cloud-hosted RDF & SPARQL storage service.

Examples
--------

### Authenticate with an API token

Access to a private repository requires authentication.
In order to provide an explicit authentication token, call `authenticate` with the token. It is then
bound to the global `*authentication-token*` to be used as the default value for service instances.
The value may be reset or rebound for use in a dynamic context.

    > (dydra:authenticate "my token")
    "my token"

A call to `authenticate` with a `nil` token, disables authentication, which limits access read-only
to just public repositories. A call without an argument attempts to locate a token, first
in the environment variable "DYDRA_TOKEN" and then in the file `~/.dydta/token`. If neither is
present, the token is set to `nil`.

Three service classes are implemented: 
* `dydra:json-rpc-service` : uses an xml-rpc interface via json and http transport
* `dydra:xml-rpc-service` : uses an xml-rpc interface via xml and http transport
* `dydra:json-rest-service` : uses an http client interface via json and Dydra's Seasame endpoint.

The json rpc service appears in this example, but the results for the other interfaces would be analogous.

    > (setq dydra:*service* (dydra:rpc-service :authority "api.dydra.com" :account-name "jhacker"))
    #<ORG.DATAGRAPH.DYDRA:RPC-SERVICE jhacker @ api.dydra.com {13B38359}>

    > (setq dydra:*repository* (dydra:repository :id "jhacker/foaf"))
    #<ORG.DATAGRAPH.DYDRA:REPOSITORY jhacker/foaf @ api.dydra.com {13BAF6B1}>
    
### Enumerate an account's repositories

    > (dydra:repositories dydra:*service*)
    (((:NAME . "foaf")) ... )
    :struct

### Create a reposiory

    > (setq dydra:*repository* (dydra:repository :id "jhacker/new"))
    #<ORG.DATAGRAPH.DYDRA:REPOSITORY jhacker/new @ hetzner.dydra.com {13676C91}>

    > (dydra::create :repository dydra:*repository*)
    (("byte_size" :INTEGER 0) ("description" :STRING "") ("homepage" :STRING "")
     ("name" :STRING "new") ("summary" :STRING "") ("triple_count" :INTEGER 0))
    :STRUCT

### Import data into the repository

    > (dydra:import "http://www.w3.org/People/Berners-Lee/card" :repository dydra:*repository*)
    "d4cde500-53b3-012e-1b6a-001d92633de7"
    :STRING

    > (dydra:import-status)             ; rely on the default
    (("time" :INTEGER 1303988076) ("status" :STRING "completed")
     ("uuid" :STRING "d4cde500-53b3-012e-1b6a-001d92633de7")
     ("options" :STRUCT (("account" :STRING "5") ("repository" :STRING "5/280")
                         ("url" :STRING "http://www.w3.org/People/Berners-Lee/card")
                         ("context" :STRING "") ("base_uri" :STRING "")))
     ("name" :STRING "Dydra::Job::ImportDataFromURL({\"account\"=>\"5\", \"repository\"=>\"5/280\", \"url\"=>\"http://www.w3.org/People/Berners-Lee/card\", \"context\"=>\"\", \"base_uri\"=>\"\"})")
     ("num" :INTEGER 100) ("total" :INTEGER 100)
     ("message" :STRING "Completed at 2011-04-28 12:54:38 +0200"))
    :STRUCT

### Retrieving data from a repository, in this case json/rest

    > (dydra::query "select * where {?s ?p ?o} limit 3")          ; either raw, or
    ((:COLUMNS "s" "p" "o")
     (:ROWS
      (((:TYPE . "uri") (:VALUE . "http://www.w3.org/People/Berners-Lee/card#i"))
       ((:TYPE . "uri") (:VALUE . "http://xmlns.com/foaf/0.1/title"))
       ((:TYPE . "literal") (:VALUE . "Sir")))
      (((:TYPE . "uri") (:VALUE . "http://www.w3.org/People/Berners-Lee/"))
       ((:TYPE . "uri") (:VALUE . "http://xmlns.com/foaf/0.1/maker"))
       ((:TYPE . "uri") (:VALUE . "http://www.w3.org/People/Berners-Lee/card#i")))
      (((:TYPE . "uri") (:VALUE . "http://www.w3.org/People/Berners-Lee/card#i"))
       ((:TYPE . "uri") (:VALUE . "http://xmlns.com/foaf/0.1/phone"))
       ((:TYPE . "uri") (:VALUE . "tel:+1-(617)-253-5702"))))
     (:TOTAL . 3))
    200

    > (dydra:map-query 'list #'(lambda (s p o) (declare (ignore p o)) s)         ; similar, but interned
                 "select * where {?s ?p ?o} limit 10")
    (#<PURI:URI http://www.w3.org/People/Berners-Lee/card#i>
     #<PURI:URI http://www.w3.org/People/Berners-Lee/>
     #<PURI:URI http://www.w3.org/People/Berners-Lee/card#i>
     #<PURI:URI http://www.w3.org/People/Berners-Lee/card#i> #:|g24491800|
     #:|g25059020| #<PURI:URI http://dig.csail.mit.edu/2008/webdav/timbl/foaf.rdf>
     #<PURI:URI http://www4.wiwiss.fu-berlin.de/booksMeshup/books/006251587X>
     #:|g24650080| #<PURI:URI http://www.w3.org/People/Berners-Lee/card#i>)


Dependencies
------------

If one requres the xml rpc srevice interface, roughly fifty systems contribute to a `dydra-cl` build.
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
    > (ql:quickload "dydra-cl")

Status
------

The API implementation is still in progress. In two senses
* some work remains to align it with the SDK APIs for other languages, and
* server-side reamins to provide consistent results for analogous operations.

We expect a release by the end of week eighteen.

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
