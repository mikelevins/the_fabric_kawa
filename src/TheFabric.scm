;;; the Fabric client main program

(module-name TheFabric)
(module-compile-options main: #t)

(require "client.scm")

(the-client (make-client))
((the-client):start)




