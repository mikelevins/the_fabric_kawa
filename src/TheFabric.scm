;;; the Fabric client main program

(module-name TheFabric)
(module-compile-options main: #t)

(require client)

(the-client (make-client))
(let ((client::FabricClient (the-client)))
  (client:start))




