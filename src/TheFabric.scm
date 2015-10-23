;;; the Fabric client main program

(module-name TheFabric)
(module-compile-options main: #t)

(require client)

(the-client (make-client))
(let ((client::FabricClient (the-client)))
  ;; TODO: this initialization of defaults is to support demos
  ;;       the demo defaults should be removed before a release
  (init-defaults "Jupiter")
  (client:start))




