;;; the Fabric server main program

(module-name FabricServer)
(module-compile-options main: #t)

(require "app-server.scm")

(define $server (make-server))
(start-server $server)


