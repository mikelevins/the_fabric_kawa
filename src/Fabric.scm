;;; the Fabric client main program

(module-name Fabric)
(module-compile-options main: #t)

(require "app-client.scm")


(define $client (make-client))
(invoke $client 'start)


