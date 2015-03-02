;;; the Fabric client main program

(module-name Fabric)
(module-compile-options main: #t)

(require "client-main.scm")

(define $client (make-client))
(invoke $client 'start)


