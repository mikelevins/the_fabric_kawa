;;; the Fabric server main program

(module-name FabricServer)
(module-compile-options main: #t)
(module-export fabric-manager)

(require "app-server.scm")

(define fabric-manager (make-parameter (make-server)))

(begin
  (start-listener (fabric-manager))
  (kawa.Shell:run (gnu.expr.Language:getDefaultLanguage)
                  (gnu.mapping.Environment:getCurrent)))





