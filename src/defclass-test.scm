(module-export TestApp)

(require "syntax-classes.scm")

(defclass TestApp (FabricApp)
  (methods:
   ((onAnalog name value tpf) (begin #f))
   ((onAction name key-pressed? tpf) (begin key-pressed?))
   ((simpleInitApp)(init-client (this)))))

