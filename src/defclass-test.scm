(module-export TestApp)

(require "syntax-classes.scm")

(expand
 '(defclass TestApp (FabricApp)
    (annotations: @Serializable)
    (methods:
     ((onAnalog name value tpf) (begin #f))
     ((onAction name key-pressed? tpf) (begin key-pressed?))
     ((simpleInitApp)(init-client (this))))))

