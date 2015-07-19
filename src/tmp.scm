(defclass FabricClientState (AbstractAppState)
  (slots:
   (client init-form: #!null getter: getClient setter: setClient)) 
  (methods:
   ((cleanup) (%state-cleanup (this)))
   ((initialize) (%state-initialize (this)))
   ((isEnabled) (%state-enabled? (this)))
   ((isInitialized) (%state-initialized? (this)))
   ((stateAttached state-manager::AppStateManager)
    (%state-attached (this) state-manager))
   ((stateDetached state-manager::AppStateManager)
    (%state-detached (this) state-manager))))
