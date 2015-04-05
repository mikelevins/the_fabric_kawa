;;;; ***********************************************************************
;;;;
;;;; Name:          client-state.scm
;;;; Project:       The Fabric: a far-future MMORPG
;;;; Purpose:       fabric AppStates
;;;; Author:        mikel evins
;;;; Copyright:     2015 by mikel evins
;;;;
;;;; ***********************************************************************
(format #t "~%loading client-state.cm")

(module-export
 FabricClientState)

;;; ---------------------------------------------------------------------
;;; required modules
;;; ---------------------------------------------------------------------

(require "util-error.scm")
(require "util-java.scm")
(require "util-lists.scm")
(require "data-nodes.scm")
(require "syntax-classes.scm")

;;; ---------------------------------------------------------------------
;;; Java imports
;;; ---------------------------------------------------------------------

(import-as AbstractAppState com.jme3.app.state.AbstractAppState)
(import-as Application com.jme3.app.Application)
(import-as AppStateManager com.jme3.app.state.AppStateManager)
(import-as Boolean java.lang.Boolean)
(import-as Float java.lang.Float)
(import-as RenderManager com.jme3.renderer.RenderManager)

;;; ---------------------------------------------------------------------
;;; the client AppState class
;;; ---------------------------------------------------------------------

;;; CLASS FabricClientState
;;; ---------------------------------------------------------------------

(defclass FabricClientState (AbstractAppState)
  (slots:
   (cleanupFn #!null getter: getCleanupFn setter: setCleanupFn)
   (initializeFn #!null getter: getInitializeFn setter: setInitializeFn)
   (isEnabledFn #!null getter: getIsEnabledFn setter: setIsEnabledFn)
   (isInitializedFn #!null getter: getIsInitializedFn setter: setIsInitializedFn)
   (postRenderFn #!null getter: getPostRenderFn setter: setPostRenderFn)
   (renderFn #!null getter: getRenderFn setter: setRenderFn)
   (setEnabledFn #!null getter: getSetEnabledFn setter: setSetEnabledFn)
   (stateAttachedFn #!null getter: getStateAttachedFn setter: setStateAttachedFn)
   (stateDetachedFn #!null getter: getStateDetachedFn setter: setStateDetachedFn)
   (updateFn #!null getter: getUpdateFn setter: setUpdateFn)) 
  (methods:
   ((cleanup)(unless (jnull? cleanupFn)(cleanupFn (this))))
   ((initialize state-manager::AppStateManager app::Application)
    (unless (jnull? initializeFn)(initializeFn (this) state-manager app)))
   ((isEnabled)(unless (jnull? isEnabledFn)(isEnabledFn (this))))
   ((isInitialized)(unless (jnull? isInitializedFn)(isInitializedFn (this))))
   ((postRender)(unless (jnull? postRenderFn)(postRenderFn (this))))
   ((render render-manager::RenderManager)(unless (jnull? renderFn)(renderFn (this) render-manager)))
   ((setEnabled enabled?::Boolean)(unless (jnull? setEnabledFn)(setEnabledFn (this) enabled?)))
   ((stateAttached state-manager::AppStateManager)(unless (jnull? stateAttachedFn)(stateAttachedFn (this))))
   ((stateDetached state-manager::AppStateManager)(unless (jnull? stateDetachedFn)(stateDetachedFn (this))))
   ((update tpf::Float)(unless (jnull? updateFn)(updateFn (this))))))




