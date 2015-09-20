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

(require util-error)
(require util-java)
(require util-lists)
(require data-nodes)

;;; ---------------------------------------------------------------------
;;; Java imports
;;; ---------------------------------------------------------------------

(import (class com.jme3.app Application))
(import (class com.jme3.app.state AbstractAppState AppStateManager))
(import (class com.jme3.renderer RenderManager))

;;; ---------------------------------------------------------------------
;;; the client AppState class
;;; ---------------------------------------------------------------------
;;; the methods of FabricClientState, and of all sublcasses of it,
;;; are implemented by simply calling functions that implement the
;;; behavior of the methods. We do it this way so that redefinition
;;; of the method implementations does not change the implementation
;;; of the class itself, which in turn avoids the necessity of
;;; restarting Kawa when class methods are redefined. (Kawa on the
;;; JVM cannot redefine classes themselves once they are defined,
;;; except by killing and restarting the process.)

;;; CLASS FabricClientState
;;; ---------------------------------------------------------------------

(define-simple-class FabricClientState (AbstractAppState)
  ;; slots
  (client init: #!null)
  ;; methods
  ((cleanup) (%state-cleanup (this)))
  ((initialize) (%state-initialize (this)))
  ((isEnabled) (%state-enabled? (this)))
  ((isInitialized) (%state-initialized? (this)))
  ((stateAttached state-manager::AppStateManager)
   (%state-attached (this) state-manager))
  ((stateDetached state-manager::AppStateManager)
   (%state-detached (this) state-manager))
  ((handleAnalogEvent name value tpf)
   (format #t "~%handleAnalogEvent must be implemented in a subclass of FabricClientState"))
  ((handleActionEvent name key-pressed? tpf)
   (format #t "~%handleActionEvent must be implemented in a subclass of FabricClientState")))

(define (%state-cleanup state::FabricClientState) #!void)
(define (%state-initialize state::FabricClientState) #!void)
(define (%state-enabled? state::FabricClientState) #t)
(define (%state-initialized? state::FabricClientState) #t)
(define (%state-attached state::FabricClientState manager::AppStateManager) #!void)
(define (%state-detached state::FabricClientState manager::AppStateManager) #!void)

