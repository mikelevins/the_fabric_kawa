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

(define (%state-cleanup state::FabricClientState) #!void)
(define (%state-initialize state::FabricClientState) #!void)
(define (%state-enabled? state::FabricClientState) #t)
(define (%state-initialized? state::FabricClientState) #t)
(define (%state-attached state::FabricClientState manager::AppStateManager) #!void)
(define (%state-detached state::FabricClientState manager::AppStateManager) #!void)

