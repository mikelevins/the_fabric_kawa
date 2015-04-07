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
   (client init-form: #!null getter: getClient setter: setClient)) 
  (methods:))

