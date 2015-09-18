;;;; ***********************************************************************
;;;;
;;;; Name:          state-transit.scm
;;;; Project:       The Fabric: a far-future MMORPG
;;;; Purpose:       fabric AppStates
;;;; Author:        mikel evins
;;;; Copyright:     2015 by mikel evins
;;;;
;;;; ***********************************************************************
(format #t "~%loading state-transit.scm")

(module-export
 TransitState
 did-attach-transit-state
 did-detach-transit-state
 make-transit-state
 prepare-to-attach-transit-state)

;;; ---------------------------------------------------------------------
;;; required modules
;;; ---------------------------------------------------------------------

(require util-error)
(require util-java)
(require util-lists)
(require data-nodes)
(require client-state)
(require client-class)

;;; ---------------------------------------------------------------------
;;; Java imports
;;; ---------------------------------------------------------------------

(import-as Application com.jme3.app.Application)
(import-as AppStateManager com.jme3.app.state.AppStateManager)
(import-as BitmapFont com.jme3.font.BitmapFont)
(import-as Label tonegod.gui.controls.text.Label)
(import-as Node com.jme3.scene.Node)
(import-as Screen tonegod.gui.core.Screen)
(import-as Spatial com.jme3.scene.Spatial)
(import-as Vector2f com.jme3.math.Vector2f)
(import-as Window tonegod.gui.controls.windows.Window)

;;; ---------------------------------------------------------------------
;;; the client transit AppState class
;;; ---------------------------------------------------------------------

;;; CLASS TransitState
;;; ---------------------------------------------------------------------

(define-simple-class TransitState (FabricClientState)
  ;; slots
  (initialized? init: #f)
  ;; methods
  ((cleanup) (%transit-state-cleanup (this)))
  ((isEnabled) (%transit-state-enabled? (this)))
  ((stateAttached state-manager::AppStateManager)
   (%transit-state-attached (this) state-manager))
  ((stateDetached state-manager::AppStateManager)
   (%transit-state-detached (this) state-manager))
  ;; init
  ((initialize) (%transit-state-initialize (this)))
  ((isInitialized) (%transit-state-initialized? (this))))

(define (%transit-state-cleanup state::TransitState)
  (format #t "~%%transit-state-cleanup called"))

(define (%transit-state-initialize state::TransitState)
  #f)

(define (%transit-state-enabled? state::TransitState) #t)

(define (%transit-state-initialized? state::TransitState) #t)

(define (%transit-state-attached state::TransitState manager::AppStateManager)
  #f)

(define (%transit-state-detached state::TransitState manager::AppStateManager)
  (did-detach-transit-state state manager))

(define (make-transit-state client::Application)
  (let ((state (TransitState)))
    (set! state:client client)
    state))

;;; ---------------------------------------------------------------------
;;; TransitState functions
;;; ---------------------------------------------------------------------

(define (prepare-to-attach-transit-state state::TransitState client::FabricClient)
  (unless state:initialized?
    (let* ((screen::Screen client:screen)
           (gui-node::Node client:guiNode)
           (Align BitmapFont:Align))
      (set! state:initialized? #t))))

(define (did-attach-transit-state state::TransitState mgr::AppStateManager)
  (when state:initialized?
    (let* ((client::FabricClient state:client)
           (screen::Screen client:screen)
           (gui-node::Node client:guiNode))
      (*:enqueue client
                 (runnable (lambda ()
                             (let ((root::Node client:rootNode))
                               (*:addControl gui-node screen))))))))

(define (did-detach-transit-state state::TransitState mgr::AppStateManager)
  (when state:initialized?
    (let* ((client::FabricClient state:client)
           (screen::Screen client:screen)
           (gui-node::Node client:guiNode))
      (*:enqueue client
                 (runnable (lambda ()
                             (let ((root::Node client:rootNode))
                               (*:removeControl gui-node screen))))))))
