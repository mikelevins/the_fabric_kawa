;;;; ***********************************************************************
;;;;
;;;; Name:          state-pick-character.scm
;;;; Project:       The Fabric: a far-future MMORPG
;;;; Purpose:       fabric AppStates
;;;; Author:        mikel evins
;;;; Copyright:     2015 by mikel evins
;;;;
;;;; ***********************************************************************
(format #t "~%loading state-pick-character.scm")

(module-export
 did-attach-pick-character-state
 did-detach-pick-character-state
 PickCharacterState
 make-pick-character-state
 prepare-to-attach-pick-character-state)

;;; ---------------------------------------------------------------------
;;; required modules
;;; ---------------------------------------------------------------------

(require util-error)
(require util-lists)
(require data-nodes)
(require client-state)
(require client-class)

;;; ---------------------------------------------------------------------
;;; Java imports
;;; ---------------------------------------------------------------------

(import (class com.jme3.app Application))
(import (class com.jme3.app.state AppStateManager))
(import (class com.jme3.font BitmapFont))
(import (class com.jme3.math Vector2f))
(import (class com.jme3.scene Node Spatial))
(import (class tonegod.gui.controls.text Label))
(import (class tonegod.gui.controls.windows Window))
(import (class tonegod.gui.core Screen))

;;; ---------------------------------------------------------------------
;;; the client character-creator AppState class
;;; ---------------------------------------------------------------------

;;; CLASS PickCharacterState
;;; ---------------------------------------------------------------------

(define-simple-class PickCharacterState (FabricClientState)
  ;; slots
  (initialized? init: #f)
  ;; methods
  ((cleanup) (%pick-character-state-cleanup (this)))
  ((isEnabled) (%pick-character-state-enabled? (this)))
  ((stateAttached state-manager::AppStateManager)
   (%pick-character-state-attached (this) state-manager))
  ((stateDetached state-manager::AppStateManager)
   (%pick-character-state-detached (this) state-manager))
  ;; init
  ((initialize) (%pick-character-state-initialize (this)))
  ((isInitialized) (%pick-character-state-initialized? (this))))

(define (%pick-character-state-cleanup state::PickCharacterState)
  (format #t "~%%pick-character-state-cleanup called"))

(define (%pick-character-state-initialize state::PickCharacterState)
  (format #t "~%%pick-character-state-initialize called"))

(define (%pick-character-state-enabled? state::PickCharacterState) #t)

(define (%pick-character-state-initialized? state::PickCharacterState) #t)

(define (%pick-character-state-attached state::PickCharacterState manager::AppStateManager)
  (format #t "~%%pick-character-state-attached called"))

(define (%pick-character-state-detached state::PickCharacterState manager::AppStateManager)
  (did-detach-pick-character-state state manager))

(define (make-pick-character-state client::Application)
  (let ((state (PickCharacterState)))
    (set! state:client client)
    state))

;;; ---------------------------------------------------------------------
;;; PickCharacterState functions
;;; ---------------------------------------------------------------------

(define (prepare-to-attach-pick-character-state state::PickCharacterState client::FabricClient)
  (unless state:initialized?
    (let* ((screen::Screen client:screen)
           (gui-node::Node client:guiNode)
           (Align BitmapFont:Align))
      (set! state:initialized? #t))))

(define (did-attach-pick-character-state state::PickCharacterState mgr::AppStateManager)
  (when state:initialized?
    (let* ((client::FabricClient state:client)
           (screen::Screen client:screen)
           (gui-node::Node client:guiNode))
      (*:enqueue client
                 (runnable (lambda ()
                             (let ((root::Node client:rootNode))
                               (*:addControl gui-node screen))))))))

(define (did-detach-pick-character-state state::PickCharacterState mgr::AppStateManager)
  (when state:initialized?
    (let* ((client::FabricClient state:client)
           (screen::Screen client:screen)
           (gui-node::Node client:guiNode))
      (*:enqueue client
                 (runnable (lambda ()
                             (let ((root::Node client:rootNode))
                               (*:removeControl gui-node screen))))))))
