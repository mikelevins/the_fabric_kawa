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
 PickCharacterState
 make-pick-character-state)

;;; ---------------------------------------------------------------------
;;; required modules
;;; ---------------------------------------------------------------------

(require "util-error.scm")
(require "util-java.scm")
(require "util-lists.scm")
(require "data-nodes.scm")
(require "client-state.scm")

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
;;; the client character-creator AppState class
;;; ---------------------------------------------------------------------

;;; CLASS PickCharacterState
;;; ---------------------------------------------------------------------

(define-simple-class PickCharacterState (FabricClientState)
  ;; slots
  (initialized? init: #f)
  ((getInitialized) initialized?)
  ((setInitialized newstate) (set! initialized? newstate))
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
  (format #t "~%%pick-character-state-detached called"))

(define (make-pick-character-state client::Application)
  (let ((state (PickCharacterState)))
    (*:setClient state client)
    state))

;;; ---------------------------------------------------------------------
;;; PickCharacterState functions
;;; ---------------------------------------------------------------------

(define (prepare-to-attach-pick-character-state state::PickCharacterState client)
  (unless (*:getInitialized state)
          (let* ((screen::Screen (*:getScreen client))
                 (gui-node::Node (*:getGuiNode client))
                 (Align BitmapFont:Align)
                 )
            (*:setInitialized state #t))))

(define (did-attach-pick-character-state state::PickCharacterState mgr::AppStateManager)
  (when (*:getInitialized state)
        (let* ((client (*:getClient state))
               (screen::Screen (*:getScreen client))
               (gui-node::Node (*:getGuiNode client)))
          (*:enqueue client
                     (runnable (lambda ()
                                 (let ((client (*:getClient state))
                                       (root::Node (*:getRootNode client)))
                                   (*:addControl gui-node screen))))))))
