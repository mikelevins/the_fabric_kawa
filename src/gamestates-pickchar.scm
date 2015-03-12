;;;; ***********************************************************************
;;;;
;;;; Name:          gamestates-pickchar.scm
;;;; Project:       The Fabric: a far-future MMORPG
;;;; Purpose:       supporting functions for PickCharacterGameState
;;;; Author:        mikel evins
;;;; Copyright:     2015 by mikel evins
;;;;
;;;; ***********************************************************************

(module-export
 did-attach-pick-character-gamestate
 did-detach-pick-character-gamestate
 prepare-to-attach-pick-character-gamestate)

;;; ---------------------------------------------------------------------
;;; required modules
;;; ---------------------------------------------------------------------

(require "util-java.scm")
(require "util-error.scm")
(require "util-lists.scm")
(require "syntax-classes.scm")
(require "view-loginbox.scm")
(require "gamestates.scm")
(require "client-main.scm")
(require "view-pickcharacter.scm")

;;; ---------------------------------------------------------------------
;;; Java imports
;;; ---------------------------------------------------------------------

(import-as AppStateManager com.jme3.app.state.AppStateManager)
(import-as Node com.jme3.scene.Node)
(import-as Screen tonegod.gui.core.Screen)
(import-as Vector2f com.jme3.math.Vector2f)


;;; ---------------------------------------------------------------------
;;; PickCharacterGameState functions
;;; ---------------------------------------------------------------------

(define (prepare-to-attach-pick-character-gamestate state::PickCharacterGameState client::FabricClient)
  (unless (*:getInitialized state)
    (let* ((client::FabricClient (*:getApp state))
           (screen::Screen (*:getScreen client))
           (gui-node::Node (*:getGuiNode client)))
      (*:setCharacterPicker state (make-character-picker screen))
      (*:setInitialized state #t))))

(define (did-attach-pick-character-gamestate state::PickCharacterGameState mgr::AppStateManager)
  (when (*:getInitialized state)
    (let* ((client::FabricClient (*:getApp state))
           (screen::Screen (*:getScreen client))
           (gui-node::Node (*:getGuiNode client)))
      (*:enqueue client
                 (runnable (lambda ()
                             (*:addElement screen (*:getCharacterPicker state))
                             (*:addControl gui-node screen)))))))

(define (did-detach-pick-character-gamestate state::PickCharacterGameState mgr::AppStateManager)
  (when (*:getInitialized state)
    (let* ((client::FabricClient (*:getApp state))
           (screen::Screen (*:getScreen client))
           (gui-node::Node (*:getGuiNode client)))
      (*:enqueue client
                 (runnable (lambda ()
                             (*:removeElement screen (*:getCharacterPicker state))
                             (*:removeControl gui-node screen)))))))
