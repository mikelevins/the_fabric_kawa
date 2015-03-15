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
 compute-character-picker-rect
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
(require "model-rect.scm")
(require "view-skybox.scm")

;;; ---------------------------------------------------------------------
;;; Java imports
;;; ---------------------------------------------------------------------

(import-as AppStateManager com.jme3.app.state.AppStateManager)
(import-as Node com.jme3.scene.Node)
(import-as Screen tonegod.gui.core.Screen)
(import-as Spatial com.jme3.scene.Spatial)
(import-as Vector2f com.jme3.math.Vector2f)


;;; ---------------------------------------------------------------------
;;; PickCharacterGameState functions
;;; ---------------------------------------------------------------------

(define (compute-character-picker-rect screen::Screen)
  (let* ((screen-height (*:getHeight screen))
         (picker-left 16)
         (picker-top 16)
         (picker-width 512)
         (picker-height (- screen-height (* 2 16))))
    (make-rectangle picker-left
                    picker-top
                    picker-width
                    picker-height)))

(define (prepare-to-attach-pick-character-gamestate state::PickCharacterGameState client::FabricClient)
  (unless (*:getInitialized state)
    (let* ((client::FabricClient (*:getApp state))
           (screen::Screen (*:getScreen client))
           (sky::Spatial (make-sky-box))
           (gui-node::Node (*:getGuiNode client)))
      (*:setCharacterPicker state (make-character-picker screen))
      (*:setSky state sky)
      (*:setInitialized state #t))))

(define (did-attach-pick-character-gamestate state::PickCharacterGameState mgr::AppStateManager)
  (when (*:getInitialized state)
    (let* ((client::FabricClient (*:getApp state))
           (screen::Screen (*:getScreen client))
           (gui-node::Node (*:getGuiNode client)))
      (*:enqueue client
                 (runnable (lambda ()
                             (let ((client::FabricClient (*:getApp state))
                                   (root::Node (*:getRootNode client)))
                               (*:attachChild root (*:getSky state))
                               (*:addElement screen (*:getCharacterPicker state))
                               (*:addControl gui-node screen))))))))

(define (did-detach-pick-character-gamestate state::PickCharacterGameState mgr::AppStateManager)
  (when (*:getInitialized state)
    (let* ((client::FabricClient (*:getApp state))
           (screen::Screen (*:getScreen client))
           (gui-node::Node (*:getGuiNode client)))
      (*:enqueue client
                 (runnable (lambda ()
                             (let ((client::FabricClient (*:getApp state))
                                   (root::Node (*:getRootNode client))
                                   (sky::Spatial (*:getSky state)))
                               (*:detachChild root sky)
                               (*:setSky state #!null)
                               (*:removeElement screen (*:getCharacterPicker state))
                               (*:removeControl gui-node screen)
                               (*:setInitialized state #f))))))))
