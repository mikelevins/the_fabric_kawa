;;;; ***********************************************************************
;;;;
;;;; Name:          state-play.scm
;;;; Project:       The Fabric: a far-future MMORPG
;;;; Purpose:       fabric AppStates
;;;; Author:        mikel evins
;;;; Copyright:     2015 by mikel evins
;;;;
;;;; ***********************************************************************
(format #t "~%loading state-play.scm")

(module-export
 PlayState
 make-play-state)

;;; ---------------------------------------------------------------------
;;; required modules
;;; ---------------------------------------------------------------------

(require "util-error.scm")
(require "util-java.scm")
(require "util-lists.scm")
(require "data-nodes.scm")
(require "view-skybox.scm")
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
;;; the client play AppState class
;;; ---------------------------------------------------------------------

;;; CLASS PlayState
;;; ---------------------------------------------------------------------

(define-simple-class PlayState (FabricClientState)
  ;; slots
  (node-name init: #f)
  ((getNodeName) node-name)
  ((setNodeName new-name) (set! node-name new-name))
  (sky init: #f)
  ((getSky) sky)
  ((setSky new-sky) (set! sky new-sky))
  (initialized? init: #f)
  ((getInitialized) initialized?)
  ((setInitialized newstate) (set! initialized? newstate))
  ;; methods
  ((cleanup) (%play-state-cleanup (this)))
  ((isEnabled) (%play-state-enabled? (this)))
  ((stateAttached state-manager::AppStateManager)
   (%play-state-attached (this) state-manager))
  ((stateDetached state-manager::AppStateManager)
   (%play-state-detached (this) state-manager))
  ;; init
  ((initialize) (%play-state-initialize (this)))
  ((isInitialized) (%play-state-initialized? (this))))

(define (%play-state-cleanup state::PlayState)
  (format #t "~%%play-state-cleanup called"))

(define (%play-state-initialize state::PlayState)
  (format #t "~%%play-state-initialize called"))

(define (%play-state-enabled? state::PlayState) #t)

(define (%play-state-initialized? state::PlayState) #t)

(define (%play-state-attached state::PlayState manager::AppStateManager)
  (let ((client::Application (*:getClient state)))
    (prepare-to-attach-play-state state client)
    (did-attach-play-state state manager)))

(define (%play-state-detached state::PlayState manager::AppStateManager)
  (did-detach-play-state state manager))

(define (make-play-state client::Application node-name)
  (let ((state (PlayState)))
    (*:setClient state client)
    (*:setNodeName state node-name)
    state))

;;; ---------------------------------------------------------------------
;;; PlayState functions
;;; ---------------------------------------------------------------------

(define (prepare-to-attach-play-state state::PlayState client)
  (unless (*:getInitialized state)
          (let* ((screen::Screen (*:getScreen client))
                 (gui-node::Node (*:getGuiNode client))
                 (Align BitmapFont:Align)
                 (sky::Spatial (make-sky-box)))
            (*:setSky state sky)
            (*:setInitialized state #t))))

(define (did-attach-play-state state::PlayState mgr::AppStateManager)
  (when (*:getInitialized state)
        (let* ((client (*:getClient state))
               (screen::Screen (*:getScreen client))
               (gui-node::Node (*:getGuiNode client)))
          (*:enqueue client
                     (runnable (lambda ()
                                 (let ((client (*:getClient state))
                                       (root::Node (*:getRootNode client)))
                                   (*:attachChild root (*:getSky state))
                                   (*:addControl gui-node screen))))))))

(define (did-detach-play-state state::PlayState mgr::AppStateManager)
  (when (*:getInitialized state)
    (let* ((client (*:getClient state))
           (screen::Screen (*:getScreen client))
           (gui-node::Node (*:getGuiNode client)))
      (*:enqueue client
                 (runnable (lambda ()
                             (let ((client (*:getClient state))
                                   (root::Node (*:getRootNode client))
                                   (sky::Spatial (*:getSky state)))
                               (*:detachChild root sky)
                               (*:removeControl gui-node screen))))))))
