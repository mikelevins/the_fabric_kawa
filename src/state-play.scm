;;;; ***********************************************************************
;;;;
;;;; Name:          state-play.scm
;;;; Project:       The Fabric: a far-future MMORPG
;;;; Purpose:       the play AppState
;;;; Author:        mikel evins
;;;; Copyright:     2015 by mikel evins
;;;;
;;;; ***********************************************************************

(module-export
 PlayState)

;;; ---------------------------------------------------------------------
;;; required modules
;;; ---------------------------------------------------------------------

(require client)
(require data-assets)
(require model-namegen)
(require state)
(require view-action-bar)
(require view-camera-movement)
(require view-character-model)
(require view-character-nameplate)
(require view-location)
(require view-location-nameplate)
(require view-skybox)

;;; ---------------------------------------------------------------------
;;; Java imports
;;; ---------------------------------------------------------------------

(import (class com.jme3.app.state AbstractAppState AppStateManager))
(import (class com.jme3.math Vector3f))
(import (class com.jme3.renderer Camera))
(import (class com.jme3.scene CameraNode Geometry Node Spatial))
(import (class com.jme3.scene.control CameraControl))
(import (class java.lang String))
(import (class tonegod.gui.controls.text Label))
(import (class tonegod.gui.controls.windows Panel))
(import (class tonegod.gui.core Screen))

(define ControlDirection CameraControl:ControlDirection)

;;; ---------------------------------------------------------------------
;;; PlayState
;;; ---------------------------------------------------------------------

(define (%play-state-cleanup state::PlayState)
  (let* ((client::FabricClient (the-client))
         (screen::Screen client:screen)
         (gui-node::Node (*:getGuiNode client))
         (root::Node (*:getRootNode client))
         (camera::Camera (*:getCamera client)))
    (*:setLocation camera (Vector3f 0.0 0.0 0.0))
    (*:lookAtDirection camera (Vector3f 0.0 0.0 1.0) (Vector3f 0.0 1.0 0.0))
    (*:detachChild root state:sky)
    (*:detachChild root state:location)
    (*:detachChild root state:location-nameplate)
    (*:detachChild root state:character-model)
    (*:removeElement screen state:action-bar)
    (*:removeControl gui-node screen)))

(define (%play-state-initialize state::PlayState)
  (let* ((client::FabricClient (the-client))
         (user::FabricUser client:current-user)
         (character::FabricCharacter (current-character))
         (character-name (if (eqv? #!null character)
                             ""
                             (fabric-name->string character:name)))
         (character-nameplate::Label (make-character-nameplate client:screen))
         (model::Node (make-character-model character))
         (gui-node::Node (*:getGuiNode client))
         (screen::Screen client:screen)
         (location-nameplate::Label (make-location-nameplate client:screen))
         (location::Spatial (make-location (current-location)))
         (action-bar::Panel (make-action-bar state client:screen))
         (sky (make-sky-box))
         (root::Node (*:getRootNode client))
         (camera::Camera (*:getCamera client))
         (camera-node::CameraNode (CameraNode "CameraNode" camera)))
    (*:setControlDir camera-node ControlDirection:SpatialToCamera)
    (*:attachChild model camera-node)
    (*:setLocalTranslation camera-node (Vector3f 0 0 40))
    (*:lookAt camera-node (*:getLocalTranslation model) Vector3f:UNIT_Y)
    (*:setFrustumFar camera 100000)
    (*:setLocalTranslation model (Vector3f 0.0 1000.0 40000.0))
    (set! state:location location)
    (set! state:location-nameplate location-nameplate)
    (set! state:action-bar action-bar)
    (set! state:sky sky)
    (set! state:character-model model)
    (set! state:character-nameplate character-nameplate)
    (*:attachChild root state:sky)
    (*:attachChild root state:location)
    (*:attachChild root state:location-nameplate)
    (*:attachChild root state:character-model)
    (*:attachChild root state:character-nameplate)
    (*:addElement screen action-bar)
    (*:addControl gui-node screen)
    (*:setText location-nameplate (current-location))
    (*:setText character-nameplate character-name)
    (set! state:state-initialized? #t)))

(define (%play-state-enabled? state::FabricClientState) #t)
(define (%play-state-initialized? state::FabricClientState) state:state-initialized?)
(define (%play-state-attached state::FabricClientState manager::AppStateManager) #!void)
(define (%play-state-detached state::FabricClientState manager::AppStateManager) #!void)

(define (%play-state-handle-analog-event state::PlayState name value tpf)
  (let* ((client::FabricClient (the-client))
         (speed state:speed)
         (pchar::FabricCharacter (current-character))
         (node state:character-model)
         (right-button-down? client:right-button?))
    (cond
     ((*:equals name "KeyW") (move-node-forward! client node (* speed tpf)))
     ((*:equals name "MouseButtonLeft") (when right-button-down?
                                          (move-node-forward! client node (* speed tpf))))
     ((*:equals name "KeyS")(move-node-backward! client node (* 0.6 speed tpf)))
     ((*:equals name "KeyD")(move-node-right! client node (* speed tpf)))
     ((*:equals name "KeyA")(move-node-left! client node (* speed tpf)))
     ((*:equals name "KeyQ")(roll-node-left! node (* 0.5 tpf)))
     ((*:equals name "KeyE")(roll-node-right! node (* 0.5 tpf)))
     ((*:equals name "KeySPACE")(move-node-up! client node (* speed tpf)))
     ((*:equals name "KeyX")(move-node-down! client node (* speed tpf)))
     ((*:equals name "MouseDragRight")(when right-button-down? (rotate-node-right! node (* 6.0 tpf))))
     ((*:equals name "MouseDragLeft")(when right-button-down? (rotate-node-left! node (* 6.0 tpf))))
     ((*:equals name "MouseDragUp")(when right-button-down? (rotate-node-down! node (* 3.0 tpf))))
     ((*:equals name "MouseDragDown")(when right-button-down? (rotate-node-up! node (* 3.0 tpf))))
     (else #f))))

(define (%play-state-handle-action-event state name key-pressed? tpf)
  (let ((client::FabricClient (the-client)))
    (cond
     ((*:equals name "MouseButtonLeft")(set! client:left-button? key-pressed?))
     ((*:equals name "MouseButtonRight")(set! client:right-button? key-pressed?))
     (else #f))))

(define-simple-class PlayState (FabricClientState)
  ;; slots
  (character-model::Node init: #!null)
  (character-nameplate::Label init: #!null)
  (location::Spatial init: #!null)
  (location-nameplate::Label init: #!null)
  (sky::Geometry init: #!null)
  (action-bar::Panel init: #!null)
  (speed type: float init-form: 1000.0)
  ;; methods
  ((cleanup) (%play-state-cleanup (this)))
  ((initialize state-manager::AppStateManager app::FabricClient)
   (begin (invoke-special FabricClientState (this) 'initialize state-manager app)
          (set! app:state (this))
          (%play-state-initialize (this))))
  ((isEnabled) (%play-state-enabled? (this)))
  ((isInitialized) (%play-state-initialized? (this)))
  ((stateAttached state-manager::AppStateManager)
   (%play-state-attached (this) state-manager))
  ((stateDetached state-manager::AppStateManager)
   (%play-state-detached (this) state-manager))
  ((handleAnalogEvent name value tpf)
   (%play-state-handle-analog-event (this) name value tpf))
  ((handleActionEvent name key-pressed? tpf)
   (%play-state-handle-action-event (this) name key-pressed? tpf)))

