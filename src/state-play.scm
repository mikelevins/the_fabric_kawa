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

(require data-assets)
(require view-skybox)
(require state)
(require client)
(require model-namegen)
(require view-location)
(require view-location-nameplate)
(require view-action-bar)
(require view-character-model)
(require view-character-nameplate)

;;; ---------------------------------------------------------------------
;;; Java imports
;;; ---------------------------------------------------------------------

(import (class com.jme3.app.state AbstractAppState AppStateManager))
(import (class com.jme3.math Vector3f))
(import (class com.jme3.renderer Camera))
(import (class com.jme3.scene Geometry Node Spatial))
(import (class java.lang String))
(import (class tonegod.gui.controls.text Label))
(import (class tonegod.gui.controls.windows Panel))
(import (class tonegod.gui.core Screen))

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
         (camera::Camera (*:getCamera client)))
    (*:setLocation camera (Vector3f 0.0 2000.0 25000.0))
    (*:lookAtDirection camera (Vector3f 0.0 0.0 -1.0) (Vector3f 0.0 1.0 0.0))
    (*:setFrustumFar camera 100000)
    (*:setLocalTranslation model (Vector3f 0.0 1995.0 24970.0))
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

(define (%play-state-handle-analog-event state name value tpf)
  ;;; TODO: implement event handling
  #!void)

(define (%play-state-handle-action-event state name key-pressed? tpf)
  ;;; TODO: implement event handling
  #!void)

(define-simple-class PlayState (FabricClientState)
  ;; slots
  (character-model::Node init: #!null)
  (character-nameplate::Label init: #!null)
  (location::Spatial init: #!null)
  (location-nameplate::Label init: #!null)
  (sky::Geometry init: #!null)
  (action-bar::Panel init: #!null)
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

