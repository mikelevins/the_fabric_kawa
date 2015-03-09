;;;; ***********************************************************************
;;;;
;;;; Name:          gamestates.scm
;;;; Project:       The Fabric: a far-future MMORPG
;;;; Purpose:       Fabric game states
;;;; Author:        mikel evins
;;;; Copyright:     2015 by mikel evins
;;;;
;;;; ***********************************************************************

(module-export
 CreateCharacterGameState
 FabricGameState
 LoginGameState
 PickCharacterGameState
 PlayGameState)

;;; ---------------------------------------------------------------------
;;; ABOUT
;;; ---------------------------------------------------------------------

;;; a FabricGameState is an AppState that represents one of the Fabric
;;; client's major modes: login, create a character, pick a character,
;;; or play. FabricGameState defines a small set of common slots and methods
;;; that all Fabric GameStates must implement

;;; ---------------------------------------------------------------------
;;; required modules
;;; ---------------------------------------------------------------------

(require "util-java.scm")
(require "util-error.scm")
(require "util-lists.scm")
(require "syntax-classes.scm")
(require "view-loginbox.scm")
(require "data-nodes.scm")
(require "client-main.scm")
(require "view-pickcharacter.scm")
(require "view-rotatecontrol.scm")

;;; ---------------------------------------------------------------------
;;; Java imports
;;; ---------------------------------------------------------------------

(import-as AssetManager com.jme3.asset.AssetManager)
(import-as AbstractAppState com.jme3.app.state.AbstractAppState)
(import-as AppStateManager com.jme3.app.state.AppStateManager)
(import-as BitmapFont com.jme3.font.BitmapFont)
(import-as Camera com.jme3.renderer.Camera)
(import-as CameraNode com.jme3.scene.CameraNode)
(import-as ColorRGBA com.jme3.math.ColorRGBA)
(import-as Control com.jme3.scene.control.Control)
(import-as Geometry com.jme3.scene.Geometry)
(import-as Material com.jme3.material.Material)
(import-as Node com.jme3.scene.Node)
(import-as PI com.jme3.math.FastMath:PI)
(import-as Quaternion com.jme3.math.Quaternion)
(import-as Screen tonegod.gui.core.Screen)
(import-as SkyFactory com.jme3.util.SkyFactory)
(import-as Spatial com.jme3.scene.Spatial)
(import-as Sphere com.jme3.scene.shape.Sphere)
(import-as Label tonegod.gui.controls.text.Label)
(import-as Vector2f com.jme3.math.Vector2f)
(import-as Vector3f com.jme3.math.Vector3f)


;;; =====================================================================
;;; CLASS FabricGameState
;;; =====================================================================

(defclass FabricGameState (AbstractAppState)
  (slots:
   (app init-form: #!null getter: getApp setter: setApp)
   (initialized init-form: #F getter: getInitialized setter: setInitialized))
  (methods:
   ((cleanup)
    (error "cleanup must be implemented in subclasses of FabricGameState"))
   ((isEnabled)
    (error "isEnabled must be implemented in subclasses of FabricGameState"))
   ((isInitialized) initialized)
   ((prepareToAttach mgr::AppStateManager client::FabricClient)
    (error "prepareToAttach must be implemented in subclasses of FabricGameState"))
   ((stateAttached mgr::AppStateManager)
    (error "stateAttached must be implemented in subclasses of FabricGameState"))
   ((stateDetached mgr::AppStateManager)
    (error "stateDetached must be implemented in subclasses of FabricGameState"))
   ((cleanupDetached mgr::AppStateManager client::FabricClient)
    (error "cleanupDetached must be implemented in subclasses of FabricGameState"))))


;;; =====================================================================
;;; CLASS CreateCharacterGameState
;;; =====================================================================

(define (%prepare-to-attach-create-character-gamestate state::CreateCharacterGameState client::FabricClient)
  (unless (*:getInitialized state)
    (let* ((screen::Screen (*:getScreen client))
           (gui-node::Node (*:getGuiNode client))
           (Align BitmapFont:Align)
           (label::Label (Label screen "FactionNameplate" (Vector2f 600 40)(Vector2f 1200 40))))
      (*:setFactionNameplate state label)
      (*:setText label "Faction: ")
      (*:setTextAlign label Align:Left)
      (*:setFont label "Interface/Fonts/Laconic30.fnt")
      (*:setFontSize label 30)
      (*:setFontColor label ColorRGBA:Green)
      (*:setInitialized state #t))))

(define (%did-attach-create-character-gamestate state::CreateCharacterGameState mgr::AppStateManager)
  (when (*:getInitialized state)
    (let* ((client::FabricClient (*:getApp state))
           (screen::Screen (*:getScreen client))
           (gui-node::Node (*:getGuiNode client)))
      (*:enqueue client
                 (runnable (lambda ()
                             (*:addElement screen (*:getFactionNameplate state))
                             (*:addControl gui-node screen)))))))

(define (%did-detach-create-character-gamestate state::CreateCharacterGameState mgr::AppStateManager)
  (when (*:getInitialized state)
    (let* ((client::FabricClient (*:getApp state))
           (screen::Screen (*:getScreen client))
           (gui-node::Node (*:getGuiNode client)))
      (*:enqueue client
                 (runnable (lambda ()
                             (*:removeElement screen (*:getFactionNameplate state))
                             (*:removeControl gui-node screen)))))))

;;; CLASS CreateCharacterGameState
;;; ---------------------------------------------------------------------
;;; a GameState that offers a UI for constructing a new Fabric character

(defclass CreateCharacterGameState (FabricGameState)
  (slots:
   (faction-nameplate init-form: #!null getter: getFactionNameplate setter: setFactionNameplate))
  (methods:
   ((cleanup) #!void)
   ((isEnabled) #t)
   ((isInitialized) initialized)
   ;; prepare to attach the state
   ((prepareToAttach mgr::AppStateManager client::FabricClient)
    (%prepare-to-attach-create-character-gamestate (this) client))
   ;; the state has been attached
   ((stateAttached mgr::AppStateManager)
    (%did-attach-create-character-gamestate (this) mgr))
   ;; the state has been detached
   ((stateDetached mgr::AppStateManager)
    (%did-detach-create-character-gamestate (this) mgr))
   ;; cleanup after the state is detached
   ((cleanupDetached mgr::AppStateManager client::FabricClient) #!void)))


;;; =====================================================================
;;; CLASS LoginGameState
;;; =====================================================================

(define (%prepare-to-attach-login-gamestate state::LoginGameState client::FabricClient)
  (unless (*:getInitialized state)
    (let* ((client::FabricClient (*:getApp state))
           (screen::Screen (*:getScreen client))
           (gui-node::Node (*:getGuiNode client))
           (box::FabricLoginBox (FabricLoginBox screen "LoginBox" (Vector2f 700 300)(Vector2f 700 300))))
      (*:setWindowTitle box "Log in to the Fabric")
      (*:setLoginBox state box)
      (*:setInitialized state #t))))

(define (%did-attach-login-gamestate state::LoginGameState mgr::AppStateManager)
  (when (*:getInitialized state)
    (let* ((client::FabricClient (*:getApp state))
           (screen::Screen (*:getScreen client))
           (gui-node::Node (*:getGuiNode client)))
      (*:enqueue client
                 (runnable (lambda ()
                             (*:addElement screen (*:getLoginBox state))
                             (*:addControl gui-node screen)))))))

(define (%did-detach-login-gamestate state::LoginGameState mgr::AppStateManager)
  (when (*:getInitialized state)
    (let* ((client::FabricClient (*:getApp state))
           (screen::Screen (*:getScreen client))
           (gui-node::Node (*:getGuiNode client)))
      (*:enqueue client
                 (runnable (lambda ()
                             (*:removeElement screen (*:getLoginBox state))
                             (*:removeControl gui-node screen)))))))


;;; CLASS LoginGameState
;;; ---------------------------------------------------------------------
;;; a GameState that offers a UI for logging in to the game

(defclass LoginGameState (FabricGameState)
  (slots:
   (loginbox init-form: #!null getter: getLoginBox setter: setLoginBox))
  (methods:
   ((cleanup) #!void)
   ((isEnabled) #t)
   ((isInitialized) initialized)
   ;; prepare to attach the state
   ((prepareToAttach mgr::AppStateManager client::FabricClient)
    (%prepare-to-attach-login-gamestate (this) client))
   ;; the state has been attached
   ((stateAttached mgr::AppStateManager)
    (%did-attach-login-gamestate (this) mgr))
   ;; the state has been detached
   ((stateDetached mgr::AppStateManager)
    (%did-detach-login-gamestate (this) mgr))
   ((cleanupDetached mgr::AppStateManager client::FabricClient) #!void)))


;;; =====================================================================
;;; CLASS PickCharacterGameState
;;; =====================================================================
;;; TODO: make a node picker so the player can choose which accessible node
;;; to enter the game at

(define (%prepare-to-attach-pick-character-gamestate state::PickCharacterGameState client::FabricClient)
  (unless (*:getInitialized state)
    (let* ((client::FabricClient (*:getApp state))
           (screen::Screen (*:getScreen client))
           (gui-node::Node (*:getGuiNode client)))
      (*:setCharacterPicker state (make-character-picker screen))
      (*:setInitialized state #t))))

(define (%did-attach-pick-character-gamestate state::PickCharacterGameState mgr::AppStateManager)
  (when (*:getInitialized state)
    (let* ((client::FabricClient (*:getApp state))
           (screen::Screen (*:getScreen client))
           (gui-node::Node (*:getGuiNode client)))
      (*:enqueue client
                 (runnable (lambda ()
                             (*:addElement screen (*:getCharacterPicker state))
                             (*:addControl gui-node screen)))))))

(define (%did-detach-pick-character-gamestate state::PickCharacterGameState mgr::AppStateManager)
  (when (*:getInitialized state)
    (let* ((client::FabricClient (*:getApp state))
           (screen::Screen (*:getScreen client))
           (gui-node::Node (*:getGuiNode client)))
      (*:enqueue client
                 (runnable (lambda ()
                             (*:removeElement screen (*:getCharacterPicker state))
                             (*:removeControl gui-node screen)))))))


;;; CLASS PickCharacterGameState
;;; ---------------------------------------------------------------------
;;; a GameState that offers a UI for choosing a character to play

(defclass PickCharacterGameState (FabricGameState)
  (slots:
   (picker init-form: #!null getter: getCharacterPicker setter: setCharacterPicker))
  (methods:
   ((cleanup) #!void)
   ((isEnabled) #t)
   ((isInitialized) initialized)
   ;; prepare to attach the state
   ((prepareToAttach mgr::AppStateManager client::FabricClient)
    (%prepare-to-attach-pick-character-gamestate (this) client))
   ;; the state has been attached
   ((stateAttached mgr::AppStateManager)
    (%did-attach-pick-character-gamestate (this) mgr))
   ;; the state has been detached
   ((stateDetached mgr::AppStateManager)
    (%did-detach-pick-character-gamestate (this) mgr))
   ((cleanupDetached mgr::AppStateManager client::FabricClient)
    (format #t "~%Cleaning up after detaching LoginGameState..."))))


;;; =====================================================================
;;; CLASS PlayGameState
;;; =====================================================================
;;; for the moment, until I make a real solution for picking nodes
;;; from the character picker, we'll just choose a random Fabric node
;;; when the state is attached and display the texture for that
;;;
;;; TODO: destroy the central celestial object when detaching
;;; so that we can reattach with a different one

(define (%make-sky)
  (let ((asset-manager::AssetManager (get-asset-manager)))
    (SkyFactory:createSky
     asset-manager
     (*:loadTexture asset-manager "Textures/TychoSkymapII.t3_08192x04096_80_mx.jpg")
     (*:loadTexture asset-manager "Textures/TychoSkymapII.t3_08192x04096_80_px.jpg")
     (*:loadTexture asset-manager "Textures/TychoSkymapII.t3_08192x04096_80_mz.jpg")
     (*:loadTexture asset-manager "Textures/TychoSkymapII.t3_08192x04096_80_pz.jpg")
     (*:loadTexture asset-manager "Textures/TychoSkymapII.t3_08192x04096_80_py.jpg")
     (*:loadTexture asset-manager "Textures/TychoSkymapII.t3_08192x04096_80_my.jpg"))))

(define (%prepare-to-attach-play-gamestate state::PlayGameState client::FabricClient)
  (unless (*:getInitialized state)
    (let* ((client::FabricClient (*:getApp state))
           (screen::Screen (*:getScreen client))
           (gui-node::Node (*:getGuiNode client))
           (asset-manager::AssetManager (get-asset-manager))
           (celestial-body::Sphere (Sphere 128 128 2048.0))
           (body-mat::Material (Material asset-manager "Common/MatDefs/Misc/Unshaded.j3md"))
           (fabric-node (choose-any +fabric-nodes+))
           (texname (string-append "Textures/" (get-key (cdr fabric-node) 'body-texture:)))
           (body-texture (*:loadTexture asset-manager texname))
           (body-pivot::Geometry (Geometry "Center" celestial-body))
           (body-rotator::RotateControl (RotateControl 0.0 0.0 0.025))
           (sky::Spatial (%make-sky))
           (nameplate::Label (Label screen "NodeNameplate" (Vector2f 32 32)(Vector2f 800 40)))
           (rotation (Quaternion))
           (TextureMode Sphere:TextureMode)
           (Projected TextureMode:Projected)
           (pitch-axis (Vector3f 1 0 0))
           (camera (*:getCamera client))
           (Align BitmapFont:Align))
      (*:setNodeNameplate state nameplate)
      (*:setText nameplate (car fabric-node))
      (*:setTextAlign nameplate Align:Left)
      (*:setFont nameplate "Interface/Fonts/Laconic30.fnt")
      (*:setFontSize nameplate 30)
      (*:setFontColor nameplate ColorRGBA:Green)
      (*:setCelestialBody state celestial-body)
      (*:setBodyRotator state body-rotator)
      (*:setBodyPivot state body-pivot)
      (*:setFrustumFar camera 40000)
      (*:setLocation camera (Vector3f 0.0 0.0 18000))
      (*:fromAngleAxis rotation (* -1 (/ PI 2)) pitch-axis)
      (*:setLocalRotation body-pivot rotation)
      (*:setLocalTranslation body-pivot 0 0 0)
      (*:setSky state sky)
      (*:setTextureMode celestial-body Projected)
      (*:setTexture body-mat "ColorMap" body-texture)
      (*:setMaterial body-pivot body-mat)
      (*:addControl body-pivot body-rotator)
      (*:setInitialized state #t))))

(define (%did-attach-play-gamestate state::PlayGameState mgr::AppStateManager)
  (when (*:getInitialized state)
    (let* ((client::FabricClient (*:getApp state))
           (screen::Screen (*:getScreen client))
           (gui-node::Node (*:getGuiNode client)))
      (*:enqueue client
                 (runnable (lambda ()
                             (let* ((client::FabricClient (*:getApp state))
                                    (root::Node (*:getRootNode client)))
                               (*:addElement screen (*:getNodeNameplate state))
                               (*:attachChild root (*:getSky state))
                               (*:attachChild root (*:getBodyPivot state))
                               (*:addControl gui-node screen))))))))

(define (%did-detach-play-gamestate state::PlayGameState mgr::AppStateManager)
  (when (*:getInitialized state)
    (let* ((client::FabricClient (*:getApp state))
           (screen::Screen (*:getScreen client))
           (gui-node::Node (*:getGuiNode client)))
      (*:enqueue client
                 (runnable (lambda ()
                             (let* ((client::FabricClient (*:getApp state))
                                    (root::Node (*:getRootNode client))
                                    (sky::Spatial (*:getSky state))
                                    (body-pivot::Geometry (*:getBodyPivot state))
                                    (body-rotator::Control (*:getBodyRotator state)))
                               (*:removeElement screen (*:getNodeNameplate state))
                               (*:setNodeNameplate state #!null)
                               (*:detachChild root sky)
                               (*:detachChild root body-pivot)
                               (*:removeControl body-pivot body-rotator)
                               (*:setSky state #!null)
                               (*:setBodyPivot state #!null)
                               (*:setBodyRotator state #!null)
                               (*:setCelestialBody state #!null)
                               (*:removeControl gui-node screen)
                               (*:setInitialized state #f))))))))


;;; CLASS PlayGameState
;;; ---------------------------------------------------------------------
;;; a GameState that manages playable game scene

(defclass PlayGameState (FabricGameState)
  (slots:
   (celestial-body init-form: #!null getter: getCelestialBody setter: setCelestialBody)
   (body-rotator init-form: #!null getter: getBodyRotator setter: setBodyRotator)
   (body-pivot init-form: #!null getter: getBodyPivot setter: setBodyPivot)
   (sky init-form: #!null getter: getSky setter: setSky)
   (node-nameplate init-form: #!null getter: getNodeNameplate setter: setNodeNameplate))
  (methods:
   ((cleanup) #!void)
   ((isEnabled) #t)
   ((isInitialized) initialized)
   ;; prepare to attach the state
   ((prepareToAttach mgr::AppStateManager client::FabricClient)
    (%prepare-to-attach-play-gamestate (this) client))
   ;; the state has been attached
   ((stateAttached mgr::AppStateManager)
    (%did-attach-play-gamestate (this) mgr))
   ;; the state has been detached; dispose of the celestial body
   ((stateDetached mgr::AppStateManager)
    (%did-detach-play-gamestate (this) mgr))
   ((cleanupDetached mgr::AppStateManager client::FabricClient) #!void)))


