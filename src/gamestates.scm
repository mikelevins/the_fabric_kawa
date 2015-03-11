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
 PlayGameState
 TransitGameState)

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
(require "view-pickarmor.scm")
(require "view-pickaugment.scm")
(require "view-pickcharacter.scm")
(require "view-pickfaction.scm")
(require "view-pickname.scm")
(require "view-pickweapon.scm")
(require "view-rotatecontrol.scm")
(require "view-skybox.scm")
(require "view-celestial-body.scm")
(require "view-node-nameplate.scm")
(require "view-faction-nameplate.scm")
(require "view-character-nameplate.scm")

;;; ---------------------------------------------------------------------
;;; Java imports
;;; ---------------------------------------------------------------------

(import-as AbstractAppState com.jme3.app.state.AbstractAppState)
(import-as AppStateManager com.jme3.app.state.AppStateManager)
(import-as BitmapFont com.jme3.font.BitmapFont)
(import-as Camera com.jme3.renderer.Camera)
(import-as CameraNode com.jme3.scene.CameraNode)
(import-as ColorRGBA com.jme3.math.ColorRGBA)
(import-as Geometry com.jme3.scene.Geometry)
(import-as Node com.jme3.scene.Node)
(import-as Screen tonegod.gui.core.Screen)
(import-as SkyFactory com.jme3.util.SkyFactory)
(import-as Spatial com.jme3.scene.Spatial)
(import-as Label tonegod.gui.controls.text.Label)
(import-as Vector2f com.jme3.math.Vector2f)
(import-as Vector3f com.jme3.math.Vector3f)
(import-as Window tonegod.gui.controls.windows.Window)


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

(defclass CreateCharacterGameState (FabricGameState)
  (slots:
   (faction-nameplate init-form: #!null getter: getFactionNameplate setter: setFactionNameplate)
   (character-nameplate init-form: #!null getter: getCharacterNameplate setter: setCharacterNameplate)
   (faction-picker init-form: #!null getter: getFactionPicker setter: setFactionPicker)
   (weapon-picker init-form: #!null getter: getWeaponPicker setter: setWeaponPicker)
   (armor-picker init-form: #!null getter: getArmorPicker setter: setArmorPicker)
   (augment-picker init-form: #!null getter: getAugmentPicker setter: setAugmentPicker)
   (name-picker init-form: #!null getter: getNamePicker setter: setNamePicker))
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

(define (%prepare-to-attach-create-character-gamestate state::CreateCharacterGameState client::FabricClient)
  (unless (*:getInitialized state)
    (let* ((screen::Screen (*:getScreen client))
           (gui-node::Node (*:getGuiNode client))
           (Align BitmapFont:Align)
           (faction-nameplate::Label (make-faction-nameplate screen))
           (character-nameplate::Label (make-character-nameplate screen))
           (faction-picker::Window (make-faction-picker screen))
           (weapon-picker::Window (make-weapon-picker screen))
           (armor-picker::Window (make-armor-picker screen))
           (augment-picker::Window (make-augment-picker screen))
           (name-picker::Window (make-name-picker screen)))
      (*:setFactionPicker state faction-picker)
      (*:setWeaponPicker state weapon-picker)
      (*:setArmorPicker state armor-picker)
      (*:setAugmentPicker state augment-picker)
      (*:setNamePicker state name-picker)
      (*:setFactionNameplate state faction-nameplate)
      (*:setCharacterNameplate state character-nameplate)
      (*:setInitialized state #t))))

(define (%did-attach-create-character-gamestate state::CreateCharacterGameState mgr::AppStateManager)
  (when (*:getInitialized state)
    (let* ((client::FabricClient (*:getApp state))
           (screen::Screen (*:getScreen client))
           (gui-node::Node (*:getGuiNode client)))
      (*:enqueue client
                 (runnable (lambda ()
                             (*:addElement screen (*:getFactionNameplate state))
                             (*:addElement screen (*:getCharacterNameplate state))
                             (*:addElement screen (*:getFactionPicker state))
                             (*:addElement screen (*:getWeaponPicker state))
                             (*:addElement screen (*:getArmorPicker state))
                             (*:addElement screen (*:getAugmentPicker state))
                             (*:addElement screen (*:getNamePicker state))
                             (*:addControl gui-node screen)))))))

(define (%did-detach-create-character-gamestate state::CreateCharacterGameState mgr::AppStateManager)
  (when (*:getInitialized state)
    (let* ((client::FabricClient (*:getApp state))
           (screen::Screen (*:getScreen client))
           (gui-node::Node (*:getGuiNode client)))
      (*:enqueue client
                 (runnable (lambda ()
                             (*:removeElement screen (*:getFactionNameplate state))
                             (*:removeElement screen (*:getCharacterNameplate state))
                             (*:removeElement screen (*:getFactionPicker state))
                             (*:removeElement screen (*:getWeaponPicker state))
                             (*:removeElement screen (*:getArmorPicker state))
                             (*:removeElement screen (*:getAugmentPicker state))
                             (*:removeElement screen (*:getNamePicker state))
                             (*:removeControl gui-node screen)))))))


;;; =====================================================================
;;; CLASS LoginGameState
;;; =====================================================================

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


;;; =====================================================================
;;; CLASS PickCharacterGameState
;;; =====================================================================
;;; TODO: make a node picker so the player can choose which accessible node
;;; to enter the game at

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


;;; =====================================================================
;;; CLASS PlayGameState
;;; =====================================================================
;;; for the moment, until I make a real solution for picking nodes
;;; from the character picker, we'll just choose a random Fabric node
;;; when the state is attached and display the texture for that
;;;
;;; TODO: destroy the central celestial object when detaching
;;; so that we can reattach with a different one

(defclass PlayGameState (FabricGameState)
  (slots:
   (celestial-body init-form: #!null getter: getCelestialBody setter: setCelestialBody)
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

(define (%prepare-to-attach-play-gamestate state::PlayGameState client::FabricClient)
  (unless (*:getInitialized state)
    (let* ((client::FabricClient (*:getApp state))
           (screen::Screen (*:getScreen client))
           (gui-node::Node (*:getGuiNode client))
           (fabric-node (choose-any +fabric-nodes+))
           (nodename (car fabric-node))
           (texname::String (get-key (cdr fabric-node) 'body-texture:))
           (body (make-celestial-body texname))
           (sky::Spatial (make-sky-box))
           (nameplate::Label (make-node-nameplate screen nodename))
           (camera (*:getCamera client))
           (Align BitmapFont:Align))
      (*:setNodeNameplate state nameplate)
      (*:setCelestialBody state body)
      (*:setFrustumFar camera 40000)
      (*:setLocation camera (Vector3f 0.0 0.0 18000))
      (*:setSky state sky)
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
                               (*:attachChild root (*:getCelestialBody state))
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
                                    (body::Geometry (*:getCelestialBody state)))
                               (*:removeElement screen (*:getNodeNameplate state))
                               (*:setNodeNameplate state #!null)
                               (*:detachChild root sky)
                               (*:detachChild root body)
                               (*:setSky state #!null)
                               (*:setCelestialBody state #!null)
                               (*:removeControl gui-node screen)
                               (*:setInitialized state #f))))))))


;;; =====================================================================
;;; CLASS TransitGameState
;;; =====================================================================
;;; A GameState used to display transit from one Fabric node to another

(defclass TransitGameState (FabricGameState)
  (slots:
   (status-label init-form: #!null getter: getStatusLabel setter: setFactionNameplate))
  (methods:
   ((cleanup) #!void)
   ((isEnabled) #t)
   ((isInitialized) initialized)
   ;; prepare to attach the state
   ((prepareToAttach mgr::AppStateManager client::FabricClient)
    (%prepare-to-attach-transit-gamestate (this) client))
   ;; the state has been attached
   ((stateAttached mgr::AppStateManager)
    (%did-attach-transit-gamestate (this) mgr))
   ;; the state has been detached
   ((stateDetached mgr::AppStateManager)
    (%did-detach-transit-gamestate (this) mgr))
   ;; cleanup after the state is detached
   ((cleanupDetached mgr::AppStateManager client::FabricClient) #!void)))

(define (%prepare-to-attach-transit-gamestate state::TransitGameState client::FabricClient)
  (unless (*:getInitialized state)
    (let* ((screen::Screen (*:getScreen client))
           (gui-node::Node (*:getGuiNode client))
           (Align BitmapFont:Align)
           (label::Label (Label screen "FactionNameplate" (Vector2f 600 40)(Vector2f 1200 40))))
      (*:setFactionNameplate state label)
      (*:setText label "In transit...")
      (*:setTextAlign label Align:Left)
      (*:setFont label "Interface/Fonts/Laconic30.fnt")
      (*:setFontSize label 30)
      (*:setFontColor label ColorRGBA:Green)
      (*:setInitialized state #t))))

(define (%did-attach-transit-gamestate state::TransitGameState mgr::AppStateManager)
  (when (*:getInitialized state)
    (let* ((client::FabricClient (*:getApp state))
           (screen::Screen (*:getScreen client))
           (gui-node::Node (*:getGuiNode client)))
      (*:enqueue client
                 (runnable (lambda ()
                             (*:addElement screen (*:getStatusLabel state))
                             (*:addControl gui-node screen)))))))

(define (%did-detach-transit-gamestate state::TransitGameState mgr::AppStateManager)
  (when (*:getInitialized state)
    (let* ((client::FabricClient (*:getApp state))
           (screen::Screen (*:getScreen client))
           (gui-node::Node (*:getGuiNode client)))
      (*:enqueue client
                 (runnable (lambda ()
                             (*:removeElement screen (*:getStatusLabel state))
                             (*:removeControl gui-node screen)))))))
