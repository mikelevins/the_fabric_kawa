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
(require "gamestates-login.scm")
(require "gamestates-createchar.scm")
(require "gamestates-pickchar.scm")
(require "gamestates-play.scm")
(require "gamestates-transit.scm")
(require "client-main.scm")

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
(import-as Panel tonegod.gui.controls.windows.Panel)
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
   (name-picker init-form: #!null getter: getNamePicker setter: setNamePicker)
   (character-acceptor init-form: #!null getter: getCharacterAcceptor setter: setCharacterAcceptor))
  (methods:
   ((cleanup) #!void)
   ((isEnabled) #t)
   ((isInitialized) initialized)
   ;; prepare to attach the state
   ((prepareToAttach mgr::AppStateManager client::FabricClient)
    (prepare-to-attach-create-character-gamestate (this) client))
   ;; the state has been attached
   ((stateAttached mgr::AppStateManager)
    (did-attach-create-character-gamestate (this) mgr))
   ;; the state has been detached
   ((stateDetached mgr::AppStateManager)
    (did-detach-create-character-gamestate (this) mgr))
   ;; cleanup after the state is detached
   ((cleanupDetached mgr::AppStateManager client::FabricClient) #!void)))


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
    (prepare-to-attach-login-gamestate (this) client))
   ;; the state has been attached
   ((stateAttached mgr::AppStateManager)
    (did-attach-login-gamestate (this) mgr))
   ;; the state has been detached
   ((stateDetached mgr::AppStateManager)
    (did-detach-login-gamestate (this) mgr))
   ((cleanupDetached mgr::AppStateManager client::FabricClient) #!void)))

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
    (prepare-to-attach-pick-character-gamestate (this) client))
   ;; the state has been attached
   ((stateAttached mgr::AppStateManager)
    (did-attach-pick-character-gamestate (this) mgr))
   ;; the state has been detached
   ((stateDetached mgr::AppStateManager)
    (did-detach-pick-character-gamestate (this) mgr))
   ((cleanupDetached mgr::AppStateManager client::FabricClient) #!void)))


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
   (node-nameplate init-form: #!null getter: getNodeNameplate setter: setNodeNameplate)
   (action-bar init-form: #!null getter: getActionBar setter: setActionBar))
  (methods:
   ((cleanup) #!void)
   ((isEnabled) #t)
   ((isInitialized) initialized)
   ;; prepare to attach the state
   ((prepareToAttach mgr::AppStateManager client::FabricClient)
    (prepare-to-attach-play-gamestate (this) client))
   ;; the state has been attached
   ((stateAttached mgr::AppStateManager)
    (did-attach-play-gamestate (this) mgr))
   ;; the state has been detached; dispose of the celestial body
   ((stateDetached mgr::AppStateManager)
    (did-detach-play-gamestate (this) mgr))
   ((cleanupDetached mgr::AppStateManager client::FabricClient) #!void)))


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
    (prepare-to-attach-transit-gamestate (this) client))
   ;; the state has been attached
   ((stateAttached mgr::AppStateManager)
    (did-attach-transit-gamestate (this) mgr))
   ;; the state has been detached
   ((stateDetached mgr::AppStateManager)
    (did-detach-transit-gamestate (this) mgr))
   ;; cleanup after the state is detached
   ((cleanupDetached mgr::AppStateManager client::FabricClient) #!void)))

