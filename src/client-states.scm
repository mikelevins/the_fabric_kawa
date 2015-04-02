;;;; ***********************************************************************
;;;;
;;;; Name:          client-states.scm
;;;; Project:       The Fabric: a far-future MMORPG
;;;; Purpose:       Fabric client states
;;;; Author:        mikel evins
;;;; Copyright:     2015 by mikel evins
;;;;
;;;; ***********************************************************************

(module-export
 CreateCharacterClientState
 FabricClientState
 LoginClientState
 PickCharacterClientState
 PlayClientState
 TransitClientState
 WorkshopClientState)

;;; ---------------------------------------------------------------------
;;; ABOUT
;;; ---------------------------------------------------------------------

;;; a FabricClientState is an AppState that represents one of the Fabric
;;; client's major modes: login, create a character, pick a character,
;;; or play. FabricClientState defines a small set of common slots and methods
;;; that all Fabric Client-States must implement

;;; ---------------------------------------------------------------------
;;; required modules
;;; ---------------------------------------------------------------------

(require "util-java.scm")
(require "util-error.scm")
(require "util-lists.scm")
(require "syntax-classes.scm")
(require "view-loginbox.scm")
(require "client-state-login.scm")
(require "client-state-create-character.scm")
(require "client-state-pick-character.scm")
(require "client-state-play.scm")
(require "client-state-transit.scm")
(require "client-state-workshop.scm")
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
;;; CLASS FabricClientState
;;; =====================================================================

(defclass FabricClientState (AbstractAppState)
  (slots:
   (app init-form: #!null getter: getApp setter: setApp)
   (initialized init-form: #F getter: getInitialized setter: setInitialized))
  (methods:
   ((cleanup)
    (error "cleanup must be implemented in subclasses of FabricClientState"))
   ((isEnabled)
    (error "isEnabled must be implemented in subclasses of FabricClientState"))
   ((isInitialized) initialized)
   ((prepareToAttach mgr::AppStateManager client::FabricClient)
    (error "prepareToAttach must be implemented in subclasses of FabricClientState"))
   ((stateAttached mgr::AppStateManager)
    (error "stateAttached must be implemented in subclasses of FabricClientState"))
   ((stateDetached mgr::AppStateManager)
    (error "stateDetached must be implemented in subclasses of FabricClientState"))
   ((cleanupDetached mgr::AppStateManager client::FabricClient)
    (error "cleanupDetached must be implemented in subclasses of FabricClientState"))))


;;; =====================================================================
;;; CLASS CreateCharacterClientState
;;; =====================================================================

(defclass CreateCharacterClientState (FabricClientState)
  (slots:
   (faction init-form: #f getter: getFaction setter: setFaction)
   (weapon init-form: #f getter: getWeapon setter: setWeapon)
   (armor init-form: #f getter: getArmor setter: setArmor)
   (augment init-form: #f getter: getAugment setter: setAugment)
   (faction-nameplate init-form: #!null getter: getFactionNameplate setter: setFactionNameplate)
   (character-nameplate init-form: #!null getter: getCharacterNameplate setter: setCharacterNameplate)
   (sky init-form: #!null getter: getSky setter: setSky)
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
    (prepare-to-attach-create-character-client-state (this) client))
   ;; the state has been attached
   ((stateAttached mgr::AppStateManager)
    (did-attach-create-character-client-state (this) mgr))
   ;; the state has been detached
   ((stateDetached mgr::AppStateManager)
    (did-detach-create-character-client-state (this) mgr))
   ;; cleanup after the state is detached
   ((cleanupDetached mgr::AppStateManager client::FabricClient) #!void)))


;;; =====================================================================
;;; CLASS LoginClientState
;;; =====================================================================

(defclass LoginClientState (FabricClientState)
  (slots:
   (sky init-form: #!null getter: getSky setter: setSky)
   (loginbox init-form: #!null getter: getLoginBox setter: setLoginBox))
  (methods:
   ((cleanup) #!void)
   ((isEnabled) #t)
   ((isInitialized) initialized)
   ;; prepare to attach the state
   ((prepareToAttach mgr::AppStateManager client::FabricClient)
    (prepare-to-attach-login-client-state (this) client))
   ;; the state has been attached
   ((stateAttached mgr::AppStateManager)
    (did-attach-login-client-state (this) mgr))
   ;; the state has been detached
   ((stateDetached mgr::AppStateManager)
    (did-detach-login-client-state (this) mgr))
   ((cleanupDetached mgr::AppStateManager client::FabricClient) #!void)))

;;; =====================================================================
;;; CLASS PickCharacterClientState
;;; =====================================================================
;;; TODO: make a node picker so the player can choose which accessible node
;;; to enter the game at

(defclass PickCharacterClientState (FabricClientState)
  (slots:
   (sky init-form: #!null getter: getSky setter: setSky)
   (picker init-form: #!null getter: getCharacterPicker setter: setCharacterPicker))
  (methods:
   ((cleanup) #!void)
   ((isEnabled) #t)
   ((isInitialized) initialized)
   ;; prepare to attach the state
   ((prepareToAttach mgr::AppStateManager client::FabricClient)
    (prepare-to-attach-pick-character-client-state (this) client))
   ;; the state has been attached
   ((stateAttached mgr::AppStateManager)
    (did-attach-pick-character-client-state (this) mgr))
   ;; the state has been detached
   ((stateDetached mgr::AppStateManager)
    (did-detach-pick-character-client-state (this) mgr))
   ((cleanupDetached mgr::AppStateManager client::FabricClient) #!void)))


;;; =====================================================================
;;; CLASS PlayClientState
;;; =====================================================================
;;; for the moment, until I make a real solution for picking nodes
;;; from the character picker, we'll just choose a random Fabric node
;;; when the state is attached and display the texture for that
;;;
;;; TODO: destroy the central celestial object when detaching
;;; so that we can reattach with a different one

(defclass PlayClientState (FabricClientState)
  (slots:
   (celestial-body init-form: #!null getter: getCelestialBody setter: setCelestialBody)
   (sky init-form: #!null getter: getSky setter: setSky)
   (node-nameplate init-form: #!null getter: getNodeNameplate setter: setNodeNameplate)
   (action-bar init-form: #!null getter: getActionBar setter: setActionBar)
   (chatbox init-form: #!null getter: getChatbox setter: setChatbox))
  (methods:
   ((cleanup) #!void)
   ((isEnabled) #t)
   ((isInitialized) initialized)
   ;; prepare to attach the state
   ((prepareToAttach mgr::AppStateManager client::FabricClient)
    (prepare-to-attach-play-client-state (this) client))
   ;; the state has been attached
   ((stateAttached mgr::AppStateManager)
    (did-attach-play-client-state (this) mgr))
   ;; the state has been detached; dispose of the celestial body
   ((stateDetached mgr::AppStateManager)
    (did-detach-play-client-state (this) mgr))
   ((cleanupDetached mgr::AppStateManager client::FabricClient) #!void)))


;;; =====================================================================
;;; CLASS TransitClientState
;;; =====================================================================
;;; A FabricClientState used to display transit from one Fabric node to another

(defclass TransitClientState (FabricClientState)
  (slots:
   (sky init-form: #!null getter: getSky setter: setSky)
   (status-label init-form: #!null getter: getStatusLabel setter: setFactionNameplate))
  (methods:
   ((cleanup) #!void)
   ((isEnabled) #t)
   ((isInitialized) initialized)
   ;; prepare to attach the state
   ((prepareToAttach mgr::AppStateManager client::FabricClient)
    (prepare-to-attach-transit-client-state (this) client))
   ;; the state has been attached
   ((stateAttached mgr::AppStateManager)
    (did-attach-transit-client-state (this) mgr))
   ;; the state has been detached
   ((stateDetached mgr::AppStateManager)
    (did-detach-transit-client-state (this) mgr))
   ;; cleanup after the state is detached
   ((cleanupDetached mgr::AppStateManager client::FabricClient) #!void)))


;;; =====================================================================
;;; CLASS WorkshopClientState
;;; =====================================================================
;;; A FabricClientState used to test and display game elements

(defclass WorkshopClientState (FabricClientState)
  (slots:
   (sky init-form: #!null getter: getSky setter: setSky))
  (methods:
   ((cleanup) #!void)
   ((isEnabled) #t)
   ((isInitialized) initialized)
   ;; prepare to attach the state
   ((prepareToAttach mgr::AppStateManager client::FabricClient)
    (prepare-to-attach-workshop-client-state (this) client))
   ;; the state has been attached
   ((stateAttached mgr::AppStateManager)
    (did-attach-workshop-client-state (this) mgr))
   ;; the state has been detached
   ((stateDetached mgr::AppStateManager)
    (did-detach-workshop-client-state (this) mgr))
   ;; cleanup after the state is detached
   ((cleanupDetached mgr::AppStateManager client::FabricClient) #!void)))

