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
;;; that all Fabric FabricGameStates must implement

;;; ---------------------------------------------------------------------
;;; required modules
;;; ---------------------------------------------------------------------

(require "util-java.scm")
(require "util-error.scm")
(require "syntax-classes.scm")
(require "view-loginbox.scm")
(require "client-main.scm")
(require "view-pickcharacter.scm")

;;; ---------------------------------------------------------------------
;;; Java imports
;;; ---------------------------------------------------------------------

(import-as AbstractAppState com.jme3.app.state.AbstractAppState)
(import-as AppStateManager com.jme3.app.state.AppStateManager)
(import-as BitmapFont com.jme3.font.BitmapFont)
(import-as ColorRGBA com.jme3.math.ColorRGBA)
(import-as Node com.jme3.scene.Node)
(import-as Screen tonegod.gui.core.Screen)
(import-as Label tonegod.gui.controls.text.Label)
(import-as Vector2f com.jme3.math.Vector2f)


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
   (faction-nameplate init-form: #!null getter: getFactionNameplate setter: setFactionNameplate))
  (methods:
   ((cleanup) #!void)
   ((isEnabled) #t)
   ((isInitialized) initialized)
   ;; prepare to attach the state
   ((prepareToAttach mgr::AppStateManager client::FabricClient)
    (unless (*:getInitialized (this))
      (let* ((client::FabricClient (*:getApp (this)))
             (screen::Screen (*:getScreen client))
             (gui-node::Node (*:getGuiNode client))
             (Align BitmapFont:Align))
        (set! faction-nameplate (Label screen "FactionNameplate" (Vector2f 600 40)(Vector2f 1200 40)))
        (*:setText (as Label faction-nameplate) "Faction: ")
        (*:setTextAlign (as Label faction-nameplate) Align:Left)
        (*:setFont (as Label faction-nameplate) "Interface/Fonts/Laconic30.fnt")
        (*:setFontSize (as Label faction-nameplate) 30)
        (*:setFontColor (as Label faction-nameplate) ColorRGBA:Green)
        (*:setInitialized (this) #t))))
   ;; the state has been attached
   ((stateAttached mgr::AppStateManager)
    (when (*:getInitialized (this))
      (let* ((client::FabricClient (*:getApp (this)))
             (screen::Screen (*:getScreen client))
             (gui-node::Node (*:getGuiNode client)))
        (*:enqueue client
                   (runnable (lambda ()
                               (*:addElement screen faction-nameplate)
                               (*:addControl gui-node screen)))))))
   ;; the state has been detached
   ((stateDetached mgr::AppStateManager)
    (when (*:getInitialized (this))
      (let* ((client::FabricClient (*:getApp (this)))
             (screen::Screen (*:getScreen client))
             (gui-node::Node (*:getGuiNode client)))
        (*:enqueue client
                   (runnable (lambda ()
                               (*:removeElement screen faction-nameplate)
                               (*:removeControl gui-node screen)))))))
   ((cleanupDetached mgr::AppStateManager client::FabricClient)
    (format #t "~%Cleaning up after detaching LoginGameState..."))))


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
    (unless (*:getInitialized (this))
      (let* ((client::FabricClient (*:getApp (this)))
             (screen::Screen (*:getScreen client))
             (gui-node::Node (*:getGuiNode client)))
        (set! loginbox (FabricLoginBox screen "LoginBox" (Vector2f 700 300)(Vector2f 700 300)))
        (*:setInitialized (this) #t))))
   ;; the state has been attached
   ((stateAttached mgr::AppStateManager)
    (when (*:getInitialized (this))
      (let* ((client::FabricClient (*:getApp (this)))
             (screen::Screen (*:getScreen client))
             (gui-node::Node (*:getGuiNode client)))
        (*:enqueue client
                   (runnable (lambda ()
                               (*:addElement screen loginbox)
                               (*:addControl gui-node screen)))))))
   ;; the state has been detached
   ((stateDetached mgr::AppStateManager)
    (when (*:getInitialized (this))
      (let* ((client::FabricClient (*:getApp (this)))
             (screen::Screen (*:getScreen client))
             (gui-node::Node (*:getGuiNode client)))
        (*:enqueue client
                   (runnable (lambda ()
                               (*:removeElement screen loginbox)
                               (*:removeControl gui-node screen)))))))
   ((cleanupDetached mgr::AppStateManager client::FabricClient)
    (format #t "~%Cleaning up after detaching LoginGameState..."))))


;;; =====================================================================
;;; CLASS PickCharacterGameState
;;; =====================================================================

(defclass PickCharacterGameState (FabricGameState)
  (slots:
   (picker init-form: #!null getter: getCharacterPicker setter: setCharacterPicker))
  (methods:
   ((cleanup) #!void)
   ((isEnabled) #t)
   ((isInitialized) initialized)
   ;; prepare to attach the state
   ((prepareToAttach mgr::AppStateManager client::FabricClient)
    (unless (*:getInitialized (this))
      (let* ((client::FabricClient (*:getApp (this)))
             (screen::Screen (*:getScreen client))
             (gui-node::Node (*:getGuiNode client)))
        (set! picker (make-character-picker screen))
        (*:setInitialized (this) #t))))
   ;; the state has been attached
   ((stateAttached mgr::AppStateManager)
    (when (*:getInitialized (this))
      (let* ((client::FabricClient (*:getApp (this)))
             (screen::Screen (*:getScreen client))
             (gui-node::Node (*:getGuiNode client)))
        (*:enqueue client
                   (runnable (lambda ()
                               (*:addElement screen picker)
                               (*:addControl gui-node screen)))))))
   ;; the state has been detached
   ((stateDetached mgr::AppStateManager)
    (when (*:getInitialized (this))
      (let* ((client::FabricClient (*:getApp (this)))
             (screen::Screen (*:getScreen client))
             (gui-node::Node (*:getGuiNode client)))
        (*:enqueue client
                   (runnable (lambda ()
                               (*:removeElement screen picker)
                               (*:removeControl gui-node screen)))))))
   ((cleanupDetached mgr::AppStateManager client::FabricClient)
    (format #t "~%Cleaning up after detaching LoginGameState..."))))


;;; =====================================================================
;;; CLASS PlayGameState
;;; =====================================================================

(defclass PlayGameState (FabricGameState)
  (slots:)
  (methods:
   ((cleanup)
    (format #t "~%cleanup called for PlayGameState..."))
   ((isEnabled) #t)
   ((isInitialized) initialized)
   ((prepareToAttach mgr::AppStateManager client::FabricClient)
    (format #t "~%Preparing to attach PlayGameState..."))
   ((stateAttached mgr::AppStateManager)
    (format #t "~%PlayGameState attached..."))
   ((stateDetached mgr::AppStateManager)
    (format #t "~%PlayGameState detached..."))
   ((cleanupDetached mgr::AppStateManager client::FabricClient)
    (format #t "~%Cleaning up after detaching PlayGameState..."))))
