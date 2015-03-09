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
;;; for the moment, until I make a real solution for picking nodes
;;; from the character picker, we'll just choose a random Fabric node
;;; when the state is attached and display the texture for that
;;;
;;; TODO: destroy the central celestial object when detaching
;;; so that we can reattach with a different one

(define (load-any-celestial-body)
  (let* ((asset-manager::AssetManager (get-asset-manager))
         (node-entry (choose-any +fabric-nodes+))
         (texname (get-key (cdr node-entry) 'body-texture:)))
    (*:loadTexture asset-manager (string-append "Textures/" texname))))

(defclass PlayGameState (FabricGameState)
  (slots:
   (celestial-body init-form: #!null getter: getCelestialBody setter: setCelestialBody)
   (body-rotator init-form: #!null getter: getBodyRotator setter: setBodyRotator)
   (body-pivot init-form: #!null getter: getBodyPivot setter: setBodyPivot)
   (sky init-form: #!null getter: getSky setter: setSky))
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
             (asset-manager::AssetManager (get-asset-manager))
             (body-mat (Material asset-manager "Common/MatDefs/Misc/Unshaded.j3md"))
             (body-texture (load-any-celestial-body))
             (rotation (Quaternion))
             (TextureMode Sphere:TextureMode)
             (Projected TextureMode:Projected)
             (pitch-axis (Vector3f 1 0 0))
             (camera (*:getCamera client)))
        (set! celestial-body (Sphere 128 128 2048.0))
        (set! body-rotator (RotateControl 0.0 0.0 0.025))
        (set! body-pivot (Geometry "Center" celestial-body))
        (*:setFrustumFar camera 40000)
        (*:setLocation camera (Vector3f 0.0 0.0 18000))
        (*:fromAngleAxis rotation (* -1 (/ PI 2)) pitch-axis)
        (*:setLocalRotation (as Geometry body-pivot) rotation)
        (*:setLocalTranslation (as Geometry body-pivot) 0 0 0)
        (set! sky
              (SkyFactory:createSky
               asset-manager
               (*:loadTexture asset-manager "Textures/TychoSkymapII.t3_08192x04096_80_mx.jpg")
               (*:loadTexture asset-manager "Textures/TychoSkymapII.t3_08192x04096_80_px.jpg")
               (*:loadTexture asset-manager "Textures/TychoSkymapII.t3_08192x04096_80_mz.jpg")
               (*:loadTexture asset-manager "Textures/TychoSkymapII.t3_08192x04096_80_pz.jpg")
               (*:loadTexture asset-manager "Textures/TychoSkymapII.t3_08192x04096_80_py.jpg")
               (*:loadTexture asset-manager "Textures/TychoSkymapII.t3_08192x04096_80_my.jpg")))
        (*:setTextureMode (as Sphere celestial-body) Projected)
        (*:setTexture (as Material body-mat) "ColorMap" body-texture)
        (*:setMaterial (as Geometry body-pivot) body-mat)
        (*:addControl (as Geometry body-pivot) body-rotator)
        (*:setInitialized (this) #t))))
   ;; the state has been attached
   ((stateAttached mgr::AppStateManager)
    (when (*:getInitialized (this))
      (let* ((client::FabricClient (*:getApp (this)))
             (screen::Screen (*:getScreen client))
             (gui-node::Node (*:getGuiNode client)))
        (*:enqueue client
                   (runnable (lambda ()
                               (let* ((client::FabricClient (*:getApp (this)))
                                     (root::Node (*:getRootNode client)))
                                 (*:attachChild root sky)
                                 (*:attachChild root body-pivot))))))))
   ;; the state has been detached; dispose of the celestial body
   ((stateDetached mgr::AppStateManager)
    (when (*:getInitialized (this))
      (let* ((client::FabricClient (*:getApp (this)))
             (screen::Screen (*:getScreen client))
             (gui-node::Node (*:getGuiNode client)))
        (*:enqueue client
                   (runnable (lambda ()
                               (let* ((client::FabricClient (*:getApp (this)))
                                      (root::Node (*:getRootNode client)))
                                 (*:detachChild root sky)
                                 (*:detachChild root body-pivot)
                                 (*:removeControl (as Geometry body-pivot) (as Control body-rotator))
                                 (set! sky #!null)
                                 (set! body-pivot #!null)
                                 (set! body-rotator #!null)
                                 (set! celestial-body #!null)
                                 (*:setInitialized (this) #f))))))))
   ((cleanupDetached mgr::AppStateManager client::FabricClient) #!void)))


