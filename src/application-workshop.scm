;;;; ***********************************************************************
;;;; Name:          application-workshop.scm
;;;; Project:       The Fabric: a far-future MMORPG
;;;; Purpose:       the builder app
;;;; Author:        mikel evins
;;;; Copyright:     2014 by mikel evins
;;;;
;;;; ***********************************************************************

(module-export FabricWorkshop make-workshop worker-position worker-position-watcher)

;;; ---------------------------------------------------------------------
;;; required modules
;;; ---------------------------------------------------------------------

(require 'list-lib)
(require "utilities-math.scm")
(require "utilities-java.scm")
(require "model-hubs.scm")
(require "interface-frame.scm")
(require "assets-general.scm")
(require "application-common.scm")

;;; ---------------------------------------------------------------------
;;; Java imports
;;; ---------------------------------------------------------------------

(define-private-alias AssetManager com.jme3.asset.AssetManager)
(define-private-alias AppSettings com.jme3.system.AppSettings)
(define-private-alias BloomFilter com.jme3.post.filters.BloomFilter)
(define-private-alias ButtonAdapter tonegod.gui.controls.buttons.ButtonAdapter)
(define-private-alias Camera com.jme3.renderer.Camera)
(define-private-alias Element tonegod.gui.core.Element)
(define-private-alias FilterPostProcessor com.jme3.post.FilterPostProcessor)
(define-private-alias FlyByCamera com.jme3.input.FlyByCamera)
(define-private-alias Geometry com.jme3.scene.Geometry)
(define-private-alias HashPMap org.pcollections.HashPMap)
(define-private-alias Label tonegod.gui.controls.text.Label)
(define-private-alias MotionAllowedListener com.jme3.collision.MotionAllowedListener)
(define-private-alias Mouse org.lwjgl.input.Mouse)
(define-private-alias Node com.jme3.scene.Node)
(define-private-alias Screen tonegod.gui.core.Screen)
(define-private-alias SkyFactory com.jme3.util.SkyFactory)
(define-private-alias Spatial com.jme3.scene.Spatial)
(define-private-alias Sphere com.jme3.scene.shape.Sphere)
(define-private-alias Vector2f com.jme3.math.Vector2f)
(define-private-alias Vector3f com.jme3.math.Vector3f)
(define-private-alias ViewPort com.jme3.renderer.ViewPort)
(define-private-alias Window tonegod.gui.controls.windows.Window)

;;; ---------------------------------------------------------------------
;;; FabricApp - the abstract client application class
;;; ---------------------------------------------------------------------

(define (make-celestial-object object-name)
  (let* ((asset-manager::AssetManager (get-asset-manager))
         (object::Sphere (Sphere 128 128 2048.0))
         (object-geom (com.jme3.scene.Geometry object-name object))
         (object-mat (com.jme3.material.Material asset-manager "Common/MatDefs/Misc/Unshaded.j3md"))
         (object-tex (*:loadTexture asset-manager (string-append "Textures/" object-name ".jpg"))))
    (*:setTextureMode object Sphere:TextureMode:Projected)
    ;;(*:setTextureMode object Sphere:TextureMode:Original)
    (*:setTexture object-mat "ColorMap" object-tex)
    (*:setMaterial object-geom object-mat)
    (*:setLocalTranslation object-geom 0 0 -8000)
    (*:rotate object-geom (degrees->radians -90) 0 0)
    object-geom))

(define-simple-class FabricWorkshop (FabricApp)
  ((setFrameKey key val) (cond
                          ;; focus-object:
                          ((eq? focus-object: key)
                           (let* ((focus-object (make-celestial-object val))
                                  (root::Node (get-key (this) root-node:))
                                  (already (get-key (this) focus-object:))
                                  (old-slots::HashPMap (*:frameSlots (this))))
                             (unless (or (not already)
                                         (jnull? already))
                               (*:detachChild root already))
                             (*:setFrameSlots (this)
                                              (*:plus old-slots focus-object: focus-object))
                             (*:attachChild root focus-object)))
                          ;; default handler
                          (#t (invoke-special FabricApp (this) 'setFrameKey key val)))))

;;; ---------------------------------------------------------------------
;;; application initialization
;;; ---------------------------------------------------------------------

(define (setup-lighting app::FabricWorkshop)
  (let* ((asset-manager::AssetManager (get-asset-manager))
         (bloom (BloomFilter BloomFilter:GlowMode:Objects))
         (filter-processor::FilterPostProcessor (FilterPostProcessor asset-manager))
         (viewport::ViewPort (get-key app viewport:)))
    (*:setDownSamplingFactor bloom 2.0)
    (*:setBloomIntensity bloom 2.0)
    (*:addFilter filter-processor bloom)
    (*:addProcessor viewport filter-processor)))

(define (make-workshop-sky)
  (let* ((asset-manager::AssetManager (get-asset-manager))
         (sky::Spatial (SkyFactory:createSky
                        asset-manager 
                        (*:loadTexture asset-manager "Textures/boxgrid.jpg")
                        (*:loadTexture asset-manager "Textures/boxgrid.jpg")
                        (*:loadTexture asset-manager "Textures/boxgrid.jpg")
                        (*:loadTexture asset-manager "Textures/boxgrid.jpg")
                        (*:loadTexture asset-manager "Textures/boxgrid.jpg")
                        (*:loadTexture asset-manager "Textures/boxgrid.jpg"))))
    
    (*:setName sky "skybox")
    sky))

;;; ---------------------------------------------------------------------
;;; tracking camera position
;;; ---------------------------------------------------------------------

(define worker-position (make-parameter (Vector3f 0 0 0)))
(define worker-position-watcher (make-parameter #f))

(define (notify-worker-moved position)
  (worker-position position)
  (when (worker-position-watcher)
    (let* ((watcher::Element (worker-position-watcher))
           (pos::Vector3f (worker-position))
           (posx (*:getX pos))
           (posy (*:getY pos))
           (posz (*:getZ pos))
           (pos-text (format #f "Position: ~6,2f, ~6,2f, ~6,2f" posx posy posz)))
      (*:setText watcher pos-text))))

(define-simple-class ReportMotionListener (MotionAllowedListener)
  ((checkMotionAllowed position velocity) (begin (notify-worker-moved position)
                                                 (*:addLocal position velocity))))

;;; ---------------------------------------------------------------------
;;; the inspector window
;;; ---------------------------------------------------------------------

(define (make-inspector app screen)
  (let* ((screen (get-key app gui-screen:))
         (settings::AppSettings (get-key app app-settings:))
         (screen-margin 8)
         (inspector-width 400)
         (inspector-height 400)
         (inspector-left (- (*:getWidth settings) inspector-width screen-margin))
         (inspector-top screen-margin)
         (win (Window screen "Inspector"
                      (Vector2f inspector-left inspector-top)
                      (Vector2f inspector-width inspector-height)))
         (position-label (Label screen "Position Watcher"
                                (Vector2f 8 8)(Vector2f 384 24))))
    (*:addChild win position-label)
    (worker-position-watcher position-label)
    win))

;;; ---------------------------------------------------------------------
;;; the palette window
;;; ---------------------------------------------------------------------

(define-simple-class PaletteButton (ButtonAdapter)
  (name init-form: #!null)
  ((*init* screen button-name::java.lang.String position)
   (invoke-special ButtonAdapter (this) '*init* screen button-name position)
   (set! name button-name))
  ((onMouseLeftReleased event)(set-key! (*:getApplication (*:getScreen (this)))
                                        focus-object: name)))

(define (make-palette app screen)
  (let* ((screen (get-key app gui-screen:))
         (settings::AppSettings (get-key app app-settings:))
         (screen-margin 8)
         (palette-width 144)
         (palette-height (- (*:getHeight settings)
                            176
                            (* 2 screen-margin)))
         (palette-left screen-margin)
         (palette-top screen-margin)
         (win (Window screen "Palette"
                      (Vector2f palette-left palette-top)
                      (Vector2f palette-width palette-height)))
         (hubs-label (Label screen "Hubs" (Vector2f 16 24)(Vector2f 100 32)))
         (hnames (hub-names))
         (hoffsets (map (lambda (n)(+ 60 (* n 40)))
                        (iota (length hnames))))
         (hub-buttons (map (lambda (nm::java.lang.String i)
                             (let ((btn (PaletteButton screen nm (Vector2f 32 i))))
                               (*:setText btn nm)
                               btn))
                           hnames hoffsets)))
    (*:setText hubs-label "Hubs")
    (*:addChild win hubs-label)
    (for-each (lambda (btn)(*:addChild win btn))
              hub-buttons)
    win))

;;; ---------------------------------------------------------------------
;;; set up the workshop
;;; ---------------------------------------------------------------------

(define (setup-workshop-gui app)
  (let* ((screen::Screen (get-key app gui-screen:))
         (inspector (make-inspector app screen))
         (palette (make-palette app screen)))
    (*:addElement screen inspector)
    (*:addElement screen palette)
    app))

(define (init-workshop app)
  (let ((default-sky (make-workshop-sky))
        (cam::Camera (get-key app camera:))
        (fly-cam::FlyByCamera (get-key app flyby-camera:))
        (motion-reporter (ReportMotionListener)))
    (*:setMotionAllowedListener fly-cam motion-reporter)
    (*:setFrustumFar cam 30000)
    (setup-lighting app)
    (setup-workshop-gui app)
    (Mouse:setGrabbed #f)
    (*:setDragToRotate fly-cam #t)
    (*:setMoveSpeed fly-cam 800)
    (set-key! app skybox: default-sky)
    (set-key! app focus-object: "Earth")))

;;; ---------------------------------------------------------------------
;;; create the workshop
;;; ---------------------------------------------------------------------

(define (make-workshop)
  (let* ((shop (FabricWorkshop))
         (settings::AppSettings (get-key shop app-settings:)))
    (set-key! shop application-init: init-workshop)
    (*:setResolution settings 1920 1200)
    (*:setTitle settings "The Fabric")
    (*:setSettingsDialogImage settings "Interface/icon.jpg")
    (*:setSettings shop settings)
    (*:setDisplayFps shop #t) ; #t to show FPS
    (*:setShowSettings shop #t) ; #t to show settings dialog
    (*:setDisplayStatView shop #t) ; #t to show stats
    (*:setPauseOnLostFocus shop #f)
    shop))
