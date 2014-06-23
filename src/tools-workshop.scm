;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          tools-workshop.scm
;;;; Project:       The Fabric: a far-future MMORPG
;;;; Purpose:       workshop for tinkering with content
;;;; Author:        mikel evins
;;;; Copyright:     2014 by mikel evins
;;;;
;;;; ***********************************************************************

(module-export make-workshop)

;;; ---------------------------------------------------------------------
;;; required modules
;;; ---------------------------------------------------------------------

(require 'list-lib)
(require 'srfi-95) ; sorting

(require "util-java.scm")
(require "util-random.scm")
(require "util-general.scm")
(require "util-lists.scm")
(require "model-id.scm")
(require "language-types.scm")
(require "language-gf.scm")
(require "model-frame.scm")
(require "model-namegen-domains.scm")
(require "model-namegen.scm")
(require "model-kind.scm")
(require "model-entity.scm")
(require "assets-general.scm")
(require "view-controls.scm")
(require "view-colors.scm")
(require "view-shapes.scm")
(require "view-plasma.scm")
(require "view-name-cube.scm")
(require "view-player.scm")
(require "view-node.scm")

;;; ---------------------------------------------------------------------
;;; Java imports
;;; ---------------------------------------------------------------------

(define-private-alias ActionListener com.jme3.input.controls.ActionListener)
(define-private-alias AnalogListener com.jme3.input.controls.AnalogListener)
(define-private-alias AppSettings com.jme3.system.AppSettings)
(define-private-alias AssetManager com.jme3.asset.AssetManager)
(define-private-alias BitmapFont com.jme3.font.BitmapFont)
(define-private-alias BitmapText com.jme3.font.BitmapText)
(define-private-alias BloomFilter com.jme3.post.filters.BloomFilter)
(define-private-alias ColorRGBA com.jme3.math.ColorRGBA)
(define-private-alias Container com.simsilica.lemur.Container)
(define-private-alias FilterPostProcessor com.jme3.post.FilterPostProcessor)
(define-private-alias GuiGlobals com.simsilica.lemur.GuiGlobals)
(define-private-alias KeyInput com.jme3.input.KeyInput)
(define-private-alias KeyTrigger com.jme3.input.controls.KeyTrigger)
(define-private-alias Label com.simsilica.lemur.Label)
(define-private-alias TLabel tonegod.gui.controls.text.Label)
(define-private-alias Mouse org.lwjgl.input.Mouse)
(define-private-alias MouseAxisTrigger com.jme3.input.controls.MouseAxisTrigger)
(define-private-alias MouseButtonTrigger com.jme3.input.controls.MouseButtonTrigger)
(define-private-alias MouseInput com.jme3.input.MouseInput)
(define-private-alias Node com.jme3.scene.Node)
(define-private-alias Panel com.simsilica.lemur.Panel)
(define-private-alias PI com.jme3.math.FastMath:PI)
(define-private-alias QuadBackgroundComponent com.simsilica.lemur.component.QuadBackgroundComponent)
(define-private-alias Quaternion com.jme3.math.Quaternion)
(define-private-alias Screen tonegod.gui.core.Screen)
(define-private-alias SimpleApplication com.jme3.app.SimpleApplication)
(define-private-alias Spatial com.jme3.scene.Spatial)
(define-private-alias Styles com.simsilica.lemur.style.Styles)
(define-private-alias TbtQuadBackgroundComponent com.simsilica.lemur.component.TbtQuadBackgroundComponent)
(define-private-alias TextField com.simsilica.lemur.TextField)
(define-private-alias TTextField tonegod.gui.controls.text.TextField)
(define-private-alias Vector2f com.jme3.math.Vector2f)
(define-private-alias Vector3f com.jme3.math.Vector3f)
(define-private-alias VideoRecorderAppState com.jme3.app.state.VideoRecorderAppState)
(define-private-alias Window tonegod.gui.controls.windows.Window)

;;; ---------------------------------------------------------------------
;;; <fabric-workshop> - the workshop class
;;; ---------------------------------------------------------------------

(define-simple-class <fabric-workshop> (SimpleApplication)

  ;; slots
  ;; -------
  (player init-form: #!null)
  (player-node :: Node init-form: #!null)
  (direction ::Vector3f init-form: (Vector3f))
  (app-settings ::AppSettings init-form: (AppSettings #t))
  (left-button? init-form: #f)
  (right-button? init-form: #f)
  (player-location-hud init-form: #!null)

  ;; accessors
  ;; ---------
  ((getDirection) direction)
  ((setDirection dir) (set! direction dir))
  ((getAppSettings) app-settings)
  ((setAppSettings settings) (set! app-settings settings))
  ((getAudioRenderer) audioRenderer)
  ((getViewport) viewPort)
  ((getInputManager) inputManager)
  ((getStateManager) stateManager)
  ((getKeyInput) keyInput)
  ((getGuiFont) guiFont)
  ((getGuiNode) guiNode)
  ((getPlayer) player)
  ((setPlayer p) (set! player p))
  ((getPlayerNode) player-node)
  ((setPlayerNode n) (set! player-node n))
  ((getLeftButton) left-button?)
  ((setLeftButton down?) (set! left-button? down?))
  ((getRightButton) right-button?)
  ((setRightButton down?) (set! right-button? down?))

  ;; methods
  ;; -------
  ;; SimpleApplication implementation
  ((simpleInitApp)(init-workshop (this))))

;;; ---------------------------------------------------------------------
;;; <fabric-workshop> accessors
;;; ---------------------------------------------------------------------

(define (workshop-app-settings app ::SimpleApplication)(@ 'getAppSettings app))
(define (workshop-audio-renderer app ::SimpleApplication)(@ 'getAudioRenderer app))
(define (workshop-direction app  :: <fabric-workshop>)(@ 'getDirection app))
(define (workshop-gui-font app ::SimpleApplication)(@ 'getGuiFont app))
(define (workshop-gui-node app ::SimpleApplication)(@ 'getGuiNode app))
(define (workshop-input-manager app ::SimpleApplication)(@ 'getInputManager app))
(define (workshop-key-input app ::SimpleApplication)(@ 'getKeyInput app))
(define (workshop-player app ::SimpleApplication)(@ 'getPlayer app))
(define (workshop-player-node app ::SimpleApplication)(@ 'getPlayerNode app))
(define (workshop-state-manager app ::SimpleApplication)(@ 'getStateManager app))
(define (workshop-viewport app ::SimpleApplication)(@ 'getViewport app))

(define (fly-by-camera app ::SimpleApplication)(@ 'getFlyByCamera app))
(define (left-button? app)(@ 'getLeftButton app))
(define (right-button? app)(@ 'getRightButton app))
(define (root-node app ::SimpleApplication)(@ 'getRootNode app))

(define (set-workshop-app-settings! app ::SimpleApplication settings)(@ 'setAppSettings app settings))
(define (set-workshop-direction! app  :: <fabric-workshop> dir)(@ 'setDirection app dir))
(define (set-workshop-player! app ::SimpleApplication player)(@ 'setPlayer app player))
(define (set-workshop-player-node! app ::SimpleApplication node)(@ 'setPlayerNode app node))
(define (set-left-button! app key-pressed?)(@ 'setLeftButton app key-pressed?))
(define (set-right-button! app key-pressed?)(@ 'setRightButton app key-pressed?))


;;; ---------------------------------------------------------------------
;;; <fabric-workshop> initialization
;;; ---------------------------------------------------------------------

;;; $available-armorers
;;; ---------------------------------------------------------------------
;;; armor constructors

(define $available-armorers
  (list make-enclosing-cube
        make-enclosing-wire-cube
        make-enclosing-pyramid
        make-enclosing-sphere
        make-enclosing-wire-sphere))

;;; (make-armors n)
;;; ---------------------------------------------------------------------
;;; make N randomly-chosen armors

(define (make-armors n)
  (let ((armorers (cons make-any-plasma-generator
                        (map (lambda (a)(choose-any $available-armorers))
                             (iota n)))))
    (map (lambda (make-armor)
           (let ((armor (make-armor))
                 (rotator (any-rotator)))
             (@ 'addControl armor rotator)
             armor))
         armorers)))

;;; (assemble-player-character pc-node pc-geom pc-controls pc-armors)
;;; ---------------------------------------------------------------------
;;; assemble a player-character's node, geometry, controls, and armors

(define (assemble-player-character pc-node pc-geom pc-controls pc-armors)
  (@ 'attachChild pc-node pc-geom)
  (for-each (lambda (ctrl)
              (@ 'addControl pc-geom ctrl))
            pc-controls)
  (for-each (lambda (armor)
              (@ 'attachChild pc-node armor)
              (@ 'setLocalTranslation armor 0 0 0))
            pc-armors))


;;; (init-player-character app ::SimpleApplication)
;;; ---------------------------------------------------------------------
;;; prepare a player character and present it in the scene

(define (init-player-character app ::SimpleApplication)
  (let* ((player-node (Node "Player"))
         (player (make-player-character (any-lit-color)))
         (player-cube (get-key player name-cube:))
         (player-rotator (any-rotator))
         ;;(armor-count (random-integer 3))
         (armor-count 0))
    (set-workshop-player! app player)
    (set-workshop-player-node! app player-node)

    ;; assemble the player character's parts
    (assemble-player-character player-node
                               player-cube
                               (list player-rotator)
                               (make-armors armor-count))
    
    ;; add the player to the scene
    (@ 'attachChild (root-node app) player-node)))

(define (init-hud app ::SimpleApplication name-string)
  (let ((screen (Screen app)))
    (@ 'initialize screen)
    (@ 'addControl (workshop-gui-node app) screen)
    (let* ((Align BitmapFont:Align)
           (VAlign BitmapFont:VAlign)
           (width (@ 'getWidth (workshop-app-settings app)))
           (height (@ 'getHeight (workshop-app-settings app)))
           (nameplate (TLabel screen "nameplate"
                              (Vector2f 8 8)
                              (Vector2f 400 40)))
           (nodeplate (TLabel screen "nodeplate"
                              (Vector2f 8 48)
                              (Vector2f 400 40))))

      (@ 'setText nameplate name-string)
      (@ 'setTextAlign nameplate Align:Left)
      (@ 'setFont nameplate "Interface/Fonts/Laconic30.fnt")
      (@ 'setFontSize nameplate 30)
      (@ 'setFontColor nameplate ColorRGBA:Green)

      (@ 'setTextAlign nodeplate Align:Left)
      (@ 'setFont nodeplate "Interface/Fonts/Laconic24.fnt")
      (@ 'setFontSize nodeplate 24)
      (@ 'setFontColor nodeplate ColorRGBA:Green)

      (@ 'addElement screen nameplate)
      (@ 'addElement screen nodeplate))))


;;; (setup-lighting app ::SimpleApplication)
;;; ---------------------------------------------------------------------
;;; initialize glow and lighting effects for the scene

(define (setup-lighting app ::SimpleApplication)
  (let* ((asset-manager::AssetManager (get-asset-manager))
         (bloom (BloomFilter BloomFilter:GlowMode:Objects))
         (filter-processor::FilterPostProcessor (FilterPostProcessor asset-manager)))
    (@ 'setDownSamplingFactor bloom 2.0)
    (@ 'setBloomIntensity bloom 2.0)
    (@ 'addFilter filter-processor bloom)
    (@ 'addProcessor (workshop-viewport app) filter-processor)))


;;; (init-workshop app)
;;; ---------------------------------------------------------------------
;;; set up the scene and add the player character

(define (init-workshop app)
  (let* ((fly-cam (fly-by-camera app)))
    (setup-lighting app)
    (init-player-character app)

    (let* ((player (workshop-player app))
           (name-strings (fabric-name-strings (get-key player name: "")))
           (player-name (call-with-output-string
                         (lambda (out)
                           (for-each (lambda (s)
                                       (format out "~a " s))
                                     name-strings)))))
      (init-hud app player-name)
      (@ 'setEnabled fly-cam #t)
      (@ 'setMoveSpeed fly-cam 30)
      (@ 'setRotationSpeed fly-cam 2)
      (@ 'setDragToRotate fly-cam #t)
      (Mouse:setGrabbed #f))

    ;; uncomment to capture video to a file
    ;; (@ 'attach (workshop-state-manager app) (VideoRecorderAppState))
    #!void))


;;; ---------------------------------------------------------------------
;;; <fabric-workshop> event-handler functions
;;; ---------------------------------------------------------------------

(define-syntax on-analog
  (syntax-rules (->)
    ((on-analog (evt-name)
                (s -> expr) ...)
     (cond
      ((invoke evt-name 'equals s) expr) ...
      (#t #f)))))


;;; ---------------------------------------------------------------------
;;; construct the workshop
;;; ---------------------------------------------------------------------

;;; (make-workshop)
;;; ---------------------------------------------------------------------
;;; puts everything together into a runnable workshop

(define (make-workshop)
  (let* ((workshop :: <fabric-workshop> (<fabric-workshop>))
	 (settings::AppSettings (invoke workshop 'getAppSettings)))

    (@ 'setResolution settings 1400 1024)
    (@ 'setTitle settings "The Fabric")
    (@ 'setSettingsDialogImage settings "Interface/icon.jpg")
    (@ 'setSettings workshop settings)
    (@ 'setDisplayFps workshop #t) ; #t to show FPS
    (@ 'setShowSettings workshop #f) ; #t to show settings dialog
    (@ 'setDisplayStatView workshop #t) ; #t to show stats
    (@ 'setPauseOnLostFocus workshop #f)
    workshop))

;;; (define $the-workshop (make-workshop))
;;; (invoke $the-workshop 'start)
