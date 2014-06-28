;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          app-common.scm
;;;; Project:       The Fabric: a far-future MMORPG
;;;; Purpose:       definitions shared by client and workshop
;;;; Author:        mikel evins
;;;; Copyright:     2014 by mikel evins
;;;;
;;;; ***********************************************************************

(module-export FabricApp app-settings)

;;; ---------------------------------------------------------------------
;;; required modules
;;; ---------------------------------------------------------------------

(require 'list-lib)
(require "util-java.scm")
(require "util-general.scm")
(require "util-random.scm")
(require "assets-general.scm")
(require "model-frame.scm")
(require "model-namegen.scm")
(require "net-messaging.scm")
(require "util-lists.scm")
(require "view-shapes.scm")
(require "view-controls.scm")
(require "view-colors.scm")
(require "view-plasma.scm")
(require "view-player.scm")
(require "init-config.scm")
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
(define-private-alias CameraControl com.jme3.scene.control.CameraControl)
(define-private-alias CameraNode com.jme3.scene.CameraNode)
(define-private-alias Client com.jme3.network.Client)
(define-private-alias ChatBox tonegod.gui.controls.extras.ChatBox)
(define-private-alias ColorRGBA com.jme3.math.ColorRGBA)
(define-private-alias ConnectException java.net.ConnectException)
(define-private-alias Container com.simsilica.lemur.Container)
(define-private-alias EffectEvent tonegod.gui.effects.Effect:EffectEvent)
(define-private-alias FilterPostProcessor com.jme3.post.FilterPostProcessor)
(define-private-alias GuiGlobals com.simsilica.lemur.GuiGlobals)
(define-private-alias KeyInput com.jme3.input.KeyInput)
(define-private-alias KeyTrigger com.jme3.input.controls.KeyTrigger)
(define-private-alias Label com.simsilica.lemur.Label)
(define-private-alias TLabel tonegod.gui.controls.text.Label)
(define-private-alias MessageListener com.jme3.network.MessageListener)
(define-private-alias Mouse org.lwjgl.input.Mouse)
(define-private-alias MouseAxisTrigger com.jme3.input.controls.MouseAxisTrigger)
(define-private-alias MouseButtonTrigger com.jme3.input.controls.MouseButtonTrigger)
(define-private-alias MouseInput com.jme3.input.MouseInput)
(define-private-alias Network com.jme3.network.Network)
(define-private-alias Node com.jme3.scene.Node)
(define-private-alias Panel com.simsilica.lemur.Panel)
(define-private-alias PI com.jme3.math.FastMath:PI)
(define-private-alias QuadBackgroundComponent com.simsilica.lemur.component.QuadBackgroundComponent)
(define-private-alias Quaternion com.jme3.math.Quaternion)
(define-private-alias Screen tonegod.gui.core.Screen)
(define-private-alias Serializable com.jme3.network.serializing.Serializable)
(define-private-alias Serializer com.jme3.network.serializing.Serializer)
(define-private-alias SimpleApplication com.jme3.app.SimpleApplication)
(define-private-alias Spatial com.jme3.scene.Spatial)
(define-private-alias String java.lang.String)
(define-private-alias Styles com.simsilica.lemur.style.Styles)
(define-private-alias TbtQuadBackgroundComponent com.simsilica.lemur.component.TbtQuadBackgroundComponent)
(define-private-alias TextField com.simsilica.lemur.TextField)
(define-private-alias TTextField tonegod.gui.controls.text.TextField)
(define-private-alias Vector2f com.jme3.math.Vector2f)
(define-private-alias Vector3f com.jme3.math.Vector3f)
(define-private-alias VideoRecorderAppState com.jme3.app.state.VideoRecorderAppState)
(define-private-alias Window tonegod.gui.controls.windows.Window)

;;; ---------------------------------------------------------------------
;;; FabricApp - the abstract client application class
;;; ---------------------------------------------------------------------

(define-simple-class FabricApp (SimpleApplication AnalogListener ActionListener)
  ;; slots
  ;; -------
  (app-settings init-form: (AppSettings #t))

  ;; accessors
  ;; ---------
  ((getAppSettings) app-settings)

  ;; implementation methods
  ;; ---------

  ;; AnalogListener and ActionListener implementation
  ((onAnalog name value tpf) #!abstract)
  ((onAction name key-pressed? tpf) #!abstract)
  
  ;; SimpleApplication implementation
  ((simpleInitApp) #!abstract))

;;; ---------------------------------------------------------------------
;;; accessor functions
;;; ---------------------------------------------------------------------

(defgetter (app-settings FabricApp) getAppSettings)

