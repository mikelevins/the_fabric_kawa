;;;; ***********************************************************************
;;;; Name:          application-client.scm
;;;; Project:       The Fabric: a far-future MMORPG
;;;; Purpose:       the game client
;;;; Author:        mikel evins
;;;; Copyright:     2014 by mikel evins
;;;;
;;;; ***********************************************************************

(module-export FabricClient make-client)

;;; ---------------------------------------------------------------------
;;; required modules
;;; ---------------------------------------------------------------------

(require 'list-lib)
(require "utilities-math.scm")
(require "utilities-java.scm")
(require "setting-hubs.scm")
(require "setting-sky.scm")
(require "setting-scene.scm")
(require "setting-lighting.scm")
(require "setting-celestial-objects.scm")
(require "interface-frame.scm")
(require "assets-general.scm")
(require "controls-client-camera.scm")
(require "controls-client-inputs.scm")
(require "ui-client-hud.scm")
(require "ui-client-chat.scm")
(require "application-common.scm")

;;; ---------------------------------------------------------------------
;;; Java imports
;;; ---------------------------------------------------------------------

(define-private-alias ActionListener com.jme3.input.controls.ActionListener)
(define-private-alias AnalogListener com.jme3.input.controls.AnalogListener)
(define-private-alias AppSettings com.jme3.system.AppSettings)
(define-private-alias HashPMap org.pcollections.HashPMap)
(define-private-alias Node com.jme3.scene.Node)
(define-private-alias Vector2f com.jme3.math.Vector2f)
(define-private-alias Vector3f com.jme3.math.Vector3f)

;;; ---------------------------------------------------------------------
;;; initialize the client
;;; ---------------------------------------------------------------------

(define (init-client app)
  (init-scene app)
  (init-lighting app)
  (init-player app)
  (init-camera app (get-key app player:))
  (init-client-hud app)
  (init-client-chat app)
  (init-inputs app))

;;; ---------------------------------------------------------------------
;;; FabricClient - the application class
;;; ---------------------------------------------------------------------

;;; slot-accessor functions
;;; -----------------------

(define (%set-client-hub! app::FabricApp key hub)
  (let* ((root::Node (get-key app root-node:))
         (already (get-key app hub:))
         (old-slots::HashPMap (*:getSlots app)))
    (unless (absent? already)
      (*:detachChild root already))
    (*:setSlots app (*:plus old-slots hub: hub))
    (*:attachChild root hub)))

(define (%set-client-inputs! app::FabricApp key input-specs)
  (let ((mgr (get-key app input-manager:)))
    (for-each (lambda (spec)
                (let ((name (input-spec-name spec))
                      (signal (input-spec-signal spec)))
                  (*:addMapping mgr name signal)
                  (*:addListener mgr app name)))
              input-specs)))

(define (%set-outbound-chat-message! app::FabricApp key message)
  (error "Sending chat messages not yet implemented"))

;;; class
;;; -----

(define-simple-class FabricClient (FabricApp AnalogListener ActionListener)
  ;; slots
  (direction ::Vector3f init-form: (Vector3f))
  ((getDirection) direction)
  ((setDirection dir) (set! direction dir))

  (left-button? init-form: #f)
  ((getLeftButton) left-button?)
  ((setLeftButton down?) (set! left-button? down?))

  (right-button? init-form: #f)
  ((getRightButton) right-button?)
  ((setRightButton down?) (set! right-button? down?))

  ;; AnalogListener and ActionListener implementation
  ((onAnalog name value tpf)(handle-client-analog-event (this) name value tpf))
  ((onAction name key-pressed? tpf)(handle-action-event (this) name key-pressed? tpf))

  ;; instance initializer
  (init: (begin
           ;; set client slots
           (set-key! (this) application-init: init-client)
           
           ;; slot getters
           (set-slot-getter! (this) direction: (lambda (app key)(*:getDirection app)))
           (set-slot-getter! (this) key-input: (lambda (app key)(*:getKeyInput app)))
           (set-slot-getter! (this) input-manager: (lambda (app key)(*:getInputManager app)))
           (set-slot-getter! (this) left-button: (lambda (app key)(*:getLeftButton app)))
           (set-slot-getter! (this) right-button: (lambda (app key)(*:getRightButton app)))

           ;; slot setters
           (set-slot-setter! (this) direction: (lambda (app key val)(*:setDirection app val)))
           (set-slot-setter! (this) hub: %set-client-hub!)
           (set-slot-setter! (this) inputs: %set-client-inputs!)
           (set-slot-setter! (this) left-button: (lambda (app key val)(*:setLeftButton app val)))
           (set-slot-setter! (this) right-button: (lambda (app key val)(*:setRightButton app val)))
           (set-slot-setter! (this) chat-message: %set-outbound-chat-message!))))


;;; ---------------------------------------------------------------------
;;; create the client
;;; ---------------------------------------------------------------------

(define (make-client #!optional (hub-name "Earth"))
  (let* ((client (FabricClient))
         (settings::AppSettings (get-key client settings:)))
    (set-key! client hub-name: hub-name)
    (set-key! client character-name: "J. Random Character")
    ;; set JME3 settings
    (*:setResolution settings 1920 1200)
    (*:setTitle settings "The Fabric")
    (*:setSettingsDialogImage settings "Interface/icon.jpg")
    (*:setSettings client settings)
    (*:setDisplayFps client #f) ; #t to show FPS
    (*:setShowSettings client #t) ; #t to show settings dialog
    (*:setDisplayStatView client #f) ; #t to show stats
    (*:setPauseOnLostFocus client #f)
    client))
