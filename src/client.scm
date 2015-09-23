;;;; ***********************************************************************
;;;;
;;;; Name:          client.scm
;;;; Project:       The Fabric: a far-future MMORPG
;;;; Purpose:       the Fabric client
;;;; Author:        mikel evins
;;;; Copyright:     2015 by mikel evins
;;;;
;;;; ***********************************************************************

(module-export
 FabricClient
 activate-state
 start-client
 stop-client)



;;; ---------------------------------------------------------------------
;;; required modules
;;; ---------------------------------------------------------------------

(require util-error)
(require data-assets)
(require state)
(require state-login)
(require state-create-character)
(require state-pick-character)
(require state-play)
(require state-transition)

;;; ---------------------------------------------------------------------
;;; Java imports
;;; ---------------------------------------------------------------------

(import (class com.jme3.app SimpleApplication))
(import (class com.jme3.app.state AbstractAppState AppStateManager))
(import (class com.jme3.input.controls ActionListener AnalogListener
          KeyTrigger MouseAxisTrigger MouseButtonTrigger))
(import (class com.jme3.math Vector3f))
(import (class gnu.mapping Symbol))
(import (class tonegod.gui.core Screen))

;;; ---------------------------------------------------------------------
;;; FabricClient
;;; ---------------------------------------------------------------------

(define-variable $client #!null)

(define-simple-class FabricClient (SimpleApplication AnalogListener ActionListener)
  ;; slots
  (app-settings init: #!null)
  (state init: #!null)
  (user init: #!null)
  (screen init: #!null)
  (speed init: #!null)
  (direction type: Vector3f init-form: (Vector3f))
  (left-button? init-form: #f)
  (right-button? init-form: #f)
  ;; accessors
  ((getKeyInput) keyInput)
  ((getCameraDirection) (*:getDirection cam))
  ((getViewport) viewPort)
  ;; event handlers
  ((onAnalog name value tpf)(*:handleAnalogEvent (as FabricClientState state) name value tpf))
  ((onAction name key-pressed? tpf)(*:handleActionEvent (as FabricClientState state) name key-pressed? tpf))
  ;; init
  ((simpleInitApp) (init-app (this))))

(define (init-app app::FabricClient)
  (begin (*:setEnabled (*:getFlyByCamera app) #f)
         (set! app:screen (Screen app))
         (activate-state app 'transition)
         #!void))

(define (start-client)
  (set! $client (FabricClient))
  (*:start (as FabricClient $client)))

(define (stop-client)
  (*:stop (as FabricClient $client)))

;;; ---------------------------------------------------------------------
;;; state changes
;;; ---------------------------------------------------------------------

(define (activate-state client::FabricClient state-name::Symbol)
  (let ((new-state (case state-name
                     ((login)(LoginState))
                     ((create-character)(CreateCharacterState))
                     ((pick-character)(PickCharacterState))
                     ((play)(PlayState))
                     ((transition)(TransitionState))
                     (else (error "Unknown state name: " state-name)))))
    (let* ((mgr::AppStateManager (*:getStateManager client))
           (current-state client:state))
      (unless (eqv? #!null current-state)
        (*:detach mgr current-state))
      (*:attach mgr new-state)
      (*:initialize (as FabricClientState new-state) mgr client))))
