;;;; ***********************************************************************
;;;;
;;;; Name:          client-class.scm
;;;; Project:       The Fabric: a far-future MMORPG
;;;; Purpose:       The FabricClient class
;;;; Author:        mikel evins
;;;; Copyright:     2015 by mikel evins
;;;;
;;;; ***********************************************************************

(module-export
 client-set-login-state!
 client-set-create-character-state!
 client-set-pick-character-state!
 client-set-play-state!
 client-transit-to!
 FabricClient
 make-client
 move-node!
 move-node-backward!
 move-node-forward!
 move-node-left!
 move-node-right!
 normalize-camera!
 rotate-node-down!
 rotate-node-left!
 rotate-node-right!
 rotate-node-up!)

;;; ---------------------------------------------------------------------
;;; required modules
;;; ---------------------------------------------------------------------

(require util-error)
(require util-java)
(require state-login)
(require state-create-character)
(require state-pick-character)
(require state-play)
(require model-character)
(require state-transit)
(require client-state)

;;; ---------------------------------------------------------------------
;;; Java imports
;;; ---------------------------------------------------------------------

(import-as ActionListener com.jme3.input.controls.ActionListener)
(import-as AnalogListener com.jme3.input.controls.AnalogListener)
(import-as AppSettings com.jme3.system.AppSettings)
(import-as Mouse org.lwjgl.input.Mouse)
(import-as Node com.jme3.scene.Node)
(import-as Screen tonegod.gui.core.Screen)
(import-as SimpleApplication com.jme3.app.SimpleApplication)
(import-as Thread java.lang.Thread)
(import-as Vector3f com.jme3.math.Vector3f)

;;; ---------------------------------------------------------------------
;;; FabricClient
;;; ---------------------------------------------------------------------

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
  ;; event handlers
  ((onAnalog name value tpf)(*:handleAnalogEvent (as FabricClientState state) name value tpf))
  ((onAction name key-pressed? tpf)(*:handleActionEvent (as FabricClientState state) name key-pressed? tpf))
  ;; init
  ((simpleInitApp) (init-app (this))))

(define (init-app app::FabricClient)
  (begin (*:setEnabled (*:getFlyByCamera app) #f)
         (set! app:screen (Screen app))
         #!void))

;;; ---------------------------------------------------------------------
;;; construct the client app
;;; ---------------------------------------------------------------------

;;; (make-client #!key client settings screen-width screen-height title
;;;                    settings-image show-fps show-settings
;;;                    show-statistics pause-on-lost-focus grab-mouse)
;;; ---------------------------------------------------------------------
;;; returns a newly-created and -configured instance of the
;;; client application. The game can be started by calling
;;; the new client application's start method.

(define (make-client #!key
                     (client::FabricClient (FabricClient))
                     (settings::AppSettings (AppSettings #t))
                     (screen-width 1920)
                     (screen-height 1200)
                     (title "The Fabric")
                     (settings-image "Interface/icon.jpg")
                     (show-fps #f)
                     (show-settings #t)
                     (show-statistics #f)
                     (pause-on-lost-focus #f)
                     (grab-mouse #f))
  (*:setResolution settings screen-width screen-height)
  (*:setTitle settings title)
  (*:setSettingsDialogImage settings settings-image)
  (*:setSettings client settings)
  (*:setDisplayFps client show-fps)
  (*:setShowSettings client show-settings)
  (*:setDisplayStatView client show-statistics)
  (*:setPauseOnLostFocus client pause-on-lost-focus)
  (Mouse:setGrabbed grab-mouse)
  client)


;;; PRIVATE
;;; ---------------------------------------------------------------------
;;; these functions are private and are not thread-safe; do not call
;;; them directly; rely on enqueue-state-change
;;; ---------------------------------------------------------------------
;;; %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(define (%detach-and-cleanup-current-state! client::FabricClient)
  (let ((current-state client:state)
        (mgr (*:getStateManager client)))
    (unless (jnull? current-state)
      (*:detach mgr current-state)
      (set! client:state #!null))))

(define (%attach-and-activate-new-state! client::FabricClient new-state)
  (let ((mgr (*:getStateManager client)))
    (set! client:state new-state)
    (*:attach mgr new-state)))

(define (%update-client-state! client::FabricClient new-state)
  (unless (equal? new-state client:state)
    (%detach-and-cleanup-current-state! client)
    (%attach-and-activate-new-state! client new-state)))

;;; %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
;;; ---------------------------------------------------------------------

;;; ---------------------------------------------------------------------
;;; set the client's main state
;;; ---------------------------------------------------------------------

(define (%enqueue-state-change client::FabricClient new-state)
  (*:enqueue client
             (runnable (lambda ()
                         (%update-client-state! client new-state)))))

(define (client-set-login-state! client::FabricClient)
  (%enqueue-state-change client (make-login-state client)))

(define (client-set-create-character-state! client::FabricClient character::FabricCharacter)
  (%enqueue-state-change client (make-create-character-state client character)))

(define (client-set-pick-character-state! client::FabricClient)
  (%enqueue-state-change client (make-pick-character-state client)))

(define (client-set-play-state! client::FabricClient character::FabricCharacter #!optional (node-name "The Sun"))
  (%enqueue-state-change client (make-play-state client character node-name)))

(define (client-transit-to! client::FabricClient character::FabricCharacter destination::String)
  (%enqueue-state-change client (make-transit-state client))
  (Thread:sleep 500)
  (%enqueue-state-change client (make-play-state client character destination)))

;;; ---------------------------------------------------------------------
;;; node and camera movement
;;; ---------------------------------------------------------------------

;;; (normalize-camera! app :: FabricClient)
;;; ---------------------------------------------------------------------
;;; orients the camera to where the player's node is facing

(define (normalize-camera! app :: FabricClient)
  (let ((dir :: Vector3f (*:getCameraDirection app)))
    (*:normalizeLocal dir)))


;;; (move-node!  app :: FabricClient node :: Node amount :: float invert?)
;;; ---------------------------------------------------------------------
;;; moves  _node_  a distance along an arbitrary vector.
;;; used by more specific move- functions like move-node-forward!

(define (move-node!  app :: FabricClient node :: Node amount :: float invert?)
  (let ((dir :: Vector3f app:direction)
        (sign (if invert? -1 1)))
    (*:multLocal dir (* sign amount))
    (*:move node dir)))


;;; (move-node-forward! app :: FabricClient node :: Node amount :: float)
;;; ---------------------------------------------------------------------
;;; moves _node_ forward a distance of _amount_

(define (move-node-forward! app :: FabricClient node :: Node amount :: float)
  (normalize-camera! app)
  (set! app:direction (*:getCameraDirection app))
  (move-node! app node amount #f))


;;; (move-node-backward! app :: FabricClient node :: Node amount :: float)
;;; ---------------------------------------------------------------------
;;; moves _node_ backward a distance of _amount_

(define (move-node-backward! app :: FabricClient node :: Node amount :: float)
  (normalize-camera! app)
  (set! app:direction (*:getCameraDirection app))
  (move-node! app node amount #t))


;;; (move-node-left! app :: FabricClient node :: Node amount :: float)
;;; ---------------------------------------------------------------------
;;; moves _node_ to the left a distance of _amount_

(define (move-node-left! app :: FabricClient node :: Node amount :: float)
  (set! app:direction (*:normalizeLocal (*:getLeft (*:getCamera app))))
  (move-node! app node amount #f))


;;; (move-node-right! app :: FabricClient node :: Node amount :: float)
;;; ---------------------------------------------------------------------
;;; moves _node_ to the right a distance of _amount_

(define (move-node-right! app :: FabricClient node :: Node amount :: float)
  (set! app:direction (*:normalizeLocal (*:getLeft (*:getCamera app))))
  (move-node! app node amount #t))


;;; (rotate-node-right! node :: Node amount :: float)
;;; ---------------------------------------------------------------------
;;; rotates _node_ to the right an angle of _amount_ 

(define (rotate-node-right! node :: Node amount :: float)
  (*:rotate node 0 (* -1 amount) 0))


;;; (rotate-node-left! node :: Node amount :: float)
;;; ---------------------------------------------------------------------
;;; rotates _node_ to the left an angle of _amount_ 

(define (rotate-node-left! node :: Node amount :: float)
  (*:rotate node 0 amount 0))


;;; (rotate-node-up! node :: Node amount :: float)
;;; ---------------------------------------------------------------------
;;; rotates _node_ upward an angle of _amount_ 

(define (rotate-node-up! node :: Node amount :: float)
  (*:rotate node (* -1 amount) 0 0))


;;; (rotate-node-down! node :: Node amount :: float)
;;; ---------------------------------------------------------------------
;;; rotates _node_ downward an angle of _amount_ 

(define (rotate-node-down! node :: Node amount :: float)
  (*:rotate node amount 0 0))
