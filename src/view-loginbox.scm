;;;; ***********************************************************************
;;;;
;;;; Name:          view-loginbox.scm
;;;; Project:       The Fabric: a far-future MMORPG
;;;; Purpose:       the login dialog 
;;;; Author:        mikel evins
;;;; Copyright:     2014 by mikel evins
;;;;
;;;; ***********************************************************************

(module-export
 FabricLoginBox
 compute-login-box-rect
 make-loginbox)

;;; ---------------------------------------------------------------------
;;; required modules
;;; ---------------------------------------------------------------------

(require util-crypt)
(require data-users)
(require model-user)
(require model-rect)
(require state)
(require client)
(require view-alert)

;;; ---------------------------------------------------------------------
;;; Java imports
;;; ---------------------------------------------------------------------

(import (class java.lang String))
(import (class com.jme3.input.event MouseButtonEvent))
(import (class com.jme3.math Vector2f))
(import (class tonegod.gui.controls.windows LoginBox))
(import (class tonegod.gui.core Screen))

;;; CLASS FabricLoginBox
;;; ---------------------------------------------------------------------
;;; a LoginBox subclass that presents a form that enables players to
;;; log in to the remote Fabric server in order to play

(define-simple-class FabricLoginBox (LoginBox)
  ((*init* screen::Screen uid::String position::Vector2f size::Vector2f)
   (invoke-special LoginBox (this) '*init* screen uid position size))
  ((onButtonLoginPressed evt::MouseButtonEvent toggle::boolean)
   ;;; TODO: make this button log in to the server
   (let* ((username::String (*:getTextUserName (this)))
          (user::FabricUser (get-user username))
          (client::FabricClient (the-client))
          (screen::Screen client:screen))
     (if (eqv? #!null user)
         (alert screen (format #f "No such user ~A" username))
         (let* ((pw::String (*:getTextPassword (this)))
                (pwdigest (text->digest pw (compute-random-salt)))
                (pwhash (car pwdigest))
                (pwsalt (cdr pwdigest)))
           (set! client:current-user user)
           (set! client:username username)
           (set! client:password-hash pwhash)
           (set! client:password-salt pwsalt)
           (activate-pick-character-state)))))
  ((onButtonCancelPressed evt::MouseButtonEvent toggle::boolean)
   (*:stop app)))

(define (compute-login-box-rect screen::Screen)
  (let* ((screen-width screen:width)
         (screen-height screen:height)
         (box-width 700)
         (box-height 300)
         (box-left (- (/ screen-width 2)
                      (/ box-width 2)))
         (box-top (- (/ screen-height 2)
                     (/ box-height 2))))
    (make-rectangle box-left
                    box-top
                    box-width
                    box-height)))

(define (make-loginbox state::FabricClientState)
  (let* ((client::FabricClient (the-client))
         (screen::Screen client:screen)
         (rect (compute-login-box-rect screen))
         (box (FabricLoginBox screen "LoginBox"
                              (Vector2f (get-left rect) (get-top rect))
                              (Vector2f (get-width rect) (get-height rect)))))
    box))
