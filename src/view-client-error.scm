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
 client-warn
 compute-client-warning-rect
 compute-error-cancel-button-rect
 compute-error-okay-button-rect
 compute-warning-button-rect
 compute-warning-label-rect
 )

(require client)
(require model-rect)
(require view-warning-button)

(import (class com.jme3.font BitmapFont))
(import (class com.jme3.math Vector2f))
(import (class tonegod.gui.controls.windows Panel))
(import (class tonegod.gui.controls.text Label))
(import (class tonegod.gui.core Screen))


;;; ---------------------------------------------------------------------
;;; warning and error dialogs
;;; ---------------------------------------------------------------------

(define (compute-client-warning-rect screen::Screen)
  (let* ((screen-width (*:getWidth screen))
         (screen-height (*:getHeight screen))
         (width 512)
         (height 256)
         (left (- (/ screen-width 2)
                  (/ width 2)))
         (top (- (/ screen-height 2)
                 (/ height 2))))
    (make-rectangle left top width height)))

(define (compute-warning-button-rect rect)
  (let* ((width 128)
         (height 40)
         (left (- (/ (get-width rect) 2)
                  (/ width 2)))
         (top (- (get-height rect)
                 (+ height 16))))
    (make-rectangle left top width height)))

(define (compute-warning-label-rect rect)
  (let* ((left 16)
         (top 16)
         (width (- (get-width rect)
                   (* 2 left)))
         (height  (- (get-height rect)
                     (* 2 top))))
    (make-rectangle left top width height)))

(define (client-warn message::String)
  (let* ((client::FabricClient (the-client))
         (screen::Screen client:screen)
         (align BitmapFont:Align)
         (panel-rect (compute-client-warning-rect screen))
         (panel (Panel screen "WarningDialog"
                       (Vector2f (get-left panel-rect)(get-top panel-rect))
                       (Vector2f (get-width panel-rect)(get-height panel-rect))))
         (button-rect (compute-warning-button-rect panel-rect))
         (button (WarningDialogButton client screen panel "Okay"
                                      (Vector2f (get-left button-rect)(get-top button-rect))
                                      (Vector2f (get-width button-rect)(get-height button-rect))))
         (label-rect (compute-warning-label-rect panel-rect))
         (label (Label screen "WarningLabel"
                       (Vector2f (get-left label-rect)(get-top label-rect))
                       (Vector2f (get-width label-rect)(get-height label-rect)))))
    (*:setText label message)
    (*:setTextAlign label align:Center)
    (*:addChild panel label)
    (*:addChild panel button)
    (*:enqueue client (runnable (lambda ()(*:addElement screen panel))))))

(define (compute-error-okay-button-rect rect)
  (let* ((width 128)
         (height 40)
         (left (- (/ (get-width rect) 2)
                  (/ width 2)))
         (top (- (get-height rect)
                 (+ height 16))))
    (make-rectangle left top width height)))

(define (compute-error-cancel-button-rect rect)
  (let* ((width 128)
         (height 40)
         (left (- (/ (get-width rect) 2)
                  (/ width 2)))
         (top (- (get-height rect)
                 (+ height 16))))
    (make-rectangle left top width height)))
