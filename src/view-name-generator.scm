;;;; ***********************************************************************
;;;;
;;;; Name:          view-name-generator.scm
;;;; Project:       The Fabric: a far-future MMORPG
;;;; Purpose:       the random name generator 
;;;; Author:        mikel evins
;;;; Copyright:     2015 by mikel evins
;;;;
;;;; ***********************************************************************

(module-export
 make-name-generator)

;;; ---------------------------------------------------------------------
;;; required modules
;;; ---------------------------------------------------------------------

(require 'list-lib)
(require util-java)
(require util-error)
(require client-class)
(require model-character)
(require model-rect)
(require data-names)
(require state-create-character)

;;; ---------------------------------------------------------------------
;;; Java imports
;;; ---------------------------------------------------------------------

(import-as BitmapFont com.jme3.font.BitmapFont)
(import-as BitmapText com.jme3.font.BitmapText)
(import-as ColorRGBA com.jme3.math.ColorRGBA)
(import-as Label tonegod.gui.controls.text.Label)
(import-as Screen tonegod.gui.core.Screen)
(import-as SelectList tonegod.gui.controls.lists.SelectList)
(import-as Vector2f com.jme3.math.Vector2f)
(import-as Window tonegod.gui.controls.windows.Window)

;;; make-name-picker
;;; ---------------------------------------------------------------------

(define (make-name-generator screen::Screen fname::FabricName)
  (let* ((Align BitmapFont:Align)
         (rect (compute-name-picker-rect screen))
         (position (Vector2f 10 48))
         (size (Vector2f 600 24.0))
         (f (Label screen "name field" position size))
         (win (Window screen "NameGenerator"
                      (Vector2f (get-left rect) (get-top rect))
                      (Vector2f (get-width rect) (get-height rect)))))
    (*:setText f "")
    (*:setTextAlign f Align:Left)
    (*:setFont f "Interface/Fonts/Laconic24.fnt")
    (*:setFontSize f 24)
    (*:setFontColor f ColorRGBA:Green)
    (*:addChild win f)
    (*:setWindowTitle win "Pick a Fabric name:")
    win))
