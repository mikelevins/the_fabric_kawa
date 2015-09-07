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

(require "util-java.scm")
(require "util-error.scm")
(require "client-class.scm")
(require "model-character.scm")
(require "model-rect.scm")
(require "data-names.scm")
(require "state-create-character.scm")

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
         (name-strings (fabric-name-strings fname))
         (indexes (iota 8))
         (index-strings (map number->string indexes))
         (uids (map (lambda (i)(format #f "name field ~a" i))
                    indexes))
         (positions (map (lambda (i)(Vector2f (+ 16 (* i 200)) 48.0))
                         indexes))
         (size (Vector2f 190.0 24.0))
         (fields (map (lambda (i)(Label screen (list-ref uids i)(list-ref positions i) size))
                      indexes))
         (win (Window screen "NameGenerator"
                      (Vector2f (get-left rect) (get-top rect))
                      (Vector2f (get-width rect) (get-height rect)))))
    (for-each (lambda (i)
                (let ((f::Label (list-ref fields i)))
                  (*:setText f (list-ref name-strings i))
                  (*:setTextAlign f Align:Left)
                  (*:setFont f "Interface/Fonts/Laconic24.fnt")
                  (*:setFontSize f 24)
                  (*:setFontColor f ColorRGBA:Green)
                  (*:addChild win f)))
              indexes)
    (*:setWindowTitle win "Pick a Fabric name:")
    win))
