;;;; ***********************************************************************
;;;;
;;;; Name:          view-character-picker.scm
;;;; Project:       The Fabric: a far-future MMORPG
;;;; Purpose:       the character picker 
;;;; Author:        mikel evins
;;;; Copyright:     2015 by mikel evins
;;;;
;;;; ***********************************************************************

(module-export
 compute-character-picker-rect
 make-character-picker)

;;; ---------------------------------------------------------------------
;;; required modules
;;; ---------------------------------------------------------------------

(require 'list-lib)
(require client)
(require state)
(require state-pick-character)
(require model-namegen)
(require model-rect)
(require model-user)
(require view-character-picker-group)
(require view-character-picker-button)

;;; ---------------------------------------------------------------------
;;; Java imports
;;; ---------------------------------------------------------------------

(import (class com.jme3.font BitmapFont))
(import (class com.jme3.math ColorRGBA Vector2f))
(import (class tonegod.gui.controls.buttons Button RadioButton))
(import (class tonegod.gui.controls.windows Window))
(import (class tonegod.gui.core Screen))

;;; ---------------------------------------------------------------------
;;; 
;;; ---------------------------------------------------------------------

(define (compute-character-picker-rect screen::Screen)
  (let* ((screen-height (*:getHeight screen)))
    (make-rectangle 16 16 512 (- screen-height 32))))

(define (compute-character-picker-button-origin screen i)
  (Vector2f 32 (+ 64 (* i (+ 32 40)))))

(define (compute-character-picker-button-size screen)
  (Vector2f 16 16))

(define (make-character-picker-buttons state::PickCharacterState screen::Screen)
  (let* ((align BitmapFont:Align)
         (client::FabricClient (the-client))
         (user::FabricUser (the-user))
         (characters (if (eqv? #!null user)
                         '()
                         user:characters)))
    (map (lambda (char::FabricCharacter i)
           (let* ((name::FabricName char:name)
                  (namestring::String (fabric-name->string name))
                  (btn::CharacterPickerButton
                   (CharacterPickerButton state char
                                          screen (format #f "~A Button" namestring)
                                          (compute-character-picker-button-origin screen i)
                                          (compute-character-picker-button-size screen))))
             (*:setLabelText btn namestring)
             (*:setTextAlign btn align:Right)
             (*:setButtonIcon btn 32 32 #!null)
             btn))
         characters
         (iota (length characters)))))

(define (make-character-picker state::PickCharacterState screen::Screen)
  (let* ((rect (compute-character-picker-rect screen))
         (align BitmapFont:Align)
         (valign BitmapFont:VAlign)
         (win::Window (Window screen "CharacterPicker"
                              (Vector2f (get-left rect)(get-top rect))
                              (Vector2f (get-width rect)(get-height rect))))
         (character-group::CharacterPickerGroup (CharacterPickerGroup state screen "CharacterGroup"))
         (picker-buttons (make-character-picker-buttons state screen)))
    (for-each (lambda (pb)
                (add-character-picker-button! state pb)
                (*:addButton character-group pb)
                (*:addChild win pb))
              picker-buttons)
    (set! character-group:state state)
    (*:setWindowTitle win "Pick a character")
    win))
