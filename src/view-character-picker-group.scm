;;;; ***********************************************************************
;;;;
;;;; Name:          view-character-picker-group.scm
;;;; Project:       The Fabric: a far-future MMORPG
;;;; Purpose:       radio button group for picking characters
;;;; Author:        mikel evins
;;;; Copyright:     2015 by mikel evins
;;;;
;;;; ***********************************************************************

(module-export
 CharacterPickerGroup)

(require client)
(require model-character)
(require state-pick-character)
(require view-character-model)
(require view-character-picker-button)

(import (class java.lang String))
(import (class com.jme3.math Vector3f))
(import (class com.jme3.scene Node))
(import (class tonegod.gui.controls.buttons Button RadioButtonGroup))
(import (class tonegod.gui.core Screen))

(define-simple-class CharacterPickerGroup (RadioButtonGroup)
  ;; slots
  (state::PickCharacterState init-form: #!null)
  ;; methods
  ((*init* a-state::PickCharacterState screen::Screen uid::String)
   (invoke-special RadioButtonGroup (this) '*init* screen uid)
   (set! state a-state))
  ((onSelect index::int button::CharacterPickerButton)
   (let* ((client::FabricClient (the-client))
          (character::FabricCharacter button:character)
          (new-character-model::Node (make-character-model character))
          (root-node::Node (*:getRootNode client))
          (old-character-model::Node state:character-model))
     (set! state:picked-character character)
     (unless (eqv? #!null old-character-model)
       (*:enqueue client (runnable (lambda () (*:detachChild root-node old-character-model)))))
     (unless (eqv? #!null new-character-model)
       (*:enqueue client (runnable (lambda () (begin (set! state:character-model new-character-model)
                                                     (*:setLocalTranslation new-character-model
                                                                            (Vector3f 5.0 0.0 -10.0))
                                                     (*:attachChild root-node new-character-model)))))))))


