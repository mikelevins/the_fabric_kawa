;;;; ***********************************************************************
;;;;
;;;; Name:          appstate-character-creator.scm
;;;; Project:       The Fabric: a far-future MMORPG
;;;; Purpose:       create a new character
;;;; Author:        mikel evins
;;;; Copyright:     2015 by mikel evins
;;;;
;;;; ***********************************************************************

(module-export CharacterCreatorAppState)

;;; ---------------------------------------------------------------------
;;; required modules
;;; ---------------------------------------------------------------------

(require "util-java.scm")
(require "syntax-classes.scm")
(require "view-player-character.scm")
(require "client-main.scm")

;;; ---------------------------------------------------------------------
;;; Java imports
;;; ---------------------------------------------------------------------

(import-as AbstractAppState com.jme3.app.state.AbstractAppState)
(import-as AppStateManager com.jme3.app.state.AppStateManager)
(import-as AssetManager com.jme3.asset.AssetManager)
(import-as BitmapFont com.jme3.font.BitmapFont)
(import-as Button tonegod.gui.controls.buttons.Button)
(import-as ButtonAdapter tonegod.gui.controls.buttons.ButtonAdapter)
(import-as ColorRGBA com.jme3.math.ColorRGBA)
(import-as Label tonegod.gui.controls.text.Label)
(import-as MouseButtonEvent com.jme3.input.event.MouseButtonEvent)
(import-as Panel tonegod.gui.controls.windows.Panel)
(import-as RadioButton tonegod.gui.controls.buttons.RadioButton)
(import-as RadioButtonGroup tonegod.gui.controls.buttons.RadioButtonGroup)
(import-as Screen tonegod.gui.core.Screen)
(import-as Serializer com.jme3.network.serializing.Serializer)
(import-as SimpleApplication com.jme3.app.SimpleApplication)
(import-as SkyFactory com.jme3.util.SkyFactory)
(import-as TLabel tonegod.gui.controls.text.Label)
(import-as Vector2f com.jme3.math.Vector2f)
(import-as Vector4f com.jme3.math.Vector4f)
(import-as Window tonegod.gui.controls.windows.Window)

;; ---------------------------------------------------------------------
;;; the CharacterCreatorAppState class
;;; ---------------------------------------------------------------------

;;; class FactionButtonGroup
;;; ---------------------------------------------------------------------

(defclass FactionButtonGroup (RadioButtonGroup)
  (slots:
   (app-state init-form: #!null getter: getAppState setter: setAppState))
  (methods:
   ((*init* screen::Screen uid::String)
    (invoke-special RadioButtonGroup (this) '*init* screen uid))
   ((onSelect index::int value::Button)
    (let ((button-id (*:getUID value))
          (state::CharacterCreatorAppState (get-app-state (this))))
      (cond
       ((equal? "CaretakerButton" button-id)(set-current-faction state 'caretakers))
       ((equal? "RogueButton" button-id)(set-current-faction state 'rogues))
       ((equal? "AbjurerButton" button-id)(set-current-faction state 'abjurers))
       (else (format #t "~%Unknown faction selected")))))))

(define (get-app-state group::FactionButtonGroup)
  (*:getAppState group))

(define (set-app-state! group::FactionButtonGroup state::CharacterCreatorAppState)
  (*:setAppState group state))

;;; functions for building the display
;;; ---------------------------------------------------------------------

(define (make-sky app::SimpleApplication)
  (let ((asset-manager::AssetManager (get-asset-manager)))
    (SkyFactory:createSky asset-manager 
                          (*:loadTexture asset-manager "Textures/tycholeft.png")
                          (*:loadTexture asset-manager "Textures/tychoright.png")
                          (*:loadTexture asset-manager "Textures/tychofront.png")
                          (*:loadTexture asset-manager "Textures/tychoback.png")
                          (*:loadTexture asset-manager "Textures/tychotop.png")
                          (*:loadTexture asset-manager "Textures/tychobottom.png"))))

(define (compute-nameplate-origin screen::Screen)
  (let ((width (*:getWidth screen))
        (height (*:getHeight screen)))
    (Vector2f (/ width 2.0) 8)))

(define (compute-nameplate-size screen::Screen)
  (let ((width (*:getWidth screen)))
    (Vector2f (/ width 5.0) 40)))

(define (make-nameplate screen::Screen)
  (TLabel screen "FactionNameplate"
          (compute-nameplate-origin screen)
          (compute-nameplate-size screen)))

(define (compute-faction-palette-origin screen::Screen)
  (Vector2f 10 10))

(define (compute-faction-palette-size screen::Screen)
  (Vector2f 400 256))

(define (make-faction-palette screen::Screen)
  (Window screen "FactionPalette"
          (compute-faction-palette-origin screen)
          (compute-faction-palette-size screen)))

(define (compute-caretaker-button-origin screen::Screen)
  (Vector2f 8 32))

(define (compute-caretaker-button-size screen::Screen)
  (Vector2f 128 128))

(define (make-caretaker-button screen::Screen)
  (RadioButton screen "CaretakerButton"
               (compute-caretaker-button-origin screen)
               (compute-caretaker-button-size screen)))

(define (compute-abjurer-button-origin screen::Screen)
  (Vector2f 136 32))

(define (compute-abjurer-button-size screen::Screen)
  (Vector2f 128 128))

(define (make-abjurer-button screen::Screen)
  (RadioButton screen "AbjurerButton"
               (compute-abjurer-button-origin screen)
               (compute-abjurer-button-size screen)))

(define (compute-rogue-button-origin screen::Screen)
  (Vector2f 264 32))

(define (compute-rogue-button-size screen::Screen)
  (Vector2f 128 128))

(define (make-rogue-button screen::Screen)
  (RadioButton screen "RogueButton"
               (compute-rogue-button-origin screen)
               (compute-rogue-button-size screen)))

(define (compute-name-palette-origin screen::Screen)
  (let ((height (*:getHeight screen)))
    (Vector2f 10 (- height 180))))

(define (compute-name-palette-size screen::Screen)
  (let ((width (*:getWidth screen)))
    (Vector2f (- width 20) 160)))

(define (make-name-palette screen::Screen)
  (Window screen "NamePalette"
          (compute-name-palette-origin screen)
          (compute-name-palette-size screen)))

(define (compute-armor-palette-origin screen::Screen)
  (let ((faction-palette-origin (compute-faction-palette-origin screen))
        (faction-palette-size (compute-faction-palette-size screen)))
    (Vector2f 10
              (+ (*:getY faction-palette-origin)
                 (*:getY faction-palette-size)
                 8))))

(define (compute-armor-palette-size screen::Screen)
  (let* ((armor-palette-origin (compute-armor-palette-origin screen))
         (name-palette-origin (compute-name-palette-origin screen))
         (height (- (*:getY name-palette-origin)
                    (*:getY armor-palette-origin)
                    8))
         (screen-width (*:getWidth screen))
         (width (- (/ screen-width 8.0) 10)))
    (Vector2f width height)))

(define (make-armor-palette screen::Screen)
  (Window screen "ArmorPalette"
          (compute-armor-palette-origin screen)
          (compute-armor-palette-size screen)))

(define (compute-weapons-palette-origin screen::Screen)
  (let ((armor-palette-origin (compute-armor-palette-origin screen))
        (armor-palette-size (compute-armor-palette-size screen))
        (screen-width (*:getWidth screen)))
    (Vector2f (- screen-width (*:getX armor-palette-size) 8)
              (*:getY armor-palette-origin))))

(define (compute-weapons-palette-size screen::Screen)
  (let ((armor-palette-size (compute-armor-palette-size screen)))
    (Vector2f (*:getX armor-palette-size)
              (*:getY armor-palette-size))))

(define (make-weapons-palette screen::Screen)
  (Window screen "WeaponsPalette"
          (compute-weapons-palette-origin screen)
          (compute-weapons-palette-size screen)))

(defclass CharacterCreatorAppState (AbstractAppState)
  (slots:
   (current-character init-form: #f getter: getCurrentCharacter setter: setCurrentCharacter)
   (current-faction init-form: #f getter: getCurrentFaction setter: setCurrentFaction)
   (faction-nameplate::TLabel init-form: #!null getter: getFactionNameplate)
   (app::SimpleApplication init-form: #!null getter: getApp setter: setApp)
   (state-manager::AppStateManager init-form: #!null getter: getStateManager setter: setStateManager))
  (methods:
   ((initialize mgr::AppStateManager client::SimpleApplication)
    (*:setApp (this) client)
    (let* ((screen (Screen client))
           (gui-node (*:getGuiNode client))
           (root-node (*:getRootNode (as SimpleApplication app)))
           (align BitmapFont:Align)
           (valign BitmapFont:VAlign)
           (sky (make-sky app))
           (nameplate::TLabel (make-nameplate screen))
           (faction-palette (make-faction-palette screen))
           (faction-group (FactionButtonGroup screen "FactionGroup"))
           (highlight-color (ColorRGBA 1.0 1.0 0.0 1.0))
           (caretaker-button (make-caretaker-button screen))
           (abjurer-button (make-abjurer-button screen))
           (rogue-button (make-rogue-button screen))
           (character (make-player-character))
           (char-cube (get-property character 'cube: default: #f))
           (name-palette (make-name-palette screen))
           (armor-palette (make-armor-palette screen))
           (weapons-palette (make-weapons-palette screen)))
      ;; --------------------
      ;; init the faction buttons
      ;; --------------------
      (set-app-state! faction-group (this))
      ;; --------------------
      ;; add the sky to the scene
      ;; --------------------
      (*:attachChild root-node sky)
      ;; --------------------
      ;; add the character model
      ;; --------------------
      (if char-cube
          (begin (*:attachChild root-node char-cube)
                 (*:setCurrentCharacter (this) character)))
      ;; --------------------
      ;; add the nameplate
      ;; --------------------
      (set! faction-nameplate nameplate)
      (*:setText nameplate "")
      (*:setTextAlign nameplate align:Left)
      (*:setFont nameplate "Interface/Fonts/Laconic30.fnt")
      (*:setFontSize nameplate 30)
      (*:setFontColor nameplate ColorRGBA:Green)
      (*:addElement screen nameplate)
      ;; --------------------
      ;; faction palette
      ;; --------------------
      (*:setWindowTitle faction-palette "Choose a Faction:")
      (*:setButtonIcon caretaker-button 128 128 "Interface/caretakers-icon128.png")
      (*:setButtonPressedInfo caretaker-button "Interface/caretakers-icon-lit128.png"
                              (ColorRGBA 0.0 1.0 0.0 1.0))
      (*:setText caretaker-button "Caretakers")
      (*:setTextAlign caretaker-button align:Center)
      (*:setTextVAlign caretaker-button valign:Bottom)
      (*:setFontSize caretaker-button 20)
      (*:addButton faction-group caretaker-button)
      (*:setButtonIcon abjurer-button 128 128 "Interface/abjurers-icon128.png")
      (*:setButtonPressedInfo abjurer-button "Interface/abjurers-icon-lit128.png"
                              (ColorRGBA 1.0 0.0 0.0 1.0))
      (*:setText abjurer-button "Abjurers")
      (*:setTextAlign abjurer-button align:Center)
      (*:setTextVAlign abjurer-button valign:Bottom)
      (*:setFontSize abjurer-button 20)
      (*:addButton faction-group abjurer-button)
      (*:setButtonIcon rogue-button 128 128 "Interface/rogues-icon128.png")
      (*:setButtonPressedInfo rogue-button "Interface/rogues-icon-lit128.png"
                              (ColorRGBA 0.0 0.75 1.0 1.0))
      (*:setText rogue-button "Rogues")
      (*:setTextAlign rogue-button align:Center)
      (*:setTextVAlign rogue-button valign:Bottom)
      (*:setFontSize rogue-button 20)
      (*:addButton faction-group rogue-button)
      (*:addChild faction-palette caretaker-button)
      (*:addChild faction-palette abjurer-button)
      (*:addChild faction-palette rogue-button)
      (*:addElement screen faction-palette)
      ;; --------------------
      ;; name palette
      ;; --------------------
      (*:setWindowTitle name-palette "Choose a Name:")
      (*:addElement screen name-palette)
      ;; --------------------
      ;; armor palette
      ;; --------------------
      (*:setWindowTitle armor-palette "Choose Armor:")
      (*:addElement screen armor-palette)
      ;; --------------------
      ;; weapons palette
      ;; --------------------
      (*:setWindowTitle weapons-palette "Choose Weapons:")
      (*:addElement screen weapons-palette)
      ;; --------------------
      ;; add the gui to the scene
      ;; --------------------
      (*:addControl gui-node screen)))))

(define (update-state-for-faction state::CharacterCreatorAppState faction)
  (let* ((character-entity (*:getCurrentCharacter state)))
    (if (member faction '(caretakers rogues abjurers))
        (let ((char-cube (get-property character-entity 'cube: default: #f)))
          (if char-cube
              (case faction
                ((caretakers)(let ((nameplate::TLabel (*:getFactionNameplate state)))
                               (set-player-character-cube-color! char-cube (caretakers-character-color))
                               (*:setText nameplate "Faction: Caretakers")))
                ((rogues)(let ((nameplate::TLabel (*:getFactionNameplate state)))
                           (set-player-character-cube-color! char-cube (rogues-character-color))
                           (*:setText nameplate "Faction: Rogues")))
                ((abjurers)(let ((nameplate::TLabel (*:getFactionNameplate state)))
                             (set-player-character-cube-color! char-cube (abjurers-character-color))
                             (*:setText nameplate "Faction: Abjurers"))))
              (error "update-state-for-faction: missing character cube ")))
        (error "set-current-faction: unrecognized faction: " faction))))

(define (set-current-faction state::CharacterCreatorAppState faction)
  (if (member faction '(caretakers rogues abjurers))
      (begin (*:setCurrentFaction state faction)
             (update-state-for-faction state faction))
      (error "set-current-faction: unrecognized faction: " faction)))
