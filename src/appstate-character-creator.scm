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
;;; ABOUT
;;; ---------------------------------------------------------------------
;;; the character creator provides players the ability to create and
;;; customize new playable characters

;;; ---------------------------------------------------------------------
;;; required modules
;;; ---------------------------------------------------------------------

(require 'list-lib)
(require "util-java.scm")
(require "syntax-classes.scm")
(require "data-names.scm")
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
(import-as ComboBox tonegod.gui.controls.lists.ComboBox)
(import-as Integer java.lang.Integer)
(import-as Label tonegod.gui.controls.text.Label)
(import-as MouseButtonEvent com.jme3.input.event.MouseButtonEvent)
(import-as Panel tonegod.gui.controls.windows.Panel)
(import-as RadioButton tonegod.gui.controls.buttons.RadioButton)
(import-as RadioButtonGroup tonegod.gui.controls.buttons.RadioButtonGroup)
(import-as Screen tonegod.gui.core.Screen)
(import-as SelectBox tonegod.gui.controls.lists.SelectBox)
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

;;; CLASS NameMenu
;;; ---------------------------------------------------------------------
;;; a SelectBox subclass used to present name options for player
;;; characters

(defclass NameMenu (SelectBox)
  (slots:)
  (methods:
   ((*init* screen::Screen uid::String position::Vector2f size::Vector2f)
    (invoke-special SelectBox (this) '*init* screen uid position size))
   ((onChange index::int value::Object) #!void)))

;;; CLASS FactionButtonGroup
;;; ---------------------------------------------------------------------
;;; a RadioButtonGroup subclass used to present faction options to
;;; players

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

;;; (get-app-state group::FactionButtonGroup)
;;; ---------------------------------------------------------------------
;;; returns _group_'s AppState object 

(define (get-app-state group::FactionButtonGroup)
  (*:getAppState group))

;;; (set-app-state! group::FactionButtonGroup state::CharacterCreatorAppState)
;;; ---------------------------------------------------------------------
;;; assigns _state_ to _group_'s app-state slot

(define (set-app-state! group::FactionButtonGroup state::CharacterCreatorAppState)
  (*:setAppState group state))

;;; ---------------------------------------------------------------------
;;; the skybox
;;; ---------------------------------------------------------------------

;;; (make-sky app::SimpleApplication)
;;; ---------------------------------------------------------------------
;;; returns a newly-constructed Fabric skybox

(define (make-sky app::SimpleApplication)
  (let ((asset-manager::AssetManager (get-asset-manager)))
    (SkyFactory:createSky asset-manager 
                          (*:loadTexture asset-manager "Textures/tycholeft.png")
                          (*:loadTexture asset-manager "Textures/tychoright.png")
                          (*:loadTexture asset-manager "Textures/tychofront.png")
                          (*:loadTexture asset-manager "Textures/tychoback.png")
                          (*:loadTexture asset-manager "Textures/tychotop.png")
                          (*:loadTexture asset-manager "Textures/tychobottom.png"))))

;;; ---------------------------------------------------------------------
;;; the faction nameplate
;;; ---------------------------------------------------------------------

;;; (compute-faction-nameplate-origin screen::Screen)
;;; ---------------------------------------------------------------------
;;; computes and returns a suitable origin for the faction nameplate,
;;; taking into accoun the dimensions of the screen

(define (compute-faction-nameplate-origin screen::Screen)
  (let ((width (*:getWidth screen))
        (height (*:getHeight screen)))
    (Vector2f (/ width 2.0) 8)))


;;; (compute-faction-nameplate-size screen::Screen)
;;; ---------------------------------------------------------------------
;;; computes and returns a suitable size for the faction nameplate,
;;; taking into accoun the dimensions of the screen

(define (compute-faction-nameplate-size screen::Screen)
  (let ((width (*:getWidth screen)))
    (Vector2f (/ width 5.0) 40)))


;;; (make-faction-nameplate screen::Screen)
;;; ---------------------------------------------------------------------
;;; constructs and returns a new TLabel object for use as the faction
;;; nameplate

(define (make-faction-nameplate screen::Screen)
  (TLabel screen "FactionNameplate"
          (compute-faction-nameplate-origin screen)
          (compute-faction-nameplate-size screen)))


;;; ---------------------------------------------------------------------
;;; the faction palette
;;; ---------------------------------------------------------------------


;;; (compute-faction-palette-origin screen::Screen)
;;; ---------------------------------------------------------------------
;;; computes and returns a suitable origin for the faction palette,
;;; taking into accoun the dimensions of the screen

(define (compute-faction-palette-origin screen::Screen)
  (Vector2f 10 10))


;;; (compute-faction-palette-size screen::Screen)
;;; ---------------------------------------------------------------------
;;; computes and returns a suitable size for the faction palette,
;;; taking into accoun the dimensions of the screen


(define (compute-faction-palette-size screen::Screen)
  (Vector2f 400 256))


;;; (make-faction-palette screen::Screen)
;;; ---------------------------------------------------------------------
;;; returns a newly-constructed and -populated faction-palette window

(define (make-faction-palette screen::Screen)
  (Window screen "FactionPalette"
          (compute-faction-palette-origin screen)
          (compute-faction-palette-size screen)))


;;; (compute-caretaker-button-origin screen::Screen)
;;; ---------------------------------------------------------------------
;;; computes and returns a suitable origin for the caretakers faction
;;; button


(define (compute-caretaker-button-origin screen::Screen)
  (Vector2f 8 32))


;;; (compute-caretaker-button-size screen::Screen)
;;; ---------------------------------------------------------------------
;;; computes and returns a suitable size for the caretakers faction
;;; button


(define (compute-caretaker-button-size screen::Screen)
  (Vector2f 128 128))


;;; (make-caretaker-button screen::Screen)
;;; ---------------------------------------------------------------------
;;; returns a newly-constructed caretakers faction button

(define (make-caretaker-button screen::Screen)
  (RadioButton screen "CaretakerButton"
               (compute-caretaker-button-origin screen)
               (compute-caretaker-button-size screen)))


;;; (compute-abjurer-button-origin screen::Screen)
;;; ---------------------------------------------------------------------
;;; computes and returns a suitable origin for the abjurers faction
;;; button

(define (compute-abjurer-button-origin screen::Screen)
  (Vector2f 136 32))


;;; (compute-abjurer-button-size screen::Screen)
;;; ---------------------------------------------------------------------
;;; computes and returns a suitable size for the abjurers faction
;;; button

(define (compute-abjurer-button-size screen::Screen)
  (Vector2f 128 128))


;;; (make-abjurer-button screen::Screen)
;;; ---------------------------------------------------------------------
;;; returns a newly-constructed abjurers faction button

(define (make-abjurer-button screen::Screen)
  (RadioButton screen "AbjurerButton"
               (compute-abjurer-button-origin screen)
               (compute-abjurer-button-size screen)))


;;; (compute-rogue-button-origin screen::Screen)
;;; ---------------------------------------------------------------------
;;; computes and returns a suitable origin for the rogues faction
;;; button

(define (compute-rogue-button-origin screen::Screen)
  (Vector2f 264 32))


;;; (compute-rogue-button-size screen::Screen)
;;; ---------------------------------------------------------------------
;;; computes and returns a suitable size for the rogues faction
;;; button

(define (compute-rogue-button-size screen::Screen)
  (Vector2f 128 128))


;;; (make-rogue-button screen::Screen)
;;; ---------------------------------------------------------------------
;;; returns a newly-constructed rogues faction button

(define (make-rogue-button screen::Screen)
  (RadioButton screen "RogueButton"
               (compute-rogue-button-origin screen)
               (compute-rogue-button-size screen)))


;;; ---------------------------------------------------------------------
;;; the name palette
;;; ---------------------------------------------------------------------


;;; (compute-name-palette-origin screen::Screen)
;;; ---------------------------------------------------------------------
;;; computes and returns a suitable origin for the name palette

(define (compute-name-palette-origin screen::Screen)
  (let ((height (*:getHeight screen)))
    (Vector2f 10 (- height 180))))


;;; (compute-name-palette-size screen::Screen)
;;; ---------------------------------------------------------------------
;;; computes and returns a suitable size for the name palette

(define (compute-name-palette-size screen::Screen)
  (let ((width (*:getWidth screen)))
    (Vector2f (- width 20) 160)))


;;; (compute-name-menu-bounds size)
;;; ---------------------------------------------------------------------
;;; computes and returns lists of coordinates to be used as the bounds
;;; of name menus in the name palette

(define (compute-name-menu-bounds size)
  (let* ((width (*:getX size))
         (adjusted-width (- width 16))
         (segment-width (/ adjusted-width 8))
         (adjusted-segment-width (- segment-width 32))
         (lefts (map (lambda (i)
                       (+ 48
                          (* i 24)
                          (* i adjusted-segment-width)))
                     (iota 8)))
         (tops (map (lambda (i) 40)
                    (iota 8)))
         (widths (map (lambda (i) adjusted-segment-width)
                      (iota 8)))
         (heights (map (lambda (i) 24)
                       (iota 8))))
    (values lefts tops widths heights)))


;;; (make-name-palette screen::Screen)
;;; ---------------------------------------------------------------------
;;; returns a newly-constructed and -populated name palette

(define (make-name-palette screen::Screen)
  (let ((size (compute-name-palette-size screen)))
    (receive (lefts tops widths heights) (compute-name-menu-bounds size)
      (let* ((origin (compute-name-palette-origin screen))
             (palette (Window screen "NamePalette" origin size))
             (domain0-menu (NameMenu screen "Domain0Menu"
                                     (Vector2f (list-ref lefts 0)(list-ref tops 0))
                                     (Vector2f (list-ref widths 0)(list-ref heights 0))))
             (domain1-menu (NameMenu screen "Domain1Menu"
                                     (Vector2f (list-ref lefts 1)(list-ref tops 1))
                                     (Vector2f (list-ref widths 1)(list-ref heights 1))))
             (domain2-menu (NameMenu screen "Domain2Menu"
                                     (Vector2f (list-ref lefts 2)(list-ref tops 2))
                                     (Vector2f (list-ref widths 2)(list-ref heights 2))))
             (domain3-menu (NameMenu screen "Domain3Menu"
                                     (Vector2f (list-ref lefts 3)(list-ref tops 3))
                                     (Vector2f (list-ref widths 3)(list-ref heights 3))))
             (domain4-menu (NameMenu screen "Domain4Menu"
                                     (Vector2f (list-ref lefts 4)(list-ref tops 4))
                                     (Vector2f (list-ref widths 4)(list-ref heights 4))))
             (domain5-menu (NameMenu screen "Domain5Menu"
                                     (Vector2f (list-ref lefts 5)(list-ref tops 5))
                                     (Vector2f (list-ref widths 5)(list-ref heights 5))))
             (domain6-menu (NameMenu screen "Domain6Menu"
                                     (Vector2f (list-ref lefts 6)(list-ref tops 6))
                                     (Vector2f (list-ref widths 6)(list-ref heights 6))))
             (domain7-menu (NameMenu screen "Domain7Menu"
                                     (Vector2f (list-ref lefts 7)(list-ref tops 7))
                                     (Vector2f (list-ref widths 7)(list-ref heights 7)))))
        
        ;; domain0
        (for-each (lambda (nm)(*:addListItem domain0-menu nm nm))
                  (list-ref (name-domains) 0))
        (*:addChild palette domain0-menu)
        ;; domain1
        (for-each (lambda (nm::int)(*:addListItem domain1-menu (format #f "~a" nm) (Integer nm)))
                  (list-ref (name-domains) 1))
        (*:addChild palette domain1-menu)
        ;; domain2
        (for-each (lambda (nm)(*:addListItem domain2-menu nm nm))
                  (list-ref (name-domains) 2))
        (*:addChild palette domain2-menu)
        ;; domain3
        (for-each (lambda (nm)(*:addListItem domain3-menu nm nm))
                  (list-ref (name-domains) 3))
        (*:addChild palette domain3-menu)
        ;; domain4
        (for-each (lambda (nm)(*:addListItem domain4-menu nm nm))
                  (list-ref (name-domains) 4))
        (*:addChild palette domain4-menu)
        ;; domain5
        (for-each (lambda (nm)(*:addListItem domain5-menu nm nm))
                  (list-ref (name-domains) 5))
        (*:addChild palette domain5-menu)
        ;; domain6
        (for-each (lambda (nm)(*:addListItem domain6-menu nm nm))
                  (list-ref (name-domains) 6))
        (*:addChild palette domain6-menu)
        ;; domain7
        (for-each (lambda (nm)(*:addListItem domain7-menu nm nm))
                  (list-ref (name-domains) 7))
        (*:addChild palette domain7-menu)
        palette))))


;;; ---------------------------------------------------------------------
;;; the armor palette
;;; ---------------------------------------------------------------------


;;; (compute-armor-palette-origin screen::Screen)
;;; ---------------------------------------------------------------------
;;; computes and returns a suitable origin for the armor palette, taking
;;; into account the dimensions of the screen

(define (compute-armor-palette-origin screen::Screen)
  (let ((faction-palette-origin (compute-faction-palette-origin screen))
        (faction-palette-size (compute-faction-palette-size screen)))
    (Vector2f 10
              (+ (*:getY faction-palette-origin)
                 (*:getY faction-palette-size)
                 8))))


;;; (compute-armor-palette-size screen::Screen)
;;; ---------------------------------------------------------------------
;;; computes and returns a suitable size for the armor palette, taking
;;; into account the dimensions of the screen

(define (compute-armor-palette-size screen::Screen)
  (let* ((armor-palette-origin (compute-armor-palette-origin screen))
         (name-palette-origin (compute-name-palette-origin screen))
         (height (- (*:getY name-palette-origin)
                    (*:getY armor-palette-origin)
                    8))
         (screen-width (*:getWidth screen))
         (width (- (/ screen-width 8.0) 10)))
    (Vector2f width height)))


;;; (make-armor-palette screen::Screen)
;;; ---------------------------------------------------------------------
;;; returns a newly-contructed armor palette

(define (make-armor-palette screen::Screen)
  (Window screen "ArmorPalette"
          (compute-armor-palette-origin screen)
          (compute-armor-palette-size screen)))


;;; ---------------------------------------------------------------------
;;; the weapons palette
;;; ---------------------------------------------------------------------


;;; (compute-weapons-palette-origin screen::Screen)
;;; ---------------------------------------------------------------------
;;; computes and returns a suitable origin for the weapons palette, taking
;;; into account the dimensions of the screen

(define (compute-weapons-palette-origin screen::Screen)
  (let ((armor-palette-origin (compute-armor-palette-origin screen))
        (armor-palette-size (compute-armor-palette-size screen))
        (screen-width (*:getWidth screen)))
    (Vector2f (- screen-width (*:getX armor-palette-size) 8)
              (*:getY armor-palette-origin))))


;;; (compute-weapons-palette-size screen::Screen)
;;; ---------------------------------------------------------------------
;;; computes and returns a suitable size for the weapons palette, taking
;;; into account the dimensions of the screen

(define (compute-weapons-palette-size screen::Screen)
  (let ((armor-palette-size (compute-armor-palette-size screen)))
    (Vector2f (*:getX armor-palette-size)
              (*:getY armor-palette-size))))


;;; (make-weapons-palette screen::Screen)
;;; ---------------------------------------------------------------------
;;; returns a newly-contructed weapons palette

(define (make-weapons-palette screen::Screen)
  (Window screen "WeaponsPalette"
          (compute-weapons-palette-origin screen)
          (compute-weapons-palette-size screen)))


;;; ---------------------------------------------------------------------
;;; the AppState
;;; ---------------------------------------------------------------------


;;; CLASS CharacterCreatorAppState
;;; ---------------------------------------------------------------------
;;; the AppState class that constructs and manages the character
;;; creator scene in the Fabric client

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
           (fnameplate::TLabel (make-faction-nameplate screen))
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
      ;; add the faction-nameplate
      ;; --------------------
      (set! faction-nameplate fnameplate)
      (*:setText fnameplate "")
      (*:setTextAlign fnameplate align:Left)
      (*:setFont fnameplate "Interface/Fonts/Laconic30.fnt")
      (*:setFontSize fnameplate 30)
      (*:setFontColor fnameplate ColorRGBA:Green)
      (*:addElement screen fnameplate)
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


;;; (update-state-for-faction state::CharacterCreatorAppState faction)
;;; ---------------------------------------------------------------------
;;; updates the display state of the character creator's user interface
;;; in response to any changes made by the user or the game

(define (update-state-for-faction state::CharacterCreatorAppState faction)
  (let* ((character-entity (*:getCurrentCharacter state)))
    (if (member faction '(caretakers rogues abjurers))
        (let ((char-cube (get-property character-entity 'cube: default: #f)))
          (if char-cube
              (case faction
                ((caretakers)(let ((fnameplate::TLabel (*:getFactionNameplate state)))
                               (set-player-character-cube-color! char-cube (caretakers-character-color))
                               (*:setText fnameplate "Faction: Caretakers")))
                ((rogues)(let ((fnameplate::TLabel (*:getFactionNameplate state)))
                           (set-player-character-cube-color! char-cube (rogues-character-color))
                           (*:setText fnameplate "Faction: Rogues")))
                ((abjurers)(let ((fnameplate::TLabel (*:getFactionNameplate state)))
                             (set-player-character-cube-color! char-cube (abjurers-character-color))
                             (*:setText fnameplate "Faction: Abjurers"))))
              (error "update-state-for-faction: missing character cube ")))
        (error "set-current-faction: unrecognized faction: " faction))))


;;; (set-current-faction state::CharacterCreatorAppState faction)
;;; ---------------------------------------------------------------------
;;; updates the currently-selected player faction in the character
;;; creator in response to a user selection

(define (set-current-faction state::CharacterCreatorAppState faction)
  (if (member faction '(caretakers rogues abjurers))
      (begin (*:setCurrentFaction state faction)
             (update-state-for-faction state faction))
      (error "set-current-faction: unrecognized faction: " faction)))
