;;;; ***********************************************************************
;;;;
;;;; Name:          appstate-character-creator.scm
;;;; Project:       The Fabric: a far-future MMORPG
;;;; Purpose:       create a new character
;;;; Author:        mikel evins
;;;; Copyright:     2015 by mikel evins
;;;;
;;;; ***********************************************************************

(module-export
 +character-x-position+
 +character-y-position+
 +character-z-position+
 CharacterCreatorAppState
 current-fabric-name
 notify-name-selection-changed
 set-current-armor
 set-current-augment
 set-current-fabric-name!
 set-current-faction
 set-current-weapon)



;;; =====================================================================
;;; ABOUT
;;; =====================================================================
;;; the character creator provides players the ability to create and
;;; customize new playable characters

;;; ---------------------------------------------------------------------
;;; required modules
;;; ---------------------------------------------------------------------

(require 'list-lib)
(require "util-java.scm")
(require "util-lists.scm")
(require "syntax-classes.scm")
(require "data-names.scm")
(require "data-assets.scm")
(require "model-namegen.scm")
(require "model-entity.scm")
(require "view-skybox.scm")
(require "view-name-menu.scm")
(require "view-armor-palette.scm")
(require "view-augment-palette.scm")
(require "view-faction-palette.scm")
(require "view-weapons-palette.scm")
(require "view-faction-nameplate.scm")
(require "view-name-palette.scm")
(require "view-character-nameplate.scm")
(require "view-controls.scm")
(require "view-colors.scm")
(require "view-armor.scm")
(require "view-augments.scm")
(require "view-weapons.scm")
(require "view-player-character.scm")
(require "util-error.scm")
(require "client-main.scm")

;;; ---------------------------------------------------------------------
;;; Java imports
;;; ---------------------------------------------------------------------

(import-as AbstractAppState com.jme3.app.state.AbstractAppState)
(import-as AppStateManager com.jme3.app.state.AppStateManager)
(import-as AssetManager com.jme3.asset.AssetManager)
(import-as BitmapFont com.jme3.font.BitmapFont)
(import-as BloomFilter com.jme3.post.filters.BloomFilter)
(import-as Button tonegod.gui.controls.buttons.Button)
(import-as ButtonAdapter tonegod.gui.controls.buttons.ButtonAdapter)
(import-as ColorRGBA com.jme3.math.ColorRGBA)
(import-as ComboBox tonegod.gui.controls.lists.ComboBox)
(import-as FilterPostProcessor com.jme3.post.FilterPostProcessor)
(import-as Geometry com.jme3.scene.Geometry)
(import-as Integer java.lang.Integer)
(import-as Label tonegod.gui.controls.text.Label)
(import-as Material com.jme3.material.Material)
(import-as MouseButtonEvent com.jme3.input.event.MouseButtonEvent)
(import-as Node com.jme3.scene.Node)
(import-as Panel tonegod.gui.controls.windows.Panel)
(import-as RadioButton tonegod.gui.controls.buttons.RadioButton)
(import-as RadioButtonGroup tonegod.gui.controls.buttons.RadioButtonGroup)
(import-as SafeArrayList com.jme3.util.SafeArrayList)
(import-as Screen tonegod.gui.core.Screen)
(import-as SelectBox tonegod.gui.controls.lists.SelectBox)
(import-as Serializer com.jme3.network.serializing.Serializer)
(import-as SimpleApplication com.jme3.app.SimpleApplication)
(import-as TLabel tonegod.gui.controls.text.Label)
(import-as Vector2f com.jme3.math.Vector2f)
(import-as Vector4f com.jme3.math.Vector4f)
(import-as ViewPort com.jme3.renderer.ViewPort)
(import-as Window tonegod.gui.controls.windows.Window)

;;; ---------------------------------------------------------------------
;;; constants
;;; ---------------------------------------------------------------------

(define +character-x-position+ 0.0)
(define +character-y-position+ 0.0)
(define +character-z-position+ -8.0)


;;; =====================================================================
;;; the AppState Class
;;; =====================================================================

;;; CLASS CharacterCreatorAppState
;;; ---------------------------------------------------------------------
;;; the AppState class that constructs and manages the character
;;; creator scene in the Fabric client

(defclass CharacterCreatorAppState (AbstractAppState)
  (slots:
   (current-character init-form: #f getter: getCurrentCharacter setter: setCurrentCharacter)
   (character-name init-form: (blank-fabric-name) getter: getCharacterName setter: setCharacterName)
   (current-faction init-form: #f getter: getCurrentFaction setter: setCurrentFaction)
   (character-armor init-form: #!null getter: getCharacterArmor setter: setCharacterArmor)
   (character-augment init-form: #!null getter: getCharacterAugment setter: setCharacterAugment)
   (character-weapon init-form: #!null getter: getCharacterWeapon setter: setCharacterWeapon)
   (character-nameplate::TLabel init-form: #!null getter: getCharacterNameplate setter: setCharacterNameplate)
   (faction-nameplate::TLabel init-form: #!null getter: getFactionNameplate setter: setFactionNameplate)
   (name-palette::Window init-form: #!null getter: getNamePalette setter: setNamePalette)
   (app::SimpleApplication init-form: #!null getter: getApp setter: setApp)
   (state-manager::AppStateManager init-form: #!null getter: getStateManager setter: setStateManager))
  (methods:
   ((initialize mgr::AppStateManager client::SimpleApplication)
    (init-character-creator (this) mgr client))))

;;; =====================================================================
;;; Scene setup
;;; =====================================================================

;;; (init-character-creator-sky client::SimpleApplication)
;;; ---------------------------------------------------------------------
;;; construct the character creator's skybox

(define (init-character-creator-sky client::FabricClient)
  (let* ((sky (make-sky-box client))
         ;;(sky (make-sky-sphere client)) ; if we want to try sphere maps instead
         (root-node (*:getRootNode (as SimpleApplication client))))
    ;; add the sky to the scene
    (*:attachChild root-node sky)))


;;; (init-character-creator-model state::CharacterCreatorAppState client::SimpleApplication)
;;; ---------------------------------------------------------------------
;;; construct the character creator's skybox

(define (init-character-creator-model state::CharacterCreatorAppState client::FabricClient)
  (let* ((root-node (*:getRootNode (as SimpleApplication client)))
         (character (make-player-character))
         (node::Node (get-property character 'node: default: #f))
         (cubes-pivot (*:getChild node "CubesPivot")))
    (*:setCurrentCharacter state character)
    (if node
        (let ((rotator::RotatorControl (make-rotator-control 0.1 0.2 0.0)))
          (*:attachChild root-node (as Node node))
          (*:setLocalTranslation (as Node node)
                                 +character-x-position+
                                 +character-y-position+
                                 +character-z-position+)
          (*:setCurrentCharacter state character)
          (*:addControl (as Node node) rotator))
        (warn "init-character-creator-model: creating a starting character failed"))))

;;; (init-character-nameplate state::CharacterCreatorAppState client::SimpleApplication)
;;; ---------------------------------------------------------------------
;;; construct the character nameplate

(define (init-character-nameplate state::CharacterCreatorAppState screen::Screen client::SimpleApplication)
  (let* ((align BitmapFont:Align)
         (cnameplate::TLabel (make-character-nameplate screen)))
    (*:setCharacterNameplate state cnameplate)
    (*:setText cnameplate "")
    (*:setTextAlign cnameplate align:Left)
    (*:setFont cnameplate "Interface/Fonts/Laconic30.fnt")
    (*:setFontSize cnameplate 30)
    (*:setFontColor cnameplate ColorRGBA:Green)
    (*:addElement screen cnameplate)))


;;; (init-faction-nameplate state::CharacterCreatorAppState screen::Screen client::SimpleApplication)
;;; ---------------------------------------------------------------------
;;; construct the faction nameplate

(define (init-faction-nameplate state::CharacterCreatorAppState screen::Screen  client::SimpleApplication)
  (let* ((align BitmapFont:Align)
         (nameplate::TLabel (make-faction-nameplate screen)))
    (*:setFactionNameplate state nameplate)
    (*:setText nameplate "")
    (*:setTextAlign nameplate align:Left)
    (*:setFont nameplate "Interface/Fonts/Laconic30.fnt")
    (*:setFontSize nameplate 30)
    (*:setFontColor nameplate ColorRGBA:Green)
    (*:addElement screen nameplate)
    nameplate))

;;; (init-faction-palette state::CharacterCreatorAppState screen::Screen client::SimpleApplication)
;;; ---------------------------------------------------------------------
;;; construct the faction palette

(define (init-faction-palette state::CharacterCreatorAppState screen::Screen client::SimpleApplication)
  (let* ((align BitmapFont:Align)
         (valign BitmapFont:VAlign)
         (faction-group (FactionButtonGroup screen "FactionGroup"))
         (faction-palette (make-faction-palette screen))
         (caretaker-button (make-caretaker-button screen))
         (abjurer-button (make-abjurer-button screen))
         (rogue-button (make-rogue-button screen)))
    (*:setWindowTitle faction-palette "Choose a Faction:")
    ;; caretaker button
    (*:setButtonIcon caretaker-button 128 128 "Interface/caretakers-icon128.png")
    (*:setButtonPressedInfo caretaker-button "Interface/caretakers-icon-lit128.png"
                            (ColorRGBA 0.0 1.0 0.0 1.0))
    (*:setText caretaker-button "Caretakers")
    (*:setTextAlign caretaker-button align:Center)
    (*:setTextVAlign caretaker-button valign:Bottom)
    (*:setFontSize caretaker-button 20)
    (*:addButton faction-group caretaker-button)
    ;; abjurer button
    (*:setButtonIcon abjurer-button 128 128 "Interface/abjurers-icon128.png")
    (*:setButtonPressedInfo abjurer-button "Interface/abjurers-icon-lit128.png"
                            (ColorRGBA 1.0 0.0 0.0 1.0))
    (*:setText abjurer-button "Abjurers")
    (*:setTextAlign abjurer-button align:Center)
    (*:setTextVAlign abjurer-button valign:Bottom)
    (*:setFontSize abjurer-button 20)
    (*:addButton faction-group abjurer-button)
    ;; rogue button
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
    ;; set initial faction state
    (set-faction-palette-app-state! faction-group state)
    faction-palette))


;;; (init-armor-palette state::CharacterCreatorAppState screen::Screen client::SimpleApplication)
;;; ---------------------------------------------------------------------
;;; construct the armor palette

(define (init-armor-palette state::CharacterCreatorAppState screen::Screen client::SimpleApplication)
  (let ((align BitmapFont:Align)
        (valign BitmapFont:VAlign)
        (armor-group (ArmorButtonGroup screen "ArmorGroup"))
        (armor-palette (make-armor-palette screen))
        (absorb-armor-button (make-absorb-armor-button screen))
        (regen-armor-button (make-regen-armor-button screen))
        (power-armor-button (make-power-armor-button screen))
        (energy-armor-button (make-energy-armor-button screen)))
    (*:setWindowTitle armor-palette "Choose Armor:")
    (*:setAppState armor-group state)
    ;; absorb armor button
    (*:setButtonIcon absorb-armor-button 128 128 "Interface/absorb-armor-icon128.png")
    (*:setButtonPressedInfo absorb-armor-button "Interface/absorb-armor-icon128.png"
                            (ColorRGBA 1.0 1.0 0.0 1.0))
    (*:setText absorb-armor-button "Absorb")
    (*:setTextAlign absorb-armor-button align:Center)
    (*:setTextVAlign absorb-armor-button valign:Bottom)
    (*:setFontSize absorb-armor-button 20)
    (*:addButton armor-group absorb-armor-button)
    (*:addChild armor-palette absorb-armor-button)
    ;; regen armor button
    (*:setButtonIcon regen-armor-button 128 128 "Interface/regen-armor-icon128.png")
    (*:setButtonPressedInfo regen-armor-button "Interface/regen-armor-icon128.png"
                            (ColorRGBA 0.0 1.0 0.0 1.0))
    (*:setText regen-armor-button "Regenerate")
    (*:setTextAlign regen-armor-button align:Center)
    (*:setTextVAlign regen-armor-button valign:Bottom)
    (*:setFontSize regen-armor-button 20)
    (*:addButton armor-group regen-armor-button)
    (*:addChild armor-palette regen-armor-button)
    ;; power armor button
    (*:setButtonIcon power-armor-button 128 128 "Interface/power-armor-icon128.png")
    (*:setButtonPressedInfo power-armor-button "Interface/power-armor-icon128.png"
                            (ColorRGBA 1.0 0.5 0.0 1.0))
    (*:setText power-armor-button "Power")
    (*:setTextAlign power-armor-button align:Center)
    (*:setTextVAlign power-armor-button valign:Bottom)
    (*:setFontSize power-armor-button 20)
    (*:addButton armor-group power-armor-button)
    (*:addChild armor-palette power-armor-button)
    ;; energy armor button
    (*:setButtonIcon energy-armor-button 128 128 "Interface/energy-armor-icon128.png")
    (*:setButtonPressedInfo energy-armor-button "Interface/energy-armor-icon128.png"
                            (ColorRGBA 0.0 6.0 1.0 1.0))
    (*:setText energy-armor-button "Energy")
    (*:setTextAlign energy-armor-button align:Center)
    (*:setTextVAlign energy-armor-button valign:Bottom)
    (*:setFontSize energy-armor-button 20)
    (*:addButton armor-group energy-armor-button)
    (*:addChild armor-palette energy-armor-button)
    ;; add palette to screen
    (*:addElement screen armor-palette)))


;;; (init-weapons-palette state::CharacterCreatorAppState screen::Screen client::SimpleApplication)
;;; ---------------------------------------------------------------------
;;; construct the weapons palette

(define (init-weapons-palette state::CharacterCreatorAppState screen::Screen client::SimpleApplication)
  (let* ((align BitmapFont:Align)
         (valign BitmapFont:VAlign)
         (weapons-palette (make-weapons-palette screen))
         (weapons-group (WeaponsButtonGroup screen "WeaponsGroup"))
         (cannon-weapon-button (make-cannon-weapon-button screen))
         (impulse-weapon-button (make-impulse-weapon-button screen))
         (malware-weapon-button (make-malware-weapon-button screen))
         (bots-weapon-button (make-bots-weapon-button screen)))
    (*:setWindowTitle weapons-palette "Choose Weapons:")
    (*:setAppState weapons-group state)
    ;; cannon weapon button
    (*:setButtonIcon cannon-weapon-button 128 128 "Interface/cannon-weapon-icon128.png")
    (*:setButtonPressedInfo cannon-weapon-button "Interface/cannon-weapon-icon128.png"
                            (ColorRGBA 1.0 1.0 0.0 1.0))
    (*:setText cannon-weapon-button "Cannon")
    (*:setTextAlign cannon-weapon-button align:Center)
    (*:setTextVAlign cannon-weapon-button valign:Bottom)
    (*:setFontSize cannon-weapon-button 20)
    (*:addButton weapons-group cannon-weapon-button)
    (*:addChild weapons-palette cannon-weapon-button)
    ;; impulse weapon button
    (*:setButtonIcon impulse-weapon-button 128 128 "Interface/impulse-weapon-icon128.png")
    (*:setButtonPressedInfo impulse-weapon-button "Interface/impulse-weapon-icon128.png"
                            (ColorRGBA 0.0 1.0 0.0 1.0))
    (*:setText impulse-weapon-button "Impulse")
    (*:setTextAlign impulse-weapon-button align:Center)
    (*:setTextVAlign impulse-weapon-button valign:Bottom)
    (*:setFontSize impulse-weapon-button 20)
    (*:addButton weapons-group impulse-weapon-button)
    (*:addChild weapons-palette impulse-weapon-button)
    ;; malware weapon button
    (*:setButtonIcon malware-weapon-button 128 128 "Interface/malware-weapon-icon128.png")
    (*:setButtonPressedInfo malware-weapon-button "Interface/malware-weapon-icon128.png"
                            (ColorRGBA 1.0 0.5 0.0 1.0))
    (*:setText malware-weapon-button "Malware")
    (*:setTextAlign malware-weapon-button align:Center)
    (*:setTextVAlign malware-weapon-button valign:Bottom)
    (*:setFontSize malware-weapon-button 20)
    (*:addButton weapons-group malware-weapon-button)
    (*:addChild weapons-palette malware-weapon-button)
    ;; bots weapon button
    (*:setButtonIcon bots-weapon-button 128 128 "Interface/bots-weapon-icon128.png")
    (*:setButtonPressedInfo bots-weapon-button "Interface/bots-weapon-icon128.png"
                            (ColorRGBA 0.0 6.0 1.0 1.0))
    (*:setText bots-weapon-button "Bots")
    (*:setTextAlign bots-weapon-button align:Center)
    (*:setTextVAlign bots-weapon-button valign:Bottom)
    (*:setFontSize bots-weapon-button 20)
    (*:addButton weapons-group bots-weapon-button)
    (*:addChild weapons-palette bots-weapon-button)
    (*:addElement screen weapons-palette)))


;;; (init-name-palette state::CharacterCreatorAppState screen::Screen client::SimpleApplication)
;;; ---------------------------------------------------------------------
;;; construct the name palette

(define (init-name-palette state::CharacterCreatorAppState screen::Screen client::SimpleApplication)
  (let* ((name-palette::Window (make-name-palette state screen)))
    (*:setWindowTitle name-palette "Choose a Name:")
    (*:setNamePalette state name-palette)
    (*:addElement screen name-palette)))


;;; (init-augments-palette state::CharacterCreatorAppState screen::Screen client::SimpleApplication)
;;; ---------------------------------------------------------------------
;;; construct the augments palette

(define (init-augments-palette state::CharacterCreatorAppState screen::Screen client::SimpleApplication)
  (let* ((align BitmapFont:Align)
         (valign BitmapFont:VAlign)
         (augments-palette (make-augment-palette screen))
         (force-augment-button (make-force-augment-button screen))
         (optics-augment-button (make-optics-augment-button screen))
         (portals-augment-button (make-portals-augment-button screen))
         (turrets-augment-button (make-turrets-augment-button screen))
         (augments-group (AugmentsButtonGroup screen "AugmentsGroup")))
    (*:setWindowTitle augments-palette "Choose Augments:")
    (*:setAppState augments-group state)
    ;; force augment button
    (*:setButtonIcon force-augment-button 128 128 "Interface/force-augment-icon128.png")
    (*:setButtonPressedInfo force-augment-button "Interface/force-augment-icon128.png"
                            (ColorRGBA 1.0 1.0 0.0 1.0))
    (*:setText force-augment-button "Force Fields")
    (*:setTextAlign force-augment-button align:Center)
    (*:setTextVAlign force-augment-button valign:Bottom)
    (*:setFontSize force-augment-button 20)
    (*:addButton augments-group force-augment-button)
    (*:addChild augments-palette force-augment-button)
    ;; optics augment button
    (*:setButtonIcon optics-augment-button 128 128 "Interface/optics-augment-icon128.png")
    (*:setButtonPressedInfo optics-augment-button "Interface/optics-augment-icon128.png"
                            (ColorRGBA 0.0 1.0 0.0 1.0))
    (*:setText optics-augment-button "Optics")
    (*:setTextAlign optics-augment-button align:Center)
    (*:setTextVAlign optics-augment-button valign:Bottom)
    (*:setFontSize optics-augment-button 20)
    (*:addButton augments-group optics-augment-button)
    (*:addChild augments-palette optics-augment-button)
    ;; portals augment button
    (*:setButtonIcon portals-augment-button 128 128 "Interface/portals-augment-icon128.png")
    (*:setButtonPressedInfo portals-augment-button "Interface/portals-augment-icon128.png"
                            (ColorRGBA 1.0 0.5 0.0 1.0))
    (*:setText portals-augment-button "Portals")
    (*:setTextAlign portals-augment-button align:Center)
    (*:setTextVAlign portals-augment-button valign:Bottom)
    (*:setFontSize portals-augment-button 20)
    (*:addButton augments-group portals-augment-button)
    (*:addChild augments-palette portals-augment-button)
    ;; turrets augment button
    (*:setButtonIcon turrets-augment-button 128 128 "Interface/turrets-augment-icon128.png")
    (*:setButtonPressedInfo turrets-augment-button "Interface/turrets-augment-icon128.png"
                            (ColorRGBA 0.0 6.0 1.0 1.0))
    (*:setText turrets-augment-button "Turrets")
    (*:setTextAlign turrets-augment-button align:Center)
    (*:setTextVAlign turrets-augment-button valign:Bottom)
    (*:setFontSize turrets-augment-button 20)
    (*:addButton augments-group turrets-augment-button)
    (*:addChild augments-palette turrets-augment-button)
    (*:addElement screen augments-palette)))

;;; (init-lighting state::CharacterCreatorAppState client::SimpleApplication)
;;; ---------------------------------------------------------------------
;;; set up the scene lighting

(define (init-lighting state::CharacterCreatorAppState client::FabricClient)
  (let* ((asset-manager::AssetManager (get-asset-manager))
         (bloom (BloomFilter BloomFilter:GlowMode:Objects))
         (filter-processor::FilterPostProcessor (FilterPostProcessor asset-manager))
         (viewport::ViewPort (*:getViewPort client)))
    (*:setDownSamplingFactor bloom 2.0)
    (*:setBloomIntensity bloom 2.0)
    (*:addFilter filter-processor bloom)
    (*:addProcessor viewport filter-processor)))

;;; (init-character-creator state::CharacterCreatorAppState client::SimpleApplication)
;;; ---------------------------------------------------------------------
;;; construct the character creator's scene and UI

(define (init-character-creator state::CharacterCreatorAppState mgr::AppStateManager client::SimpleApplication)
  (*:setApp state client)
  (let* ((screen (Screen client))
         (gui-node (*:getGuiNode client))
         (root-node (*:getRootNode (as SimpleApplication client)))
         (align BitmapFont:Align)
         (valign BitmapFont:VAlign))
    ;; setup the lighting
    (init-lighting state client)
    ;; build the UI elements
    (init-character-creator-sky client)
    (init-character-creator-model state client)
    (init-character-nameplate state screen client)
    (init-faction-nameplate state screen client)
    (init-faction-palette state screen client)
    (init-armor-palette state screen client)
    (init-weapons-palette state screen client)
    (init-name-palette state screen client)
    (init-augments-palette state screen client)
    ;; add the gui to the scene
    (*:addControl gui-node screen))
  (set-current-fabric-name! state (blank-fabric-name)))


;;; (update-state-for-faction state::CharacterCreatorAppState faction)
;;; ---------------------------------------------------------------------
;;; updates the display state of the character creator's user interface
;;; in response to any changes made by the user or the game

(define (update-state-for-faction state::CharacterCreatorAppState faction)
  (let* ((fnameplate::TLabel (*:getFactionNameplate state))
         (character-name (*:getCharacterName state))
         (faction-name (case faction
                         ((caretakers) "Faction: Caretakers")
                         ((rogues) "Faction: Rogues")
                         ((abjurers) "Faction: Abjurers")
                         (else ""))))
    (*:setText fnameplate faction-name)
    (update-character-model! state)))



;;; (set-current-faction state::CharacterCreatorAppState faction)
;;; ---------------------------------------------------------------------
;;; updates the currently-selected player faction in the character
;;; creator in response to a user selection

(define (set-current-faction state::CharacterCreatorAppState faction)
  (if (member faction '(caretakers rogues abjurers))
      (begin (*:setCurrentFaction state faction)
             (update-state-for-faction state faction))
      (error "set-current-faction: unrecognized faction: " faction)))

;;; (set-current-armor state::CharacterCreatorAppState armor)
;;; ---------------------------------------------------------------------
;;; updates the currently-selected player armor in the character
;;; creator in response to a user selection

(define (set-current-armor state::CharacterCreatorAppState armor)
  (let* ((current-armor (*:getCharacterArmor state))
         (character (*:getCurrentCharacter state))
         (node::Node (get-property character 'node:)))
    (if (not (jnull? current-armor))
        (begin (*:detachChild node current-armor)
               (*:setCharacterArmor state #!null)))
    (case armor
      ((absorb-armor)(begin (*:setCharacterArmor state (make-absorb-armor))
                            (*:attachChild node (*:getCharacterArmor state))))
      ((regenerate-armor) (begin (*:setCharacterArmor state (make-regenerate-armor))
                                 (*:attachChild node (*:getCharacterArmor state))))
      ((power-armor) (begin (*:setCharacterArmor state (make-power-armor))
                            (*:attachChild node (*:getCharacterArmor state))))
      ((energy-armor) (begin (*:setCharacterArmor state (make-energy-armor))
                             (*:attachChild node (*:getCharacterArmor state))))
      ;; not a known type of armor; ignore it 
      (else 'do-nothing))))



;;; (set-current-weapon state::CharacterCreatorAppState weapon)
;;; ---------------------------------------------------------------------
;;; updates the currently-selected player weapon in the character
;;; creator in response to a user selection

(define (set-current-weapon state::CharacterCreatorAppState weapon)
  (let* ((current-weapon (*:getCharacterWeapon state))
         (character (*:getCurrentCharacter state))
         (node::Node (get-property character 'node:)))
    (if (not (jnull? current-weapon))
        (begin (*:detachChild node current-weapon)
               (*:setCharacterWeapon state #!null)))
    (case weapon
      ((cannon-weapons)(begin (*:setCharacterWeapon state (make-cannon-weapons))
                              (*:attachChild node (*:getCharacterWeapon state))))
      ((impulse-weapons) (begin (*:setCharacterWeapon state (make-impulse-weapons))
                                (*:attachChild node (*:getCharacterWeapon state))))
      ((malware-weapons) (begin (*:setCharacterWeapon state (make-malware-weapons))
                                (*:attachChild node (*:getCharacterWeapon state))))
      ((bots-weapons) (begin (*:setCharacterWeapon state (make-bots-weapons))
                             (*:attachChild node (*:getCharacterWeapon state))))
      ;; not a known type of weapon; ignore it 
      (else 'do-nothing))))


;;; (set-current-augment state::CharacterCreatorAppState augment)
;;; ---------------------------------------------------------------------
;;; updates the currently-selected player augment in the character
;;; creator in response to a user selection

(define (set-current-augment state::CharacterCreatorAppState augment)
  (let* ((current-augment (*:getCharacterAugment state))
         (character (*:getCurrentCharacter state))
         (node::Node (get-property character 'node:)))
    (if (not (jnull? current-augment))
        (begin (*:detachChild node current-augment)
               (*:setCharacterAugment state #!null)))
    (case augment
      ((force-augment)(begin (*:setCharacterAugment state (make-force-augment))
                              (*:attachChild node (*:getCharacterAugment state))))
      ((optics-augment) (begin (*:setCharacterAugment state (make-optics-augment))
                                (*:attachChild node (*:getCharacterAugment state))))
      ((portals-augment) (begin (*:setCharacterAugment state (make-portals-augment))
                                 (*:attachChild node (*:getCharacterAugment state))))
      ((turrets-augment) (begin (*:setCharacterAugment state (make-turrets-augment))
                                 (*:attachChild node (*:getCharacterAugment state))))
      ;; not a known type of augment; ignore it 
      (else 'do-nothing))))

;;; (current-fabric-name app-state::CharacterCreatorAppState)
;;; ---------------------------------------------------------------------
;;; returns the FabricName that is currently selected in the name palette

(define (current-fabric-name app-state::CharacterCreatorAppState)
  (*:getCharacterName app-state))


;;; (set-current-fabric-name! app-state new-name)
;;; ---------------------------------------------------------------------
;;; sets the FabricName that is currently selected in the name palette

(define (set-current-fabric-name! app-state::CharacterCreatorAppState new-name)
  (*:setCharacterName app-state new-name)
  (let* ((name-strings (fabric-name-strings new-name))
         (name-parts (filter (lambda (nm)(not (equal? "" nm))) name-strings))
         (name-string (apply string-append (interpose " " name-parts)))
         (nameplate::TLabel (*:getCharacterNameplate app-state)))
    (*:setText nameplate name-string)))


;;; BUGFIX: non-unique elements zero in the domains meant
;;; that selecting blank names didn't work; a blank name
;;; was always being interpreted as being from domain 0
;;; fixed by using the index of the menu  to figure out
;;; which domain is meant
(define (menu-id->domain-index menu-id)
  (cond
   ((equal? menu-id "Domain0Menu") 0)
   ((equal? menu-id "Domain1Menu") 1)
   ((equal? menu-id "Domain2Menu") 2)
   ((equal? menu-id "Domain3Menu") 3)
   ((equal? menu-id "Domain4Menu") 4)
   ((equal? menu-id "Domain5Menu") 5)
   ((equal? menu-id "Domain6Menu") 6)
   ((equal? menu-id "Domain7Menu") 7)
   (else -1)))

;;; (notify-name-selection-changed app-state index value)
;;; ---------------------------------------------------------------------
;;; handles a change to the currently-selected FabricName caused by
;;; a UI event

(define (notify-name-selection-changed app-state menu::NameMenu menu-index value)
  (let* ((current-name (current-fabric-name app-state))
         (menu-id (*:getUID menu))
         (domain-index (menu-id->domain-index menu-id))
         (new-name (update-fabric-name current-name domain-index menu-index)))
    (set-current-fabric-name! app-state new-name)
    (update-character-model! app-state)))


