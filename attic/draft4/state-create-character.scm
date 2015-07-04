;;;; ***********************************************************************
;;;;
;;;; Name:          state-create-character.scm
;;;; Project:       The Fabric: a far-future MMORPG
;;;; Purpose:       fabric AppStates
;;;; Author:        mikel evins
;;;; Copyright:     2015 by mikel evins
;;;;
;;;; ***********************************************************************
(format #t "~%loading state-create-character.scm")

(module-export
 CreateCharacterState
 make-create-character-state)

;;; ---------------------------------------------------------------------
;;; required modules
;;; ---------------------------------------------------------------------

(require "util-error.scm")
(require "util-java.scm")
(require "util-lists.scm")
(require "data-nodes.scm")
(require "syntax-classes.scm")
(require "client-state.scm")

;;; ---------------------------------------------------------------------
;;; Java imports
;;; ---------------------------------------------------------------------

(import-as Application com.jme3.app.Application)
(import-as AppStateManager com.jme3.app.state.AppStateManager)

;;; ---------------------------------------------------------------------
;;; the client character-creator AppState class
;;; ---------------------------------------------------------------------

;;; CLASS CreateCharacterState
;;; ---------------------------------------------------------------------

(defclass CreateCharacterState (FabricClientState)
  (slots:
   (initialized? init-form: #f getter: getInitialized setter: setInitialized)) 
  (methods:
   ((cleanup) (%create-character-state-cleanup (this)))
   ((initialize) (%create-character-state-initialize (this)))
   ((isEnabled) (%create-character-state-enabled? (this)))
   ((isInitialized) (%create-character-state-initialized? (this)))
   ((stateAttached state-manager::AppStateManager)
    (%create-character-state-attached (this) state-manager))
   ((stateDetached state-manager::AppStateManager)
    (%create-character-state-detached (this) state-manager))))

(define (%create-character-state-cleanup state::CreateCharacterState)
  (format #t "~%%create-character-state-cleanup called"))

(define (%create-character-state-initialize state::CreateCharacterState)
  (format #t "~%%create-character-state-initialize called"))

(define (%create-character-state-enabled? state::CreateCharacterState) #t)

(define (%create-character-state-initialized? state::CreateCharacterState) #t)

(define (%create-character-state-attached state::CreateCharacterState manager::AppStateManager)
  (format #t "~%%create-character-state-attached called"))

(define (%create-character-state-detached state::CreateCharacterState manager::AppStateManager)
  (format #t "~%%create-character-state-detached called"))

(define (make-create-character-state client::Application)
  (let ((state (CreateCharacterState)))
    (*:setClient state client)
    state))
