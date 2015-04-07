;;;; ***********************************************************************
;;;;
;;;; Name:          client-state-create-character.scm
;;;; Project:       The Fabric: a far-future MMORPG
;;;; Purpose:       fabric AppStates
;;;; Author:        mikel evins
;;;; Copyright:     2015 by mikel evins
;;;;
;;;; ***********************************************************************
(format #t "~%loading client-state-create-character.scm")

(module-export
 CreateCharacterClientState
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

;;; CLASS CreateCharacterClientState
;;; ---------------------------------------------------------------------

(defclass CreateCharacterClientState (FabricClientState)
  (slots:
   (initialized? init-form: #f getter: getInitialized setter: setInitialized)) 
  (methods:
   ((cleanup) (%create-character-client-state-cleanup (this)))
   ((initialize) (%create-character-client-state-initialize (this)))
   ((isEnabled) #t)
   ((isInitialized) (*:getInitialized (this)))
   ((stateAttached state-manager::AppStateManager)
    (%create-character-client-state-attached (this) state-manager))
   ((stateDetached state-manager::AppStateManager)
    (%create-character-client-state-detached (this) state-manager))))

(define (%create-character-client-state-cleanup state::CreateCharacterClientState)
  #!void)

(define (%create-character-client-state-initialize state::CreateCharacterClientState)
  #!void)

(define (%create-character-client-state-attached state::CreateCharacterClientState manager::AppStateManager)
  #!void)

(define (%create-character-client-state-detached state::CreateCharacterClientState manager::AppStateManager)
  #!void)

(define (make-create-character-state client::Application)
  (let ((state (CreateCharacterClientState)))
    (*:setClient state client)
    state))
