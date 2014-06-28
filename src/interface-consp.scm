;;; ***********************************************************************
;;;;
;;;; Name:          interface-consp.scm
;;;; Project:       The Fabric: a far-future MMORPG
;;;; Purpose:       consp API
;;;; Author:        mikel evins
;;;; Copyright:     2014 by mikel evins
;;;;
;;;; ***********************************************************************

(module-export ConspServer)

(define-simple-class ConspServer () interface: #t
  ((answerError message info) #!abstract)
  ((askKeys path info) #!abstract)
  ((answerKeys keyList info) #!abstract)
  ((askGet key info) #!abstract)
  ((answerGet obj info) #!abstract)
  ((askPut path obj info) #!abstract)
  ((answerPut info) #!abstract)
  ((askCopy srcPath destPath info) #!abstract)
  ((answerCopy info) #!abstract)
  ((askMove srcPath destPath info) #!abstract)
  ((answerMove info) #!abstract)
  ((askDelete path info) #!abstract)
  ((answerDeleted info) #!abstract))

