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

(require "syntax-classes.scm")

(definterface ConspServer ()
  (answerError message info)
  (askKeys path info)
  (answerKeys keyList info)
  (askGet key info)
  (answerGet obj info)
  (askPut path obj info)
  (answerPut info)
  (askCopy srcPath destPath info)
  (answerCopy info)
  (askMove srcPath destPath info)
  (answerMove info)
  (askDelete path info)
  (answerDeleted info))

