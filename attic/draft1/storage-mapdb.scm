;;;; ***********************************************************************
;;;;
;;;; Name:          storage-users.scm
;;;; Project:       The Fabric: a far-future MMORPG
;;;; Purpose:       user accounts
;;;; Author:        mikel evins
;;;; Copyright:     2014 by mikel evins
;;;;
;;;; ***********************************************************************

(module-export )

(require "util-java.scm")
(require "syntax-classes.scm")

(import-as DBMaker org.mapdb.DBMaker)
(import-as File java.io.File)

;;; (define $db (*:make (DBMaker:newFileDB (File "/Users/mikel/Desktop/fabric-store/users.store"))))
;;; (define $treemap (*:getTreeMap $db "map"))
;;; MapDB barfs on kawa lists, but stores vectors without trouble
;;; (*:put $treemap "mikel" (vector 'user username: "mikel"))
;;; (*:commit $db)
;;; (*:get $treemap "mikel")
;;; (*:close $db)


