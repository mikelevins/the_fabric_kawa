;;; ***********************************************************************
;;;;
;;;; Name:          neotest.scm
;;;; Project:       The Fabric: a far-future MMORPG
;;;; Purpose:       tests of neo4j integration
;;;; Author:        mikel evins
;;;; Copyright:     2014 by mikel evins
;;;;
;;;; ***********************************************************************

(require "util-java.scm")

(define-enum relations (KNOWS))

(import-as GraphDatabaseFactory org.neo4j.graphdb.factory.GraphDatabaseFactory)
(import-as Runtime java.lang.Runtime)
(import-as Thread java.lang.Thread)
(import-as Transaction org.neo4j.graphdb.Transaction)

(define $graph-factory (GraphDatabaseFactory))

(define graphdb (make-parameter (*:newEmbeddedDatabase $graph-factory
                                                       "/Users/mikel/Desktop/temp_fabric.db")))

;; (*:addShutdownHook (Runtime:getRuntime)
;;                    (object (Thread)
;;                            ((run) (*:shutdown (graphdb)))))

(define-syntax with-graphdb
  (syntax-rules ()
    ((with-graphdb (gdb) expr ...)
     (let ((tx (*:beginTx gdb)))
       (let ((result (begin expr ...)))
         (*:success tx)
         result)))))

(define $node1 #f)
;;(with-graphdb ((graphdb))
;;              (set! $node1 (*:createNode (graphdb))))

;; (with-graphdb ((graphdb))
;;               (*:setProperty $node1 "message" "Hello!"))

;;(with-graphdb ((graphdb))
;;              (*:getProperty $node1 "message"))

;;(*:shutdown (graphdb))



