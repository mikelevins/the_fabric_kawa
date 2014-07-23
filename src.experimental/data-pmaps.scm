;;;; ***********************************************************************
;;;; Name:          data-pmaps.scm
;;;; Project:       The Fabric: a far-future MMORPG
;;;; Purpose:       conveniences for working with map structures
;;;; Author:        mikel evins
;;;; Copyright:     2014 by mikel evins
;;;;
;;;; ***********************************************************************

(module-export make-hash-pmap hashpmap map-keys)

(define-private-alias HashPMap org.pcollections.HashPMap)
(define-private-alias HashTreePMap org.pcollections.HashTreePMap)
(define-private-alias LList gnu.lists.LList)
(define-private-alias Map java.util.Map)

(define (make-hash-pmap k/v-plist)
  (let ((hpmap (HashTreePMap:empty)))
    (let loop ((kvs k/v-plist)
               (result::HashPMap hpmap))
      (if (null? kvs)
          result
          (if (null? (cdr kvs))
              (error "Malformed init args to make-hash-pmap: ~S" k/v-plist)
              (let ((k (car kvs))
                    (v (cadr kvs)))
                (loop (cddr kvs)
                      (*:plus result k v))))))))

(define (hashpmap . inits)
  (make-hash-pmap inits))

(define (map-keys pmap::Map)
  (LList:makeList (*:toArray (*:keySet pmap)) 0))

