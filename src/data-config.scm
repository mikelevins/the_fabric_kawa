;;;; ***********************************************************************
;;;;
;;;; Name:          data-config.scm
;;;; Project:       The Fabric: a far-future MMORPG
;;;; Purpose:       storing client and server configuration data
;;;; Author:        mikel evins
;;;; Copyright:     2015 by mikel evins
;;;;
;;;; ***********************************************************************

(module-export
 ClientConfiguration
 ServerConfiguration
 client-configuration
 default-client-configuration
 default-configuration-path
 get-configuration-path
 load-client-configuration
 load-server-configuration
 make-client-configuration
 make-server-configuration
 save-client-configuration
 save-server-configuration
 server-configuration)

(require version)
(require data-config)
(require data-sexp)
(require data-file)

(define (default-configuration-path)
  "/usr/local/fabric/conf")

(define client-configuration (make-parameter '()))
(define server-configuration (make-parameter '()))

(define (get-configuration-path)
  (let ((env-val (get-environment-variable "FABRIC_CONFIGURATION_PATH")))
    (or env-val
        (default-configuration-path))))

(define-simple-class ClientConfiguration ()
  ;; slots
  (version init: #!null)
  (host init: #!null)
  (port init: #!null)
  (default-username init: #!null)
  ((*init* a-version a-host a-port uname)
   (set! version a-version)
   (set! host a-host)
   (set! port a-port)
   (set! default-username uname)))

(define (make-client-configuration
         #!key
         (version (fabric-version-string))
         (host "localhost")
         (port 6143)
         (default-username "fabric"))
  (ClientConfiguration version host port default-username))

(define (default-client-configuration)
  (make-client-configuration
   version: (fabric-version-string)
   host: "localhost"
   port: 6143
   default-username: "fabric"))

(define (save-client-configuration conf::ClientConfiguration)
  (let* ((conf-path (get-configuration-path))
         (save-path (string-append conf-path "/client.conf"))
         (conf-sexp (object->s-expression conf)))
    (write-sexp conf-sexp save-path)))

(define (load-client-configuration)
  (let* ((conf-path (get-configuration-path))
         (load-path (string-append conf-path "/client.conf")))
    (if (file-exists? load-path)
        (let* ((conf-sexp (read-file load-path default: #f))
               (conf (if conf-sexp
                         (s-expression->object conf-sexp)
                         (default-client-configuration))))
          (client-configuration conf)
          conf)
        (begin (client-configuration (default-client-configuration))
               (client-configuration)))))

(define-simple-class ServerConfiguration ()
  ;; slots
  (version init: #!null)
  (port init: #!null)
  ((*init* a-version a-port)
   (set! version a-version)
   (set! port a-port)))

(define (make-server-configuration
         #!key
         (version (fabric-version-string))
         (port 6143))
  (ServerConfiguration version port))

(define (save-server-configuration conf::ServerConfiguration)
  (let* ((conf-path (get-configuration-path))
         (save-path (string-append conf-path "/server.conf"))
         (conf-sexp (object->s-expression conf)))
    (write-sexp conf-sexp save-path)))

(define (load-server-configuration)
  (let* ((conf-path (get-configuration-path))
         (load-path (string-append conf-path "/server.conf"))
         (conf-sexp (read-file load-path))
         (conf (s-expression->object conf-sexp)))
    (server-configuration conf)
    conf))

;;; (define $client-conf (make-client-configuration))
;;; (object->s-expression $client-conf)
;;; (save-client-configuration $client-conf)
;;; (define $client-conf2 (load-client-configuration))
;;; (object->s-expression $client-conf)
;;; (object->s-expression $client-conf2)

;;; (define $server-conf (make-server-configuration))
;;; (object->s-expression $server-conf)
;;; (save-server-configuration $server-conf)
;;; (define $server-conf2 (load-server-configuration))
;;; (object->s-expression $server-conf)
;;; (object->s-expression $server-conf2)


