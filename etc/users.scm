;;; setting up base Fabric user accounts
;;; salt = (compute-random-salt)
;;; password = (text->digest <password> <salt>)

;;; ---------------------------------------------------------------------
;;; the base users
;;; ---------------------------------------------------------------------

(define $mikel
  '(user username: "mikel" id: "fe8e7652-5799-43ce-9b1e-4d9b583e87bb"
         password: "iulQwBjPrFaPyHM+5349TaGD3qg="
         salt: (-45 -104 13 5 -83 -75 45 -80) name: "mikel evins"
         roles: ("creator" "developer" "admin" "player")))

(define $corey
  '(user username: "corey" id: "d553cdb4-2fad-4f40-be73-c56e7c2d3eeb"
         password: "zE8bsOPHoYVT56PuUlxQ7xPuW5g="
         salt: (64 120 -40 -2 -117 70 -126 122) name: "Corey Gagnon"
         roles: ("developer" "admin" "player")))

;;; ---------------------------------------------------------------------
;;; creating the users database
;;; ---------------------------------------------------------------------
;;; requires system-store.scm

