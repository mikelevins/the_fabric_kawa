;;; static Fabric user accounts
;;; salt = (compute-random-salt)
;;; password = (text->digest <password> <salt>)

(user username: "mikel" password: "iulQwBjPrFaPyHM+5349TaGD3qg="
      salt: (-45 -104 13 5 -83 -75 45 -80) name: "mikel evins"
      roles: ("creator" "developer" "admin" "player"))

(user username: "corey" password: "zE8bsOPHoYVT56PuUlxQ7xPuW5g="
      salt: (64 120 -40 -2 -117 70 -126 122) name: "Corey Gagnon"
      roles: ("developer" "admin" "player"))

