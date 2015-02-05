;;; Fabric user accounts
;;; salt = (compute-random-salt)
;;; password = (text->digest <password> <salt>)

;;; ---------------------------------------------------------------------
;;; known users
;;; ---------------------------------------------------------------------
;;; a user with an empty password is disabled

(user username: "mikel" id: "fe8e7652-5799-43ce-9b1e-4d9b583e87bb"
      name: "mikel evins" password: "iulQwBjPrFaPyHM+5349TaGD3qg="
      salt: (-45 -104 13 5 -83 -75 45 -80)
      capabilities: ("all"))

(user username: "corey" id: "d553cdb4-2fad-4f40-be73-c56e7c2d3eeb"
      name: "Corey Gagnon" password: ""
      salt: (64 120 -40 -2 -117 70 -126 122) 
      capabilities: ("all"))


