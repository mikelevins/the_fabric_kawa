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
      roles: ("creator" "developer" "admin" "player"))

(user username: "corey" id: "d553cdb4-2fad-4f40-be73-c56e7c2d3eeb"
      name: "Corey Gagnon" password: ""
      salt: (64 120 -40 -2 -117 70 -126 122) 
      roles: ("developer" "admin" "player"))

(user username: "granny" id: "0a448afc-1201-4816-ace8-6d0d8529cfcc"
      name: "Sally Schuster" password: ""
      salt: (-47 -121 -44 120 -107 113 -120 58) 
      roles: ("player"))

(user username: "jayare" id: "0c1b235d-2766-4185-b062-9ca55a1692de"
      name: "John Rabasa" password: ""
      salt: (13 87 -113 -8 100 -98 -40 83)
      roles: ("player"))

(user username: "jay" id: "1de6103b-dab3-497e-aa0d-8b71f33e86da"
      name: "Jay Logan" password: ""
      salt: (-128 58 26 102 51 66 -3 4)
      roles: ("player"))

(user username: "beth" id: "f9f18350-fa23-47fb-a3d4-690dadfa2ebf"
      name: "Beth Stone" password: ""
      salt: (-123 1 44 84 -47 -73 31 -111)
      roles: ("player"))

(user username: "dick" id: "e429993c-9355-44e6-a6e0-38807573232d"
      name: "Dick Jarvinen" password: ""
      salt: (-88 -55 81 -8 50 54 32 12)
      roles: ("player"))


