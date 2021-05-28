En primer lugar, se debe verificar que todos los archivos anteriores se encuentren en la misma carpeta de raíz, de otro modo DrRacket arrojará un error que no encuentra archivos necesarios para la ejecución del main.
Luego, se debe abrir el archivo “main_203877544_CristhoferParada.rkt” y hacerlo correr buscando el botón “Run” en la esquina superior derecha. Con esto el programa ya estaría en ejecución a la espera de que se ingresen los comandos.
Lo primero que podemos hacer para seguir el curso normal del programa, seria crear una red social vacía, lo cual puede ser logrado por el comando “crearRedSocial <nombre> <fecha> <encrypt> <encrypt>” lo cual devuelve una red social vacía. Con esto hecho, podemos registrar un usuario de nombre “Harunomi”, con una contraseña “123” de la siguiente manera: “(register <nombreRedSocial> <fecha de registro> <”Harunomi”> <”123”>)”. Lo cual nos retornaría una red social nueva con el usuario “Harunomi” registrado en la lista de usuarios.
Dentro de la función main.rkt en el encabezado de cada función, se encontrarán comentarios con diversas maneras en las que se puede probar cada función, es recomendable que se ejecuten en el orden que se muestran.
Una forma en la que puedes ver de mejor manera   el contenido de una red social es mediante la función “socialnetwork->string” la cual, dada una red social existente, permite mostrar información referente a ella de una manera mucho más comprensible que la dada por DrRacket. Una forma en la que se puede usar es la siguiente (asumiendo al existencia de una red social) “(display (socialnetwork->string <red  social>))”

Dada la naturaleza de este paradigma, se recomienda que cada comando que se haga sea re-definido  con nombres que sean comprensivos, esto es para de alguna forma “ir guardando” las redes sociales que son resultantes de los comandos utilizados. 

SCRIPTS DE PRUEBA
(define emptyTwitter (crearRedSocial "Twitter" '(20 5 2021) encrypt encrypt))
(define emptyFacebook (crearRedSocial "Facebook" '(20 5 2021) encrypt encrypt))
(define emptyRs (crearRedSocial "RedSocial" '(20 5 2021) encrypt encrypt))
(define twitter1 (register emptyTwitter '(20 5 2021) "Haru" "123"))
(define twitter2 (register twitter1 '(20 5 2021) "Haru" "123")) 
(define twitter3 (register(register(register emptyTwitter '(25 5 2021) "Harunomi" "321") '(25 5 2021) "Cybele" "uwu") '(25 5 2021) "Hylia" "ily"))
(define twitter4 (login twitter3 "Harunomi" "321" display))
(define twitter5 (login twitter3 "Cybele" "uwu" car))
(define twitter6 (login twitter3 "Hylia" "ily" cdr))
(define twitter7 (((login twitter2 "Harunomi" "321" post) '(20 5 2021)) "Mi primer HolaMundo")
(define twitter8 (((login twitter7 "Hylia" "ily" post) '(20 5 2021)) "Contenta en la nueva red Social")
(define twitter9 (((login twitter8 "Cybele" "uwu" post) '(20 5 2021)) "gracias por invitarme a la red social" "Harunomi") 
(define twitter10 (((login twitter8 "Cybele" "uwu" follow) '(22 5 2021)) "Harunomi")
(define twitter11 (((login twitter10 "Hylia" "ily" follow) '(23 5 2021)) "Harunomi")
(define twitter12 (((login twitter11 "Harunomi" "321" follow) '(23 5 2021)) "Hylia")
(define twitter13 (((login twitter12 "Harunomi" "321" share) '(23 5 2021)) 2)
(define twitter14 (((login twitter13 "Harunomi" "321" share) '(23 5 2021)) 3)
(define twitter15 (((login twitter14 "Cybele" "uwu" share) '(23 5 2021)) 1) ; 
(display (socialnetwork->string twitter14)
(display (socialnetwork->string twitter13)
(login twitter14 "Harunomi" "321" socialnetwork->string)
(define twitter17 (((login twitter14 "Cybele" "uwu" comment) '(28 5 2021)) 1) "Dame follow"))
(define twitter18 (((login twitter17 "Harunomi" "321" comment) '(28 5 2021)) 4) "De inmediato"))
(define twitter19 (((login twitter18 "Hylia" "ily" comment) '(28 5 2021)) 5) "No lo sigas, es una cuenta maliciosa"))
(define twitter20 (((login twitter19 "Harunomi" "321" like) '(27 5 2021)) 2))
(define twitter21 (((login twitter20 "Hylia" "Ily" like) '(27 5 2021)) 2))
(define twitter22 (((login twitter21 "Harunomi" "321" like) '(27 5 2021)) 4))
(viral twitter22 2) 
(viral twitter22 1)
(viral twitter22 4) 