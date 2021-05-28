#lang racket
(require "tdaSocialNetwork_203877544_CristhoferParada.rkt")
(require "tdaUser_203877544_CristhoferParada.rkt")
(require "tdaPublicacion_203877544_CristhoferParada.rkt")
(require "tdaFecha_203877544_CristhoferParada.rkt")


; (define emptyTwitter (crearRedSocial "Twitter" '(20 5 2021) encrypt encrypt))
; (define emptyFacebook (crearRedSocial "Facebook" '(20 5 2021) encrypt encrypt))
; (define emptyRs (crearRedSocial "RedSocial" '(20 5 2021) encrypt encrypt))


; (define twitter1 (register emptyTwitter '(20 5 2021) "Haru" "123"))
; (define twitter2 (register twitter1 '(20 5 2021) "Haru" "123")) ejemplo de usuario ya existente
; (define twitter3 (register(register(register emptyTwitter '(25 5 2021) "Harunomi" "321") '(25 5 2021) "Cybele" "uwu") '(25 5 2021) "Hylia" "ily"))
; Descripcion: Funcion que permite registrar a un nuevo usuario en la red social
; Dominio: string x fecha x string x string
; Recorrido: TDA SocialNetwork
(define register
  (lambda (nombreRed fecha nombreUsuario password)
    (if (esRedSocial? nombreRed)
        (crearRedSocialAux (getNombreRedSocial nombreRed) (getFechaRedSocial nombreRed)
                           (agregarUsuarioRS (crearUsuario (IDcounter (getUsuariosRedSocial nombreRed))
                                                           ((getEncryptFn nombreRed) nombreUsuario)
                                                           ((getEncryptFn nombreRed) password)
                                                           fecha
                                                           null
                                                           null)
                                             (getUsuariosRedSocial nombreRed))
                           (getPublicacionesRedSocial nombreRed)
                           (getUsuarioOnline nombreRed)
                           (getEncryptFn nombreRed)
                           (getDecryptFn nombreRed))
        null
        )
    )   
  )


; (define twitter4 (login twitter3 "Harunomi" "321" display)) mostraria toda la red social
; (define twitter5 (login twitter3 "Cybele" "uwu" car)) mostraria el primer elemento de una red social, lo que esto seria el nombre
; (define twitter6 (login twitter3 "Hylia" "ily" cdr)) mostraria toda la red social, excepto el primer dato
; Descripcion: Funcion que permite loggear a un usuario dentro de la red social y ejecutar una operacion
; dominio: socialnetwork x string x string x funcion
; recorrido: funcion
(define login
  (lambda (redSocial username password function)
    (if ( equal? (buscarUsuario redSocial username password) #f)
        null ; no se encontro al usuario
        (function (crearRedSocialAux (getNombreRedSocial redSocial)
                                     (getFechaRedSocial redSocial)
                                     (getUsuariosRedSocial redSocial)
                                     (getPublicacionesRedSocial redSocial)
                                     (buscarUsuario redSocial username password) ; ID del usuario loggeado
                                     (getEncryptFn redSocial)
                                     (getDecryptFn redSocial)))
        )
    )
  )

; (define twitter7 (((login twitter2 "Harunomi" "321" post) '(20 5 2021)) "Mi primer HolaMundo")
; (define twitter8 (((login twitter7 "Hylia" "ily" post) '(20 5 2021)) "Contenta en la nueva red Social")
; (define twitter9 (((login twitter8 "Cybele" "uwu" post) '(20 5 2021)) "gracias por invitarme a la red social" "Harunomi") post fallido puesto que de momento "Harunomi" no sigue a "Cybele"
; Descripcion: funcion que le permite a un usuario loggeado realizar una publicacion.
; Dominio: fecha x string x tags
; recorrido: tda socialnetwork
(define (post redSocial)
  (lambda (fecha)
    (lambda (texto . tags)
      (if (equal? tags null) ;; no etiquetados
          (crearRedSocialAux (getNombreRedSocial redSocial);el usuario no etiqueto a nadie
                             (getFechaRedSocial redSocial)
                             (actualizarUsuario (getUsuariosRedSocial redSocial) ; agrego el usuario a la lista existente
                                                (crearUsuario (getUserID (IDtoUser redSocial (getUsuarioOnline redSocial))) ;creo al usuario con la publicacion nueva
                                                              (getUsername (IDtoUser redSocial (getUsuarioOnline redSocial)))
                                                              (getPassword (IDtoUser redSocial (getUsuarioOnline redSocial)))
                                                              (getUserFecha (IDtoUser redSocial (getUsuarioOnline redSocial)))
                                                              (getUserFollowers (IDtoUser redSocial (getUsuarioOnline redSocial)))
                                                              (agregarPublicacion (getUserPosts (IDtoUser redSocial (getUsuarioOnline redSocial))) ; creo la publicacion y la agrego a la lista
                                                                                  (crearPublicacion (IDcounter (getPublicacionesRedSocial redSocial))
                                                                                                    ((getEncryptFn redSocial)texto)
                                                                                                    null
                                                                                                    null
                                                                                                    fecha
                                                                                                    null
                                                                                                    null)
                                                                                  )
                                                              )
                                                )
                             (agregarPublicacion (getPublicacionesRedSocial redSocial) ; agrego la nueva publicacion a la red social
                                                 (crearPublicacion (IDcounter (getPublicacionesRedSocial redSocial))
                                                                   ((getEncryptFn redSocial)texto)
                                                                   null
                                                                   null
                                                                   fecha
                                                                   null
                                                                   null)
                                                 )
                             0 ; reinicio el iD de los usuarios en linea a cero (es decir, ninguno)
                             (getEncryptFn redSocial)
                             (getDecryptFn redSocial)
                             )                      
          (if (sonAmigos? redSocial (IDtoUser redSocial (getUsuarioOnline redSocial)) tags) ; etiquetados
              (crearRedSocialAux (getNombreRedSocial redSocial);el usuario no etiqueto a nadie
                                 (getFechaRedSocial redSocial)
                                 (actualizarUsuario (getUsuariosRedSocial redSocial) ; agrego el usuario a la lista existente
                                                    (crearUsuario (getUserID (IDtoUser redSocial (getUsuarioOnline redSocial))) ;creo al usuario con la publicacion nueva
                                                                  (getUsername (IDtoUser redSocial (getUsuarioOnline redSocial)))
                                                                  (getPassword (IDtoUser redSocial (getUsuarioOnline redSocial)))
                                                                  (getUserFecha (IDtoUser redSocial (getUsuarioOnline redSocial)))
                                                                  (getUserFollowers (IDtoUser redSocial (getUsuarioOnline redSocial)))
                                                                  (agregarPublicacion (getUserPosts (IDtoUser redSocial (getUsuarioOnline redSocial))) ; creo la publicacion y la agrego a la lista
                                                                                      (crearPublicacion (IDcounter (getPublicacionesRedSocial redSocial))
                                                                                                        ((getEncryptFn redSocial)texto)
                                                                                                        null
                                                                                                        null
                                                                                                        fecha
                                                                                                        (encriptarLista (getEncryptFn redSocial) tags)
                                                                                                        null)
                                                                                      )
                                                                  )
                                                    )
                                 (agregarPublicacion (getPublicacionesRedSocial redSocial) ; agrego la nueva publicacion a la red social
                                                     (crearPublicacion (IDcounter (getPublicacionesRedSocial redSocial))
                                                                       ((getEncryptFn redSocial)texto)
                                                                       null
                                                                       null
                                                                       fecha
                                                                       (encriptarLista (getEncryptFn redSocial) tags)
                                                                       null)
                                                     )
                                 0 ; reinicio el iD de los usuarios en linea a cero (es decir, ninguno)
                                 (getEncryptFn redSocial)
                                 (getDecryptFn redSocial)
                                 )
              (display "No puedes etiquetar a alguien que no es tu amigo")    
              )
          )
      )
    )
  )

; (define twitter10 (((login twitter8 "Cybele" "uwu" follow) '(22 5 2021)) "Harunomi")
; (define twitter11 (((login twitter10 "Hylia" "ily" follow) '(23 5 2021)) "Harunomi")
; (define twitter12 (((login twitter11 "Harunomi" "321" follow) '(23 5 2021)) "Hylia")
; Descripcion: funcion que le permite a un usuario loggeado seguir a otro, asumiendo que ambos existen
; Dominio: fecha x string
; recorrido: social network
(define (follow redSocial)
  (lambda (fecha)
    (lambda (user)
      (if (equal? (getUserID (IDtoUser redSocial (getUsuarioOnline redSocial))) (getUserID (UsernametoUser redSocial user))) ;Verificamos que el usuario loggeado no quiera seguirse a si mismo
          (display "El usuario no se puede seguir a si mismo")
          (crearRedSocialAux (getNombreRedSocial redSocial)
                             (getFechaRedSocial redSocial)
                             (actualizarUsuario (getUsuariosRedSocial redSocial)
                                                (crearUsuario (getUserID (UsernametoUser redSocial user))
                                                              (getUsername (UsernametoUser redSocial user))
                                                              (getPassword (UsernametoUser redSocial user))
                                                              (getUserFecha (UsernametoUser redSocial user))
                                                              (addNumber (getUserFollowers(UsernametoUser redSocial user)) ; agrego el follower a la lista de followers del usuario
                                                                         (getUserID (IDtoUser redSocial (getUsuarioOnline redSocial)))
                                                                         )
                                                              (getUserPosts (UsernametoUser redSocial user))
                                                              )
                                                )
                             (getPublicacionesRedSocial redSocial)
                             0
                             (getEncryptFn redSocial)
                             (getDecryptFn redSocial)
                             )
          )
      )
    )
  )

; (define twitter13 (((login twitter12 "Harunomi" "321" share) '(23 5 2021)) 2)
; (define twitter14 (((login twitter13 "Harunomi" "321" share) '(23 5 2021)) 3)
; (define twitter15 (((login twitter14 "Cybele" "uwu" share) '(23 5 2021)) 1) ; compartido fallido, puesto que "Harunomi" no sigue a "Cybele"
; Descripcion: Funcion que permite compartir una publicacion
; Dominio: date x integer x userlist
; Recorrido: tdaSocialNetwork
(define (share redSocial)
  (lambda (fecha)
    (lambda (IDpost . tags)
      (if (equal? tags null)
          (if (existePublicacion? IDpost (getPublicacionesRedSocial redSocial))
              (crearRedSocialAux (getNombreRedSocial redSocial)
                                 (getFechaRedSocial redSocial)
                                 (actualizarUsuario (getUsuariosRedSocial redSocial)
                                                    (crearUsuario (getUserID (IDtoUser redSocial (getUsuarioOnline redSocial)))
                                                                  (getUsername (IDtoUser redSocial (getUsuarioOnline redSocial)))
                                                                  (getPassword (IDtoUser redSocial (getUsuarioOnline redSocial)))
                                                                  (getUserFecha (IDtoUser redSocial (getUsuarioOnline redSocial)))
                                                                  (getUserFollowers (IDtoUser redSocial (getUsuarioOnline redSocial)))
                                                                  (agregarPublicacion (getUserPosts (IDtoUser redSocial (getUsuarioOnline redSocial)))
                                                                                      (crearPublicacion (IDcounter (getPublicacionesRedSocial redSocial))
                                                                                                        (getTextoPublicacion (IDtoPublicacion IDpost (getPublicacionesRedSocial redSocial)))
                                                                                                        null
                                                                                                        null
                                                                                                        fecha
                                                                                                        null
                                                                                                        null)                                                                                                                                                                                                      
                                                                                      )
                                                                  )
                                                    )
                                 
                                 (agregarPublicacion (actualizarPublicacion (getPublicacionesRedSocial redSocial)
                                                                            (crearPublicacion (getPublicacionID (IDtoPublicacion IDpost (getPublicacionesRedSocial redSocial)))
                                                                                              (getTextoPublicacion (IDtoPublicacion IDpost (getPublicacionesRedSocial redSocial)))
                                                                                              (getReacts (IDtoPublicacion IDpost (getPublicacionesRedSocial redSocial)))
                                                                                              (getComments (IDtoPublicacion IDpost (getPublicacionesRedSocial redSocial)))
                                                                                              (getFechaPublicacion (IDtoPublicacion IDpost (getPublicacionesRedSocial redSocial)))
                                                                                              (getTags (IDtoPublicacion IDpost (getPublicacionesRedSocial redSocial)))
                                                                                              (addNumber (getShared (IDtoPublicacion IDpost (getPublicacionesRedSocial redSocial)))
                                                                                                         (getUserID (IDtoUser redSocial (getUsuarioOnline redSocial)))
                                                                                                         )
                                                                                              )
                                                                            )
                                                     (crearPublicacion (IDcounter (getPublicacionesRedSocial redSocial)) ; agrego la nueva publicacion hecha por el usuario
                                                                       (getTextoPublicacion (IDtoPublicacion IDpost (getPublicacionesRedSocial redSocial)))
                                                                       (getReacts (IDtoPublicacion IDpost (getPublicacionesRedSocial redSocial)))
                                                                       (getComments (IDtoPublicacion IDpost (getPublicacionesRedSocial redSocial)))
                                                                       fecha
                                                                       (getTags (IDtoPublicacion IDpost (getPublicacionesRedSocial redSocial)))
                                                                       (getShared (IDtoPublicacion IDpost (getPublicacionesRedSocial redSocial)))                                                                                                                                                                                                      
                                                                       )
                                                     )              
                                 0
                                 (getEncryptFn redSocial)
                                 (getDecryptFn redSocial)
                                 )
              (display "la publicacion dada no existe en la red social")
              )
          (if (existePublicacion? IDpost (getPublicacionesRedSocial redSocial)); Caso en que haya etiquetados
              (if (sonAmigos? redSocial (IDtoUser redSocial (getUsuarioOnline redSocial)) tags)
                  (crearRedSocialAux (getNombreRedSocial redSocial)
                                     (getFechaRedSocial redSocial)
                                     (actualizarUsuario (getUsuariosRedSocial redSocial)
                                                        (crearUsuario (getUserID (IDtoUser redSocial (getUsuarioOnline redSocial)))
                                                                      (getUsername (IDtoUser redSocial (getUsuarioOnline redSocial)))
                                                                      (getPassword (IDtoUser redSocial (getUsuarioOnline redSocial)))
                                                                      (getUserFecha (IDtoUser redSocial (getUsuarioOnline redSocial)))
                                                                      (getUserFollowers (IDtoUser redSocial (getUsuarioOnline redSocial)))
                                                                      (agregarPublicacion (getUserPosts (IDtoUser redSocial (getUsuarioOnline redSocial)))
                                                                                          (crearPublicacion (IDcounter (getPublicacionesRedSocial redSocial))
                                                                                                            (getTextoPublicacion (IDtoPublicacion IDpost (getPublicacionesRedSocial redSocial)))
                                                                                                            null
                                                                                                            null
                                                                                                            fecha
                                                                                                            (encriptarLista (getEncryptFn redSocial) tags)
                                                                                                            null)
                                                                                          )
                                                                      )
                                                        )
                                     (agregarPublicacion (actualizarPublicacion (getPublicacionesRedSocial redSocial)
                                                                                (crearPublicacion (getPublicacionID (IDtoPublicacion IDpost (getPublicacionesRedSocial redSocial)))
                                                                                                  (getTextoPublicacion (IDtoPublicacion IDpost (getPublicacionesRedSocial redSocial)))
                                                                                                  (getReacts (IDtoPublicacion IDpost (getPublicacionesRedSocial redSocial)))
                                                                                                  (getComments (IDtoPublicacion IDpost (getPublicacionesRedSocial redSocial)))
                                                                                                  (getFechaPublicacion (IDtoPublicacion IDpost (getPublicacionesRedSocial redSocial)))
                                                                                                  (getTags (IDtoPublicacion IDpost (getPublicacionesRedSocial redSocial)))
                                                                                                  (addNumber (getShared (IDtoPublicacion IDpost (getPublicacionesRedSocial redSocial)))
                                                                                                             (getUserID (IDtoUser redSocial (getUsuarioOnline redSocial)))
                                                                                                             )
                                                                                                  )
                                                                                )
                                                         (crearPublicacion (IDcounter (getPublicacionesRedSocial redSocial))
                                                                           (getTextoPublicacion (IDtoPublicacion IDpost (getPublicacionesRedSocial redSocial)))
                                                                           null
                                                                           null
                                                                           fecha
                                                                           (encriptarLista (getEncryptFn redSocial) tags)
                                                                           null)
                                                         )                                                                                    
                                     )
                  (display "El usuario esta intentando etiquetar usuarios que no estan en sus lista de amigos")
                  )
              (display "No existe la publicacion ingresada en la red social")   
              )
          )
      )
    )
  )
; (display (socialnetwork->string twitter14)
; (display (socialnetwork->string twitter13)
; (login twitter14 "Harunomi" "321" socialnetwork->string)
; Descripcion: Funcion que genera un string con informacion respecto de la red social o del usuario loggeado
; Dominio: TDA socialnetwork
; recorrido: string
(define socialnetwork->string
  (lambda (redSocial)
    (if  (equal? (getUsuarioOnline redSocial) 0)
         (string-append "" ; printeo informacion de la red social
                       "La red social tiene como nombre: "
                       (getNombreRedSocial redSocial)
                       "\nFue creada el dia "
                       (number->string (getDay(getFechaRedSocial redSocial)))
                       " del mes "
                       (number->string (getMonth(getFechaRedSocial redSocial)))
                       " del año "
                       (number->string (getYear(getFechaRedSocial redSocial)))
                       "\nTiene en total "
                       (number->string (length (getUsuariosRedSocial redSocial)))
                       " usuarios, los cuales son:\n\n\n"
                       (listaUsuarios->string (getUsuariosRedSocial redSocial) "" redSocial)
                       "\nTiene un total de: "
                       (number->string (length (getPublicacionesRedSocial redSocial)))
                       " publicaciones, las cuales son:\n"
                       (listaPublicaciones->string (getPublicacionesRedSocial redSocial) "" redSocial) 
                        )
         (listaUsuarios->string (list (IDtoUser redSocial (getUsuarioOnline redSocial))) "" redSocial); printeo informacion del usuario
         
         )
    )
  )

; (define twitter17 (((login twitter14 "Cybele" "uwu" comment) '(28 5 2021)) 1) "Dame follow"))
; (define twitter18 (((login twitter17 "Harunomi" "321" comment) '(28 5 2021)) 4) "De inmediato"))
; (define twitter19 (((login twitter18 "Hylia" "ily" comment) '(28 5 2021)) 5) "No lo sigas, es una cuenta maliciosa"))
; Descripcion: funcion que permite agregar un comentario a una publicacion
; Dominio: fecha x integer x string
; Recorrido: tdaSocialNetwork
(define (comment redSocial)
  (lambda (fecha)
    (lambda (ID)
      (lambda (comentario)
        (if (string? comentario)
            (if (existePublicacion? ID (getPublicacionesRedSocial redSocial))
                (crearRedSocialAux (getNombreRedSocial redSocial)
                                   (getFechaRedSocial redSocial)
                                   (actualizarUsuario (getUsuariosRedSocial redSocial)
                                                      (crearUsuario (getUserID (IDtoUser redSocial (getUsuarioOnline redSocial)))
                                                                      (getUsername (IDtoUser redSocial (getUsuarioOnline redSocial)))
                                                                      (getPassword (IDtoUser redSocial (getUsuarioOnline redSocial)))
                                                                      (getUserFecha (IDtoUser redSocial (getUsuarioOnline redSocial)))
                                                                      (getUserFollowers (IDtoUser redSocial (getUsuarioOnline redSocial)))
                                                                      (agregarPublicacion (getUserPosts (IDtoUser redSocial (getUsuarioOnline redSocial)))
                                                                                          (crearPublicacion (IDcounter (getPublicacionesRedSocial redSocial))
                                                                                                            ((getEncryptFn redSocial) comentario)
                                                                                                            null
                                                                                                            null
                                                                                                            fecha
                                                                                                            null
                                                                                                            null)
                                                                                          )
                                                                      )
                                                      )
                                   (agregarPublicacion (actualizarPublicacion (getPublicacionesRedSocial redSocial)
                                                                              (crearPublicacion (getPublicacionID (IDtoPublicacion ID (getPublicacionesRedSocial redSocial))) ; actualizo la parte de comments de la publicacion comentada
                                                                                                (getTextoPublicacion (IDtoPublicacion ID (getPublicacionesRedSocial redSocial)))
                                                                                                (getReacts (IDtoPublicacion ID (getPublicacionesRedSocial redSocial)))
                                                                                                (addNumber (getComments (IDtoPublicacion ID (getPublicacionesRedSocial redSocial))) (IDcounter (getPublicacionesRedSocial redSocial))) ; Agrego el comentario
                                                                                                (getFechaPublicacion (IDtoPublicacion ID (getPublicacionesRedSocial redSocial)))
                                                                                                (getTags (IDtoPublicacion ID (getPublicacionesRedSocial redSocial)))
                                                                                                (getShared (IDtoPublicacion ID (getPublicacionesRedSocial redSocial)))
                                                                                                )
                                                                              )
                                                       (crearPublicacion (IDcounter (getPublicacionesRedSocial redSocial))
                                                                         ((getEncryptFn redSocial) comentario)
                                                                         null
                                                                         null
                                                                         fecha
                                                                         null
                                                                         null)
                                                       )
                                   0 ; Desconecto al usuario
                                   (getEncryptFn redSocial)
                                   (getDecryptFn redSocial)
                                   )
                (display "El id de la publicacion ingresada no existe")
                )
            (display "el comentario ingresado no es valido")
            )
        )
      )
    )
  )
; (define twitter20 (((login twitter19 "Harunomi" "321" like) '(27 5 2021)) 2))
; (define twitter21 (((login twitter20 "Hylia" "Ily" like) '(27 5 2021)) 2))
; (define twitter22 (((login twitter21 "Harunomi" "321" like) '(27 5 2021)) 4))
; Descripcion: Funcion que permite reaccionar a publicaciones
; Dominio: fecha x integer
; recorrido: tda redSocial
(define (like redSocial)
  (lambda (fecha)
    (lambda (ID)
      (if (existePublicacion? ID (getPublicacionesRedSocial redSocial))
          (crearRedSocialAux (getNombreRedSocial redSocial)
                             (getFechaRedSocial redSocial)
                             (getUsuariosRedSocial redSocial)
                             (actualizarPublicacion (getPublicacionesRedSocial redSocial)
                                                      (crearPublicacion (getPublicacionID (IDtoPublicacion ID (getPublicacionesRedSocial redSocial)))
                                                                        (getTextoPublicacion (IDtoPublicacion ID (getPublicacionesRedSocial redSocial)))
                                                                        (addNumber (getReacts (IDtoPublicacion ID (getPublicacionesRedSocial redSocial))) ; agrego la reaccion
                                                                                   (getUsuarioOnline redSocial)
                                                                                   )
                                                                        (getComments (IDtoPublicacion ID (getPublicacionesRedSocial redSocial)))
                                                                        (getFechaPublicacion (IDtoPublicacion ID (getPublicacionesRedSocial redSocial)))
                                                                        (getTags (IDtoPublicacion ID (getPublicacionesRedSocial redSocial)))
                                                                        (getShared (IDtoPublicacion ID (getPublicacionesRedSocial redSocial)))
                                                                        )
                                                      )
                             0 ; desconecto al usuario
                             (getEncryptFn redSocial)
                             (getDecryptFn redSocial)
                             )
          (display "La publicacion ingresada no existe")
          )
      )
    )
  )

; (viral twitter22 2) retornaria nulo, puesto que no habria ninguna publicacion
; (viral twitter22 1)
; (viral twitter22 4) retornaria nulo, puesto que no habria ninguna publicacion
; Descripcion: funcion que permite ver que publicaciones han sido compartida K veces
; Dominio: Tda socialnetwork x integer
; recorrido list
(define viral
  (lambda (redSocial numero)
    (if (equal? (getPublicacionesRedSocial redSocial) null)
        (list (getPublicacionesRedSocial redSocial))
        (if (equal? (length (getShared(car (getPublicacionesRedSocial redSocial)))) numero)
            (cons (car (getPublicacionesRedSocial redSocial))
                  (viral (crearRedSocialAux (getNombreRedSocial redSocial)
                                         (getFechaRedSocial redSocial)
                                         (getUsuariosRedSocial redSocial)
                                         (cdr (getPublicacionesRedSocial redSocial))
                                         (getUsuarioOnline redSocial)
                                         (getEncryptFn redSocial)
                                         (getDecryptFn redSocial)
                                         )
                         numero)
                  )
            (viral (crearRedSocialAux (getNombreRedSocial redSocial)
                                   (getFechaRedSocial redSocial)
                                   (getUsuariosRedSocial redSocial)
                                   (cdr (getPublicacionesRedSocial redSocial))
                                   (getUsuarioOnline redSocial)
                                   (getEncryptFn redSocial)
                                   (getDecryptFn redSocial)
                                   ) 
                   numero)
            )
        )
    )
  )                                                                       
     
      

; Descripcion: Funcion que agrega una ID de un usuario a la lista de numeros 
; Dominio: lista x integer
; Recorrido: list
(define addNumber
  (lambda (lista follower)
    (if (equal? lista null)
        (list follower)
        (cons (car lista) (addNumber (cdr lista) follower))
        )
    )
  )

; Descripcion: funcion que recorre una lista de usuarios y pregunta si es amigo de un usuario
; Dominio: tda socialnetwork x usuario x lista
; recorrido: bool
(define sonAmigos?
  (lambda (redSocial user tags)
    (if (null? tags)
        #t
        (if (esAmigo? (getUserID (UsernametoUser redSocial (car tags))) (getUserFollowers user))
            (sonAmigos? redSocial user (cdr tags))
            #f
            )
        )
    )
  )

; Descripcion: funcion que revisa si el usuario es amigo de otro
; Dominio: integer x list
; recorrido: bool
(define esAmigo?
  (lambda (ID followers)
    (if (equal? followers null)
        #f
        (if (equal? ID (car followers))
            #t
            (esAmigo? ID (cdr followers))
            )
        )
    )
  )


; Descripcion: funcion que permite obtener el ID del ultimo usuario/Publicacion
; dominio: lista
; recorrido: integer
(define IDcounter
  (lambda (lista)
    (if (null? lista)
        1
        (+ 1 (length lista))
        )
    )
  )

; Descripcion: funcion que permite retornar un usuario dada una ID
; Dominio: TDA SocialNetwork x ID
; Recorrido: TDA USER
(define IDtoUser
  (lambda (redSocial ID)
    (if (equal? (getUsuariosRedSocial redSocial) null)
        (display "El usuario no existe en la red social")
        (if (equal? ID (getUserID (car(getUsuariosRedSocial redSocial))))
            (car(getUsuariosRedSocial redSocial))
            (IDtoUser (crearRedSocialAux
                       (getNombreRedSocial redSocial)
                       (getFechaRedSocial redSocial)
                       (cdr (getUsuariosRedSocial redSocial)) ; cambio el valor de la lista al resto de usuarios
                       (getPublicacionesRedSocial redSocial)
                       (getUsuarioOnline redSocial)
                       (getEncryptFn redSocial)
                       (getDecryptFn redSocial)
                       ) ID)
            )
        )
    )
  )
      
; Descripcion: funcion que permite retornar un usuario dado un username
; Dominio: TDA socialNetwork x string
; Recorrido: TDA user
(define UsernametoUser
  (lambda (redSocial username)
    (if (equal? (getUsuariosRedSocial redSocial) null)
        #f ;el usuario no existe en la red social
        (if (equal? ((getEncryptFn redSocial) username) (getUsername (car(getUsuariosRedSocial redSocial))))
            (car(getUsuariosRedSocial redSocial)) ; retorno el usuario encontrado
            (UsernametoUser (crearRedSocialAux (getNombreRedSocial redSocial)
                                               (getFechaRedSocial redSocial)
                                               (cdr(getUsuariosRedSocial redSocial))
                                               (getPublicacionesRedSocial redSocial)
                                               (getUsuarioOnline redSocial)
                                               (getEncryptFn redSocial)
                                               (getDecryptFn redSocial)
                                               )
                            username)
            )
        )
    )
  )
      
      
; Descripcion: funcion que permite buscar a un usuario dentro de la lista de usuarios y devolver su ID
; Dominio: tda redsocial x string x string
; recorrido: integer / bool
(define buscarUsuario
  (lambda (redSocial username password)
    (if (equal? (getUsuariosRedSocial redSocial) null)
        #f ; no se encontro el usuario o no coincidia la password
        (if (and(equal? ((getEncryptFn redSocial) username) (getUsername (car (getUsuariosRedSocial redSocial))))
                (equal? ((getEncryptFn redSocial) password) (getPassword (car(getUsuariosRedSocial redSocial)))))
            (getUserID (car (getUsuariosRedSocial redSocial))) ; se verifica el nombre de usuario con su contrasena y retorna su ID
            (buscarUsuario (crearRedSocialAux
                            (getNombreRedSocial redSocial)
                            (getFechaRedSocial redSocial)
                            (cdr (getUsuariosRedSocial redSocial))
                            (getPublicacionesRedSocial redSocial)
                            (getUsuarioOnline redSocial)
                            (getEncryptFn redSocial)
                            (getDecryptFn redSocial))
                           username password)
            )
        )
    )
  )

;(define emptyTwitter (crearRedSocial "Twitter" '(20 5 2021) encrypt encrypt))
;(define emptyFacebook (crearRedSocial "Facebook" '(20 5 2021) encrypt encrypt))
;(define emptyRs (crearRedSocial "RedSocial" '(20 5 2021) encrypt encrypt))
;(define twitter1 (register emptyTwitter '(20 5 2021) "Haru" "123"))
;(define twitter2 (register twitter1 '(20 5 2021) "Haru" "123")) 
;(define twitter3 (register(register(register emptyTwitter '(25 5 2021) "Harunomi" "321") '(25 5 2021) "Cybele" "uwu") '(25 5 2021) "Hylia" "ily"))
;(define twitter4 (login twitter3 "Harunomi" "321" display))
;(define twitter5 (login twitter3 "Cybele" "uwu" car))
;(define twitter6 (login twitter3 "Hylia" "ily" cdr))
;(define twitter7 (((login twitter2 "Harunomi" "321" post) '(20 5 2021)) "Mi primer HolaMundo")
;(define twitter8 (((login twitter7 "Hylia" "ily" post) '(20 5 2021)) "Contenta en la nueva red Social")
;(define twitter9 (((login twitter8 "Cybele" "uwu" post) '(20 5 2021)) "gracias por invitarme a la red social" "Harunomi") 
;(define twitter10 (((login twitter8 "Cybele" "uwu" follow) '(22 5 2021)) "Harunomi")
;(define twitter11 (((login twitter10 "Hylia" "ily" follow) '(23 5 2021)) "Harunomi")
;(define twitter12 (((login twitter11 "Harunomi" "321" follow) '(23 5 2021)) "Hylia")
;(define twitter13 (((login twitter12 "Harunomi" "321" share) '(23 5 2021)) 2)
;(define twitter14 (((login twitter13 "Harunomi" "321" share) '(23 5 2021)) 3)
;(define twitter15 (((login twitter14 "Cybele" "uwu" share) '(23 5 2021)) 1) ; 
;(display (socialnetwork->string twitter14)
;(display (socialnetwork->string twitter13)
;(login twitter14 "Harunomi" "321" socialnetwork->string)
;(define twitter17 (((login twitter14 "Cybele" "uwu" comment) '(28 5 2021)) 1) "Dame follow"))
;(define twitter18 (((login twitter17 "Harunomi" "321" comment) '(28 5 2021)) 4) "De inmediato"))
;(define twitter19 (((login twitter18 "Hylia" "ily" comment) '(28 5 2021)) 5) "No lo sigas, es una cuenta maliciosa"))
;(define twitter20 (((login twitter19 "Harunomi" "321" like) '(27 5 2021)) 2))
;(define twitter21 (((login twitter20 "Hylia" "Ily" like) '(27 5 2021)) 2))
;(define twitter22 (((login twitter21 "Harunomi" "321" like) '(27 5 2021)) 4))
;(viral twitter22 2) 
;(viral twitter22 1)
;(viral twitter22 4) 

                    
    

      
                                                                                                 
            

  