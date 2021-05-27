#lang racket
(require "tdaSocialNetwork.rkt")
(require "tdaUser.rkt")
(require "tdaPublicacion.rkt")
(require "tdaFecha.rkt")

(define twitter (crearRedSocial "twitter" '(24 5 2021) encrypt encrypt))


; Descripcion: Funcion que permite registrar a un nuevo usuario en la red social
; Dominio: string x fecha x string x string
; Recorrido: TDA SocialNetwork
(define register
  (lambda (nombreRed fecha nombreUsuario password)
    (if (esRedSocial? nombreRed)
        (crearRedSocialAux (getNombreRedSocial nombreRed) (getFechaRedSocial nombreRed)
                           (agregarUsuarioRS (crearUsuario (IDcounter (getUsuariosRedSocial nombreRed)) (encrypt nombreUsuario)(encrypt password) fecha null null) (getUsuariosRedSocial nombreRed))
                           (getPublicacionesRedSocial nombreRed) (getUsuarioOnline nombreRed)(getEncryptFn nombreRed) (getDecryptFn nombreRed))
        null
        )
    )   
  )

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
                                                                                                        tags
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
                                                                       tags
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
                                                              (addFollow (getUserFollowers(UsernametoUser redSocial user)) ; agrego el follower a la lista de followers del usuario
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
                                                                                              (addFollow (getShared (IDtoPublicacion IDpost (getPublicacionesRedSocial redSocial)))
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
                                                                                                            null
                                                                                                            tags)
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
                                                                                                  (addFollow (getShared (IDtoPublicacion IDpost (getPublicacionesRedSocial redSocial)))
                                                                                                             (getUserID (IDtoUser redSocial (getUsuarioOnline redSocial)))
                                                                                                             )
                                                                                                  )
                                                                                )
                                                         (crearPublicacion (IDcounter (getPublicacionesRedSocial redSocial))
                                                                           (getTextoPublicacion (IDtoPublicacion IDpost (getPublicacionesRedSocial redSocial)))
                                                                           null
                                                                           null
                                                                           fecha
                                                                           null
                                                                           tags)
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

                                 
          
      
      

; Descripcion: Funcion que agrega una ID de un usuario a la lista de followers de otro
; Dominio: tda User x integer
; Recorrido: list
(define addFollow
  (lambda (lista follower)
    (if (equal? lista null)
        (list follower)
        (cons (car lista) (addFollow (cdr lista) follower))
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
; recorrido: integer
(define buscarUsuario
  (lambda (redSocial username password)
    (if (equal? (getUsuariosRedSocial redSocial) null)
        #f ; no se encontro el usuario o no coincidia la password
        (if (and(equal? (encrypt username) (getUsername (car (getUsuariosRedSocial redSocial)))) (equal? (encrypt password) (getPassword (car(getUsuariosRedSocial redSocial)))))
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

                    
    

      
                                                                                                 
            

  