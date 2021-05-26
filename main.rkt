#lang racket
(require "tdaSocialNetwork.rkt")
(require "tdaUser.rkt")
(require "tdaPublicacion.rkt")
(require "tdaFecha.rkt")

(define twitter (crearRedSocial "twitter" '(24 5 2021) encrypt encrypt))

; Descripcion: funcion que permite obtener el ID del ultimo usuario
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

; Descripcion: Funcion que permite registrar a un nuevo usuario en la red social
; Dominio: string x fecha x string x string
; Recorrido: TDA SocialNetwork
(define register
  (lambda (nombreRed fecha nombreUsuario password)
    (if (esRedSocial? nombreRed)
        (crearRedSocialAux (getNombreRedSocial nombreRed) (getFechaRedSocial nombreRed)
                           (agregarUsuarioRS (crearUsuario (IDcounter (getUsuariosRedSocial nombreRed)) (encrypt nombreUsuario)(encrypt password) fecha null null) (getUsuariosRedSocial nombreRed))
                           (getPublicacionesRedSocial nombreRed) (getUsuariosOnline nombreRed)(getEncryptFn nombreRed) (getDecryptFn nombreRed))
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
        (function (crearRedSocialAux (getNombreRedSocial redSocial)(getFechaRedSocial redSocial) (getUsuariosRedSocial redSocial) (getPublicacionesRedSocial redSocial)
                                      (agregarUsuarioOnline (buscarUsuario redSocial username password) (getUsuariosOnline redSocial)) (getEncryptFn redSocial) (getDecryptFn redSocial)))
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
            (buscarUsuario (crearRedSocialAux (getNombreRedSocial redSocial)(getFechaRedSocial redSocial) (cdr (getUsuariosRedSocial redSocial)) (getPublicacionesRedSocial redSocial)
                                      (getUsuariosOnline redSocial) (getEncryptFn redSocial) (getDecryptFn redSocial)) username password)
            )
        )
    )
  )



      
                                                                                                 
            

  