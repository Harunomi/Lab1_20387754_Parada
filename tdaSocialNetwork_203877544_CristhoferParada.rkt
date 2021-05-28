#lang racket
(require "tdaUser_203877544_CristhoferParada.rkt")
(require "tdaFecha_203877544_CristhoferParada.rkt")
(require "tdaPublicacion_203877544_CristhoferParada.rkt")
(provide (all-defined-out))

(define encrypt (lambda (s) (list->string (reverse (string->list s)))))


; TDA SocialNetwork
; Representacion del TDA: string x fecha x TDA USER x TDA PUBLICACIONES
; (nombreRedSocial fechaCreacion usuarios publicaciones

; CONSTRUCTOR

; Descripcion: funcion que permite crear una nueva red social
; Dominio: string x fecha x encryptFunction x DecrypFunction
; Recorrido: list
(define crearRedSocial
  (lambda (nombre fecha encrypt decrypt)
    (if (and(string? nombre)(esFecha? fecha))
        (crearRedSocialAux nombre fecha '() '() 0 encrypt decrypt )
        null
        )
    )
  )

; Descripcion: funcion auxiliar al constructor crearRedSocial
; Dominio: string x TDA fecha x tdaUsuarios x TDA Publicacion
; Recorrido list
(define crearRedSocialAux
  (lambda (nombre fecha usuarios publicaciones usuarioOnline encrypt decrypt)
    (if (and (string? nombre) (esFecha? fecha) (list? usuarios)(list? publicaciones) (integer? usuarioOnline))
        (list nombre fecha usuarios publicaciones usuarioOnline encrypt decrypt)
        null
        )
    )
  )

; SELECTORES

; Descripcion: funcion que permite obtener el nombre de una red social
; Dominio: TDA Social Network
; Recorrido: string
(define getNombreRedSocial
  (lambda (entrada)
    (if (not(null? entrada))
        (car entrada)
        null
        )
    )
  )

; Descripcion: funcion que permite obtener la fecha de creacion de una redSocial
; Dominio: TDA SocialNetwork
; Recorrido: TDA Fecha
(define getFechaRedSocial
  (lambda (entrada)
    (if (not(null? entrada))
        (car(cdr entrada))
        null
        )
    )
  )

; Descripcion: funcion que permite obtener la lista de usuarios de una red social
; dominio: TDA SocialNetwork
; Recorrido: TDA Usuarios
(define getUsuariosRedSocial
  (lambda (entrada)
    (if (list? entrada)
        (car(cdr(cdr entrada)))
        null
        )
    )
  )

; Descripcion: funcion que permite obtener la lista de publicaciones de una red social
; Dominio: TDA SocialNetwork
; Recorrido: TDA Publicaciones
(define getPublicacionesRedSocial
  (lambda (entrada)
    (if (list? entrada)
        (car (cdr(cdr(cdr entrada))))
        null
        )
    )
  )
; Descripcion: funcion que permite obtener la lista de usuarios online en una red social
; Dominio: TDA social network
; recorrido: list
(define getUsuarioOnline
  (lambda (entrada)
    (if (list? entrada)
        (car(cdr(cdr(cdr(cdr entrada)))))
        null
        )
    )
  )

; Descripcion: funcion que permite obtener la funcion de encriptado
; Dominio: TDA Social network
; Recorrido: function
(define getEncryptFn
  (lambda (entrada)
    (car(cdr(cdr(cdr(cdr(cdr entrada))))))
    )
  )

; Descripcion: funcion que permite obtener la funcion de desencriptado
; Dominio: TDA Social network
; Recorrido: function
(define getDecryptFn
  (lambda (entrada)
    (car(cdr(cdr(cdr(cdr(cdr(cdr entrada)))))))
    )
  )

; PERTENENCIA

; Descripcion: permite saber si la entrada dada pertenece al tda Socialnetwork
; dominio: cualquier elmento
; recorrido: bool
(define esRedSocial?
  (lambda (entrada)
    (and (list? entrada) (= (length entrada) 7)
         (not(null? (crearRedSocialAux (getNombreRedSocial entrada) (getFechaRedSocial entrada) (getUsuariosRedSocial entrada) (getPublicacionesRedSocial entrada)
                            (getUsuarioOnline entrada)(getEncryptFn entrada)(getDecryptFn entrada))))
         )
    )
  )



; MODIFICADORES
                            
; Descripcion: funcion que permite agregar un usuario a la lista de usuarios
; Dominio: TDA User x lista
; Recorrido: TDA SocialNetwork
(define agregarUsuarioRS
  (lambda (usuario listaUsuarios)
    (if (equal? listaUsuarios null)
        (list usuario)
        (if (equal? (getUsername usuario) (getUsername (car listaUsuarios)))
            #f
            (cons (car listaUsuarios) (agregarUsuarioRS usuario (cdr listaUsuarios)))
            )
        )
    )
  )

; Descripcion: funcion que permite agregar un usuario a la lista de usuarios online
; Dominio: list x integer
; Recorrido: list
(define agregarUsuarioOnline
  (lambda (ID lista)
    (if (equal? lista null)
        (list ID)
        (if (equal? ID (car (lista)))
            #f ; el usuario ya se encuentra online
            (cons (car lista) (agregarUsuarioOnline ID (cdr(lista))))
            )
        )
    )
  )

; Descripcion: funcion que permite agregar una publicacion a la lista de publicaciones
; Dominio: list x publicacion
; recorrido: list
(define agregarPublicacion
  (lambda (lista publicacion)
    (if (equal? lista null)
        (list publicacion)
        (cons (car lista) (agregarPublicacion (cdr lista) publicacion))
        )
    )
  )

; Descripcion: funcion que permite actualizar un usuario dada una lista y un nuevo usuario a actualizar
; Dominio: lista x usuario
; Recorrido: lista
(define actualizarUsuario
  (lambda (listaUsuarios usuario)
    (if (equal? listaUsuarios null)
        (list usuario) 
        (if (equal? (getUserID usuario) (getUserID (car listaUsuarios)))
           (actualizarUsuario (cdr listaUsuarios) usuario)
           (cons (car listaUsuarios) (actualizarUsuario (cdr listaUsuarios) usuario))
           )
        )
    )
  )

; Descripcion: funcion que permite actualizar una publicacion dada una lista y un nuevo usuario a actualizar
; Dominio: lista x publicacion
; recorrido: lista
(define actualizarPublicacion
  (lambda (lista publicacion)
    (if (equal? lista null)
        (list publicacion)
        (if (equal? (getPublicacionID publicacion) (getPublicacionID (car lista)))
            (actualizarPublicacion (cdr lista) publicacion)
            (cons (car lista) (actualizarPublicacion (cdr lista) publicacion))
            )
        )
    )
  )

; Descripcion: funcion que devuelve el ID del autor de una publicacion dada una ID de una publicacion
; Dominio: redSocial x integer
; Recorrido: integer
(define publicacionToAutor
  (lambda (redSocial ID)
    (if (equal? (getPublicacionesRedSocial redSocial) null)
        (display "la publicacion no existe")
        (if (existePublicacion? ID (getUserPosts (car (getUsuariosRedSocial redSocial))))
            (getUserID (car (getUsuariosRedSocial redSocial)))
            (publicacionToAutor (crearRedSocialAux (getNombreRedSocial redSocial)
                                                   (getFechaRedSocial redSocial)
                                                   (cdr (getUsuariosRedSocial redSocial))
                                                   (getPublicacionesRedSocial redSocial)
                                                   (getUsuarioOnline redSocial)
                                                   (getEncryptFn redSocial)
                                                   (getDecryptFn redSocial)
                                                   )
                                ID)
            )
        )
    )
  )

; Descripcion: funcion que permite recorre una lista de usuarios y agrega informacion de  ellos a un string
; Dominio: lista x string x tda socialnetwork
; Recorrido: string
(define listaUsuarios->string
  (lambda (lista string redSocial)
    (if (equal? lista null)
        string
        (string-append string " Cuenta creada el dia "
                       (number->string (getDay(getUserFecha(car lista)))) ; devuelvo el dia como string
                       " del mes "
                       (number->string (getMonth(getUserFecha(car lista)))) ; devuelvo el mes como string
                       " del año "
                       (number->string (getYear(getUserFecha(car lista)))) ; devuelvo el año como string
                       "\nCon el nombre de usuario "
                       ((getDecryptFn redSocial) (getUsername (car lista)))
                       " y contraseña "
                       ((getDecryptFn redSocial) (getPassword (car lista)))
                       "\nLo siguen "
                       (number->string (length (getUserFollowers (car lista)))) " usuarios."
                       "\nTiene "
                       (number->string (length (getUserPosts (car lista)))) " publicaciones, las cuales son:\n\n"
                       (listaPublicaciones->string (getUserPosts (car lista)) string redSocial)
                       "\n///////////////////////////////////////////////////////////\n"
                       (listaUsuarios->string (cdr lista) string redSocial) ; Paso al siguiente usuario
                       )
        )
    )
  )

; Descripcion: funcion que recorre una lista de publicaciones y agrega su informacion a un string
; Dominio: lista x string x  tda socialnetwork
; recorrido: string
(define listaPublicaciones->string
  (lambda (lista string redSocial)
    (if (equal? lista null)
        string
        (string-append string " Publicacion creada el dia "
                       (number->string (getDay(getFechaPublicacion (car lista))))
                       " del mes "
                       (number->string (getMonth(getFechaPublicacion (car lista))))
                       " del año "
                       (number->string (getYear(getFechaPublicacion (car lista))))
                       "\nContenido: "
                       ((getDecryptFn redSocial) (getTextoPublicacion (car lista)))
                       "\nHa obtenido "
                       (number->string (length (getReacts (car lista)))) " likes. "
                       "\nHa sido compartida "
                       (number->string (length (getShared (car lista)))) " veces. "
                       "\nTiene "
                       (number->string (length (getComments (car lista)))) " comentarios."
                       "\nTiene etiquetado a "
                       (listaString->string (encriptarLista (getDecryptFn redSocial) (getTags(car lista))) string)
                       "\n******************************************************\n"
                       (listaPublicaciones->string (cdr lista) string redSocial)
                       )
        )
    )
  )

; Descripcion: funcion que junta una lista de strings en un mismo string
; Dominio: lista x string
; Recorrido: string
(define listaString->string
  (lambda (lista string)
    (if (equal? lista null)
        string
        (string-append string
                       (car lista)
                       " "
                       (listaString->string (cdr lista) string)
                       )
        )
    )
  )

; Descripcion: Funcion que permite aplicar una funcion de encriptado a una lista con strings
; Dominio: function x list
; recorrido:  list
(define encriptarLista
  (lambda (function lista)
    (if (equal? lista null)
        lista
        (cons (function (car lista)) (encriptarLista function (cdr lista)))
        )
    )
  )
  
                 



                    
            


