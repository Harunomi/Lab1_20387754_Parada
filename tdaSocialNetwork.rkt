#lang racket
(require "tdaUser.rkt")
(require "tdaFecha.rkt")
(require "tdaPublicacion.rkt")
(provide (all-defined-out))

(define encrypt (lambda (s) (list->string (reverse (string->list s)))))

(define user1 (crearUsuario 1 "asagaki" "culo" '(25 5 2021) null null))
(define user2 (crearUsuario 2 "cyb" "culo" '(25 5 2021) null null))


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
        (crearRedSocialAux nombre fecha '() '() '() encrypt decrypt )
        null
        )
    )
  )

; Descripcion: funcion auxiliar al constructor crearRedSocial
; Dominio: string x TDA fecha x tdaUsuarios x TDA Publicacion
; Recorrido list
(define crearRedSocialAux
  (lambda (nombre fecha usuarios publicaciones usuariosOnline encrypt decrypt)
    (if (and (string? nombre) (esFecha? fecha) (list? usuarios)(list? publicaciones) (list? usuariosOnline))
        (list nombre fecha usuarios publicaciones usuariosOnline encrypt decrypt)
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
(define getUsuariosOnline
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
                            (getUsuariosOnline entrada)(getEncryptFn entrada)(getDecryptFn entrada))))
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
            


