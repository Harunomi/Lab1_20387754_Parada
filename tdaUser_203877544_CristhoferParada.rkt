#lang racket
(require "tdaFecha_203877544_CristhoferParada.rkt")
(provide (all-defined-out))
; TDA USER
; Representacion del TDA: (intger x string x string x fecha x list x list)
; (list nickname password fecha followers posts) 

; CONSTRUCTOR
; Descripcion: Crea un usuario
; Dominio: integer x string x string x fecha x list x list
; Recorrido: list
(define crearUsuario
  (lambda (ID username password fechaCreacion followers posts)
    (if (and (integer? ID) (string? username) (string? password) (esFecha? fechaCreacion) (list? followers)(list? posts))
      (list ID username password fechaCreacion followers posts)
      null
      )
    )
  )

; PERTENENCIA
; Descripcion: Funcion que evalua si la entrada pertenece al TDA User
; Dominio: elemento de cualquier tipo
; Recorrido: un booleano
(define esUser?
  (lambda (entrada)
  (and (list? entrada) ( = (length entrada) 6)
       (not (null? (crearUsuario (getUserID entrada) (getUsername entrada)(getPassword entrada)(getUserFecha entrada)(getUserFollowers entrada)(getUserPosts entrada)))))
    )
  )

; SELECTORES

; Descripcion: Permite obtener la ID de un usuario
; Dominio: user
; Recorrido: integer
(define getUserID
  (lambda (entrada)
    (if (not(null? entrada))
        (car entrada)
        null
        )
    )
  )

; Descripcion: Funcion que permite obtener el nombre de usuario de un user
; Dominio: User
; Recorrido: string
(define getUsername
  (lambda (entrada)
    (if (not(null? entrada))
      (car(cdr entrada))
      null
      )
    )
  )

; Descripcion: Funcion que permite obtener la contraseña de un user
; Dominio: User
; Recorrido: string
(define getPassword
  (lambda (entrada)
    (if (not(null? entrada))
      (car(cdr(cdr entrada)))
      null
      )
    )
  )

; Descripcion: Funcion que permite obtener la fecha de creacion del user
; Dominio: user
; Recorrido: Fecha
(define getUserFecha
  (lambda (entrada)
    (if (not(null? entrada))
      (car(cdr(cdr(cdr entrada))))
      null
      )
    )
  )
; Descripcion: Funcion que permite obtener la lista de followers de un user
; Dominio: user
; Recorrido: list
(define getUserFollowers
  (lambda (entrada)
    (if (not(null? entrada))
      (car(cdr(cdr(cdr(cdr entrada)))))
      null
      )
    )
  )

; Descripcion: Funcion que permite obtener los posts de un user 
; Dominio: user
; Recorrido: list
(define getUserPosts
  (lambda (entrada)
    (if (not(null? entrada))
      (car(cdr(cdr(cdr(cdr(cdr entrada))))))
      null
      )
    )
  )
; MODIFICADORES


; El modificador de iD no es necesario de crear, puesto que no tiene sentido cambiar el ID de un usuario dentro de una red social 


; Descripcion: Crea una lista nueva a partir de la entregada, modificando el dato del nicnkame
; dominio: user x string
; Recorrido: user
(define changeNickname
  (lambda (entrada newNick)
    (if (esUser? entrada)
      (crearUsuario (getUserID entrada) newNick (getPassword entrada) (getUserFecha entrada) (getUserFollowers entrada) (getUserPosts entrada))
      null
      )
    )
  )

; Descripcion: Crea una lista nueva a partir de la entregada, pero con un nueva contraseña
; Dominio: User x string
; Recorrido: User
(define changePassword
  (lambda (entrada newPassword)
    (if (esUser? entrada)
      (crearUsuario (getUserID entrada) (getUsername entrada) newPassword (getUserFecha entrada) (getUserFollowers entrada) (getUserPosts entrada))
      null
      )
    )
  )

; Descripcion: Crea una lista nueva a partir de la entregada pero con una nueva fecha de creacion?
; Dominio: User x fecha
; Recorrido: User
(define changeFechaCreacion
  (lambda (entrada newFechaCreacion)
    (if  (and (esUser? entrada) (esFecha? newFechaCreacion))
      (crearUsuario (getUserID entrada) (getUsername entrada) (getPassword entrada) newFechaCreacion (getUserFollowers entrada) (getUserPosts entrada))
      null
      )
    )
  )

;******* los modificadores de followers y publicaciones fueron skippeados de esta seccion, puesto uqe se veran implementados en el main *******
; FUNCIONES AUXILIARES


