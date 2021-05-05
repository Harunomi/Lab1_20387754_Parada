#lang racket
(require "tdaFecha.rkt")
; TDA USER
; Representacion del TDA: (string x string x fecha x integer x integer)
; (list nickname password fecha publicaciones amigos) 

; CONSTRUCTOR
; Descripcion: Crea un usuario
; Dominio: string x string x fecha x integer x integer
; Recorrido: list
(define (crearUsuario username password fechaCreacion nPublicaciones nAmigos)
  (if (and(string? username) (string? password)(string>? password "abcde") (esFecha? fechaCreacion) (integer? nPublicaciones)(integer? nAmigos))
      (list username password fechaCreacion nPublicaciones nAmigos)
      null
      )
  )

; PERTENENCIA
; Descripcion: Funcion que evalua si la entrada pertenece al TDA User
; Dominio: elemento de cualquier tipo
; Recorrido: un booleano
(define (esUser? entrada)
  (and (list? entrada) ( = (length entrada) 5)
       (not (null? (crearUsuario (car entrada) (cadr entrada) (caddr entrada) (cadddr entrada) (caddddr entrada)))))
  )

; SELECTORES

; Descripcion: Funcion que permite obtener el nombre de usuario de un user
; Dominio: User
; Recorrido: string

(define (getUsername entrada)
  (if (esUser? entrada)
      (car entrada)
      0
      )
  )

; Descripcion: Funcion que permite obtener la contraseña de un user
; Dominio: User
; Recorrido: string
(define (getPassword entrada)
  (if (esUser? entrada)
      (cadr entrada)
      0
      )
  )

; Descripcion: Funcion que permite obtener la fecha de creacion del user
; Dominio: user
; Recorrido: Fecha
(define (getFechaCreacion entrada)
  (if (esUser? entrada)
      (caddr entrada)
      0
      )
  )
; Descripcion: Funcion que permite obtener la cantidad de publicaciones del user
; Dominio: user
; Recorrido: integer
(define (getnPublicaciones entrada)
  (if (esUser? entrada)
      (cadddr entrada)
      0
      )
  )

; Descripcion: Funcion que permite obtener la cantidad de amigos de un user
; Dominio: user
; Recorrido: integer
(define (getnAmigos entrada)
  (if (esUser? entrada)
      (caddddr entrada)
      0
      )
  )
; MODIFICADORES

; Descripcion: Crea una lista nueva a partir de la entregada, modificando el dato del nicnkame
; dominio: user x string
; Recorrido: user
(define (changeNickname entrada newNick)
  (if (esUser? entrada)
      (crearUsuario newNick (getPassword entrada) (getFechaCreacion entrada) (getnPublicaciones entrada) (getnAmigos entrada))
      null
      )
  )

; Descripcion: Crea una lista nueva a partir de la entregada, pero con un nueva contraseña
; Dominio: User x string
; Recorrido: User
(define (changePassword entrada newPassword)
  (if (esUser? entrada)
      (crearUsuario (getUsername entrada) newPassword (getFechaCreacion entrada) (getnPublicaciones entrada) (getnAmigos entrada))
      null
      )
  )

; Descripcion: Crea una lista nueva a partir de la entregada pero con una nueva fecha de creacion?
; Dominio: User x fecha
; Recorrido: User
(define (changeFechaCreacion entrada newFechaCreacion)
  (if  (and (esUser? entrada) (esFecha? newFechaCreacion))
      (crearUsuario (getUsername entrada) (getPassword entrada) newFechaCreacion (getnPublicaciones entrada) (getnAmigos entrada))
      null
      )
  )

; Descripcion: Crea una lista nueva a partir de la entregada, con una nueva cantidad de publicaciones
; Dominio: User x integer
; Recorrido: User
(define (changenPublicaciones entrada newnPublicaciones)
  (if (esUser? entrada)
      (crearUsuario (getUsername entrada) (getPassword entrada) (getFechaCreacion entrada) newnPublicaciones (getnAmigos entrada))
      null
      )
  )

; Descripcion: Crea una lista nueva a partir de la entregada cambiando la cantidad de amigos
; Dominio: User x integer
; Recorrido: User
(define (changenAmigos entrada newnAmigos)
  (if (esUser? entrada)
      (crearUsuario (getUsername entrada) (getPassword entrada) (getFechaCreacion entrada) (getnPublicaciones entrada) newnAmigos)
      null
      )
  )


; FUNCIONES COMPLEMENTARIAS QUE NO PERTENECEN AL TDA USER
; Descripcion: permite acceder al 5 elemento de una lista
; Dominio: list
; Recorrido: cualquier elemento
; CADDDR
(define (caddddr lista)
  (car(cdr(cdr(cdr(cdr lista)))))
  )
