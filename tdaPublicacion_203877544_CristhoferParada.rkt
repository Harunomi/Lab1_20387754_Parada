#lang racket
(provide (all-defined-out))
(require "tdaUser_203877544_CristhoferParada.rkt")
(require "tdaFecha_203877544_CristhoferParada.rkt")

; TDA Publicacion
; Representacion del TDA: (integer x string x list x list x fecha
; (texto attachedFilereacts reacts comments fecha)

; CONSTRUCTOR
; Descripcion: crea una publicacion
; Dominio: integer x string x string x integer x list x fecha x list
; recorrido: list
(define crearPublicacion
  (lambda (ID text reacts comments fecha tags shared)
    (if (and (integer? ID) (string? text) (list? reacts) (list? comments) (esFecha? fecha) (list? tags) (list? shared) )
          (list ID text reacts comments fecha tags shared)
          null
          )
    )
  )


; SELECTORES

; Descripcion: funcion que permite obtener el ID de una publicacion
; Dominio: TDA publicacion
; recorrido: integer
(define getPublicacionID
  (lambda (publicacion)
    (car publicacion)
    )
  )

; Descripcion: funcion que permite obtener el texto de una publicacion
; Domininio: TDA Publicacion
; Recorrido: string
(define getTextoPublicacion
  (lambda (publicacion)
    (car(cdr publicacion))
      )
  )

; Descripcion: Funcion que permite obtener la lista de reacciones que de una publicacion
; Dominio: TDA Publicacion
; Recorrido: lista
(define getReacts
  (lambda (publicacion)
    (car(cdr(cdr publicacion)))
    )
  )

; Descripcion: Funcion que permite obtener la lista de comentarios de una publicacion
; Dominio: TDA Publicacion
; Recorrido: list
(define getComments
  (lambda (publicacion)
    (car(cdr(cdr(cdr publicacion))))
     ) 
  )


; Descripcion: Funcion que permite obtener la fecha de una publicacion
; Dominio: TDA publicacion
; Recorrido: fecha

(define getFechaPublicacion
  (lambda (publicacion)
    (if (esFecha? (car(cdr(cdr(cdr(cdr publicacion))))))
        (car(cdr(cdr(cdr(cdr publicacion)))))
        null
        )
    )
  )

; Descripcion: Funcion que permite obtener la lista de usuarios taggeados
; Dominio: TDA publicacion
; Recorrido: list
(define getTags
  (lambda (entrada)
    (car(cdr(cdr(cdr(cdr(cdr entrada))))))
    )
  )

; Descripcion: funcion que permite obtener la lista de compartidos
; Dominio: TDA publicacion
; Recorrido: list
(define getShared
  (lambda (entrada)
    (car (cdr(cdr(cdr(cdr(cdr(cdr entrada)))))))
    )
  )

; PERTENENCIA

; Descripcion: Funcion que comprueba que la entrada corresponda al tda publicacion
; dominio: cualquier tipo de dato
; recorrido: bool
(define esPublicacion?
  (lambda (entrada)
    (and (list? entrada) ( = (length entrada 7))
         (not (null? (crearPublicacion
                      (getPublicacionID entrada)
                      (getTextoPublicacion entrada)
                      (getReacts entrada)
                      (getComments entrada)
                      (getFechaPublicacion entrada)
                      (getTags entrada)
                      (getShared entrada)))
        )
    )
  )
  )

; MODIFICADORES

; Descripcion: Crea una nueva publicacion pero cambiando el dato de texto
; Dominio: TDA publicacion x string
; Recorrido: TDA publicacion
(define cambiarTextoPublicacion
  (lambda (publicacion nuevoTexto)
    (if (and(esPublicacion? publicacion) (string? nuevoTexto))
        (crearPublicacion (getPublicacionID publicacion) nuevoTexto (getReacts publicacion) (getComments publicacion) (getFechaPublicacion publicacion))
        null
        )
    )
  )

; la implementacion de cambiar comentario fue ignorada a proposito, puesto que es preferible primero implementar una
; funcion para buscar comentarios existentes y luego actualizar la lista de comentarios

; Descripcion: Crea una nueva publicacion modificando la fecha 
; Dominio: TDA Publicacion x fecha
; Recorrido: TDA Publicacion
(define cambiarFechaPublicacion
  (lambda (publicacion nuevaFechaPublicacion)
    (if (and(esPublicacion? publicacion) (esFecha? nuevaFechaPublicacion))
        (crearPublicacion (getPublicacionID publicacion)
                          (getTextoPublicacion publicacion)
                          (getReacts publicacion)
                          (getComments publicacion)
                          nuevaFechaPublicacion)
        null
        )
    )
  )


; FUNCIONES AUXILIARES

; Descripcion: funcion que permite encontrar una publicacion dada su iD y una lista de publicaciones
; dominio: integer x list
; recorrido: bool
(define existePublicacion?
  (lambda (ID lista)
    (if (equal? lista null)
        #f
        (if (equal? ID (getPublicacionID (car lista)))
            #t
            (existePublicacion? ID (cdr lista))
            )
        )
    )
  )

; Descripcion: funcion que permite devolver una publicacion dada su ID y una lista de publicaciones
; dominio: integer x list
; recorrido: tdaPublicacion
(define IDtoPublicacion
  (lambda (ID lista)
    (if (equal? lista null)
        #f
        (if (equal? ID (getPublicacionID (car lista)))
            (car lista)
            (IDtoPublicacion ID (cdr lista))
            )
        )
    )
  )



                                


            
            
            
    
  
        
                             

                             

      
  

