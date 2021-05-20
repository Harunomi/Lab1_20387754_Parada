#lang racket
(provide (all-defined-out))
(require "tdaUser.rkt")
(require "tdaFecha.rkt")

; FUNCION AUXILIAR
; Descripcion: permite saber si un numero es mayor o igual a otro
; DOminio: integer x integer
; Recorrido: bool
(define >=?
  (lambda (num1 num2)
  (if (or( > num1 num2) (= num1 num2))
      #t
      #f
      )
    )
  )

; TDA Publicacion
; Representacion del TDA: (string x string x integer x list x fecha
; (texto attachedFilereacts reacts comments fecha)

; CONSTRUCTOR
; Descripcion: crea una publicacion
; Dominio: string x string x integer x list x fecha
; recorrido: list
(define crearPublicacion
  (lambda (text attachedFile reacts comments fecha)
  (if (and(string? text) (string? attachedFile) (integer? reacts) (>=? reacts 0) (list? comments) (esFecha? fecha))
          (list text attachedFile reacts comments fecha)
          null
          )
    )
  )

; SELECTORES

; Descripcion: funcion que permite obtener el texto de una publicacion
; Dominio: TDA publicacion
; recorrido: string
(define getTextoPublicacion
  (lambda (publicacion)
    (car publicacion)
    )
  )

; Descripcion: funcion que permite obtener el "archivo adjunto" de una publicacion
; Domininio: TDA Publicacion
; Recorrido: string
(define getAttachedFile
  (lambda (publicacion)
    (car(cdr publicacion))
      )
  )

; Descripcion: Funcion que permite obtener la cantidad de reacciones que de una publicacion
; Dominio: TDA Publicacion
; Recorrido: integer
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

; PERTENENCIA

; Descripcion: Funcion que comprueba que la entrada corresponda al tda publicacion
; dominio: cualquier tipo de dato
; recorrido: bool
(define esPublicacion?
  (lambda (publicacion)
    (if (and(string? getTextPublicacion)(string? getAttachedFile)(integer? getReacts)(list? getComments)(esFecha? getFechaPublicacion))
        #t
        #f
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
        (crearPublicacion nuevoTexto (getAttachedFile publicacion) (getReacts publicacion) (getComments publicacion) (getFechaPublicacion publicacion))
        null
        )
    )
  )

; Descripcion: Crea una nueva publicacion pero cambiando el dato de attachedFile
; Dominio: TDA Publicacion x string
; Recorrido: TDA publicacion
(define cambiarAttachedFile
  (lambda (publicacion nuevoAttachedFile)
    (if (and(esPublicacion? publicacion)(string? nuevoAttachedFile))
        (crearPublciacion (getTextoPublicacion publicacion) nuevoAttachedFile (getReacts publicacion) (getComments publicacion) (getFechaPublicacion publicacion))
        null
        )
    )
  )

; Descripcion: Crea una nueva publicacion pero cambiando el numero de reacciones
; Dominio: TDA Publicacion x integer
; Recorrido: TDA publicacion
(define cambiarReactsPublicacion
  (lambda (publicacion nuevoReacts)
    (if (and(esPublicacion? publicacion)(integer? nuevoReacts)(>=? nuevoReacts 0))
        (crearPublicacion (getTextoPublicacion publicacion)(getAttachedFile publicacion)nuevoReacts (getComments publicacion)(getFechaPublicacion publicacion))
        null
        )
    )
  )

; Descripcion: Crea una nueva publicacion con una nueva lista de comentarios
; Dominio: TDA Publicacion x list
; Recoriddo: TDA Publicacion
(define cambiarComments
  (lambda (publicacion nuevoComment)
    (if (and(esPublicacion? publicacion) (list? nuevoComment))
        (crearPublicacion (getTextoPublicacion publicacion)(getAttachedFile publicacion)(getReacts publicacion) nuevoComment (getFechaPublicacion publicacion)) ;; this need a better implementation, since comments is a list and i can't just add a element to a list by doing this
        null
        )
    )
  )

; Descripcion: Crea una nueva publicacion modificando la fecha 
; Dominio: TDA Publicacion x fecha
; Recorrido: TDA Publicacion
(define cambiarFechaPublicacion
  (lambda (publicacion nuevaFechaPublicacion)
    (if (and(esPublicacion? publicacion) (esFecha? nuevaFechaPublicacion))
        (crearPublicacion (getTextoPublicacion publicacion)(getAttachedFile publicacion)(getReacts publicacion)(getComments publicacion)nuevaFechaPublicacion)
        null
        )
    )
  )


            
    
  
        
                             

                             

      
  

