#lang racket
(provide (all-defined-out))
;TDA FECHA
;Representacion del TDA: (integer x integer x integer) o bien (list day month year)

;CONSTRUCTOR
;Descripcion: Crea una fecha / posibles necesidades: verificar años bisiestos, cuales meses tienen 31 dias y que febrero tiene 28 dias
;Dominio: (integer X integer X integer)
;Recorrido: list
(define (crearFecha day month year)
  (if (and(integer? day)(integer? month)(integer? year)(> day 0)(< day 32)(> month 0)(< month 13)(> year 0))
          (list day month year)
          null
          )
  )


;PERTENENCIA

;descripción: Funcion que evalua si la entrada pertenece al TDA fecha
;dom: elemento de cualquier tipo
;rec: boolean
(define (esFecha? entrada)
  (and (list? entrada) 
       (= (length entrada) 3)
       (not (null? (crearFecha (car entrada) (cadr entrada) (caddr entrada)))))
)

;SELECTORES

;Descripcion: Funcion que busca obtener el dia de una fecha
;Dominio: TDA Fecha
;Recorrido: integer
(define (getDay entrada)
  (if (esFecha? entrada)
      (car entrada)
      0
      )
  )
;Descripcion: Funcion que busca obtener el mes de una fecha
;Dominio: TDA Fecha
;Recorrido: integer
(define (getMonth entrada)
  (if (esFecha? entrada)
      (cadr entrada)
      0
      )
  )
;Descripcion: Funcion que busca obtener el año de una fecha
;Dominio: TDA Fecha
;Recorrido: integer
(define (getYear entrada)
  (if (esFecha? entrada)
      (caddr entrada)
      0
      )
  )
  
; MODIFICADORES

;Descripcion: Crea una lista nueva a partir de la entregada modificando el dato de dia
;Dominio: fecha x integer
;Recorrido: fecha
(define(changeDay entrada newDay)
  (if (esFecha? entrada)
      (crearFecha newDay (getMonth entrada) (getYear entrada))
      null
      )
  )

;Descripcion: Crea una lista nueva a partir de la entregada modificando el dato de mes
;Dominio: fecha x integer
;Recorrido: fecha
(define(changeMonth entrada newMonth)
  (if (esFecha? entrada)
      (crearFecha (getDay entrada) newMonth (getYear entrada))
      null
      )
  )

;Descripcion: Crea una lista nueva a partir de la entregada modificando el dato de mes
;Dominio: fecha x integer
;Recorrido: fecha
(define(changeYear entrada newYear)
  (if (esFecha? entrada)
      (crearFecha (getDay entrada) (getMonth entrada) newYear)
      null
      )
  )
