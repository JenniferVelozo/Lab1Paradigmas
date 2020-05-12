#lang racket

;Función git
;Función pull
;Función add
;Función commit
;Función push

;Función zonas

;Función git que permite aplicar los comandos
;Dominio: un string que indica el comando a ejecutar
;Recorrido: la función correspondiente al comando
;(define git (lambda (comando)
;             ))


;Función que agrega un elemento al final de una lista
;Dominio: elemento x lista
;Recorrido: una lista con el elemento agregado
;Recursión: natural
(define append (lambda(elemento lista)
                 (if (null? lista)
                     (cons elemento null)
                     (cons (car lista) (append (cdr lista) elemento))
                  )))


;Permite obtner el elemento ubicado en la posición que se indique
;Dominio: posicion x lista
;Recorrido: entrega el elemento que se encuentra en la posición indicada
;Recursión: natural
(define obtenerElemento (lambda (pos lista)
  (if (list? lista)
      (if (null? lista)
          lista
          (if (= pos 0)
              (car lista)
              (obtenerElemento (- pos 1)(cdr lista))
              )
          )
      null)
  ))

