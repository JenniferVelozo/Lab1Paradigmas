#lang racket

;desc:  entrega último elemento de la lista
;dom: Lista
;rec: Elemento de Lista
;recursión: de cola
(define getLast ( lambda (L) (if (null? (cdr L));si no queda nada en L
                                 (car L);devolver la cabeza => (algo, null)
                                 (getLast (cdr L))
                                 )))


;Desc: agrega un elemento al final de una lista
;Dominio: elemento x lista
;Recorrido: una lista con el elemento agregado
;Recursión: natural
(define myAppend (lambda(elemento lista)
                 (if (null? lista)
                     (cons elemento null)
                     (cons (car lista) (myAppend elemento (cdr lista)))
                  )))

;Función que une dos listas
;Dominio: lista x lista
;Recorrido: una lista
;Recursión: natural
(define myAppend2 (lambda (L1 L2)
                     (if (null? L1)
                         L2
                         (if (null? L2)
                             L1
                             (cons (car L1) (myAppend2 (cdr L1) L2))))))

;Permite obtener el elemento ubicado en la posición que se indique
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

(provide (all-defined-out))