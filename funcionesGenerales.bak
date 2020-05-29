#lang racket

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



(provide (all-defined-out))