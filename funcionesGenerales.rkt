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

;desc: permite saber si un elemento es parte de una lista
;dom: elemento x lista
;rec: booleano
;tipo de recursión: de cola
(define esta? (lambda (elemento lista)
                   (if (null? lista);si la lista ingresada es nula, se retorna falso
                       #f
                       (if (equal? elemento (car lista)) ;si el elemento evaluado es igual al primer elemento de la lista, se retorna verdadero
                           #t
                           (esta? elemento (cdr lista))))));sino, se busca el elemento en la cola de la lista

(define noEstaArchivo? (lambda (archivo lista)
                       (if (null? lista)
                           #t
                           (if (equal? (car archivo) (car(car lista)))
                               #f
                               (noEstaArchivo? archivo (cdr lista))))))
(provide (all-defined-out))