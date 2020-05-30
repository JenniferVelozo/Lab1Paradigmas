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
                     (if (null? L1) ;si la lista 1 está nula, se entrega la lista 2
                         L2
                         (if (null? L2) ;si la lista 2 está nula, se entrega la lista 1, ya que lista 1 no se une con nada
                             L1
                             (cons (car L1) (myAppend2 (cdr L1) L2)))))) ;sino se une el primer elemento de la lista 1 con un llado recursivo
                                                                         ;donde la lista 1 se reemplaza por su cola, y la lista 2 sigue intacta

;desc: permite saber si un elemento es parte de una lista
;dom: elemento x lista
;rec: booleano
;tipo de recursión: de cola
(define esta? (lambda (elemento lista)
                   (if (null? lista);si la lista ingresada es nula, se retorna falso
                       #f
                       (if (equal? elemento (car lista)) ;si el elemento es igual al primer elemento de la lista, se retorna verdadero
                           #t
                           (esta? elemento (cdr lista))))));sino, se busca el elemento en la cola de la lista

;desc: permite saber si un archivo no está dentro de una lista, a través de su nombre
;dom: archivo (lista) x lista
;rec: booleano
;tipo de recursión: de cola
(define noEstaArchivo? (lambda (archivo lista)
                       (if (null? lista) ;si la lista está nula, quiere decir que el archivo no está en ella 
                           #t ;se retorna verdadero
                           (if (equal? (car archivo) (car(car lista))) ;si el nombre del archivo es igual a algún nombre de archivo dentro de la lista
                               #f ;se retorna falso, lo que quiere decir que ya está ese archivo en la lista
                               (noEstaArchivo? archivo (cdr lista)))))) ;sino, se hace un llamado recursivo, revisando la cola de la lista
(provide (all-defined-out))