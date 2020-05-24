#lang racket
(require "TDAzonas.rkt")
(require "TDAcommit.rkt")
(require "funcionesGenerales.rkt")

(define workspace (list (list "archivo1" "contenido1") (list "archivo2" "contenido2") (list "archivo3" "contenido3")))
(define index (list (list "uno.c" "codigo1") (list "dos.c" "codigo2")))
(define localR '())
(define remoteR '())
(define zonas (zonasCons workspace index localR remoteR))

;desc: función que añade los archivos de la lista 1, contenidos en el workspace, a una lista 2
;dom: lista x lista x lista
;rec: lista 
(define myAdd (lambda (workspace L1 L2)
                                    (if (null? L1)
                                        L2
                                        (if (null? workspace)
                                            null
                                            (if (equal? (car L1) (caar workspace))
                                                (myAdd workspace(cdr L1)(myAppend (car workspace) L2))
                                                (myAdd (cdr workspace) L1 L2))))))

;desc:Función que añade los cambios locales registrados en el Workspace al Index registrados en las zonas de trabajo
;dom: lista 
;rec: lista (nueva versión de zonas donde se ven reflejados los cambios)
(define add (lambda (lista) (lambda (zonas)
                              (setIndex zonas (myAdd (zonaWorkspace zonas) lista '()) ))))

;desc: función que crea un commit si hay cambios en el Index
;dom: string x lista
;rec: commit o 0 en caso que no hayan cambios en el Index
(define commitAux (lambda(mensaje zonas)
                    (if (null? (zonaIndex zonas))
                        0
                        (commitCons mensaje (zonaIndex zonas)))))
                    
;desc: Función que genera un commit con los cambios almacenados en index especificando un mensaje descriptivo (un string) para llevarlos al LocalRepository 
;dom: string
;rec: lista (nueva versión de zonas donde se ven reflejados los cambios.
(define commit (lambda (mensaje)
                 (lambda (zonas)
                   (if (equal? (commitAux mensaje zonas) 0)
                       (display "No hay cambios en el Index")
                       (setIndex (setLocalR zonas (myAppend (commitAux mensaje zonas) (zonaLocalR zonas))) '()))))) ;el Index queda vacío
