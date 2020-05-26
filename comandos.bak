#lang racket
(require "TDAzonas.rkt")
(require "TDAcommit.rkt")
(require "funcionesGenerales.rkt")

(define workspace (list (list "archivo1" "contenido1") (list "archivo2" "contenido2") (list "archivo3" "contenido3")))
(define index null)
(define localR (list (list "commit1" (list( list "file1" "contenido1") (list"file2" "contenido2"))) ))
(define remoteR (list (list "commit1" (list( list "file2" "contenido1") (list"file6" "contenido2"))) ))

(define zonas (zonasCons workspace index localR remoteR))

;desc: función que añade los archivos de la lista 1, contenidos en el workspace, a una lista 2
;dom: lista x lista x lista
;rec: lista
;tipo recursión: de cola
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
                              (setIndex zonas (myAppend2 (zonaIndex zonas)(myAdd (zonaWorkspace zonas) lista '()))))))


;desc: función que crea un commit si hay cambios en el Index
;dom: string x lista
;rec: commit o 0 en caso que no hayan cambios en el Index
(define commitAux (lambda(mensaje zonas)
                    (if (null? (zonaIndex zonas))
                        0
                        (commitCons mensaje (zonaIndex zonas)))))
                    
;desc: Función que genera un commit con los cambios almacenados en index especificando un mensaje descriptivo (un string) para llevarlos al LocalRepository 
;dom: string
;rec: lista (nueva versión de zonas donde se ven reflejados los cambios en el Index)
(define commit (lambda (mensaje)
                 (lambda (zonas)
                   (if (equal? (commitAux mensaje zonas) 0)
                       (display "No hay cambios en el Index")
                       (setIndex (setLocalR zonas (myAppend (commitAux mensaje zonas) (zonaLocalR zonas))) '()))))) ;el Index queda vacío

;desc: permite saber si un elemento es parte de una lista
;dom: elemento x lista
;rec: booleano
;tipo de recursión: de cola
(define esta? (lambda (elemento lista)
                   (if (null? lista)
                       #f
                       (if (equal? elemento (car lista))
                           #t
                           (esta? elemento (cdr lista))))))
;desc: Función auxiliar que entrega el Repositorio Remoto modificado
;dom: lista x lista
;rec: lista (repositorio remoto modificado)
;tipo de recursión: de cola
(define myPush (lambda (localR remoteR)
                                 (if (null? localR) ;si el Local Repository está vacío;se entrega el Remote Repository
                                        remoteR ;se entrega el Remote Repository
                                        (if (esta? (car localR) remoteR) ;se pregunta si el commit ya está en el Remote Repository
                                            (myPush (cdr localR) remoteR) ; si está, no se agrega a Remote Repository y se revisa la cola del Local Repository
                                            (myPush (cdr localR) (myAppend (car localR) remoteR)));si no está, se agrega el commit al Remote Repository
                                        )))

;desc: Función que envía los commit desde el repositorio local al repositorio remoto registrado en las zonas de trabajo
;dom: lista (zonas)
;rec: lista (nueva versión de zonas donde se ven reflejados los cambios en el Remote Repository)
(define push (lambda (zonas)
               (setRemoteR zonas (myPush (zonaLocalR zonas) (zonaRemoteR zonas)))))