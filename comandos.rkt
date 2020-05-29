#lang racket
(require "TDAzonas.rkt")
(require "TDAcommit.rkt")
(require "funcionesGenerales.rkt")

(define workspace (list (list "archivo1" "contenido1") (list "archivo2" "contenido2") (list "archivo3" "contenido3")))
(define index (list (list "file1.rkt" "codigo1") (list "file2.rkt" "codigo2")))
(define localR (list (list "Edición" (list( list "file1" "contenido1") (list"file2" "contenido2"))) ))
(define remoteR (list (list "Se editó archivo 2 y 6" (list( list "file2" "contenido1") (list"file6" "contenido2")))))

(define zonas (zonasCons workspace index localR remoteR))
;----------------------------------- FUNCIÓN GIT ----------------------------------------------
;desc:Función que permite aplicar los comandos sobre las zonas de trabajo.
;dom: funcion x parámetro
;rec: una función que recibe los parámetros propios del comando que se pretende aplicar.
(define git (lambda (funcion)
              (lambda(parametro)
                (if (not(or (equal? funcion add) (equal? funcion push) (equal? funcion commit) (equal? funcion zonas->string)))
                    (display "El comando ingresado no existe")
                    (funcion parametro)))))
                    
;----------------------------------- FUNCIÓN PULL ---------------------------------------------

;----------------------------------- FUNCIÓN ADD ----------------------------------------------
;desc: función que añade los archivos de la lista 1, contenidos en el workspace, a una lista 2
;dom: lista x lista x lista
;rec: lista
;tipo recursión: de cola
(define myAdd (lambda (workspace L1 L2)
                                    (if (null? L1);si la lista ingresada es nula, se retorna la lista 2
                                        L2
                                        (if (null? workspace) ;si el workspace es nulo, se retorna null
                                            null
                                            (if (equal? (car L1) (caar workspace)) ;se pregunta si el nombre del primer archivo es igual al nombre del primer archivo del workspace
                                                (myAdd workspace(cdr L1)(myAppend (car workspace) L2));si es así, se revisa la cola de la lista 1, y se agrega el archivo revisado anteriormente a la lista 2
                                                (myAdd (cdr workspace) L1 L2))))));sino, se revisa la misma lista1 pero en la cola del workspace

;desc:Función que añade los cambios locales registrados en el Workspace al Index registrados en las zonas de trabajo
;dom: lista 
;rec: lista (nueva versión de zonas donde se ven reflejados los cambios)
(define add (lambda (lista) (lambda (zonas)
                              (if (and (list? lista) (zonas? zonas))
                                  ;se modifica el Index, uniendo la lista Index con la lista de archivos que se desea agregar
                                  (setIndex zonas (myAppend2 (zonaIndex zonas)(myAdd (zonaWorkspace zonas) lista '())))
                                  (display "No se ingresaron los parametros correctos");para indicar que no e ingresaron los paramtros correctos
                              ))))
                                  
                              

;----------------------------------- FUNCIÓN COMMIT ----------------------------------------------
;desc: función que crea un commit si hay cambios en el Index
;dom: string x lista
;rec: commit o 0 en caso que no hayan cambios en el Index
(define commitAux (lambda(mensaje zonas)
                    (if (null? (zonaIndex zonas)) ;si el Index está vacío, se retorna un 0
                        0
                        (commitCons mensaje (zonaIndex zonas))))) ;sino, se crea un commit en base al mensaje y el Index
                    
;desc: Función que genera un commit con los cambios almacenados en index especificando un mensaje descriptivo (un string) para llevarlos al LocalRepository 
;dom: string
;rec: lista (nueva versión de zonas donde se ven reflejados los cambios en el Index)
(define commit (lambda (mensaje)
                 (lambda (zonas)
                   (if (and(string? mensaje) (zonas? zonas))
                       (if (equal? (commitAux mensaje zonas) 0) ;si el Index está vaciío, significa que no hay cambios en él
                       (display "No hay cambios en el Index")
                       ;Sino, se modifica el index, el cual queda vacío, y además, se modifica el Local Repostiroy, al cual se le agrega el commit creado
                       (setIndex (setLocalR zonas (myAppend (commitAux mensaje zonas) (zonaLocalR zonas))) '()))
                       (display "No se ingresaron los parametros correctos"))
                   )))

;----------------------------------- FUNCIÓN PUSH ----------------------------------------------
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
;desc: Función auxiliar que entrega el Repositorio Remoto modificado
;dom: lista x lista
;rec: lista (repositorio remoto modificado)
;tipo de recursión: de cola
(define myPush (lambda (localR remoteR)
                                 (if (null? localR) ;si el Local Repository está vacío,se entrega el Remote Repository
                                        remoteR
                                        (if (esta? (car localR) remoteR) ;se pregunta si el commit ya está en el Remote Repository
                                            (myPush (cdr localR) remoteR) ; si está, no se agrega a Remote Repository y se revisa la cola del Local Repository
                                            (myPush (cdr localR) (myAppend (car localR) remoteR)));si no está, se agrega el commit al Remote Repository
                                        )))

;desc: Función que envía los commit desde el repositorio local al repositorio remoto registrado en las zonas de trabajo
;dom: lista (zonas)
;rec: lista (nueva versión de zonas donde se ven reflejados los cambios en el Remote Repository)
(define push (lambda (zonas)
               (if (zonas? zonas)
                   ;se modifica el repositorio remoto
                   (setRemoteR zonas (myPush (zonaLocalR zonas) (zonaRemoteR zonas)))
                   (display "El parametro ingresado no corresponde a una zona"))
               ))

;----------------------------------- FUNCIÓN ZONAS->STRING ----------------------------------------------
;desc: Función que transforma el Workspace y el Index a un string
;dom: lista x string
;rec: string
;tipo de recursión: de cola
(define listToString (lambda (lista string)
                   (if (null? lista);si la lista es nula, se retorna el string
                       string
                       ;sino, se revisa la cola de la lista y se unen los strings, es decir, cada archivo con su respectivo contenido
                       (listToString (cdr lista) (string-append string (string-append (car(car lista)) " -> " (car(cdr(car lista))) "\n"))))))
;desc: Función que transforma el Local Repository y el Remote Repository a un string
;dom: lista x string
;rec: string
;tipo de recursión: de cola
(define listToString2 (lambda (lista string)
                        (if (null? lista);si la lista es nula, se retorna el string
                            string
                            ;sino , se revisa la cola la lista y se unen los strings, es decir el mensaje del commit y los cambios hechos (archivos)
                            (listToString2 (cdr lista) (string-append string "Mensaje: "(mensaje(car lista)) "\n" "Archivos: \n"(listToString (cambios(car lista)) "") " \n ")))))
;desc: Función que recibe las zonas de trabajo y entrega una representación de las mismas como un string.
;dom:lista (zonas)
;rec: string
(define zonas->string (lambda (zonas)
                        (if (zonas? zonas);si la zona ingresada corresponde al tipo zonas, se entrega un string, respresentando las zonas
                            (string-append "\n**** CONTENIDO WORKSPACE **** \n" (listToString (zonaWorkspace zonas) "")
                                                "\n**** CONTENIDO INDEX **** \n" (listToString (zonaIndex zonas) "")
                                                "\n**** CONTENIDO LOCAL REPOSITORY **** \n" (listToString2 (zonaLocalR zonas) "")
                                                "\n**** CONTENIDO REMOTE REPOSITORY **** \n" (listToString2 (zonaRemoteR zonas) ""))
                            "El parametro ingresado no corresponde a una zona");sino se retorna un mensaje de error
                        ))
