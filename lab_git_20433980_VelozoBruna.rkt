#lang racket
(require "TDAzonas.rkt")
(require "TDAcommit.rkt")
(require "funcionesGenerales.rkt")

;(define workspace (list (list "archivo1" "contenido1") (list "archivo2" "contenido2") (list "archivo3" "contenido3")))
;(define index (list (list "file1.rkt" "codigo1") (list "file2.rkt" "codigo2")))
;(define localR (list (list "Edición" (list( list "file1" "contenido1") (list"file2" "contenido2"))) ))
;(define remoteR (list (list "Se editó archivo 2 y 6" (list( list "file2" "contenido1") (list"file6" "contenido2"))) (list "Se editó archivo 2" (list( list "file2" "contenido1ver2") (list"file6" "contenido2")))))

;(define zonas (zonasCons workspace index localR remoteR null))
;-------------------------------------------------------------- FUNCIÓN GIT --------------------------------------------------------------------------------------
;desc:Función que permite aplicar los comandos sobre las zonas de trabajo.
;dom: funcion x parámetro
;rec: una función que recibe los parámetros propios del comando que se pretende aplicar.
(define git (lambda (funcion)
              (lambda(parametro)
                ;si la función ingresada no es igual a pull, add, push, commit o zonas->string
                (if (not(or (equal? funcion pull)(equal? funcion add) (equal? funcion push) (equal? funcion commit) (equal? funcion zonas->string)))
                    ;se entrega un mensaje avisando que comando ingresado no existe
                    (display "El comando ingresado no existe")
                    ;sino, se aplica la funcion al parametro ingresado
                    (funcion parametro)))))
 
;---------------------------------------------------------------- FUNCIÓN PULL ----------------------------------------------------------------------------------
;desc: función que entrega una lista con los cambios de un commit
;dom: cambios hechos (lista)
;rec: lista
;tipo de recursión: natural
(define myPull1 (lambda(cambios)
                  (if (null? (cdr cambios))
                      (list(car cambios))
                      (append (list(car cambios)) (myPull1 (cdr cambios))))))

;desc: función que entrega una lista sólo con los archivos que posee el Remote Repository
;dom: lista (remote repository)
;rec: lista
;tipo de recursión: natural
(define myPull2 (lambda (remoteR)
                  (if (null? remoteR)
                      null
                      (if (null? (cdr remoteR))
                      (myPull1 (cambios (car remoteR)))
                      (append (myPull1 (cambios (car remoteR))) (myPull2 (cdr remoteR)))))))
                  
;desc: función que entrega una lista de aquellos archivos de la lista 2 que no están en la lista 1
;dom: lista x lista
;rec: lista
;tipo de recursión: natural
(define myPull3 (lambda (L1 L2)
                  (if (null? L1)
                      L2
                      (if (noEstaArchivo? (car L1) L2)
                          (myPull3 (cdr L1) (append (list (car L1)) L2))
                          (myPull3 (cdr L1) L2)
                          ))))
;desc:Función que retorna una lista con todos los cambios (commits) desde el RemoteRepository al Workspace 
;dom: zonas
;rec: lista (nueva versión de zonas)
(define pull (lambda (zonas)
               (if (zonas? zonas)
                   (setRegistroComandos (setWorkspace zonas (myPull3 (zonaWorkspace zonas) (myPull3 (reverse (myPull2 (zonaRemoteR zonas))) null)))
                                        (myAppend "pull" (comandos zonas)))
                   (display "El parametro ingresado no crresponde a una zona"))))

;--------------------------------------------------------------- FUNCIÓN ADD -------------------------------------------------------------------------------------
;desc: función que añade los archivos de la lista 1, contenidos en el workspace, a una lista 2
;dom: lista x lista x lista
;rec: lista
;tipo recursión: de cola
(define myAdd (lambda (workspace L1 L2)
                                    (if (null? L1);si la lista ingresada es nula, se retorna la lista 2
                                        L2
                                        (if (null? workspace) ;si el workspace es nulo, se retorna null
                                            null
                                            (if (equal? (car L1) (caar workspace))
                                                ;se pregunta si el nombre del primer archivo es igual al nombre del primer archivo del workspace
                                                (myAdd workspace(cdr L1)(myAppend (car workspace) L2));si es así, se revisa la cola de la lista 1, y se agrega el archivo revisado anteriormente a la lista 2
                                                (myAdd (cdr workspace) L1 L2))))));sino, se revisa la misma lista1 pero en la cola del workspace

;desc: función que entrega una lista de aquellos archivos que no estén en el Index, para que no se agreguen nuevamente
;dom: lista x lista 
;rec: lista
;tipo de recursión: de cola
(define appendIndex (lambda (add index)
                      (if (null? add)
                          index
                          (if (noEstaArchivo? (car add) index)
                              (appendIndex (cdr add) (append index (list (car add))))
                              (appendIndex (cdr add) index )))))
;desc:Función que añade los cambios locales registrados en el Workspace al Index registrados en las zonas de trabajo
;dom: lista 
;rec: lista (nueva versión de zonas donde se ven reflejados los cambios)
(define add (lambda (lista) (lambda (zonas)
                              (if (and (list? lista) (zonas? zonas))
                                  (if (andmap string? lista)
                                      
                                      ;se modifica el Index, uniendo la lista Index con la lista de archivos que se desea agregar
                                      (setRegistroComandos (setIndex zonas (appendIndex (myAdd (zonaWorkspace zonas) lista '()) (zonaIndex zonas))) (myAppend "add" (comandos zonas)))
                                      (display "Debe ingresar una lista de archivos"));para indicar que no e ingresaron los parametros correctos
                           
                                  (display "No se ingresaron los parametros correctos")))))
                                  
                              

;-------------------------------------------------------------- FUNCIÓN COMMIT -----------------------------------------------------------------------------------
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
                            (display "No hay cambios en el Index\n")
                            ;Sino, se modifica el index, el cual queda vacío, y además, se modifica el Local Repostiroy, al cual se le agrega el commit creado
                            (setRegistroComandos (setIndex (setLocalR zonas (myAppend (commitAux mensaje zonas) (zonaLocalR zonas))) '()) (myAppend "commit" (comandos zonas))))
                       (display "No se ingresaron los parametros correctos\n"))
                   )))

;------------------------------------------------------------- FUNCIÓN PUSH --------------------------------------------------------------------------------------
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
                   (setRegistroComandos (setRemoteR zonas (myPush (zonaLocalR zonas) (zonaRemoteR zonas))) (myAppend "push" (comandos zonas)))
                   (display "El parametro ingresado no corresponde a una zona"))
               ))

;------------------------------------------------------------- FUNCIÓN ZONAS->STRING -----------------------------------------------------------------------------
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
                        (if (andmap commit? lista)
                     
                        (if (null? lista);si la lista es nula, se retorna el string
                            string
                            ;sino , se revisa la cola la lista y se unen los strings, es decir el mensaje del commit y los cambios hechos (archivos)
                            (listToString2 (cdr lista) (string-append string "Mensaje: "(mensaje(car lista)) "\n"
                                                                      "Archivos: \n"(listToString (cambios(car lista)) "") " \n ")))
                        0)))

;desc: Función que transforma el registro de comandos a string
;dom: lista x string
;rec: string
;tipo de recursión: de cola
(define listToString3 (lambda (lista string)
                        (if (null? lista)
                            string
                            (listToString3 (cdr lista) (string-append string (car lista) "\n")))))
;desc: Función que recibe las zonas de trabajo y entrega una representación de las mismas como un string.
;dom:lista (zonas)
;rec: string
(define zonas->string (lambda (zonas)
                        (if (zonas? zonas);si la zona ingresada corresponde al tipo zonas, se entrega un string, respresentando las zonas
                            (string-append "\n**** CONTENIDO WORKSPACE **** \n" (listToString (zonaWorkspace zonas) "")
                                                "\n**** CONTENIDO INDEX **** \n" (listToString (zonaIndex zonas) "")
                                                "\n**** CONTENIDO LOCAL REPOSITORY **** \n" (listToString2 (zonaLocalR zonas) "")
                                                "\n**** CONTENIDO REMOTE REPOSITORY **** \n" (listToString2 (zonaRemoteR zonas) "")
                                                "\n**** REGISTRO DE COMANDOS **** \n" (listToString3 (comandos zonas) ""))
                            "El parametro ingresado no corresponde a una zona");sino se retorna un mensaje de error
                        ))

;----------------------------------------------------------- EJEMPLOS DE USO ------------------------------------------------------------------------------------
;TDA zonas
;se definen previamente las zona de trabajo
#|
(define workspace (list (list "archivo1" "contenido1") (list "archivo2" "contenido2") (list "archivo3" "contenido3")))
(define index (list (list "file1.rkt" "codigo1") (list "file2.rkt" "codigo2")))
(define localR (list (list "Edición" (list( list "file1" "contenido1") (list"file2" "contenido2"))) ))
(define remoteR (list (list "Se editó archivo 2 y 6" (list( list "file2" "contenido1") (list"file6" "contenido2"))) (list "Se editó archivo 2" (list( list "file2" "contenido1ver2") (list"file6" "contenido2")))))

;3 ejemplos de uso
;uso del constructor
(zonasCons workspace index localR remoteR null);retorna una lista con las zonas de trabajo
;uso de pertenencia
(zonas? (list "archivo1.rkt" "archivo2.rkt")) ;en este caso retorna falso, ya que no es del tipo zonas
;uso de un selector
(zonaIndex (zonasCons workspace index localR remoteR null)) ;retorna lista de archivos del index
|#

;TDA commit
;uso del constructor
#|
(commitCons "mi primer commit" (list (list "archivo1.rkt" "contenido1") (list "archivo2.rkt" "archivo2")))
;uso de pertenencia
(commit? (list "mi primer commit" (list (list "file1.rkt" "contenido1") (list "file2.rkt" "contenido2"))))
;uso de un selector
(mensaje (list "mi primer commit" (list (list "file1.rkt" "contenido1") (list "file2.rkt" "contenido2"))))
|#

;FUNCIÓN GIT

#|
;Se definen previamente las zonas de trabajo
(define workspace (list (list "archivo1.rkt" "contenido1") (list "archivo2.rkt" "contenido2") (list "archivo3.rkt" "contenido3")))
(define index (list (list "codigo1.rkt" "contenido codigo1") (list "codigo2.rkt" "contenido codigo2")))
(define localR (list (list "Edición" (list( list "file1.rkt" "contenido1 versión 2") (list "file2.rkt" "contenido2 versión 5"))) ))
(define remoteR (list (list "Se editó archivo 2 y 6" (list( list "file2.rkt" "contenido1") (list"file6.rkt" "contenido2"))) (list "Se editó archivo 2" (list( list "file2.rkt" "contenido1ver2") (list"file6.rkt" "contenido2")))))

(define zonas (zonasCons workspace index localR remoteR null))
;3 ejemplos de uso de git
((git push)zonas)
(((git add)'("archivo1.rkt" "archivo3.rkt"))zonas)
(((git commit)"myCommit")zonas)
|#

;FUNCIÓN PULL
#|
;Se definen previamente las zonas de trabajo
(define workspace (list (list "archivo1.rkt" "contenido1") (list "archivo2.rkt" "contenido2") (list "archivo3.rkt" "contenido3")))
(define index (list (list "codigo1.rkt" "contenido codigo1") (list "codigo2.rkt" "contenido codigo2")))
(define localR (list (list "Edición" (list( list "file1.rkt" "contenido1 versión 2") (list "file2.rkt" "contenido2 versión 5"))) ))
(define remoteR (list (list "Se editó archivo 2 y 6" (list( list "file2.rkt" "contenido1") (list"file6.rkt" "contenido2"))) (list "Se editó archivo 2" (list( list "file2.rkt" "contenido1ver2") (list"file6.rkt" "contenido2 actualizado")))))
(define zonas (zonasCons workspace index localR remoteR null))

;3 ejemplos de uso de pull
(pull zonas)
(pull ((add (list "archivo1.rkt" "archivo2.rkt"))zonas))
(pull ((commit "mi primer commit")zonas))
|#

;FUNCIÓN ADD
#|
;Se definen previamente las zonas de trabajo
(define workspace (list (list "archivo1.rkt" "contenido1") (list "archivo2.rkt" "contenido2") (list "archivo3.rkt" "contenido3")))
(define index (list (list "codigo1.rkt" "contenido codigo1") (list "codigo2.rkt" "contenido codigo2")))
(define localR (list (list "Edición" (list( list "file1.rkt" "contenido1 versión 2") (list "file2.rkt" "contenido2 versión 5"))) ))
(define remoteR (list (list "Se editó archivo 2 y 6" (list( list "file2.rkt" "contenido1") (list"file6.rkt" "contenido2"))) (list "Se editó archivo 2" (list( list "file2.rkt" "contenido1ver2") (list"file6.rkt" "contenido2")))))

(define zonas (zonasCons workspace index localR remoteR null))

;3 ejemplos de uso de add
((add (list "archivo3.rkt"))zonas)
((add (list "archivo1.rkt" "archivo2.rkt"))zonas)
((add (list "archivo2.rkt"))zonas)
|#

;FUNCIÓN COMMIT
#|
;Se definen previamente las zonas de trabajo
(define workspace (list (list "archivo1.rkt" "contenido1") (list "archivo2.rkt" "contenido2") (list "archivo3.rkt" "contenido3")))
(define index (list (list "codigo1.rkt" "contenido codigo1") (list "codigo2.rkt" "contenido codigo2")))
(define localR (list (list "Edición" (list( list "file1.rkt" "contenido1 versión 2") (list "file2.rkt" "contenido2 versión 5"))) ))
(define remoteR (list (list "Se editó archivo 2 y 6" (list( list "file2.rkt" "contenido1") (list"file6.rkt" "contenido2"))) (list "Se editó archivo 2" (list( list "file2.rkt" "contenido1ver2") (list"file6.rkt" "contenido2")))))

(define zonas (zonasCons workspace index localR remoteR null))

;3 ejemplos de uso commit
((commit "mi segundo commit")zonas)
((commit "prueba")'((("archivo1.rkt" "contenido1") ("archivo2.rkt" "contenido2") ("archivo3.rkt" "contenido3"))
  ()
  (("Edición" (("file1.rkt" "contenido1 versión 2") ("file2.rkt" "contenido2 versión 5")))
   ("mi segundo commit" (("codigo1.rkt" "contenido codigo1") ("codigo2.rkt" "contenido codigo2"))))
  (("Se editó archivo 2 y 6" (("file2.rkt" "contenido1") ("file6.rkt" "contenido2")))
   ("Se editó archivo 2" (("file2.rkt" "contenido1ver2") ("file6.rkt" "contenido2"))))
  ("commit")))

((commit "tercer commit")(push zonas))
|#

;FUNCIÓN PUSH
;Se definen previamente las zonas de trabajo
#|
(define workspace (list (list "archivo1.rkt" "contenido1") (list "archivo2.rkt" "contenido2") (list "archivo3.rkt" "contenido3")))
(define index (list (list "codigo1.rkt" "contenido codigo1") (list "codigo2.rkt" "contenido codigo2")))
(define localR (list (list "Edición" (list( list "file1.rkt" "contenido1 versión 2") (list "file2.rkt" "contenido2 versión 5"))) ))
(define remoteR (list (list "Se editó archivo 2 y 6" (list( list "file2.rkt" "contenido1") (list"file6.rkt" "contenido2"))) (list "Se editó archivo 2" (list( list "file2.rkt" "contenido1ver2") (list"file6.rkt" "contenido2")))))

(define zonas (zonasCons workspace index localR remoteR null))

;3 ejemplos de uso de push
(push zonas)
(push ((commit "mi primer commit")zonas))
(push ((add '("archivo1.rkt" "archivo3.rkt"))zonas))
|#

;FUNCIÓN ZONAS->STRING
;Se definen previamente las zonas de trabajo
#|
(define workspace (list (list "archivo1.rkt" "contenido1") (list "archivo2.rkt" "contenido2") (list "archivo3.rkt" "contenido3")))
(define index (list (list "codigo1.rkt" "contenido codigo1") (list "codigo2.rkt" "contenido codigo2")))
(define localR (list (list "Edición" (list( list "file1.rkt" "contenido1 versión 2") (list "file2.rkt" "contenido2 versión 5"))) ))
(define remoteR (list (list "Se editó archivo 2 y 6" (list( list "file2.rkt" "contenido1") (list"file6.rkt" "contenido2"))) (list "Se editó archivo 2" (list( list "file2.rkt" "contenido1ver2") (list"file6.rkt" "contenido2")))))

(define zonas (zonasCons workspace index localR remoteR null))
;3 ejemplos de uso de zonas->string
(zonas->string zonas)
(zonas->string (pull zonas))
(zonas->string ((commit "primer commit") ((add (list "archivo2.rkt"))zonas)))
|#