#lang racket
(require "TDAcommit.rkt")
(require "funcionesGenerales.rkt")
;TDA zonas
;La zonas de trabajo se representan por medio de una lista, donde cada elemento de ésta son listas que representan las respectivas zonas de trabajo
;workspace, index, local repository, remote repository y el registro de comandos.
;zonas: (workspace, index, local repository, remote repository, registro de comandos)

;Constructor
;Desc: permite crear zonas. Posee el workspace, index, local repository, remote repository y un registro de comandos
;Dom: lista x lista x lista x lista x lista
;Rec: lista
(define zonasCons (lambda (workspace index localR remoteR registroComandos)
                (if (and (list? workspace) (list? index) (list? localR) (list? remoteR) (list? registroComandos)
                         (or (null? workspace) (andmap esArchivo? workspace))
                         (or (null? index) (andmap esArchivo? index))
                         (or (null? localR) (andmap commit? localR))
                         (or (null? remoteR) (andmap commit? remoteR)))
                    (list workspace index localR remoteR registroComandos) ;se crea una lista de listas
                    null))) ;sino se entrega una lista vacía

;Pertenencia
;Desc: Función que permite determinar si un elemento cualquiera es del tipo zonas
;      se implementa a partir del constructor
;      evaluando el retorno
;Dom: elemento de cualquier tipo
;Rec: booleano
(define zonas? (lambda (zonas)
                    ;para que un elemeento cualquiera sea de tipo zonas, debe ser una lista de largo 5, y cada uno de los elementos deben ser listas
                    (if (and (list? zonas) (= (length zonas) 5) (list? (car zonas)) (list? (cadr zonas)) (list? (caddr zonas))
                             (list? (cadddr zonas)) (list? (car(cdr(cdr(cdr(cdr zonas)))))))
                        #t
                        #f)))

;Selectores
;descripción: Función que retorna la zona Workspace en zonas
;dom: zonas (lista)
;rec: workspace (lista)
(define zonaWorkspace (lambda (zonas)
                        (if (zonas? zonas) ;si es una zona
                            (car zonas) ;se entrega el primer elemento de zonas
                            0 ;sino un 0, ya que una lista puede ser nula
                            )))
;descripción: Función que retorna la zona Index en zonas
;dom: zonas (lista)
;rec: index (lista)
(define zonaIndex (lambda (zonas)
                    (if (zonas? zonas) ;si es una zona
                        (cadr zonas);se entrega el segundo elemento de zonas
                        0 ;sino un 0, ya que una lista puede ser nula
                        )))
;descripción: Función que retorna la zona Local Repository en zonas
;dom: zonas (lista)
;rec: local repository (lista)
(define zonaLocalR(lambda (zonas)
                    (if (zonas? zonas) ;si es una zona
                        (caddr zonas) ;se entrega el tercer elemento de zonas
                        0 ;sino un 0, ya que una lista puede ser nula
                        )))
;descripción: Función que retorna la zona Remote Repository en zonas
;dom: zonas
;rec: remote repository (lista)
(define zonaRemoteR (lambda (zonas)
                      (if (zonas? zonas) ;si es una zona
                          (cadddr zonas) ;se entrega el cuarto elemento de zonas
                          0 ;sino un 0, ya que una lista puede ser nula
                          )))

;descripción: Función que retorna el registro de comandos en zonas
;dom: zonas (lista)
;rec: comandos (lista)
(define comandos (lambda (zonas)
                   (if (zonas? zonas) ;si es una zona
                       (car(cdr(cdr(cdr(cdr zonas))))) ;se entrega el quinto elemento de zonas
                       0 ;sino un 0, ya que una lista puede ser nula
                       )))
;Modificadores
;desc: crea nuevas zonas a partir de zonas de entrada reemplazando el valor correspondiente al la zona de trabajo Workspace
;dom: zonas(lista) x lista
;rec: zonas
(define setWorkspace (lambda (zonas listaWorkspace)
                   (zonasCons listaWorkspace (zonaIndex zonas) (zonaLocalR zonas) (zonaRemoteR zonas) (comandos zonas))))

;desc: crea nuevas zonas a partir de zonas de entrada reemplazando el valor correspondiente al la zona de trabajo Index
;dom: zonas(lista) x lista
;rec: zonas
(define setIndex (lambda (zonas listaIndex)
                   (zonasCons (zonaWorkspace zonas) listaIndex (zonaLocalR zonas) (zonaRemoteR zonas) (comandos zonas))))

;desc: crea nuevas zonas a partir de zonas de entrada reemplazando el valor correspondiente al la zona de trabajo Local Repository
;dom: zonas(lista) x lista
;rec: zonas
(define setLocalR (lambda (zonas listaLocal)
                    (zonasCons (zonaWorkspace zonas) (zonaIndex zonas) listaLocal (zonaRemoteR zonas) (comandos zonas))))

;desc: crea nuevas zonas a partir de zonas de entrada reemplazando el valor correspondiente al la zona de trabajo Remote Repository
;dom: zonas (lista) x lista
;rec: zonas
(define setRemoteR (lambda (zonas listaRemote)
                    (zonasCons (zonaWorkspace zonas) (zonaIndex zonas) (zonaLocalR zonas) listaRemote (comandos zonas))))

;desc: crea nuevas zonas a partir de zonas de entrada reemplazando el valor correspondiente al registro de comandos
;dom: zonas (lista) x lista
;rec: zonas
(define setRegistroComandos (lambda (zonas listaComandos)
                              (zonasCons (zonaWorkspace zonas) (zonaIndex zonas) (zonaLocalR zonas) (zonaRemoteR zonas) listaComandos )))
(provide (all-defined-out))

