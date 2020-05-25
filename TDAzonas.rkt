#lang racket

;TDA zonas

;Constructor
;Desc: permite crear zonas 
;Dom: lista x lista x lista x lista
;Rec: lista
(define zonasCons (lambda (workspace index localR remoteR)
                (if (and (list? workspace) (list? index) (list? localR) (list? remoteR))
                    (list workspace index localR remoteR)
                    null)))

;Pertenencia
;Desc: Función que permite determinar si un elemento cualquiera es del tipo zonas
;      se implementa a partir del constructor
;      evaluando el retorno
;Dom: elemento de cualquier tipo
;Rec: booleano
(define zonas? (lambda (zonas)
                    (if (and (list? zonas) (= (length zonas) 4) (list? (car zonas)) (list? (cadr zonas)) (list? (caddr zonas)) (list? (cadddr zonas)))
                        #t
                        #f)))

;Selectores
;descripción: Función que retorna la zona Workspace en zonas
;dom: zonas
;rec: una lista o 0, ya que esa lista también puede ser nula
(define zonaWorkspace (lambda (zonas)
                        (if zonas?
                            (car zonas)
                            0
                            )))
;descripción: Función que retorna la zona Index en zonas
;dom: zonas
;rec: una lista o 0, ya que esa lista también puede ser nula
(define zonaIndex (lambda (zonas)
                    (if zonas?
                        (cadr zonas)
                        0
                        )))
;descripción: Función que retorna la zona Local Repository en zonas
;dom: zonas
;rec: una lista o 0, ya que esa lista también puede ser nula
(define zonaLocalR(lambda (zonas)
                    (if zonas?
                        (caddr zonas)
                        0
                        )))
;descripción: Función que retorna la zona Remote Repository en zonas
;dom: zonas
;rec: una lista o 0, ya que esa lista también puede ser nula
(define zonaRemoteR (lambda (zonas)
                      (if zonas?
                          (cadddr zonas)
                          0
                          )))

;Modificadores
;desc: crea nuevas zonas a partir de zonas de entrada reemplazando el valor correspondiente al la zona de trabajo Index
;dom: lista x lista
;rec: zonas
(define setIndex (lambda (zonas listaIndex)
                   (zonasCons (zonaWorkspace zonas) listaIndex (zonaLocalR zonas) (zonaRemoteR zonas))))

;desc: crea nuevas zonas a partir de zonas de entrada reemplazando el valor correspondiente al la zona de trabajo Local Repository
;dom: lista x lista
;rec: zonas
(define setLocalR (lambda (zonas listaLocal)
                    (zonasCons (zonaWorkspace zonas) (zonaIndex zonas) listaLocal (zonaRemoteR zonas))))

;desc: crea nuevas zonas a partir de zonas de entrada reemplazando el valor correspondiente al la zona de trabajo Remote Repository
;dom: lista x lista
;rec: zonas
(define setRemoteR (lambda (zonas listaRemote)
                    (zonasCons (zonaWorkspace zonas) (zonaIndex zonas) (zonaLocalR zonas) listaRemote)))

(provide (all-defined-out))

