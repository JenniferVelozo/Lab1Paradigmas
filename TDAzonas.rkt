#lang racket

;TDA zonas

;Constructor
;Desc: construye una lista de 4 listas 
;Dom: lista x lista x lista x lista
;Rec: una lista con 4 listas dentro
(define zonasCons (lambda (workspace index localR remoteR)
                (if (and (list? workspace) (list? index) (list? localR) (list? remoteR))
                    (list workspace index localR remoteR)
                    null)))

;Pertenencia
;Desc: verifica si una zona corresponde a una zona o no, la cual debe ser una lista, con 4 elementos dentro, los cuales también deben ser lista
;Dom: lista
;Rec: un valor booleano
(define zonas? (lambda (zonas)
                    (if (and (list? zonas) (= (length zonas) 4) (list? (car zonas)) (list? (cadr zonas)) (list? (caddr zonas)) (list? (cadddr zonas)))
                        #t
                        #f)))

;Selectores
;Desc: Indica un elemento específico de la lista zonas, ya sea el Workspace, Index, Local Repository o Remote Repository
;Dom: la lista zonas
;Rec: una lista o lista vacía
(define zonaWorkspace (lambda (zonas)
                        (if zonas?
                            (car zonas)
                            null
                            )))

(define zonaIndex (lambda (zonas)
                    (if zonas?
                        (cadr zonas)
                        null
                        )))

(define zonaLocalR(lambda (zonas)
                    (if zonas?
                        (caddr zonas)
                        null
                        )))

(define zonaRemoteR (lambda (zonas)
                      (if zonas?
                          (cadddr zonas)
                          null
                          )))

(define workspace '("hola0"))
(define index '("hola1"))
(define localR '("hola2"))
(define remoteR '("hola3"))
(define zonas (zonasCons workspace index localR remoteR))

(provide (all-defined-out))

