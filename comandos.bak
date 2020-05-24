#lang racket
(require "TDAzonas.rkt")

(define workspace (list (list "archivo1" "contenido1") (list "archivo2" "contenido2") (list "archivo3" "contenido3")))
(define index '())
(define localR '())
(define remoteR '())

(define zonas (zonasCons workspace index localR remoteR))

(define myAdd (lambda (workspace L1 L2)
                                    (if (null? L1)
                                        L2
                                        (if (null? workspace)
                                            null
                                            (if (equal? (car L1) (caar workspace))
                                                (myAdd workspace(cdr L1)(append L2 (list(car workspace))))
                                                (myAdd (cdr workspace) L1 L2))))))

(define add (lambda (lista) (lambda (zonas)
                              (setIndex zonas (myAdd (zonaWorkspace zonas) lista '() )))))

