#lang racket
(require "TDAzonas.rkt")
(require "TDAcommit.rkt")
(require "codigo.rkt")

(define workspace '("file3.rkt"))
(define index '("file40" "file45"))
(define localR '("hola2"))
(define remoteR '("hola3"))
(define zonas (zonasCons workspace index localR remoteR))

#|(define setIndex (lambda (listaindex elemento)
                   (myAppend elemento listaindex)))|#

(define add (lambda (lista)
              (define add1 (lambda (lista) (lambda (zonas)
                              (myAppend lista (zonaIndex zonas)))))
              ((add1 lista) zonas)))

(define commit (lambda (mensaje)
                 (if (string? mensaje)
                     (commitCons (getLast index ) mensaje )
                     #f)))


;(define pull (lambda ()))
;(define git (lambda(comando)
 ;             ))
