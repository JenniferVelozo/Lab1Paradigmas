#lang racket
;TDA commit

;Constructor
;Desc: permite crear un commit, el cual contiene un mensaje y los cambios hechos
;Dom: string x lista
;Rec: una lista
(define commitCons (lambda (mensaje cambios)
                (if (and (string? mensaje) (list? cambios))
                    (list mensaje cambios)
                    null)))

;Pertenencia
;Desc: verifica si el elemento entregado corresponde a un commit o no, el cual debe ser una lista de 2 elementos, donde cada uno es string y el otro es una lista 
;Dom: lista
;Rec: un valor booleano
(define commit? (lambda (commit)
                    (if (and (list? commit)(= (length commit) 2) (string? (car commit)) (list? (cadr commit)))
                        #t
                        #f)))

;Selectores
;descripción: Función que retorna el mensaje en un commit
;dom: commit
;rec: string
(define mensaje (lambda (commit)
                        (if commit?
                            (car commit)
                            null
                            )))

;descripción: Función que retorna cambios en un commit
;dom: commit
;rec: lista
(define cambios (lambda (commit)
                    (if commit?
                        (cadr commit)
                        0 ;ya que los cambios son una lista y puede estar vacía
                        )))

(provide (all-defined-out))

