#lang racket
;TDA commit

;Constructor
;Desc: construye un commit en base a 2 palabras que corresponden al mensaje y los cambios hechos
;Dom: string x string
;Rec: una lista con 2 strings
(define commitCons (lambda (palabra1 palabra2)
                (if (and (string? palabra1) (string? palabra2))
                    (list palabra1 palabra2)
                    null)))

;Pertenencia
;Desc: verifica si el elemento entregado corresponde a un commit o no, el cual debe ser una lista de 2 elementos, donde cada uno de ellos es un string 
;Dom: lista
;Rec: un valor booleano
(define commit? (lambda (commit)
                    (if (and (list? commit)(= (length commit) 2) (string? (car commit)) (string? (cadr commit)))
                        #t
                        #f)))

;Selectores
;Desc: entrega un elemento específico del commit, ya sea el mensaje o los cambios hechos del commit
;Dom: lista
;Rec: un string o lista vacía
(define mensaje (lambda (commit)
                        (if commit?
                            (car commit)
                            null
                            )))

(define cambios (lambda (commit)
                    (if commit?
                        (cadr commit)
                        null
                        )))

(provide (all-defined-out))

