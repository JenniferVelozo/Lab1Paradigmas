#lang racket
;TDA commit

;Constructor
;Desc: construye un commit en base a 3 palabras que corresponden al autor del commit, el mensaje y los cambios hechos
;Dom: string x string x string
;Rec: una lista con 3 strings
(define commitCons (lambda (palabra1 palabra2 palabra3)
                (if (and (string? palabra1) (string? palabra2) (string? palabra3))
                    (list palabra1 palabra2 palabra3)
                    null)))

;Pertenencia
;Desc: verifica si el elemento entregado corresponde a un commit o no, el cual debe ser una lista de 3 elementos, donde cada uno de ellos es un string 
;Dom: lista
;Rec: un valor booleano
(define commit? (lambda (commit)
                    (if (and (list? commit)(= (length commit) 3) (string? (car commit)) (string? (cadr commit)) (string? (caddr commit)))
                        #t
                        #f)))

;Selectores
;Desc: entrega un elemento específico del commit, ya sea el autor, el mensaje o los cambios de este
;Dom: lista
;Rec: un string o lista vacía
(define autor (lambda (commit)
                        (if commit?
                            (car commit)
                            null
                            )))

(define mensaje (lambda (commit)
                    (if commit?
                        (cadr commit)
                        null
                        )))

(define cambios(lambda (commit)
                    (if commit?
                        (caddr commit)
                        null
                        )))


(define palabra1 "Jennifer")
(define palabra2 "edicion")
(define palabra3 "cambios")
