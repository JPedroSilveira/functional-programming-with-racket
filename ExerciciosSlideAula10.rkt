;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname ExerciciosSlideAula10) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp")) #f)))
(define-struct carta (valor nipe))
;Um elemento do conjunto carta tem o formato:
;(make-carta valor nipe)
;onde:
;valor: String, representa o valor da carta pertence ao conjunto (1,2,3,4,5,6,7,8,9,10,J,Q,K,A)
;nipe: String, representa o nipe da carta, pertence ao conjunto (Ouro, Madeira, Espadas, Copas)

;Nipes
(define Ouro "Ouro")
(define Madeira "Madeira")
(define Espadas "Espadas")
(define Copas "Copas")

;Lista de nipes válidos
(define nipes (list Ouro Madeira Espadas Copas))

;Lista de valores válidos
(define valores (list "2" "3" "4" "5" "6" "7" "8" "9" "10" "J" "Q" "K" "A"))

;Lista de carta
(define lista-cartas-um (list (make-carta "A" Ouro) (make-carta "A" Espadas) (make-carta "K" Ouro) (make-carta "10" Copas) (make-carta "5" Madeira)))

;carta-valida? carta -> booleano
;Recebe uma carta e valida seu valor e nipe
(define (carta-valida? carta)
  (and (nipe-valido? (carta-nipe carta) nipes) (valor-valido? (carta-valor carta) valores)))

;nipe-valido? nipe nipes-validos -> booleano
;Recebe um nipe e uma lista de nipes validos e verifica se ele eh valido
(define (nipe-valido? nipe nipes-validos)
  (cond
    [(empty? nipes-validos) #f]
    [(equal? nipe (first nipes-validos)) #t]
    [else (nipe-valido? nipe (rest nipes-validos))]))

;valor-valido? valor valores-validos -> booleano
;Recebe um valor e uma lista de valores validos e verifica se ele eh valido
(define (valor-valido? valor valores-validos)
  (cond
    [(empty? valores-validos) #f]
    [(equal? valor (first valores-validos)) #t]
    [else (valor-valido? valor (rest valores-validos))]))

;filtrar-por-nipe nipe lista-cartas -> lista-cartas
;Dada uma lista de cartas filtra elas pelo nipe e retorna uma nova lista apenas com o nipe filtrado
(define (filtrar-por-nipe nipe lista-cartas)
  (cond
    [(empty? lista-cartas) empty]
    [(equal? (carta-nipe (first lista-cartas)) nipe) (cons (first lista-cartas) (filtrar-por-nipe nipe (rest lista-cartas)))]
    [else (filtrar-por-nipe nipe (rest lista-cartas))]))

(check-expect (filtrar-por-nipe Ouro lista-cartas-um) (list (make-carta "A" Ouro) (make-carta "K" Ouro)))
(check-expect (filtrar-por-nipe Madeira lista-cartas-um) (list (make-carta "5" Madeira)))
(check-expect (filtrar-por-nipe Espadas lista-cartas-um) (list (make-carta "A" Espadas)))

;valor-esta-na-lista? valor lista-valores -> Booleano
;Recebe um valor e ums lista contendo varios valores e verifica se o valor ja se encontra na lista
(define (valor-esta-na-lista? valor lista-valores)
  (cond
    [(empty? lista-valores) #f]
    [(equal? (first lista-valores) valor) #t]
    [else (valor-esta-na-lista? valor (rest lista-valores))]))

;lista-valores-aux lista-cartas -> lista-valores
;Dada uma lista de cartas retorna a lista com os valores das cartas
(define (lista-valores-aux lista-cartas)
  (cond
    [(empty? lista-cartas) empty]
    [else (cons (carta-valor (first lista-cartas)) (lista-valores-aux (rest lista-cartas)))]))

;remover-repeciacao-valores lista-valores -> lista-valores
;Dada uma lista de valores remove os valores repetidos
(define (remover-repeticao-valores lista-valores)
  (cond
    [(empty? lista-valores) empty]
    [(valor-esta-na-lista? (first lista-valores) (rest lista-valores)) (remover-repeticao-valores (rest lista-valores))]
    [else (cons (first lista-valores) (remover-repeticao-valores (rest lista-valores)))]))

;lista-valores lista-cartas -> lista-valores
;Dada uma lista de cartas retorna a lista dos valores destas cartas sem repeticao
(define (lista-valores lista-cartas)
  (remover-repeticao-valores (lista-valores-aux lista-cartas)))

(check-expect (lista-valores lista-cartas-um) (list "A" "K" "10" "5"))
(check-expect (lista-valores (append lista-cartas-um lista-cartas-um)) (list "A" "K" "10" "5"))

;pegar-valor-carta carta -> valor
;Dada uma carta retorna seu valor
(define (pegar-valor-carta carta)
  (cond
    [(equal? (carta-valor carta) "A") 1]
    [(equal? (carta-valor carta) "2") 2]
    [(equal? (carta-valor carta) "3") 3]
    [(equal? (carta-valor carta) "4") 4]
    [(equal? (carta-valor carta) "5") 5]
    [(equal? (carta-valor carta) "6") 6]
    [(equal? (carta-valor carta) "7") 7]
    [(equal? (carta-valor carta) "1") 1]
    [(equal? (carta-valor carta) "8") 8]
    [(equal? (carta-valor carta) "9") 9]
    [(equal? (carta-valor carta) "10") 10]
    [(equal? (carta-valor carta) "J") 11]
    [(equal? (carta-valor carta) "Q") 12]
    [(equal? (carta-valor carta) "K") 13]
    [else 0]))

;soma-por-nipe lista-cartas -> Numero
;Dada uma lista de cartas soma seus valores
(define (soma-por-nipe lista-cartas)
  (cond
    [(empty? lista-cartas) 0]
    [else (+ (pegar-valor-carta (first lista-cartas)) (soma-por-nipe (rest lista-cartas)))]))

(check-expect (soma-por-nipe lista-cartas-um) 30)
(check-expect (soma-por-nipe (append lista-cartas-um lista-cartas-um)) 60)

