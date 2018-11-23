;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname |Exercicio2Lab5 (1)|) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp")) #f)))
;;Grafo para testar as funções, cada lista dentro da lista do grafo-teste representa um vértice com seu valor e uma lista de vizinhos
(define grafo-teste
  (list
   (list "A" (list "C" "B"))
   (list "B" (list "A" "B" "C"))
   (list "C" (list "A" "D" "B"))
   (list "D" (list "C" "B" "H" "E"))
   (list "E" (list "D" "G" "F"))
   (list "F" empty)
   (list "G" empty)
   (list "H" empty)
   (list "Z" empty)
  )
)
 
;;vizinhos: Grafo Nodo -> Vizinhos
;;Onde:
;; Grafo:  Lista com N nodos, cada nodo é uma lista com um valor e uma lista de vizinhos
;; Nodo: Valor de um novo
;; Vizinhos: Lista com os vizinhos do nodo escolhido
(define (vizinhos g n)
  (cond
    [(empty? g) empty]
    [(equal? (first (first g)) n) (first(rest (first g)))]
    [else (vizinhos (rest g) n)]))

;;encontra-caminho: Grafo Nodo-Inicial Nodo-Alvo -> Caminho
;;Onde:
;; Grafo: Lista com N nodos, cada nodo é uma lista com um valor e uma lista de vizinhos
;; Nodo-Inicial: valor do nodo onde o caminho iniciará
;; Nodo-Alvo: valor final do caminho
;; Caminho: Lista com os valores dos Nodo que devem ser percorridos de forma ordenada para chegar do Nodo-Inicial ao Nodo-Alvo
(define (encontra-caminho g o d)
  (caminho g o d (list o)))

;;caminho: Grafo Nodo-Inicial Nodo-Alvo Lista-Nodos-Percorridos -> Caminho
;;Onde:
;; Grafo: Lista com N nodos, cada nodo é uma lista com um valor e uma lista de vizinhos
;; Nodo-Inicial: valor do nodo onde o caminho iniciará
;; Nodo-Alvo: valor final do caminho
;; Lista-Nodos-Percorridos: Lista contendo os valores de Nodos já percorridos pelo caminho
;; Caminho: Lista com os valores dos Nodo que devem ser percorridos de forma ordenada para chegar do Nodo-Inicial ao Nodo-Alvo
;;Recebe um Grafo, um Nodo-Inicial, um Nodo-Final e uma lista de nodos percorridos e retorna um caminho entro o Nodo-Inicial e o Nodo-Final dentro do Grafo
(define (caminho g o d l)
  (cond
    [(equal? o d) (list d)]
    [else
     (local
       ((define v (vizinhos g o))
        (define caminho-potencial (busca-caminhos g v d l)))
       (cond
         [(boolean? caminho-potencial) #false]
         [else (cons o caminho-potencial)]))]))

;;busca-caminhos: Grafo Nodo-Inicial Nodo-Alvo Lista-Nodos-Percorridos -> Caminho
;;Onde:
;; Grafo: Lista com N nodos, cada nodo é uma lista com um valor e uma lista de vizinhos
;; Nodo-Inicial: valor do nodo onde o caminho iniciará
;; Nodo-Alvo: valor final do caminho
;; Lista-Nodos-Percorridos: Lista contendo os valores de Nodos já percorridos pelo caminho
;; Caminho: Lista com os valores dos Nodo que devem ser percorridos de forma ordenada para chegar do Nodo-Inicial ao Nodo-Alvo
;;Recebe um Grafo, um Nodo-Inicial, um Nodo-Final e uma lista de nodos percorridos e retorna um caminho entro o Nodo-Inicial e o Nodo-Final dentro do Grafo
(define (busca-caminhos g v d l)
  (cond 
    [(empty? v) #false]
    [(esta-contido? l (first v)) (busca-caminhos g (rest v) d l)]
    [else (local 
            ((define caminho-potencial (caminho g (first v) d (cons (first v) l))))
            (cond
              [(boolean? caminho-potencial) (busca-caminhos g (rest v) d l)]
              [else caminho-potencial]))]))

;;esta-contido: Lista Item -> Boolean
;;Dada uma lista retorna se o item valor está contido nela
(define (esta-contido? l i)
  (cond
    [(empty? l) #false] 
    [(equal? (first l) i) #true]
    [else (esta-contido? (rest l) i)]
  ))

;;Testes para a função "encontra-caminho"
(check-expect (encontra-caminho grafo-teste "A" "B") (list "A" "C" "D" "B"))
(check-expect (encontra-caminho grafo-teste "A" "E") (list "A" "C" "D" "E"))
(check-expect (encontra-caminho grafo-teste "B" "E") (list "B" "A" "C" "D" "E"))
(check-expect (encontra-caminho grafo-teste "E" "B") (list "E" "D" "C" "A" "B"))
(check-expect (encontra-caminho grafo-teste "E" "H") (list "E" "D" "H"))
(check-expect (encontra-caminho grafo-teste "C" "H") (list "C" "D" "H"))
(check-expect (encontra-caminho grafo-teste "C" "Z") #false)
(check-expect (encontra-caminho grafo-teste "Z" "H") #false)
(check-expect (encontra-caminho grafo-teste "H" "A") #false)
(check-expect (encontra-caminho grafo-teste "E" "A") (list "E" "D" "C" "A"))