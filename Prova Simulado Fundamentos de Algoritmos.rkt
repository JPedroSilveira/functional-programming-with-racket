;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname |Prova Simulado Fundamentos de Algoritmos|) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp")) #f)))
;Prova Simulada
;Aluno: João Pedro Silveira e Silva

;Letra A

(define-struct candidato (numero nome))
;Um elemento do conjunto candidato tem o formato:
;(make-candidato numero nome)
;onde:
;nome: String, representa o nome do candidato
;numero: Numero, representa o número do candidato para voto

(define-struct voto (numero horario))
;Um elemento do conjunto voto tem o formato:
;(make-voto numero horario)
;onde:
;numero: Numero, representa o número do candidato ao qual o voto representa
;horario: String, represent ao horário em que o voto foi feito

(define-struct urna (lista-votos))
;Um elemento do conjunto urna tem o formato:
;(make-urna lista-votos)
;onde:
;lista-votos: Lista de votos, lista com os votos que foram computador pela urna

;Constantes
(define vota-em-mim (make-candidato 666 "Vota em mim"))
(define nao-vota-em-mim (make-candidato 777 "Não vota em mim"))
(define vou-vencer (make-candidato 111 "Vencedor"))

(define voto-um (make-voto 666 "11:11:11"))
(define voto-dois (make-voto 777 "12:12:12"))
(define voto-tres (make-voto 789 "14:14:14"))
(define voto-quatro (make-voto 111 "15:15:15"))

(define urna-um (make-urna (list voto-um voto-dois voto-tres voto-quatro)))
(define urna-dois (make-urna (list voto-um voto-tres voto-quatro)))
(define urna-tres (make-urna (list voto-tres voto-quatro)))
(define urna-quatro (make-urna (list voto-tres voto-quatro)))

;Letra B

;calcula-voto-candidato canditato urna -> Número
;Dado um candidato e uma urna calcula quantos votos o candidato ganhou
(define (calcula-voto-candidato candidato urna)
  (cond
    [(empty? (urna-lista-votos urna)) 0]
    [(equal? (voto-numero (first (urna-lista-votos urna))) (candidato-numero candidato)) (+ 1 (calcula-voto-candidato candidato (make-urna (rest (urna-lista-votos urna)))))]
    [else (calcula-voto-candidato candidato (make-urna (rest (urna-lista-votos urna))))]))

(check-expect (calcula-voto-candidato vou-vencer urna-um) 1)
(check-expect (calcula-voto-candidato nao-vota-em-mim urna-um) 1)
(check-expect (calcula-voto-candidato vota-em-mim urna-um) 1)
(check-expect (calcula-voto-candidato vota-em-mim urna-tres) 0)

;Letra C

;numero-votos-validos lista-candidato lista-urna -> Número
;Dada uma lista de urnas e uma lista de candidados, calcula o número de votos válidos
(define (numero-votos-validos lista-candidato lista-urna)
  (cond
    [(empty? lista-urna) 0]
    [else (+ (calcula-votos-validos-urna lista-candidato (first lista-urna)) (numero-votos-validos lista-candidato (rest lista-urna)))]))

;calcula-votos-validos-urna lista-candidato urna -> Número
;Dada uma urna e uma lista de candidados, calcula o número de votos válidos da urna
(define (calcula-votos-validos-urna lista-candidato urna)
  (cond
    [(empty? (urna-lista-votos urna)) 0]
    [(valida-voto lista-candidato (first (urna-lista-votos urna))) (+ 1 (calcula-votos-validos-urna lista-candidato (make-urna (rest (urna-lista-votos urna)))))]
    [else (calcula-votos-validos-urna lista-candidato (make-urna (rest (urna-lista-votos urna))))]))

;valida-voto lista-cadidato voto -> Booleano
;Verifica se o voto possui algum candidato válido
(define (valida-voto lista-candidato voto)
  (cond
    [(empty? lista-candidato) #f]
    [(equal? (candidato-numero (first lista-candidato)) (voto-numero voto)) #t]
    [else (valida-voto (rest lista-candidato) voto)]))

;calcula-votos-total-candidato candidato lista-urna -> Número
;Dada uma lista de urnas e um candidado, calcula o número total de votos do candidato
(define (calcula-votos-total-candidato candidato lista-urna)
  (cond
    [(empty? lista-urna) 0]
    [else (+ (calcula-voto-candidato candidato (first lista-urna)) (calcula-votos-total-candidato candidato (rest lista-urna)))]))

;qtd-votos-mais-votado lista-candidato lista-urna -> Número
;Dado uma lista de candidatos e uma lita de urnas realiza a contagem de votos e retorna o número de votos do candidato mais votado
(define (qtd-votos-mais-votado lista-candidato lista-urna)
  (cond
    [(empty? lista-candidato) 0]
    [(empty? (rest lista-candidato)) (calcula-votos-total-candidato (first lista-candidato) lista-urna)]
    [(> (calcula-votos-total-candidato (first lista-candidato) lista-urna) (qtd-votos-mais-votado (rest lista-candidato) lista-urna)) (calcula-votos-total-candidato (first lista-candidato) lista-urna)]
    [else (qtd-votos-mais-votado (rest lista-candidato) lista-urna)]))

(check-expect (qtd-votos-mais-votado (list vota-em-mim nao-vota-em-mim vou-vencer)(list urna-um urna-dois urna-tres urna-quatro)) 4)
(check-expect (qtd-votos-mais-votado (list vota-em-mim nao-vota-em-mim vou-vencer)(list urna-um urna-dois urna-tres)) 3)
(check-expect (qtd-votos-mais-votado (list vota-em-mim nao-vota-em-mim vou-vencer)(list urna-um urna-dois)) 2)
(check-expect (qtd-votos-mais-votado (list vota-em-mim nao-vota-em-mim vou-vencer)(list urna-um)) 1)
(check-expect (qtd-votos-mais-votado (list vota-em-mim nao-vota-em-mim vou-vencer) empty) 0)

;tera-segundo-turno? lista-candidato lista-urna -> Booleano
(define (tera-segundo-turno? lista-candidato lista-urna)
  (<= (qtd-votos-mais-votado lista-candidato lista-urna) (* 0.5 (numero-votos-validos lista-candidato lista-urna))))

(check-expect (tera-segundo-turno? (list vota-em-mim nao-vota-em-mim vou-vencer)(list urna-um urna-dois)) #t)
(check-expect (tera-segundo-turno? (list vota-em-mim nao-vota-em-mim vou-vencer)(list urna-um urna-dois urna-tres)) #t)
(check-expect (tera-segundo-turno? (list vota-em-mim nao-vota-em-mim vou-vencer)(list urna-um urna-dois urna-tres urna-quatro)) #f)
(check-expect (tera-segundo-turno? (list vota-em-mim nao-vota-em-mim vou-vencer) empty) #t)
(check-expect (tera-segundo-turno? empty (list urna-um urna-dois urna-tres urna-quatro)) #t)
