;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname ExerciciosEstudoProva2) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;Um elemento data do conjunto Data é uma estrutura
;;(make-data d m a) onde:
;;d: Número, representa o dia de envio
;;m: Número, representa o mês de envio
;;a: Número, representa o ano de envio
(define-struct data (dia mes ano))

;;Um elemento email do conjunto Email é uma estrutura
;;(make-email r d t) onde:
;;r: String, endereço de email do remetende
;;d: Data, data do envio
;;t: String, texto do email
(define-struct email (remetente data texto))

;Dados para teste
(define email_1 (make-email "teste@gmail.com" (make-data 12 02 2010) "Sla"))
(define email_2 (make-email "roberto@outlook.com" (make-data 15 05 2011) "Textão"))
(define email_3 (make-email "cintia@outlook.com" (make-data 15 06 2011) "Email interessante"))
(define email_4 (make-email "cintia@outlook.com" (make-data 14 05 2011) "Email interessante"))
(define email_5 (make-email "cintia@outlook.com" (make-data 14 05 2012) "Email interessante"))

(define lista_emails (list email_1 email_2 email_3 email_4 email_5))

;;valor-data: Data -> Número
;;Dada uma data do conjunto Data, retorna um número que representa a sua grandeza (datas mais recentes são maiores)
(define (valor-data d)
  (+ (* (data-ano d) 372) (* (data-mes d) 31) (data-dia d)))

;;data-maior?: Data Data -> Boolean
;;Dado duas datas retorna #true se a primeira for maior que a segunda, se não #false
;;(define (data-maior? data_1 data_2)
;;  (local
;;    ((define (compara-ano comparador)
;;       (comparador (data-ano data_1) (data-ano data_2)))
;;     (define (compara-mes comparador)
;;       (comparador (data-mes data_1) (data-mes data_2)))
;;     (define (compara-dia comparador)
;;       (comparador (data-dia data_1) (data-dia data_2))))
;;  (cond
;;    [(compara-ano >) #true]
;;    [(compara-ano equal?)
;;     (cond
;;       [(compara-mes >) #true]
;;       [(compara-mes equal?)
;;        (cond
;;          [(compara-dia >) #true]
;;          [else #false])]
;;       [else #false])
;;     ]
;;   [else #false])))

;;data-maior?: Data Data -> Boolean
;;Dado duas datas retorna #true se a primeira for maior que a segunda, se não #false
(define (data-maior? d1 d2)
  (> (valor-data d1) (valor-data d2)))

(check-expect (data-maior? (make-data 15 06 2011) (make-data 14 05 2011)) #true)
(check-expect (data-maior? (make-data 15 06 2011) (make-data 14 05 2012)) #false)
(check-expect (data-maior? (make-data 15 05 2011) (make-data 14 05 2011)) #true)

;;email-mais-recente?: Email Email -> Boolean
;;Dados dois emails, retorna #true se o primeira for mais recente que o segundo, do contrário retorna #false
(define (email-mais-recente? e1 e2)
  (data-maior? (email-data e1) (email-data e2)))

;;ordena-emails: Lista-Email -> Lista-Email
;;Lista-Email é:
;; - empty, ou
;; - (cons e lm) onde:
;;   - e: Email
;;   - lm: Lista-Email
(define (ordena-emails le)
  (quicksort le email-mais-recente?))

;;Um elemento nodo do conjunto Nodo é uma estrutura
;;(make-nodo n d e) onde:
;;n: Numero, valor do nodo atual
;;d: Nodo, nodo a direita
;;e: Nodo, nodo a esquerda
(define-struct nodo(n d e))

;; gera_lista_random: Number --> List-of-Numbers
;;   Objetivo: Dado um número n, retorna uma lista com números aleatórios de 1 a 100 de n elementos
;;   Exemplo: (gera_lista_random 30)
;;   Implementação:
(define (gera_lista_random n)
  (cond
    [(equal? n 0) '()]
    [else (cons (random 100) (gera_lista_random (- n 1)))]
   )
)

;;add-to-abp: Número Nodo-Raiz -> Nodo
;;Dado um número, e um nodo-raiz, adiciona o nodo a árvore binária
(define (add-to-abp n r)
  (cond
    [(empty? r) (make-nodo n empty empty)]
    [(> n (nodo-n r)) (make-nodo (nodo-n r) (add-to-abp n (nodo-d r)) (nodo-e r))]
    [else (make-nodo (nodo-n r) (nodo-d r) (add-to-abp n (nodo-e r)))])) 

;;cria-abp: Lista-Numeros -> Nodo
;;Dado uma lista de números retorna uma árvore binária de pesquisa composta por eles
(define (cria-abp l)
  (cond
    [(empty? l) empty]
    [else (add-to-abp (first l) (cria-abp (rest l)))]))

(define lista_num (gera_lista_random 10))
(define ab (cria-abp lista_num))

;;caminhamento: Nodo -> Lista-Valores
;;Dado um nodo raiz, retorna a lista de valores da árvore binária por caminhamento InOrder
(define (caminhamento nr)
  (append
   (list (nodo-n nr))
   (cond
     [(empty? (nodo-d nr)) empty]
     [else (caminhamento (nodo-d nr))])
   (cond
     [(empty? (nodo-e nr)) empty]
     [else (caminhamento (nodo-e nr))])))
  
