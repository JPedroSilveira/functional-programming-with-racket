;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname Exercicio1Lab5) (read-case-sensitive #t) (teachpacks ((lib "universe.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "universe.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp")) #f)))
;;Variávels de teste para a função "validaExpressao"
(define teste1 "(a+b*(c + b) - z)")
(define teste2 "([][]{[]})()[]")
(define teste3 "(zzzz([dddd)]ssss)")
(define teste4 "([pl]z]z)")
(define teste5 "([][][][]][])")
(define teste6 "())")
(define teste7 "))))")
(define teste8 "(((((")

;;validaExpressao: String -> Boolean
;;Dada uma string contendo uma expresão matemática com os caracteres {"(" ")" "[" "]" "{" "}"}
;;retorna verdadeiro caso esta expressão tenha estes caracteres em ordem válida e falso caso contrário
;;   naLista?: Caractere Lista-de-Caractere -> Boolean
;;   Dado um caractere e uma lista de caracteres, verifica se o caractere está contido na lista,
;;   caso sim retorna verdadeiro, caso não retorna falso
;;   ehFechamentoDaAbertura?: Caractere Caractere -> Boolean
;;   Dados dois caracteres, retorna verdadeiro caso o primeiro seja o fechamendo de parenteses do segundo
;;   e retorna falso caso contrário. Exemplo: (ehFechamentoDaAbertura? "(" ")") -> #true
;;   validador: Lista-de-Caracteres Lista-de-Caracteres -> Booolean
;;   Dada duas listas de caracteres, a primeira contendo a expressão e a segunda inicialmente vazia
;;   a função percorre toda a expressão validando a ordem de seus parenteses
(define (validaExpressao espressao)
  (local
    ((define ex (explode espressao))
     (define lsParenteses (list "(" ")" "[" "]" "{" "}"))
     (define lsFechamento (list ")" "]" "}"))
     (define (naLista? val ls)
       (cond
         [(empty? ls) #false]
         [(equal? val (first ls)) #true]
         [else (naLista? val (rest ls))]))
     (define (ehFechamentoDaAbertura? abertura fechamento)
       (cond
         [(and (equal? "(" abertura)(equal? ")" fechamento)) #true]
         [(and (equal? "[" abertura)(equal? "]" fechamento)) #true]
         [(and (equal? "{" abertura)(equal? "}" fechamento)) #true]
         [else #false]))
     (define (validador le l)
       (cond
         [(empty? le) (empty? l)]
         [(naLista? (first le) lsParenteses)
          (cond
            [(naLista? (first le) lsFechamento)
             (cond
               [(empty? l) #false]
               [(naLista? (first l) lsFechamento) #false]
               [(ehFechamentoDaAbertura? (first l) (first le)) (recursao (rest le) (rest l))]
               [else #false])]
            [else (recursao (rest le) (cons (first le) l))])]
         [else (recursao (rest le) l)])))
    (validador ex empty)))

;;Testes para a função "validaExpressao"
(check-expect (validaExpressao teste1) #true)
(check-expect (validaExpressao teste2) #true)
(check-expect (validaExpressao teste3) #false)
(check-expect (validaExpressao teste4) #false)
(check-expect (validaExpressao teste5) #false)
(check-expect (validaExpressao teste6) #false)
(check-expect (validaExpressao teste7) #false)
(check-expect (validaExpressao teste8) #false)