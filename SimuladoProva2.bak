;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname SimuladoProva2) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;Questão 4 Simulado Prova 2

(define-struct Pessoa (nome idade profissao))

(define teste (make-Pessoa "Joao" 19 "Testador"))

;;gera-csv: Pessoa -> String
;;Recebe uma pessoa do tipo Pessoa e retorna uma string no formato CSV com os elementos separados por "," representando a pessoa
(define (gera-csv p)
  (local
    ((define virgula (list ",")))
    (implode 
     (append
      (explode (Pessoa-nome p))
      virgula
      (explode (number->string (Pessoa-idade p)))
      virgula
      (explode (Pessoa-profissao p))))))

(define (csv-para-pessoa s)
  (local
    ((define ls (explode s))
     (define (pegar-elemento i l)
       (cond
         [(empty? l) empty]
         [(equal? 0 i)
          (cond 
            [(equal? (first l) ",") empty]
            [else (append (list (first l)) (pegar-elemento i (rest l)))])]
         [(equal? (first l) ",") (pegar-elemento (- i 1) (rest l))]
         [else (pegar-elemento i (rest l))])))
  (make-Pessoa (implode (pegar-elemento 0 ls))
               (string->number (implode (pegar-elemento 1 ls)))
               (implode (pegar-elemento 2 ls)))))

(check-expect (csv-para-pessoa (gera-csv teste)) teste)

;;QuickSort

(define (quick-sort ls)
  (local
    ((define (menores v)
       (< v (first ls)))
     (define (maiores v)
       (> v (first ls))))
    (cond
      [(empty? ls) empty]
      [else
       (append
        (quick-sort (filter menores ls))
        (list (first ls))
        (quick-sort (filter maiores ls)))])))

;;Grafos e caminhos

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

;;Encontra todos os caminhos possíveis

(define (encontra-caminho g o d)
  (local
    ((define (esta-contido? l i)
       (cond
         [(empty? l) #false]
         [(equal? (first l) i) #true]
         [else (esta-contido? (rest l) i)]))
     (define (vizinhos g n)
      (cond
        [(empty? g) empty]
        [(equal? (first (first g)) n) (first (rest (first g)))]
        [else (vizinhos (rest g) n)]))
     (define (caminho g o d l)
       (cond
         [(equal? o d) (list d)]
         [else 
          (local
            ((define v (vizinhos g o))
             (define caminho-potencial (busca-caminho g v d l o)))
             (cond
               [(empty? caminho-potencial) empty]
               [else caminho-potencial]))]))
     (define (busca-caminho g v d l o)
       (cond
         [(empty? v) empty]
         [(esta-contido? l (first v)) (busca-caminho g (rest v) d l o)]
         [else
          (local
            ((define caminho-potencial (caminho g (first v) d (cons (first v) l))))
            (cond
              [(empty? caminho-potencial) (busca-caminho g (rest v) d l o)]
              [else (append (list (cons o caminho-potencial)) (busca-caminho g (rest v) d l o))]))])))
    (caminho g o d (list o)))) 

  

     
