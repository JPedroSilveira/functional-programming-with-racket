;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname Prova2) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define (media-csv s)
  (local
    ((define ls (explode s))
     (define (soma-numeros l ln)
       (cond
         [(empty? l) (string->number (implode ln))]
         [(equal? (first l) ",")
          (cond
            [(empty? ln) (soma-numeros (rest l) empty)]
            [else (+ (string->number (implode ln))
                     (soma-numeros (rest l) empty))])]
         [else (soma-numeros (rest l) (append ln (list (first l))))]))
     (define (cont-v l) 
       (cond 
         [(empty? l) 1]
         [(equal? (first l) ",") (+ 1 (cont-v (rest l)))]
         [else (cont-v (rest l))])))
    (/ (soma-numeros ls empty) (cont-v ls))))

(define (media-csv2 nums)
  (local (
          (define lista (string-to-list (explode nums) '()))
          (define lista-int (str-to-int lista))
          (define soma (soma-lista lista-int))
          (define quantidade (conta-lista lista-int)))
    (/ soma quantidade)))

(define (string-to-list l ant)
  (cond
    [(and (empty? l) (empty? ant)) '()]
    [(empty? l) (cons (implode ant) '())]
    [(equal? "," (first l)) (cons (implode ant) (string-to-list (rest l) '()))]
    [else (string-to-list (rest l) (append ant (list (first l))))]))  

(define (conta-lista l)
  (cond
    [(empty? l) 0]
    [else (+ 1 (conta-lista (rest l)))]))

(define (soma-lista l)
  (cond
    [(empty? l) 0]
    [else (+ (first l) (soma-lista (rest l)))]))

(define (str-to-int l)
  (cond
    [(empty? l) '()]
    [else (cons (string->number (first l)) (str-to-int (rest l)))]))