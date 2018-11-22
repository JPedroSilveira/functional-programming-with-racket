;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |Lista 1 - Fundamentos Algoritmos|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define (m x)
  (sqrt (sqr x)))

(define (p c1 c2)
 (sqrt (+ (sqr c1) (sqr c2))))

(define (delt a b c)
 (sqrt (- (sqr b) (* 4 a c))))

(define (k a b)
 (+ a b))

(define (calcVolume base altura)
  (* (/ 1 3) 3.14 (/ base 2) altura))

(define (calcValGastoPercurso valorLitro consumoKmLitro kmPercorrido)
  (+ (* consumoKmLitro kmPercorrido valorLitro) (* kmPercorrido 0.91)))
  