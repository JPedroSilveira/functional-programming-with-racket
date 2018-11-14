;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname TesteCircle) (read-case-sensitive #t) (teachpacks ((lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "universe.rkt" "teachpack" "2htdp")) #f)))
(require 2htdp/universe)
(require 2htdp/image)

;Background Numero -> image
;Retorna um fundo baseado em um tamanho de x passado
(define (Background l h) (empty-scene l h))

;Definição de constante para o fator do tamanho do círculo interno a outro circulo na função circleOverCircle
(define InternalFactor 0.8)

;Definição de constante para o fator do aumento e diminuição na velocidade de movimento da animação
(define SpeedFactor 1.5)

;Definição de constante para o fator do aumento e diminuição no tamanho do círculo durante a animação
(define SizeFactor 1.5)


(define-struct NewCircle (image internalRadius))
;Um elemento do conjunto NewCircle tem o formato
;(make-NewCircle image internalRadius)
;onde:
;image: image, representa a imagem de um circulo
;internalRadius: representa o raio do menor circulo contido na imagem

;CommonCircle Numero String -> image
;Retorna um circulo com uma determinada cor e um determinado tamanho passados como parametros
(define (CommonCircle x color)
  (make-NewCircle (circle x "solid" color) x))

;CircleOverCircle Numero -> image
;Retorna um circulo com outro circulo de tamanho proporcional ao da constante internalRadius com a cor passada de parametro.
(define (CircleOverCircle x newCircleColor)
  (cond
    [(NewCircle? x) (make-NewCircle
                     (overlay
                      (NewCircle-image (CommonCircle (* (NewCircle-internalRadius x) InternalFactor) newCircleColor ))
                      (NewCircle-image x))
                     (* (NewCircle-internalRadius x) InternalFactor))]
    [else (make-NewCircle (overlay (NewCircle-image (CommonCircle (* x InternalFactor) newCircleColor )) (NewCircle-image (CommonCircle x "black"))) (* x InternalFactor))]))

;CircleOverCircle Numero -> image
;Retorna um circulo colorido baseado em um tamanho x passado
(define (ColoredCircle x) (CircleOverCircle (CircleOverCircle (CircleOverCircle (CircleOverCircle x "red") "gray") "green") "blue"))

(define-struct CircleControl (x y z d l h))
;Um elemento do conjunto CircleControl tem o formato
;(make-CircleControl x y z d l h)
;onde:
;x: Numero -> representa a posição no eixo x (horizontal)
;y: Numero -> representa a posição no eixo y (vertical)
;z: Numero -> representa o raio do círculo
;d: Numero -> representa o fator multiplicador de deslocamento
;l: Numero -> representa a largura do fundo
;h: Numero -> representa a altura do fundo

;Render: Numero -> image
;Cria um ambiete com um círculo colorido baseado no CircleControl passado
(define (Render circleControl)
  (underlay/xy (Background (CircleControl-l circleControl) (CircleControl-h circleControl)) (CircleControl-x circleControl) (CircleControl-y circleControl) (NewCircle-image (ColoredCircle (CircleControl-z circleControl)))))

;Constante de CircleControl genérica para testes
(define CircleControlBase (make-CircleControl 0 500 50 5 1500 1000))

;MoveCircle: CircleControl String -> CircleControl
;Recebe um estado da animação e realiza uma operação nele baseado na tecla pressionada
(define (MoveCircle circleControl key)
  (cond
    [(equal? key "right") (MoveRight circleControl)]
    [(equal? key "left") (MoveLeft circleControl)]
    [(equal? key "up") (MoveUp circleControl)];
    [(equal? key "down") (MoveDown circleControl)]
    [(equal? key "a") (IncreaseSpeed circleControl)]
    [(equal? key "d") (DecreaseSpeed circleControl)]
    [(equal? key "add") (IncreaseSize circleControl)]
    [(equal? key "subtract") (DecreaseSize circleControl)]
    [else key]
  ))

;MoveRight: CircleControl -> CircleControl
;Recebe um estado da animação e realiza uma operação nele para movimentar o circulo para direita
(define(MoveRight circleControl)
  (cond
    [(<= (+ (CircleControl-x circleControl) (CircleControl-d circleControl)) (- (CircleControl-l circleControl) (Diameter (CircleControl-z circleControl))))
     (make-CircleControl
      (+ (CircleControl-x circleControl) (CircleControl-d circleControl))
      (CircleControl-y circleControl)
      (CircleControl-z circleControl)
      (CircleControl-d circleControl)
      (CircleControl-l circleControl)
      (CircleControl-h circleControl))]
    [else (make-CircleControl
      (- (CircleControl-l circleControl) (Diameter (CircleControl-z circleControl)))
      (CircleControl-y circleControl)
      (CircleControl-z circleControl)
      (CircleControl-d circleControl)
      (CircleControl-l circleControl)
      (CircleControl-h circleControl))]))

;MoveLeft: CircleControl -> CircleControl
;Recebe um estado da animação e realiza uma operação nele para movimentar o circulo para esquerda
(define(MoveLeft circleControl)
  (cond
    [(>= (- (CircleControl-x circleControl) (CircleControl-d circleControl)) 0)
     (make-CircleControl
      (- (CircleControl-x circleControl) (CircleControl-d circleControl))
      (CircleControl-y circleControl)
      (CircleControl-z circleControl)
      (CircleControl-d circleControl)
      (CircleControl-l circleControl)
      (CircleControl-h circleControl))]
    [else (make-CircleControl
      0
      (CircleControl-y circleControl)
      (CircleControl-z circleControl)
      (CircleControl-d circleControl)
      (CircleControl-l circleControl)
      (CircleControl-h circleControl))]))

;MoveUp: CircleControl -> CircleControl
;Recebe um estado da animação e realiza uma operação nele para movimentar o circulo para cima
(define(MoveUp circleControl)
  (cond
    [(>= (- (CircleControl-y circleControl) (CircleControl-d circleControl)) 0)
     (make-CircleControl
      (CircleControl-x circleControl)
      (- (CircleControl-y circleControl) (CircleControl-d circleControl))
      (CircleControl-z circleControl)
      (CircleControl-d circleControl)
      (CircleControl-l circleControl)
      (CircleControl-h circleControl))]
    [else (make-CircleControl
      (CircleControl-x circleControl)
      0
      (CircleControl-z circleControl)
      (CircleControl-d circleControl)
      (CircleControl-l circleControl)
      (CircleControl-h circleControl))]))

;MoveDown: CircleControl -> CircleControl
;Recebe um estado da animação e realiza uma operação nele para movimentar o circulo para baixo
(define(MoveDown circleControl)
  (cond
    [(<= (+ (CircleControl-y circleControl) (CircleControl-d circleControl)) (- (CircleControl-h circleControl) (Diameter (CircleControl-z circleControl))))
     (make-CircleControl
      (CircleControl-x circleControl)
      (+ (CircleControl-y circleControl) (CircleControl-d circleControl))
      (CircleControl-z circleControl)
      (CircleControl-d circleControl)
      (CircleControl-l circleControl)
      (CircleControl-h circleControl))]
    [else (make-CircleControl
      (CircleControl-x circleControl)
      (- (CircleControl-h circleControl) (Diameter (CircleControl-z circleControl)))
      (CircleControl-z circleControl)
      (CircleControl-d circleControl)
      (CircleControl-l circleControl)
      (CircleControl-h circleControl))]))

;IncreaseSpeed: CircleControl -> CircleControl
;Recebe um estado da animação e realiza uma operação nele para aumentar a velocidade de movimento
(define(IncreaseSpeed circleControl)
     (make-CircleControl
      (CircleControl-x circleControl)
      (CircleControl-y circleControl)
      (CircleControl-z circleControl)
      (* (CircleControl-d circleControl) SpeedFactor)
      (CircleControl-l circleControl)
      (CircleControl-h circleControl)))

;DecreaseSpeed: CircleControl -> CircleControl
;Recebe um estado da animação e realiza uma operação nele para diminuir a velocidade de movimento
(define(DecreaseSpeed circleControl)
     (make-CircleControl
      (CircleControl-x circleControl)
      (CircleControl-y circleControl)
      (CircleControl-z circleControl)
      (/ (CircleControl-d circleControl) SpeedFactor)
      (CircleControl-l circleControl)
      (CircleControl-h circleControl)))

;IncreaseSize: CircleControl -> CircleControl
;Recebe um estado da animação e realiza uma operação nele para aumentar o tamanho do círculo
(define(IncreaseSize circleControl)
     (make-CircleControl
      (CircleControl-x circleControl)
      (CircleControl-y circleControl)
      (* (CircleControl-z circleControl) SizeFactor)
      (CircleControl-d circleControl)
      (CircleControl-l circleControl)
      (CircleControl-h circleControl)))

;DecreaseSize: CircleControl -> CircleControl
;Recebe um estado da animação e realiza uma operação nele para diminuir o tamanho do círculo
(define(DecreaseSize circleControl)
     (make-CircleControl
      (CircleControl-x circleControl)
      (CircleControl-y circleControl)
      (/ (CircleControl-z circleControl) SizeFactor)
      (CircleControl-d circleControl)
      (CircleControl-l circleControl)
      (CircleControl-h circleControl)))

;Diameter: Numero -> Numero
;Recebe o raio e calcula o diâmetro de um círculo
(define (Diameter radius) (* radius 2))

;Main: CircleControl -> CircleControl
;Funcao principal para executar a animação
(define (Main circleControl)
  (big-bang circleControl
    [to-draw Render]
    [on-key MoveCircle]
    ))

;Constante para inicializar animação com parametros de teste
(define (Start x) (Main CircleControlBase))