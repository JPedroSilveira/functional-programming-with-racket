;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname SpaceInvaders) (read-case-sensitive #t) (teachpacks ((lib "universe.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "universe.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp")) #f)))
(require 2htdp/universe)
(require 2htdp/image)

(define-struct pixel(image color length))
;Um elemento do conjunto pixel tem o formato
;(make-pixel image color length)
;onde:
;image: image, representa a imagem do pixel
;color: String, representa a cor do pixel
;length: Número, representa o tamanho do lado do pixel

(define-struct pixel-line(image length pixel))
;Um elemento do conjunto pixel-line tem o formato
;(make-pixel-line image length)
;onde:
;image: image, representa a imagem do pixel
;length: Número, representa o comprimento da linha de pixels
;pixel: pixel, representa a unidade de pixel que forma a linha

(define-struct stack-lines(image height pixel-line))
;Um elemento do conjunto stack-lines tem o formato
;(make-stack-lines image height length)
;onde:
;image: image, representa a imagem do pixel
;height: Número, representa a altura da pilha de linhas
;pixel-line: pixel-line, representa a unidade de linha que forma a pilha

(define-struct nave(image stack-lines-list x y))
;Um elemento do conjunto nave tem o formato
;(make-nave image stack-lines-list x y)
;onde:
;image: image, representa a imagem de uma nave
;stack-lines-list: lista de stack-lines, representam as pilhas de pixels que formam a nave
;x: representa a posicao da nave no eixo x
;y: representa a posicao da nave no eixo y

(define-struct nave-shoot(image x y))
;Um elemento do conjunto nave-shoot tem o formato
;(make-nave-shoot image x y)
;onde:
;image: image, representa a imagem de uma nave
;x: numero, representa a posicao do tiro no eixo horizontal
;y: numero, representa a posicao do tiro no eixo vertical
  
(define-struct game-space(image height length color))
;Um elemento do conjunto game-space tem o formato:
;(make-game-space image height lenght)
;onde:
;image: image, representa a imagem do fundo
;height: Número, representa a altura do fundo
;lenght: Número, representa o comprimento do fundo
;color: String, representa a cor do fundo

(define-struct game-state(image game-space human-nave list-human-nave-shoot))
;Um elemento do conjunto game-state tem o formato:
;(make-agem-state image game-space human-nave list-human-nave-shoot)
;onde:
;image: image, representa a imagem atual do jogo
;game-space: game-space, representa o fundo do jogo
;human-nave: nave, representa a nave humana do jogo
;nave-x: Número, representa a posição da nave em um eixo horizontal
;list-human-nave-shoot: lista de nave-shoot, representa os tiros disparados pela nave humana

;create-pixel: String Numero -> pixel
;Retorna um pixel com base em um tamanho e uma cor
(define (create-pixel color length)
  (make-pixel (square length "solid" color) color length))

;create-pixel: Numero pixel -> pixel-line
;Retorna um pixel-line com base em um pixel e uma largura para a linha
(define (create-pixel-line length pixel)
  (make-pixel-line (draw-pixel-line length pixel) length pixel))

;draw-pixel-line: Numero pixel -> image
;Retorna a imagem de uma linha com base em um tamanho e um pixel
(define (draw-pixel-line length pixel)
  (cond
    [(<= length 1) (pixel-image pixel)]
    [else (beside (pixel-image pixel) (draw-pixel-line (- length 1) pixel))]))

;create-stack-lines: Numero pixel-line -> stack-lines
;Retorna um stack-lines com uma imagem de um número de linhas, passado por parametro, empilhadas.
(define (create-stack-lines height pixel-line)
  (make-stack-lines (draw-stack-lines height pixel-line) height pixel-line))

;draw-stack-lines: Numero pixel-line -> image
;Retorna a imagem de um número de linhas, passado por parametro, empilhadas.
(define (draw-stack-lines height pixel-line)
  (cond
    [(<= height 1) (pixel-line-image pixel-line)]
    [else (above  (pixel-line-image pixel-line) (draw-stack-lines (- height 1) pixel-line))]))

;create-human-nave-shoot: nave -> nave-shoot
;Cria um tiro apartir de uma nave humana
(define (create-human-nave-shoot game-space nave)
  (make-nave-shoot
   (pixel-image (pixel-line-pixel (stack-lines-pixel-line (first (nave-stack-lines-list nave)))))
   (+ (nave-x nave) (* (floor (/ (find-longest-stack-lines (nave-stack-lines-list nave) 0) 2)) (find-pixel-height-of-nave nave)));
   (- (find-game-space-floor-for-nave game-space nave) (find-pixel-height-of-nave nave))))

;create-human-nave: pixel -> nave
;Recebe um pixel e retorna uma nave humana com base no mesmo
(define (create-human-nave pixel)
  (draw-nave
   (make-nave
    null
    (list
     (create-stack-lines 1 (create-pixel-line 1 pixel))
     (create-stack-lines 2 (create-pixel-line 3 pixel))
     (create-stack-lines 1 (create-pixel-line 11 pixel))
     (create-stack-lines 4 (create-pixel-line 13 pixel)))
    0
    0)))

;draw-nave: nave -> nave
;Recebe uma nave com uma lista de stack-lines e faz sua imagem
(define (draw-nave nave)
  (make-nave
   (draw-nave-aux (nave-stack-lines-list nave))
   (nave-stack-lines-list nave)
   (nave-x nave)
   (nave-y nave)))

;draw-nave-aux: lista stack-lines -> image
;Recebe uma lista de stack-lines e monta uma imagem empilhando elas
(define (draw-nave-aux stack-lines-list)
  (cond
    [(empty? (rest stack-lines-list)) (stack-lines-image (first stack-lines-list))]
    [else (above (stack-lines-image (first stack-lines-list)) (draw-nave-aux (rest stack-lines-list)))]))

;create-game-space: Número Número String -> game-space
;Cria um game-space com base em uma altura, um comprimento e uma cor passada
(define (create-game-space height lenght color)
  (make-game-space
   (rectangle height lenght "solid" color)
   lenght
   height
   color))

;find-length-stack-line: stack-lines -> Numero
;Encontra o comprimento da linha de pixels que forma a pilha
(define (find-length-stack-lines stack-lines)
  (pixel-line-length (stack-lines-pixel-line stack-lines)))

;find-longest-stack-lines-of-a-nave: lista stack-lines -> Numero
;Encontra a stack-lines com as linhas de pixels de maior comprimento
(define (find-longest-stack-lines stack-lines-list longest)
  (cond
    [(empty? stack-lines-list) longest]
    [(> (find-length-stack-lines (first stack-lines-list)) longest) (find-longest-stack-lines (rest stack-lines-list) (find-length-stack-lines (first stack-lines-list)))]
    [else (find-longest-stack-lines (rest stack-lines-list) longest)]))

;find-length-nave: nave -> Numero
;Retorna o comprimento da parte mais longa de uma nave
(define (find-length-nave nave)
  (* (find-longest-stack-lines (nave-stack-lines-list nave) 0) (find-pixel-height-of-nave nave)))

;find-height-of-stack-lines-list lista stack-lines -> Numero
;Retorna a altura de uma lista de stack-lines
(define (find-height-of-stack-lines-list stack-lines-list)
  (cond
    [(empty? stack-lines-list) 0]
    [else (+ (* (stack-lines-height (first stack-lines-list)) (find-pixel-height-of-stack-lines (first stack-lines-list))) (find-height-of-stack-lines-list (rest stack-lines-list)))]))

;find-height-of-nave nave -> Numero
;Retorna a altura de uma nave
(define (find-height-of-nave nave)
  (find-height-of-stack-lines-list (nave-stack-lines-list nave)))

;find-pixel-height-of-nave: nave -> Numero
;Retorna o tamanho do pixel da primeira linha da primeira pilha que forma uma nave
(define (find-pixel-height-of-nave nave)
  (find-pixel-height-of-stack-lines (first (nave-stack-lines-list nave))))

;find-pixel-height-of-stack-lines: stack-lines -> Numero
;Retorna o tamanho do pixel de uma pilha
(define (find-pixel-height-of-stack-lines stack-lines)
  (pixel-length (pixel-line-pixel (stack-lines-pixel-line stack-lines))))

;insert-human-nave-on-game-space: game-space nave -> image
;Insere uma nave humana na posicao centro-chao de uma game-space
(define (insert-human-nave-on-game-space game-space nave)
  (insert-nave-on-game-space
   game-space
   nave
   (nave-x nave)
   (nave-y nave)))

;insert-nave-on-game-space: game-space nave -> image
;Insere uma nave em uma imagem de fundo com base em valores x e y
 (define (insert-nave-on-game-space game-space nave x y)
  (underlay/xy (game-space-image game-space) x y (nave-image nave)))

;find-game-space-horizontal-center-for-nave: game-space nave -> Numero
;Encontra a posicao central no eixo horizontal de um game-space para uma nave
(define (find-game-space-horizontal-center-for-nave game-space nave)
 (- (/ (game-space-length game-space) 2) (/ (find-length-nave nave) 2)))

;find-game-space-floor-for-nave: game-space nave -> Numero
;Encontra a menor posicao no eixo vertical que uma nave pode ocupar
(define (find-game-space-floor-for-nave game-space nave)
  (- (game-space-height game-space) (find-height-of-nave nave) (* 2 (find-pixel-height-of-nave nave))))

;create-game-state-with-human-nave: game-space nave -> game-state
;Cria um game-state baseado em uma nave principal alocada no centro inferior de um fundo
(define (create-game-state-with-human-nave game-space nave)
  (make-game-state
   (insert-human-nave-on-game-space
    game-space
    (make-nave
       (nave-image nave)
       (nave-stack-lines-list nave)
       (find-game-space-horizontal-center-for-nave game-space nave)
       (find-game-space-floor-for-nave game-space nave)))
   game-space
   (make-nave
       (nave-image nave)
       (nave-stack-lines-list nave)
       (find-game-space-horizontal-center-for-nave game-space nave)
       (find-game-space-floor-for-nave game-space nave))
   empty))

;render: game-state -> image
;Cria uma imagem do jogo baseada no game-state
(define (render game-state)
  (insert-shoot-list-on-game-space (insert-human-nave-on-game-space (game-state-game-space game-state) (game-state-human-nave game-state)) (game-state-list-human-nave-shoot game-state)))

;insert-shoot-list-on-game-space: image nave-shoot(lista) -> image
;Recebe uma imagem de fundo e uma lista de tiros e insere os tiros na imagem
(define (insert-shoot-list-on-game-space image nave-shoot-list)
  (cond
    [(empty? nave-shoot-list) image]
    [(empty? (rest nave-shoot-list)) (underlay/xy image (nave-shoot-x (first nave-shoot-list)) (nave-shoot-y (first nave-shoot-list)) (nave-shoot-image (first nave-shoot-list)))]
    [else (insert-shoot-list-on-game-space (underlay/xy image (nave-shoot-x (first nave-shoot-list)) (nave-shoot-y (first nave-shoot-list)) (nave-shoot-image (first nave-shoot-list))) (rest nave-shoot-list))]))

;move-human-nave: game-state String -> game-state
;Recebe o estado do jogo e realiza uma operação nele baseado na tecla pressionada
(define (move-human-nave game-state key)
  (cond
    [(equal? key "right") (move-right game-state)]
    [(equal? key "left") (move-left game-state)]
    [(equal? key " ") (add-human-nave-shoot game-state)]
    [else game-state]
  ))

;move-right: game-state -> game-state
;Recebe o estado do jogo (game-state) e realiza uma operação nele para movimentar a nave para a direita
(define(move-right game-state)
  (cond
    [(<= (+ (nave-x (game-state-human-nave game-state)) (add-move-nave (game-state-human-nave game-state)) (find-length-nave (game-state-human-nave game-state))) (game-space-length (game-state-game-space game-state)))
     (make-game-state
      (game-state-image game-state)
      (game-state-game-space game-state)
      (make-nave
       (nave-image (game-state-human-nave game-state))
       (nave-stack-lines-list (game-state-human-nave game-state))
       (+ (nave-x (game-state-human-nave game-state)) (add-move-nave (game-state-human-nave game-state)))
       (nave-y (game-state-human-nave game-state)))
      (game-state-list-human-nave-shoot game-state))]
    [else
     (make-game-state
      (game-state-image game-state)
      (game-state-game-space game-state)
      (make-nave
       (nave-image (game-state-human-nave game-state))
       (nave-stack-lines-list (game-state-human-nave game-state))
       (- (game-space-length (game-state-game-space game-state)) (find-length-nave (game-state-human-nave game-state)))
       (nave-y (game-state-human-nave game-state)))
      (game-state-list-human-nave-shoot game-state))]))

;move-left: game-state -> game-state
;Recebe o estado do jogo (game-state) e realiza uma operação nele para movimentar a nave para a esquerda
(define(move-left game-state)
  (cond
    [(>= (- (nave-x (game-state-human-nave game-state)) (add-move-nave (game-state-human-nave game-state))) 0)
     (make-game-state
      (game-state-image game-state)
      (game-state-game-space game-state)
      (make-nave
       (nave-image (game-state-human-nave game-state))
       (nave-stack-lines-list (game-state-human-nave game-state))
       (- (nave-x (game-state-human-nave game-state)) (add-move-nave (game-state-human-nave game-state)))
       (nave-y (game-state-human-nave game-state)))
      (game-state-list-human-nave-shoot game-state))]
    [else
     (make-game-state
      (game-state-image game-state)
      (game-state-game-space game-state)
      (make-nave
       (nave-image (game-state-human-nave game-state))
       (nave-stack-lines-list (game-state-human-nave game-state))
       0
       (nave-y (game-state-human-nave game-state)))
      (game-state-list-human-nave-shoot game-state))]))

;add-move-right: nave -> Numero
;Retorna o valor do deslocamento da nave
(define (add-move-nave nave)
  (find-pixel-height-of-nave nave))

;add-human-nave-shoot: game-state -> game-state
;Adiciona um tiro da nave humana ao game-state
(define (add-human-nave-shoot game-state)
  (make-game-state
      (game-state-image game-state)
      (game-state-game-space game-state)
      (game-state-human-nave game-state)
      (cons (create-human-nave-shoot (game-state-game-space game-state)(game-state-human-nave game-state))(game-state-list-human-nave-shoot game-state))))

;Main: CircleControl -> CircleControl
;Funcao principal para executar a animação
(define (Main game-state)
  (big-bang game-state
    [to-draw render]
    [on-key move-human-nave]
    ))

