;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname LAB1_INF05008A_Joao_Pedro_E_Matheus_Albuquerque) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
;;Laboratório 2

;;Turma A
;;Aluno: João Pedro Silveira e Silva
;;Aluno: Matheus Albuquerque Lopes Mendes

;;#####################################################################################
;;# Exercício 1 #
(define-struct serie-tv(nome emissora-tv numero-episodios duracao-media n-temporadas))
;;Um elemento do conjunto serie-tv tem o formato
;;(make-serie-tv nome emissora numero-episodios duracao-media n-temporadas)
;;onde:
;;nome: String, representa o nome da série
;;emissora-tv: representa a emissora da série
;;numero-episodios: Número, representa a quantidade de episódios total
;;duracao-media: Número, representa o tempo médio por episódio
;;n-temporadas: Número, representa o número de temporadas da série
;;#####################################################################################

;;#####################################################################################
;;# Exercício 2 #
(define-struct emissora-tv(nome cidade-sede streaming?))
;;Um elemento do conjunto emissora-tv tem o formato
;;(make-emissora-tv nome cidade-sede streaming?)
;;onde:
;;nome: String, representa o nome da emissora
;;cidade-sede: String, representa o nome da cidade onde fica a sede da emissora
;;streaming?: Boolean, indica se a emissora possui serviço de streaming
;;#####################################################################################

;;#####################################################################################
;;Constantes para testes
(define hbo (make-emissora-tv "HBO" "Lugar Nenhum" #t))
(define netflix (make-emissora-tv "NETFLIX" "Lugar Nenhum" #t))
(define disney (make-emissora-tv "DISNEY" "Disneylandia" #f))

(define game-of-thrones (make-serie-tv "Game Of Thrones" hbo 67 60 7))
(define drHousacket (make-serie-tv "Dr. Housacket" netflix 50 45 5))
(define drMickey (make-serie-tv "Dr. Mickey" disney 10 20 2))
;;#####################################################################################

;;#####################################################################################
;;# Exercício 3 #

;;Letra A
;;duracao-total: serie-tv -> Número
;;Recebe uma série de televisão e calcula o tempo total da série somando o tempo de cada episódio
(define (duracao-total serie-tv) (* (serie-tv-numero-episodios serie-tv) (serie-tv-duracao-media serie-tv)))
(check-expect (duracao-total game-of-thrones) 4020)
(check-expect (duracao-total drHousacket) 2250)

;;Letra B
;;novo-episodio: Número serie-tv -> serie-tv
;;Adiciona um novo episódio na série criando uma nova temporada e atualizando os dados necessários
(define (novo-episodio duracao-episodio serie-tv)
  (make-serie-tv (serie-tv-nome serie-tv)
                 (serie-tv-emissora-tv serie-tv)
                 (+ (serie-tv-numero-episodios serie-tv) 1)
                 (/ (+ (duracao-total serie-tv) duracao-episodio) (+ (serie-tv-numero-episodios serie-tv) 1))
                 (+ (serie-tv-n-temporadas serie-tv) 1)))

(check-expect (novo-episodio 60 game-of-thrones) (make-serie-tv "Game Of Thrones" hbo 68 60 8))
(check-expect (novo-episodio 45 drHousacket) (make-serie-tv "Dr. Housacket" netflix 51 45 6))

;;Letra C
;;podePassarNetflix?: serie-tv -> Boolean
;;Verifica se a série em questão pode ser exibida na Netflix
(define (podePassarNetflix? serie-tv)
  (or (equal? (emissora-tv-nome (serie-tv-emissora-tv serie-tv)) "NETFLIX") (not (emissora-tv-streaming? (serie-tv-emissora-tv serie-tv)))))

(check-expect (podePassarNetflix? game-of-thrones) #f)
(check-expect (podePassarNetflix? drHousacket) #t)
(check-expect (podePassarNetflix? drMickey) #t)
;;#####################################################################################

;;#####################################################################################
;;# Exercício 4 #
(define-struct filme(nome genero duracao protagonista coadjuvante emissora-tv))
;;Um elemento do conjunto filme tem o formato
;;(make-filme nome genero duracao principal coadjuvante emissora-tv)
;;onde:
;;nome: String, representa o nome do filme
;;genero: String, representa o gênero do filme
;;duracao: Número, representa o tempo do filme em minutos
;;protagonista: String, representa o nome do ator/atriz principal
;;coadjuvante: String, representa o nome do ator/atriz coadjuvante
;;emissora-tv: representa a emissora do filme
;;#####################################################################################

;;#####################################################################################
;;Constantes para teste
(define titanic (make-filme "Titanic" "Drama" 180 "Leonardo" "Afogado" netflix))
(define procurando-nemo (make-filme "Procurando Nemo" "Suspense" 120 "Nemo" "Dori" disney))
;;#####################################################################################

;;#####################################################################################
;;# Exercício 5 #
;;duracao-total: serie-tv ou filme -> Número
;;Recebe uma série ou um filme e devolve a duração total dos mesmos
(define (duracao-total-mista show-tv)
  (cond
    [(serie-tv? show-tv) (* (serie-tv-numero-episodios show-tv) (serie-tv-duracao-media show-tv))]
    [else (filme-duracao show-tv)]))

(check-expect (duracao-total-mista game-of-thrones) 4020)
(check-expect (duracao-total-mista drHousacket) 2250)
(check-expect (duracao-total-mista titanic) 180)
(check-expect (duracao-total-mista procurando-nemo) 120)

;;podePassarNetflix?: serie-tv ou filme -> Boolean
;;Recebe uma série ou um filme e retorna a possibilidade do mesmo ser exibido na Netflix
(define (podePassarNetflixMista? show-tv)
  (cond
    [(serie-tv? show-tv)(or (equal? (emissora-tv-nome (serie-tv-emissora-tv show-tv)) "NETFLIX") (not (emissora-tv-streaming? (serie-tv-emissora-tv show-tv))))]
    [else (or (equal? (emissora-tv-nome (filme-emissora-tv show-tv)) "NETFLIX") (not (emissora-tv-streaming? (filme-emissora-tv show-tv))))]))

(check-expect (podePassarNetflixMista? game-of-thrones) #f)
(check-expect (podePassarNetflixMista? drHousacket) #t)
(check-expect (podePassarNetflixMista? drMickey) #t)
(check-expect (podePassarNetflixMista? titanic) #t)
(check-expect (podePassarNetflixMista? procurando-nemo) #t)
;;#####################################################################################

;;#####################################################################################
;;# Exercício 6 #
;;<-serie-filme: serie-ou-filme serie-ou-filme -> Boolean
;;Um elemento do conjunto serie-ou-filme é:
;; i)Uma serie-tv
;; ii)Um filme
;;Retorna verdadeiro se a duração da primeira série ou filme for menor que a duração da segunda série ou filme
(define (<-serie-filme show-tv-1 show-tv-2) (< (duracao-total-mista show-tv-1) (duracao-total-mista show-tv-2)))

(check-expect (<-serie-filme game-of-thrones drHousacket) #f)
(check-expect (<-serie-filme drHousacket game-of-thrones) #t)
(check-expect (<-serie-filme titanic procurando-nemo) #f)
(check-expect (<-serie-filme procurando-nemo titanic) #t)
;;#####################################################################################

;;#####################################################################################
;;# Exercício 7 #

;;VenderSerie: emissora serie -> serie
;;Retorna a serie com a emissora atualizada para a repassada na função.
(define (VenderSerie emissora serie)
  (make-serie-tv (serie-tv-nome serie)
                 emissora
                 (serie-tv-numero-episodios serie)
                 (serie-tv-duracao-media serie)
                 (serie-tv-n-temporadas serie)))
(check-expect (serie-tv-emissora-tv (VenderSerie netflix game-of-thrones)) netflix)
(check-expect (serie-tv-emissora-tv (VenderSerie hbo drHousacket)) hbo)

;;VenderFilme: emissora filme -> filme
;;Retorna o filme com a emissora atualizada para a repassada na função.
(define (VenderFilme emissora filme)
  (make-filme (filme-nome filme)
              (filme-genero filme)
              (filme-duracao filme)
              (filme-protagonista filme)
              (filme-coadjuvante filme)
              emissora))

(check-expect (filme-emissora-tv (VenderFilme hbo titanic)) hbo)
(check-expect (filme-emissora-tv (VenderFilme netflix procurando-nemo)) netflix)

;;VenderSerieOuFilme emissora serie-ou-filme -> serie-ou-filme
;;Um elemento do conjunto serie-ou-filme é:
;; i)Uma serie-tv
;; ii)Um filme
;;Retorna a serie-ou-filme com a emissora atualizada para a repassada na função.
(define (VenderSerieOuFilme emissora serie-ou-filme)
  (cond
    [(serie-tv? serie-ou-filme) (VenderSerie emissora serie-ou-filme)]
    [(filme? serie-ou-filme) (VenderFilme emissora serie-ou-filme)]
    [else serie-ou-filme]))

(check-expect (VenderSerieOuFilme netflix game-of-thrones) (VenderSerie netflix game-of-thrones))
(check-expect (VenderSerieOuFilme hbo titanic) (VenderFilme hbo titanic))
(check-expect (VenderSerieOuFilme hbo drHousacket) (VenderSerie hbo drHousacket))

;;#####################################################################################

;;#####################################################################################
;;# Exercício 8 #
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
(define SizeFactor 1.2)


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
    [else circleControl]
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
  (cond
    [(and
      (<= (+ (CircleControl-x circleControl) (Diameter (* (CircleControl-z circleControl) SizeFactor))) (CircleControl-l circleControl))
      (<= (+ (CircleControl-y circleControl) (Diameter (* (CircleControl-z circleControl) SizeFactor))) (CircleControl-h circleControl)))
      (make-CircleControl
       (CircleControl-x circleControl)
       (CircleControl-y circleControl)
       (* (CircleControl-z circleControl) SizeFactor)
       (CircleControl-d circleControl)
       (CircleControl-l circleControl)
       (CircleControl-h circleControl))]
    [else circleControl]))

;DecreaseSize: CircleControl -> CircleControl
;Recebe um estado da animação e realiza uma operação nele para diminuir o tamanho do círculo
(define(DecreaseSize circleControl)
  (cond
    [(>= (* (* (/ (CircleControl-z circleControl) SizeFactor) (/ (CircleControl-z circleControl) SizeFactor)) pi) (* (* (CircleControl-l circleControl) (CircleControl-h circleControl)) 0.001))
     (make-CircleControl
      (CircleControl-x circleControl)
      (CircleControl-y circleControl)
      (/ (CircleControl-z circleControl) SizeFactor)
      (CircleControl-d circleControl)
      (CircleControl-l circleControl)
      (CircleControl-h circleControl))]
    [else circleControl]))

;Diameter: Numero -> Numero
;Recebe o raio e calcula o diâmetro de um círculo
(define (Diameter radius) (* radius 2))

;Constante de CircleControl genérica para testes
(define CircleControlBase (make-CircleControl 0 500 50 5 1500 1000))

;Main: CircleControl -> CircleControl
;Funcao principal para executar a animação
(define (Main circleControl)
  (big-bang circleControl
    [to-draw Render]
    [on-key MoveCircle]
    ))

;Função para inicializar animação com parametros fixos de tamanho do círculo (passar qualquer valor para x)
(define (Start x) (Main CircleControlBase))
;;#####################################################################################