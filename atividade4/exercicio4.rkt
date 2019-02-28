#lang racket

;; Neste exercício vamos usar testes,
;; o objetivo é terminar com todos os testes passando
(require rackunit rackunit/text-ui)

;; --- Exercício 1 ---------------------

;; Crie uma função recursiva soma-lista (abaixo) que, dada uma lista de números,
;; calcula a soma dos números contidos
(define (soma-lista l acc)
		(if (empty? l) 
			acc
			(soma-lista (rest l) (+ (first l) acc))	
		)
)

(define-test-suite testes-soma-lista
  (test-equal? "soma da lista vazia"                (soma-lista '() 0)                  0)
  (test-equal? "soma de um número apenas"           (soma-lista '(13) 0)                13)
  (test-equal? "soma de vários números"             (soma-lista (list 5 4 3 2 1) 0)     15)
  (test-equal? "soma de números em ordem diferente" (soma-lista (list 1 2 3 4 5) 0)     15)
	(test-equal? "soma de lista com zero"             (soma-lista (list 1 0 2 0 13 0) 0)  16))
	
;; --- Exercício 2 ---------------------

;; Crie uma função recursiva mult-lista (abaixo) que, dada uma lista de números,
;; calcula o produto dos números contidos (a lista vazia deve ter produto igual a 1)
(define (mult-lista l acc)
	(if (empty? l) 
		acc
		(mult-lista (rest l) (* (first l) acc))
	)
)

(define-test-suite testes-mult-lista
  (test-equal? "produto da lista vazia"            (mult-lista '() 1)                  1)
  (test-equal? "produto de lista com zero"         (mult-lista (list 1 0 2 0 13 0) 1)  0)
  (test-equal? "produto de um número"              (mult-lista '(55) 1)                55)
  (test-equal? "produto de vários números"         (mult-lista (list 1 2 3 4 5) 1)     120)
  (test-equal? "produto de números em outra ordem" (mult-lista (list 2 5 1 4 3) 1)     120))

;; --- Exercício 3 --------------------

;; Crie uma função recursiva max-lista (abaixo) que, dada uma lista de números naturais,
;; calcula o maior número entre os presentes na lista. Use (max-lista '()) = 0.

(define (max-lista l acc)
	(if(empty? l) 
		acc
		(if(> (first l) (max-lista (rest l) acc))
			(max-lista (rest l) (first l))
			(max-lista (rest l) acc)
		)
	)
)

(define-test-suite testes-max-lista
  (test-equal? "maximo da lista vazia"       (max-lista '() 0)                     0)
  (test-equal? "maximo de lista unitaria"    (max-lista '(22) 0)                   22)
  (test-equal? "maximo de lista com numeros" (max-lista (list 8 55 13 24 45) 0)    55)
  (test-equal? "maximo não muda com ordem"   (max-lista (list 45 13 8 55 24) 0)    55)
  (test-equal? "maximo de lista com zeros"   (max-lista (list 1 0 13 0 356 0) 0)   356))

;; --- Exercício 4 --------------------

;; Muitas vezes precisamos transformar os elementos de uma lista da mesma
;; maneira. Escreva a função quadrado-lista (abaixo) que, dada uma lista de
;; números, obtém uma lista contendo o quadrado de cada número da lista
;; original (nas mesmas posições)
(define (quadrado-lista l acc)
	(if (empty? l) 
		acc
		(quadrado-lista (rest l) (append acc (cons (* (first l) (first l)) '())))
	)
)

(define-test-suite testes-quadrado-lista
  (test-equal? "quadrado da lista vazia"  (quadrado-lista '() '())        '())
  (test-equal? "quadrado de um número"    (quadrado-lista '(5) '())       '(25))
  (test-equal? "quadrado de números"
               (quadrado-lista (list 2 5 12 25) '())
               (list 4 25 144 625)))

;; --- Executa todos os testes ---------
(run-tests
	(test-suite "todos os testes"
							testes-soma-lista
							testes-mult-lista
							testes-max-lista
							testes-quadrado-lista
							))
 